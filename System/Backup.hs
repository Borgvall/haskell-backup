{-# LANGUAGE RecordWildCards #-}

module System.Backup (
  BackupConfiguration(..),
  withMount,
  checkBackupConfiguration,
) where

import System.Process (callCommand, callProcess, proc, createProcess, std_out, std_in, StdStream(..), waitForProcess)
import System.Directory (doesDirectoryExist, doesFileExist, listDirectory)
import System.FilePath ((</>))
import System.IO (hPutStrLn, stderr, withFile, IOMode(..), hGetContents, hClose)
import Data.Time.Clock (getCurrentTime)
import Data.Time.Format (formatTime, defaultTimeLocale)
import Control.Monad (when, forM_)
import Control.Exception (bracket, evaluate, onException, throwIO)
import System.Exit (exitFailure, ExitCode(..))

data BackupConfiguration = BackupConfiguration {
  snapshotDir :: FilePath,
  destDir :: FilePath
} deriving (Eq, Show)

-- Hilfsfunktion für Logging
logMsg :: String -> IO ()
logMsg = putStrLn

-- Hilfsfunktion um Befehle auszuführen
runCmd :: String -> IO ()
runCmd cmd = callCommand cmd

readFileStrict :: FilePath -> IO String
readFileStrict f = withFile f ReadMode $ \h -> do
    content <- hGetContents h
    -- Wir erzwingen das vollständige Lesen durch 'evaluate (length content)'
    _ <- evaluate (length content)
    return content

-- Hilfsfunktion: Löscht ein Btrfs Subvolume sicher
safeDeleteSubvolume :: FilePath -> IO ()
safeDeleteSubvolume path = do
    exists <- doesDirectoryExist path
    when exists $ do
        logMsg $ "Cleaning up: Deleting subvolume " ++ path
        callProcess "btrfs" ["subvolume", "delete", path]

-- Mount-Logik
withMount :: FilePath -> IO a -> IO a
withMount partitionPath action = do
    isDirEmpty <- null <$> listDirectory partitionPath
    
    let mountAction = if isDirEmpty
        then do
            logMsg $ partitionPath ++ " is empty trying to mount it"
            callProcess "mount" [partitionPath]
            return True 
        else return False 

    let unmountAction mountedByScript = when mountedByScript $ do
            logMsg $ "Unmount " ++ partitionPath
            callProcess "umount" [partitionPath]

    bracket mountAction unmountAction (const action)

checkBackupConfiguration :: BackupConfiguration -> IO (String -> FilePath -> IO ())
checkBackupConfiguration initialConfig = do
    let destDir' = destDir initialConfig
    destExists <- doesDirectoryExist destDir'
    when (not destExists) $ do
            throwIO . userError $ destDir' ++ " is not a directory"

    -- Pfad-Erkennung: Prüfen, ob das konfigurierte Verzeichnis existiert.
    -- Falls nicht, prüfen wir auf den NixOS-Pfad.
    actualSnapshotDir <- do
        let configuredDir = snapshotDir initialConfig
        exists <- doesDirectoryExist configuredDir
        if exists
            then return configuredDir
            else do
                let nixosPath = "/btrfs-root/root" ++ configuredDir
                logMsg $ "Try to find snapshot directory on NixOS: " ++ nixosPath
                nixosExists <- doesDirectoryExist nixosPath
                if nixosExists
                    then return nixosPath
                    else throwIO . userError $ "Could not find snapshot directory " ++ configuredDir

    let backupConfiguration = initialConfig { snapshotDir = actualSnapshotDir }
    pure $ backupSubvolumeWithConfig backupConfiguration


-- Die Haupt-Backup-Logik
backupSubvolumeWithConfig :: BackupConfiguration -> String -> FilePath -> IO ()
backupSubvolumeWithConfig BackupConfiguration {..} subvolName subvolBasePath = do
    let fullSubvolPath = subvolBasePath </> subvolName
    
    logMsg $ "Backup " ++ subvolName ++ " at " ++ fullSubvolPath
    
    now <- getCurrentTime
    let timestamp = formatTime defaultTimeLocale "%F-%H_%M" now
    let snapshotName = subvolName ++ "-" ++ timestamp
    let snapshotPath = snapshotDir </> snapshotName
    let snapshotCopy = destDir </> snapshotName
    let lastClonedFile = snapshotDir </> subvolName ++ ".last_copied"
    let ioPriority = "5"

    hasLastFile <- doesFileExist lastClonedFile
    
    parentSnapshot <- if hasLastFile
        then do
            content <- readFileStrict lastClonedFile
            let rawParentName = head (lines content) 
            logMsg $ "Read the base snapshot from " ++ lastClonedFile
            
            let parentCandidate = snapshotDir </> rawParentName
            exists <- doesDirectoryExist parentCandidate
            
            if exists
                then return (Just parentCandidate)
                else do
                    absExists <- doesDirectoryExist rawParentName
                    if absExists
                        then do 
                            logMsg "Nothing to do (Legacy path found)"
                            return (Just rawParentName)
                        else do
                            hPutStrLn stderr $ "ERROR " ++ rawParentName ++ " is not there."
                            exitFailure
        else do
            logMsg "Creating full backup of the subvolume"
            return Nothing

    logMsg $ "Snapshot " ++ subvolName ++ " as " ++ snapshotPath
    -- 1. Snapshot erstellen
    callProcess "btrfs" ["subvolume", "snapshot", "-r", fullSubvolPath, snapshotPath]

    -- 2. Nur der Transfer (ohne Buchführung)
    let transferAction = do
            let sendBaseArgs = ["-n", ioPriority, "btrfs", "send", "--compressed-data", "--proto", "0"]
            let sendArgs = case parentSnapshot of
                    Nothing -> sendBaseArgs ++ [snapshotPath]
                    Just parent -> sendBaseArgs ++ ["-p", parent, snapshotPath]

            let receiveArgs = ["receive", destDir]

            logMsg $ "Streaming " ++ snapshotPath ++ " to " ++ destDir

            -- Prozess 1: Sender (ionice -> btrfs send)
            -- Wir erstellen eine Pipe für stdout
            let senderProc = (proc "ionice" sendArgs) { std_out = CreatePipe }
            
            -- Prozess starten
            (_, Just hOut, _, phSend) <- createProcess senderProc

            -- Prozess 2: Empfänger (btrfs receive)
            -- Wir nutzen das Handle vom Sender als Input
            let receiverProc = (proc "btrfs" receiveArgs) { std_in = UseHandle hOut }
            
            (_, _, _, phReceive) <- createProcess receiverProc

            -- Wir schließen das Handle im Elternprozess, da es nun dem Kindprozess gehört
            -- und wir es hier nicht mehr brauchen.
            hClose hOut

            -- Auf beide Prozesse warten
            sendExit <- waitForProcess phSend
            recvExit <- waitForProcess phReceive

            -- Fehlerprüfung
            when (sendExit /= ExitSuccess || recvExit /= ExitSuccess) $
                 throwIO $ userError $ "Transfer failed: Sender=" ++ show sendExit ++ ", Receiver=" ++ show recvExit

    -- 3. Ausführung mit Fehlerbehandlung
    transferAction `onException` do
        hPutStrLn stderr $ "Transaction failed for " ++ subvolName ++ ". Reverting..."
        safeDeleteSubvolume snapshotPath -- Lösche lokalen, neu erstellten Snapshot
        safeDeleteSubvolume snapshotCopy -- Lösche (kaputten) Remote Snapshot

    -- 4. Buchführung & Cleanup (wird nur bei Erfolg erreicht)
    logMsg $ "Updating metadata in " ++ lastClonedFile
    writeFile lastClonedFile snapshotName

    case parentSnapshot of
        Just parent -> do
            logMsg $ "Delete old parent snapshot " ++ parent
            callProcess "btrfs" ["subvolume", "delete", parent]
        Nothing -> return ()

    -- 5. HACK
    callProcess "cp" ["-a", lastClonedFile, destDir]
