#!/usr/bin/env runhaskell

import System.Process (callCommand, callProcess)
import System.Directory (doesDirectoryExist, doesFileExist, listDirectory)
import System.FilePath ((</>))
import System.IO (hPutStrLn, stderr)
import Data.Time.Clock (getCurrentTime)
import Data.Time.Format (formatTime, defaultTimeLocale)
import Control.Monad (when, forM_)
import Control.Exception (bracket, onException)
import System.Exit (exitFailure)

-- Konfiguration
snapshotDir :: FilePath
snapshotDir = "/snapshots"

destPartition :: FilePath
destPartition = "/backup-btrfs"

destDir :: FilePath
destDir = destPartition </> "Snapshots"

-- Hilfsfunktion für Logging
logMsg :: String -> IO ()
logMsg = putStrLn

-- Hilfsfunktion um Befehle auszuführen
runCmd :: String -> IO ()
runCmd cmd = callCommand cmd

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

-- Die Haupt-Backup-Logik
backupSubvolume :: String -> FilePath -> IO ()
backupSubvolume subvolName subvolBasePath = do
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
            content <- readFile lastClonedFile
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
    let transferAction = case parentSnapshot of
            Nothing -> do
                logMsg $ "Copy " ++ snapshotPath ++ " to " ++ snapshotCopy
                let cmd = "ionice -n " ++ ioPriority ++ " btrfs send --compressed-data --proto 0 " ++ snapshotPath ++ " | btrfs receive " ++ destDir
                runCmd cmd

            Just parent -> do
                logMsg $ "Copy " ++ snapshotPath ++ " to " ++ snapshotCopy ++ " using " ++ parent ++ " as base"
                let cmd = "ionice -n " ++ ioPriority ++ " btrfs send --compressed-data --proto 0 -p " ++ parent ++ " " ++ snapshotPath ++ " | btrfs receive " ++ destDir
                runCmd cmd

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

backupWinePrefix :: String -> IO ()
backupWinePrefix prefix = backupSubvolume prefix "/home/johannes/wine-prefices"

backupGameWinePrefices :: IO ()
backupGameWinePrefices = do
    forM_ ["gogGalaxy", "epicgameslauncher"] backupWinePrefix

main :: IO ()
main = withMount destPartition $ do
    destExists <- doesDirectoryExist destDir
    when (not destExists) $ do
            hPutStrLn stderr $ destDir ++ " is not a directory"
            exitFailure

    backupSubvolume "johannes" "/home"
    backupGameWinePrefices
    backupSubvolume "Steam" "/home/johannes/.local/share/"
    backupSubvolume "EA-Snapshot" "/home/johannes/wine-prefices/Bottles"
    backupSubvolume "Ubisoft-Connect" "/home/johannes/wine-prefices/Bottles"
    backupSubvolume "Heroic" "/home/johannes/Games"
