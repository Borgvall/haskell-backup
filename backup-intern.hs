#!/usr/bin/env runhaskell

import System.Backup

import System.FilePath ((</>))

-- Konfiguration
snapshotDir :: FilePath
snapshotDir = "/snapshots-intern"

destPartition :: FilePath
destPartition = "/backup-intern"

destDir :: FilePath
destDir = destPartition </> "backups"

configuration :: BackupConfiguration
configuration = BackupConfiguration Main.snapshotDir Main.destDir

backupSubvolume :: String -> FilePath -> IO ()
backupSubvolume = backupSubvolumeWithConfig configuration

main :: IO ()
main = withMount destPartition $ do
    checkBackupConfiguration configuration

    backupSubvolume "johannes" "/home"
