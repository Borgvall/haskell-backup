#!/usr/bin/env runhaskell

import System.Backup

import System.FilePath ((</>))

-- Konfiguration
snapshotDir :: FilePath
snapshotDir = "/snapshots-2TB"

destPartition :: FilePath
destPartition = "/backup-btrfs-2TB"

destDir :: FilePath
destDir = destPartition </> "snapshot-backups"

configuration :: BackupConfiguration
configuration = BackupConfiguration Main.snapshotDir Main.destDir

backupSubvolume :: String -> FilePath -> IO ()
backupSubvolume = backupSubvolumeWithConfig configuration

main :: IO ()
main = withMount destPartition $ do
    checkBackupConfiguration configuration

    backupSubvolume "johannes" "/home"
