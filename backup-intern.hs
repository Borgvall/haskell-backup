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

main :: IO ()
main = withMount destPartition $ do
    backupSubvolume <- checkBackupConfiguration configuration

    backupSubvolume "johannes" "/home"
