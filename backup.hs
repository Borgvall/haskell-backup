#!/usr/bin/env runhaskell

import System.Backup

import System.FilePath ((</>))

-- Konfiguration
snapshotDir :: FilePath
snapshotDir = "/snapshots"

destPartition :: FilePath
destPartition = "/backup-btrfs"

destDir :: FilePath
destDir = destPartition </> "Snapshots"

configuration :: BackupConfiguration
configuration = BackupConfiguration Main.snapshotDir Main.destDir

backupSubvolume :: String -> FilePath -> IO ()
backupSubvolume = backupSubvolumeWithConfig configuration

main :: IO ()
main = withMount destPartition $ do
    checkBackupConfiguration configuration

    backupSubvolume "johannes" "/home"
    backupSubvolume "Steam" "/home/johannes/.local/share/"
    backupSubvolume "EA-Snapshot" "/home/johannes/wine-prefices/Bottles"
    backupSubvolume "Ubisoft-Connect" "/home/johannes/wine-prefices/Bottles"
    backupSubvolume "Heroic" "/home/johannes/Games"
