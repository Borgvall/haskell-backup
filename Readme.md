# Haskell Btrfs Backup Scripts

> **⚠️ DISCLAIMER: "It Works On My Machine"**
> This is **not** a general-purpose backup tool. It is a highly specific, opinionated set of scripts tailored exactly to **my** filesystem layout, **my** drive UUIDs, and **my** workflow.
> There is no config file parsing, no broad error handling for edge cases I don't encounter, and the library contains logic hardcoded for my specific directory structures. **Use at your own risk** (or as inspiration for your own "hacky" solution).

## Overview

This repository contains a set of Haskell scripts designed to orchestrate incremental **Btrfs** backups from a local machine to various external destinations (USB drives, internal backup partitions).

It leverages Haskell for logic safety (using `bracket` for resource management) while delegating the heavy lifting to `btrfs send` and `btrfs receive`.

## The Philosophy: Pragmatism > Purity

While written in Haskell, this code does not strive for functional purity or abstract elegance. It strives to ensure that **my** backups run reliably on **my** hardware.

* **Hardcoded Flexibility:** The library (`System.Backup`) contains "magic" logic to auto-detect if it's running on my NixOS setup (`/btrfs-root/root`) or my standard setup (`/snapshots`) and adjusts paths at runtime.
* **Local Space Optimization:** The script aggressively deletes the *old* local parent snapshot after a successful transfer. I only keep the absolute latest snapshot locally to save disk space.

## Features

* **Type-Safe Mount Handling:** Uses `Control.Exception.bracket` to ensure the backup partition is **always** unmounted after the script finishes, even if the backup crashes or is interrupted.
* **Atomic-ish Transactions:** If a `btrfs receive` fails, the script attempts to clean up the partial snapshots on both source and destination to prevent garbage accumulation.
* **Smart Incremental Backups:**
    1. Checks a local `.last_copied` file to find the common base.
    2. Verifies the base exists.
    3. Uses `btrfs send -p <parent>` for efficient delta transfers.


* **Compressed Transfer:** Sends data using `--compressed-data` to save I/O bandwidth.

## Structure

* **`System/Backup.hs`**: The core "library". It handles the mounting, snapshot creation, sending, and cleanup logic. It also contains the hardcoded path detection logic.
* **`backup.hs`**: Entry point for the standard external backup drive (`/backup-btrfs`).
* **`backup-2TB.hs`**: Entry point for the 2TB archive drive (`/backup-btrfs-2TB`).
* **`backup-intern.hs`**: Entry point for the internal backup disk (`/backup-intern`).

## Usage

Since this is a `runhaskell` script, no compilation is strictly necessary, though you need the Btrfs tools installed.

```bash
# Must be run as root (for mounting and btrfs commands)
sudo ./backup.hs

```

## How it works (The Cycle)

1. **Mount:** The script mounts the target partition defined in the entry file (e.g., `/backup-btrfs`).
2. **Snapshot:** Takes a read-only snapshot of the subvolume (e.g., `/home`) to a local snapshot directory.
3. **Detect Parent:** Reads `<subvol>.last_copied`.
    * *If found:* Performs an incremental send (`-p`).
    * *If not found:* Performs a full send.


4. **Transfer:** Pipes `btrfs send` to `btrfs receive` on the destination.
5. **Update State:** Updates `<subvol>.last_copied` with the new snapshot name.
6. **Cleanup:**
    * Deletes the **old** parent snapshot from the local disk (to save space).
    * Backs up the `.last_copied` file to the destination (as a hacky fail-safe).


7. **Unmount:** Safely unmounts the destination.

---

*Written by Johannes. If you are not Johannes, you probably need to change the paths.*
