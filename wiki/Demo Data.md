# Demo Data

On the **Dataset** tab, click **Download and Run a demo data** to try SeuratExplorer without your own data.

## What it downloads

- A Fly gut single-cell dataset: `G101_PC20res04.rds` (~74 MB).
- From the companion repository, with a **Google Drive backup** if the primary source fails.

The file is downloaded over HTTP **Range** chunks so the progress bar advances, then verified (MD5) and loaded just like an uploaded file. If the primary URL fails, the app automatically retries the backup source (the progress text shows "trying a backup source ...").

Sources, tried in order:

1. `https://raw.githubusercontent.com/fentouxungui/SeuratExplorerServer/refs/heads/main/inst/extdata/source-data/fly/Rds-file/G101_PC20res04.rds`
2. `https://drive.usercontent.google.com/download?id=1iscfl4zyNbtjAol0bwndnBI08MDcZvYE&export=download&authuser=0&confirm=t&uuid=6352f610-9125-4551-afaa-e1eedd15701c&at=AMrWOn0YlTymrOPO6Rb0GlFtJ07o%3A1790127440033`
3. `https://www.daokedao.site/data/G101_PC20res04.rds`

> The Google Drive "confirm" link can expire over time. It is only a fallback, so the app still works as long as the primary (GitHub) source is reachable.

## Caching

The demo file is **cached** so it is only downloaded once. The cache directory is resolved in this order:

1. `getOption("SeuratExplorerDemoCacheDir")` (i.e. `launchSeuratExplorer(DemoDataCacheDir = ...)`)
2. environment variable `SEURATEXPLORER_CACHE_DIR`
3. `tools::R_user_dir("SeuratExplorer", which = "cache")` — a per-user cache
   - Windows: `%LOCALAPPDATA%\R\cache\R\SeuratExplorer\`
   - macOS: `~/Library/Caches/org.R-project.R/R/SeuratExplorer/`
   - Linux: `~/.cache/R/SeuratExplorer/`
4. `file.path(tempdir(), "SeuratExplorer-cache")` (last-resort fallback)

The cached file is named `G101_PC20res04.rds`.

When a cached copy already exists, the app shows a dialog with the file's path and a "Loading the data …" spinner, then loads it (no re-download).

## Remove the cached file

```r
f <- file.path(tools::R_user_dir("SeuratExplorer", which = "cache"), "G101_PC20res04.rds")
f
file.exists(f)
unlink(f, force = TRUE)                    # delete the cached demo file
unlink(paste0(f, ".part"), force = TRUE)   # remove any partial download
```

## Point to other sources

`DemoDataURL` accepts one or more URLs, tried in order:

```r
launchSeuratExplorer(
  DemoDataURL = c(
    "https://raw.githubusercontent.com/fentouxungui/SeuratExplorerServer/refs/heads/main/inst/extdata/source-data/fly/Rds-file/G101_PC20res04.rds",
    "https://your-mirror.example/G101_PC20res04.rds"
  ),
  DemoDataCacheDir = "D:/seurat_cache",
  DemoDataMD5      = NULL   # set to NULL to skip the integrity check
)
```
