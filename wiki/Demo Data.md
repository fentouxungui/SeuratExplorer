# Demo Data

On the **Dataset** tab, click **Download and Run a demo data** to try SeuratExplorer without your own data.

## What it downloads

- A Fly gut single-cell dataset: `G101_PC20res04.rds` (~74 MB).
- Hosted in the companion repository:
  `https://raw.githubusercontent.com/fentouxungui/SeuratExplorerServer/refs/heads/main/inst/extdata/source-data/fly/Rds-file/G101_PC20res04.rds`

The file is downloaded over HTTP **Range** chunks so the progress bar advances, then verified (MD5) and loaded just like an uploaded file.

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
unlink(f, force = TRUE)               # delete the cached demo file
unlink(paste0(f, ".part"), force = TRUE)  # remove any partial download
```

## Point to a different file / mirror

```r
launchSeuratExplorer(
  DemoDataURL      = "https://your-mirror.example/G101_PC20res04.rds",
  DemoDataCacheDir = "D:/seurat_cache",
  DemoDataMD5      = NULL   # set to NULL to skip the integrity check
)
```
