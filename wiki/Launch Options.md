# Launch Options

```r
launchSeuratExplorer(
  verbose             = FALSE,
  ReductionKeyWords   = c("umap", "tsne", "pca"),
  SplitOptionMaxLevel = 12,
  MaxInputFileSize    = 20 * 1024^3,   # 20 GB
  DemoDataURL         = c(
    "https://raw.githubusercontent.com/fentouxungui/SeuratExplorerServer/refs/heads/main/inst/extdata/source-data/fly/Rds-file/G101_PC20res04.rds",
    "https://drive.usercontent.google.com/download?id=1iscfl4zyNbtjAol0bwndnBI08MDcZvYE&export=download&authuser=0&confirm=t&uuid=6352f610-9125-4551-afaa-e1eedd15701c&at=AMrWOn0YlTymrOPO6Rb0GlFtJ07o%3A1790127440033",
    "https://www.daokedao.site/data/G101_PC20res04.rds"
  )
  DemoDataCacheDir    = NULL,
  DemoDataMD5         = "e144cabc42684f037e60a603f74ed4f1"
)
```

| Argument | Default | Description |
|---|---|---|
| `verbose` | `FALSE` | Print debug messages. |
| `ReductionKeyWords` | `c("umap","tsne","pca")` | Keywords used to discover dimensionality reductions. |
| `SplitOptionMaxLevel` | `12` | Max number of levels for a metadata column to appear as a split option. |
| `MaxInputFileSize` | `20*1024^3` | Maximum upload size in bytes (sets `shiny.maxRequestSize`). |
| `DemoDataURL` | see above | URL of the demo dataset used by the **Download and Run a demo data** button. |
| `DemoDataCacheDir` | `NULL` | Cache directory for the demo data; `NULL` uses the per-user cache (see [[Demo Data]]). |
| `DemoDataMD5` | the file's MD5 | Expected checksum of the downloaded demo data; set to `NULL` to skip the check. |

Example with custom parameters:

```r
launchSeuratExplorer(
  verbose = TRUE,
  ReductionKeyWords = c("umap", "tsne", "pca"),
  MaxInputFileSize = 10 * 1024^3
)
```

## Deploy on a server

```r
# app.R
library(SeuratExplorer)
launchSeuratExplorer()
```

For a multi-user database instance with sample metadata, reports, comments and authentication, use the companion package [SeuratExplorerServer](https://github.com/fentouxungui/SeuratExplorerServer).
