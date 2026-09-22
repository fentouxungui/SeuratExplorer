# SeuratExplorer

> **An R/Shiny app for exploring single-cell RNA-seq data processed in Seurat — no coding required.**

SeuratExplorer is a one-command R package that launches an interactive dashboard for the most common single-cell visualizations. It works with a **processed Seurat object** saved as an `.rds` or `.qs2` file.

- Repo: <https://github.com/fentouxungui/SeuratExplorer>
- CRAN: <https://cran.r-project.org/package=SeuratExplorer>
- Live demo: <http://netinfo.nibs.ac.cn:666/SeuratExplorer/>
- Issues / feature requests: <https://github.com/fentouxungui/SeuratExplorer/issues>

## Why SeuratExplorer

There is no comprehensive tool that lets users **without programming skills** explore Seurat analysis results. When a bioinformatics analyst hands results to wet-lab users, those users often cannot retrieve or re-plot the data on their own. SeuratExplorer bridges that gap: it wraps command-line Seurat (and related) functions in a point-and-click interface. Users only need R installed (or access to a deployed Shiny Server) and the package.

## Key features

- **No coding required** — interactive point-and-click interface for all analyses.
- **Comprehensive visualizations** — 10+ plot types.
- **Multi-assay support** — scRNA-seq, scATAC-seq, spatial, CITE-seq and multi-omics.
- **Interactive annotation** — rename clusters, define gene-expression-based clusters, combine cluster annotations, compute module scores.
- **Publication-ready output** — download figures as PDF (WYSIWYG) and tables as CSV.
- **Interactivity** — results update as you adjust parameters; multiple genes/clusters at once.

## Getting started in 30 seconds

```r
if (!require("devtools")) install.packages("devtools")
devtools::install_github("fentouxungui/SeuratExplorer", dependencies = TRUE)

library(SeuratExplorer)
launchSeuratExplorer()
```

Then in the browser:

1. **Upload** your processed Seurat object (`.rds` / `.qs2`), **or**
2. Click **Download and Run a demo data** to try the app with a bundled demo dataset (see [[Demo Data]]).

## Wiki pages

- [[Installation]] — install from GitHub or CRAN, dependencies, system requirements.
- [[Quick Start]] — launch the app and load data.
- [[Demo Data]] — the one-click demo dataset: what it is, where it is cached.
- [[Interface Guide]] — every menu page and what it does.
- [[Data Preparation]] — what your Seurat object needs.
- [[Launch Options]] — all `launchSeuratExplorer()` parameters.
- [[Troubleshooting & FAQ]] — common questions and fixes.
- [[Citation & License]].

## Companion project

Deploying a multi-user, database-style instance (sample metadata, reports, comments, login) is handled by the companion package **[SeuratExplorerServer](https://github.com/fentouxungui/SeuratExplorerServer)**, which builds its UI on top of `SeuratExplorer::explorer_server()` / `explorer_sidebar_ui()` / `explorer_body_ui()`.
