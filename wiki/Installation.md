# Installation

## From GitHub (recommended)

```r
if (!require("devtools")) install.packages("devtools")
devtools::install_github("fentouxungui/SeuratExplorer", dependencies = TRUE)
```

## From CRAN

SeuratExplorer is on CRAN. A few suggested dependencies live outside CRAN, so install them first:

```r
# Bioconductor packages used by optional features
if (!require("BiocManager", quietly = TRUE)) install.packages("BiocManager")
BiocManager::install(c("ComplexHeatmap", "MAST", "limma", "DESeq2"))

# presto (used for fast marker detection) is GitHub-only
if (!require("devtools")) install.packages("devtools")
devtools::install_github("immunogenomics/presto")

install.packages("SeuratExplorer")
```

## System requirements

- R (>= 4.1.0)
- Seurat (>= 5.4.0)
- SeuratObject (>= 5.3.0)
- ggplot2 (>= 4.0.1)
- `curl` (used by the one-click demo-data download)

## Optional dependencies

These enable specific features and are used conditionally:

| Package | Enables |
|---|---|
| `ComplexHeatmap` | Cell-level and group-averaged heatmaps |
| `presto` | Fast `wilcox` marker tests in DEG analysis |
| `MAST` | `test.use = "MAST"` in DEG analysis |
| `limma` | `test.use = "wilcox_limma"` / limma-based tests |
| `DESeq2` | `test.use = "DESeq2"` in DEG analysis |
| `Signac` | Feature annotations for ATAC assays in **Search Features** |

## Verify

```r
library(SeuratExplorer)
packageVersion("SeuratExplorer")
```
