
<!-- README.md is generated from README.Rmd. Please edit that file -->

<!-- To generate README.md for GitHub homepage: copy this file to the package root directory, set for_github parameter to TRUE, then knit. Delete the README.Rmd file from the root directory after completion -->

# SeuratExplorer

<!-- badges: start -->

[![](https://img.shields.io/badge/lifecycle-stable-brightgreen.svg)](https://lifecycle.r-lib.org/articles/stages.html#stable)
[![](https://www.r-pkg.org/badges/version/SeuratExplorer)](https://cran.r-project.org/package=SeuratExplorer)
[![](https://img.shields.io/badge/devel%20version-0.1.3-rossellhayes.svg)](https://github.com/fentouxungui/SeuratExplorer)
[![](http://cranlogs.r-pkg.org/badges/grand-total/SeuratExplorer)](https://cran.r-project.org/package=SeuratExplorer)
![Badge](https://hitscounter.dev/api/hit?url=https%3A%2F%2Fgithub.com%2Ffentouxungui%2FSeuratExplorer&label=Visitor&icon=github&color=%23198754&message=&style=flat&tz=Asia%2FHong_Kong)
[![](https://img.shields.io/github/languages/code-size/fentouxungui/SeuratExplorer.svg)](https://github.com/fentouxungui/SeuratExplorer)
[![AskDeepWiki](https://deepwiki.com/badge.svg)](https://deepwiki.com/fentouxungui/SeuratExplorer)
[![AskZreadAI](https://img.shields.io/badge/Ask_Zread-_.svg?style=flat&color=00b0aa&labelColor=000000&logo=data%3Aimage%2Fsvg%2Bxml%3Bbase64%2CPHN2ZyB3aWR0aD0iMTYiIGhlaWdodD0iMTYiIHZpZXdCb3g9IjAgMCAxNiAxNiIgZmlsbD0ibm9uZSIgeG1sbnM9Imh0dHA6Ly93d3cudzMub3JnLzIwMDAvc3ZnIj4KPHBhdGggZD0iTTQuOTYxNTYgMS42MDAxSDIuMjQxNTZDMS44ODgxIDEuNjAwMSAxLjYwMTU2IDEuODg2NjQgMS42MDE1NiAyLjI0MDFWNC45NjAxQzEuNjAxNTYgNS4zMTM1NiAxLjg4ODEgNS42MDAxIDIuMjQxNTYgNS42MDAxSDQuOTYxNTZDNS4zMTUwMiA1LjYwMDEgNS42MDE1NiA1LjMxMzU2IDUuNjAxNTYgNC45NjAxVjIuMjQwMUM1LjYwMTU2IDEuODg2NjQgNS4zMTUwMiAxLjYwMDEgNC45NjE1NiAxLjYwMDFaIiBmaWxsPSIjZmZmIi8%2BCjxwYXRoIGQ9Ik00Ljk2MTU2IDEwLjM5OTlIMi4yNDE1NkMxLjg4ODEgMTAuMzk5OSAxLjYwMTU2IDEwLjY4NjQgMS42MDE1NiAxMS4wMzk5VjEzLjc1OTlDMS42MDE1NiAxNC4xMTM0IDEuODg4MSAxNC4zOTk5IDIuMjQxNTYgMTQuMzk5OUg0Ljk2MTU2QzUuMzE1MDIgMTQuMzk5OSA1LjYwMTU2IDE0LjExMzQgNS42MDE1NiAxMy43NTk5VjExLjAzOTlDNS42MDE1NiAxMC42ODY0IDUuMzE1MDIgMTAuMzk5OSA0Ljk2MTU2IDEwLjM5OTlaIiBmaWxsPSIjZmZmIi8%2BCjxwYXRoIGQ9Ik0xMy43NTg0IDEuNjAwMUgxMS4wMzg0QzEwLjY4NSAxLjYwMDEgMTAuMzk4NCAxLjg4NjY0IDEwLjM5ODQgMi4yNDAxVjQuOTYwMUMxMC4zOTg0IDUuMzEzNTYgMTAuNjg1IDUuNjAwMSAxMS4wMzg0IDUuNjAwMUgxMy43NTg0QzE0LjExMTkgNS42MDAxIDE0LjM5ODQgNS4zMTM1NiAxNC4zOTg0IDQuOTYwMVYyLjI0MDFDMTQuMzk4NCAxLjg4NjY0IDE0LjExMTkgMS42MDAxIDEzLjc1ODQgMS42MDAxWiIgZmlsbD0iI2ZmZiIvPgo8cGF0aCBkPSJNNCAxMkwxMiA0TDQgMTJaIiBmaWxsPSIjZmZmIi8%2BCjxwYXRoIGQ9Ik00IDEyTDEyIDQiIHN0cm9rZT0iI2ZmZiIgc3Ryb2tlLXdpZHRoPSIxLjUiIHN0cm9rZS1saW5lY2FwPSJyb3VuZCIvPgo8L3N2Zz4K&logoColor=ffffff)](https://zread.ai/fentouxungui/SeuratExplorer)
<!-- badges: end -->

> A `Shiny` App for Exploring scRNA-seq Data Processed in `Seurat`

A simple, one-command package which runs an interactive dashboard
capable of common visualizations for single cell RNA-seq.
`SeuratExplorer` requires a processed `Seurat` object, which is saved as
`rds` or `qs2` file.

## Why build this R package

> Currently, there are no comprehensive tools for visualizing Seurat
> analysis results that are accessible to users without programming
> expertise. When bioinformatics analysts hand over results to end
> users, those without R programming knowledge often struggle to
> retrieve and re-analyze the data independently. `SeuratExplorer` is
> designed to bridge this gap by enabling such users to visualize and
> explore analysis results through an intuitive graphical interface. The
> only requirement for users is to configure R and RStudio on their
> computers and install `SeuratExplorer`. Alternatively, users can
> upload their `Seurat object` files to a server deployed with
> `Shiny Server` and `SeuratExplorer` for browser-based access.

> Essentially, `SeuratExplorer` provides graphical interfaces for
> command-line tools from `Seurat` and other related packages, making
> complex analyses accessible without programming.

### Key Features

- **No Coding Required**: Interactive point-and-click interface for all
  analyses
- **Comprehensive Visualizations**: 10+ plot types for exploring
  single-cell data
- **Multi-Assay Support**: Works with scRNA-seq, scATAC-seq, spatial,
  and multi-omics data
- **Flexible Analysis**: Find markers, explore correlations, and
  summarize features interactively
- **Publication-Ready Plots**: Download high-quality PDF figures
  directly from the app
- **Batch Processing**: Analyze multiple genes/clusters simultaneously
- **Real-time Results**: See changes immediately as you adjust
  parameters
- **Data Export**: Download analysis results in CSV format for further
  analysis

## Installation

Install the latest version from github - ***Recommended***:

``` r
if(!require(devtools)){install.packages("devtools")}
install_github("fentouxungui/SeuratExplorer", dependencies = TRUE)
```

Or install from CRAN:

``` r
# Install non-CRAN dependencies first
if (!require("BiocManager", quietly = TRUE)){
  install.packages("BiocManager")
}
BiocManager::install(c("ComplexHeatmap", "MAST", "limma", "DESeq2"))

# Install presto from GitHub
if(!require(devtools)){
  install.packages("devtools")
}
devtools::install_github("immunogenomics/presto")

# Install SeuratExplorer from CRAN
install.packages("SeuratExplorer")
```

**System Requirements:**

- R (\>= 4.1.0)
- Seurat (\>= 5.4.0)
- SeuratObject (\>= 5.3.0)
- ggplot2 (\>= 4.0.1)

## Run app on local

``` r
library(SeuratExplorer)
launchSeuratExplorer()
```

You can customize the launch behavior with additional parameters:

- `verbose`: Set to `TRUE` for debug messages (default: `FALSE`)
- `ReductionKeyWords`: Keywords for dimension reduction options
  (default: `c("umap","tsne")`)
- `SplitOptionMaxLevel`: Maximum levels for split options (default:
  `12`)
- `MaxInputFileSize`: Maximum upload file size in bytes (default:
  `20*1024^3`, i.e., 20GB)

``` r
# Example with custom parameters
launchSeuratExplorer(
  verbose = TRUE,
  ReductionKeyWords = c("umap", "tsne", "pca"),
  MaxInputFileSize = 10*1024^3  # 10GB
)
```

## Deploy on server

You can deploy this app on a Shiny Server, which allows users to
visualize their data through a web interface by uploading the data to
the server.

**Live Demo**: Upload an `.rds` or `.qs2` file (up to 20GB) to the [Demo
Site](http://www.nibs.ac.cn:666/SeuratExplorer/). You can download
sample demo data from
[GitHub](https://github.com/fentouxungui/SeuratExplorerServer/blob/main/inst/extdata/source-data/fly/Rds-file/G101_PC20res04.rds).

``` r
# app.R
library(SeuratExplorer)
launchSeuratExplorer()
```

## Assay option

> [Seurat Assay](https://github.com/satijalab/seurat/wiki/Assay)
>
> The Assay class stores single cell data. For typical scRNA-seq
> experiments, a Seurat object will have a single Assay (“RNA”). This
> assay will also store multiple ‘transformations’ of the data,
> including raw counts (@counts slot), normalized data (@data slot), and
> scaled data for dimensional reduction (@scale.data slot).

SeuratExplorer supports assay switching, enabling compatibility with
multiple data types, including:

**Single-cell Modalities:**

- **scRNA-seq**: Usually the default ‘RNA’ assay containing gene
  expression data
- **scATAC-seq**: Usually named with “ATAC” (chromatin accessibility)
  and “ACTIVITY” (derived gene activity scores)
- **Spatial Transcriptomics**:
  - Xenium data, usually named with ‘Xenium’
  - Visium HD data, usually named with ‘Visium’
  - Other spatial platforms (MERFISH, seqFISH, etc.)
- **CITE-seq**: For antibody-derived tags (ADT) data, usually named with
  ‘ADT’ or ‘HTO’
- **Multi-omics**: Any custom assay types following Seurat’s assay
  structure

**Normalization Methods:**

- **SCT assay**: Using SCTransform normalization method
- **cellbender assay**: Using cellbender background correction output
- **lsi assay**: From Latent Semantic Indexing dimensionality reduction
- Other custom normalization approaches

**Assay Slots:**

- **counts**: Stores unnormalized data such as raw counts or TPMs
- **data**: Normalized data matrix (log-normalized or other
  transformations)
- **scale.data**: Scaled data matrix (used for dimensional reduction and
  visualization)

### Assay Slot Support by Feature

Different visualization features support different assay slots:

- **Feature Plot**: Supports `counts`, `data`, `scale.data`
- **Violin Plot**: Supports `counts`, `data`, `scale.data`
- **Dot Plot**: Supports `data` slot
- **Heatmap (Cell Level)**: Supports any available slot
- **Heatmap (Group Level)**: Supports `data`, `scale.data`
- **Ridge Plot**: Supports `counts`, `data`, `scale.data`
- **DEGs Analysis**: Supports `counts`, `data`
- **Top Expressed Features**: Supports `counts` (by accumulation) or any
  slot (by cell)
- **Feature Summary**: Supports `data`
- **Feature Correlation**: Supports `data`

## Introduction

### Load data

- Supports `Seurat` objects saved as `.rds` or `.qs2` files

- Supports data processed by `Seurat` V5 and older versions. Note: It
  may take some time to update older `Seurat` objects when loading data

<img src="./inst/extdata/www/upload-data.png" alt="" width="100%" />

### Dimensional Reduction Plot

- Supports selection of **dimensionality reduction methods** (UMAP,
  t-SNE, PCA, etc.)

- Supports selection of **cluster resolutions**

- Supports **split** plots

- Supports highlighting selected clusters

- Supports adjusting the height/width ratio of the plot

- Supports displaying **cluster labels**

- Supports adjusting label size

- Supports adjusting point size

- Supports downloading plots in PDF format (WYSIWYG)

**Example plots:**

<img src="./inst/extdata/www/Dimplot-1.png" alt="" width="100%" /><img src="./inst/extdata/www/Dimplot-2.png" alt="" width="100%" /><img src="./inst/extdata/www/Dimplot-1-highlight.png" alt="" width="100%" />

### Feature Plot

- Supports simultaneous display of multiple genes (gene names are
  case-insensitive). Tip: Paste multiple gene names directly from Excel

- Supports selection of **dimensionality reduction methods**

- Supports **split** plots

- Supports customizing colors for lowest and highest expression levels

- Supports adjusting the height/width ratio of the plot

- Supports adjusting point size

- Supports downloading plots in PDF format (WYSIWYG)

- Supports switching between assays containing any of the following
  slots: `counts`, `data`, `scale.data`

**Example plots:**

<img src="./inst/extdata/www/featureplot-1.png" alt="" width="100%" /><img src="./inst/extdata/www/featureplot-2.png" alt="" width="100%" /><img src="./inst/extdata/www/featureplot-3.png" alt="" width="100%" />

### Violin Plot

- Supports simultaneous display of multiple genes (gene names are
  case-insensitive). Tip: Paste multiple gene names directly from Excel

- Supports selection of **cluster resolutions**

- Supports **split** plots

- Supports **stacking** and **flipping** plots, with color mapping
  selection

- Supports adjusting point size and transparency

- Supports adjusting font sizes on x and y axes

- Supports adjusting the height/width ratio of the plot

- Supports downloading plots in PDF format (WYSIWYG)

- Supports switching between assays containing any of the following
  slots: `counts`, `data`, `scale.data`

**Example plots:**

<img src="./inst/extdata/www/violin-1.png" alt="" width="100%" /><img src="./inst/extdata/www/violin-2.png" alt="" width="100%" /><img src="./inst/extdata/www/violin-3.png" alt="" width="100%" /><img src="./inst/extdata/www/violin-4.png" alt="" width="100%" /><img src="./inst/extdata/www/violin-5.png" alt="" width="100%" /><img src="./inst/extdata/www/violin-6.png" alt="" width="100%" /><img src="./inst/extdata/www/violin-7.png" alt="" width="100%" />

### Dot Plot

- Supports simultaneous display of multiple genes (gene names are
  case-insensitive). Tip: Paste multiple gene names directly from Excel

- Supports selection of **cluster resolutions** and subsetting clusters

- Supports **split** plots

- Supports clustering clusters

- Supports rotating axes and flipping coordinates

- Supports adjusting point size and transparency

- Supports adjusting font sizes on x and y axes

- Supports adjusting the height/width ratio of the plot

- Supports downloading plots in PDF format (WYSIWYG)

- Supports switching between assays containing the `data` slot

**Example plots:**

<img src="./inst/extdata/www/dotplot.png" alt="" width="100%" /><img src="./inst/extdata/www/dotplot-1.png" alt="" width="100%" />

### Heatmap for cell level expression

- Supports simultaneous display of multiple genes (gene names are
  case-insensitive). Tip: Paste multiple gene names directly from Excel

- Supports selection of **cluster resolutions** and reordering clusters

- Supports adjusting font size and rotation angle of cluster labels, and
  flipping coordinates

- Supports adjusting the height of group bars

- Supports adjusting the gap size between groups

- Supports adjusting the font size of gene names

- Supports adjusting the height/width ratio of the plot

- Supports downloading plots in PDF format (WYSIWYG)

- Supports assay switching

**Example plots:**

<img src="./inst/extdata/www/heatmap-cell-level.png" alt="" width="100%" />

### Heatmap for group averaged expression

- Supports simultaneous display of multiple genes (gene names are
  case-insensitive). Tip: Paste multiple gene names directly from Excel

- Supports selection of **cluster resolutions** and reordering clusters

- Supports adjusting font size and rotation angle of cluster labels

- Supports adjusting the font size of gene names

- Supports adjusting the height/width ratio of the plot

- Supports downloading plots in PDF format (WYSIWYG)

- Supports switching between assays containing any of the following
  slots: `data`, `scale.data`

**Example plots:**

<img src="./inst/extdata/www/heatmap-group-level.png" alt="" width="100%" />

### Ridge Plot

- Supports simultaneous display of multiple genes (gene names are
  case-insensitive). Tip: Paste multiple gene names directly from Excel

- Supports selection of **cluster resolutions** and reordering clusters

- Supports adjusting the number of columns

- Supports stacking plots and color mapping

- Supports adjusting font sizes on x and y axes

- Supports adjusting the height/width ratio of the plot

- Supports downloading plots in PDF format (WYSIWYG)

- Supports switching between assays containing any of the following
  slots: `counts`, `data`, `scale.data`

**Example plots:**

<img src="./inst/extdata/www/ridge-plot.png" alt="" width="100%" /><img src="./inst/extdata/www/ridge-plot-2.png" alt="" width="100%" />

### Plot Cell Percentage

- Supports faceting

- Supports adjusting the height/width ratio of the plot

- Supports downloading plots in PDF format (WYSIWYG)

**Example plots:**

<img src="./inst/extdata/www/cell-ratio.png" alt="" width="100%" /><img src="./inst/extdata/www/cell-ratio-2.png" alt="" width="100%" />

### Find Cluster Markers and DEGs Analysis

This analysis typically requires more computation time. Please wait
patiently for results to complete. **Important**: Save your results
before starting a new analysis, as previous results will be overwritten.
Results can be downloaded in `csv` format.

#### Two analysis modes supported

- **Find markers for all clusters**: Identifies marker genes for each
  cluster

- **Calculate DEGs for custom groups**: Compare differential expression
  between two user-defined groups. You can subset cells before
  calculating DEGs between groups. By default, all cells from both
  groups are used

You can modify calculation parameters before starting the analysis.

- Supports switching between assays containing any of the following
  slots: `counts`, `data`

**Screen shots:**

<img src="./inst/extdata/www/DEGs-2.png" alt="" width="50%" />

#### Output description

<img src="./inst/extdata/www/DEGs-4.jpg" alt="" width="100%" />

> [FindMarkers(object,
> …)](https://satijalab.org/seurat/reference/findmarkers)
>
> A data.frame with a ranked list of putative markers as rows, and
> associated statistics as columns (p-values, ROC score, etc., depending
> on the test used (test.use)). The following columns are always
> present:
>
> avg_logFC: log fold-chage of the average expression between the two
> groups. Positive values indicate that the gene is more highly
> expressed in the first group
>
> pct.1: The percentage of cells where the gene is detected in the first
> group
>
> pct.2: The percentage of cells where the gene is detected in the
> second group
>
> p_val_adj: Adjusted p-value, based on bonferroni correction using all
> genes in the dataset

### Top Expressed Features

Highly expressed genes can reflect the main functions of cells. There
are two approaches to identifying these genes: `Find Top Genes by Cell`
identifies genes that are highly expressed in individual cells (even if
only in a few cells), while `Find Top Genes by Accumulated UMI counts`
tends to find genes with high accumulated expression across most cells
in a cluster.

- Supports assay switching

#### 1. Find Top Genes by Cell

#### How it works

**Step 1**: For each cell, identify genes with high UMI percentage. For
example, if a cell has 10,000 UMIs and the `UMI percentage cutoff` is
set to 0.01, all genes with more than 10,000 × 0.01 = 100 UMIs are
considered highly expressed in that cell.

**Step 2**: Summarize these genes for each cluster. First, collect all
highly expressed genes in a cluster. For each gene, count the number of
cells where it is highly expressed, and calculate the mean and median
UMI percentage in those highly expressed cells.

<img src="./inst/extdata/www/Find-Top-Genes-by-Cell.jpg" alt="" width="80%" />

#### Output description

- `celltype`: The cluster name as defined by
  `Choose A Cluster Resolution`

- `total.cells`: Total number of cells in this cluster

- `Gene`: Gene that is highly expressed in at least 1 cell in this
  cluster

- `total.pos.cells`: Number of cells that express this gene

- `total.UMI.pct`: (All UMIs of this gene) / (Total UMIs of this
  cluster)

- `cut.Cells`: Number of cells that highly express this gene

- `cut.pct.mean`: Mean expression percentage in highly expressed cells

- `cut.pct.median`: Median expression percentage in highly expressed
  cells

#### 2. Find Top Genes by Mean UMI counts

For each cluster, calculate the `top n` highly expressed genes by mean
UMI counts. Clusters with fewer than 3 cells will be skipped.

- Supports switching between assays containing the `counts` slot

<img src="./inst/extdata/www/Find-Top-Genes-by-Mean-UMI-counts.jpg" alt="" width="80%" />

#### Output description

- `CellType`: The cluster name as defined by
  `Choose A Cluster Resolution`

- `total.cells`: Total number of cells in this cluster

- `Gene`: The `top n` highly expressed genes

- `total.pos.cells`: Number of cells that express this gene

- `MeanUMICounts`: (Total accumulated UMI counts) / (Total cells of this
  cluster)

- `PCT`: (Total accumulated UMI counts of the gene) / (Total UMIs of
  cluster cells)

### Feature Summary

Summarize features of interest by cluster, including positive cell
percentage and mean/median expression levels.

- Supports switching between assays containing the `data` slot

<img src="./inst/extdata/www/gene-short-summary.jpg" alt="" width="80%" />

#### Output description

- `celltype`: The cluster name as defined by
  `Choose A Cluster Resolution`

- `TotalCells`: Total number of cells in this cluster

- `Gene`: The input genes

- `PCT`: The percentage of cells expressing this gene in this cluster

- `Expr.mean`: The mean normalized expression in this cluster

- `Expr.median`: The median normalized expression in this cluster

### Feature Correlation Analysis

Calculates correlation values for gene pairs within cells from a
cluster. Supports both Pearson and Spearman correlation methods.

- Supports switching between assays containing the `data` slot

#### Three analysis modes

- `Find Top Correlated Gene Pairs`: Identifies the top 1000 correlated
  gene pairs

- `Find Correlated Genes for A Gene`: Finds the most correlated genes
  for user-specified genes

- `Calculate Correlation for A Gene List`: Calculates correlation values
  for all pairs in a user-provided gene list

<img src="./inst/extdata/www/featurecorrelation.jpg" alt="" width="100%" />

#### Output description

<img src="./inst/extdata/www/feature-correlation-output.jpg" alt="" width="40%" />

- `GeneA`: The first gene in a gene pair

- `GeneB`: The second gene in a gene pair

- `correlation`: The correlation value

**Note**: If no results are returned, this is typically because the
input genes have very low expression levels. Very lowly expressed genes
are removed before analysis.

### Rename cluster names

You can rename cluster names, and changes will take effect immediately.
However, the original Seurat object file is never modified. Once you
close the session, the new annotations will be lost. You can download a
mapping file of old names to new names and send it to a bioinformatician
to request permanent changes to the Seurat object.

### Search Features

All features (genes) extracted from the row names of the assay can be
searched.

- Supports switching between assays containing any of the following
  slots: `counts`, `data`, `scale.data`
- Case-insensitive search for gene/feature names
- View feature annotations for ATAC assays (requires Signac package)
- Copy multiple feature names for use in other analysis modules

### Cell Metadata

The metadata of all cells extracted from the meta.data slot of the
Seurat object contains descriptive information for each cell, such as
quality control metrics, cell type classifications, batch information,
and experimental conditions. This metadata is crucial for organizing,
filtering, integrating, and visualizing single-cell RNA-seq data.

- Supports downloading cell metadata in `csv` format for further
  analysis

<img src="./inst/extdata/www/cell-metadata.jpg" alt="" width="100%" />

### Structure of Seurat Object

> The Seurat object is an S4 class in R designed to store and manage
> single-cell expression data and associated analyses. It is a highly
> structured and self-contained object, allowing for the integration of
> various data modalities and analytical results.

> Key Slots and their Contents:

> assays: This is a list containing one or more Assay objects. Each
> Assay object represents a specific type of expression data. Each Assay
> object itself contains slots like counts (raw data), data (normalized
> data), scale.data (scaled data), and meta.features (feature-level
> metadata).

> meta.data: A data frame storing cell-level metadata. This includes
> information such as the number of features detected per cell
> (nFeature_RNA), original identity classes (orig.ident), and can be
> extended with additional information (e.g., cell type annotations,
> sample information).

> active.assay: A character string indicating the name of the currently
> active or default assay for analysis.

> active.ident: Stores the active cluster identity for each cell,
> typically resulting from clustering analyses.

> reductions: A list of DimReduc objects, each representing a
> dimensionality reduction technique applied to the data (e.g., PCA,
> UMAP, tSNE). These objects store the lower-dimensional embeddings of
> the cells.

> graphs: A list of Graph objects, typically storing nearest-neighbor
> graphs used in clustering and other analyses. images: For spatial
> transcriptomics data, this slot stores Image objects containing
> spatial image data and information linking spots to their physical
> locations.

> project.name: A character string holding the name of the project.

> misc: A list for storing miscellaneous information not fitting into
> other specific slots.

### About

You’re reading the tutorial right now!

## Data Preparation Tips

### Preparing Your Seurat Object

For the best experience with SeuratExplorer, ensure your Seurat object
contains:

1.  **Required Elements:**
    - At least one assay with data (`RNA`, `ATAC`, etc.)
    - Dimensional reductions (`umap`, `tsne`, or `pca`)
    - Cell metadata (`meta.data` slot) with cluster information
2.  **Recommended Elements:**
    - Multiple cluster resolutions (e.g., `seurat_clusters` at different
      resolutions)
    - Sample or batch information in metadata
    - Cell type annotations (can be added interactively in the app)
    - Quality control metrics (nCount_RNA, nFeature_RNA, percent.mt,
      etc.)
3.  **File Formats:**
    - `.rds`: Standard R data format (smaller files, faster I/O)
    - `.qs2`: QSZ compression format (better compression for large
      objects)
4.  **File Size Considerations:**
    - Maximum upload size: 20GB (configurable via `MaxInputFileSize`
      parameter)
    - For large datasets, consider subsetting or using data with fewer
      cells
    - Use `.qs2` format for better compression of large objects

### Example Preprocessing Code

``` r
# Basic Seurat preprocessing
library(Seurat)

# Load your data
seurat_obj <- Read10X(data.dir = "path/to/data")
seurat_obj <- CreateSeuratObject(counts = seurat_obj)

# Standard processing
seurat_obj <- NormalizeData(seurat_obj)
seurat_obj <- FindVariableFeatures(seurat_obj)
seurat_obj <- ScaleData(seurat_obj)
seurat_obj <- RunPCA(seurat_obj)
seurat_obj <- RunUMAP(seurat_obj, dims = 1:30)

# Clustering at multiple resolutions
seurat_obj <- FindNeighbors(seurat_obj, dims = 1:30)
seurat_obj <- FindClusters(seurat_obj, resolution = 0.4)
seurat_obj <- FindClusters(seurat_obj, resolution = 0.8)

# Save for SeuratExplorer
saveRDS(seurat_obj, "my_seurat_object.rds")
# or
qs2::qs_save(seurat_obj, "my_seurat_object.qs2")
```

## FAQ

**Q: Can I use SeuratExplorer with Seurat v3 objects?**

A: Yes! SeuratExplorer automatically updates old Seurat objects when
loading. However, for very old versions (v2 or earlier), manual update
using `UpdateSeuratObject()` may be required before use.

**Q: Is my data uploaded to any server?**

A: No. When running locally (`launchSeuratExplorer()`), all data stays
on your computer. Only when deployed on a Shiny Server would data be
uploaded to that server.

**Q: Can I save my analysis results?**

A: Yes! Most visualizations can be downloaded as PDF files. Analysis
results (DEGs, feature summaries, etc.) can be downloaded as CSV files.
Cluster name mappings can also be exported.

**Q: What’s the difference between the two “Top Expressed Features”
methods?**

A: - **Find Top Genes by Cell**: Identifies genes that are highly
expressed in individual cells, useful for finding cell-specific
markers - **Find Top Genes by Mean UMI**: Finds genes with high average
expression across all cells in a cluster, useful for identifying cluster
characteristics

**Q: How do I add custom color palettes?**

A: While SeuratExplorer includes many predefined color palettes, you can
use the `getColors()` function in your own R scripts to access these
palettes for custom visualizations.

## Key related packages

### Core Dependencies

- [satijalab/seurat](https://github.com/satijalab/seurat): Seurat is an
  R toolkit for single cell genomics, developed and maintained by the
  Satija Lab at NYGC. SeuratExplorer builds upon Seurat’s powerful
  analysis capabilities to provide interactive visualization.

- [rstudio/shiny](https://shiny.rstudio.com/): Shiny is an R package
  that makes it easy to build interactive web apps straight from R.
  SeuratExplorer uses Shiny to create its interactive dashboard.

### Visualization Dependencies

- [ggplot2](https://ggplot2.tidyverse.org/): A system for declaratively
  creating graphics, based on “The Grammar of Graphics”. SeuratExplorer
  uses ggplot2 for all its visualizations.

- [ComplexHeatmap](https://jokergoo.github.io/ComplexHeatmap/reference/Heatmap.html):
  Bioconductor package for making complex heatmaps with annotations.

- [shinydashboard](https://rstudio.github.io/shinydashboard/): Create
  dashboards with Shiny. SeuratExplorer uses this for its admin-style
  interface.

### Related Projects

- [Hla-Lab/SeuratExplorer](https://github.com/rwcrocker/SeuratExplorer/):
  An interactive R shiny application for exploring scRNAseq data
  processed in Seurat (another implementation with similar goals).

- [junjunlab/scRNAtoolVis](https://github.com/junjunlab/scRNAtoolVis):
  Some useful functions to make your scRNA-seq plots more beautiful.
  Some code from this package has been adapted in SeuratExplorer.

- [rstudio/shiny-server](https://github.com/rstudio/shiny-server): Shiny
  Server is a server program that makes Shiny applications available
  over the web. Use this to deploy SeuratExplorer on a server for
  multi-user access.

## Contributing and Support

### Getting Help

- **Documentation**: See function documentation with `?function_name` in
  R
- **Issues**: Report bugs or request features at [GitHub
  Issues](https://github.com/fentouxungui/SeuratExplorer/issues)
- **WeChat**: Follow
  [微信公众号：分析力工厂](https://mp.weixin.qq.com/s/lpvI9OnyN95amOeVGmeyMQ)
  for tutorials and updates in Chinese

### Citation

If you use SeuratExplorer in your research, please cite:

``` bibtex
@software{seuratexplorer2025,
  title = {SeuratExplorer: An Shiny App for Exploring scRNA-seq Data Processed in Seurat},
  author = {Zhang, Yongchao},
  year = {2025},
  url = {https://github.com/fentouxungui/SeuratExplorer},
  note = {R package version 0.1.3}
}
```

### License

This package is licensed under GPL (\>= 3). See [LICENSE.md](LICENSE.md)
for details.

## Acknowledgments

SeuratExplorer is built upon excellent work by:

- The Seurat development team (Satija Lab)
- The RStudio/Shiny team
- The Bioconductor community
- All contributors and users who provide feedback and suggestions

## 中文介绍

[微信公众号：
分析力工厂](https://mp.weixin.qq.com/s/lpvI9OnyN95amOeVGmeyMQ)

## Session Info

    #> R version 4.4.3 (2025-02-28 ucrt)
    #> Platform: x86_64-w64-mingw32/x64
    #> Running under: Windows 11 x64 (build 26200)
    #> 
    #> Matrix products: default
    #> 
    #> 
    #> locale:
    #> [1] LC_COLLATE=Chinese (Simplified)_China.utf8 
    #> [2] LC_CTYPE=Chinese (Simplified)_China.utf8   
    #> [3] LC_MONETARY=Chinese (Simplified)_China.utf8
    #> [4] LC_NUMERIC=C                               
    #> [5] LC_TIME=Chinese (Simplified)_China.utf8    
    #> 
    #> time zone: Asia/Shanghai
    #> tzcode source: internal
    #> 
    #> attached base packages:
    #> [1] stats     graphics  grDevices utils     datasets  methods   base     
    #> 
    #> loaded via a namespace (and not attached):
    #>  [1] compiler_4.4.3    fastmap_1.2.0     cli_3.6.5         tools_4.4.3      
    #>  [5] htmltools_0.5.9   otel_0.2.0        rstudioapi_0.17.1 yaml_2.3.12      
    #>  [9] rmarkdown_2.30    knitr_1.51        xfun_0.55         digest_0.6.39    
    #> [13] rlang_1.1.6       evaluate_1.0.5
