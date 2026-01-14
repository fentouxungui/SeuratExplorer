
<!-- README.md is generated from README.Rmd. Please edit that file -->

<!-- 如果要生成github主页上的README.md, 需要将此文件复制到R包的主目录下,然后设置for_github参数为TRUE,然后knit,运行完成后删除主目录下的README.Rmd文件 -->

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

> An `Shiny` App for Exploring scRNA-seq Data Processed in `Seurat`

A simple, one-command package which runs an interactive dashboard
capable of common visualizations for single cell RNA-seq.
`SeuratExplorer` requires a processed `Seurat` object, which is saved as
`rds` or `qs2` file.

## Why build this R package

> Currently, there is still no good tools for visualising the analysis
> results from `Seurat`, when the bioinformatics analyst hands over the
> results to the user, if the user does not have any R language
> foundation, it is still difficult to retrieve the results and
> re-analysis on their own, and this R package is designed to help such
> users to visualize and explore the anaysis results. The only thing to
> do for such users is to configure R and Rstudio on their own
> computers, and then install `SeuratExplorer`, without any other
> operations, an optional way is to upload the `Seurat object` file to a
> server which has been deployed with `shinyserver` and
> `SeuratExplorer`.

> Essentially, what `SeuratExplorer` done is just to perform visual
> operations for command line tools from `Seurat` or other packages.

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

## Exported Functions

Besides the interactive Shiny app, `SeuratExplorer` provides several
utility functions that can be used in R scripts:

### Main Functions

- `launchSeuratExplorer()`: Launch the interactive Shiny application
- `getColors()`: Get color palettes for visualization
- `cellRatioPlot()`: Create stacked bar plots showing cell type
  proportions across samples
- `top_genes()`: Find top expressed genes by cell cluster

### UI/Server Components

For advanced users who want to customize the Shiny app:

- `ui()` / `server()`: Main UI and server functions
- `explorer_body_ui()` / `explorer_sidebar_ui()`: Modular UI components
- `explorer_server()`: Modular server function

See function documentation for detailed usage examples.

## Deploy on server

You can deploy this app on a shiny server, which allows people to view
their data on a webpage by uploading the data to server.

A live demo: Upload an Rds or qs2 file, with file size no more than
20GB, to [Demo Site](http://www.nibs.ac.cn:666/SeuratExplorer/). You can
download a mini demo data from
[github](https://github.com/fentouxungui/SeuratExplorerServer/blob/main/inst/extdata/source-data/fly/Rds-file/G101_PC20res04.rds).

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

SeuratExplorer allows for assay switching, thereby multiple data types
can be supported, including:

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

- support `Seurat` object saved as `.rds` or `.qs2` file.

- support data processed by `Seurat` V5 and older versions. it may takes
  a while to update `Seurat` object when loading data.

<img src="./inst/extdata/www/upload-data.png" alt="" width="100%" />

### Dimensional Reduction Plot

- support options for **Dimension Reductions**

- support options for **Cluster Resolution**

- support **split** plots

- support highlight selected clusters

- support adjust the height/width ratio of the plot

- support options for showing **cluster label**

- support adjust label size

- support adjust point size

- support download plot in pdf format, what you see is what you get

**Example plots:**

<img src="./inst/extdata/www/Dimplot-splited.png" alt="" width="80%" />

### Feature Plot

- support display multiple genes simultaneous, genes names are
  case-insensitive. Tips: paste multiple genes from excel

- support options for **Dimension Reductions**

- support **split** plots

- support change colors for the lowest expression and highest expression

- support adjust the height/width ratio of the plot

- support adjust point size

- support download plot in pdf format, what you see is what you get

- support switch Assays which contain any one of the slots: counts,
  data, scale.data

**Example plots:**

<img src="./inst/extdata/www/Featureplot-splited.png" alt="" width="50%" />

### Violin Plot

- support display multiple genes simultaneous, genes names are
  case-insensitive. Tips: paste multiple genes from excel

- support options for **Cluster Resolution**

- support **split** plots

- support **stack** and **flip** plot, and color mapping selection.

- support adjust point size and transparency

- support adjust font size on x and y axis

- support adjust the height/width ratio of the plot

- support download plot in pdf format, what you see is what you get

- support switch Assays which contain any one of the slots: counts,
  data, scale.data

**Example plots:**

<img src="./inst/extdata/www//ViolinPlot-splited-Stack.png" alt="" width="50%" />

### Dot Plot

- support display multiple genes simultaneous, genes names are
  case-insensitive. Tips: paste multiple genes from excel

- support options for **Cluster Resolution** and subset clusters

- support **split** plots

- support cluster clusters

- support rotate axis and flip coordinate

- support adjust point size and transparency

- support adjust font size on x and y axis

- support adjust the height/width ratio of the plot

- support download plot in pdf format, what you see is what you get

- support switch Assays which contain slot: data

**Example plots:**

<img src="./inst/extdata/www/DotPlot-Splited.png" alt="" width="50%" />

### Heatmap for cell level expression

- support display multiple genes simultaneous, genes names are
  case-insensitive. Tips: paste multiple genes from excel

- support options for **Cluster Resolution** and reorder clusters

- support adjust font size and rotation angle of cluster label, and flip
  coordinate

- support adjust the height of group bar

- support adjust the gap size between groups

- support adjust the font size of gene names

- support adjust the height/width ratio of the plot

- support download plot in pdf format, what you see is what you get

- support Assay switch

**Example plots:**

<img src="./inst/extdata/www/Heatmap-CellLevel.png" alt="" width="100%" />

### Heatmap for group averaged expression

- support display multiple genes simultaneous, genes names are
  case-insensitive. Tips: paste multiple genes from excel

- support options for **Cluster Resolution** and reorder clusters

- support adjust font size and rotation angle of cluster label

- support adjust the font size of gene names

- support adjust the height/width ratio of the plot

- support download plot in pdf format, what you see is what you get

- support switch Assays which contain any one of the slots: data,
  scale.data

**Example plots:**

<img src="./inst/extdata/www/Heatmap-GroupLevel-2.png" alt="" width="50%" />

### Ridge Plot

- support display multiple genes simultaneous, genes names are
  case-insensitive. Tips: paste multiple genes from excel

- support options for **Cluster Resolution** and reorder clusters

- support adjust column numbers

- support stack plot and color mapping

- support adjust font size on x and y axis

- support adjust the height/width ratio of the plot

- support download plot in pdf format, what you see is what you get

- support switch Assays which contain any one of the slots: counts,
  data, scale.data

**Example plots:**

<img src="./inst/extdata/www/RidgePlot.png" alt="" width="50%" />

### Plot Cell Percentage

- support facet

- support adjust the height/width ratio of the plot

- support download plot in pdf format, what you see is what you get

**Example plots:**

<img src="./inst/extdata/www/CellRatio-Splited.png" alt="" width="50%" />

### Find Cluster Markers and DEGs Analysis

This usually takes longer, please wait patiently.Please save the results
before start a new analysis, the old results will be overwritten by the
new results, the results can be downloaded as `csv` format.

#### Support two ways

- support find markers for all clusters

- support calculate DEGs for self-defined two groups, you can subset
  cells before calculate DEGs between two groups, default use all cells
  of two groups.

You can modify part calculation parameters before a analysis.

- support switch Assays which contain any one of the slots: counts, data

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

Highly expressed genes can reflect the main functions of cells, there
two ways to do this. the first - `Find Top Genes by Cell` could find
gene only high express in a few cells, while the second -
`Find Top Genes by Accumulated UMI counts` is biased to find the highly
expressed genes in most cells by accumulated UMI counts.

- support Assay switch

#### 1. Find Top Genes by Cell

#### How?

Step1: for each cell, find genes that has high UMI percentage, for
example, if a cell has 10000 UMIs, and the `UMI percentage cutoff` is
set to 0.01, then all genes that has more than 10000 \* 0.01 = 100 UMIs
is thought to be the highly expressed genes for this cell.

Step2: summary those genes for each cluster, firstly get all highly
expressed genes in a cluster, some genes may has less cells, then for
each gene, count cells in which this genes is highly expressed, and also
calculate the mean and median UMI percentage in those highly expressed
cells.

<img src="./inst/extdata/www/Find-Top-Genes-by-Cell.jpg" alt="" width="80%" />

#### Output description

- `celltype`: the cluster name which is define by
  `Choose A Cluster Resolution`

- `total.cells`: total cell in this cluster

- `Gene`: this Gene is highly expressed in at least 1 cell in this
  cluster

- `total.pos.cells`: how many cells express this gene

- `total.UMI.pct`: (all UMIs of this gene)/(total UMIs of this cluster)

- `cut.Cells`: how many cells highly express this gene

- `cut.pct.mean`: in those highly expressed cells, the mean expression
  percentage

- `cut.pct.median`: in those highly expressed cells, the median
  expression percentage

#### 2. Find Top Genes by Mean UMI counts

for each cluster, calculate the `top n` highly expressed genes by Mean
UMI counts. if a cluster has less than 3 cells, this cluster will be
escaped.

- support switch Assays which contain slot: counts

<img src="./inst/extdata/www/Find-Top-Genes-by-Mean-UMI-counts.jpg" alt="" width="80%" />

#### Output description

- `CellType`: the cluster name which is define by
  `Choose A Cluster Resolution`

- `total.cells`: total cell in this cluster

- `Gene`: the `top n` highly expressed genes

- `total.pos.cells`: how many cells express this gene

- `MeanUMICounts`: (total accumulated UMI counts) / (total cells of this
  cluster)

- `PCT`: (total accumulated UMI counts of the gene) / (total UMIs of
  cluster cells)

### Feature Summary

Summary interested features by cluster, such as the positive cell
percentage and mean/median expression level.

- support switch Assays which contain slot: data

<img src="./inst/extdata/www/gene-short-summary.jpg" alt="" width="80%" />

#### Output description

- `celltype`: the cluster name which is define by
  `Choose A Cluster Resolution`

- `TotalCells`: total cell in this cluster

- `Gene`: the input genes

- `PCT`: the percentage of how many cells express this gene in this
  cluster

- `Expr.mean`: the mean normalized expression in this cluster

- `Expr.median`: the median normalized expression in this cluster

### Feature Correlation Analysis

Can calculate the correlation value of gene pairs within cells from a
cluster, support pearson & spearman methods.

- support switch Assays which contain slot: data

#### 3 ways to do

- `Find Top Correlated Gene Pairs`: to find top 1000 correlated gene
  pairs

- `Find Correlated Genes for A Gene`: to find the most correlated genes
  for input genes

- `Calculate Correlation for A Gene List`: to calculate the correlation
  value for each pair of the input genes

<img src="./inst/extdata/www/featurecorrelation.jpg" alt="" width="100%" />

#### Output description

<img src="./inst/extdata/www/feature-correlation-output.jpg" alt="" width="40%" />

- `GeneA`: the first gene in a Gene pair

- `GeneB`: the second gene in a Gene pair

- `correlation`: the correlation value

if nothing return, this is because the input genes has very low
expression level, very low expressed genes will be removed before
analysis.

### Rename cluster name

You can rename cluster names, and changes will take effect immediately,
while the raw Seurat object file will be never changed, once you close
the session, the newly added annotation will be lost, You can download
the old name and new name mapping file, and send it to technician to ask
for permanent change.

### Search Features

all features(genes) extracted from the row names of assay, can be used
search features.

- support switch Assays which contain any one of the slots: counts,
  data, scale.data
- case-insensitive search for gene/feature names
- view feature annotations for ATAC assays (requires Signac package)
- copy multiple feature names for use in other analysis modules

### Cell Metadata

The metadata of all cells extracted from the meta.data slot of Seurat
object, which contains descriptive information for each cell, such as
quality control metrics, cell type classifications, batch information,
and experimental conditions. This metadata is crucial for organizing,
filtering, integrating, and visualizing single-cell RNA-seq data.

- support download cell metadata in `csv` format, which can be used for
  further analysis.

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

Yeah, it’s the tutorial right here!

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
