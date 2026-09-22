# Interface Guide

## Dataset tab

- **Upload Data** — upload a `.rds` / `.qs2` Seurat object, or click **Download and Run a demo data**.
- **Data Overview** — total cells, genes, clusters and assays.

## Explorer menu

Available after data is loaded:

| Page | What it does |
|---|---|
| **Dim Reduction Plot** | Visualize cells in UMAP / t-SNE / PCA, colored by cluster or split by metadata. |
| **Feature Plot** | Overlay gene expression on a dimensionality-reduction plot. |
| **Violin Plot** | Compare gene-expression distributions across clusters. |
| **Dot Plot** | Expression percentage and average level across clusters (dot matrix). |
| **Heatmap Cell Level** | Single-cell gene-expression heatmap. |
| **Heatmap Group Averaged** | Heatmap of per-cluster averaged expression. |
| **Ridge Plot** | Expression density ridges across clusters. |
| **Cell Percentage Plot** | Cell-type proportion across samples/groups. |
| **DEGs Analysis** | Cluster markers, or differential genes between two custom groups. |
| **Top Expressed Features** | Highly expressed genes per cluster (by cell or by accumulated UMI). |
| **Feature Summary** | Mean / median / percentage expression per cluster. |
| **Feature Correlation** | Gene-gene expression correlations. |
| **Rename Clusters** | Interactively rename cluster labels (session-only). |
| **Gene Based Cluster** (BETA) | Group cells by a gene's expression with custom cutoffs. |
| **Combination Based Cluster** (BETA) | Combine two cluster annotations into a new grouping. |
| **Module Score** (BETA) | Score cells with a gene set via `AddModuleScore`. |
| **Search Features** | Search/browse feature names across assays; ATAC annotations with Signac. |
| **Cells Metadata** | Browse and download cell-level metadata (CSV). |
| **Object Structure** | Inspect the structure of the Seurat object. |
| **About** | In-app information. |

Once data is loaded, a **Settings** entry also appears (used by the SeuratExplorerServer deployment).

## Notes on interactive annotation

Rename Clusters, Gene Based Cluster, Combination Based Cluster and Module Score add new metadata columns **in the current session only** — the original file is never modified. Downloadable mapping files let a bioinformatician apply changes permanently.
