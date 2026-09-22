# Quick Start

## 1. Launch the app

```r
library(SeuratExplorer)
launchSeuratExplorer()
```

The dashboard opens in your default browser. All processing stays on your machine.

## 2. Load data

Open the **Dataset** tab and do one of the following:

- **Choose A rds or qs2 file of Seurat Object** — upload your processed object (default limit 20 GB; see [[Launch Options]]), or
- **Download and Run a demo data** — fetch a small demo dataset and load it automatically (see [[Demo Data]]).

Older Seurat objects are updated automatically on load; this may take a moment for very old objects.

A small **Data Overview** panel shows total cells, genes, clusters and assays once loaded.

## 3. Explore

Once data is loaded, the **Explorer** menu (sidebar) appears with all analysis pages — dimension-reduction plots, expression plots, marker/DEG analysis, annotation tools and more. See [[Interface Guide]].

## 4. Save results

- Figures: **download as PDF** (what-you-see-is-what-you-get) from each plot page.
- Tables: **download as CSV** (DEGs, feature summary, cell metadata, cluster-name mappings, …).
