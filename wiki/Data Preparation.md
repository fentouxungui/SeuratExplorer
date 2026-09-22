# Data Preparation

SeuratExplorer needs a **processed** Seurat object.

## Required

- At least one **assay** with data (`RNA`, `ATAC`, …).
- A **dimensional reduction** matching the configured keywords (`umap`, `tsne`, `pca` by default). Without one, the Dim Reduction / Feature / Rename-Clusters pages cannot work.
- Cell metadata (`meta.data`), normally including cluster assignments.

## Recommended

- Multiple cluster-resolution columns.
- Sample / batch information.
- QC metrics (`nCount_RNA`, `nFeature_RNA`, `percent.mt`, …).
- Cell-type annotations (these can also be added interactively in the app).

## File formats

- `.rds` — standard R format.
- `.qs2` — better compression for large objects (`qs2::qs_save()`).

Maximum upload size is 20 GB by default (configurable with `MaxInputFileSize`).

## Example preprocessing

```r
library(Seurat)

seurat_obj <- Read10X(data.dir = "path/to/data")
seurat_obj <- CreateSeuratObject(counts = seurat_obj)

seurat_obj <- NormalizeData(seurat_obj)
seurat_obj <- FindVariableFeatures(seurat_obj)
seurat_obj <- ScaleData(seurat_obj)
seurat_obj <- RunPCA(seurat_obj)
seurat_obj <- RunUMAP(seurat_obj, dims = 1:30)

seurat_obj <- FindNeighbors(seurat_obj, dims = 1:30)
seurat_obj <- FindClusters(seurat_obj, resolution = 0.4)
seurat_obj <- FindClusters(seurat_obj, resolution = 0.8)

saveRDS(seurat_obj, "my_seurat_object.rds")
# or
qs2::qs_save(seurat_obj, "my_seurat_object.qs2")
```

## Assays and slots

SeuratExplorer supports assay switching across scRNA-seq, scATAC-seq, spatial, CITE-seq and custom assays. Different features can read different slots:

| Feature | Supported slots |
|---|---|
| Feature Plot | `counts`, `data`, `scale.data` |
| Violin Plot | `counts`, `data`, `scale.data` |
| Dot Plot | `data` |
| Heatmap (Cell Level) | `data`, `scale.data` |
| Heatmap (Group Averaged) | `data` |
| Ridge Plot | `counts`, `data`, `scale.data` |
| DEGs Analysis | `counts`, `data` |
| Top Expressed Features | `counts` |
| Feature Summary | `data` |
| Feature Correlation | `data` |
| Gene Based Cluster | `counts`, `data` |
| Module Score | `counts`, `data` |
