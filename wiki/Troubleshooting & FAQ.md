# Troubleshooting & FAQ

## FAQ

**Can I use SeuratExplorer with Seurat v3 objects?**
Yes. Old objects are updated automatically when loaded. For very old versions (v2 or earlier) you may need `UpdateSeuratObject()` first.

**Is my data uploaded anywhere?**
No. When running locally (`launchSeuratExplorer()`), everything stays on your computer. Data is only uploaded when you run it on a Shiny Server you control.

**Can I save my results?**
Yes. Most plots download as PDF; tables (DEGs, feature summaries, cell metadata, cluster mappings) download as CSV.

**What is the difference between the two "Top Expressed Features" methods?**
- *Find Top Genes by Cell*: genes highly expressed in individual cells — good for cell-specific markers.
- *Find Top Genes by Accumulated UMI Counts*: genes with high accumulated expression across a cluster — good for cluster-level characteristics.

**How do I use the color palettes elsewhere?**
Use `getColors()` in your own scripts.

## Troubleshooting

**The app opens but "No dimensionality reduction found".**
Your object has no reduction matching `ReductionKeyWords`. Add a `umap`/`tsne`/`pca` reduction, or launch with different keywords, e.g. `launchSeuratExplorer(ReductionKeyWords = c("umap"))`.

**A Feature/Violin/Dot/Heatmap/Ridge page is blank.**
Usually the entered gene is not present in the selected assay, or no cluster is selected. Gene names are case-insensitive but must exist in the chosen assay — check names on the **Search Features** page.

**Uploading a large file fails.**
Increase `MaxInputFileSize` (see [[Launch Options]]), or save the object as `.qs2` for better compression.

**Heatmap / DEG tests error about a missing package.**
Install the optional dependencies from [[Installation]] (`ComplexHeatmap`, `presto`, `MAST`, `limma`, `DESeq2`).

**The demo download seems to stall.**
`raw.githubusercontent.com` is not reachable from some networks. Set a mirror with `launchSeuratExplorer(DemoDataURL = "...")`, or download the file manually. See [[Demo Data]].

**I want to clear the cached demo data.**
See the "Remove the cached file" section in [[Demo Data]].

**I found a bug / want a feature.**
Open an issue at <https://github.com/fentouxungui/SeuratExplorer/issues>.
