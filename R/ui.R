# ui.R
## the UI side

#' some menu items of the dashboard sidebar
#' @description
#' to generate some menu items for the dashboard, which can be integrated to other
#' packages, such as 'fentouxungui/SeuratExplorerServer' from github.
#'
#' @import shiny markdown
#' @import shinyBS
#' @importFrom shinydashboard menuItem menuSubItem
#' @importFrom shinyjs useShinyjs
#' @export
#' @return return some menu items for the dashboard
#' @examples
#' explorer_sidebar_ui()
#'
explorer_sidebar_ui <- function(){
  # to make shinyBS::updateCollapse() runs correctly, refer to: https://github.com/ebailey78/shinyBS/issues/92
  shiny::addResourcePath("sbs", system.file("www", package="shinyBS"))

  # Thin divider between menu groups. It is a plain <li> with no height of its
  # own (only the 1px border), so the spacing of the surrounding items is
  # unchanged; the side margins inset the line from the menu edges.
  menu_divider <- function(){
    tags$li(
      style = paste0("list-style: none; height: 0; margin: 0 12px; padding: 0; ",
                     "border-top: 1px solid rgba(255, 255, 255, 0.15);")
    )
  }

  # Tooltip texts for each menu item (hover to show)
  tooltips <- c(
    dimplot = "Visualize cells in reduced dimensions (UMAP / t-SNE / PCA), colored by cluster or split by metadata.",
    featureplot = "Overlay gene expression values on a dimensionality reduction plot.",
    vlnplot = "Compare gene expression distributions across clusters.",
    dotplot = "Show expression percentage and average level across clusters in a dot matrix.",
    heatmap = "Gene expression heatmap at single-cell level.",
    averagedheatmap = "Gene expression heatmap averaged per cluster.",
    ridgeplot = "Gene expression density ridges across clusters.",
    cellratioplot = "Cell-type proportion across samples or groups.",
    degs = "Find marker genes per cluster or differential genes between two groups.",
    topgenes = "Find highly expressed genes per cluster.",
    featuresummary = "Summarize gene expression (mean / median / percentage) per cluster.",
    featurecorrelation = "Calculate gene-gene expression correlations.",
    renameclusters = "Interactively rename cluster labels.",
    reorderclusters = "Reorder the identity levels of a cluster resolution without changing cell assignments.",
    clustercolors = "Assign custom colors to the groups of a cluster resolution and share them across plots.",
    geneclusters = "Group cells by a gene's expression level with custom cutoffs.",
    combinedclusters = "Combine two cluster annotations into a new grouping.",
    clustercells = "Run FindNeighbors and FindClusters on a chosen reduction and save the result as a new metadata column.",
    addmodulescore = "Score cells with a gene set using AddModuleScore (Seurat) or AUCell.",
    featuresdf = "Search and browse feature / gene names across assays.",
    cellmetadata = "Browse and download cell-level metadata.",
    objectstructure = "Inspect the structure of the Seurat object.",
    exportdata = "Export the current object (with in-session annotations), optionally subset by cluster metadata."
  )

  conditionalPanel(
    condition = "output.file_loaded",
    sidebarMenu(menuItem(text = "Explorer", tabName = "explorer", icon = shiny::icon("dashboard"), startExpanded = TRUE,
                         menuSubItem(text = span(title = tooltips[["dimplot"]], "Dim Reduction Plot"), tabName = "dimplot", icon = shiny::icon("angle-right")),
                         menuSubItem(text = span(title = tooltips[["featureplot"]], "Feature Plot"), tabName = "featureplot", icon = shiny::icon("angle-right")),
                         menuSubItem(text = span(title = tooltips[["vlnplot"]], "Violin Plot"), tabName = "vlnplot", icon = shiny::icon("angle-right")),
                         menuSubItem(text = span(title = tooltips[["dotplot"]], "Dot Plot"), tabName = "dotplot", icon = shiny::icon("angle-right")),
                         menuSubItem(text = span(title = tooltips[["heatmap"]], "Heatmap Cell Level"), tabName = "heatmap", icon = shiny::icon("angle-right")),
                         menuSubItem(text = span(title = tooltips[["averagedheatmap"]], "Heatmap Group Averaged"), tabName = "averagedheatmap", icon = shiny::icon("angle-right")),
                         menuSubItem(text = span(title = tooltips[["ridgeplot"]], "Ridge Plot"), tabName = "ridgeplot", icon = shiny::icon("angle-right")),
                         menuSubItem(text = span(title = tooltips[["cellratioplot"]], "Cell Percentage Plot"), tabName = "cellratioplot", icon = shiny::icon("angle-right")),
                         menu_divider(),
                         menuSubItem(text = span(title = tooltips[["degs"]], "DEGs Analysis"), tabName = "degs", icon = shiny::icon("angle-right")),
                         menuSubItem(text = span(title = tooltips[["topgenes"]], "Top Expressed Features"), tabName = "topgenes", icon = shiny::icon("angle-right")),
                         menuSubItem(text = span(title = tooltips[["featuresummary"]], "Feature Summary"), tabName = "featuresummary", icon = shiny::icon("angle-right")),
                         menuSubItem(text = span(title = tooltips[["featurecorrelation"]], "Feature Correlation"), tabName = "featurecorrelation", icon = shiny::icon("angle-right")),
                         menuSubItem(text = span(title = tooltips[["addmodulescore"]], "Module Score", tags$small(class = "label label-info", title = "This feature is still under validation", style = "margin-left: 4px; font-size: 8px; padding: 1px 3px;", "BETA")), tabName = "addmodulescore", icon = shiny::icon("angle-right")),
                         menu_divider(),
                         menuSubItem(text = span(title = tooltips[["clustercells"]], "Cluster Cells"), tabName = "clustercells", icon = shiny::icon("angle-right")),
                         menuSubItem(text = span(title = tooltips[["renameclusters"]], "Rename Clusters"), tabName = "renameclusters", icon = shiny::icon("angle-right")),
                         menuSubItem(text = span(title = tooltips[["reorderclusters"]], "Reorder Cluster Levels"), tabName = "reorderclusters", icon = shiny::icon("angle-right")),
                         menuSubItem(text = span(title = tooltips[["clustercolors"]], "Cluster Colors"), tabName = "clustercolors", icon = shiny::icon("angle-right")),
                         menuSubItem(text = span(title = tooltips[["geneclusters"]], "Gene Based Cluster", tags$small(class = "label label-info", title = "This feature is still under validation", style = "margin-left: 4px; font-size: 8px; padding: 1px 3px;", "BETA")), tabName = "geneclusters", icon = shiny::icon("angle-right")),
                         menuSubItem(text = span(title = tooltips[["combinedclusters"]], "Combination Based Cluster", tags$small(class = "label label-info", title = "This feature is still under validation", style = "margin-left: 4px; font-size: 8px; padding: 1px 3px;", "BETA")), tabName = "combinedclusters", icon = shiny::icon("angle-right")),
                         menu_divider(),
                         menuSubItem(text = span(title = tooltips[["featuresdf"]], "Search Features"), tabName = "featuresdf", icon = shiny::icon("angle-right")),
                         menuSubItem(text = span(title = tooltips[["cellmetadata"]], "Cell Metadata"), tabName = "cellmetadata", icon = shiny::icon("angle-right")),
                         menuSubItem(text = span(title = tooltips[["objectstructure"]], "Object Structure"), tabName = "objectstructure", icon = shiny::icon("angle-right")),
                         if (isTRUE(getOption("SeuratExplorerEnableObjectExport", TRUE))) {
                           menuSubItem(text = span(title = tooltips[["exportdata"]], "Export Data"), tabName = "exportdata", icon = shiny::icon("angle-right"))
                         },
                         menuSubItem(text = "About", tabName = "about", icon = shiny::icon("angle-right"))
                         )
                )
    )
}

# One-line explanations shown as native tooltips on parameter labels, keyed by
# input id. Only ids present on the rendered page get a tooltip, so entries can
# be added page by page without touching the controls themselves.
param_hints <- function() {
  c(
    DimDimensionReduction = "Embedding used to lay out the cells (UMAP, t-SNE or PCA).",
    DimClusterResolution = "Metadata column that defines the groups used to colour and label the cells.",
    DimShowLabel = "Print each group name at the centre of its cells.",
    DimShowLegend = "Show the colour legend next to the plot.",
    DimLabelSize = "Font size of the group labels.",
    DimPointSize = "Size of each cell's point. Smaller values keep dense embeddings readable.",
    DimSplit = "Draw one panel per level of this metadata column.",
    DimHighlightedClusters = "Colour only these groups; the other cells are drawn in grey.",
    DimPlotMode = "Let SeuratExplorer size the plotting area to the window. Turn off to set the width and height yourself.",
    DimPlotHWRatio = "Height of the plot relative to its width when the plotting area is sized automatically.",
    FeatureGeneSymbol = "One gene per line (or comma-separated); each gene is drawn on the embedding.",
    FeatureDimensionReduction = "Embedding used to lay out the cells.",
    FeatureAssay = "Assay that supplies the expression values.",
    FeatureSlot = "Slot read from the assay; data holds normalized expression.",
    FeatureClusterResolution = "Grouping used for the cluster labels.",
    FeatureSplit = "Draw one panel per level of this metadata column.",
    FeatureShowLabel = "Print cluster names at the centre of each group.",
    FeatureLabelSize = "Font size of the cluster labels.",
    FeaturePlotLowestExprColor = "Colour for cells with the lowest expression.",
    FeaturePlotHighestExprColor = "Colour for cells with the highest expression.",
    FeaturePointAlpha = "Point opacity; lower it when dense regions overlap.",
    FeatureMinCutoff = "Hide cells below this expression percentile.",
    FeatureMaxCutoff = "Hide cells above this expression percentile.",
    FeaturePointSize = "Size of each cell's point.",
    FeaturePlotMode = "Let SeuratExplorer size the plot to the window; turn off to set width and height.",
    VlnGeneSymbol = "One gene per line (or comma-separated).",
    VlnAssay = "Assay that supplies the expression values.",
    VlnSlot = "Slot read from the assay; data holds normalized expression.",
    VlnSplitBy = "Draw one panel per level of this metadata column.",
    VlnClusterResolution = "Grouping shown along the x-axis.",
    VlnIdentsSelected = "Which levels of that grouping to include.",
    VlnSplitPlot = "Split each violin in two by the Split by variable (two-level variables only).",
    VlnStackPlot = "Stack the violins of several genes in one panel.",
    VlnFlipPlot = "Swap the x and y axes.",
    VlnFillBy = "Colour the stacked violins by gene or by cluster.",
    VlnPointSize = "Size of the jittered cells on each violin; 0 hides them.",
    VlnPointAlpha = "Opacity of the jittered cells.",
    VlnPlotMode = "Let SeuratExplorer size the plot to the window.",
    DotGeneSymbol = "One gene per line (or comma-separated).",
    DotAssay = "Assay that supplies the expression values (data slot only).",
    DotSplitBy = "Draw one panel per level of this metadata column.",
    DotClusterResolution = "Grouping shown along the x-axis.",
    DotIdentsSelected = "Which levels of that grouping to include.",
    DotClusterIdents = "Order the x-axis groups by hierarchical clustering.",
    IntraClusterDotplotSubsetCells = "Keep only cells matching these conditions before plotting.",
    DotRotateAxis = "Rotate x-axis labels for long names.",
    DotFlipCoordinate = "Swap the x and y axes.",
    DotPlotLowestExprColor = "Colour for the lowest average expression.",
    DotPlotHighestExprColor = "Colour for the highest average expression.",
    DotDotScale = "Overall size of the dots.",
    DotPlotMode = "Let SeuratExplorer size the plot to the window.",
    HeatmapGeneSymbol = "One gene per line (or comma-separated).",
    HeatmapAssay = "Assay that supplies the expression values.",
    HeatmapSlot = "Slot read from the assay; data or scale.data.",
    HeatmapClusterResolution = "Grouping shown across the columns.",
    HeatmapIdentsSelected = "Which levels of that grouping to include.",
    HeatmapTextSize = "Font size of the cluster labels.",
    HeatmapTextHjust = "Horizontal adjustment of the cluster labels.",
    HeatmapTextVjust = "Vertical adjustment of the cluster labels.",
    HeatmapTextRatateAngle = "Rotation of the cluster labels, in degrees.",
    HeatmapGroupBarHeight = "Height of the colour bar that marks the groups.",
    HeatmapLineWidth = "Width of the lines drawn between cells.",
    HeatmapFeatureTextSize = "Font size of the gene labels.",
    HeatmapPlotMode = "Let SeuratExplorer size the plot to the window.",
    AveragedHeatmapGeneSymbol = "One gene per line (or comma-separated).",
    AveragedHeatmapAssay = "Assay that supplies the expression values.",
    AveragedHeatmapClusterResolution = "Grouping whose average expression is shown.",
    AveragedHeatmapIdentsSelected = "Which levels of that grouping to include.",
    AveragedHeatmapClusterClusters = "Order the rows by hierarchical clustering.",
    AveragedHeatmapClusterFeatures = "Order the columns by hierarchical clustering.",
    AveragedHeatmapClusterTextSize = "Font size of the cluster labels.",
    AveragedHeatmapClusterTextRatateAngle = "Rotation of the cluster labels, in degrees.",
    AveragedHeatmapFeatureTextSize = "Font size of the gene labels.",
    AveragedHeatmapPlotMode = "Let SeuratExplorer size the plot to the window.",
    RidgeplotGeneSymbol = "One gene per line (or comma-separated).",
    RidgeplotAssay = "Assay that supplies the expression values.",
    RidgeplotSlot = "Slot read from the assay; data holds normalized expression.",
    RidgeplotClusterResolution = "Grouping shown along the y-axis.",
    RidgeplotIdentsSelected = "Which levels of that grouping to include.",
    RidgeplotNumberOfColumns = "Number of panels per row when several genes are plotted.",
    RidgeplotStackPlot = "Overlay all genes as separate ridges per group.",
    RidgeplotFillBy = "Colour the stacked ridges by gene or by cluster.",
    RidgeplotPlotMode = "Let SeuratExplorer size the plot to the window.",
    CellratioFillChoice = "Metadata column whose levels are stacked as colours.",
    CellratioXChoice = "Metadata column shown along the x-axis.",
    CellratioFacetChoice = "Split the plot into one panel per level of this column.",
    CellratioSubsetCells = "Keep only cells matching these conditions before computing percentages.",
    CellratioShowSampleSize = "Print the number of cells above each x-axis group.",
    CellratioShowPercent = "Print percentages inside the stacked segments.",
    CellratioPercentCutoff = "Hide labels for segments smaller than this percentage.",
    CellratioPercentDigits = "Number of decimal places in the printed percentages.",
    CellratioPercentListRare = "List the segments hidden by the cutoff below the plot.",
    CellratioColumnWidth = "Width of each stacked column.",
    CellratioFlowAlpha = "Opacity of the flows between columns.",
    CellratioFlowCurve = "Curvature of the flows between columns.",
    CellratioMode = "Let SeuratExplorer size the plot to the window.",
    FeaturePlotHWRatio = "Height of the plot relative to its width when the plotting area is sized automatically.",
    VlnShowLegend = "Show the colour legend.",
    VlnLegendNcol = "Number of columns used to lay out the legend.",
    VlnXlabelSize = "Font size of the x-axis title.",
    VlnYlabelSize = "Font size of the y-axis title.",
    VlnPlotHWRatio = "Height of the plot relative to its width when the plotting area is sized automatically.",
    DotXlabelSize = "Font size of the x-axis title.",
    DotYlabelSize = "Font size of the y-axis title.",
    DotPlotHWRatio = "Height of the plot relative to its width when the plotting area is sized automatically.",
    IntraClusterDotplotSubsetCellsSelectedClusters = "Levels of the selected column to keep.",
    HeatmapPlotHWRatio = "Height of the plot relative to its width when the plotting area is sized automatically.",
    AveragedHeatmapPlotHWRatio = "Height of the plot relative to its width when the plotting area is sized automatically.",
    RidgeplotXlabelSize = "Font size of the x-axis title.",
    RidgeplotYlabelSize = "Font size of the y-axis title.",
    RidgeplotHWRatio = "Height of the plot relative to its width when the plotting area is sized automatically.",
    CellratioShowLegend = "Show the colour legend.",
    CellratioLegendNcol = "Number of columns used to lay out the legend.",
    CellratioRotateAxis = "Rotate x-axis labels for long names.",
    CellratioSubsetCellsSelectedClusters = "Levels of the selected column to keep.",
    CellratioPercentSize = "Font size of the percentage labels.",
    CellratioPercentColor = "Colour of the percentage labels; Automatic picks a contrasting colour.",
    CellratioPlotHWRatio = "Height of the plot relative to its width when the plotting area is sized automatically.",
    ClusterMarkersClusterResolution = "Grouping used to define the clusters whose markers are computed.",
    DEGsAssay = "Assay used for the differential expression test.",
    logfcthreshold = "Minimum log fold change for a gene to be reported.",
    testuse = "Statistical test passed to Seurat (wilcox, t, negbinom, MAST, DESeq2...).",
    minpct = "Gene must be expressed in at least this fraction of cells in either group.",
    mindiffpct = "Minimum difference in the fraction of expressing cells between the two groups.",
    IntraClusterDEGsCustomizedGroups = "Metadata column that defines the groups being compared.",
    IntraClusterDEGsCustomizedGroupsCase = "Level or levels treated as the case group (positive log fold change).",
    IntraClusterDEGsCustomizedGroupsControl = "Level or levels treated as the control group.",
    TopGenesClusterResolution = "Grouping used to define the cell groups.",
    TopGenesSelectedClusters = "Which groups to include in the analysis.",
    TopGenesAssay = "Assay whose counts are used.",
    TopGenesClusterLevel = "Compute one result per group instead of pooling all selected cells.",
    TopGenesTopPercent = "Keep genes whose UMI share in a cell is above this percentage.",
    TopGenesTopN = "Return the N genes with the highest accumulated UMI counts.",
    FeatureSummarySymbol = "One gene per line (or comma-separated).",
    FeatureSummaryClusterResolution = "Grouping used to summarize expression.",
    FeatureSummarySelectedClusters = "Which groups to include in the summary.",
    FeatureSummaryAssay = "Assay whose normalized expression is summarized.",
    FeatureSummaryClusterLevel = "Summarize per group instead of pooling all selected cells.",
    FeatureCorrelationClusterResolution = "Grouping used to select the cells.",
    FeatureCorrelationIdentsSelected = "Which groups to include.",
    FeatureCorrelationAssay = "Assay whose normalized expression is used.",
    correlationmethod = "Correlation coefficient: Pearson (linear) or Spearman (rank based).",
    correlationTopN = "Number of top gene pairs to return.",
    MostCorrelatedAGene = "Gene whose most correlated partners are returned.",
    CorrelationGeneList = "Gene list used to compute all pairwise correlations.",
    ModuleScoreFeatures = "Gene set to score; one gene per line or comma-separated.",
    ModuleScoreAssay = "Assay used to compute the score.",
    ModuleScoreMethod = "AddModuleScore (Seurat) or AUCell (ranking based).",
    ModuleScoreName = "Name of the metadata column that will store the score.",
    ModuleScoreAucMaxRank = "Share of top-ranked genes used for the AUC (AUCell default 5 percent).",
    TopGenesMethod = "Choose how genes are ranked; hover each option for details.",
    renameclustersClusterResolution = "Grouping whose levels are being renamed.",
    renameclustersDimensionReduction = "Embedding used for the preview plot.",
    renameclustersLabelSize = "Font size of the labels in the preview plot.",
    renameclustersNewClusterName = "Name of the new metadata column that stores the renamed groups.",
    reorderclustersClusterResolution = "Grouping whose level order is being changed.",
    reorderclustersDimensionReduction = "Embedding used for the preview plot.",
    reorderclustersLabelSize = "Font size of the labels in the preview plot.",
    ClusterColorPalette = "Apply a preset palette as a starting point; individual colors can be adjusted afterwards.",
    ClusterColorShowLabels = "Print cluster names in the preview.",
    ClusterColorLabelSize = "Font size of the labels in the preview.",
    ClusterColorImportFile = "CSV with two columns, cluster and color, to reuse a palette.",
    geneclustersGeneSymbol = "Gene or metadata column whose values are split into groups.",
    GeneclustersAssay = "Assay used to read the gene expression values.",
    geneclustersSlot = "Slot read from the assay.",
    geneclustersCutoffs = "Comma-separated cutoffs that split the expression range into groups.",
    CombinedClusterResolution = "First cluster resolution to combine.",
    CombinedClusterResolutionSecond = "Second cluster resolution to combine.",
    CombinedClusterConnector = "Text placed between the two labels when they are joined.",
    FeaturesDataframeAssay = "Assay whose feature table is shown.",
    ObjectStrutureLevel = "How many levels of the object structure to print.",
    ExportPreviewGroup = "Metadata column used to group the preview table.",
    ExportKeepNA = "Keep cells whose value is NA in a filter column.",
    ExportFormat = "File format of the exported object: qs2 (faster, smaller) or rds.",
    ExportRdsCompress = "Compression used for the rds file.",
    ExportQsLevel = "Compression level for qs2; 0 is the fastest and largest.",
    ExportDropScaleData = "Remove the scale.data slot to shrink the file.",
    ExportDroplevels = "Drop unused factor levels from metadata columns.",
    ExportWriteProvenance = "Record the export settings in obj@misc.",
    ClusterCellsReduction = "Embedding used to build the neighbour graph (usually PCA).",
    ClusterCellsDims = "Dimensions of that embedding used for the graph.",
    ClusterCellsAssay = "Assay passed to FindNeighbors; it only names the graph.",
    ClusterCellsK = "Nearest neighbours per cell (Seurat default 20).",
    ClusterCellsPrune = "SNN edges weaker than this are dropped (Seurat default 1/15).",
    ClusterCellsNNMethod = "annoy ships with Seurat; rann needs the RANN package.",
    ClusterCellsMetric = "Distance metric used by annoy.",
    ClusterCellsTrees = "Number of annoy trees; more is slower and slightly more accurate.",
    ClusterCellsResolution = "Higher values give more, smaller clusters.",
    ClusterCellsAlgorithm = "1 Louvain, 2 Louvain refined, 3 SLM, 4 Leiden (needs leidenalg).",
    ClusterCellsSeed = "Random seed, so the same settings give the same clusters.",
    ClusterCellsShowLabel = "Print cluster names on the preview plot.",
    ClusterCellsName = "Metadata column that will store the new clusters; saved only on Save.",
    ClusterCellsPreviewReduction = "Embedding used only for the preview plot; the clustering itself is unchanged."
  )
}

# Render the hint vector as a single-quoted JavaScript object literal.
param_hints_js <- function(hints) {
  esc <- function(x) gsub("'", "\\\\'", x)
  paste0("{", paste0("'", names(hints), "':'", esc(unname(hints)), "'", collapse = ","), "}")
}

# Tooltips for individual options of a widget that renders its choices as
# buttons or tabs. Keyed by the container element id, then by the exact visible
# option text.
param_option_hints <- function() {
  list(
    TopGenesMethod = c(
      "By cell" = "For every cell, keep the genes above the UMI share cutoff, then summarise them per group. Answers what each cell is made of.",
      "By accumulated UMI counts" = "For every group, rank genes by their total UMI counts and return the top N. Answers which genes dominate each group."
    ),
    "degstabbox-container" = c(
      "Find Markers for All Clusters" = "Compare every group against all other cells and report the marker genes of each group.",
      "Find DEGs for two groups" = "Compare two groups you define. Cells can be filtered before the comparison.",
      "Custom Parameters" = "Assay, statistical test, log fold change and expression thresholds used by both analyses."
    ),
    "featurecorrelationtabbox-container" = c(
      "Top correlated pairs" = "Return the top N correlated gene pairs; set N on the right.",
      "Correlated with one gene" = "Find the genes most correlated with the gene you enter.",
      "All pairs in a gene list" = "Compute the correlation for every pair in a gene list you provide."
    )
  )
}

# Render the option-hint list as a nested JavaScript object literal.
param_option_hints_js <- function(option_hints) {
  esc <- function(x) gsub("'", "\\\\'", x)
  parts <- vapply(names(option_hints), function(id) {
    m <- option_hints[[id]]
    paste0("'", id, "':{",
           paste0("'", names(m), "':'", esc(unname(m)), "'", collapse = ","),
           "}")
  }, character(1))
  paste0("{", paste0(parts, collapse = ","), "}")
}

#' generate the body UI for each menu item specified in `explorer_sidebar_ui`
#' @param tab_list a tag list for the body UI of shiny dashboard
#' @import shinydashboard
#' @import shiny shinyWidgets shinyBS knitr
#' @importFrom shinydashboard tabItem
#' @importFrom shinycssloaders withSpinner
#' @importFrom shinyBS bsCollapse bsCollapsePanel
#' @export
#' @return a filled tag list for body UI
#' @examples
#' tab_list <- list()
#' tab_list <- explorer_body_ui(tab_list = tab_list)
#'
explorer_body_ui <- function(tab_list){
  tab_list[["dimplot"]] = tabItem(tabName = "dimplot",
                                  div(
                                    class = "se-page-hint",
                                    icon("circle-info"),
                                    tags$span("The plot updates automatically as you change the settings. Hover over a parameter label to see what it does.")
                                  ),
                                  fluidRow(
                                    box(title = "Dimensional Reduction Plot",
                                        div(id = "dimplot_plot_container",
                                            uiOutput("dimplot_resizable_ui")),
                                        # show the button on right end, refer to: https://stackoverflow.com/questions/28749693/shiny-r-aligning-buttons
                                        div(style = "display:inline-block; float:right",downloadButton("downloaddimplot", label = "Download PDF", class = "btn-default btn-action")),
                                        width = 9, status = "primary", collapsible = TRUE, solidHeader = TRUE),
                                    box(title = div(
                                          icon("sliders-h"),
                                          strong(" Plot Settings"),
                                          style = "display: flex; align-items: center; gap: 8px; color: white;"
                                        ),
                                        solidHeader = TRUE, status = "primary", width = 3,
                                        # Data Selection Group
                                        param_group_collapse(
                                          id = "dimplot_data", title = "Data Selection",
                                          icon_name = "database", color = "#3b82f6",
                                          subtitle = "Choose the embedding and the grouping used to colour the cells.",
                                          withSpinner(uiOutput("DimDimensionReduction.UI"), proxy.height = "10px"),
                                          withSpinner(uiOutput("DimClusterResolution.UI"), proxy.height = "10px")
                                        ),
                                        # Display Options Group
                                        param_group_collapse(
                                          id = "dimplot_display", title = "Display Options",
                                          icon_name = "eye", color = "#10b981",
                                          subtitle = "Turn the group labels and the legend on or off.",
                                          checkboxInput("DimShowLabel",label = "Show cluster label", TRUE),
                                          checkboxInput("DimShowLegend",label = "Show legend", TRUE)
                                        ),
                                        # Size Settings Group
                                        param_group_collapse(
                                          id = "dimplot_size", title = "Size Settings",
                                          icon_name = "adjust", color = "#f59e0b",
                                          subtitle = "Text and point sizes used in the plot.",
                                          sliderInput("DimLabelSize", label = "Label Size:", min = 0, max = 20, value = 7),
                                          sliderInput("DimPointSize", label = "Point Size:", min = 0.001, max = 2, value = 0.8)
                                        ),
                                        # Advanced Options
                                        param_group_collapse(
                                          id = "dimplot_advanced", title = "Advanced Options",
                                          icon_name = "cogs", color = "#6b7280",
                                          subtitle = "Split the plot into panels, highlight selected groups, and control the plotting area.",
                                          withSpinner(uiOutput("DimSplit.UI"), proxy.height = "10px"),
                                          withSpinner(uiOutput("DimHighlightedClusters.UI"), proxy.height = "10px"),
                                          uiOutput("DimManyClustersHint.UI"),
                                          checkboxInput("DimPlotMode",label = "Automatically adjust plotting area", TRUE),
                                          uiOutput("dimplot_size_ui"),
                                          p("Colors can be set in Cluster Colors (sidebar).",
                                            style = "font-size: 11px; color: #5C636A; margin: 6px 0 0 0;")
                                        )
                                    )
                                  )
  )
  tab_list[["featureplot"]] = tabItem(tabName = "featureplot",
                                      div(
                                        class = "se-page-hint",
                                        icon("circle-info"),
                                        tags$span("The plot updates automatically as you change the settings. Hover over a parameter label to see what it does.")
                                      ),
                                      fluidRow(
                                        box(title = "Features on Dimensional Reduction Plot",
                                            uiOutput("featureplot_resizable_ui"),
                                            div(style = "display:inline-block; float:right", downloadButton("downloadfeatureplot", label = "Download PDF", class = "btn-default btn-action")),
                                            width = 9, status = "primary", collapsible = TRUE, solidHeader = TRUE),
                                        box(title = div(
                                              icon("sliders-h"),
                                              strong(" Plot Settings"),
                                              style = "display: flex; align-items: center; gap: 8px; color: white;"
                                            ),
                                            solidHeader = TRUE, status = "primary", width = 3,
                                            # Gene Selection Group
                                            param_group_collapse(
                                              id = "featureplot_gene", title = "Gene Selection",
                                              icon_name = "dna", color = "#3b82f6",
                                              textAreaInput("FeatureGeneSymbol", "Gene Symbol:", value = "", height = '80px', resize = "vertical"),
                                              uiOutput("Featurehints.UI")
                                            ),
                                            # Data Selection Group
                                            param_group_collapse(
                                              id = "featureplot_data", title = "Data Selection",
                                              icon_name = "database", color = "#10b981",
                                              subtitle = "Type the genes to draw; one per line or comma-separated.",
                                              withSpinner(uiOutput("FeatureDimensionReduction.UI"), proxy.height = "10px"),
                                              withSpinner(uiOutput("FeatureAssays.UI"), proxy.height = "10px"),
                                              withSpinner(uiOutput("FeatureAssaySlots.UI"), proxy.height = "10px"),
                                              withSpinner(uiOutput("FeatureClusterResolution.UI"), proxy.height = "10px")
                                            ),
                                            # Display Options Group
                                            param_group_collapse(
                                              id = "featureplot_display", title = "Display Options",
                                              icon_name = "eye", color = "#f59e0b",
                                              subtitle = "Cluster labels, panel splitting, and label size.",
                                              checkboxInput("FeatureShowLabel",label = "Show cluster label", FALSE),
                                              withSpinner(uiOutput("FeatureSplit.UI"), proxy.height = "10px"),
                                              sliderInput("FeatureLabelSize", label = "Label Size:", min = 1, max = 20, value = 4)
                                            ),
                                            # Color Settings Group
                                            param_group_collapse(
                                              id = "featureplot_color", title = "Color Settings",
                                              icon_name = "palette", color = "#8b5cf6",
                                              subtitle = "Colour ramp from the lowest to the highest expression.",
                                              colourpicker::colourInput("FeaturePlotLowestExprColor", "Lowest expression color:", "#E5E5E5", showColour = "both", palette = "square", width = "100%"),
                                              colourpicker::colourInput("FeaturePlotHighestExprColor", "Highest expression color:", "#FF0000", showColour = "both", palette = "square", width = "100%")
                                            ),
                                            # Advanced Settings Group
                                            param_group_collapse(
                                              id = "featureplot_advanced", title = "Advanced Settings",
                                              icon_name = "cogs", color = "#6b7280",
                                              subtitle = "Opacity, expression cutoffs, point size, and the plotting area.",
                                              sliderInput("FeaturePointAlpha", label = "Point Alpha:", min = 0.1, max = 1, value = 1),
                                              sliderInput("FeatureMinCutoff", label = "Min expression cutoff (%):", min = 0, max = 100, value = 0),
                                              sliderInput("FeatureMaxCutoff", label = "Max expression cutoff (%):", min = 0, max = 100, value = 100),
                                              sliderInput("FeaturePointSize", label = "Point Size:", min = 0.001, max = 5, value = 0.8),
                                              checkboxInput("FeaturePlotMode",label = "Automatically adjust plotting area", TRUE),
                                              uiOutput("featureplot_size_ui")
                                            )
                                        )
                                      )
  )
  tab_list[["vlnplot"]] = tabItem(tabName = "vlnplot",
                                  div(
                                    class = "se-page-hint",
                                    icon("circle-info"),
                                    tags$span("The plot updates automatically as you change the settings. Hover over a parameter label to see what it does.")
                                  ),
                                  fluidRow(
                                    box(title = "Features Violin Plot",
                                        uiOutput("vlnplot_resizable_ui"),
                                        div(style = "display:inline-block; float:right", downloadButton("downloadvlnplot", label = "Download PDF", class = "btn-default btn-action")),
                                        width = 9, status = "primary", collapsible = TRUE, solidHeader = TRUE),
                                    box(title = div(
                                          icon("sliders-h"),
                                          strong(" Plot Settings"),
                                          style = "display: flex; align-items: center; gap: 8px; color: white;"
                                        ),
                                        solidHeader = TRUE, status = "primary", width = 3,

                                        # Data Selection
                                        param_group_collapse(
                                          id = "vlnplot_data", title = "Data Selection",
                                          icon_name = "database", color = "#3b82f6",
                                          subtitle = "Genes to plot, the assay to read, and an optional grouping for panels.",
                                          textAreaInput("VlnGeneSymbol", "Gene Symbols:", value = "", height = '80px', resize = "vertical"),
                                          uiOutput("Vlnhints.UI"),
                                          withSpinner(uiOutput("VlnAssays.UI"), proxy.height = "10px"),
                                          withSpinner(uiOutput("VlnAssaySlots.UI"), proxy.height = "10px"),
                                          withSpinner(uiOutput("VlnSplitBy.UI"), proxy.height = "10px")
                                        ),

                                        # Cluster Settings
                                        param_group_collapse(
                                          id = "vlnplot_cluster", title = "Cluster Settings",
                                          icon_name = "layer-group", color = "#10b981",
                                          subtitle = "Which grouping is shown and which of its levels to keep.",
                                          withSpinner(uiOutput("VlnClusterResolution.UI"), proxy.height = "10px"),
                                          withSpinner(uiOutput("VlnIdentsSelected.UI"), proxy.height = "10px")
                                        ),

                                        # Plot Options
                                        param_group_collapse(
                                          id = "vlnplot_plot", title = "Plot Options",
                                          icon_name = "sliders-h", color = "#f59e0b",
                                          subtitle = "Split and stack layout, legend, jitter points, and axis label sizes.",
                                          conditionalPanel(
                                            condition = "output.Vlnplot_splitoption_twolevels",
                                            checkboxInput("VlnSplitPlot",label = "Split Plot", FALSE)
                                          ),
                                          conditionalPanel(
                                            condition = "input.VlnSplitPlot == true && (input.VlnStackPlot != true || input.VlnFlipPlot == true)",
                                            p("Split violins use the colors defined for the Split by variable in Cluster Colors. Unsplit violins use the cluster-resolution colors; side-by-side split violins use cluster colors when defined.",
                                              style = "font-size: 11px; color: #856404; margin: -4px 0 10px 0;")
                                          ),
                                          conditionalPanel(
                                            condition = "input.VlnSplitPlot == true && input.VlnStackPlot == true && input.VlnFlipPlot != true",
                                            p("Split violins in the stacked layout require Flip axes. Without it, split groups are shown side by side instead.",
                                              style = "font-size: 11px; color: #856404; margin: -4px 0 10px 0;")
                                          ),
                                          conditionalPanel(
                                            condition = "input.VlnSplitPlot == true",
                                            uiOutput("VlnSplitColorHint.UI")
                                          ),
                                          conditionalPanel(
                                            condition = "output.Vlnplot_multiple_genes",
                                            checkboxInput("VlnStackPlot",label = "Stack Plot", FALSE)
                                          ),
                                          conditionalPanel(
                                            condition = "output.Vlnplot_StackPlot",
                                            checkboxInput("VlnFlipPlot",label = "Flip axes", FALSE)
                                          ),
                                          conditionalPanel(
                                            condition = "output.Vlnplot_StackPlot && input.VlnSplitBy == 'None'",
                                            selectInput("VlnFillBy","Color By:", choices = c(Feature = "feature", Cluster = "ident"))),
                                          checkboxInput("VlnShowLegend", "Show legend", TRUE),
                                          conditionalPanel(
                                            condition = "input.VlnShowLegend == true",
                                            sliderInput("VlnLegendNcol", "Legend columns:",
                                                        min = 1, max = 4, value = 1, step = 1)
                                          ),
                                          uiOutput("VlnLegendHint.UI"),
                                          sliderInput("VlnPointSize", label = "Point Size:", min = 0, max = 4, value = 0),
                                          sliderInput("VlnPointAlpha", label = "Point Alpha:", min = 0, max = 1, value = 1),
                                          sliderInput("VlnXlabelSize", label = "X Axis Label Size:", min = 0, max = 20, value = 14),
                                          sliderInput("VlnYlabelSize", label = "Y Axis Label Size:", min = 0, max = 20, value = 10),
                                          checkboxInput("VlnPlotMode",label = "Automatically adjust plotting area", TRUE),
                                          uiOutput("vlnplot_size_ui")
                                        )
                                    )
                                  )
  )
  tab_list[["dotplot"]] = tabItem(tabName = "dotplot",
                                  div(
                                    class = "se-page-hint",
                                    icon("circle-info"),
                                    tags$span("The plot updates automatically as you change the settings. Hover over a parameter label to see what it does.")
                                  ),
                                  fluidRow(
                                    box(title = "Features Dot Plot",
                                        uiOutput("dotplot_resizable_ui"),
                                        div(style = "display:inline-block; float:right", downloadButton("downloaddotplot", label = "Download PDF", class = "btn-default btn-action")),
                                        width = 9, status = "primary", collapsible = TRUE, solidHeader = TRUE),
                                    box(title = div(
                                          icon("sliders-h"),
                                          strong(" Plot Settings"),
                                          style = "display: flex; align-items: center; gap: 8px; color: white;"
                                        ),
                                        solidHeader = TRUE, status = "primary", width = 3,

                                        # Data Selection
                                        param_group_collapse(
                                          id = "dotplot_data", title = "Data Selection",
                                          icon_name = "database", color = "#3b82f6",
                                          subtitle = "Genes to plot, the assay to read, and an optional grouping for panels.",
                                          textAreaInput("DotGeneSymbol", "Gene Symbols:", value = "", height = '80px', resize = "vertical"),
                                          uiOutput("Dothints.UI"),
                                          withSpinner(uiOutput("DotAssays.UI"), proxy.height = "10px"),
                                          withSpinner(uiOutput("DotSplitBy.UI"), proxy.height = "10px")
                                        ),

                                        # Cluster Settings
                                        param_group_collapse(
                                          id = "dotplot_cluster", title = "Cluster Settings",
                                          icon_name = "layer-group", color = "#10b981",
                                          subtitle = "The grouping shown on the x-axis and which levels to keep.",
                                          withSpinner(uiOutput("DotClusterResolution.UI"), proxy.height = "10px"),
                                          withSpinner(uiOutput("DotIdentsSelected.UI"), proxy.height = "10px"),
                                          checkboxInput("DotClusterIdents",label = "Cluster x-axis groups", FALSE)
                                        ),

                                        # Filter Cells
                                        param_group_collapse(
                                          id = "dotplot_filter", title = "Filter Cells",
                                          icon_name = "filter", color = "#8b5cf6",
                                          subtitle = "Keep only cells matching these conditions before plotting.",
                                          withSpinner(uiOutput("IntraClusterDotplotSubsetCells.UI"), proxy.height = "10px"),
                                          withSpinner(uiOutput("IntraClusterDotplotSubsetCellsSelectedClusters.UI"), proxy.height = "10px")
                                        ),

                                        # Plot Options
                                        param_group_collapse(
                                          id = "dotplot_plot", title = "Plot Options",
                                          icon_name = "sliders-h", color = "#f59e0b",
                                          subtitle = "Axis orientation, colours, dot size, and the plotting area.",
                                          checkboxInput("DotRotateAxis",label = "Rotate x-axis labels", FALSE),
                                          checkboxInput("DotFlipCoordinate",label = "Flip axes", FALSE),
                                          conditionalPanel(
                                            condition = "output.DotPlot_Split_isNone",
                                            colourpicker::colourInput("DotPlotLowestExprColor", "Pick Color for lowest expression:", "#E5E5E5", showColour = "both", palette = "square", width = "100%"),
                                            colourpicker::colourInput("DotPlotHighestExprColor", "Pick Color for highest expression:", "#0000FF", showColour = "both", palette = "square", width = "100%")),
                                          sliderInput("DotDotScale", label = "Dot Scale:", min = 1, max = 12, value = 6),
                                          sliderInput("DotXlabelSize", label = "X Axis Label Size:", min = 0, max = 20, value = 14),
                                          sliderInput("DotYlabelSize", label = "Y Axis Label Size:", min = 0, max = 20, value = 10),
                                          checkboxInput("DotPlotMode",label = "Automatically adjust plotting area", TRUE),
                                          uiOutput("dotplot_size_ui")
                                        )
                                    )
                                  )
  )
  tab_list[["heatmap"]] = tabItem(tabName = "heatmap",
                                  div(
                                    class = "se-page-hint",
                                    icon("circle-info"),
                                    tags$span("The plot updates automatically as you change the settings. Hover over a parameter label to see what it does.")
                                  ),
                                  fluidRow(
                                    box(title = "Features Heatmap Plot",
                                        uiOutput("heatmap_resizable_ui"),
                                        div(style = "display:inline-block; float:right", downloadButton("downloadheatmap", label = "Download PDF", class = "btn-default btn-action")),
                                        width = 9, status = "primary", collapsible = TRUE, solidHeader = TRUE),
                                    box(title = div(
                                          icon("sliders-h"),
                                          strong(" Plot Settings"),
                                          style = "display: flex; align-items: center; gap: 8px; color: white;"
                                        ),
                                        solidHeader = TRUE, status = "primary", width = 3,

                                        # Data Selection
                                        param_group_collapse(
                                          id = "heatmap_data", title = "Data Selection",
                                          icon_name = "database", color = "#3b82f6",
                                          subtitle = "Genes to plot and the assay and slot used for the heatmap.",
                                          textAreaInput("HeatmapGeneSymbol", "Gene Symbols:", value = "", height = '80px', resize = "vertical"),
                                          uiOutput("Heatmaphints.UI"),
                                          withSpinner(uiOutput("HeatmapAssays.UI"), proxy.height = "10px"),
                                          withSpinner(uiOutput("HeatmapAssaySlots.UI"), proxy.height = "10px")
                                        ),

                                        # Cluster Settings
                                        param_group_collapse(
                                          id = "heatmap_cluster", title = "Cluster Settings",
                                          icon_name = "layer-group", color = "#10b981",
                                          subtitle = "The grouping shown across the columns and which levels to keep.",
                                          withSpinner(uiOutput("HeatmapClusterResolution.UI"), proxy.height = "10px"),
                                          withSpinner(uiOutput("HeatmapIdentsSelected.UI"), proxy.height = "10px")
                                        ),

                                        # Plot Options
                                        param_group_collapse(
                                          id = "heatmap_plot", title = "Plot Options",
                                          icon_name = "sliders-h", color = "#f59e0b",
                                          subtitle = "Text size and placement, group bar height, line width, plotting area.",
                                          sliderInput("HeatmapTextSize", label = "Cluster Text Size:", min = 1, max = 20, value = 6, step = 0.5),
                                          sliderInput("HeatmapTextHjust", label = "Cluster Text Hjust:", min = -10, max = 20, value = 0, step = 0.5),
                                          sliderInput("HeatmapTextVjust", label = "Cluster Text Vjust:", min = -5, max = 5, value = 0, step = 0.1),
                                          sliderInput("HeatmapTextRatateAngle", label = "Cluster Text Rotate Angle:", min = -90, max = 90, value = 0, step = 1),
                                          sliderInput("HeatmapGroupBarHeight", label = "Cluster Group Bar Height:", min = 0, max = 0.1, value = 0.04, step = 0.01),
                                          sliderInput("HeatmapLineWidth", label = "Line Width:", min = 1, max = 10, value = 1),
                                          sliderInput("HeatmapFeatureTextSize", label = "Feature Text Size:", min = 0, max = 20, value = 10),
                                          checkboxInput("HeatmapPlotMode",label = "Automatically adjust plotting area", TRUE),
                                          uiOutput("heatmap_size_ui")
                                        )
                                    )
                                  )
  )
  tab_list[["averagedheatmap"]] = tabItem(tabName = "averagedheatmap",
                                  div(
                                    class = "se-page-hint",
                                    icon("circle-info"),
                                    tags$span("The plot updates automatically as you change the settings. Hover over a parameter label to see what it does.")
                                  ),
                                  fluidRow(
                                    box(title = "Features Heatmap by Averaged Expression",
                                        uiOutput("averagedheatmap_resizable_ui"),
                                        div(style = "display:inline-block; float:right", downloadButton("downloadaveragedheatmap", label = "Download PDF", class = "btn-default btn-action")),
                                        width = 9, status = "primary", collapsible = TRUE, solidHeader = TRUE),
                                    box(title = div(
                                          icon("sliders-h"),
                                          strong(" Plot Settings"),
                                          style = "display: flex; align-items: center; gap: 8px; color: white;"
                                        ),
                                        solidHeader = TRUE, status = "primary", width = 3,

                                        # Data Selection
                                        param_group_collapse(
                                          id = "avgheatmap_data", title = "Data Selection",
                                          icon_name = "database", color = "#3b82f6",
                                          subtitle = "Genes to plot and the assay used for the averaged values.",
                                          textAreaInput("AveragedHeatmapGeneSymbol", "Gene Symbols:", value = "", height = '80px', resize = "vertical"),
                                          uiOutput("AveragedHeatmaphints.UI"),
                                          withSpinner(uiOutput("AveragedHeatmapAssays.UI"), proxy.height = "10px")
                                        ),

                                        # Cluster Settings
                                        param_group_collapse(
                                          id = "avgheatmap_cluster", title = "Cluster Settings",
                                          icon_name = "layer-group", color = "#10b981",
                                          subtitle = "The grouping averaged over, plus optional clustering of rows and columns.",
                                          withSpinner(uiOutput("AveragedHeatmapClusterResolution.UI"), proxy.height = "10px"),
                                          withSpinner(uiOutput("AveragedHeatmapIdentsSelected.UI"), proxy.height = "10px"),
                                          checkboxInput("AveragedHeatmapClusterClusters",label = "Cluster Clusters", FALSE),
                                          checkboxInput("AveragedHeatmapClusterFeatures",label = "Cluster Features", FALSE)
                                        ),

                                        # Plot Options
                                        param_group_collapse(
                                          id = "avgheatmap_plot", title = "Plot Options",
                                          icon_name = "sliders-h", color = "#f59e0b",
                                          subtitle = "Text sizes and rotation, and the plotting area.",
                                          sliderInput("AveragedHeatmapClusterTextSize", label = "Cluster Text Size:", min = 1, max = 30, value = 12),
                                          sliderInput("AveragedHeatmapClusterTextRatateAngle", label = "Cluster Text Rotate Angle:", min = -90, max = 90, value = 45),
                                          sliderInput("AveragedHeatmapFeatureTextSize", label = "Feature Text Size:", min = 1, max = 20, value = 10),
                                          checkboxInput("AveragedHeatmapPlotMode",label = "Automatically adjust plotting area", TRUE),
                                          uiOutput("averagedheatmap_size_ui")
                                        )
                                    )
                                  )
  )
  tab_list[["ridgeplot"]] = tabItem(tabName = "ridgeplot",
                                    div(
                                      class = "se-page-hint",
                                      icon("circle-info"),
                                      tags$span("The plot updates automatically as you change the settings. Hover over a parameter label to see what it does.")
                                    ),
                                    fluidRow(
                                      box(title = "Features Ridge Plot",
                                          uiOutput("ridgeplot_resizable_ui"),
                                          div(style = "display:inline-block; float:right", downloadButton("downloadridgeplot", label = "Download PDF", class = "btn-default btn-action")),
                                          width = 9, status = "primary", collapsible = TRUE, solidHeader = TRUE),
                                      box(title = div(
                                            icon("sliders-h"),
                                            strong(" Plot Settings"),
                                            style = "display: flex; align-items: center; gap: 8px; color: white;"
                                          ),
                                          solidHeader = TRUE, status = "primary", width = 3,

                                          # Data Selection
                                          param_group_collapse(
                                            id = "ridgeplot_data", title = "Data Selection",
                                            icon_name = "database", color = "#3b82f6",
                                            subtitle = "Genes to plot, the assay to read, and the slot.",
                                            textAreaInput("RidgeplotGeneSymbol", "Gene Symbols:", value = "", height = '80px', resize = "vertical"),
                                            uiOutput("Ridgeplothints.UI"),
                                            withSpinner(uiOutput("RidgeplotAssays.UI"), proxy.height = "10px"),
                                            withSpinner(uiOutput("RidgeplotAssaySlots.UI"), proxy.height = "10px")
                                          ),

                                          # Cluster Settings
                                          param_group_collapse(
                                            id = "ridgeplot_cluster", title = "Cluster Settings",
                                            icon_name = "layer-group", color = "#10b981",
                                            subtitle = "The grouping shown along the y-axis and which levels to keep.",
                                            withSpinner(uiOutput("RidgeplotClusterResolution.UI"), proxy.height = "10px"),
                                            withSpinner(uiOutput("RidgeplotIdentsSelected.UI"), proxy.height = "10px")
                                          ),

                                          # Plot Options
                                          param_group_collapse(
                                            id = "ridgeplot_plot", title = "Plot Options",
                                            icon_name = "sliders-h", color = "#f59e0b",
                                            subtitle = "Panel layout, stacking, colour source, and axis label sizes.",
                                            conditionalPanel(
                                              condition = "output.Ridgeplot_stack_NotSelected",
                                              sliderInput("RidgeplotNumberOfColumns", label = "Number of columns:", min = 1, max = 10, value = 1)),
                                            conditionalPanel(
                                              condition = "output.Ridgeplot_stack_show",
                                              checkboxInput("RidgeplotStackPlot",label = "Stack Plot", FALSE)),
                                            conditionalPanel(
                                              condition = "input.RidgeplotStackPlot",
                                              selectInput("RidgeplotFillBy","Color By:", choices = c(Feature = "feature", Cluster = "ident"))
                                            ),
                                            sliderInput("RidgeplotXlabelSize", label = "X Axis Label Size:", min = 0, max = 20, value = 14),
                                            sliderInput("RidgeplotYlabelSize", label = "Y Axis Label Size:", min = 0, max = 20, value = 10),
                                            checkboxInput("RidgeplotPlotMode",label = "Automatically adjust plotting area", TRUE),
                                            uiOutput("ridgeplot_size_ui")
                                          )
                                      )
                                    )
  )
  tab_list[["cellratioplot"]] = tabItem(tabName = "cellratioplot",
                                    div(
                                      class = "se-page-hint",
                                      icon("circle-info"),
                                      tags$span("The plot updates automatically as you change the settings. Hover over a parameter label to see what it does.")
                                    ),
                                    fluidRow(
                                      box(title = "Cell Percentage Plot",
                                          uiOutput("cellratioplot_resizable_ui"),
                                          div(style = "display:inline-block; float:right", downloadButton("downloadcellratioplot", label = "Download PDF", class = "btn-default btn-action")),
                                          div(style = "margin-top: 50px;",
                                              hr(),
                                              DT::dataTableOutput('cellratiodata'),
                                              align="center"),
                                          width = 9, status = "primary", collapsible = TRUE, solidHeader = TRUE),
                                      box(title = div(
                                            icon("sliders-h"),
                                            strong(" Plot Settings"),
                                            style = "display: flex; align-items: center; gap: 8px; color: white;"
                                          ),
                                          solidHeader = TRUE, status = "primary", width = 3,

                                          # Data Selection (Fill, X, Facet)
                                          param_group_collapse(
                                            id = "cellratio_data", title = "Data Selection",
                                            icon_name = "database", color = "#3b82f6",
                                            subtitle = "What the x-axis, fill colours, and facets represent.",
                                            withSpinner(uiOutput("CellratioFillChoice.UI"), proxy.height = "10px"),
                                            withSpinner(uiOutput("CellratioXChoice.UI"), proxy.height = "10px"),
                                            withSpinner(uiOutput("CellratioFacetChoice.UI"), proxy.height = "10px")
                                          ),

                                          # Subset Cells
                                          param_group_collapse(
                                            id = "cellratio_subset", title = "Subset Cells",
                                            icon_name = "filter", color = "#8b5cf6",
                                            subtitle = "Restrict the cells used to compute the percentages.",
                                            withSpinner(uiOutput("CellratioSubsetCells.UI"), proxy.height = "10px"),
                                            uiOutput("CellratioSubsetCellsAddFilter.UI"),
                                            uiOutput("CellratioSubsetCellsHint.UI")
                                          ),

                                          # Plot Options
                                          param_group_collapse(
                                            id = "cellratio_plot", title = "Plot Options",
                                            icon_name = "sliders-h", color = "#f59e0b",
                                            subtitle = "Legend, labels, flow appearance, and the plotting area.",
                                            checkboxInput("CellratioShowLegend", "Show legend", TRUE),
                                            conditionalPanel(
                                              condition = "input.CellratioShowLegend == true",
                                              sliderInput("CellratioLegendNcol", "Legend columns:",
                                                          min = 1, max = 4, value = 1, step = 1)
                                            ),
                                            uiOutput("CellratioManyClustersHint.UI"),
                                            checkboxInput("CellratioRotateAxis",label = "Rotate x-axis labels", FALSE),
                                            conditionalPanel(
                                              condition = "input.CellratioFacetChoice == 'None'",
                                              checkboxInput("CellratioShowSampleSize", label = "Show sample sizes on x-axis", FALSE)
                                            ),
                                            checkboxInput("CellratioShowPercent", label = "Show percentages", FALSE),
                                            conditionalPanel(
                                              condition = "input.CellratioShowPercent == true",
                                              sliderInput("CellratioPercentCutoff", "Hide labels below (%):",
                                                          min = 0, max = 20, value = 2.5, step = 0.5),
                                              sliderInput("CellratioPercentSize", "Label size:",
                                                          min = 1, max = 12, value = 4, step = 0.5),
                                              selectInput("CellratioPercentDigits", "Decimal places:",
                                                          choices = c("0" = 0, "1" = 1, "2" = 2), selected = 1),
                                              selectInput("CellratioPercentColor", "Label color:",
                                                          choices = c("Automatic" = "auto", "Black" = "black", "White" = "white"),
                                                          selected = "auto"),
                                              checkboxInput("CellratioPercentListRare", "List small segments below plot", FALSE)
                                            ),
                                            sliderInput("CellratioColumnWidth", label = "Column width:", min = 0, max = 1, value = 0.7),
                                            sliderInput("CellratioFlowAlpha", label = "Flow alpha:", min = 0, max = 1, value = 0.3),
                                            sliderInput("CellratioFlowCurve", label = "Flow curve:", min = 0, max = 1, value = 0.3),
                                            checkboxInput("CellratioMode",label = "Automatically adjust plotting area", TRUE),
                                            uiOutput("cellratioplot_size_ui")
                                          )
                                      )
                                    )
  )
  tab_list[["degs"]] = tabItem(tabName = "degs",

                               # Tab card styling - ONLY for DEGs page
                               tags$style("
                                   /* One closed card with underline tabs: the border lives
                                      on the card wrapper, the tab strip and the panel are
                                      borderless. */
                                   #degstabbox-container {
                                     padding-left: 0 !important;
                                     padding-right: 0 !important;
                                     margin-left: 0 !important;
                                     margin-right: 0 !important;
                                   }
                                   /* tabBox() wraps itself in a Bootstrap column whose 15px
                                      gutter made the card 15px narrower than the Information
                                      box above. Cancel the gutter on the card itself with
                                      negative margins so the rule does not depend on the
                                      wrapper's class name or nesting depth. */
                                   #degstabbox-container .nav-tabs-custom {
                                     background: #ffffff;
                                     border: 2px solid #10b981;
                                     border-radius: 8px;
                                     box-shadow: 0 2px 8px rgba(0,0,0,0.08);
                                     margin-left: -15px !important;
                                     margin-right: -15px !important;
                                     width: calc(100% + 30px) !important;
                                     box-sizing: border-box !important;
                                   }
                                   #degstabbox-container .nav-tabs-custom .nav-tabs {
                                     background: #ffffff;
                                     border: none;
                                     border-bottom: 1px solid #e5e7eb;
                                     border-radius: 8px 8px 0 0;
                                     padding: 10px 20px 0 20px;
                                     margin: 0 !important;
                                   }
                                   #degstabbox-container .nav-tabs-custom .nav-tabs li {
                                     border-top: none;
                                     margin: 0 0 -1px 0;
                                   }
                                   #degstabbox-container .nav-tabs-custom .nav-tabs li a {
                                     background: transparent;
                                     border: none;
                                     border-bottom: 2px solid transparent;
                                     border-radius: 0;
                                     color: #5C636A;
                                     padding: 8px 0;
                                     margin: 0 28px 0 0;
                                     transition: color 0.2s ease, border-color 0.2s ease;
                                   }
                                   #degstabbox-container .nav-tabs-custom .nav-tabs li a:hover {
                                     background: transparent;
                                     color: #047857;
                                   }
                                   #degstabbox-container .nav-tabs-custom .nav-tabs li.active:hover a,
                                   #degstabbox-container .nav-tabs-custom .nav-tabs li.active a {
                                     background: transparent;
                                     border: none;
                                     border-bottom: 2px solid #10b981;
                                     color: #047857;
                                     font-weight: 600;
                                   }
                                   #degstabbox-container .tab-content {
                                     background: #ffffff;
                                     border: none;
                                     border-radius: 0 0 8px 8px;
                                     padding: 20px;
                                     box-shadow: none;
                                     margin-left: 0 !important;
                                     margin-right: 0 !important;
                                   }
                                 "),

                               # Single fluidRow containing both Information Box and tabBox
                               div(
                                 class = "se-page-hint",
                                 icon("circle-info"),
                                 tags$span("Hover a tab for details, then click Analyze. Analyses can take a while, so save current results first.")
                               ),
                               fluidRow(id = "degs-main-row",

                                 # tabBox (bottom, full width)
                                 div(
                                   class = "col-xs-12",
                                   style = "margin-bottom: 10px;",
                                   div(id = "degstabbox-container",
                                     tabBox(
                                       # The title is laid out as its own row above the tabs
                                       # by the shared .nav-tabs > li.header rules.
                                       title = h4(
                                         icon("chart-line"),
                                         "Find Markers or DEGs",
                                         style = "margin: 0; color: #047857; font-weight: 600; display: flex; align-items: center; gap: 8px;"
                                       ),
                                       id = "tabset_degs", width = 12,
                                       tabPanel("Find Markers for All Clusters",
                                                div(
                                                  # The tab card is already outlined in green; this panel keeps its
                                                  # tint and left accent but drops the second outline so the two
                                                  # borders no longer trace the same edge.
                                                  style = "background: #f0fdf4; border-left: 4px solid #10b981; padding: 20px; border-radius: 8px; margin-bottom: 25px;",
                                                  withSpinner(uiOutput("ClusterMarkersClusterResolution.UI"), proxy.height = "10px")
                                                ),
                                                div(style = "text-align: center; margin-top: 25px;",
                                                  actionButton("DEGsClusterMarkersAnalysis",
                                                              icon = shiny::icon("magnifying-glass-chart"),
                                                              label = "Analyze",
                                                              class = "btn-primary btn-action")
                                            )),
                                   tabPanel("Find DEGs for two groups",
                                            # Step 1 - Filter Cells
                                            div(
                                              style = "background: #eff6ff; border: 1px solid #3b82f6; border-left: 4px solid #3b82f6; padding: 25px; border-radius: 8px; margin-bottom: 25px; box-shadow: 0 2px 6px rgba(59, 130, 246, 0.1);",
                                              h4(icon("filter"), "Step 1: Filter Cells (Optional)", style = "color: #1D4ED8; margin-bottom: 12px; font-weight: 600; display: flex; align-items: center; gap: 8px;"),
                                              p("Click the + button to add multiple filter conditions. Cells matching ALL conditions (intersection) will be kept.", style = "color: #5C636A; margin-bottom: 8px;"),
                                              p("If no filters are modified, all cells will be kept by default.", style = "color: #5C636A; margin-bottom: 15px;"),
                                              withSpinner(uiOutput("IntraClusterDEGsFilterRows.UI"), proxy.height = "10px"),
                                              uiOutput("IntraClusterDEGsAddFilter.UI")
                                            ),
                                            # Step 2 - Set Comparison
                                            div(
                                              style = "background: #fdf2f8; border: 1px solid #ec4899; border-left: 4px solid #ec4899; padding: 25px; border-radius: 8px; margin-bottom: 25px; box-shadow: 0 2px 6px rgba(236, 72, 153, 0.1);",
                                              h4(icon("balance-scale"), "Step 2: Set the comparison", style = "color: #BE185D; margin-bottom: 15px; font-weight: 600; display: flex; align-items: center; gap: 8px;"),
                                              withSpinner(uiOutput("IntraClusterDEGsCustomizedGroups.UI"), proxy.height = "10px"),
                                              withSpinner(uiOutput("IntraClusterDEGsCustomizedGroupsCase.UI"), proxy.height = "10px"),
                                              withSpinner(uiOutput("IntraClusterDEGsCustomizedGroupsControl.UI"), proxy.height = "10px")
                                            ),
                                            div(style = "text-align: center; margin-top: 30px;",
                                              actionButton("IntraClusterDEGssAnalysis",
                                                          icon = shiny::icon("magnifying-glass-chart"),
                                                          label = "Analyze",
                                                          class = "btn-primary btn-action")
                                            )),
                                   tabPanel("Custom Parameters",
                                            div(
                                              style = "background: #fef3c7; border: 1px solid #f59e0b; border-left: 4px solid #f59e0b; padding: 25px; border-radius: 8px; margin-bottom: 20px; box-shadow: 0 2px 6px rgba(245, 158, 11, 0.1);",
                                              h4(icon("sliders-h"), "Analysis Parameters", style = "color: #B45309; margin-bottom: 20px; font-weight: 600; display: flex; align-items: center; gap: 8px;"),
                                              withSpinner(uiOutput("DEGsAssays.UI"), proxy.height = "10px"),
                                              div(style = "margin-top: 20px;",
                                                sliderInput("logfcthreshold", label = "logFC threshold:", min = 0, max = 1, value = 0.1),
                                                selectInput("testuse","Test use:", choices = c(wilcox = "wilcox", wilcox_limma = "wilcox_limma",
                                                                                                       T_test = "t", negbinom = "negbinom", poisson = "poisson",
                                                                                                       LR = "LR", MAST = "MAST", DESeq2 = "DESeq2")),
                                                sliderInput("minpct", label = "Minimum Expression Percentage:", min = 0, max = 1, value = 0.01),
                                                sliderInput("mindiffpct", label = "Minimum Expression Percentage Difference:", min = 0, max = 1, value = 0)
                                              )
                                            ),
                                            div(style = "text-align: center;",
                                              actionButton("SetDefault",
                                                          icon = shiny::icon("save"),
                                                          label = "Reset to defaults",
                                                          class = "btn-warning btn-action")
                                            ))
                                     )  # end of tabBox
                                   )  # end of degstabbox-container div
                                 ),  # end of col-xs-12 div (tabBox)

                               conditionalPanel(
                                 condition = "!output.DEGs_ready",
                                 div(class = "se-empty-state", style = "margin: 15px 15px 0 15px;",
                                     icon("arrow-down"),
                                     tags$span("Set the options, then click Analyze. Results will appear here."))
                               ),
                               conditionalPanel(
                                   condition = "output.DEGs_ready",
                                   div(style = "margin-top: 15px; margin-left: 15px; margin-right: 15px;",
                                     fluidRow(
                                       # Analysis Results - Custom style (left column, accounting for 8/12)
                                       div(
                                         class = "col-md-8",
                                         div(
                                           class = "box",
                                           style = "background: white; border: 2px solid #3b82f6; border-radius: 8px; box-shadow: 0 2px 6px rgba(0,0,0,0.08);",
                                           div(
                                             class = "box-header",
                                             style = "padding: 15px 20px; border-bottom: 2px solid #3b82f6;",
                                             div(
                                               style = "display: flex; align-items: center; gap: 10px;",
                                               icon("table", style = "color: #1D4ED8; font-size: 18px;"),
                                               h4(style = "margin: 0; color: #1D4ED8; font-weight: 600;", "Analysis Results")
                                             )
                                           ),
                                           div(
                                             class = "box-body",
                                             style = "padding: 20px;",
                                             uiOutput("degs_run_info"),
                                             uiOutput("degs_colguide"),
                                             # Single <table> (no scrollX); scroll on narrow screens.
                                             div(style = "overflow-x: auto;",
                                               withSpinner(DT::dataTableOutput('dataset_degs')))
                                           )
                                         )
                                       ),
                                       # External Links - Custom styles (right column, accounting for 4/12)
                                       div(
                                         class = "col-md-4",
                                         conditionalPanel(
                                           condition = "output.DEGs_row_selected",
                                           div(
                                             class = "box",
                                             style = "background: white; border: 2px solid #10b981; border-radius: 8px; box-shadow: 0 2px 6px rgba(0,0,0,0.08);",
                                             div(
                                               class = "box-header",
                                               style = "padding: 15px 20px; border-bottom: 2px solid #10b981;",
                                               div(
                                                 style = "display: flex; align-items: center; gap: 10px;",
                                                 icon("external-link-alt", style = "color: #047857; font-size: 18px;"),
                                                 h4(style = "margin: 0; color: #047857; font-weight: 600;", "External Links")
                                               )
                                             ),
                                             div(
                                               class = "box-body",
                                               style = "padding: 20px;",
                                               div(
                                                 style = "margin: 10px 0;",
                                                 selectInput("selectspecies", "Choose Species:", choices = c("Human" = "human","Mouse" = "mouse","Fly" = "fly"), width = '100%'),
                                                 selectInput("selectsgenetype", "Choose Feature Types:", choices = c("Symbol" = "Symbol","Ensembl ID" = "Ensembl","Entrez ID" = "EntrezID"), width = '100%'),
                                                 withSpinner(uiOutput('ExternalLinks.UI'))
                                               )
                                             )
                                           )
                                         )
                                       )  # end of col-md-4 div
                                   )  # end of fluidRow containing Analysis Results and External Links
                                 )  # end of margin-top div
                               )  # end of conditionalPanel
                               )  # end of fluidRow containing both Information Box and tabBox
  )  # end of degs tabItem
  tab_list[["topgenes"]] = tabItem(tabName = "topgenes",

                               # The Step 2 card uses the shared .se-step-* styles defined
                               # in the global CSS block of dashboardBody.

                               # Page hint, then the settings and analysis row
                               div(
                                 class = "se-page-hint",
                                 icon("circle-info"),
                                 tags$span("Hover a ranking method for details, then click Analyze. Analyses can take a while, so save current results first.")
                               ),
                               fluidRow(id = "topgenes-main-row",

                                 # Step1: Common Settings (Left column, accounting for 3/12)
                                 div(
                                   class = "col-md-3",
                                   style = "margin-bottom: 10px;",
                                   div(
                                     style = "background: #f0fdf4; border: 1px solid #10b981; border-left: 4px solid #10b981; padding: 20px; border-radius: 8px; height: 100%;",
                                     h4(icon("sliders-h"), "Step 1: Settings", style = "color: #047857; margin-bottom: 15px; font-weight: 600; display: flex; align-items: center; gap: 8px;"),
                                     withSpinner(uiOutput("TopGenesClusterResolution.UI"), proxy.height = "10px"),
                                     withSpinner(uiOutput("TopGenesSelectedClusters.UI"), proxy.height = "10px"),
                                     withSpinner(uiOutput("TopGenesAssays.UI"), proxy.height = "10px"),
                                     div(style = "margin-top: 15px;",
                                       checkboxInput("TopGenesClusterLevel", label = "By each cluster", TRUE)
                                     )
                                   )
                                 ),

                                 # Step2: Calculate Top Genes (Right column, accounting for 9/12)
                                 div(
                                   class = "col-md-9",
                                   style = "margin-bottom: 10px;",
                                   # One card with a header and a plain form body. The two
                                   # ranking modes are mutually exclusive settings, not pages,
                                   # so they are a segmented control instead of a tab strip.
                                   div(
                                     class = "se-step-card",
                                     div(
                                       class = "se-step-header",
                                       icon("chart-line", style = "color: #1D4ED8;"),
                                       h4("Step 2: Calculate Top Genes")
                                     ),
                                     div(
                                       class = "se-step-body",
                                       tags$label(class = "se-field-label", `for` = "TopGenesMethod", "Ranking method"),
                                       shinyWidgets::radioGroupButtons(
                                         inputId = "TopGenesMethod",
                                         label = NULL,
                                         choices = c("By cell" = "cell",
                                                     "By accumulated UMI counts" = "umi"),
                                         selected = "cell",
                                         status = "primary",
                                         justified = FALSE
                                       ),
                                       conditionalPanel(
                                         # Negated test: the panel is visible before the
                                         # radio input has reported its initial value.
                                         condition = "input.TopGenesMethod != 'umi'",
                                         div(
                                           style = "margin-top: 18px;",
                                           sliderInput("TopGenesTopPercent", "UMI percentage cutoff",
                                                       min = 1, max = 50, value = 1, step = 1,
                                                       ticks = FALSE, post = "%", width = "100%"),
                                           p(class = "se-hint",
                                             "Keeps the genes that make up the top percentage of UMIs in each cell.")
                                         ),
                                         div(class = "se-actions",
                                           actionButton("TopGenesAnalysis",
                                                        icon = shiny::icon("magnifying-glass-chart"),
                                                        label = "Analyze",
                                                        class = "btn-primary btn-action"))
                                       ),
                                       conditionalPanel(
                                         condition = "input.TopGenesMethod == 'umi'",
                                         div(
                                           style = "margin-top: 18px;",
                                           sliderInput("TopGenesTopN", "Top N genes",
                                                       min = 10, max = 1000, value = 100, step = 10,
                                                       ticks = FALSE, width = "100%"),
                                           p(class = "se-hint",
                                             "Returns the N genes with the highest accumulated UMI counts across the selected cells.")
                                         ),
                                         div(class = "se-actions",
                                           actionButton("TopAccumulatedGenesAnalysis",
                                                        icon = shiny::icon("magnifying-glass-chart"),
                                                        label = "Analyze",
                                                        class = "btn-primary btn-action"))
                                       )
                                     )
                                   )
                                 ),

                                 # Analysis Results
                                 conditionalPanel(
                                   condition = "!output.TopGenes_ready",
                                   div(class = "se-empty-state", style = "margin: 15px 15px 0 15px;",
                                       icon("arrow-down"),
                                       tags$span("Set the options, then click Analyze. Results will appear here."))
                                 ),
                                 conditionalPanel(
                                   condition = "output.TopGenes_ready",
                                   div(style = "margin-top: 15px; margin-left: 15px; margin-right: 15px;",
                                     fluidRow(
                                       div(
                                         class = "col-xs-12",
                                         div(
                                           class = "box",
                                           style = "background: white; border: 2px solid #3b82f6; border-radius: 8px; box-shadow: 0 2px 6px rgba(0,0,0,0.08);",
                                           div(
                                             class = "box-header",
                                             style = "padding: 15px 20px; border-bottom: 2px solid #3b82f6;",
                                             div(
                                               style = "display: flex; align-items: center; gap: 10px;",
                                               icon("table", style = "color: #1D4ED8; font-size: 18px;"),
                                               h4(style = "margin: 0; color: #1D4ED8; font-weight: 600;", "Analysis Results")
                                             )
                                           ),
                                           div(
                                             class = "box-body",
                                             style = "padding: 20px;",
                                             uiOutput("topgenes_run_info"),
                                             uiOutput("topgenes_colguide"),
                                             # The table is a single <table> (no scrollX), so on very
                                             # narrow screens let the wrapper scroll rather than
                                             # letting the table overflow the card.
                                             div(style = "overflow-x: auto;",
                                               withSpinner(DT::dataTableOutput('dataset_topgenes')))
                                           )
                                         )
                                       )
                                     )
                                   )
                                 )
                               )
  )
  tab_list[["featuresummary"]] = tabItem(tabName = "featuresummary",

                               # Page hint, then the settings and analysis row
                               div(
                                 class = "se-page-hint",
                                 icon("circle-info"),
                                 tags$span("Set the options, then click Analyze; the results appear below. Unmatched features will be automatically ignored.")
                               ),
                               fluidRow(id = "featuresummary-main-row",

                                 # Settings (Left column, accounting for 3/12)
                                 div(
                                   class = "col-md-3",
                                   style = "margin-bottom: 10px;",
                                   div(
                                     style = "background: #f0fdf4; border: 1px solid #10b981; border-left: 4px solid #10b981; padding: 20px; border-radius: 8px; height: 100%;",
                                     h4(icon("sliders-h"), "Settings", style = "color: #047857; margin-bottom: 15px; font-weight: 600; display: flex; align-items: center; gap: 8px;"),
                                     textAreaInput("FeatureSummarySymbol", "Input Gene Symbols:", value = "", height = '100px', resize = "vertical"),
                                     withSpinner(uiOutput("FeatureSummaryClusterResolution.UI"), proxy.height = "10px"),
                                     withSpinner(uiOutput("FeatureSummarySelectedClusters.UI"), proxy.height = "10px"),
                                     withSpinner(uiOutput("FeatureSummaryAssays.UI"), proxy.height = "10px"),
                                     div(style = "margin-top: 15px;",
                                       checkboxInput("FeatureSummaryClusterLevel", label = "By each cluster", TRUE)
                                     ),
                                     div(style = "margin-top: 20px; text-align: center;",
                                       actionButton("FeatureSummaryAnalysis",
                                                   icon = shiny::icon("magnifying-glass-chart"),
                                                   label = "Analyze",
                                                   class = "btn-primary btn-action")
                                     )
                                   )
                                 ),

                                 # Gene Short Summary (Right column, accounting for 9/12)
                                 div(
                                   class = "col-md-9",
                                   style = "margin-bottom: 10px;",
                                   conditionalPanel(
                                     condition = "!output.FeatureSummary_ready",
                                     div(class = "se-empty-state",
                                         icon("arrow-down"),
                                         tags$span("Set the options, then click Analyze. Results will appear here."))
                                   ),
                                   conditionalPanel(
                                     condition = "output.FeatureSummary_ready",
                                     div(
                                       class = "box",
                                       style = "background: white; border: 2px solid #3b82f6; border-radius: 8px; box-shadow: 0 2px 6px rgba(0,0,0,0.08);",
                                       div(
                                         class = "box-header",
                                         style = "padding: 15px 20px; border-bottom: 2px solid #3b82f6;",
                                         div(
                                           style = "display: flex; align-items: center; gap: 10px;",
                                           icon("table", style = "color: #1D4ED8; font-size: 18px;"),
                                           h4(style = "margin: 0; color: #1D4ED8; font-weight: 600;", "Gene Short Summary")
                                         )
                                       ),
                                       div(
                                         class = "box-body",
                                         style = "padding: 20px;",
                                         uiOutput("featuresummary_run_info"),
                                         uiOutput("featuresummary_colguide"),
                                         # Single <table> (no scrollX); scroll on narrow screens.
                                         div(style = "overflow-x: auto;",
                                           withSpinner(DT::dataTableOutput('dataset_featuresummary')))
                                       )
                                     )
                                   )
                                 )
                               )
  )
  tab_list[["featurecorrelation"]] = tabItem(tabName = "featurecorrelation",

                               # Tab card styling - ONLY for featurecorrelation page
                               tags$style("
                                   #featurecorrelationtabbox-container {
                                     padding-left: 0 !important;
                                     padding-right: 0 !important;
                                     margin-left: 0 !important;
                                     margin-right: 0 !important;
                                   }
                                   /* Cancel the tabBox wrapper column's 15px gutter on the
                                      card itself, so the rule does not depend on the
                                      wrapper's class name or nesting depth. */
                                   #featurecorrelationtabbox-container .nav-tabs-custom {
                                     background: #ffffff;
                                     border: 2px solid #3b82f6;
                                     border-radius: 8px;
                                     box-shadow: 0 2px 8px rgba(0,0,0,0.08);
                                     margin-left: -15px !important;
                                     margin-right: -15px !important;
                                     width: calc(100% + 30px) !important;
                                     box-sizing: border-box !important;
                                   }
                                   #featurecorrelationtabbox-container .nav-tabs-custom .nav-tabs {
                                     background: #ffffff;
                                     border: none;
                                     border-bottom: 1px solid #e5e7eb;
                                     border-radius: 8px 8px 0 0;
                                     padding: 10px 20px 0 20px;
                                     margin: 0 !important;
                                   }
                                   #featurecorrelationtabbox-container .nav-tabs-custom .nav-tabs li {
                                     border-top: none;
                                     margin: 0 0 -1px 0;
                                   }
                                   #featurecorrelationtabbox-container .nav-tabs-custom .nav-tabs li a {
                                     background: transparent;
                                     border: none;
                                     border-bottom: 2px solid transparent;
                                     border-radius: 0;
                                     color: #5C636A;
                                     padding: 8px 0;
                                     margin: 0 28px 0 0;
                                     transition: color 0.2s ease, border-color 0.2s ease;
                                   }
                                   #featurecorrelationtabbox-container .nav-tabs-custom .nav-tabs li a:hover {
                                     background: transparent;
                                     color: #1D4ED8;
                                   }
                                   #featurecorrelationtabbox-container .nav-tabs-custom .nav-tabs li.active:hover a,
                                   #featurecorrelationtabbox-container .nav-tabs-custom .nav-tabs li.active a {
                                     background: transparent;
                                     border: none;
                                     border-bottom: 2px solid #3b82f6;
                                     color: #1D4ED8;
                                     font-weight: 600;
                                   }
                                   #featurecorrelationtabbox-container .tab-content {
                                     background: #ffffff;
                                     border: none;
                                     border-radius: 0 0 8px 8px;
                                     padding: 20px;
                                     box-shadow: none;
                                     margin-left: 0 !important;
                                     margin-right: 0 !important;
                                   }
                                 "),

                               # Page hint, then the settings and analysis row
                               div(
                                 class = "se-page-hint",
                                 icon("circle-info"),
                                 tags$span("Hover a tab for details, then click Analyze. Analyses can take a while, so save current results first. Very lowly expressed genes are removed first, so a result may be empty.")
                               ),
                               fluidRow(id = "featurecorrelation-main-row",

                                 # Step 1: Common Settings(Left column, accounting for 3/12)
                                 div(
                                   class = "col-md-3",
                                   style = "margin-bottom: 10px;",
                                   div(
                                     style = "background: #f0fdf4; border: 1px solid #10b981; border-left: 4px solid #10b981; padding: 20px; border-radius: 8px; height: 100%;",
                                     h4(icon("sliders-h"), "Step 1: Settings", style = "color: #047857; margin-bottom: 15px; font-weight: 600; display: flex; align-items: center; gap: 8px;"),
                                     withSpinner(uiOutput("FeatureCorrelationClusterResolution.UI"), proxy.height = "10px"),
                                     withSpinner(uiOutput("FeatureCorrelationIdentsSelected.UI"), proxy.height = "10px"),
                                     withSpinner(uiOutput("FeatureCorrelationAssays.UI"), proxy.height = "10px"),
                                     selectInput("correlationmethod", "Correlation Method:", choices = c(pearson = "pearson", spearman = "spearman"))
                                   )
                                 ),

                                 # Step 2: Calculate Correlation (Right column, accounting for 9/12)
                                 div(
                                   class = "col-md-9",
                                   style = "margin-bottom: 10px;",
                                   div(id = "featurecorrelationtabbox-container",
                                     tabBox(
                                       # The title is laid out as its own row above the tabs
                                       # by the shared .nav-tabs > li.header rules.
                                       title = h4(
                                         icon("chart-line"),
                                         "Step 2: Calculate Correlation",
                                         style = "margin: 0; color: #1D4ED8; font-weight: 600; display: flex; align-items: center; gap: 8px;"
                                       ),
                                       id = "tabset_featurecorrelation", width = 12,
                                       tabPanel("Top correlated pairs",
                                                div(style = "max-width: 280px; margin: 0 auto 5px auto;",
                                                  numericInput("correlationTopN", "Top N gene pairs to return:",
                                                               value = 1000, min = 10, max = 100000, step = 100, width = '100%')
                                                ),
                                                div(style = "text-align: center; margin-top: 25px;",
                                                  actionButton("TopCorrelationAnalysis",
                                                              icon = shiny::icon("magnifying-glass-chart"),
                                                              label = "Analyze",
                                                              class = "btn-primary btn-action")
                                                )),
                                       tabPanel("Correlated with one gene",
                                                div(
                                                  # The tab card already carries the blue outline; keep the tint
                                                  # and left accent, drop the second full outline.
                                                  style = "background: #eff6ff; border-left: 4px solid #3b82f6; padding: 20px; border-radius: 8px; margin-bottom: 25px;",
                                                  textInput(inputId = "MostCorrelatedAGene", label = "Input a gene:", width = '100%')
                                                ),
                                                div(style = "text-align: center; margin-top: 25px;",
                                                  actionButton("MostCorrelatedAnalysis",
                                                              icon = shiny::icon("magnifying-glass-chart"),
                                                              label = "Analyze",
                                                              class = "btn-primary btn-action")
                                                )),
                                       tabPanel("All pairs in a gene list",
                                                div(
                                                  # The tab card already carries the blue outline; keep the tint
                                                  # and left accent, drop the second full outline.
                                                  style = "background: #eff6ff; border-left: 4px solid #3b82f6; padding: 20px; border-radius: 8px; margin-bottom: 25px;",
                                                  textAreaInput(inputId = "CorrelationGeneList", label = "Input a group of genes:", width = '100%', height = '100px', resize = "vertical")
                                                ),
                                                div(style = "text-align: center; margin-top: 25px;",
                                                  actionButton("calculatecorrelation",
                                                              icon = shiny::icon("save"),
                                                              label = "Analyze",
                                                              class = "btn-primary btn-action")
                                                ))
                                     )
                                   )
                                 ),

                                 # Analysis Results
                                 div(
                                   class = "col-xs-12",
                                   style = "margin-bottom: 10px;",
                                   conditionalPanel(
                                     condition = "!output.FeatureCorrelation_ready",
                                     div(class = "se-empty-state",
                                         icon("arrow-down"),
                                         tags$span("Set the options, then click Analyze. Results will appear here."))
                                   ),
                                   conditionalPanel(
                                     condition = "output.FeatureCorrelation_ready",
                                     div(
                                       class = "box",
                                       style = "background: white; border: 2px solid #3b82f6; border-radius: 8px; box-shadow: 0 2px 6px rgba(0,0,0,0.08);",
                                       div(
                                         class = "box-header",
                                         style = "padding: 15px 20px; border-bottom: 2px solid #3b82f6;",
                                         div(
                                           style = "display: flex; align-items: center; gap: 10px;",
                                           icon("table", style = "color: #1D4ED8; font-size: 18px;"),
                                           h4(style = "margin: 0; color: #1D4ED8; font-weight: 600;", "Analysis Results")
                                         )
                                       ),
                                       div(
                                         class = "box-body",
                                         style = "padding: 20px;",
                                         uiOutput("correlation_run_info"),
                                         # Single <table> (no scrollX); scroll on narrow screens.
                                         div(style = "overflow-x: auto;",
                                           withSpinner(DT::dataTableOutput('dataset_correlation')))
                                       )
                                     )
                                   )
                                 )
                               )
  )
  tab_list[["renameclusters"]] = tabItem(tabName = "renameclusters",
                               div(
                                 class = "se-page-hint",
                                 icon("circle-info"),
                                 tags$span("Rename the levels of one cluster resolution, check the preview, then Update. Download the mapping to reuse the names later.")
                               ),
                               fluidRow(id = "renameclusters-main-row",
                                 # Rename Clusters Table (Left column, accounting for 9/12)
                                 div(
                                   class = "col-md-9",
                                   div(
                                     class = "box",
                                     style = "background: white; border: 2px solid #3b82f6; border-radius: 8px; box-shadow: 0 2px 6px rgba(0,0,0,0.08); height: 100%;",
                                     div(
                                       class = "box-header",
                                       style = "padding: 15px 20px; border-bottom: 2px solid #3b82f6;",
                                       div(
                                         style = "display: flex; align-items: center; gap: 10px;",
                                         icon("table", style = "color: #1D4ED8; font-size: 18px;"),
                                         h4(style = "margin: 0; color: #1D4ED8; font-weight: 600;", "Rename Clusters")
                                       )
                                     ),
                                     div(
                                       class = "box-body",
                                       style = "padding: 20px;",
                                       withSpinner(DT::dataTableOutput('cell_annotation')),
                                       conditionalPanel(
                                         condition = "output.renameclusterscheck_OK",
                                         div(style = "margin-top: 20px;",
                                           plotOutput("renameclusterdimplot")
                                         )
                                       )
                                     )
                                   )
                                 ),

                                 # Settings (Right column, accounting for 3/12)
                                 div(
                                   class = "col-md-3",
                                   div(
                                     style = "background: #f0fdf4; border: 1px solid #10b981; border-left: 4px solid #10b981; padding: 20px; border-radius: 8px; height: 100%;",
                                     h4(icon("sliders-h"), "Settings", style = "color: #047857; margin-bottom: 15px; font-weight: 600; display: flex; align-items: center; gap: 8px;"),
                                     withSpinner(uiOutput("renameclustersClusterResolution.UI"), proxy.height = "10px"),
                                     withSpinner(uiOutput("renameclustersDimensionReduction.UI"), proxy.height = "10px"),
                                     sliderInput("renameclustersLabelSize", "Preview label size:", min = 1, max = 20, value = 7, step = 0.5),
                                     textInput('renameclustersNewClusterName', 'New column name:', value = "group"),
                                     div(
                                       style = "background: #eff6ff; padding: 10px; border-radius: 4px; margin-top: 15px;",
                                       uiOutput("renameclustersNewClusterNamehints.UI")
                                     ),
                                     div(style = "margin-top: 20px; text-align: center;",
                                       actionButton("renameclustersCheck",
                                                   icon = shiny::icon("check"),
                                                   label = "Check",
                                                   class = "btn-primary btn-action")
                                     ),
                                     conditionalPanel(
                                       condition = "output.renameclusterscheck_OK",
                                       div(style = "margin-top: 15px;",
                                         div(style = "display: flex; flex-wrap: wrap; gap: 10px; justify-content: center;",
                                           actionButton("renameclustersSubmit",
                                                       icon = shiny::icon("arrows-rotate"),
                                                       label = "Update",
                                                       class = "btn-primary btn-action"),
                                           downloadButton("renameclustersDownload",
                                                         "Download",
                                                         icon = shiny::icon("file-arrow-down"),
                                                         class = "btn-default btn-action")
                                         ),
                                         div(
                                           style = "background: #fef3c7; border-left: 3px solid #f59e0b; padding: 10px; border-radius: 4px; margin-top: 15px;",
                                           p("Tips: Download the cluster name mapping file to save new cluster names permanently.", style = "font-size: 12px; margin: 0; color: #92400e;")
                                         )
                                       )
                                     )
                                   )
                                 )
                               )
  )
  tab_list[["reorderclusters"]] = tabItem(tabName = "reorderclusters",
    div(
      class = "se-page-hint",
      icon("circle-info"),
      tags$span("Drag the levels into the order you want, then Apply. Only the level order changes; no cell is reassigned.")
    ),
    fluidRow(id = "reorderclusters-main-row",
      # Order preview (Left column, 9/12)
      div(
        class = "col-md-9",
        div(
          class = "box",
          style = "background: white; border: 2px solid #3b82f6; border-radius: 8px; box-shadow: 0 2px 6px rgba(0,0,0,0.08); height: 100%;",
          div(
            class = "box-header",
            style = "padding: 15px 20px; border-bottom: 2px solid #3b82f6;",
            div(
              style = "display: flex; align-items: center; gap: 10px;",
              icon("sort", style = "color: #1D4ED8; font-size: 18px;"),
              h4(style = "margin: 0; color: #1D4ED8; font-weight: 600;", "Order Preview")
            )
          ),
          div(
            class = "box-body",
            style = "padding: 20px;",
            withSpinner(plotOutput("reorderclustersdimplot", height = "560px")),
            p("Reordering only changes factor levels (legend, axis, split and stack order). No cell is reassigned to another cluster.",
              style = "font-size: 12px; color: #5C636A; margin-top: 10px;")
          )
        )
      ),

      # Settings (Right column, 3/12)
      div(
        class = "col-md-3",
        div(
          style = "background: #f0fdf4; border: 1px solid #10b981; border-left: 4px solid #10b981; padding: 20px; border-radius: 8px; height: 100%;",
          h4(icon("sliders-h"), "Settings",
             style = "color: #047857; margin-bottom: 15px; font-weight: 600; display: flex; align-items: center; gap: 8px;"),

          withSpinner(uiOutput("reorderclustersClusterResolution.UI"), proxy.height = "10px"),
          withSpinner(uiOutput("reorderclustersDimensionReduction.UI"), proxy.height = "10px"),
          sliderInput("reorderclustersLabelSize", "Preview label size:",
                      min = 1, max = 20, value = 7, step = 0.5),

          withSpinner(uiOutput("reorderclustersOrder.UI"), proxy.height = "10px"),

          div(
            style = "margin-top: 20px; display: flex; flex-wrap: wrap; gap: 10px; justify-content: center;",
            actionButton("reorderclustersApply",
                         icon = shiny::icon("check"),
                         label = "Apply New Order",
                         class = "btn-primary btn-action"),
            actionButton("reorderclustersReset",
                         icon = shiny::icon("arrows-rotate"),
                         label = "Reset",
                         class = "btn-warning btn-action",
                         style = "padding: 10px 20px; border-radius: 8px; font-weight: 600;")
          ),

          div(
            style = "background: #fef3c7; border-left: 3px solid #f59e0b; padding: 10px; border-radius: 4px; margin-top: 15px;",
            p("Tips: drag the levels into the desired order, then click Apply. This updates the selected metadata column in place and changes only the factor level order, not the cluster assignment of any cell.",
              style = "font-size: 12px; margin: 0; color: #92400e;")
          )
        )
      )
    )
  )
  tab_list[["clustercolors"]] = tabItem(tabName = "clustercolors",
    div(
      class = "se-page-hint",
      icon("circle-info"),
      tags$span("Choose a resolution, set the colors, then Submit. Saved colors are used by every plot that colours cells by cluster.")
    ),
    fluidRow(id = "clustercolors-main-row",

      # Settings
      div(
        class = "col-md-4",
        style = "margin-bottom: 10px;",
        div(
          style = "background: #f0fdf4; border: 1px solid #10b981; border-left: 4px solid #10b981; padding: 20px; border-radius: 8px; height: 100%;",
          h4(icon("palette"), "Settings", style = "color: #047857; margin-bottom: 15px; font-weight: 600; display: flex; align-items: center; gap: 8px;"),

          h5(icon("layer-group"), "Step 1: Cluster resolution",
             style = "color: #047857; font-weight: 600; display: flex; align-items: center; gap: 8px;"),
          withSpinner(uiOutput("ClusterColorsResolution.UI"), proxy.height = "10px"),

          hr(),
          h5(icon("palette"), "Step 2: Colors",
             style = "color: #047857; font-weight: 600; display: flex; align-items: center; gap: 8px;"),
          # flex-wrap + min-width:0 let the row shrink with the box: on a
          # narrow page the Apply button drops below the select instead of
          # being pushed outside the Settings box.
          div(style = "display: flex; flex-wrap: wrap; gap: 10px; align-items: flex-end;",
            div(style = "flex: 1 1 150px; min-width: 0;",
                selectInput("ClusterColorPalette", "Palette:",
                            choices = color_choice_vector, selected = "default",
                            width = "100%")),
            div(style = "flex: 0 0 auto;",
                actionButton("ClusterColorApplyPalette", "Apply",
                             icon = icon("wand-magic-sparkles"), class = "btn-primary btn-action-sm",
                             style = "margin-bottom: 15px; border-radius: 6px;"))
          ),
          div(style = "max-height: 320px; overflow-y: auto; padding-right: 4px;",
            uiOutput("ClusterColorPickers.UI")),
          checkboxInput("ClusterColorShowLabels", "Show cluster labels in preview", TRUE),
          conditionalPanel(
            condition = "input.ClusterColorShowLabels == true",
            sliderInput("ClusterColorLabelSize", "Label size:", min = 1, max = 20,
                        value = 7, step = 0.5)
          ),

          hr(),
          h5(icon("floppy-disk"), "Step 3: Save",
             style = "color: #047857; font-weight: 600; display: flex; align-items: center; gap: 8px;"),
          div(style = "display: flex; flex-wrap: wrap; gap: 8px;",
            actionButton("ClusterColorSubmit", "Submit", icon = icon("check"),
                         class = "btn-primary btn-action",
                         style = "flex: 1 1 110px; padding: 10px; border-radius: 8px; font-weight: 600;"),
            actionButton("ClusterColorReset", "Reset", icon = icon("arrows-rotate"),
                         class = "btn-warning btn-action",
                         style = "flex: 1 1 110px; padding: 10px; border-radius: 8px; font-weight: 600;")
          ),
          div(style = "margin-top: 8px;",
            actionLink("ClusterColorClearSaved", "Remove saved colors for this resolution",
                       style = "color: #dc3545; font-size: 12px;")),
          div(style = "margin-top: 8px;", textOutput("ClusterColorStatus")),

          hr(),
          downloadButton("download_cluster_colors", "Export colors (CSV)",
                         class = "btn-default btn-action",
                         style = "width: 100%; padding: 10px; border-radius: 8px; font-weight: 600;"),
          div(style = "margin-top: 10px;",
            fileInput("ClusterColorImportFile", "Import colors (CSV)",
                      accept = ".csv", buttonLabel = "Browse...",
                      placeholder = "Two columns: cluster,color")),
          p("CSV format: two columns named cluster and color (hex, e.g. #E41A1C).",
            style = "font-size: 11px; color: #5C636A; margin: 0;")
        )
      ),

      # Preview
      div(
        class = "col-md-8",
        style = "margin-bottom: 10px;",
        div(
          class = "box",
          style = "background: white; border: 2px solid #3b82f6; border-radius: 8px; box-shadow: 0 2px 6px rgba(0,0,0,0.08);",
          div(
            class = "box-header",
            style = "padding: 15px 20px; border-bottom: 2px solid #3b82f6;",
            div(
              style = "display: flex; align-items: center; gap: 10px;",
              icon("image", style = "color: #1D4ED8; font-size: 18px;"),
              h4(style = "margin: 0; color: #1D4ED8; font-weight: 600;", "Preview")
            )
          ),
          div(
            class = "box-body",
            style = "padding: 20px;",
            withSpinner(plotOutput("clustercolors_preview", height = "520px")),
            p("Colors are applied to Dim Reduction Plot, Violin Plot (un-split and side-by-side split), Cell Percentage Plot, Ridge Plot (colored by cluster), the Heatmap Cell Level group bar and the Average Heatmap annotation bar. Split violins use the colors saved for the Split by variable.",
              style = "font-size: 12px; color: #5C636A; margin-top: 10px;")
          )
        )
      )
    )
  )

  tab_list[["geneclusters"]] = tabItem(tabName = "geneclusters",
    div(
      class = "se-page-hint",
      icon("circle-info"),
      tags$span("Pick a gene or metadata column, generate the histogram, choose cutoffs, then Check and Submit to add the annotation.")
    ),
    fluidRow(
      box(title = "Gene Expression Histogram",
        plotOutput("geneclusters_histogram"),
        width = 9, status = "primary", collapsible = TRUE, solidHeader = TRUE),
      box(title = div(
            icon("sliders-h"),
            strong(" Plot Settings"),
            style = "display: flex; align-items: center; gap: 8px; color: white;"
          ),
          solidHeader = TRUE, status = "primary", width = 3,
        textInput("geneclustersGeneSymbol", "Gene Symbol or Metadata Column:", value = "", width = '100%'),
        div(
          style = "background-color: #e9ecef; border: 1px solid #3b82f6; padding: 5px; border-radius: 4px; margin-top: 5px; margin-bottom: 10px;",
          uiOutput("geneclusters_hints.UI")
        ),
        withSpinner(uiOutput("GeneclustersAssays.UI"), proxy.height = "10px"),
        withSpinner(uiOutput("geneclustersAssaySlots.UI"), proxy.height = "10px"),
        div(style = "margin-top: 15px;",
          actionButton("geneclustersPlotHistogram", label = "Generate Histogram",
            icon = icon("chart-bar"), class = "btn-primary btn-action",
            style = "width: 100%; padding: 10px; border-radius: 6px; font-weight: 600;")
        )
      )
    ),
    conditionalPanel(
      condition = "output.geneclusters_histogram_ready",
      fluidRow(
        div(class = "col-xs-12",
          style = "margin-top: 15px;",
          div(class = "box", style = "background: white; border: 2px solid #f59e0b; border-radius: 8px;",
            div(class = "box-body", style = "padding: 20px;",
              textInput("geneclustersCutoffs", "Cutoff values (comma-separated, e.g. 0.5, 1.0, 2.0):",
                value = "", width = "100%"),
              div(style = "margin-top: 10px;",
                actionButton("geneclustersApplyCutoffs", label = "Apply Cutoffs",
                  icon = icon("cut"), class = "btn-warning btn-action",
                  style = "padding: 10px 30px; border-radius: 6px; font-weight: 600;")
              )
            )
          )
        )
      )
    ),
    # Placeholder only once the cutoffs panel is available (histogram done) but
    # before the classification table exists.
    conditionalPanel(
      condition = "output.geneclusters_histogram_ready && !output.geneclusters_table_ready",
      div(class = "se-empty-state", style = "margin-top: 15px;",
          icon("arrow-down"),
          tags$span("Enter cutoff values above and click Apply Cutoffs. The classification table will appear here."))
    ),
    conditionalPanel(
      condition = "output.geneclusters_table_ready",
      fluidRow(style = "margin-top: 15px;",
        div(class = "col-md-9",
          div(class = "box", style = "background: white; border: 2px solid #3b82f6; border-radius: 8px;",
            div(class = "box-header", style = "padding: 15px 20px; border-bottom: 2px solid #3b82f6;",
              h4(icon("table"), "Classification Table", style = "color: #1D4ED8; font-weight: 600; margin: 0;")
            ),
            div(class = "box-body", style = "padding: 20px;",
              withSpinner(DT::dataTableOutput('geneclusters_table'))
            )
          )
        ),
        div(class = "col-md-3",
          div(style = "background: #f0fdf4; border: 1px solid #10b981; border-left: 4px solid #10b981; padding: 20px; border-radius: 8px;",
            h4(icon("check"), "Actions", style = "color: #047857; margin-bottom: 15px; font-weight: 600;"),
            div(style = "margin-bottom: 15px;",
              actionButton("geneclustersCheck", label = "Check", icon = icon("check"),
                class = "btn-primary btn-action", style = "width: 100%; padding: 10px; border-radius: 6px; font-weight: 600;")
            ),
            actionButton("geneclustersSubmit", label = "Submit", icon = icon("upload"),
              class = "btn-primary btn-action", style = "width: 100%; padding: 10px; border-radius: 6px; font-weight: 600;"),
            div(style = "margin-top: 15px;",
              downloadButton("geneclustersDownload", "Download Mapping", icon = icon("file-arrow-down"),
                class = "btn-default btn-action", style = "width: 100%; padding: 10px; border-radius: 6px; font-weight: 600;")
            ),
            div(style = "background: #fef3c7; border-left: 3px solid #f59e0b; padding: 10px; border-radius: 4px; margin-top: 15px;",
              p("Tips: Click 'Check' to preview, then 'Submit' to add annotation.", style = "font-size: 12px; margin: 0; color: #92400e;")
            )
          )
        )
      )
    )
  )
  tab_list[["addmodulescore"]] = tabItem(tabName = "addmodulescore",
    div(
      class = "se-page-hint",
      icon("circle-info"),
      tags$span("Paste a gene set, click Check Genes, then name the score and compute it.")
    ),
    fluidRow(
      box(title = "Module Score", solidHeader = TRUE, status = "primary", width = 12,
        textAreaInput("ModuleScoreFeatures", "Gene Symbols:",
          value = "", height = '100px', resize = "vertical",
          placeholder = "Paste gene symbols, one per line or comma-separated"),
        withSpinner(uiOutput("ModuleScoreAssays.UI"), proxy.height = "10px"),
        div(style = "margin-top: 15px;",
          actionButton("ModuleScoreCheckGenes", label = "Check Genes",
            icon = icon("check"), class = "btn-primary btn-action",
            style = "padding: 10px 30px; border-radius: 6px; font-weight: 600;")
        ),
        hr(),
        uiOutput("ModuleScoreName.UI"),
        uiOutput("ModuleScoreSubmit.UI")
      )
    ),
    uiOutput("ModuleScoreResult.UI")
  )
  tab_list[["clustercells"]] = tabItem(tabName = "clustercells",
    div(
      class = "se-page-hint",
      icon("circle-info"),
      tags$span("Choose a reduction and parameters, then Compute. Nothing is written to the object until you click Save.")
    ),
    fluidRow(
      div(
        class = "col-md-9",
        div(
          class = "box",
          style = "background: white; border: 2px solid #3b82f6; border-radius: 8px; box-shadow: 0 2px 6px rgba(0,0,0,0.08); height: 100%;",
          div(
            class = "box-header",
            style = "padding: 15px 20px; border-bottom: 2px solid #3b82f6;",
            div(
              style = "display: flex; align-items: center; gap: 10px;",
              icon("image", style = "color: #1D4ED8; font-size: 18px;"),
              h4(style = "margin: 0; color: #1D4ED8; font-weight: 600;", "Cluster Preview")
            )
          ),
          div(
            class = "box-body",
            style = "padding: 20px;",
            conditionalPanel(
              condition = "!output.ClusterCells_ready",
              div(class = "se-empty-state",
                  icon("arrow-down"),
                  tags$span("Set the parameters on the right and click Compute. The preview appears here."))
            ),
            conditionalPanel(
              condition = "output.ClusterCells_ready",
              uiOutput("ClusterCells_run_info"),
              # Plot the same clusters on any other embedding after Compute.
              div(style = "display: flex; align-items: center; gap: 10px; margin-bottom: 8px;",
                uiOutput("ClusterCellsPreviewReduction.UI")
              ),
              withSpinner(plotOutput("ClusterCellsPreview", height = "520px")),
              DT::dataTableOutput("ClusterCellsSummary"),
              div(style = "text-align: right; margin-top: 10px;",
                downloadButton("ClusterCellsDownload", "Download mapping (CSV)", class = "btn-default btn-action"))
            )
          )
        )
      ),
      div(
        class = "col-md-3",
        div(
          style = "background: #f0fdf4; border: 1px solid #10b981; border-left: 4px solid #10b981; padding: 20px; border-radius: 8px; height: 100%;",
          h4(icon("sliders-h"), "Settings", style = "color: #047857; margin-bottom: 15px; font-weight: 600; display: flex; align-items: center; gap: 8px;"),

          h5(icon("database"), "Data", style = "color: #047857; font-weight: 600; display: flex; align-items: center; gap: 8px;"),
          withSpinner(uiOutput("ClusterCellsReduction.UI"), proxy.height = "10px"),
          withSpinner(uiOutput("ClusterCellsDims.UI"), proxy.height = "10px"),
          withSpinner(uiOutput("ClusterCellsAssay.UI"), proxy.height = "10px"),

          hr(),
          h5(icon("layer-group"), "Graph", style = "color: #047857; font-weight: 600; display: flex; align-items: center; gap: 8px;"),
          numericInput("ClusterCellsK", "Neighbours (k.param):", value = 20, min = 2, step = 1, width = "100%"),
          numericInput("ClusterCellsPrune", "prune.SNN:", value = 0.0667, min = 0, max = 1, step = 0.01, width = "100%"),
          selectInput("ClusterCellsNNMethod", "nn.method:",
                      choices = c("annoy" = "annoy", "rann" = "rann"), width = "100%"),
          selectInput("ClusterCellsMetric", "annoy.metric:",
                      choices = c("euclidean" = "euclidean", "cosine" = "cosine", "manhattan" = "manhattan"), width = "100%"),
          numericInput("ClusterCellsTrees", "n.trees (annoy):", value = 50, min = 10, max = 200, step = 10, width = "100%"),

          hr(),
          h5(icon("chart-line"), "Clustering", style = "color: #047857; font-weight: 600; display: flex; align-items: center; gap: 8px;"),
          numericInput("ClusterCellsResolution", "resolution:", value = 0.8, min = 0.01, max = 10, step = 0.1, width = "100%"),
          selectInput("ClusterCellsAlgorithm", "algorithm:",
                      choices = c("1 - Louvain" = "1", "2 - Louvain refined" = "2",
                                  "3 - SLM" = "3", "4 - Leiden (needs leidenalg)" = "4"),
                      width = "100%"),
          numericInput("ClusterCellsSeed", "random.seed:", value = 0, min = 0, step = 1, width = "100%"),
          checkboxInput("ClusterCellsShowLabel", "Show cluster labels in preview", TRUE),

          hr(),
          h5(icon("tag"), "Output", style = "color: #047857; font-weight: 600; display: flex; align-items: center; gap: 8px;"),
          textInput("ClusterCellsName", "New metadata column:", value = "",
                    placeholder = "auto: assay_snn_res.<resolution>", width = "100%"),

          div(style = "text-align: center; margin-top: 15px;",
            actionButton("ClusterCellsCompute", "Compute",
                         icon = icon("play"),
                         class = "btn-primary btn-action")
          ),
          conditionalPanel(
            condition = "output.ClusterCells_ready",
            div(style = "text-align: center; margin-top: 10px;",
              actionButton("ClusterCellsSave", "Save to object",
                           icon = icon("check"),
                           class = "btn-primary btn-action",
                           style = "padding: 10px 20px; border-radius: 8px; font-weight: 600;")
            )
          )
        )
      )
    )
  )
  tab_list[["combinedclusters"]] = tabItem(tabName = "combinedclusters",
    div(
      class = "se-page-hint",
      icon("circle-info"),
      tags$span("Join two cluster resolutions with a connector, generate the table, then name and submit the new grouping.")
    ),
    fluidRow(
      box(title = "Combination Table",
        conditionalPanel(
          condition = "!output.combinedclusters_ready",
          div(class = "se-empty-state", style = "margin: 10px 0;",
              icon("arrow-down"),
              tags$span("Choose two resolutions and a connector on the right, then click Generate. The table will appear here."))
        ),
        conditionalPanel(
          condition = "output.combinedclusters_ready",
          DT::dataTableOutput('combinedclusters_table')
        ),
        width = 9, status = "primary", collapsible = TRUE, solidHeader = TRUE),
      box(title = div(
            icon("sliders-h"),
            strong(" Plot Settings"),
            style = "display: flex; align-items: center; gap: 8px; color: white;"
          ),
          solidHeader = TRUE, status = "primary", width = 3,
        withSpinner(uiOutput("CombinedClusterResolution.UI"), proxy.height = "10px"),
        withSpinner(uiOutput("CombinedClusterResolutionSecond.UI"), proxy.height = "10px"),
        selectInput("CombinedClusterConnector", "Connector:",
          choices = c("-" = "-", "+" = "+", "_" = "_"), selected = "_"),
        div(style = "margin-top: 15px;",
          actionButton("combinedclustersGenerate", label = "Generate",
            icon = icon("table"), class = "btn-primary btn-action",
            style = "width: 100%; padding: 10px; border-radius: 6px; font-weight: 600;")
        ),
        hr(),
        uiOutput("combinedclustersName.UI"),
        uiOutput("combinedclustersSubmit.UI")
      )
    )
  )
  tab_list[["featuresdf"]] = tabItem(tabName = "featuresdf",
                               div(
                                 class = "se-page-hint",
                                 icon("circle-info"),
                                 tags$span("Look up genes across assays. The table is read-only; use the search box to filter it.")
                               ),
                               fluidRow(id = "featuresdf-main-row",
                                 div(
                                   class = "col-xs-12",
                                   div(
                                     class = "box",
                                     style = "background: white; border: 2px solid #10b981; border-radius: 8px; box-shadow: 0 2px 6px rgba(0,0,0,0.08);",
                                     div(
                                       class = "box-header",
                                       style = "padding: 15px 20px; border-bottom: 2px solid #10b981;",
                                       div(
                                         style = "display: flex; align-items: center; gap: 10px;",
                                         icon("search", style = "color: #047857; font-size: 18px;"),
                                         h4(style = "margin: 0; color: #047857; font-weight: 600;", "Search Features")
                                       )
                                     ),
                                     div(
                                       class = "box-body",
                                       style = "padding: 20px;",
                                       withSpinner(uiOutput("FeaturesDataframeAssays.UI"), proxy.height = "10px"),
                                       withSpinner(DT::dataTableOutput('dataset_features'))
                                     )
                                   )
                                 )
                               )
  )
  tab_list[["cellmetadata"]] = tabItem(tabName = "cellmetadata",
                               div(
                                 class = "se-page-hint",
                                 icon("circle-info"),
                                 tags$span("Browse the cell-level metadata of the current object and download it as a CSV.")
                               ),
                               fluidRow(id = "cellmetadata-main-row",
                                 div(
                                   class = "col-xs-12",
                                   div(
                                     class = "box",
                                     style = "background: white; border: 2px solid #3b82f6; border-radius: 8px; box-shadow: 0 2px 6px rgba(0,0,0,0.08);",
                                     div(
                                       class = "box-header",
                                       style = "padding: 15px 20px; border-bottom: 2px solid #3b82f6;",
                                       div(
                                         style = "display: flex; align-items: center; gap: 10px;",
                                         icon("table", style = "color: #1D4ED8; font-size: 18px;"),
                                         h4(style = "margin: 0; color: #1D4ED8; font-weight: 600;", "Cell Metadata")
                                       )
                                     ),
                                     div(
                                       class = "box-body",
                                       style = "padding: 20px;",
                                       withSpinner(tagList(
                                         div(style = "margin-bottom: 15px; text-align: right;",
                                           downloadButton("download_meta_data", "Download", icon = shiny::icon("file-arrow-down"),
                                                         class = "btn-default btn-action")
                                         ),
                                         DT::dataTableOutput('dataset_meta')
                                       ))
                                     )
                                   )
                                 )
                               )
  )
  tab_list[["objectstructure"]] = tabItem(tabName = "objectstructure",
                               div(
                                 class = "se-page-hint",
                                 icon("circle-info"),
                                 tags$span("Inspect the slots of the Seurat object. Increase the depth to print nested slots.")
                               ),
                               fluidRow(id = "objectstructure-main-row",
                                 div(
                                   class = "col-xs-12",
                                   div(
                                     class = "box",
                                     style = "background: white; border: 2px solid #8b5cf6; border-radius: 8px; box-shadow: 0 2px 6px rgba(0,0,0,0.08);",
                                     div(
                                       class = "box-header",
                                       style = "padding: 15px 20px; border-bottom: 2px solid #8b5cf6;",
                                       div(
                                         style = "display: flex; align-items: center; gap: 10px;",
                                         icon("code", style = "color: #6D28D9; font-size: 18px;"),
                                         h4(style = "margin: 0; color: #6D28D9; font-weight: 600;", "Structure of Seurat Object")
                                       )
                                     ),
                                     div(
                                       class = "box-body",
                                       style = "padding: 20px;",
                                       div(
                                         style = "background: #f5f3ff; border: 1px solid #8b5cf6; border-left: 4px solid #8b5cf6; padding: 20px; border-radius: 8px; margin-bottom: 20px;",
                                         h4(icon("sliders-h"), "Display Settings", style = "color: #6D28D9; margin-bottom: 15px; font-weight: 600; display: flex; align-items: center; gap: 8px;"),
                                         sliderInput("ObjectStrutureLevel", label = "Structure Depth:", min = 1, max = 10, value = 3)
                                       ),
                                       withSpinner(verbatimTextOutput("object_structure"))
                                     )
                                   )
                                 )
                               )
  )
  # Object export. Can be turned off for multi-user deployments with
  # options(SeuratExplorerEnableObjectExport = FALSE).
  if (isTRUE(getOption("SeuratExplorerEnableObjectExport", TRUE))) {
    tab_list[["exportdata"]] = tabItem(tabName = "exportdata",
      div(
        class = "se-page-hint",
        icon("circle-info"),
        tags$span("Exports the in-session object with the metadata added here; the file on disk is never modified. A large object takes a while and the page stays unresponsive until the download starts.")
      ),
      fluidRow(id = "exportdata-main-row",

        # Settings
        div(
          class = "col-md-4",
          style = "margin-bottom: 10px;",
          div(
            style = "background: #f0fdf4; border: 1px solid #10b981; border-left: 4px solid #10b981; padding: 20px; border-radius: 8px; height: 100%;",
            h4(icon("sliders-h"), "Settings", style = "color: #047857; margin-bottom: 15px; font-weight: 600; display: flex; align-items: center; gap: 8px;"),

            h5(icon("filter"), "Step 1: Filter Cells (optional)",
               style = "color: #047857; font-weight: 600; display: flex; align-items: center; gap: 8px;"),
            p("Cells matching ALL conditions are kept. Two rows on the same column are intersected.",
              style = "color: #5C636A; font-size: 12px; margin-bottom: 10px;"),
            withSpinner(uiOutput("ExportFilterRows.UI"), proxy.height = "10px"),
            uiOutput("ExportAddFilter.UI"),
            checkboxInput("ExportKeepNA", "Keep cells with NA in a filter column", FALSE),

            hr(),
            h5(icon("file-arrow-down"), "Step 2: Output format",
               style = "color: #047857; font-weight: 600; display: flex; align-items: center; gap: 8px;"),
            radioButtons("ExportFormat", NULL,
                         choices = c("qs2 (QSZ compression)" = "qs2", "rds (R data)" = "rds"),
                         selected = "qs2", inline = TRUE),
            conditionalPanel(
              condition = "input.ExportFormat == 'rds'",
              selectInput("ExportRdsCompress", "Compression:",
                          choices = c("gzip (default)" = "gzip",
                                      "xz (smallest, slowest)" = "xz",
                                      "none (fastest, largest)" = "none"),
                          selected = "gzip")
            ),
            conditionalPanel(
              condition = "input.ExportFormat == 'qs2'",
              selectInput("ExportQsLevel", "Compression level:",
                          choices = c("3 (smallest, default)" = "3", "2 (balanced)" = "2",
                                      "1 (fastest)" = "1", "0 (none)" = "0"),
                          selected = "3")
            ),

            hr(),
            h5(icon("cogs"), "Step 3: Options",
               style = "color: #047857; font-weight: 600; display: flex; align-items: center; gap: 8px;"),
            checkboxInput("ExportDropScaleData", "Drop scale.data (smaller file)", FALSE),
            checkboxInput("ExportDroplevels", "Drop empty factor levels", FALSE),
            checkboxInput("ExportWriteProvenance", "Record export parameters in obj@misc", TRUE),

            div(style = "margin-top: 15px;",
              actionButton("ExportResetFilters", "Reset filters",
                           icon = icon("arrows-rotate"), class = "btn-warning btn-action",
                           style = "width: 100%; padding: 10px; border-radius: 8px; font-weight: 600;")),
            div(style = "margin-top: 15px;", uiOutput("ExportDownload.UI"))
          )
        ),

        # Preview
        div(
          class = "col-md-8",
          style = "margin-bottom: 10px;",
          div(
            class = "box",
            style = "background: white; border: 2px solid #3b82f6; border-radius: 8px; box-shadow: 0 2px 6px rgba(0,0,0,0.08);",
            div(
              class = "box-header",
              style = "padding: 15px 20px; border-bottom: 2px solid #3b82f6;",
              div(
                style = "display: flex; align-items: center; gap: 10px;",
                icon("table", style = "color: #1D4ED8; font-size: 18px;"),
                h4(style = "margin: 0; color: #1D4ED8; font-weight: 600;", "Export Preview")
              )
            ),
            div(
              class = "box-body",
              style = "padding: 20px;",
              uiOutput("ExportSummary.UI"),
              hr(),
              div(style = "max-width: 320px;", withSpinner(uiOutput("ExportPreviewGroup.UI"))),
              withSpinner(DT::dataTableOutput("export_preview_table"))
            )
          )
        )
      )
    )
  }

  tab_list[["about"]] = tabItem(tabName = "about",
                                fluidRow(
                                  box(title = "About Seurat Explorer", solidHeader = TRUE, status = "primary", width = 12,
                                      withSpinner(uiOutput("about_page")))
                                )
  )
  return(tab_list)
}


#' Shared global styles
#'
#' The style block used by both SeuratExplorer and SeuratExplorerServer, so the
#' two apps cannot drift apart.
#'
#' @return A `tags$head` object holding the shared styles.
#' @export
explorer_global_css <- function() {
      tags$head(
        tags$style(HTML("
          /* Global font optimization */
          body {
            font-family: -apple-system, BlinkMacSystemFont, 'Segoe UI', Roboto, 'Helvetica Neue', Arial, sans-serif;
          }
  
          /* Reserve the vertical scrollbar so auto-sized plots cannot feed a
             scrollbar-appears/disappears loop back into their measured width. */
          html {
            overflow-y: scroll;
            scrollbar-gutter: stable;
          }
  
          /* Sidebar menu: compact spacing */
          .sidebar-menu .treeview-menu li a {
            white-space: nowrap;
            padding: 4px 5px 4px 12px !important;
          }
  
          /* Panels sit at one fixed depth. A pointer passing over the page is
             not a state change, so the card does not lift on hover. */
          .box {
            border-radius: 6px;
            box-shadow: 0 1px 3px rgba(0,0,0,0.1);
          }

          .box.box-solid .box-title {
            font-weight: 600;
          }
  
          /* Optimize button style */
          .btn {
            border-radius: 6px;
            font-weight: 500;
            transition: background-color 0.15s ease, border-color 0.15s ease,
                        color 0.15s ease, box-shadow 0.15s ease;
          }

          /* A button shows hover through its fill, not by lifting off the page.
             Disabled controls have to read as inert at a glance. */
          .btn[disabled],
          .btn.disabled,
          fieldset[disabled] .btn {
            opacity: 0.65;
            box-shadow: none !important;
            cursor: not-allowed;
          }

          .btn-primary[disabled],
          .btn-primary.disabled {
            background-color: #cfd8e3 !important;
            border-color: #cfd8e3 !important;
            color: #475569 !important;
          }

          .btn-primary[disabled] .action-label,
          .btn-primary.disabled .action-label {
            color: #475569 !important;
          }

          /* One primary action style on every analysis page: same fill (the
             global primary), same height and radius. The section colour stays
             on the surrounding card instead of moving onto the button. */
          /* !important is deliberate: many buttons still carry an inline
             padding/radius/gradient. The class wins on those presentational
             properties only; inline width, flex and margin stay untouched. */
          .btn-action {
            padding: 10px 24px !important;
            border-radius: 6px !important;
            font-size: 14px !important;
            font-weight: 600 !important;
            line-height: 1.4;
            background-image: none !important;
            box-shadow: none !important;
          }

          /* Secondary action (download, copy, neutral toggles): same height
             as the primary action, neutral surface instead of the accent. */
          .btn-default {
            background-color: #ffffff !important;
            border-color: #cfd8e3;
            color: #374151;
          }

          .btn-default:hover,
          .btn-default:focus,
          .btn-default:active {
            background-color: #f3f7fa !important;
            border-color: #b6c2cc;
            color: #1f2937;
          }

          /* Compact action used inside panels: add filter, apply palette. */
          .btn-action-sm {
            padding: 5px 12px !important;
            border-radius: 4px !important;
            font-size: 13px !important;
            font-weight: 500 !important;
            line-height: 1.4;
            background-image: none !important;
            box-shadow: none !important;
          }
  
          /* Modal Dialog Styles */
          .modal-content {
            border-radius: 8px;
            box-shadow: 0 10px 25px rgba(0,0,0,0.15);
          }
  
          .modal-header {
            border-radius: 8px 8px 0 0;
            padding: 16px 20px;
          }
  
          .modal-body {
            padding: 20px;
          }
  
          .modal-footer {
            border-top: 1px solid #dee2e6;
            padding: 16px 20px;
          }
  
          .modal-footer .btn {
            border-radius: 6px;
            padding: 8px 16px;
            font-weight: 500;
          }
  
          /* Collapsible parameter group styles */
          summary::-webkit-details-marker,
          summary::marker {
            display: none;
          }
  
          .param-group-chevron {
            transition: transform 0.18s ease;
            display: inline-block;
          }
  
          details:not([open]) .param-group-chevron {
            transform: rotate(-90deg);
          }
  
          /* Accessibility: AA text tokens and fixed shinydashboard surfaces */
          :root {
            --se-primary: #2563EB;
            --se-primary-text: #1D4ED8;
            --se-success-text: #047857;
            --se-warning-text: #B45309;
            --se-pink-text: #BE185D;
            --se-violet-text: #6D28D9;
            --se-muted-text: #5C636A;
            /* Filled brand surface (header, primary buttons) and its states. */
            --se-accent: #256b8e;
            --se-accent-dark: #1e576f;
            --se-border: #cfd8e3;
            --se-focus: #1D4ED8;
          }
  
          .skin-blue .main-header .navbar,
          .skin-blue .main-header .logo,
          .skin-blue .main-header .logo:hover {
            background-color: #256b8e !important;
          }
  
          .sidebar-menu .treeview-menu > li > a {
            color: #c4d1d8 !important;
          }
  
          /* Current page in the sidebar. The muted colour above applies to every
             sub-item link with !important, which also hid the built-in active
             state; restate it explicitly so the selected page is obvious. */
          .sidebar-menu .treeview-menu > li.active > a,
          .sidebar-menu .treeview-menu > li.active > a:hover,
          .sidebar-menu .treeview-menu > li.active > a:focus {
            color: #ffffff !important;
            background-color: rgba(255, 255, 255, 0.14) !important;
            font-weight: 600;
            box-shadow: inset 3px 0 0 #67c1e8;
          }
  
          .box.box-solid.box-primary > .box-header {
            background-color: #256b8e !important;
          }

          /* AdminLTE paints a primary box outline with its own #3c8dbc. Keep
             the whole surface on the app accent instead of a second blue. */
          .box.box-primary {
            border-top-color: var(--se-accent);
          }

          .box.box-solid.box-primary {
            border-color: var(--se-accent);
          }
  
          .label-info,
          .badge-info {
            background-color: #0e7490 !important;
            color: #fff !important;
          }
  
          .sidebar-menu .label-info {
            font-size: 10px !important;
            padding: 2px 4px !important;
          }
  
          .btn-primary,
          .btn-primary:visited {
            background-color: #256b8e !important;
            border-color: #256b8e !important;
            color: #fff !important;
          }
  
          /* actionButton wraps its label in .action-label. Do not paint that
             inner span: it must stay transparent over the button surface. */
          .btn-primary .action-label,
          .btn-primary a {
            background-color: transparent !important;
            color: #fff !important;
          }

          /* The !important fill above also pins every pseudo-state, which left
             primary buttons with no visible hover. Restate the pressed and
             hovered fills explicitly, one step darker than the rest state. */
          .btn-primary:hover,
          .btn-primary:focus,
          .btn-primary:active,
          .btn-primary.active,
          .open > .dropdown-toggle.btn-primary {
            background-color: #1e576f !important;
            border-color: #1e576f !important;
          }
  
          .btn-info {
            background-color: #0e7490 !important;
            border-color: #0e7490 !important;
            color: #fff !important;
          }
  
          .btn-info .action-label {
            background-color: transparent !important;
            color: #fff !important;
          }

          .btn-info:hover,
          .btn-info:focus,
          .btn-info:active,
          .btn-info.active {
            background-color: #0b5f78 !important;
            border-color: #0b5f78 !important;
          }
  
          .btn-warning,
          .btn-warning:visited {
            background-color: #B45309 !important;
            border-color: #B45309 !important;
            color: #fff !important;
          }
  
          .btn-warning .action-label,
          .btn-warning a {
            background-color: transparent !important;
            color: #fff !important;
          }

          .btn-warning:hover,
          .btn-warning:focus,
          .btn-warning:active,
          .btn-warning.active {
            background-color: #92400e !important;
            border-color: #92400e !important;
          }
  
          .content-wrapper a {
            color: var(--se-primary-text);
          }
  
          #ClusterColorClearSaved {
            color: #B42318 !important;
            min-height: 24px;
            line-height: 24px;
            padding: 4px 0;
          }
  
          .bootstrap-select .filter-option-inner-inner,
          .bootstrap-select .bs-placeholder,
          .selectize-control .bs-placeholder,
          .selectize-control .filter-option-inner-inner,
          .selectize-control .selectized {
            color: var(--se-muted-text) !important;
          }
  
          input[id$='-selectized'] {
            min-width: 24px !important;
            min-height: 24px !important;
          }
  
          .irs-single,
          .irs-from,
          .irs-to {
            background-color: #256b8e !important;
            color: #fff !important;
          }
  
          .irs-bar {
            border-color: #256b8e !important;
            background-color: #256b8e !important;
          }

          /* In the 3/12 settings column a 10+ step slider prints every number
             under the track and they collide. Keep the track and the value
             bubble; drop only the unreadable grid numerals in that column. */
          .col-md-3 .irs-grid-text,
          .col-sm-3 .irs-grid-text {
            display: none !important;
          }

          /* jQuery-UI ships a blue striped corner grip on resizable plots,
             which belongs to no part of this palette. Replace it with a
             quiet grey grip that still reads as draggable. */
          .ui-resizable-handle {
            background-image: none !important;
          }

          .ui-resizable-se {
            width: 14px !important;
            height: 14px !important;
            right: 4px !important;
            bottom: 4px !important;
            border-radius: 3px;
            background-color: #cbd5e1 !important;
            box-shadow: inset 0 0 0 1px #94a3b8;
          }

          .ui-resizable-se:hover {
            background-color: #94a3b8 !important;
          }
  
          .form-control,
          .form-control:disabled,
          .form-control[readonly] {
            color: #495057;
          }
  
          .form-control::placeholder {
            color: var(--se-muted-text) !important;
            opacity: 1;
          }

          /* Theme the native focus glow from the palette instead of leaving
             Bootstrap's default #66afe9 halo. */
          .form-control:focus {
            border-color: var(--se-focus);
            box-shadow: 0 0 0 3px rgba(29, 78, 216, 0.15);
          }

          .selectize-control .selectize-input.focus {
            border-color: var(--se-focus);
            box-shadow: 0 0 0 3px rgba(29, 78, 216, 0.15);
          }

          /* Native checkboxes and radios follow the brand accent. */
          .checkbox input[type='checkbox'],
          .radio input[type='radio'] {
            accent-color: var(--se-accent);
          }
  
          .se-sr-only {
            position: absolute;
            width: 1px;
            height: 1px;
            padding: 0;
            margin: -1px;
            overflow: hidden;
            clip: rect(0, 0, 0, 0);
            white-space: nowrap;
            border: 0;
          }
  
          .box-tools .btn-box-tool {
            min-width: 28px;
            min-height: 28px;
            padding: 3px 4px;
          }
  
          .box-body details > summary {
            min-height: 32px;
            padding: 4px 0;
          }
  
          /* One focus ring for every interactive element, drawn outside the
             element so a panel's overflow never clips it. */
          .param-group-summary:focus-visible,
          .btn:focus-visible,
          .content-wrapper a:focus-visible,
          .se-page-hint-close:focus-visible,
          .se-hint-toggle:focus-visible,
          .se-colguide > summary:focus-visible {
            outline: 2px solid var(--se-focus);
            outline-offset: 2px;
            border-radius: 4px;
          }
  
          .selectize-control .selectize-input {
            min-height: 32px;
          }
  
          .selectize-control .selectized {
            min-width: 24px !important;
            min-height: 24px !important;
          }
  
          /* Keep native checkbox/radio sizing: Bootstrap positions the input
             with a negative margin, so enlarging it overlaps the label text. */
          .checkbox,
          .radio {
            min-height: 24px;
          }
  
          .checkbox label,
          .radio label {
            min-height: 24px;
            line-height: 24px;
          }
  
          /* tabBox title row.
             shinydashboard renders the tabBox() `title` as a right-floated
             <li class='header'> inside the tab strip, so it competes with the tab
             labels for the same line: on narrow windows it wraps to two lines or
             drops onto the strip's bottom border. Lay it out as its own
             full-width row above the tabs instead, so the tab labels never share
             a line with it. Applies to the DEGs and Feature Correlation tab
             boxes. */
          [id$='tabbox-container'] .nav-tabs-custom > ul.nav-tabs {
            display: flex;
            flex-wrap: wrap;
            align-items: flex-end;
          }
  
          [id$='tabbox-container'] .nav-tabs-custom > ul.nav-tabs > li.header {
            order: -1;
            flex: 0 0 100%;
            float: none !important;
            padding: 0 0 8px 0;
            margin: 0;
            line-height: 1.35;
            font-size: inherit;
            border: none;
          }
  
          /* Step card: header + plain form body. Used by the Top Genes page,
             where the two ranking modes are a segmented control rather than a
             tab strip, so the card keeps a single closed outline. */
          .se-step-card {
            background: #ffffff;
            border: 2px solid #3b82f6;
            border-radius: 8px;
            box-shadow: 0 2px 8px rgba(0,0,0,0.08);
          }
  
          .se-step-header {
            display: flex;
            align-items: center;
            gap: 8px;
            padding: 14px 20px 12px 20px;
            border-bottom: 1px solid #eef2f7;
          }
  
          .se-step-header h4 {
            margin: 0;
            font-size: 18px;
            font-weight: 600;
            color: #1D4ED8;
          }
  
          .se-step-body {
            padding: 20px;
          }
  
          .se-step-body .control-label,
          .se-step-body .se-field-label {
            display: block;
            font-weight: 600;
            color: #374151;
            margin: 0 0 8px 0;
          }
  
          .se-hint {
            margin: 8px 0 0 0;
            color: #5C636A;
            font-size: 13px;
            line-height: 1.5;
          }
  
          .se-actions {
            display: flex;
            justify-content: center;
            margin-top: 16px;
          }
  
          /* Short usage banner shown at the top of a page. */
          .se-page-hint {
            position: relative;
            display: flex;
            align-items: center;
            gap: 8px;
            margin: 0 0 10px 0;
            padding: 8px 36px 8px 12px;
            background: #eff6ff;
            border: 1px solid #bfdbfe;
            border-radius: 6px;
            color: #1e40af;
            font-size: 13px;
            line-height: 1.5;
          }
  
          .se-page-hint-close {
            position: absolute;
            top: 50%;
            right: 8px;
            transform: translateY(-50%);
            border: none;
            background: transparent;
            color: inherit;
            font-size: 18px;
            line-height: 1;
            padding: 0 6px;
            cursor: pointer;
            opacity: 0.65;
            border-radius: 4px;
          }
  
          .se-page-hint-close:hover,
          .se-page-hint-close:focus {
            opacity: 1;
            background: rgba(30, 64, 175, 0.12);
          }
  
          /* Floating control that brings the page hints back after they were
             dismissed; only visible while the hints are hidden. */
          .se-hint-toggle {
            position: fixed;
            right: 18px;
            bottom: 18px;
            z-index: 1040;
            width: 34px;
            height: 34px;
            padding: 0;
            border-radius: 50%;
            border: 1px solid #bfdbfe;
            background: #eff6ff;
            color: #1e40af;
            font-size: 15px;
            font-weight: 600;
            line-height: 1;
            cursor: pointer;
            box-shadow: 0 2px 6px rgba(0, 0, 0, 0.15);
            display: none;
            align-items: center;
            justify-content: center;
          }
  
          .se-hint-toggle:hover,
          .se-hint-toggle:focus {
            background: #dbeafe;
          }
  
          /* Placeholder shown in a results area before the first analysis run. */
          .se-empty-state {
            display: flex;
            align-items: center;
            justify-content: center;
            gap: 8px;
            padding: 22px 16px;
            border: 1px dashed #cbd5e1;
            border-radius: 8px;
            background: #f8fafc;
            color: #5C636A;
            font-size: 13px;
            text-align: center;
          }
  
          /* Collapsible column guide shown above result tables. */
          .se-colguide {
            margin: 0 0 10px 0;
          }
  
          .se-colguide > summary {
            display: inline-flex;
            align-items: center;
            gap: 6px;
            cursor: pointer;
            color: #1D4ED8;
            font-size: 13px;
            font-weight: 600;
            list-style: none;
          }
  
          .se-colguide > summary::-webkit-details-marker {
            display: none;
          }
  
          .se-colguide[open] > summary {
            margin-bottom: 8px;
          }
  
          .se-colguide-table {
            margin: 0;
            font-size: 13px;
            max-width: 900px;
          }
  
          .se-colguide-table > thead > tr > th {
            color: #374151;
            font-weight: 600;
            border-top: none;
          }
  
          .se-colguide-table > tbody > tr > td {
            color: #5C636A;
            vertical-align: top;
          }
  
          .se-colguide-raw {
            display: block;
            /* #9CA3AF sat at 2.6:1 on white; this tone clears 4.5:1. */
            color: #6B7280;
            font-size: 11px;
            margin-top: 2px;
          }
  
          /* Segmented method switch (#TopGenesMethod).
             shinyWidgets gives every button the same btn-<status> class, and the
             global .btn-primary rule paints them all with !important, so the
             selected button is indistinguishable. Restate both states
             explicitly: white outline for unselected, solid accent for selected. */
          #TopGenesMethod .btn-group {
            display: inline-flex;
          }
  
          #TopGenesMethod .btn-group > .btn {
            background: #ffffff !important;
            background-image: none !important;
            border: 1px solid #cfd8e3 !important;
            border-radius: 6px !important;
            color: #374151 !important;
            font-weight: 500;
            box-shadow: none !important;
            /* Space the pills apart and cancel Bootstrap's -1px collapsed border. */
            margin: 0 12px 0 0 !important;
            padding: 8px 16px;
          }
  
          #TopGenesMethod .btn-group > .btn:hover:not(.active) {
            background: #f3f6fb !important;
          }
  
          #TopGenesMethod .btn-group > .btn.active {
            background: #1D4ED8 !important;
            border-color: #1D4ED8 !important;
            color: #ffffff !important;
            font-weight: 600;
            box-shadow: none !important;
            position: relative;
            z-index: 2;
          }
  
          /* Two-column analysis pages (3/12 settings + 9/12 content).
             Bootstrap's two 15px gutters left a 30px channel between the two
             panels, wider than the 8px gap each page has between its Information
             box and the panel below. Narrow both gutters to 4px so the panel
             spacing matches in both directions. Only applies while the columns
             sit side by side; below md they stack and keep their normal gutters. */
          @media (min-width: 992px) {
            #topgenes-main-row > .col-md-3,
            #featuresummary-main-row > .col-md-3,
            #featurecorrelation-main-row > .col-md-3 {
              padding-right: 4px !important;
            }
  
            #topgenes-main-row > .col-md-9,
            #featuresummary-main-row > .col-md-9,
            #featurecorrelation-main-row > .col-md-9 {
              padding-left: 4px !important;
            }
          }
  
          /* Responsive optimization */
          @media (max-width: 768px) {
            .content-wrapper {
              padding: 10px;
            }

            .box {
              margin-bottom: 10px;
            }
          }

          /* Browser surfaces -------------------------------------------------
             Selection, scrollbars and table numerals ship with browser
             defaults. They are themed from the palette so the parts we did not
             draw still belong to the app. */
          ::selection {
            background: rgba(37, 107, 142, 0.22);
          }

          html {
            scrollbar-color: #b6c2cc #eef2f7;
          }

          ::-webkit-scrollbar {
            width: 10px;
            height: 10px;
          }

          ::-webkit-scrollbar-track {
            background: #eef2f7;
          }

          ::-webkit-scrollbar-thumb {
            background: #b6c2cc;
            border: 2px solid #eef2f7;
            border-radius: 6px;
          }

          ::-webkit-scrollbar-thumb:hover {
            background: #94a3b8;
          }

          /* Data tables use the app's body size, and digits share a width so
             numeric columns line up down the page. */
          .dataTables_wrapper {
            font-size: 13px;
          }

          table.dataTable td,
          table.dataTable th {
            font-variant-numeric: tabular-nums;
          }

          /* Highlight the row under the pointer so wide result tables can be
             read across without losing the line. */
          table.dataTable tbody tr:hover {
            background-color: #f3f7fa !important;
          }

          @media (prefers-reduced-motion: reduce) {
            *,
            *::before,
            *::after {
              transition-duration: 0.01ms !important;
              animation-duration: 0.01ms !important;
              animation-iteration-count: 1 !important;
            }
          }
        "))
      )
}

#' Shared client-side helpers
#'
#' Parameter tooltips, page-hint dismissal, accessibility fixes and the plot
#' width observer used by both SeuratExplorer and SeuratExplorerServer.
#'
#' @return A tag list of script blocks.
#' @export
explorer_global_scripts <- function() {
  tagList(
        # Parameter hints: put the one-line explanations from param_hints() on the
        # label of each matching input. Re-runs when Shiny binds an output and when
        # the DOM changes, so inputs created by renderUI get the tooltip too.
        tags$script(HTML(sprintf("
          (function() {
            var hints = %s;
            var optionHints = %s;
            function applyOptions() {
              Object.keys(optionHints).forEach(function(id) {
                var root = document.getElementById(id);
                if (!root) return;
                var map = optionHints[id];
                root.querySelectorAll('button, label.btn, a').forEach(function(btn) {
                  if (btn.title) return;
                  var text = (btn.textContent || '').trim();
                  if (map[text]) {
                    btn.title = map[text];
                    btn.style.cursor = 'help';
                  }
                });
              });
            }
            var HINT_KEY = 'se-page-hints-hidden';
            function hintsAreHidden() {
              try { return window.sessionStorage.getItem(HINT_KEY) === '1'; }
              catch (e) { return false; }
            }
            function setHintsHidden(hidden) {
              try {
                if (hidden) { window.sessionStorage.setItem(HINT_KEY, '1'); }
                else { window.sessionStorage.removeItem(HINT_KEY); }
              } catch (e) {}
              applyHintVisibility();
            }
            function applyHintVisibility() {
              var hidden = hintsAreHidden();
              document.querySelectorAll('.se-page-hint').forEach(function(box) {
                box.style.display = hidden ? 'none' : '';
              });
              var toggle = document.getElementById('se-hint-toggle');
              if (toggle) { toggle.style.display = hidden ? 'flex' : 'none'; }
            }
            function addHintCloseButtons() {
              document.querySelectorAll('.se-page-hint').forEach(function(box) {
                if (box.querySelector('.se-page-hint-close')) return;
                var btn = document.createElement('button');
                btn.type = 'button';
                btn.className = 'se-page-hint-close';
                btn.title = 'Hide all page hints';
                btn.setAttribute('aria-label', 'Hide all page hints');
                btn.textContent = String.fromCharCode(215);
                btn.addEventListener('click', function() { setHintsHidden(true); });
                box.appendChild(btn);
              });
              if (!document.getElementById('se-hint-toggle')) {
                var toggle = document.createElement('button');
                toggle.id = 'se-hint-toggle';
                toggle.type = 'button';
                toggle.className = 'se-hint-toggle';
                toggle.title = 'Show page hints';
                toggle.setAttribute('aria-label', 'Show page hints');
                toggle.textContent = '?';
                toggle.addEventListener('click', function() { setHintsHidden(false); });
                document.body.appendChild(toggle);
              }
              applyHintVisibility();
            }
            function apply() {
              Object.keys(hints).forEach(function(id) {
                var el = document.getElementById(id);
                if (!el || el.getAttribute('data-se-hint')) return;
                var label = null;
                if (el.labels && el.labels.length) { label = el.labels[0]; }
                if (!label && el.closest) {
                  var group = el.closest('.form-group');
                  if (group) { label = group.querySelector('label'); }
                }
                if (!label && el.closest) { label = el.closest('label'); }
                if (!label) { label = document.querySelector('label[for=' + id + ']'); }
                if (!label) return;
                if (!label.title) { label.title = hints[id]; }
                label.style.cursor = 'help';
                el.setAttribute('data-se-hint', '1');
              });
              applyOptions();
              addHintCloseButtons();
            }
            function later() { setTimeout(apply, 0); }
            if (window.jQuery) { jQuery(document).on('shiny:bound shiny:connected', later); }
            window.addEventListener('load', later);
            later();
            var timer = null;
            new MutationObserver(function() {
              clearTimeout(timer);
              timer = setTimeout(apply, 150);
            }).observe(document.body, { childList: true, subtree: true });
          })();
        ", param_hints_js(param_hints()), param_option_hints_js(param_option_hints())))),
        tags$script(HTML(
          "document.querySelector('body > div.wrapper > header > nav > div > ul > li > a > span').style.visibility = 'hidden';"
        )), # to hide how many notifications in shinydashboard::dropdownMenu(), refer to:https://stackoverflow.com/questions/65915414/alter-dropdown-menu-in-shiny
        tags$script(HTML("
          (function() {
            var lastWidth = null;
            var timer = null;
    
            function sendWidth(width) {
              clearTimeout(timer);
              timer = setTimeout(function() {
                if (window.Shiny) {
                  Shiny.setInputValue('dimplot_measured_width', Math.round(width),
                                      {priority: 'event'});
                }
              }, 200);
            }
    
            function measure() {
              var el = document.getElementById('dimplot_plot_container');
              if (!el) return;
              var width = el.getBoundingClientRect().width;
              if (!width) return;
              // Ignore scrollbar-width changes (~15-17px) otherwise the plot height
              // can toggle the scrollbar and feed an endless resize loop.
              if (lastWidth === null || Math.abs(width - lastWidth) >= 30) {
                lastWidth = width;
                sendWidth(width);
              }
            }
    
            if (window.ResizeObserver) {
              var observer = new ResizeObserver(measure);
              var tries = 0;
              (function attach() {
                var el = document.getElementById('dimplot_plot_container');
                if (el) {
                  observer.observe(el);
                  measure();
                } else if (tries++ < 40) {
                  setTimeout(attach, 250);
                }
              })();
            } else {
              $(window).on('resize', measure);
              measure();
            }
          })();
        ")),
        tags$script(HTML("
          (function() {
            document.documentElement.lang = 'en';
    
            function fixA11y() {
              // shinydashboard puts aria-selected on navigation links, where it is
              // not an allowed attribute. The visual active state is class-based.
              document.querySelectorAll('.sidebar-menu a[aria-selected]').forEach(function(a) {
                a.removeAttribute('aria-selected');
              });
    
              // Font Awesome icons carry aria-label with role=presentation. Keep
              // them decorative, but preserve the control name when the icon is
              // the only content of a button/link.
              document.querySelectorAll('i[aria-label]').forEach(function(i) {
                var label = i.getAttribute('aria-label');
                var control = i.closest('button,a');
                if (control && !control.getAttribute('aria-label') &&
                    !control.textContent.trim()) {
                  control.setAttribute('aria-label',
                                       control.getAttribute('title') ||
                                       label.replace(/ icon$/, ''));
                }
                i.removeAttribute('aria-label');
                i.setAttribute('aria-hidden', 'true');
              });
    
              // colourpicker renders label=NULL. Associate every picker with the
              // visible cluster label in the same row.
              document.querySelectorAll('input[id^=\"clustercolor_\"]').forEach(function(input) {
                if (input.getAttribute('aria-label')) return;
                var row = input.closest('.row');
                var label = row ? row.querySelector('.col-sm-5') : null;
                var text = label ? label.textContent.trim() : '';
                input.setAttribute('aria-label', text ? 'Color for ' + text : 'Cluster color');
              });
    
              // The header GitHub dropdown is icon-only.
              document.querySelectorAll('.main-header .dropdown-toggle').forEach(function(a) {
                if (!a.getAttribute('aria-label') && a.querySelector('.fa-github')) {
                  a.setAttribute('aria-label', 'GitHub repositories');
                }
              });
    
              // Legacy shinyWidgets::downloadBttn wraps an empty Shiny download
              // link in a button. Keep the link out of the tab order.
              document.querySelectorAll('.bttn .shiny-download-link').forEach(function(a) {
                a.style.display = 'none';
                a.setAttribute('aria-hidden', 'true');
                a.setAttribute('tabindex', '-1');
                a.setAttribute('aria-live', 'off');
              });
            }
    
            fixA11y();
            new MutationObserver(fixA11y).observe(document.body, {
              childList: true,
              subtree: true,
              attributes: true,
              attributeFilter: ['aria-label', 'role']
            });
          })();
        "))
  )
}

#' Shared head for SeuratExplorer and SeuratExplorerServer
#'
#' Convenience wrapper returning both the shared styles and the shared scripts.
#'
#' @return A tag list with the shared style and script blocks.
#' @export
explorer_global_head <- function() {
  tagList(
    explorer_global_css(),
    explorer_global_scripts()
  )
}

#' UI
#' @import shiny shinydashboard shinyWidgets
#' @export
#' @return the UI part of the shiny app
#' @examples
#' ui()
#'
ui <-  function(){
  # shinydashboard::notificationItem: the default function can not open link
  # to make a new function: refer to: https://forum.posit.co/t/shinydashboard-notification-item-with-link-in-new-tab/37580/2
  notificationItemWithAttr <- function(text, icon = shiny::icon("warning"), status = "success", href = NULL, ...) {
    if (is.null(href)){href <- "#"}
    icon <- shiny::tagAppendAttributes(icon, class = paste0("text-",status))
    shiny::tags$li(a(href = href, icon, text, ...))
  }

  # Header ----
  header <- shinydashboard::dashboardHeader(title = p(strong(em("Seurat Explorer"))),
                           tags$li(class = "dropdown", uiOutput("header_status")),
                           shinydashboard::dropdownMenu(type = "notifications", icon = shiny::icon("github"), headerText = "R packages on Github:",
                                        notificationItemWithAttr(icon = shiny::icon("github"), status = "info", text = "Seurat Explorer", href = "https://github.com/fentouxungui/SeuratExplorer", target = "_blank"),
                                        notificationItemWithAttr(icon = shiny::icon("github"), status = "info", text = "Seurat Explorer Server", href = "https://github.com/fentouxungui/SeuratExplorerServer", target = "_blank")))

  # Sidebar ----
  sidebar <- shinydashboard::dashboardSidebar(
    # tags$head(tags$style(HTML('* {font-family: "Times New Roman"};'))),
    shinydashboard::sidebarMenu(
      shinydashboard::menuItem("Dataset", tabName = "dataset", icon = shiny::icon("database")),
      explorer_sidebar_ui()
     )
  )

  # BODY ----
  tab_list <- list()

  tab_list[["dataset"]] <- shinydashboard::tabItem(tabName = "dataset",
                                  fluidRow(
                                    # upload a file
                                    box(status = "primary", title = "Upload Data", width = 12, collapsible = TRUE, solidHeader = TRUE,
                                        fileInput("dataset_file", "Choose a Seurat object file (.rds or .qs2):", accept = c('.rds', ".qs2")),
                                        div(style = "display: flex; align-items: center; text-align: center; color: #5C636A; margin: 15px 0;",
                                            div(style = "flex: 1; border-bottom: 1px dashed #dee2e6;"),
                                            span(style = "padding: 0 10px;", "OR"),
                                            div(style = "flex: 1; border-bottom: 1px dashed #dee2e6;")),
                                        div(style = "text-align: center;",
                                            actionButton("demo_data", "Download and run demo data", icon = icon("download"), class = "btn-primary btn-action"),
                                            tags$small(style = "display: block; color: #5C636A; margin-top: 8px;",
                                                       "Downloads a Fly gut demo dataset (~74 MB, 1000 cells) and loads it automatically. The file is cached locally for later use.")),
                                        uiOutput("dataOverview"))
                                  )
  )

  tab_list <- explorer_body_ui(tab_list = tab_list)

  body <- shinydashboard::dashboardBody(
    shinyjs::useShinyjs(),
    explorer_global_css(),
    div(class= "tab-content", tab_list),
    explorer_global_scripts()
  )

  # combine
  ui_out <- shinydashboard::dashboardPage(header, sidebar, body,
                                         title = "Seurat Explorer")
  return(ui_out)
}



