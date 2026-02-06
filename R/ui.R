# ui.R
## the UI side

#' some menu items of the dashboard sidebar
#' @description
#' to generate some menu items for the dashboard, which can be integrated to other
#' packages, such as 'fentouxungui/SeuratExplorerServer' from github.
#'
#' @import shiny markdown
#' @import shinydashboard shinyBS
#' @importFrom shinydashboard menuItem menuSubItem
#' @export
#' @return return some menu items for the dashboard
#' @examples
#' explorer_sidebar_ui()
#'
explorer_sidebar_ui <- function(){
  # to make shinyBS::updateCollapse() runs correctly, refer to: https://github.com/ebailey78/shinyBS/issues/92
  shiny::addResourcePath("sbs", system.file("www", package="shinyBS"))

  conditionalPanel(
    condition = "output.file_loaded",
    sidebarMenu(menuItem(text = "Explorer", tabName = "explorer", icon = shiny::icon("dashboard"), startExpanded = TRUE,
                         menuSubItem(text = "Dimensional Reduction Plot", tabName = "dimplot", icon = shiny::icon("angle-double-right")),
                         menuSubItem(text = "Feature Plot", tabName = "featureplot", icon = shiny::icon("angle-double-right")),
                         menuSubItem(text = "Violin Plot", tabName = "vlnplot", icon = shiny::icon("angle-double-right")),
                         menuSubItem(text = "Dot Plot", tabName = "dotplot", icon = shiny::icon("angle-double-right")),
                         menuSubItem(text = "Heatmap Cell Level", tabName = "heatmap", icon = shiny::icon("angle-double-right")),
                         menuSubItem(text = "Heatmap Group Averaged", tabName = "averagedheatmap", icon = shiny::icon("angle-double-right")),
                         menuSubItem(text = "Ridge Plot", tabName = "ridgeplot", icon = shiny::icon("angle-double-right")),
                         menuSubItem(text = "Cell Percentage Plot", tabName = "cellratioplot", icon = shiny::icon("angle-double-right")),
                         menuSubItem(text = "DEGs Analysis", tabName = "degs", icon = shiny::icon("angle-double-right")),
                         menuSubItem(text = "Top Expressed Features", tabName = "topgenes", icon = shiny::icon("angle-double-right")),
                         menuSubItem(text = "Feature Summary", tabName = "featuresummary", icon = shiny::icon("angle-double-right")),
                         menuSubItem(text = "Feature Correlation", tabName = "featurecorrelation", icon = shiny::icon("angle-double-right")),
                         menuSubItem(text = "Rename Clusters", tabName = "renameclusters", icon = shiny::icon("angle-double-right")),
                         menuSubItem(text = "Search Features", tabName = "featuresdf", icon = shiny::icon("angle-double-right")),
                         menuSubItem(text = "Cells Metadata", tabName = "cellmetadata", icon = shiny::icon("angle-double-right")),
                         menuSubItem(text = "Object Structure", tabName = "objectstructure", icon = shiny::icon("angle-double-right")),
                         menuSubItem(text = "About", tabName = "about", icon = shiny::icon("angle-double-right"))
                         )
                )
    )
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
                                  fluidRow(
                                    box(title = "Dimensional Reduction Plot",
                                        uiOutput("dimplot_resizable_ui"),
                                        # show the button on right end, refer to: https://stackoverflow.com/questions/28749693/shiny-r-aligning-buttons
                                        div(style = "display:inline-block; float:right",downloadBttn(outputId = "downloaddimplot",style = "bordered",color = "primary")),
                                        width = 9, status = "primary", collapsible = TRUE, solidHeader = TRUE),
                                    box(title = "Settings", solidHeader = TRUE, status = "primary", width = 3,
                                        withSpinner(uiOutput("DimDimensionReduction.UI"), proxy.height = "10px"),
                                        withSpinner(uiOutput("DimClusterResolution.UI"), proxy.height = "10px"),
                                        # refer to: https://ebailey78.github.io/shinyBS/docs/Collapses.html
                                        bsCollapse(id = "collapseDimplot", open = "Change Cluster Order",
                                                   bsCollapsePanel(title = "Change Cluster Order",
                                                                            withSpinner(uiOutput("DimClusterOrder.UI"), proxy.height = "10px"),
                                                                            style = "info")),
                                        withSpinner(uiOutput("DimSplit.UI"), proxy.height = "10px"),
                                        withSpinner(uiOutput("DimHighlightedClusters.UI"), proxy.height = "10px"),
                                        checkboxInput("DimShowLabel",label = "Show cluster label", TRUE),
                                        checkboxInput("DimShowLegend",label = "Show Legend", TRUE),
                                        sliderInput("DimLabelSize", label = "Label Size:", min = 0, max = 10, value = 7),
                                        sliderInput("DimPointSize", label = "Point Size", min = 0.001, max = 2, value = 0.8),
                                        checkboxInput("DimPlotMode",label = "Automatically adjust plotting area", TRUE),
                                        uiOutput("dimplot_size_ui")
                                    )
                                  )
  )
  tab_list[["featureplot"]] = tabItem(tabName = "featureplot",
                                      fluidRow(
                                        box(title = "Features on Dimensional Reduction Plot",
                                            uiOutput("featureplot_resizable_ui"),
                                            div(style = "display:inline-block; float:right", downloadBttn(outputId = "downloadfeatureplot",style = "bordered",color = "primary")),
                                            width = 9, status = "primary", collapsible = TRUE, solidHeader = TRUE),
                                        box(title = "Settings", solidHeader = TRUE, status = "primary", width = 3,
                                            textAreaInput("FeatureGeneSymbol", "Gene Symbol:", value = "", height = '80px', resize = "vertical"),
                                            div(
                                              style = "background-color: #e7f3ff; border-left: 4px solid #007bff; padding: 5px; border-radius: 4px;",
                                              uiOutput("Featurehints.UI")
                                            ),
                                            withSpinner(uiOutput("FeatureDimensionReduction.UI"), proxy.height = "10px"),
                                            withSpinner(uiOutput("FeatureAssays.UI"), proxy.height = "10px"),
                                            withSpinner(uiOutput("FeatureAssaySlots.UI"), proxy.height = "10px"),
                                            withSpinner(uiOutput("FeatureClusterResolution.UI"), proxy.height = "10px"),
                                            checkboxInput("FeatureShowLabel",label = "Show Cluster Label", FALSE),
                                            withSpinner(uiOutput("FeatureSplit.UI"), proxy.height = "10px"),
                                            sliderInput("FeatureLabelSize", label = "Label Size:", min = 1, max = 12, value = 4),
                                            # https://daattali.com/shiny/colourInput/
                                            colourpicker::colourInput("FeaturePlotLowestExprColor", "Pick Color for lowest expression:", "#E5E5E5", palette = "limited"),
                                            colourpicker::colourInput("FeaturePlotHighestExprColor", "Pick Color for highest expression:", "#FF0000",palette = "limited"),
                                            sliderInput("FeaturePointAlpha", label = "Point Alpha:", min = 0.1, max = 1, value = 1),
                                            sliderInput("FeatureMinCutoff", label = "Minimum expression cutoff by quantile:", min = 0, max = 100, value = 0),
                                            sliderInput("FeatureMaxCutoff", label = "Maximum expression cutoff by quantile::", min = 0, max = 100, value = 100),
                                            sliderInput("FeaturePointSize", label = "Point Size:", min = 0.001, max = 5, value = 0.8),
                                            checkboxInput("FeaturePlotMode",label = "Automatically adjust plotting area", TRUE),
                                            uiOutput("featureplot_size_ui")
                                        )
                                      )
  )
  tab_list[["vlnplot"]] = tabItem(tabName = "vlnplot",
                                  fluidRow(
                                    box(title = "Features Violin Plot",
                                        uiOutput("vlnplot_resizable_ui"),
                                        div(style = "display:inline-block; float:right", downloadBttn(outputId = "downloadvlnplot",style = "bordered",color = "primary")),
                                        width = 9, status = "primary", collapsible = TRUE, solidHeader = TRUE),
                                    box(title = "Settings", solidHeader = TRUE, status = "primary", width = 3,
                                        textAreaInput("VlnGeneSymbol", "Gene Symbols:", value = "", height = '80px', resize = "vertical"),
                                        div(
                                          style = "background-color: #e7f3ff; border-left: 4px solid #007bff; padding: 5px; border-radius: 4px;",
                                          uiOutput("Vlnhints.UI")
                                        ),
                                        withSpinner(uiOutput("VlnClusterResolution.UI"), proxy.height = "10px"),
                                        withSpinner(uiOutput("VlnIdentsSelected.UI"), proxy.height = "10px"),
                                        bsCollapse(id = "collapseVlnplot", open = "0",
                                                            bsCollapsePanel(title = "Change Cluster Order",
                                                                                     withSpinner(uiOutput("VlnClusterOrder.UI"), proxy.height = "10px"),
                                                                                     style = "info", value = "0")),
                                        withSpinner(uiOutput("VlnAssays.UI"), proxy.height = "10px"),
                                        withSpinner(uiOutput("VlnAssaySlots.UI"), proxy.height = "10px"),
                                        withSpinner(uiOutput("VlnSplitBy.UI"), proxy.height = "10px"),
                                        conditionalPanel(
                                          condition = "output.Vlnplot_splitoption_twolevels",
                                          checkboxInput("VlnSplitPlot",label = "Split Plot", FALSE)
                                        ),
                                        conditionalPanel(
                                          condition = "output.Vlnplot_multiple_genes",
                                          checkboxInput("VlnStackPlot",label = "Stack Plot", FALSE)
                                        ),
                                        conditionalPanel(
                                          condition = "output.Vlnplot_StackPlot",
                                          checkboxInput("VlnFlipPlot",label = "Flip Plot", FALSE)
                                        ),
                                        conditionalPanel(
                                          condition = "output.Vlnplot_StackPlot && input.VlnSplitBy == 'None'", # only work when split is set to NULL
                                          selectInput("VlnFillBy","Color By:", choices = c(Feature = "feature", Cluster = "ident")),
                                        ),
                                        conditionalPanel(
                                          condition = "input.VlnSplitBy == 'None'", # only work when split is set to NULL
                                          selectInput("Vlnfillcolorplatte","select color plate:", choices = color_choice_vector, selected = "Default")
                                        ),
                                        sliderInput("VlnPointSize", label = "Point Size:", min = 0, max = 4, value = 0),
                                        sliderInput("VlnPointAlpha", label = "Point Alpha:", min = 0, max = 1, value = 1),
                                        sliderInput("VlnXlabelSize", label = "x Axis Label Size:", min = 0, max = 20, value = 14),
                                        sliderInput("VlnYlabelSize", label = "Y Axis Label Size:", min = 0, max = 20, value = 10),
                                        checkboxInput("VlnPlotMode",label = "Automatically adjust plotting area", TRUE),
                                        uiOutput("vlnplot_size_ui")
                                    )
                                  )
  )
  tab_list[["dotplot"]] = tabItem(tabName = "dotplot",
                                  fluidRow(
                                    box(title = "Features Dot Plot",
                                        uiOutput("dotplot_resizable_ui"),
                                        div(style = "display:inline-block; float:right", downloadBttn(outputId = "downloaddotplot",style = "bordered",color = "primary")),
                                        width = 9, status = "primary", collapsible = TRUE, solidHeader = TRUE),
                                    box(title = "Settings", solidHeader = TRUE, status = "primary", width = 3,
                                        textAreaInput("DotGeneSymbol", "Gene Symbols:", value = "", height = '80px', resize = "vertical"),
                                        div(
                                          style = "background-color: #e7f3ff; border-left: 4px solid #007bff; padding: 5px; border-radius: 4px;",
                                          uiOutput("Dothints.UI")
                                        ),
                                        withSpinner(uiOutput("DotClusterResolution.UI"), proxy.height = "10px"),
                                        withSpinner(uiOutput("DotIdentsSelected.UI"), proxy.height = "10px"),
                                        bsCollapse(id = "collapseDotplot", open = "0",
                                                            bsCollapsePanel(title = "Change Cluster Order",
                                                                                     withSpinner(uiOutput("DotClusterOrder.UI"), proxy.height = "10px"),
                                                                                     style = "info", value = "0")),
                                        withSpinner(uiOutput("DotSplitBy.UI"), proxy.height = "10px"),
                                        withSpinner(uiOutput("DotAssays.UI"), proxy.height = "10px"),
                                        checkboxInput("DotClusterIdents",label = "Cluster the Clusters", FALSE),
                                        checkboxInput("DotRotateAxis",label = "Rotate x Axis Lables", FALSE),
                                        checkboxInput("DotFlipCoordinate",label = "Flip XY Coordinate", FALSE),
                                        conditionalPanel(
                                          condition = "output.DotPlot_Split_isNone",
                                          colourpicker::colourInput("DotPlotLowestExprColor", "Pick Color for lowest expression:", "#E5E5E5", palette = "limited"),
                                          colourpicker::colourInput("DotPlotHighestExprColor", "Pick Color for highest expression:", "#0000FF",palette = "limited"),
                                        ),
                                        sliderInput("DotDotScale", label = "Dot Scale:", min = 1, max = 12, value = 6),
                                        sliderInput("DotXlabelSize", label = "x Axis Label Size:", min = 0, max = 20, value = 14),
                                        sliderInput("DotYlabelSize", label = "Y Axis Label Size:", min = 0, max = 20, value = 10),
                                        checkboxInput("DotPlotMode",label = "Automatically adjust plotting area", TRUE),
                                        uiOutput("dotplot_size_ui")
                                    )
                                  )
  )
  tab_list[["heatmap"]] = tabItem(tabName = "heatmap",
                                  fluidRow(
                                    box(title = "Features Heatmap Plot",
                                        uiOutput("heatmap_resizable_ui"),
                                        div(style = "display:inline-block; float:right", downloadBttn(outputId = "downloadheatmap",style = "bordered",color = "primary")),
                                        width = 9, status = "primary", collapsible = TRUE, solidHeader = TRUE),
                                    box(title = "Settings", solidHeader = TRUE, status = "primary", width = 3,
                                        textAreaInput("HeatmapGeneSymbol", "Gene Symbols:", value = "", height = '80px', resize = "vertical"),
                                        div(
                                          style = "background-color: #e7f3ff; border-left: 4px solid #007bff; padding: 5px; border-radius: 4px;",
                                          uiOutput("Heatmaphints.UI")
                                        ),
                                        withSpinner(uiOutput("HeatmapClusterResolution.UI"), proxy.height = "10px"),
                                        withSpinner(uiOutput("HeatmapIdentsSelected.UI"), proxy.height = "10px"),
                                        bsCollapse(id = "collapseHeatmap", open = "0",
                                                            bsCollapsePanel(title = "Change Cluster Order",
                                                                                     withSpinner(uiOutput("HeatmapClusterOrder.UI"), proxy.height = "10px"),
                                                                                     style = "info", value = "0")),
                                        withSpinner(uiOutput("HeatmapAssays.UI"), proxy.height = "10px"),
                                        withSpinner(uiOutput("HeatmapAssaySlots.UI"), proxy.height = "10px"),
                                        sliderInput("HeatmapTextSize", label = "Cluster Text Size:", min = 1, max = 12, value = 6, step = 0.5),
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
  tab_list[["averagedheatmap"]] = tabItem(tabName = "averagedheatmap",
                                  fluidRow(
                                    box(title = "Features Heatmap by Averaged Expression",
                                        uiOutput("averagedheatmap_resizable_ui"),
                                        div(style = "display:inline-block; float:right", downloadBttn(outputId = "downloadaveragedheatmap",style = "bordered",color = "primary")),
                                        width = 9, status = "primary", collapsible = TRUE, solidHeader = TRUE),
                                    box(title = "Settings", solidHeader = TRUE, status = "primary", width = 3,
                                        textAreaInput("AveragedHeatmapGeneSymbol", "Gene Symbols:", value = "", height = '80px', resize = "vertical"),
                                        div(
                                          style = "background-color: #e7f3ff; border-left: 4px solid #007bff; padding: 5px; border-radius: 4px;",
                                          uiOutput("AveragedHeatmaphints.UI")
                                        ),
                                        withSpinner(uiOutput("AveragedHeatmapClusterResolution.UI"), proxy.height = "10px"),
                                        withSpinner(uiOutput("AveragedHeatmapIdentsSelected.UI"), proxy.height = "10px"),
                                        bsCollapse(id = "collapseHeatmap", open = "0",
                                                            bsCollapsePanel(title = "Change Cluster Order",
                                                                                     withSpinner(uiOutput("AveragedHeatmapClusterOrder.UI"), proxy.height = "10px"),
                                                                                     style = "info", value = "0")),
                                        withSpinner(uiOutput("AveragedHeatmapAssays.UI"), proxy.height = "10px"),
                                        sliderInput("AveragedHeatmapClusterTextSize", label = "Cluster Text Size:", min = 1, max = 30, value = 12),
                                        sliderInput("AveragedHeatmapClusterTextRatateAngle", label = "Cluster Text Rotate Angle:", min = -90, max = 90, value = 45),
                                        sliderInput("AveragedHeatmapFeatureTextSize", label = "Feature Text Size:", min = 1, max = 20, value = 10),
                                        checkboxInput("AveragedHeatmapClusterClusters",label = "Cluster Clusters", FALSE),
                                        checkboxInput("AveragedHeatmapClusterFeatures",label = "Cluster Features", FALSE),
                                        checkboxInput("AveragedHeatmapPlotMode",label = "Automatically adjust plotting area", TRUE),
                                        uiOutput("averagedheatmap_size_ui")
                                    )
                                  )
  )
  tab_list[["ridgeplot"]] = tabItem(tabName = "ridgeplot",
                                    fluidRow(
                                      box(title = "Features Ridge Plot",
                                          uiOutput("ridgeplot_resizable_ui"),
                                          div(style = "display:inline-block; float:right", downloadBttn(outputId = "downloadridgeplot",style = "bordered",color = "primary")),
                                          width = 9, status = "primary", collapsible = TRUE, solidHeader = TRUE),
                                      box(title = "Settings", solidHeader = TRUE, status = "primary", width = 3,
                                          textAreaInput("RidgeplotGeneSymbol", "Gene Symbols:", value = "", height = '80px', resize = "vertical"),
                                          div(
                                            style = "background-color: #e7f3ff; border-left: 4px solid #007bff; padding: 5px; border-radius: 4px;",
                                            uiOutput("Ridgeplothints.UI")
                                          ),
                                          withSpinner(uiOutput("RidgeplotClusterResolution.UI"), proxy.height = "10px"),
                                          withSpinner(uiOutput("RidgeplotIdentsSelected.UI"), proxy.height = "10px"),
                                          bsCollapse(id = "collapseRidgeplot", open = "0",
                                                              bsCollapsePanel(title = "Change Cluster Order",
                                                                                       withSpinner(uiOutput("RidgeplotClusterOrder.UI"), proxy.height = "10px"),
                                                                                       style = "info", value = "0")),
                                          withSpinner(uiOutput("RidgeplotAssays.UI"), proxy.height = "10px"),
                                          withSpinner(uiOutput("RidgeplotAssaySlots.UI"), proxy.height = "10px"),
                                          conditionalPanel(
                                            condition = "output.Ridgeplot_stack_NotSelected",
                                            sliderInput("RidgeplotNumberOfColumns", label = "Number of columns:", min = 1, max = 10, value = 1),
                                          ),
                                          conditionalPanel(
                                            condition = "output.Ridgeplot_stack_show",
                                            checkboxInput("RidgeplotStackPlot",label = "Stack Plot", FALSE),
                                          ),
                                          conditionalPanel(
                                            condition = "input.RidgeplotStackPlot",
                                            selectInput("RidgeplotFillBy","Color By:", choices = c(Feature = "feature", Cluster = "ident"))
                                          ),
                                          sliderInput("RidgeplotXlabelSize", label = "x Axis Label Size:", min = 0, max = 20, value = 14),
                                          sliderInput("RidgeplotYlabelSize", label = "Y Axis Label Size:", min = 0, max = 20, value = 10),
                                          checkboxInput("RidgeplotPlotMode",label = "Automatically adjust plotting area", TRUE),
                                          uiOutput("ridgeplot_size_ui")
                                      )
                                    )
  )
  tab_list[["cellratioplot"]] = tabItem(tabName = "cellratioplot",
                                    fluidRow(
                                      box(title = "Cell Percentage Plot",
                                          uiOutput("cellratioplot_resizable_ui"),
                                          div(style = "display:inline-block; float:right", downloadBttn(outputId = "downloadcellratioplot",style = "bordered",color = "primary")),
                                          div(style = "margin-top: 50px;",
                                              hr(),
                                              DT::dataTableOutput('cellratiodata'),
                                              align="center"),
                                          width = 9, status = "primary", collapsible = TRUE, solidHeader = TRUE),
                                      box(title = "Settings", solidHeader = TRUE, status = "primary", width = 3,
                                          # Fill in part
                                          withSpinner(uiOutput("CellratioFillChoice.UI"), proxy.height = "10px"),
                                          # subset clusters
                                          withSpinner(uiOutput("CellratioIdentsSelected.UI"), proxy.height = "10px"),
                                          # order clusters
                                          bsCollapse(id = "collapseCellratioFillplot", open = "0",
                                                              bsCollapsePanel(title = "Change Order",
                                                                                       withSpinner(uiOutput("CellratioplotFillOrder.UI"), proxy.height = "10px"),
                                                                                       style = "info", value = "0")),
                                          # X axis part
                                          withSpinner(uiOutput("CellratioXChoice.UI"), proxy.height = "10px"),
                                          bsCollapse(id = "collapseCellratioXplot", open = "0",
                                                              bsCollapsePanel(title = "Change Order",
                                                                                       withSpinner(uiOutput("CellratioplotXOrder.UI"), proxy.height = "10px"),
                                                                                       style = "info", value = "0")),

                                          # facet part
                                          withSpinner(uiOutput("CellratioFacetChoice.UI"), proxy.height = "10px"),
                                          bsCollapse(id = "collapseCellratioFacetplot", open = "0",
                                                              bsCollapsePanel(title = "Change Order",
                                                                                       withSpinner(uiOutput("CellratioplotFacetOrder.UI"), proxy.height = "10px"),
                                                                                       style = "info", value = "0")),
                                          selectInput("Cellratiofillcolorplatte","select color plate:", choices = color_choice_vector, selected = "Default"),
                                          checkboxInput("CellratioRotateAxis",label = "Rotate X Axis", FALSE),
                                          sliderInput("CellratioColumnWidth", label = "Column width:", min = 0, max = 1, value = 0.7),
                                          sliderInput("CellratioFlowAlpha", label = "Flow alpha:", min = 0, max = 1, value = 0.3),
                                          sliderInput("CellratioFlowCurve", label = "Flow curve:", min = 0, max = 1, value = 0.3),
                                          checkboxInput("CellratioMode",label = "Automatically adjust plotting area", TRUE),
                                          uiOutput("cellratioplot_size_ui")
                                      )
                                    )
  )
  tab_list[["degs"]] = tabItem(tabName = "degs",
                               fluidRow(
                                 shinydashboardPlus::box(title = 'Information', textOutput("degs_info"),
                                                         background = "green", width = 12, closable = TRUE),
                                 tags$style(type="text/css", "#degs_info {white-space: pre-wrap;}"),
                                 # Outputting multiple lines of text with renderText() in R shiny
                                 # https://stackoverflow.com/questions/23233497/outputting-multiple-lines-of-text-with-rendertext-in-r-shiny
                                 tags$style(".nav-tabs {background: #f4f4f4;}
                                 .nav-tabs-custom .nav-tabs li.active:hover a, .nav-tabs-custom .nav-tabs li.active a {background-color: #fff;
                                 border-color: #fff;
                                 }
                                 .nav-tabs-custom .nav-tabs li.active {border-top-color:
                                 #314a6d;
                                 }"), # refer to: https://stackoverflow.com/questions/45247290/shiny-dashboard-tabbox-tabpanel-css
                                 # attention: all tabBox will use style above!
                                 tabBox(
                                   title = "Find Markers or DEGs",
                                   id = "tabset_degs", width = 12, # height = "250px",
                                   tabPanel("Find Markers for All Clusters",
                                            withSpinner(uiOutput("ClusterMarkersClusterResolution.UI"), proxy.height = "10px"),
                                            actionButton("DEGsClusterMarkersAnalysis", "Analyze", icon = shiny::icon("magnifying-glass-chart"), class = "btn-primary")),
                                   tabPanel("Find DEGs for two groups",
                                            strong(h3("Step 1: Filter Cells (Optional)")),
                                            p("Modify parameters bellow if you want to filter cells before the comparison, otherwise ignore it."),
                                            p("Only the selected cells will be kept for Step2, and all cells will be kept by default."),
                                            withSpinner(uiOutput("IntraClusterDEGsSubsetCells.UI"), proxy.height = "10px"),
                                            withSpinner(uiOutput("IntraClusterDEGsSubsetCellsSelectedClusters.UI"), proxy.height = "10px"),
                                            tags$hr(style="border: none; border-top: 1px dashed #ccc;"),
                                            strong(h3("Step 2: Set the comparision")),
                                            withSpinner(uiOutput("IntraClusterDEGsCustomizedGroups.UI"), proxy.height = "10px"),
                                            withSpinner(uiOutput("IntraClusterDEGsCustomizedGroupsCase.UI"), proxy.height = "10px"),
                                            withSpinner(uiOutput("IntraClusterDEGsCustomizedGroupsControl.UI"), proxy.height = "10px"),
                                            tags$hr(style="border: none; border-top: 1px dashed #ccc;"),
                                            actionButton("IntraClusterDEGssAnalysis", "Analyze", icon = shiny::icon("magnifying-glass-chart"), class = "btn-primary")),
                                   tabPanel("Custom Parameters",
                                            withSpinner(uiOutput("DEGsAssays.UI"), proxy.height = "10px"),
                                            sliderInput("logfcthreshold", label = "Logfc Threshold:", min = 0, max = 1, value = 0.1),
                                            selectInput("testuse","Test use:", choices = c(wilcox = "wilcox", wilcox_limma = "wilcox_limma",
                                                                                           T_test = "t", negbinom = "negbinom", poisson = "poisson",
                                                                                           LR = "LR", MAST = "MAST", DESeq2 = "DESeq2")),
                                            sliderInput("minpct", label = "Minimum Expression Percentage:", min = 0, max = 1, value = 0.01),
                                            sliderInput("mindiffpct", label = "Minimum Expression Percentage Difference:", min = 0, max = 1, value = 0),
                                            actionButton("SetDefault", "Set to Default", icon = shiny::icon("save"), class = "btn-primary"))

                                 ),
                                 conditionalPanel(
                                   condition = "output.DEGs_ready",
                                   box(title = "Analysis Results:", collapsible = TRUE, width = 8,align = "center",
                                       withSpinner(DT::dataTableOutput('dataset_degs'))),
                                   conditionalPanel(
                                     condition = "output.DEGs_row_selected",
                                     box(title = "External Links:", collapsible = TRUE, width = 4,
                                         selectInput("selectspecies", "Choose Species:", choices = c("Human" = "human","Mouse" = "mouse","Fly" = "fly"), width = '180px'),
                                         selectInput("selectsgenetype", "Choose Feature Types:", choices = c("Symbol" = "Symbol","Ensembl ID" = "Ensembl","Entrez ID" = "EntrezID"), width = '180px'),
                                         withSpinner(uiOutput('ExternalLinks.UI')))
                                   )
                                 )
                               )
  )
  tab_list[["topgenes"]] = tabItem(tabName = "topgenes",
                               fluidRow(
                                 shinydashboardPlus::box(title = 'Information', textOutput("topgenes_info"), background = "green", width = 12, closable = TRUE),
                                 tags$style(type="text/css", "#topgenes_info {white-space: pre-wrap;}"),
                                 box(title = "Step1: Common Settings", solidHeader = TRUE, status = "primary", width = 3,
                                 withSpinner(uiOutput("TopGenesClusterResolution.UI"), proxy.height = "10px"),
                                 withSpinner(uiOutput("TopGenesSelectedClusters.UI"), proxy.height = "10px"),
                                 withSpinner(uiOutput("TopGenesAssays.UI"), proxy.height = "10px"),
                                 checkboxInput("TopGenesClusterLevel",label = "by each cluster", TRUE)),
                                 shinydashboard::tabBox(
                                   title = "Step2: Calcuate Top Genes",
                                   id = "tabset_topgenes", width = 9, # height = "250px",
                                   tabPanel("Find Top Genes by Cell", # strong(h3("Top Correlated Genes")),
                                            sliderInput("TopGenesTopPercent","UMI percentage cutoff(%):",min = 1,  max = 10, value = 1, step = 1),
                                            actionButton("TopGenesAnalysis", "Analyze", icon = shiny::icon("magnifying-glass-chart"), class = "btn-primary")),
                                   tabPanel("Find Top Genes by Accumulated UMI Counts",
                                            sliderInput("TopGenesTopN","Top n:",min = 100,  max = 1000, value = 100, step = 100),
                                            actionButton("TopAccumulatedGenesAnalysis", "Analyze", icon = shiny::icon("magnifying-glass-chart"), class = "btn-primary"))),
                                 conditionalPanel(
                                   condition = "output.TopGenes_ready",
                                   box(title = "Analysis Results:", collapsible = TRUE, width = 12,solidHeader = TRUE, status = "primary",align = "center",
                                       withSpinner(DT::dataTableOutput('dataset_topgenes'))))
                               )
  )
  tab_list[["featuresummary"]] = tabItem(tabName = "featuresummary",
                                   fluidRow(
                                     shinydashboardPlus::box(title = 'Information',textOutput("featuresummary_info"), background = "green", width = 12, closable = TRUE),
                                     tags$style(type="text/css", "#featuresummary_info {white-space: pre-wrap;}"),
                                     box(title = "Settings", solidHeader = TRUE, status = "primary", width = 3,
                                         textAreaInput("FeatureSummarySymbol", "Input Gene Symbols:", value = "", height = '100px', resize = "vertical"),
                                         withSpinner(uiOutput("FeatureSummaryClusterResolution.UI"), proxy.height = "10px"),
                                         withSpinner(uiOutput("FeatureSummarySelectedClusters.UI"), proxy.height = "10px"),
                                         withSpinner(uiOutput("FeatureSummaryAssays.UI"), proxy.height = "10px"),
                                         checkboxInput("FeatureSummaryClusterLevel",label = "by each cluster", TRUE),
                                         actionButton("FeatureSummaryAnalysis", "Submit", icon = shiny::icon("magnifying-glass-chart"), class = "btn-primary")),
                                     box(title = "Gene Short Summary:", collapsible = TRUE, width = 9,solidHeader = TRUE, status = "primary",align = "center",
                                         conditionalPanel(
                                           condition = "output.FeatureSummary_ready",
                                           withSpinner(DT::dataTableOutput('dataset_featuresummary')))
                                     )
                                   )
  )
  tab_list[["featurecorrelation"]] = tabItem(tabName = "featurecorrelation",
                                         fluidRow(
                                           shinydashboardPlus::box(title = 'Information',textOutput("featurecorrelation_info"), background = "green", width = 12, closable = TRUE),
                                           tags$style(type="text/css", "#featurecorrelation_info {white-space: pre-wrap;}"),
                                           box(title = "Step1: Common Settings", solidHeader = TRUE, status = "primary", width = 3,
                                               withSpinner(uiOutput("FeatureCorrelationClusterResolution.UI"), proxy.height = "10px"),
                                               withSpinner(uiOutput("FeatureCorrelationIdentsSelected.UI"), proxy.height = "10px"),
                                               withSpinner(uiOutput("FeatureCorrelationAssays.UI"), proxy.height = "10px"),
                                               selectInput("correlationmethod","Correlation Method:", choices = c(pearson = "pearson", spearman = "spearman"))),
                                           shinydashboard::tabBox(
                                             title = "Step2: Calcuate Correlation",
                                             id = "tabset_featurecorrelation", width = 9, # height = "250px",
                                             tabPanel("Find Top Correlated Gene Pairs", # strong(h3("Top Correlated Genes")),
                                                      actionButton("TopCorrelationAnalysis", "Analyze", icon = shiny::icon("magnifying-glass-chart"), class = "btn-primary")),
                                             tabPanel("Find Top Correlated Genes for A Gene", # strong(h3("Find correlated genes for a gene")),
                                                      textInput(inputId = "MostCorrelatedAGene", label = "Input a gene:", width = '30%'),
                                                      actionButton("MostCorrelatedAnalysis", "Analyze", icon = shiny::icon("magnifying-glass-chart"), class = "btn-primary")),
                                             tabPanel("Calcuate Correlation of All Pairs in A Gene List", # strong(h3("Calculate correlation for a gene group")),
                                                      textAreaInput(inputId = "CorrelationGeneList", label = "Input a group of genes:", width = '30%', height = '100px', resize = "vertical"),
                                                      actionButton("calculatecorrelation", "Analyze", icon = shiny::icon("save"), class = "btn-primary"))

                                           ),
                                           conditionalPanel(
                                             condition = "output.FeatureCorrelation_ready",
                                             box(title = "Analysis Results:", collapsible = TRUE, width = 12, solidHeader = TRUE, status = "primary",align = "center",
                                                 withSpinner(DT::dataTableOutput('dataset_correlation')))
                                           )
                                         )
  )
  tab_list[["renameclusters"]] = tabItem(tabName = "renameclusters",
                                             fluidRow(
                                               box(title = "Rename Clusters", solidHeader = TRUE, status = "primary", width = 9,
                                                   withSpinner(DT::dataTableOutput('cell_annotation')),
                                                   # verbatimTextOutput("updated_df_output"), # for debug use
                                                   conditionalPanel(
                                                     condition = "output.renameclusterscheck_OK",
                                                     plotOutput("renameclusterdimplot"),
                                                   )
                                                   ),
                                               box(title = "Settings", solidHeader = TRUE, status = "primary", width = 3,
                                                   withSpinner(uiOutput("renameclustersClusterResolution.UI"), proxy.height = "10px"),
                                                   withSpinner(uiOutput("renameclustersDimensionReduction.UI"), proxy.height = "10px"),
                                                   textInput('renameclustersNewClusterName', 'Input Cluster name:', value = "group"),
                                                   withSpinner(uiOutput("renameclustersNewClusterNamehints.UI"), proxy.height = "10px"),
                                                   actionButton("renameclustersCheck", "Check", icon = shiny::icon("check"), class = "btn-primary"),
                                                   conditionalPanel(
                                                     condition = "output.renameclusterscheck_OK",
                                                     div(style = "margin-top: 10px;",
                                                         actionButton("renameclustersSubmit", "Update", icon = shiny::icon("arrows-rotate"), class = "btn-primary"),
                                                         downloadButton("renameclustersDownload", "Download", icon = shiny::icon("file-arrow-down"), class = "btn-primary")
                                                         ),
                                                   ))
                                             )
  )
  tab_list[["featuresdf"]] = tabItem(tabName = "featuresdf",
                                             fluidRow(
                                               box(title = "Search Features", solidHeader = TRUE, status = "primary", width = 12,
                                                   withSpinner(uiOutput("FeaturesDataframeAssays.UI"), proxy.height = "10px"),
                                                   withSpinner(DT::dataTableOutput('dataset_features')))
                                             )
  )
  tab_list[["cellmetadata"]] = tabItem(tabName = "cellmetadata",
                                     fluidRow(
                                       box(title = "Metadata of Cells", collapsible = TRUE, width = 12,solidHeader = TRUE, status = "primary", # align = "center",
                                           withSpinner(tagList(downloadButton("download_meta_data","Download"),
                                                               DT::dataTableOutput('dataset_meta'))))
                                     )
  )
  tab_list[["objectstructure"]] = tabItem(tabName = "objectstructure",
                                       fluidRow(
                                         box(title = "Structure of Seurat Object", collapsible = TRUE, width = 12,solidHeader = TRUE, status = "primary",
                                             sliderInput("ObjectStrutureLevel", label = "Structure Depth:", min = 1, max = 10, value = 3),
                                             withSpinner(verbatimTextOutput("object_structure")))
                                       )
  )
  tab_list[["about"]] = tabItem(tabName = "about",
                                fluidRow(
                                  box(title = "About Seurat Explorer", solidHeader = TRUE, status = "primary", width = 12,
                                      HTML(markdown::markdownToHTML(knitr::knit(system.file("extdata", "README.Rmd", package ="SeuratExplorer"), quiet=T),fragment.only = T)))
                                )
  )
  return(tab_list)
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
                                        fileInput("dataset_file", "Choose A rds or qs2 file of Seurat Object:", accept = c('.rds', ".qs2")))
                                    )
                                  )

  tab_list <- explorer_body_ui(tab_list = tab_list)

  body <- shinydashboard::dashboardBody(
    # tags$head(tags$style(HTML('* {font-family: "Times New Roman"};'))),
    div(class= "tab-content", tab_list),
    tags$script(HTML(
      "document.querySelector('body > div.wrapper > header > nav > div > ul > li > a > span').style.visibility = 'hidden';"
    )) # to hide how many notifications in shinydashboard::dropdownMenu(), refer to:https://stackoverflow.com/questions/65915414/alter-dropdown-menu-in-shiny
  )

  # combine
  ui_out <- shinydashboard::dashboardPage(header, sidebar, body,
                                         title = "Seurat Explorer")
  return(ui_out)
}



