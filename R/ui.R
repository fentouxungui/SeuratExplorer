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
                                    box(title = div(
                                          icon("sliders-h"),
                                          strong(" Plot Settings"),
                                          style = "display: flex; align-items: center; gap: 8px; color: white;"
                                        ),
                                        solidHeader = TRUE, status = "primary", width = 3,
                                        # Data Selection Group
                                        div(
                                          style = "background: #f8f9fa; border: 1px solid #3b82f6; border-left: 4px solid #3b82f6; padding: 12px; border-radius: 6px; margin-bottom: 15px;",
                                          h5(icon("database"), "Data Selection", style = "color: #3b82f6; font-size: 14px; font-weight: 600; margin-bottom: 10px;"),
                                          withSpinner(uiOutput("DimDimensionReduction.UI"), proxy.height = "10px"),
                                          withSpinner(uiOutput("DimClusterResolution.UI"), proxy.height = "10px")
                                        ),
                                        # Display Options Group
                                        div(
                                          style = "background: #f8f9fa; border: 1px solid #10b981; border-left: 4px solid #10b981; padding: 12px; border-radius: 6px; margin-bottom: 15px;",
                                          h5(icon("eye"), "Display Options", style = "color: #10b981; font-size: 14px; font-weight: 600; margin-bottom: 10px;"),
                                          checkboxInput("DimShowLabel",label = "Show cluster label", TRUE),
                                          checkboxInput("DimShowLegend",label = "Show legend", TRUE)
                                        ),
                                        # Size Settings Group
                                        div(
                                          style = "background: #f8f9fa; border: 1px solid #f59e0b; border-left: 4px solid #f59e0b; padding: 12px; border-radius: 6px; margin-bottom: 15px;",
                                          h5(icon("adjust"), "Size Settings", style = "color: #f59e0b; font-size: 14px; font-weight: 600; margin-bottom: 10px;"),
                                          sliderInput("DimLabelSize", label = "Label Size:", min = 0, max = 10, value = 7),
                                          sliderInput("DimPointSize", label = "Point Size:", min = 0.001, max = 2, value = 0.8)
                                        ),
                                        # Advanced Options
                                        div(
                                          style = "background: #f8f9fa; border: 1px solid #6b7280; border-left: 4px solid #6b7280; padding: 12px; border-radius: 6px; margin-bottom: 15px;",
                                          h5(icon("cogs"), "Advanced Options", style = "color: #6b7280; font-size: 14px; font-weight: 600; margin-bottom: 10px;"),
                                          withSpinner(uiOutput("DimSplit.UI"), proxy.height = "10px"),
                                          withSpinner(uiOutput("DimHighlightedClusters.UI"), proxy.height = "10px"),
                                          bsCollapse(id = "collapseDimplot", open = "Change Cluster Order",
                                                     bsCollapsePanel(title = "Change Cluster Order",
                                                                              withSpinner(uiOutput("DimClusterOrder.UI"), proxy.height = "10px"),
                                                                              style = "info")),
                                          checkboxInput("DimPlotMode",label = "Automatically adjust plotting area", TRUE),
                                          uiOutput("dimplot_size_ui")
                                        )
                                    )
                                  )
  )
  tab_list[["featureplot"]] = tabItem(tabName = "featureplot",
                                      fluidRow(
                                        box(title = "Features on Dimensional Reduction Plot",
                                            uiOutput("featureplot_resizable_ui"),
                                            div(style = "display:inline-block; float:right", downloadBttn(outputId = "downloadfeatureplot",style = "bordered",color = "primary")),
                                            width = 9, status = "primary", collapsible = TRUE, solidHeader = TRUE),
                                        box(title = div(
                                              icon("sliders-h"),
                                              strong(" Plot Settings"),
                                              style = "display: flex; align-items: center; gap: 8px; color: white;"
                                            ),
                                            solidHeader = TRUE, status = "primary", width = 3,
                                            # Gene Input Group
                                            div(
                                              style = "background: #f8f9fa; border: 1px solid #3b82f6; border-left: 4px solid #3b82f6; padding: 12px; border-radius: 6px; margin-bottom: 15px;",
                                              h5(icon("dna"), "Gene Selection", style = "color: #3b82f6; font-size: 14px; font-weight: 600; margin-bottom: 10px;"),
                                              textAreaInput("FeatureGeneSymbol", "Gene Symbol:", value = "", height = '80px', resize = "vertical"),
                                              div(
                                                style = "background-color: #e9ecef; border: 1px solid #3b82f6; padding: 5px; border-radius: 4px; margin-top: 5px; margin-bottom: 10px;",
                                                uiOutput("Featurehints.UI")
                                              )
                                            ),
                                            # Data Selection Group
                                            div(
                                              style = "background: #f8f9fa; border: 1px solid #10b981; border-left: 4px solid #10b981; padding: 12px; border-radius: 6px; margin-bottom: 15px;",
                                              h5(icon("database"), "Data Selection", style = "color: #10b981; font-size: 14px; font-weight: 600; margin-bottom: 10px;"),
                                              withSpinner(uiOutput("FeatureDimensionReduction.UI"), proxy.height = "10px"),
                                              withSpinner(uiOutput("FeatureAssays.UI"), proxy.height = "10px"),
                                              withSpinner(uiOutput("FeatureAssaySlots.UI"), proxy.height = "10px"),
                                              withSpinner(uiOutput("FeatureClusterResolution.UI"), proxy.height = "10px")
                                            ),
                                            # Display Options Group
                                            div(
                                              style = "background: #f8f9fa; border: 1px solid #f59e0b; border-left: 4px solid #f59e0b; padding: 12px; border-radius: 6px; margin-bottom: 15px;",
                                              h5(icon("eye"), "Display Options", style = "color: #f59e0b; font-size: 14px; font-weight: 600; margin-bottom: 10px;"),
                                              checkboxInput("FeatureShowLabel",label = "Show cluster label", FALSE),
                                              withSpinner(uiOutput("FeatureSplit.UI"), proxy.height = "10px"),
                                              sliderInput("FeatureLabelSize", label = "Label Size:", min = 1, max = 12, value = 4)
                                            ),
                                            # Color Settings Group
                                            div(
                                              style = "background: #f8f9fa; border: 1px solid #8b5cf6; border-left: 4px solid #8b5cf6; padding: 12px; border-radius: 6px; margin-bottom: 15px;",
                                              h5(icon("palette"), "Color Settings", style = "color: #8b5cf6; font-size: 14px; font-weight: 600; margin-bottom: 10px;"),
                                              colourpicker::colourInput("FeaturePlotLowestExprColor", "Lowest expression color:", "#E5E5E5", palette = "limited"),
                                              colourpicker::colourInput("FeaturePlotHighestExprColor", "Highest expression color:", "#FF0000", palette = "limited")
                                            ),
                                            # Advanced Settings Group
                                            div(
                                              style = "background: #f8f9fa; border: 1px solid #6b7280; border-left: 4px solid #6b7280; padding: 12px; border-radius: 6px; margin-bottom: 15px;",
                                              h5(icon("cogs"), "Advanced Settings", style = "color: #6b7280; font-size: 14px; font-weight: 600; margin-bottom: 10px;"),
                                              sliderInput("FeaturePointAlpha", label = "Point Alpha:", min = 0.1, max = 1, value = 1),
                                              sliderInput("FeatureMinCutoff", label = "Min expression cutoff (%):", min = 0, max = 100, value = 0),
                                              sliderInput("FeatureMaxCutoff", label = "Max expression cutoff (%):", min = 0, max = 100, value = 100),
                                              sliderInput("FeaturePointSize", label = "Point Size:", min = 0.001, max = 5, value = 0.8),
                                              checkboxInput("FeaturePlotMode",label = "Auto-adjust plotting area", TRUE),
                                              uiOutput("featureplot_size_ui")
                                            )
                                        )
                                      )
  )
  tab_list[["vlnplot"]] = tabItem(tabName = "vlnplot",
                                  fluidRow(
                                    box(title = "Features Violin Plot",
                                        uiOutput("vlnplot_resizable_ui"),
                                        div(style = "display:inline-block; float:right", downloadBttn(outputId = "downloadvlnplot",style = "bordered",color = "primary")),
                                        width = 9, status = "primary", collapsible = TRUE, solidHeader = TRUE),
                                    box(title = div(
                                          icon("sliders-h"),
                                          strong(" Plot Settings"),
                                          style = "display: flex; align-items: center; gap: 8px; color: white;"
                                        ),
                                        solidHeader = TRUE, status = "primary", width = 3,

                                        # Data Selection
                                        div(
                                          style = "background: #f8f9fa; border: 1px solid #3b82f6; border-left: 4px solid #3b82f6; padding: 12px; border-radius: 6px; margin-bottom: 15px;",
                                          h5(icon("database"), "Data Selection", style = "color: #3b82f6; margin-bottom: 10px; font-size: 14px; font-weight: 600;"),
                                          textAreaInput("VlnGeneSymbol", "Gene Symbols:", value = "", height = '80px', resize = "vertical"),
                                          div(
                                            style = "background-color: #e9ecef; border: 1px solid #3b82f6; padding: 5px; border-radius: 4px; margin-top: 5px; margin-bottom: 10px;",
                                            uiOutput("Vlnhints.UI")
                                          ),
                                          withSpinner(uiOutput("VlnAssays.UI"), proxy.height = "10px"),
                                          withSpinner(uiOutput("VlnAssaySlots.UI"), proxy.height = "10px"),
                                          withSpinner(uiOutput("VlnSplitBy.UI"), proxy.height = "10px")
                                        ),

                                        # Cluster Settings
                                        div(
                                          style = "background: #f8f9fa; border: 1px solid #10b981; border-left: 4px solid #10b981; padding: 12px; border-radius: 6px; margin-bottom: 15px;",
                                          h5(icon("layer-group"), "Cluster Settings", style = "color: #10b981; margin-bottom: 10px; font-size: 14px; font-weight: 600;"),
                                          withSpinner(uiOutput("VlnClusterResolution.UI"), proxy.height = "10px"),
                                          withSpinner(uiOutput("VlnIdentsSelected.UI"), proxy.height = "10px"),
                                          bsCollapse(id = "collapseVlnplot", open = "0",
                                                            bsCollapsePanel(title = "Change Cluster Order",
                                                                                     withSpinner(uiOutput("VlnClusterOrder.UI"), proxy.height = "10px"),
                                                                                     style = "info", value = "0"))
                                        ),

                                        # Plot Options
                                        div(
                                          style = "background: #f8f9fa; border: 1px solid #f59e0b; border-left: 4px solid #f59e0b; padding: 12px; border-radius: 6px; margin-bottom: 15px;",
                                          h5(icon("sliders-h"), "Plot Options", style = "color: #f59e0b; margin-bottom: 10px; font-size: 14px; font-weight: 600;"),
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
                                          sliderInput("VlnXlabelSize", label = "X Axis Label Size:", min = 0, max = 20, value = 14),
                                          sliderInput("VlnYlabelSize", label = "Y Axis Label Size:", min = 0, max = 20, value = 10),
                                          checkboxInput("VlnPlotMode",label = "Automatically adjust plotting area", TRUE),
                                          uiOutput("vlnplot_size_ui")
                                        )
                                    )
                                  )
  )
  tab_list[["dotplot"]] = tabItem(tabName = "dotplot",
                                  fluidRow(
                                    box(title = "Features Dot Plot",
                                        uiOutput("dotplot_resizable_ui"),
                                        div(style = "display:inline-block; float:right", downloadBttn(outputId = "downloaddotplot",style = "bordered",color = "primary")),
                                        width = 9, status = "primary", collapsible = TRUE, solidHeader = TRUE),
                                    box(title = div(
                                          icon("sliders-h"),
                                          strong(" Plot Settings"),
                                          style = "display: flex; align-items: center; gap: 8px; color: white;"
                                        ),
                                        solidHeader = TRUE, status = "primary", width = 3,

                                        # Data Selection
                                        div(
                                          style = "background: #f8f9fa; border: 1px solid #3b82f6; border-left: 4px solid #3b82f6; padding: 12px; border-radius: 6px; margin-bottom: 15px;",
                                          h5(icon("database"), "Data Selection", style = "color: #3b82f6; margin-bottom: 10px; font-size: 14px; font-weight: 600;"),
                                          textAreaInput("DotGeneSymbol", "Gene Symbols:", value = "", height = '80px', resize = "vertical"),
                                          div(
                                            style = "background-color: #e9ecef; border: 1px solid #3b82f6; padding: 5px; border-radius: 4px; margin-top: 5px; margin-bottom: 10px;",
                                            uiOutput("Dothints.UI")
                                          ),
                                          withSpinner(uiOutput("DotAssays.UI"), proxy.height = "10px"),
                                          withSpinner(uiOutput("DotSplitBy.UI"), proxy.height = "10px")
                                        ),

                                        # Cluster Settings
                                        div(
                                          style = "background: #f8f9fa; border: 1px solid #10b981; border-left: 4px solid #10b981; padding: 12px; border-radius: 6px; margin-bottom: 15px;",
                                          h5(icon("layer-group"), "Cluster Settings", style = "color: #10b981; margin-bottom: 10px; font-size: 14px; font-weight: 600;"),
                                          withSpinner(uiOutput("DotClusterResolution.UI"), proxy.height = "10px"),
                                          withSpinner(uiOutput("DotIdentsSelected.UI"), proxy.height = "10px"),
                                          bsCollapse(id = "collapseDotplot", open = "0",
                                                            bsCollapsePanel(title = "Change Cluster Order",
                                                                                     withSpinner(uiOutput("DotClusterOrder.UI"), proxy.height = "10px"),
                                                                                     style = "info", value = "0")),
                                          checkboxInput("DotClusterIdents",label = "Cluster the Clusters", FALSE)
                                        ),

                                        # Plot Options
                                        div(
                                          style = "background: #f8f9fa; border: 1px solid #f59e0b; border-left: 4px solid #f59e0b; padding: 12px; border-radius: 6px; margin-bottom: 15px;",
                                          h5(icon("sliders-h"), "Plot Options", style = "color: #f59e0b; margin-bottom: 10px; font-size: 14px; font-weight: 600;"),
                                          checkboxInput("DotRotateAxis",label = "Rotate x Axis Labels", FALSE),
                                          checkboxInput("DotFlipCoordinate",label = "Flip XY Coordinate", FALSE),
                                          conditionalPanel(
                                            condition = "output.DotPlot_Split_isNone",
                                            colourpicker::colourInput("DotPlotLowestExprColor", "Pick Color for lowest expression:", "#E5E5E5", palette = "limited"),
                                            colourpicker::colourInput("DotPlotHighestExprColor", "Pick Color for highest expression:", "#0000FF",palette = "limited"),
                                          ),
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
                                  fluidRow(
                                    box(title = "Features Heatmap Plot",
                                        uiOutput("heatmap_resizable_ui"),
                                        div(style = "display:inline-block; float:right", downloadBttn(outputId = "downloadheatmap",style = "bordered",color = "primary")),
                                        width = 9, status = "primary", collapsible = TRUE, solidHeader = TRUE),
                                    box(title = div(
                                          icon("sliders-h"),
                                          strong(" Plot Settings"),
                                          style = "display: flex; align-items: center; gap: 8px; color: white;"
                                        ),
                                        solidHeader = TRUE, status = "primary", width = 3,

                                        # Data Selection
                                        div(
                                          style = "background: #f8f9fa; border: 1px solid #3b82f6; border-left: 4px solid #3b82f6; padding: 12px; border-radius: 6px; margin-bottom: 15px;",
                                          h5(icon("database"), "Data Selection", style = "color: #3b82f6; margin-bottom: 10px; font-size: 14px; font-weight: 600;"),
                                          textAreaInput("HeatmapGeneSymbol", "Gene Symbols:", value = "", height = '80px', resize = "vertical"),
                                          div(
                                            style = "background-color: #e9ecef; border: 1px solid #3b82f6; padding: 5px; border-radius: 4px; margin-top: 5px; margin-bottom: 10px;",
                                            uiOutput("Heatmaphints.UI")
                                          ),
                                          withSpinner(uiOutput("HeatmapAssays.UI"), proxy.height = "10px"),
                                          withSpinner(uiOutput("HeatmapAssaySlots.UI"), proxy.height = "10px")
                                        ),

                                        # Cluster Settings
                                        div(
                                          style = "background: #f8f9fa; border: 1px solid #10b981; border-left: 4px solid #10b981; padding: 12px; border-radius: 6px; margin-bottom: 15px;",
                                          h5(icon("layer-group"), "Cluster Settings", style = "color: #10b981; margin-bottom: 10px; font-size: 14px; font-weight: 600;"),
                                          withSpinner(uiOutput("HeatmapClusterResolution.UI"), proxy.height = "10px"),
                                          withSpinner(uiOutput("HeatmapIdentsSelected.UI"), proxy.height = "10px"),
                                          bsCollapse(id = "collapseHeatmap", open = "0",
                                                            bsCollapsePanel(title = "Change Cluster Order",
                                                                                     withSpinner(uiOutput("HeatmapClusterOrder.UI"), proxy.height = "10px"),
                                                                                     style = "info", value = "0"))
                                        ),

                                        # Plot Options
                                        div(
                                          style = "background: #f8f9fa; border: 1px solid #f59e0b; border-left: 4px solid #f59e0b; padding: 12px; border-radius: 6px; margin-bottom: 15px;",
                                          h5(icon("sliders-h"), "Plot Options", style = "color: #f59e0b; margin-bottom: 10px; font-size: 14px; font-weight: 600;"),
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
  )
  tab_list[["averagedheatmap"]] = tabItem(tabName = "averagedheatmap",
                                  fluidRow(
                                    box(title = "Features Heatmap by Averaged Expression",
                                        uiOutput("averagedheatmap_resizable_ui"),
                                        div(style = "display:inline-block; float:right", downloadBttn(outputId = "downloadaveragedheatmap",style = "bordered",color = "primary")),
                                        width = 9, status = "primary", collapsible = TRUE, solidHeader = TRUE),
                                    box(title = div(
                                          icon("sliders-h"),
                                          strong(" Plot Settings"),
                                          style = "display: flex; align-items: center; gap: 8px; color: white;"
                                        ),
                                        solidHeader = TRUE, status = "primary", width = 3,

                                        # Data Selection
                                        div(
                                          style = "background: #f8f9fa; border: 1px solid #3b82f6; border-left: 4px solid #3b82f6; padding: 12px; border-radius: 6px; margin-bottom: 15px;",
                                          h5(icon("database"), "Data Selection", style = "color: #3b82f6; margin-bottom: 10px; font-size: 14px; font-weight: 600;"),
                                          textAreaInput("AveragedHeatmapGeneSymbol", "Gene Symbols:", value = "", height = '80px', resize = "vertical"),
                                          div(
                                            style = "background-color: #e9ecef; border: 1px solid #3b82f6; padding: 5px; border-radius: 4px; margin-top: 5px; margin-bottom: 10px;",
                                            uiOutput("AveragedHeatmaphints.UI")
                                          ),
                                          withSpinner(uiOutput("AveragedHeatmapAssays.UI"), proxy.height = "10px")
                                        ),

                                        # Cluster Settings
                                        div(
                                          style = "background: #f8f9fa; border: 1px solid #10b981; border-left: 4px solid #10b981; padding: 12px; border-radius: 6px; margin-bottom: 15px;",
                                          h5(icon("layer-group"), "Cluster Settings", style = "color: #10b981; margin-bottom: 10px; font-size: 14px; font-weight: 600;"),
                                          withSpinner(uiOutput("AveragedHeatmapClusterResolution.UI"), proxy.height = "10px"),
                                          withSpinner(uiOutput("AveragedHeatmapIdentsSelected.UI"), proxy.height = "10px"),
                                          bsCollapse(id = "collapseHeatmap", open = "0",
                                                            bsCollapsePanel(title = "Change Cluster Order",
                                                                                     withSpinner(uiOutput("AveragedHeatmapClusterOrder.UI"), proxy.height = "10px"),
                                                                                     style = "info", value = "0")),
                                          checkboxInput("AveragedHeatmapClusterClusters",label = "Cluster Clusters", FALSE),
                                          checkboxInput("AveragedHeatmapClusterFeatures",label = "Cluster Features", FALSE)
                                        ),

                                        # Plot Options
                                        div(
                                          style = "background: #f8f9fa; border: 1px solid #f59e0b; border-left: 4px solid #f59e0b; padding: 12px; border-radius: 6px; margin-bottom: 15px;",
                                          h5(icon("sliders-h"), "Plot Options", style = "color: #f59e0b; margin-bottom: 10px; font-size: 14px; font-weight: 600;"),
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
                                    fluidRow(
                                      box(title = "Features Ridge Plot",
                                          uiOutput("ridgeplot_resizable_ui"),
                                          div(style = "display:inline-block; float:right", downloadBttn(outputId = "downloadridgeplot",style = "bordered",color = "primary")),
                                          width = 9, status = "primary", collapsible = TRUE, solidHeader = TRUE),
                                      box(title = "Settings", solidHeader = TRUE, status = "primary", width = 3,

                                          # Data Selection
                                          div(
                                            style = "background: #f8f9fa; border: 1px solid #3b82f6; border-left: 4px solid #3b82f6; padding: 12px; border-radius: 6px; margin-bottom: 15px;",
                                            h5(icon("database"), "Data Selection", style = "color: #3b82f6; margin-bottom: 10px; font-size: 14px; font-weight: 600;"),
                                            textAreaInput("RidgeplotGeneSymbol", "Gene Symbols:", value = "", height = '80px', resize = "vertical"),
                                            div(
                                              style = "background-color: #e9ecef; border: 1px solid #3b82f6; padding: 5px; border-radius: 4px; margin-top: 5px; margin-bottom: 10px;",
                                              uiOutput("Ridgeplothints.UI")
                                            ),
                                            withSpinner(uiOutput("RidgeplotAssays.UI"), proxy.height = "10px"),
                                            withSpinner(uiOutput("RidgeplotAssaySlots.UI"), proxy.height = "10px")
                                          ),

                                          # Cluster Settings
                                          div(
                                            style = "background: #f8f9fa; border: 1px solid #10b981; border-left: 4px solid #10b981; padding: 12px; border-radius: 6px; margin-bottom: 15px;",
                                            h5(icon("layer-group"), "Cluster Settings", style = "color: #10b981; margin-bottom: 10px; font-size: 14px; font-weight: 600;"),
                                            withSpinner(uiOutput("RidgeplotClusterResolution.UI"), proxy.height = "10px"),
                                            withSpinner(uiOutput("RidgeplotIdentsSelected.UI"), proxy.height = "10px"),
                                            bsCollapse(id = "collapseRidgeplot", open = "0",
                                                              bsCollapsePanel(title = "Change Cluster Order",
                                                                                       withSpinner(uiOutput("RidgeplotClusterOrder.UI"), proxy.height = "10px"),
                                                                                       style = "info", value = "0"))
                                          ),

                                          # Plot Options
                                          div(
                                            style = "background: #f8f9fa; border: 1px solid #f59e0b; border-left: 4px solid #f59e0b; padding: 12px; border-radius: 6px; margin-bottom: 15px;",
                                            h5(icon("sliders-h"), "Plot Options", style = "color: #f59e0b; margin-bottom: 10px; font-size: 14px; font-weight: 600;"),
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
                                            sliderInput("RidgeplotXlabelSize", label = "X Axis Label Size:", min = 0, max = 20, value = 14),
                                            sliderInput("RidgeplotYlabelSize", label = "Y Axis Label Size:", min = 0, max = 20, value = 10),
                                            checkboxInput("RidgeplotPlotMode",label = "Automatically adjust plotting area", TRUE),
                                            uiOutput("ridgeplot_size_ui")
                                          )
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

                                          # Data Selection (Fill, X, Facet)
                                          div(
                                            style = "background: #f8f9fa; border: 1px solid #3b82f6; border-left: 4px solid #3b82f6; padding: 12px; border-radius: 6px; margin-bottom: 15px;",
                                            h5(icon("database"), "Data Selection", style = "color: #3b82f6; margin-bottom: 10px; font-size: 14px; font-weight: 600;"),
                                            withSpinner(uiOutput("CellratioFillChoice.UI"), proxy.height = "10px"),
                                            withSpinner(uiOutput("CellratioXChoice.UI"), proxy.height = "10px"),
                                            withSpinner(uiOutput("CellratioFacetChoice.UI"), proxy.height = "10px")
                                          ),

                                          # Cluster Settings
                                          div(
                                            style = "background: #f8f9fa; border: 1px solid #10b981; border-left: 4px solid #10b981; padding: 12px; border-radius: 6px; margin-bottom: 15px;",
                                            h5(icon("layer-group"), "Cluster Settings", style = "color: #10b981; margin-bottom: 10px; font-size: 14px; font-weight: 600;"),
                                            withSpinner(uiOutput("CellratioIdentsSelected.UI"), proxy.height = "10px"),
                                            bsCollapse(id = "collapseCellratioFillplot", open = "0",
                                                              bsCollapsePanel(title = "Change Order (Fill)",
                                                                                       withSpinner(uiOutput("CellratioplotFillOrder.UI"), proxy.height = "10px"),
                                                                                       style = "info", value = "0")),
                                            bsCollapse(id = "collapseCellratioXplot", open = "0",
                                                              bsCollapsePanel(title = "Change Order (X)",
                                                                                       withSpinner(uiOutput("CellratioplotXOrder.UI"), proxy.height = "10px"),
                                                                                       style = "info", value = "0")),
                                            bsCollapse(id = "collapseCellratioFacetplot", open = "0",
                                                              bsCollapsePanel(title = "Change Order (Facet)",
                                                                                       withSpinner(uiOutput("CellratioplotFacetOrder.UI"), proxy.height = "10px"),
                                                                                       style = "info", value = "0"))
                                          ),

                                          # Plot Options
                                          div(
                                            style = "background: #f8f9fa; border: 1px solid #f59e0b; border-left: 4px solid #f59e0b; padding: 12px; border-radius: 6px; margin-bottom: 15px;",
                                            h5(icon("sliders-h"), "Plot Options", style = "color: #f59e0b; margin-bottom: 10px; font-size: 14px; font-weight: 600;"),
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
  )
  tab_list[["degs"]] = tabItem(tabName = "degs",
                               # CSS styles for close button hover effect
                               tags$style(type="text/css", "#close_degs_info:hover { color: #059669 !important; }"),
                               # CSS styles for info text
                               tags$style(type="text/css", "#degs_info {white-space: pre-wrap; color: #495057; line-height: 1.8;}"),

                               # Modernized tab styling - ONLY for DEGs page
                               tags$style("
                                   /* 确保 DEGs 页面的两个主容器完全对齐 */
                                   #degstabbox-container {
                                     padding-left: 0 !important;
                                     padding-right: 0 !important;
                                     margin-left: 0 !important;
                                     margin-right: 0 !important;
                                   }
                                   #degstabbox-container > .nav-tabs-custom {
                                     margin-left: -15px !important;
                                     margin-right: -15px !important;
                                     width: calc(100% + 30px) !important;
                                     box-sizing: border-box !important;
                                   }
                                   #degstabbox-container .nav-tabs-custom .nav-tabs {
                                     background: linear-gradient(to bottom, #f8f9fa 0%, #ffffff 100%);
                                     border-bottom: 2px solid #10b981;
                                     border-radius: 8px 8px 0 0;
                                     padding: 10px 15px 0 15px;
                                     margin: 0 !important;
                                   }
                                   #degstabbox-container .nav-tabs-custom .nav-tabs li.active:hover a,
                                   #degstabbox-container .nav-tabs-custom .nav-tabs li.active a {
                                     background-color: white;
                                     border-color: #10b981 #10b981 white #10b981;
                                     color: #10b981;
                                     font-weight: 600;
                                   }
                                   #degstabbox-container .nav-tabs-custom .nav-tabs li.active {
                                     border-top-color: #10b981;
                                     border-top-width: 3px;
                                   }
                                   #degstabbox-container .nav-tabs-custom .nav-tabs li a {
                                     color: #6c757d;
                                     border-radius: 6px 6px 0 0;
                                     transition: all 0.2s ease;
                                     margin-right: 5px;
                                   }
                                   #degstabbox-container .nav-tabs-custom .nav-tabs li a:hover {
                                     background-color: #f8f9fa;
                                     color: #10b981;
                                   }
                                   #degstabbox-container .tab-content {
                                     background: white;
                                     border: 2px solid #10b981;
                                     border-top: none;
                                     border-radius: 0 0 8px 8px;
                                     padding: 25px;
                                     box-shadow: 0 2px 8px rgba(0,0,0,0.1);
                                     margin-left: 0 !important;
                                     margin-right: 0 !important;
                                   }
                                 "),

                               # Single fluidRow containing both Information Box and tabBox
                               fluidRow(id = "degs-main-row",
                                 # Information Box - 白色背景+绿色边框 (top, full width)
                                 div(
                                   id = "degs-info-box",
                                   class = "col-xs-12",
                                   style = "margin-bottom: 10px;",
                                   div(
                                     class = "box",
                                     style = "background: white; border: 2px solid #10b981; border-radius: 8px; box-shadow: 0 2px 6px rgba(0,0,0,0.08); position: relative;",
                                     div(
                                       class = "box-header",
                                       style = "padding: 15px 20px; border-bottom: 2px solid #10b981; position: relative;",
                                       div(
                                         style = "display: flex; align-items: center; gap: 10px;",
                                         icon("info-circle", style = "color: #10b981; font-size: 20px;"),
                                         h4(style = "margin: 0; color: #10b981; font-weight: 600;", "Information")
                                       ),
                                       # Close button in top-right corner
                                       actionButton(
                                         "close_degs_info",
                                         label = NULL,
                                         icon = icon("times"),
                                         style = "position: absolute; right: 15px; top: 50%; transform: translateY(-50%); background: transparent; border: none; color: #10b981; font-size: 24px; cursor: pointer; padding: 0; line-height: 1; transition: color 0.2s ease;",
                                         class = "btn btn-link"
                                       )
                                     ),
                                     div(
                                       class = "box-body",
                                       style = "padding: 15px 20px;",
                                       textOutput("degs_info")
                                     )
                                   )
                                 ),

                                 # tabBox (bottom, full width)
                                 div(
                                   class = "col-xs-12",
                                   style = "margin-bottom: 10px;",
                                   div(id = "degstabbox-container",
                                     tabBox(
                                       title = div(
                                         style = "display: flex; align-items: center; gap: 10px;",
                                         icon("chart-line", style = "color: #10b981; font-size: 18px;"),
                                         strong("Find Markers or DEGs", style = "color: #495057; font-size: 16px;")
                                       ),
                                       id = "tabset_degs", width = 12,
                                       tabPanel("Find Markers for All Clusters",
                                                div(
                                                  style = "background: #f0fdf4; border: 1px solid #10b981; border-left: 4px solid #10b981; padding: 20px; border-radius: 8px; margin-bottom: 25px;",
                                                  withSpinner(uiOutput("ClusterMarkersClusterResolution.UI"), proxy.height = "10px")
                                                ),
                                                div(style = "text-align: center; margin-top: 25px;",
                                                  actionButton("DEGsClusterMarkersAnalysis",
                                                              icon = shiny::icon("magnifying-glass-chart"),
                                                              label = "Analyze",
                                                          class = "btn-primary btn-lg",
                                                          style = "padding: 12px 35px; border-radius: 8px; font-weight: 600; background: linear-gradient(135deg, #10b981 0%, #059669 100%); border: none; box-shadow: 0 4px 12px rgba(16, 185, 129, 0.3);")
                                            )),
                                   tabPanel("Find DEGs for two groups",
                                            # Step 1 - Filter Cells
                                            div(
                                              style = "background: #eff6ff; border: 1px solid #3b82f6; border-left: 4px solid #3b82f6; padding: 25px; border-radius: 8px; margin-bottom: 25px; box-shadow: 0 2px 6px rgba(59, 130, 246, 0.1);",
                                              h4(icon("filter"), "Step 1: Filter Cells (Optional)", style = "color: #3b82f6; margin-bottom: 12px; font-weight: 600; display: flex; align-items: center; gap: 8px;"),
                                              p("Modify parameters below if you want to filter cells before the comparison, otherwise ignore it.", style = "color: #6c757d; margin-bottom: 8px;"),
                                              p("Only the selected cells will be kept for Step 2, and all cells will be kept by default.", style = "color: #6c757d; margin-bottom: 15px;"),
                                              withSpinner(uiOutput("IntraClusterDEGsSubsetCells.UI"), proxy.height = "10px"),
                                              withSpinner(uiOutput("IntraClusterDEGsSubsetCellsSelectedClusters.UI"), proxy.height = "10px")
                                            ),
                                            # Step 2 - Set Comparison
                                            div(
                                              style = "background: #fdf2f8; border: 1px solid #ec4899; border-left: 4px solid #ec4899; padding: 25px; border-radius: 8px; margin-bottom: 25px; box-shadow: 0 2px 6px rgba(236, 72, 153, 0.1);",
                                              h4(icon("balance-scale"), "Step 2: Set the comparison", style = "color: #ec4899; margin-bottom: 15px; font-weight: 600; display: flex; align-items: center; gap: 8px;"),
                                              withSpinner(uiOutput("IntraClusterDEGsCustomizedGroups.UI"), proxy.height = "10px"),
                                              withSpinner(uiOutput("IntraClusterDEGsCustomizedGroupsCase.UI"), proxy.height = "10px"),
                                              withSpinner(uiOutput("IntraClusterDEGsCustomizedGroupsControl.UI"), proxy.height = "10px")
                                            ),
                                            div(style = "text-align: center; margin-top: 30px;",
                                              actionButton("IntraClusterDEGssAnalysis",
                                                          icon = shiny::icon("magnifying-glass-chart"),
                                                          label = "Analyze",
                                                          class = "btn-primary btn-lg",
                                                          style = "padding: 12px 35px; border-radius: 8px; font-weight: 600; background: linear-gradient(135deg, #ec4899 0%, #be185d 100%); border: none; box-shadow: 0 4px 12px rgba(236, 72, 153, 0.3);")
                                            )),
                                   tabPanel("Custom Parameters",
                                            div(
                                              style = "background: #fef3c7; border: 1px solid #f59e0b; border-left: 4px solid #f59e0b; padding: 25px; border-radius: 8px; margin-bottom: 20px; box-shadow: 0 2px 6px rgba(245, 158, 11, 0.1);",
                                              h4(icon("sliders-h"), "Analysis Parameters", style = "color: #f59e0b; margin-bottom: 20px; font-weight: 600; display: flex; align-items: center; gap: 8px;"),
                                              withSpinner(uiOutput("DEGsAssays.UI"), proxy.height = "10px"),
                                              div(style = "margin-top: 20px;",
                                                sliderInput("logfcthreshold", label = "Logfc Threshold:", min = 0, max = 1, value = 0.1),
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
                                                          label = "Set to Default",
                                                          class = "btn-warning",
                                                          style = "padding: 10px 30px; border-radius: 8px; font-weight: 600;")
                                            ))
                                     )  # end of tabBox
                                   )  # end of degstabbox-container div
                                 ),  # end of col-xs-12 div (tabBox)

                               conditionalPanel(
                                   condition = "output.DEGs_ready",
                                   div(style = "margin-top: 15px; margin-left: 15px; margin-right: 15px;",
                                     fluidRow(
                                       # Analysis Results - 自定义样式 (左列，占8/12)
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
                                               icon("table", style = "color: #3b82f6; font-size: 18px;"),
                                               h4(style = "margin: 0; color: #3b82f6; font-weight: 600;", "Analysis Results")
                                             )
                                           ),
                                           div(
                                             class = "box-body",
                                             style = "padding: 20px;",
                                             withSpinner(DT::dataTableOutput('dataset_degs'))
                                           )
                                         )
                                       ),
                                       # External Links - 自定义样式 (右列，占4/12)
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
                                                 icon("external-link-alt", style = "color: #10b981; font-size: 18px;"),
                                                 h4(style = "margin: 0; color: #10b981; font-weight: 600;", "External Links")
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
                               # CSS styles for close button hover effect
                               tags$style(type="text/css", "#close_topgenes_info:hover { color: #059669 !important; }"),
                               # CSS styles for info text
                               tags$style(type="text/css", "#topgenes_info {white-space: pre-wrap; color: #495057; line-height: 1.8;}"),

                               # Modernized tab styling - ONLY for topgenes page
                               tags$style("
                                   #topgenestabbox-container {
                                     padding-left: 0 !important;
                                     padding-right: 0 !important;
                                     margin-left: 0 !important;
                                     margin-right: 0 !important;
                                   }
                                   #topgenestabbox-container > .nav-tabs-custom {
                                     margin-left: -15px !important;
                                     margin-right: -15px !important;
                                     width: calc(100% + 30px) !important;
                                     box-sizing: border-box !important;
                                   }
                                   #topgenestabbox-container .nav-tabs-custom .nav-tabs {
                                     background: linear-gradient(to bottom, #f8f9fa 0%, #ffffff 100%);
                                     border-bottom: 2px solid #3b82f6;
                                     border-radius: 8px 8px 0 0;
                                     padding: 10px 15px 0 15px;
                                     margin: 0 !important;
                                   }
                                   #topgenestabbox-container .nav-tabs-custom .nav-tabs li.active:hover a,
                                   #topgenestabbox-container .nav-tabs-custom .nav-tabs li.active a {
                                     background-color: white;
                                     border-color: #3b82f6 #3b82f6 white #3b82f6;
                                     color: #3b82f6;
                                     font-weight: 600;
                                   }
                                   #topgenestabbox-container .nav-tabs-custom .nav-tabs li.active {
                                     border-top-color: #3b82f6;
                                     border-top-width: 3px;
                                   }
                                   #topgenestabbox-container .nav-tabs-custom .nav-tabs li a {
                                     color: #6c757d;
                                     border-radius: 6px 6px 0 0;
                                     transition: all 0.2s ease;
                                     margin-right: 5px;
                                   }
                                   #topgenestabbox-container .nav-tabs-custom .nav-tabs li a:hover {
                                     background-color: #f8f9fa;
                                     color: #3b82f6;
                                   }
                                   #topgenestabbox-container .tab-content {
                                     background: white;
                                     border: 2px solid #3b82f6;
                                     border-top: none;
                                     border-radius: 0 0 8px 8px;
                                     padding: 25px;
                                     box-shadow: 0 2px 8px rgba(0,0,0,0.1);
                                     margin-left: 0 !important;
                                     margin-right: 0 !important;
                                   }
                                 "),

                               # Single fluidRow containing both Information Box and Settings/Analysis
                               fluidRow(id = "topgenes-main-row",
                                 # Information Box - 白色背景+绿色边框 (top, full width)
                                 div(
                                   id = "topgenes-info-box",
                                   class = "col-xs-12",
                                   style = "margin-bottom: 10px;",
                                   div(
                                     class = "box",
                                     style = "background: white; border: 2px solid #10b981; border-radius: 8px; box-shadow: 0 2px 6px rgba(0,0,0,0.08); position: relative;",
                                     div(
                                       class = "box-header",
                                       style = "padding: 15px 20px; border-bottom: 2px solid #10b981; position: relative;",
                                       div(
                                         style = "display: flex; align-items: center; gap: 10px;",
                                         icon("info-circle", style = "color: #10b981; font-size: 20px;"),
                                         h4(style = "margin: 0; color: #10b981; font-weight: 600;", "Information")
                                       ),
                                       # Close button in top-right corner
                                       actionButton(
                                         "close_topgenes_info",
                                         label = NULL,
                                         icon = icon("times"),
                                         style = "position: absolute; right: 15px; top: 50%; transform: translateY(-50%); background: transparent; border: none; color: #10b981; font-size: 24px; cursor: pointer; padding: 0; line-height: 1; transition: color 0.2s ease;",
                                         class = "btn btn-link"
                                       )
                                     ),
                                     div(
                                       class = "box-body",
                                       style = "padding: 15px 20px;",
                                       textOutput("topgenes_info")
                                     )
                                   )
                                 ),

                                 # Step1: Common Settings (左列，占3/12)
                                 div(
                                   class = "col-md-3",
                                   style = "margin-bottom: 10px;",
                                   div(
                                     style = "background: #f0fdf4; border: 1px solid #10b981; border-left: 4px solid #10b981; padding: 20px; border-radius: 8px; height: 100%;",
                                     h4(icon("sliders-h"), "Step 1: Settings", style = "color: #10b981; margin-bottom: 15px; font-weight: 600; display: flex; align-items: center; gap: 8px;"),
                                     withSpinner(uiOutput("TopGenesClusterResolution.UI"), proxy.height = "10px"),
                                     withSpinner(uiOutput("TopGenesSelectedClusters.UI"), proxy.height = "10px"),
                                     withSpinner(uiOutput("TopGenesAssays.UI"), proxy.height = "10px"),
                                     div(style = "margin-top: 15px;",
                                       checkboxInput("TopGenesClusterLevel", label = "By each cluster", TRUE)
                                     )
                                   )
                                 ),

                                 # Step2: Calculate Top Genes (右列，占9/12)
                                 div(
                                   class = "col-md-9",
                                   style = "margin-bottom: 10px;",
                                   div(id = "topgenestabbox-container",
                                     tabBox(
                                       title = div(
                                         style = "display: flex; align-items: center; gap: 10px;",
                                         icon("chart-line", style = "color: #3b82f6; font-size: 18px;"),
                                         strong("Step 2: Calculate Top Genes", style = "color: #495057; font-size: 16px;")
                                       ),
                                       id = "tabset_topgenes", width = 12,
                                       tabPanel("Find Top Genes by Cell",
                                                div(
                                                  style = "background: #eff6ff; border: 1px solid #3b82f6; border-left: 4px solid #3b82f6; padding: 20px; border-radius: 8px; margin-bottom: 25px;",
                                                  sliderInput("TopGenesTopPercent", "UMI percentage cutoff(%):", min = 1, max = 10, value = 1, step = 1)
                                                ),
                                                div(style = "text-align: center; margin-top: 25px;",
                                                  actionButton("TopGenesAnalysis",
                                                              icon = shiny::icon("magnifying-glass-chart"),
                                                              label = "Analyze",
                                                              class = "btn-primary btn-lg",
                                                              style = "padding: 12px 35px; border-radius: 8px; font-weight: 600; background: linear-gradient(135deg, #3b82f6 0%, #2563eb 100%); border: none; box-shadow: 0 4px 12px rgba(59, 130, 246, 0.3);")
                                                )),
                                       tabPanel("Find Top Genes by Accumulated UMI Counts",
                                                div(
                                                  style = "background: #eff6ff; border: 1px solid #3b82f6; border-left: 4px solid #3b82f6; padding: 20px; border-radius: 8px; margin-bottom: 25px;",
                                                  sliderInput("TopGenesTopN", "Top n:", min = 100, max = 1000, value = 100, step = 100)
                                                ),
                                                div(style = "text-align: center; margin-top: 25px;",
                                                  actionButton("TopAccumulatedGenesAnalysis",
                                                              icon = shiny::icon("magnifying-glass-chart"),
                                                              label = "Analyze",
                                                              class = "btn-primary btn-lg",
                                                              style = "padding: 12px 35px; border-radius: 8px; font-weight: 600; background: linear-gradient(135deg, #3b82f6 0%, #2563eb 100%); border: none; box-shadow: 0 4px 12px rgba(59, 130, 246, 0.3);")
                                                ))
                                     )
                                   )
                                 ),

                                 # Analysis Results
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
                                               icon("table", style = "color: #3b82f6; font-size: 18px;"),
                                               h4(style = "margin: 0; color: #3b82f6; font-weight: 600;", "Analysis Results")
                                             )
                                           ),
                                           div(
                                             class = "box-body",
                                             style = "padding: 20px;",
                                             withSpinner(DT::dataTableOutput('dataset_topgenes'))
                                           )
                                         )
                                       )
                                     )
                                   )
                                 )
                               )
  )
  tab_list[["featuresummary"]] = tabItem(tabName = "featuresummary",
                               # CSS styles for close button hover effect
                               tags$style(type="text/css", "#close_featuresummary_info:hover { color: #059669 !important; }"),
                               # CSS styles for info text
                               tags$style(type="text/css", "#featuresummary_info {white-space: pre-wrap; color: #495057; line-height: 1.8;}"),

                               # Single fluidRow containing both Information Box and Settings/Analysis
                               fluidRow(id = "featuresummary-main-row",
                                 # Information Box - 白色背景+绿色边框
                                 div(
                                   id = "featuresummary-info-box",
                                   class = "col-xs-12",
                                   style = "margin-bottom: 10px;",
                                   div(
                                     class = "box",
                                     style = "background: white; border: 2px solid #10b981; border-radius: 8px; box-shadow: 0 2px 6px rgba(0,0,0,0.08); position: relative;",
                                     div(
                                       class = "box-header",
                                       style = "padding: 15px 20px; border-bottom: 2px solid #10b981; position: relative;",
                                       div(
                                         style = "display: flex; align-items: center; gap: 10px;",
                                         icon("info-circle", style = "color: #10b981; font-size: 20px;"),
                                         h4(style = "margin: 0; color: #10b981; font-weight: 600;", "Information")
                                       ),
                                       # Close button in top-right corner
                                       actionButton(
                                         "close_featuresummary_info",
                                         label = NULL,
                                         icon = icon("times"),
                                         style = "position: absolute; right: 15px; top: 50%; transform: translateY(-50%); background: transparent; border: none; color: #10b981; font-size: 24px; cursor: pointer; padding: 0; line-height: 1; transition: color 0.2s ease;",
                                         class = "btn btn-link"
                                       )
                                     ),
                                     div(
                                       class = "box-body",
                                       style = "padding: 15px 20px;",
                                       textOutput("featuresummary_info")
                                     )
                                   )
                                 ),

                                 # Settings (左列，占3/12)
                                 div(
                                   class = "col-md-3",
                                   style = "margin-bottom: 10px;",
                                   div(
                                     style = "background: #f0fdf4; border: 1px solid #10b981; border-left: 4px solid #10b981; padding: 20px; border-radius: 8px; height: 100%;",
                                     h4(icon("sliders-h"), "Settings", style = "color: #10b981; margin-bottom: 15px; font-weight: 600; display: flex; align-items: center; gap: 8px;"),
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
                                                   class = "btn-primary btn-lg",
                                                   style = "padding: 12px 35px; border-radius: 8px; font-weight: 600; background: linear-gradient(135deg, #10b981 0%, #059669 100%); border: none; box-shadow: 0 4px 12px rgba(16, 185, 129, 0.3);")
                                     )
                                   )
                                 ),

                                 # Gene Short Summary (右列，占9/12)
                                 div(
                                   class = "col-md-9",
                                   style = "margin-bottom: 10px;",
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
                                           icon("table", style = "color: #3b82f6; font-size: 18px;"),
                                           h4(style = "margin: 0; color: #3b82f6; font-weight: 600;", "Gene Short Summary")
                                         )
                                       ),
                                       div(
                                         class = "box-body",
                                         style = "padding: 20px;",
                                         withSpinner(DT::dataTableOutput('dataset_featuresummary'))
                                       )
                                     )
                                   )
                                 )
                               )
  )
  tab_list[["featurecorrelation"]] = tabItem(tabName = "featurecorrelation",
                               # CSS styles for close button hover effect
                               tags$style(type="text/css", "#close_featurecorrelation_info:hover { color: #059669 !important; }"),
                               # CSS styles for info text
                               tags$style(type="text/css", "#featurecorrelation_info {white-space: pre-wrap; color: #495057; line-height: 1.8;}"),

                               # Modernized tab styling - ONLY for featurecorrelation page
                               tags$style("
                                   #featurecorrelationtabbox-container {
                                     padding-left: 0 !important;
                                     padding-right: 0 !important;
                                     margin-left: 0 !important;
                                     margin-right: 0 !important;
                                   }
                                   #featurecorrelationtabbox-container > .nav-tabs-custom {
                                     margin-left: -15px !important;
                                     margin-right: -15px !important;
                                     width: calc(100% + 30px) !important;
                                     box-sizing: border-box !important;
                                   }
                                   #featurecorrelationtabbox-container .nav-tabs-custom .nav-tabs {
                                     background: linear-gradient(to bottom, #f8f9fa 0%, #ffffff 100%);
                                     border-bottom: 2px solid #3b82f6;
                                     border-radius: 8px 8px 0 0;
                                     padding: 10px 15px 0 15px;
                                     margin: 0 !important;
                                   }
                                   #featurecorrelationtabbox-container .nav-tabs-custom .nav-tabs li.active:hover a,
                                   #featurecorrelationtabbox-container .nav-tabs-custom .nav-tabs li.active a {
                                     background-color: white;
                                     border-color: #3b82f6 #3b82f6 white #3b82f6;
                                     color: #3b82f6;
                                     font-weight: 600;
                                   }
                                   #featurecorrelationtabbox-container .nav-tabs-custom .nav-tabs li.active {
                                     border-top-color: #3b82f6;
                                     border-top-width: 3px;
                                   }
                                   #featurecorrelationtabbox-container .nav-tabs-custom .nav-tabs li a {
                                     color: #6c757d;
                                     border-radius: 6px 6px 0 0;
                                     transition: all 0.2s ease;
                                     margin-right: 5px;
                                   }
                                   #featurecorrelationtabbox-container .nav-tabs-custom .nav-tabs li a:hover {
                                     background-color: #f8f9fa;
                                     color: #3b82f6;
                                   }
                                   #featurecorrelationtabbox-container .tab-content {
                                     background: white;
                                     border: 2px solid #3b82f6;
                                     border-top: none;
                                     border-radius: 0 0 8px 8px;
                                     padding: 25px;
                                     box-shadow: 0 2px 8px rgba(0,0,0,0.1);
                                     margin-left: 0 !important;
                                     margin-right: 0 !important;
                                   }
                                 "),

                               # Single fluidRow containing both Information Box and Settings/Analysis
                               fluidRow(id = "featurecorrelation-main-row",
                                 # Information Box - 白色背景+绿色边框
                                 div(
                                   id = "featurecorrelation-info-box",
                                   class = "col-xs-12",
                                   style = "margin-bottom: 10px;",
                                   div(
                                     class = "box",
                                     style = "background: white; border: 2px solid #10b981; border-radius: 8px; box-shadow: 0 2px 6px rgba(0,0,0,0.08); position: relative;",
                                     div(
                                       class = "box-header",
                                       style = "padding: 15px 20px; border-bottom: 2px solid #10b981; position: relative;",
                                       div(
                                         style = "display: flex; align-items: center; gap: 10px;",
                                         icon("info-circle", style = "color: #10b981; font-size: 20px;"),
                                         h4(style = "margin: 0; color: #10b981; font-weight: 600;", "Information")
                                       ),
                                       # Close button
                                       actionButton(
                                         "close_featurecorrelation_info",
                                         label = NULL,
                                         icon = icon("times"),
                                         style = "position: absolute; right: 15px; top: 50%; transform: translateY(-50%); background: transparent; border: none; color: #10b981; font-size: 24px; cursor: pointer; padding: 0; line-height: 1; transition: color 0.2s ease;",
                                         class = "btn btn-link"
                                       )
                                     ),
                                     div(
                                       class = "box-body",
                                       style = "padding: 15px 20px;",
                                       textOutput("featurecorrelation_info")
                                     )
                                   )
                                 ),

                                 # Step 1: Common Settings (左列，占3/12)
                                 div(
                                   class = "col-md-3",
                                   style = "margin-bottom: 10px;",
                                   div(
                                     style = "background: #f0fdf4; border: 1px solid #10b981; border-left: 4px solid #10b981; padding: 20px; border-radius: 8px; height: 100%;",
                                     h4(icon("sliders-h"), "Step 1: Settings", style = "color: #10b981; margin-bottom: 15px; font-weight: 600; display: flex; align-items: center; gap: 8px;"),
                                     withSpinner(uiOutput("FeatureCorrelationClusterResolution.UI"), proxy.height = "10px"),
                                     withSpinner(uiOutput("FeatureCorrelationIdentsSelected.UI"), proxy.height = "10px"),
                                     withSpinner(uiOutput("FeatureCorrelationAssays.UI"), proxy.height = "10px"),
                                     selectInput("correlationmethod", "Correlation Method:", choices = c(pearson = "pearson", spearman = "spearman"))
                                   )
                                 ),

                                 # Step 2: Calculate Correlation (右列，占9/12)
                                 div(
                                   class = "col-md-9",
                                   style = "margin-bottom: 10px;",
                                   div(id = "featurecorrelationtabbox-container",
                                     tabBox(
                                       title = div(
                                         style = "display: flex; align-items: center; gap: 10px;",
                                         icon("chart-line", style = "color: #3b82f6; font-size: 18px;"),
                                         strong("Step 2: Calculate Correlation", style = "color: #495057; font-size: 16px;")
                                       ),
                                       id = "tabset_featurecorrelation", width = 12,
                                       tabPanel("Find Top Correlated Gene Pairs",
                                                div(style = "text-align: center; margin-top: 25px;",
                                                  actionButton("TopCorrelationAnalysis",
                                                              icon = shiny::icon("magnifying-glass-chart"),
                                                              label = "Analyze",
                                                              class = "btn-primary btn-lg",
                                                              style = "padding: 12px 35px; border-radius: 8px; font-weight: 600; background: linear-gradient(135deg, #3b82f6 0%, #2563eb 100%); border: none; box-shadow: 0 4px 12px rgba(59, 130, 246, 0.3);")
                                                )),
                                       tabPanel("Find Top Correlated Genes for A Gene",
                                                div(
                                                  style = "background: #eff6ff; border: 1px solid #3b82f6; border-left: 4px solid #3b82f6; padding: 20px; border-radius: 8px; margin-bottom: 25px;",
                                                  textInput(inputId = "MostCorrelatedAGene", label = "Input a gene:", width = '100%')
                                                ),
                                                div(style = "text-align: center; margin-top: 25px;",
                                                  actionButton("MostCorrelatedAnalysis",
                                                              icon = shiny::icon("magnifying-glass-chart"),
                                                              label = "Analyze",
                                                              class = "btn-primary btn-lg",
                                                              style = "padding: 12px 35px; border-radius: 8px; font-weight: 600; background: linear-gradient(135deg, #3b82f6 0%, #2563eb 100%); border: none; box-shadow: 0 4px 12px rgba(59, 130, 246, 0.3);")
                                                )),
                                       tabPanel("Calculate Correlation of All Pairs in A Gene List",
                                                div(
                                                  style = "background: #eff6ff; border: 1px solid #3b82f6; border-left: 4px solid #3b82f6; padding: 20px; border-radius: 8px; margin-bottom: 25px;",
                                                  textAreaInput(inputId = "CorrelationGeneList", label = "Input a group of genes:", width = '100%', height = '100px', resize = "vertical")
                                                ),
                                                div(style = "text-align: center; margin-top: 25px;",
                                                  actionButton("calculatecorrelation",
                                                              icon = shiny::icon("save"),
                                                              label = "Analyze",
                                                              class = "btn-primary btn-lg",
                                                              style = "padding: 12px 35px; border-radius: 8px; font-weight: 600; background: linear-gradient(135deg, #3b82f6 0%, #2563eb 100%); border: none; box-shadow: 0 4px 12px rgba(59, 130, 246, 0.3);")
                                                ))
                                     )
                                   )
                                 ),

                                 # Analysis Results
                                 div(
                                   class = "col-xs-12",
                                   style = "margin-bottom: 10px;",
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
                                           icon("table", style = "color: #3b82f6; font-size: 18px;"),
                                           h4(style = "margin: 0; color: #3b82f6; font-weight: 600;", "Analysis Results")
                                         )
                                       ),
                                       div(
                                         class = "box-body",
                                         style = "padding: 20px;",
                                         withSpinner(DT::dataTableOutput('dataset_correlation'))
                                       )
                                     )
                                   )
                                 )
                               )
  )
  tab_list[["renameclusters"]] = tabItem(tabName = "renameclusters",
                               fluidRow(id = "renameclusters-main-row",
                                 # Rename Clusters Table (左列，占9/12)
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
                                         icon("table", style = "color: #3b82f6; font-size: 18px;"),
                                         h4(style = "margin: 0; color: #3b82f6; font-weight: 600;", "Rename Clusters")
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

                                 # Settings (右列，占3/12)
                                 div(
                                   class = "col-md-3",
                                   div(
                                     style = "background: #f0fdf4; border: 1px solid #10b981; border-left: 4px solid #10b981; padding: 20px; border-radius: 8px; height: 100%;",
                                     h4(icon("sliders-h"), "Settings", style = "color: #10b981; margin-bottom: 15px; font-weight: 600; display: flex; align-items: center; gap: 8px;"),
                                     withSpinner(uiOutput("renameclustersClusterResolution.UI"), proxy.height = "10px"),
                                     withSpinner(uiOutput("renameclustersDimensionReduction.UI"), proxy.height = "10px"),
                                     textInput('renameclustersNewClusterName', 'Input Cluster name:', value = "group"),
                                     div(
                                       style = "background: #eff6ff; padding: 10px; border-radius: 4px; margin-top: 15px;",
                                       uiOutput("renameclustersNewClusterNamehints.UI")
                                     ),
                                     div(style = "margin-top: 20px; text-align: center;",
                                       actionButton("renameclustersCheck",
                                                   icon = shiny::icon("check"),
                                                   label = "Check",
                                                   class = "btn-primary btn-lg",
                                                   style = "padding: 10px 30px; border-radius: 8px; font-weight: 600; background: linear-gradient(135deg, #10b981 0%, #059669 100%); border: none; box-shadow: 0 4px 12px rgba(16, 185, 129, 0.3);")
                                     ),
                                     conditionalPanel(
                                       condition = "output.renameclusterscheck_OK",
                                       div(style = "margin-top: 15px;",
                                         div(style = "display: flex; gap: 10px; justify-content: center;",
                                           actionButton("renameclustersSubmit",
                                                       icon = shiny::icon("arrows-rotate"),
                                                       label = "Update",
                                                       class = "btn-primary",
                                                       style = "padding: 10px 20px; border-radius: 8px; font-weight: 600; background: linear-gradient(135deg, #3b82f6 0%, #2563eb 100%); border: none;"),
                                           downloadButton("renameclustersDownload",
                                                         "Download",
                                                         icon = shiny::icon("file-arrow-down"),
                                                         class = "btn-primary",
                                                         style = "padding: 10px 20px; border-radius: 8px; font-weight: 600; background: linear-gradient(135deg, #f59e0b 0%, #d97706 100%); border: none;")
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
  tab_list[["featuresdf"]] = tabItem(tabName = "featuresdf",
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
                                         icon("search", style = "color: #10b981; font-size: 18px;"),
                                         h4(style = "margin: 0; color: #10b981; font-weight: 600;", "Search Features")
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
                                         icon("table", style = "color: #3b82f6; font-size: 18px;"),
                                         h4(style = "margin: 0; color: #3b82f6; font-weight: 600;", "Metadata of Cells")
                                       )
                                     ),
                                     div(
                                       class = "box-body",
                                       style = "padding: 20px;",
                                       withSpinner(tagList(
                                         div(style = "margin-bottom: 15px; text-align: right;",
                                           downloadButton("download_meta_data", "Download", icon = shiny::icon("file-arrow-down"),
                                                         class = "btn-primary",
                                                         style = "padding: 10px 25px; border-radius: 8px; font-weight: 600; background: linear-gradient(135deg, #3b82f6 0%, #2563eb 100%); border: none;")
                                         ),
                                         DT::dataTableOutput('dataset_meta')
                                       ))
                                     )
                                   )
                                 )
                               )
  )
  tab_list[["objectstructure"]] = tabItem(tabName = "objectstructure",
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
                                         icon("code", style = "color: #8b5cf6; font-size: 18px;"),
                                         h4(style = "margin: 0; color: #8b5cf6; font-weight: 600;", "Structure of Seurat Object")
                                       )
                                     ),
                                     div(
                                       class = "box-body",
                                       style = "padding: 20px;",
                                       div(
                                         style = "background: #f5f3ff; border: 1px solid #8b5cf6; border-left: 4px solid #8b5cf6; padding: 20px; border-radius: 8px; margin-bottom: 20px;",
                                         h4(icon("sliders-h"), "Display Settings", style = "color: #8b5cf6; margin-bottom: 15px; font-weight: 600; display: flex; align-items: center; gap: 8px;"),
                                         sliderInput("ObjectStrutureLevel", label = "Structure Depth:", min = 1, max = 10, value = 3)
                                       ),
                                       withSpinner(verbatimTextOutput("object_structure"))
                                     )
                                   )
                                 )
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
                                        fileInput("dataset_file", "Choose A rds or qs2 file of Seurat Object:", accept = c('.rds', ".qs2")),
                                        uiOutput("dataOverview"))
                                  )
  )

  tab_list <- explorer_body_ui(tab_list = tab_list)

  body <- shinydashboard::dashboardBody(
    shinyjs::useShinyjs(),
    tags$head(
      tags$style(HTML("
        /* 全局字体优化 */
        body {
          font-family: -apple-system, BlinkMacSystemFont, 'Segoe UI', Roboto, 'Helvetica Neue', Arial, sans-serif;
        }

        /* 优化box样式 - 保持默认背景 */
        .box {
          border-radius: 6px;
          box-shadow: 0 1px 3px rgba(0,0,0,0.1);
          transition: box-shadow 0.2s ease;
        }

        .box:hover {
          box-shadow: 0 2px 6px rgba(0,0,0,0.15);
        }

        .box.box-solid .box-title {
          font-weight: 600;
        }

        /* 优化按钮样式 */
        .btn {
          border-radius: 6px;
          font-weight: 500;
          transition: all 0.2s ease;
        }

        .btn:hover {
          box-shadow: 0 2px 6px rgba(0,0,0,0.15);
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

        /* 响应式优化 */
        @media (max-width: 768px) {
          .content-wrapper {
            padding: 10px;
          }

          .box {
            margin-bottom: 10px;
          }
        }
      "))
    ),
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



