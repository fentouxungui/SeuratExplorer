# server.R
## the server side

#' server side functions related to `explorer_sidebar_ui`
#'
#' @param input server input
#' @param output server output
#' @param session server session
#' @param verbose for debug use
#' @param data the Seurat object and related parameters
#'
#' @import Seurat SeuratObject
#' @importFrom utils str
#' @importFrom grDevices dev.off pdf
#' @importFrom stats na.omit
#' @export
#' @return server side functions related to `explorer_sidebar_ui`
#'
explorer_server <- function(input, output, session, data, verbose=FALSE){
  temp_dir <- tempdir() # temporary directory, for save plots

  if (dir.exists(temp_dir)) {
    unlink(temp_dir, recursive = TRUE)
  }

  dir.create(temp_dir, showWarnings = FALSE)
  # to make shinyBS::updateCollapse() runs correctly, refer to: https://github.com/ebailey78/shinyBS/issues/92
  shiny::addResourcePath("sbs", system.file("www", package="shinyBS"))

  # Using an un-exported function from another R package:
  # https://stackoverflow.com/questions/32535773/using-un-exported-function-from-another-r-package
  subset_Seurat <- utils::getFromNamespace('subset.Seurat', 'SeuratObject')

  # batch define some output elements
  ## dimension reduction options for dimplot and featureplot
  dimension_reduction_UI_names <- c('DimDimensionReduction', 'FeatureDimensionReduction', 'renameclustersDimensionReduction')
  dimension_reduction_df <- data.frame(Element =  paste0(dimension_reduction_UI_names, '.UI'), UIID = dimension_reduction_UI_names)
  output_dimension_reduction <- lapply(1:nrow(dimension_reduction_df), function(i){
    output[[dimension_reduction_df$Element[i]]] <- renderUI({
      req(data$obj)
      if(verbose){message(paste0("SeuratExplorer: preparing ", dimension_reduction_df$Element[i], "..."))}
      selectInput(dimension_reduction_df$UIID[i],
                  'Dimension Reduction:',
                  choices = data$reduction_options,
                  selected = data$reduction_default) # set default reduction
    })
  })

  ## cluster resolution options for dimplot, featureplot, etc.
  resolution_UI_names <- c('DimClusterResolution',
                           'FeatureClusterResolution',
                           'VlnClusterResolution',
                           'DotClusterResolution',
                           'HeatmapClusterResolution',
                           'AveragedHeatmapClusterResolution',
                           'RidgeplotClusterResolution',
                           'ClusterMarkersClusterResolution',
                           'TopGenesClusterResolution',
                           'FeatureSummaryClusterResolution',
                           'FeatureCorrelationClusterResolution',
                           'renameclustersClusterResolution')
  resolution_df <- data.frame(Element = paste0(resolution_UI_names, '.UI'),
                              UIID = resolution_UI_names)
  output_resolution <- lapply(1:nrow(resolution_df), function(i){
    output[[resolution_df$Element[i]]] <- renderUI({
      req(data$obj)
      if(verbose){message(paste0("SeuratExplorer: preparing ", resolution_df$Element[i], "..."))}
      selectInput(resolution_df$UIID[i], 'Cluster Resolution:',
                  choices = data$cluster_options,
                  selected = data$cluster_default)
    })
  })

  # ## Cluster order # Not Work when need values from input, such as: input[[cluster_order_UI_df$UIRelyOn[i]]]
  # cluster_order_UI_names <- c('DimClusterOrder')
  # cluster_order_UI_relyon <- c('DimClusterResolution')
  # cluster_order_UI_df <- data.frame(Eelement = paste0(cluster_order_UI_names, '.UI'),
  #                                   UIID = cluster_order_UI_names,
  #                                   UIRelyOn = cluster_order_UI_relyon)
  # output_cluster_order <- lapply(1:nrow(cluster_order_UI_df), function(i){
  #   output[[cluster_order_UI_df$Element[i]]] <- renderUI({
  #     # req(input[[cluster_order_UI_df$UIRelyOn[i]]])
  #     if(verbose){message(paste0("SeuratExplorer: preparing ", cluster_order_UI_df$Element[i], "..."))}
  #     items_full <- input[[cluster_order_UI_df$UIRelyOn[i]]]
  #     shinyjqui::orderInput(inputId = cluster_order_UI_df$UIID[i], label = 'Drag to order', items = levels(data$obj@meta.data[,items_full]),width = '100%')
  #   })
  # })

  # allowed data slots for each assay in each plot/summary functions
  assay_allowed_slots <- list('FeatureAssay' = isolate(data$assay_slots),
                              # use isolate for in case of error:
                              # Can't access reactive value 'assay_slots' outside of reactive consumer.
                              'VlnAssay' = isolate(data$assay_slots),
                              # DotPlot right now can only FetchData from data slot of the assay,
                              # so only assays with data slot can be supplied for the assay options
                              'DotAssay' = 'data',
                              'HeatmapAssay' = c('data', 'scale.data'),
                              'AveragedHeatmapAssay' = 'data',
                              'RidgeplotAssay' = isolate(data$assay_slots),
                              'DEGsAssay' = c('data', 'counts'),
                              'TopGenesAssay' = c('counts'),
                              'FeatureSummaryAssay' = c('data'),
                              'FeatureCorrelationAssay' = c('data'),
                              'FeaturesDataframeAssay'= isolate(data$assay_slots))

  filter_assay <- function(assay_info, allowed_slots){
    # assay_info is a list contains all slot names for each assay
    assays_options <- names(assay_info)[unlist(lapply(assay_info,function(x) any(allowed_slots %in% x)))]
    return(assays_options)
  }

  filter_slot <- function(assay_info, assay_selected, allowed_slots){
    slots_existed <- assay_info[[assay_selected]]
    return(slots_existed[slots_existed %in% allowed_slots])
  }

  ## define assays choices UI
  assay_df <- data.frame(Element = paste0(names(assay_allowed_slots), 's.UI'),
                         UIID = names(assay_allowed_slots))

  output_assay <- lapply(1:nrow(assay_df), function(i){
    output[[assay_df$Element[i]]] <- renderUI({
      if(verbose){message(paste0("SeuratExplorer: preparing ", assay_df$Element[i], "..."))}
      assays_options <- filter_assay(assay_info = data$assays_slots_options,
                                     allowed_slots = assay_allowed_slots[[assay_df$UIID[i]]])
      selectInput(assay_df$UIID[i],
                  "Assay:",
                  choices = assays_options,
                  selected = ifelse(data$assay_default %in% assays_options,
                                    data$assay_default,
                                    assays_options[1]))
    })
  })


  ## batch addin
  do.call(tagList, c(output_dimension_reduction, output_resolution, output_assay))

  ############################# Dimension Reduction Plot
  # define Cluster order
  output$DimClusterOrder.UI <- renderUI({
    if(verbose){message("SeuratExplorer: preparing DimClusterOrder.UI...")}
    shinyjqui::orderInput(inputId = 'DimClusterOrder',
                          label = 'Drag to order:',
                          items = levels(data$obj@meta.data[,input$DimClusterResolution]),
                          width = '100%')
  })

  # when change cluster resolution, open the shinyBS::bsCollapsePanel,
  # otherwise will cause cluster order not update
  # a bad effect is: each time changing the resolution option,
  # will collapse cluster order ui
  observeEvent(input$DimClusterResolution, ({
    if(verbose){message("SeuratExplorer: updateCollapse for collapseDimplot...")}
    shinyBS::updateCollapse(session, "collapseDimplot", open = "Change Cluster Order")
  }))

  # define Split Choice UI
  output$DimSplit.UI <- renderUI({
    if(verbose){message("SeuratExplorer: preparing DimSplit.UI...")}
    selectInput("DimSplit","Split by:", choices = c("None" = "None", data$split_options))
  })

  # Revise Split selection which will be appropriate for plot
  DimSplit.Revised <- reactive({
    req(input$DimSplit) # only run after split is ready
    if(verbose){message("SeuratExplorer: preparing DimSplit.Revised...")}
    # Revise the Split choice
    if(is.na(input$DimSplit) | input$DimSplit == "None") {
      return(NULL)
    }else{
      return(input$DimSplit)
    }
  })

  # define Cluster choice for highlight
  output$DimHighlightedClusters.UI <- renderUI({
    req(input$DimClusterResolution)
    if(verbose){message("SeuratExplorer: preparing DimHighlightedClusters.UI...")}
    shinyWidgets::pickerInput(inputId = "DimHighlightedClusters", label = "Highlight Clusters:",
                              choices = levels(data$obj@meta.data[,input$DimClusterResolution]),
                              selected = NULL,
                              options = shinyWidgets::pickerOptions(actionsBox = TRUE,
                                                                    size = 10,
                                                                    selectedTextFormat = "count > 3"),
                              multiple = TRUE)
  })


  dimplot_width  <- reactive({ session$clientData$output_dimplot_width })

  # Pixel (X) to Centimeter: 1 pixel (X)	= 0.0264583333 cm, if use this value,
  # the picture is a little bit of small, unknown why.
  px2cm <- 0.03

  output$dimplot <- renderPlot({
    if(verbose){message("SeuratExplorer: preparing dimplot...")}
    cds <- data$obj # not a memory saving way
    # for highlight cells
    if (any(is.null(input$DimHighlightedClusters))) {
      dim_cells_highlighted <- NULL
    }else{
      dim_cells_highlighted <- colnames(cds)[cds@meta.data[,input$DimClusterResolution] %in% input$DimHighlightedClusters]
    }
    cds@meta.data[,input$DimClusterResolution] <- factor(cds@meta.data[,input$DimClusterResolution],
                                                         levels = input$DimClusterOrder)
    if (is.null(DimSplit.Revised())) { # not splited
      p <- Seurat::DimPlot(cds,
                           reduction = input$DimDimensionReduction,
                           label = input$DimShowLabel,
                           pt.size = input$DimPointSize,
                           label.size = input$DimLabelSize,
                           group.by = input$DimClusterResolution,
                           cells.highlight = dim_cells_highlighted)
      }else{ # splited
      plot_numbers <- length(levels(cds@meta.data[,DimSplit.Revised()]))
      p <- Seurat::DimPlot(cds, reduction = input$DimDimensionReduction,
                           label = input$DimShowLabel, pt.size = input$DimPointSize,
                           label.size = input$DimLabelSize,
                           group.by = input$DimClusterResolution,
                           split.by = DimSplit.Revised(),
                           ncol = ceiling(sqrt(plot_numbers)),
                           cells.highlight = dim_cells_highlighted)
      }
    if(!input$DimShowLegend){
      p <- p & NoLegend()
    }
    ggplot2::ggsave(paste0(temp_dir,"/dimplot.pdf"),
                    p,
                    width = dimplot_width() * px2cm,
                    height = dimplot_width() * input$DimPlotHWRatio * px2cm,
                    units = "cm",
                    limitsize = FALSE)
    return(p)
  }, height = function(){session$clientData$output_dimplot_width * input$DimPlotHWRatio})
  # box plot: height = width default

  # refer to: https://stackoverflow.com/questions/14810409/how-to-save-plots-that-are-made-in-a-shiny-app
  output$downloaddimplot <- downloadHandler(
    filename = function(){'dimplot.pdf'},
    content = function(file) {
      file.copy(paste0(temp_dir,"/dimplot.pdf"), file, overwrite=TRUE)
    })

  ################################ Feature Plot
  # define slot Choice UI
  output$FeatureAssaySlots.UI <- renderUI({
    req(input$FeatureAssay)
    if(verbose){message("SeuratExplorer: preparing FeatureAssaySlots.UI...")}
    slot_choices <- filter_slot(assay_info = data$assays_slots_options,
                                assay_selected = input$FeatureAssay,
                                allowed_slots = assay_allowed_slots[['FeatureAssay']])
    selectInput("FeatureSlot", "Slot:",
                choices = slot_choices,
                selected = ifelse('data' %in% slot_choices, 'data', slot_choices[1])) # default use data slot
  })

  # define Split Choice UI
  output$FeatureSplit.UI <- renderUI({
    if(verbose){message("SeuratExplorer: preparing FeatureSplit.UI...")}
    selectInput("FeatureSplit","Split by:", choices = c("None" = "None", data$split_options))
  })

  # inform extra qc options for Gene symbol input
  output$Featurehints.UI <- renderUI({
    if(verbose){message("SeuratExplorer: preparing Featurehints.UI...")}
    helpText(strong(paste("Also supports: ",
                          paste(data$extra_qc_options, collapse = " "), ".",
                          sep = "")),
             br(),
             strong("Tips: You can paste multiple genes from a column in excel."),style = "font-size:12px;")
  })


  # Revise Split selection which will be appropriate for DimPlot, FeaturePlot and Vlnplot functions.
  FeatureSplit.Revised <- reactive({
    req(input$FeatureSplit)
    if(verbose){message("SeuratExplorer: preparing FeatureSplit.Revised...")}
    # Revise the Split choice
    if(is.na(input$FeatureSplit) | input$FeatureSplit == "None") {
      return(NULL)
    }else{
      return(input$FeatureSplit)
    }
  })

  # only render plot when the inputs are really changed
  features_dimplot <- reactiveValues(features_current = NA, features_last = NA)

  observeEvent(input$FeatureGeneSymbol,{
    features_input <- CheckGene(InputGene = input$FeatureGeneSymbol,
                                GeneLibrary =  c(rownames(data$obj@assays[[input$FeatureAssay]]),
                                                 data$extra_qc_options))
    if (!identical(sort(features_dimplot$features_current), sort(features_input))) {
      features_dimplot$features_last <- features_dimplot$features_current
      features_dimplot$features_current <- features_input
    }
  })

  # though none errors show, very slow for Error in Seurat::FeaturePlot: None of the requested features were found: CD8A, CD4, SHANK3 in slot  data
  # observe({
  #   features_input <- CheckGene(InputGene = input$FeatureGeneSymbol, GeneLibrary =  c(rownames(data$obj@assays[[input$FeatureAssay]]), data$extra_qc_options))
  #   if (!identical(sort(features_dimplot$features_current), sort(features_input))) {
  #     features_dimplot$features_last <- features_dimplot$features_current
  #     features_dimplot$features_current <- features_input
  #   }
  # })

  featureplot_width  <- reactive({ session$clientData$output_featureplot_width })

  output$featureplot <- renderPlot({
    req(input$FeatureSlot)
    if(verbose){message("SeuratExplorer: preparing featureplot...")}
    if(input$FeatureMinCutoff == 0){
      expr_min_cutoff <- NA
    }else{
      expr_min_cutoff <- paste0('q', round(input$FeatureMinCutoff))
    }
    if(input$FeatureMaxCutoff == 100){
      expr_max_cutoff <- NA
    }else{
        expr_max_cutoff <- paste0('q', round(input$FeatureMaxCutoff))
    }
    if (any(is.na(features_dimplot$features_current))) { # when NA value
      p <- empty_plot # when all wrong input, show a blank pic.
    }else{
      cds <- data$obj
      Seurat::Idents(cds) <- input$FeatureClusterResolution
      Seurat::DefaultAssay(cds) <- input$FeatureAssay
      # check gene again, if all the input symbols not exist in the selected assay, specially case: when switch assay!
      if(!any(features_dimplot$features_current %in% c(rownames(cds[[input$FeatureAssay]]),data$extra_qc_options))){
        p <- empty_plot
      }else{
        if(is.null(FeatureSplit.Revised())) { # not split
          p <- Seurat::FeaturePlot(cds,
                                   features = features_dimplot$features_current,
                                   pt.size = input$FeaturePointSize,
                                   reduction = input$FeatureDimensionReduction,
                                   slot = input$FeatureSlot,
                                   cols = c(input$FeaturePlotLowestExprColor,input$FeaturePlotHighestExprColor),
                                   label = input$FeatureShowLabel,
                                   label.size = input$FeatureLabelSize,
                                   alpha = input$FeaturePointAlpha,
                                   min.cutoff = expr_min_cutoff,
                                   max.cutoff = expr_max_cutoff)

        }else{ # split
          p <- Seurat::FeaturePlot(cds,
                                   features = features_dimplot$features_current,
                                   pt.size = input$FeaturePointSize,
                                   reduction = input$FeatureDimensionReduction,
                                   slot = input$FeatureSlot,
                                   cols =  c(input$FeaturePlotLowestExprColor,input$FeaturePlotHighestExprColor),
                                   split.by = FeatureSplit.Revised(),
                                   label = input$FeatureShowLabel,
                                   label.size = input$FeatureLabelSize,
                                   alpha = input$FeaturePointAlpha,
                                   min.cutoff = expr_min_cutoff,
                                   max.cutoff = expr_max_cutoff)
          if (length( features_dimplot$features_current) == 1) { # only one gene
            plot_numbers <- length(levels(cds@meta.data[,FeatureSplit.Revised()]))
            p <- p + patchwork::plot_layout(ncol = ceiling(sqrt(plot_numbers)),
                                            nrow = ceiling(plot_numbers/ceiling(sqrt(plot_numbers))))
          }
        }
      }
    }
    ggplot2::ggsave(paste0(temp_dir,"/featureplot.pdf"),
                    p,
                    width = featureplot_width() * px2cm,
                    height = featureplot_width() * input$FeaturePlotHWRatio * px2cm,
                    units = "cm",
                    limitsize = FALSE)
    return(p)
  }, height = function(){session$clientData$output_featureplot_width * input$FeaturePlotHWRatio})
  # box plot: height = width default


  output$downloadfeatureplot <- downloadHandler(
    filename = function(){'featureplot.pdf'},
    content = function(file) {
      if (file.exists(paste0(temp_dir,"/featureplot.pdf"))) { # problem: will throw an error when file not exists; or with a uncorrected input, will download the pic of previous corrected input.
        file.copy(paste0(temp_dir,"/featureplot.pdf"), file, overwrite=TRUE)
      }
    })

  ################################ Violin Plot
  # define slot Choice UI
  output$VlnAssaySlots.UI <- renderUI({
    req(input$VlnAssay)
    if(verbose){message("SeuratExplorer: preparing VlnAssaySlots.UI...")}
    slot_choices <- filter_slot(assay_info = data$assays_slots_options,
                                assay_selected = input$VlnAssay,
                                allowed_slots = assay_allowed_slots[['VlnAssay']])
    selectInput("VlnSlot", "Slot:",
                choices = slot_choices,
                selected = ifelse('data' %in% slot_choices, 'data', slot_choices[1]))
  })

  # only render plot when the inputs are really changed
  features_vlnplot <- reactiveValues(features_current = NA, features_last = NA)

  observeEvent(input$VlnGeneSymbol,{
    features_input <- CheckGene(InputGene = input$VlnGeneSymbol,
                                GeneLibrary =  c(rownames(data$obj@assays[[input$VlnAssay]]),
                                                 data$extra_qc_options))
    if (!identical(sort(features_vlnplot$features_current), sort(features_input))) {
      features_vlnplot$features_last <- features_vlnplot$features_current
      features_vlnplot$features_current <- features_input
    }
  })

  output$Vlnhints.UI <- renderUI({
    if(verbose){message("SeuratExplorer: preparing Vlnhints.UI...")}
    helpText(strong(paste("Also supports: ",
                          paste(data$extra_qc_options, collapse = " "),
                          ".",
                          sep = "")),
             br(),
             strong("Tips: You can paste multiple genes from a column in excel."),style = "font-size:12px;")
  })

  # define the idents used
  output$VlnIdentsSelected.UI <- renderUI({
    req(input$VlnClusterResolution)
    if(verbose){message("SeuratExplorer: preparing VlnIdentsSelected.UI...")}
    shinyWidgets::pickerInput(inputId = "VlnIdentsSelected", label = "Clusters Used:",
                              choices = levels(data$obj@meta.data[,input$VlnClusterResolution]),
                              selected = levels(data$obj@meta.data[,input$VlnClusterResolution]),
                              options = shinyWidgets::pickerOptions(actionsBox = TRUE,
                                                                    size = 10,
                                                                    selectedTextFormat = "count > 3"),
                              multiple = TRUE)
  })

  # define Cluster order
  output$VlnClusterOrder.UI <- renderUI({
    if(verbose){message("SeuratExplorer: preparing VlnClusterOrder.UI...")}
    shinyjqui::orderInput(inputId = 'VlnClusterOrder',
                          label = 'Drag to order:',
                          # items = levels(data$obj@meta.data[,input$VlnClusterResolution]),
                          items = input$VlnIdentsSelected,
                          width = '100%')
  })

  # when change cluster resolution, open the shinyBS::bsCollapsePanel, otherwise will cause cluster order not update
  observeEvent(input$VlnClusterResolution, ({
    if(verbose){message("SeuratExplorer: updateCollapse for collapseVlnplot...")}
    shinyBS::updateCollapse(session, "collapseVlnplot", open = "0")
  }))



  # define Split Choice UI
  output$VlnSplitBy.UI <- renderUI({
    if(verbose){message("SeuratExplorer: preparing VlnSplitBy.UI...")}
    selectInput("VlnSplitBy","Split by:", choices = c("None" = "None", data$split_options))
  })


  # Conditional panel: show this panel when split.by is selected and the the level equals to 2
  output$Vlnplot_splitoption_twolevels = reactive({
    req(input$VlnSplitBy)
    if(verbose){message("SeuratExplorer: preparing Vlnplot_splitoption_twolevels...")}
    if (input$VlnSplitBy == "None"){
      return(FALSE)
    }else if(length(levels(data$obj@meta.data[,input$VlnSplitBy])) == 2) {
      return(TRUE)
    }else{
      return(FALSE)
    }
  })

  # Disable suspend for output$file_loaded,
  # When TRUE (the default), the output object will be suspended (not execute) when it is hidden on the web page.
  # When FALSE, the output object will not suspend when hidden, and if it was already hidden and suspended,
  # then it will resume immediately.
  outputOptions(output, 'Vlnplot_splitoption_twolevels', suspendWhenHidden = FALSE)

  # Conditional panel: show this panel when input multiple gene symbols
  output$Vlnplot_multiple_genes = reactive({
    req(input$VlnGeneSymbol)
    if(verbose){message("SeuratExplorer: preparing Vlnplot_multiple_genes...")}
    if (length(features_vlnplot$features_current) > 1) {
      return(TRUE)
    }else{
      return(FALSE)
    }
  })

  outputOptions(output, 'Vlnplot_multiple_genes', suspendWhenHidden = FALSE)


  # Conditional panel: show this panel when input multiple genes and stack is set to TRUE
  output$Vlnplot_StackPlot = reactive({
    req(input$VlnStackPlot)
    req(input$VlnGeneSymbol)
    if(verbose){message("SeuratExplorer: preparing Vlnplot_StackPlot...")}
    if (length(features_vlnplot$features_current) > 1 & input$VlnStackPlot) {
      return(TRUE)
    }else{
      return(FALSE)
    }
  })

  outputOptions(output, 'Vlnplot_StackPlot', suspendWhenHidden = FALSE)

  # Revise Split selection which will be appropriate for DimPlot, FeaturePlot and Vlnplot functions.
  VlnSplit.Revised <- reactive({
    if(verbose){message("SeuratExplorer: preparing VlnSplit.Revised...")}
    req(input$VlnSplitBy)
    # Revise the Split choice
    if(is.na(input$VlnSplitBy) | input$VlnSplitBy == "None") {
      return(NULL)
    }else{
      return(input$VlnSplitBy)
    }
  })

  # reset VlnSplitPlot value to FALSE when change the split options
  observe({
    req(input$VlnSplitBy)
    if(verbose){message("SeuratExplorer: vlnplot update UI...")}
    updateCheckboxInput(session, "VlnSplitPlot", value = FALSE)
    updateCheckboxInput(session, "VlnStackPlot", value = FALSE)
    updateCheckboxInput(session, "VlnFlipPlot", value = FALSE)
    updateSelectInput(session, "VlnFillBy", selected = "feature")
  })

  # shiny related bug
  # debug in future! 2024.05.15
  # how to make sure renderPlot run after the observe(input$VlnSplitBy)[Warning: Error in SingleExIPlot: Unknown plot type: splitViolin,
  # for the VlnSplitPlot is not updated

  # seurat related bug
  # VlnPlot(cds,features = c("CD4","CD8A"),split.by = "orig.ident", stack = TRUE,group.by = "cca_clusters_res_0.2",flip = FALSE,split.plot = TRUE)
  # Error:
  # Error in `vln.geom()`:
  #   ! Problem while converting geom to grob.
  # Caused by error in `$<-.data.frame`:
  # Run `rlang::last_trace()` to see where the error occurred
  # not related to ggplot2, pathcwork, rlang versions

  vlnplot_width  <- reactive({ session$clientData$output_vlnplot_width })

  output$vlnplot <- renderPlot({
    if(verbose){message("SeuratExplorer: preparing vlnplot...")}
    if (any(is.na(features_vlnplot$features_current))) { # when NA value
      p <- empty_plot # when no symbol or wrong input, show a blank pic.
    }else{
      cds <- data$obj
      cds@meta.data[,input$VlnClusterResolution] <- factor(cds@meta.data[,input$VlnClusterResolution],
                                                           levels = input$VlnClusterOrder)
      SeuratObject::Idents(cds) <- input$VlnClusterResolution
      # check gene again, if all the input symbols not exist in the selected assay, specially case: when switch assay!
      if(!any(features_vlnplot$features_current %in% c(rownames(cds[[input$VlnAssay]]),data$extra_qc_options))){
        p <- empty_plot
      }else{
        if(length(features_vlnplot$features_current) == 1) { # only One Gene
          p <- Seurat::VlnPlot(cds,
                               features = features_vlnplot$features_current,
                               assay = input$VlnAssay,
                               layer = input$VlnSlot,
                               split.by = VlnSplit.Revised(),
                               split.plot = input$VlnSplitPlot,
                               pt.size = input$VlnPointSize,
                               alpha = input$VlnPointAlpha,
                               idents = input$VlnIdentsSelected) &
            ggplot2::theme(axis.text.x = ggplot2::element_text(size = input$VlnXlabelSize),
                           axis.text.y = ggplot2::element_text(size = input$VlnYlabelSize))
        }else{ # multiple genes
          p <- Seurat::VlnPlot(cds,
                               features = features_vlnplot$features_current,
                               assay = input$VlnAssay,
                               layer = input$VlnSlot,
                               split.by = VlnSplit.Revised(),
                               split.plot = input$VlnSplitPlot,
                               stack = input$VlnStackPlot,
                               flip = input$VlnFlipPlot,
                               fill.by = input$VlnFillBy,
                               idents = input$VlnIdentsSelected,
                               pt.size = input$VlnPointSize,
                               alpha = input$VlnPointAlpha) &
            ggplot2::theme(axis.text.x = ggplot2::element_text(size = input$VlnXlabelSize),
                           axis.text.y = ggplot2::element_text(size = input$VlnYlabelSize))
        }
        if (input$Vlnfillcolorplatte != 'default' & input$VlnSplitBy == 'None'){
          # color
          fill.colors <- getColors(color.platte = color_list,
                                   choice = input$Vlnfillcolorplatte,
                                   n = length(levels(Idents(cds))))
          names(fill.colors) <- levels(Idents(cds))
          p <- p & scale_fill_manual(values = fill.colors)
        }
      }
    }
    ggplot2::ggsave(paste0(temp_dir,"/vlnplot.pdf"),
                    p,
                    width = vlnplot_width() * px2cm,
                    height = vlnplot_width() * input$VlnPlotHWRatio * px2cm,
                    units = "cm",
                    limitsize = FALSE)
    return(p)
  }, height = function(){session$clientData$output_vlnplot_width * input$VlnPlotHWRatio})
  # box plot: height = width default


  output$downloadvlnplot <- downloadHandler(
    filename = function(){'vlnplot.pdf'},
    content = function(file) {
      if (file.exists(paste0(temp_dir,"/vlnplot.pdf"))) {
        file.copy(paste0(temp_dir,"/vlnplot.pdf"), file, overwrite=TRUE)
      }
    })


  ################################ Dot Plot
  # only render plot when the inputs are really changed
  features_dotplot <- reactiveValues(features_current = NA, features_last = NA)

  observeEvent(input$DotGeneSymbol,{
    features_input <- CheckGene(InputGene = input$DotGeneSymbol,
                                GeneLibrary =  rownames(data$obj@assays[[input$DotAssay]]))
    if (!identical(sort(features_dotplot$features_current), sort(features_input))) {
      features_dotplot$features_last <- features_dotplot$features_current
      features_dotplot$features_current <- features_input
    }
  })


  output$Dothints.UI <- renderUI({
    if(verbose){message("SeuratExplorer: preparing Dothints.UI...")}
    helpText(strong("Tips: You can paste multiple genes from a column in excel."),
             style = "font-size:12px;")
  })

  # define the idents used
  output$DotIdentsSelected.UI <- renderUI({
    req(input$DotClusterResolution)
    if(verbose){message("SeuratExplorer: preparing DotIdentsSelected.UI...")}
    shinyWidgets::pickerInput(inputId = "DotIdentsSelected", label = "Clusters Used:",
                              choices = levels(data$obj@meta.data[,input$DotClusterResolution]),
                              selected = levels(data$obj@meta.data[,input$DotClusterResolution]),
                              options = shinyWidgets::pickerOptions(actionsBox = TRUE,
                                                                    size = 10,
                                                                    selectedTextFormat = "count > 3"),
                              multiple = TRUE)
  })

  # define Cluster order
  output$DotClusterOrder.UI <- renderUI({
    if(verbose){message("SeuratExplorer: preparing DotClusterOrder.UI...")}
    shinyjqui::orderInput(inputId = 'DotClusterOrder',
                          label = 'Drag to order:',
                          # items = levels(data$obj@meta.data[,input$DotClusterResolution]),
                          items = input$DotIdentsSelected,
                          width = '100%')
  })

  # when change cluster resolution, open the shinyBS::bsCollapsePanel, otherwise will cause cluster order not update
  observeEvent(input$DotClusterResolution, ({
    if(verbose){message("SeuratExplorer: updateCollapse for collapseDotplot...")}
    shinyBS::updateCollapse(session, "collapseDotplot", open = "0")
  }))

  # define Split Choice UI
  output$DotSplitBy.UI <- renderUI({
    if(verbose){message("SeuratExplorer: preparing DotSplitBy.UI...")}
    selectInput("DotSplitBy","Split by:", choices = c("None" = "None", data$split_options))
  })


  # Revise Split selection which will be appropriate for DimPlot, FeaturePlot and Vlnplot functions.
  DotSplit.Revised <- reactive({
    req(input$DotSplitBy)
    if(verbose){message("SeuratExplorer: preparing DotSplit.Revised...")}
    # Revise the Split choice
    if(is.na(input$DotSplitBy) | input$DotSplitBy == "None") {
      return(NULL)
    }else{
      return(input$DotSplitBy)
    }
  })

  # Conditional panel: when split is NULL, You can set the corresponding color for highest and lowest value,
  # when split is not NULL, ggplot2 will generate colors for point.
  output$DotPlot_Split_isNone <- reactive({
    req(input$DotSplitBy)
    if(verbose){message("SeuratExplorer: preparing DotPlot_Split_isNone...")}
    if(is.na(input$DotSplitBy) | input$DotSplitBy == "None") {
      return(TRUE)
    }else{
      return(FALSE)
    }
  })

  outputOptions(output, 'DotPlot_Split_isNone', suspendWhenHidden = FALSE)

  dotplot_width  <- reactive({ session$clientData$output_dotplot_width })

  output$dotplot <- renderPlot({
    if(verbose){message("SeuratExplorer: preparing dotplot...")}
    if (any(is.na(features_dotplot$features_current))) { # NA
      p <- empty_plot # when no symbol or wrong input, show a blank pic.
    }else{
      cds <- data$obj
      Seurat::DefaultAssay(cds) <- input$DotAssay
      Idents(cds) <- input$DotClusterResolution
      cds@meta.data[,input$DotClusterResolution] <- factor(cds@meta.data[,input$DotClusterResolution],
                                                           levels = input$DotClusterOrder)
      # check gene again, if all the input symbols not exist in the selected assay, specially case: when switch assay!
      if(!any(features_dotplot$features_current %in% rownames(cds[[input$DotAssay]]))){
        p <- empty_plot
      }else{
        if (is.null(DotSplit.Revised())) {
          p <- Seurat::DotPlot(cds,
                               features = features_dotplot$features_current,
                               group.by = input$DotClusterResolution,
                               idents = input$DotIdentsSelected,
                               split.by = DotSplit.Revised(),
                               cluster.idents = input$DotClusterIdents,
                               dot.scale = input$DotDotScale,
                               cols = c(input$DotPlotLowestExprColor, input$DotPlotHighestExprColor))
        }else{
          split.levels.length <- length(levels(cds@meta.data[,DotSplit.Revised()]))
          p <- Seurat::DotPlot(cds,
                               features = features_dotplot$features_current,
                               group.by = input$DotClusterResolution,
                               idents = input$DotIdentsSelected,
                               split.by = DotSplit.Revised(),
                               cluster.idents = input$DotClusterIdents,
                               dot.scale = input$DotDotScale,
                               cols = scales::hue_pal()(split.levels.length))
        }
        p <- p & ggplot2::theme(axis.text.x = ggplot2::element_text(size = input$DotXlabelSize),
                                axis.text.y = ggplot2::element_text(size = input$DotYlabelSize))
        if (input$DotRotateAxis) { p <- p + Seurat::RotatedAxis() }
        if (input$DotFlipCoordinate) { p <- p + ggplot2::coord_flip() }
      }
    }
    ggplot2::ggsave(paste0(temp_dir,"/dotplot.pdf"),
                    p,
                    width = dotplot_width() * px2cm,
                    height = dotplot_width() * input$DotPlotHWRatio * px2cm,
                    units = "cm",
                    limitsize = FALSE)
    return(p)
  }, height = function(){session$clientData$output_dotplot_width * input$DotPlotHWRatio})
  # box plot: height = width default


  output$downloaddotplot <- downloadHandler(
    filename = function(){'dotplot.pdf'},
    content = function(file) {
      if (file.exists(paste0(temp_dir,"/dotplot.pdf"))) {
        file.copy(paste0(temp_dir,"/dotplot.pdf"), file, overwrite=TRUE)
      }
    })

  ################################ Heatmap Cell Level
  # define slot Choice UI
  output$HeatmapAssaySlots.UI <- renderUI({
    req(input$HeatmapAssay)
    if(verbose){message("SeuratExplorer: preparing HeatmapAssaySlots.UI...")}
    slot_choices <- filter_slot(assay_info = data$assays_slots_options,
                                assay_selected = input$HeatmapAssay,
                                allowed_slots = assay_allowed_slots[['HeatmapAssay']])
    selectInput("HeatmapSlot", "Slot:",
                choices = slot_choices,
                selected = ifelse('scale.data' %in% slot_choices, 'scale.data', slot_choices[1]))
  })


  # only render plot when the inputs are really changed
  features_heatmap <- reactiveValues(features_current = NA, features_last = NA)

  observeEvent(input$HeatmapGeneSymbol,{
    features_input <- CheckGene(InputGene = input$HeatmapGeneSymbol,
                                GeneLibrary =  rownames(data$obj@assays[[input$HeatmapAssay]]))
    if (!identical(sort(features_heatmap$features_current), sort(features_input))) {
      features_heatmap$features_last <- features_heatmap$features_current
      features_heatmap$features_current <- features_input
    }
  })

  output$Heatmaphints.UI <- renderUI({
    if(verbose){message("SeuratExplorer: preparing Heatmaphints.UI...")}
    helpText(strong("Tips: You can paste multiple genes from a column in excel."),
             style = "font-size:12px;")
  })

  # define Cluster order
  output$HeatmapClusterOrder.UI <- renderUI({
    if(verbose){message("SeuratExplorer: preparing HeatmapClusterOrder.UI...")}
    shinyjqui::orderInput(inputId = 'HeatmapClusterOrder',
                          label = 'Drag to order:',
                          items = levels(data$obj@meta.data[,input$HeatmapClusterResolution]),
                          width = '100%')
  })

  observeEvent(input$HeatmapClusterResolution, ({
    if(verbose){message("SeuratExplorer: updateCollapse for collapseHeatmap...")}
    shinyBS::updateCollapse(session, "collapseHeatmap", open = "0")
  }))

  heatmap_width  <- reactive({ session$clientData$output_heatmap_width })

  output$heatmap <- renderPlot({
    if(verbose){message("SeuratExplorer: preparing heatmap...")}
    if (any(is.na(features_heatmap$features_current))) { # NA
      p <- empty_plot # when no symbol or wrong input, show a blank pic.
    }else{
      cds <- data$obj
      cds@meta.data[,input$HeatmapClusterResolution] <- factor(cds@meta.data[,input$HeatmapClusterResolution],
                                                               levels = input$HeatmapClusterOrder)
      # check gene again, if all the input symbols not exist in the selected assay, specially case: when switch assay!
      if(!any(features_heatmap$features_current %in% rownames(cds[[input$HeatmapAssay]]))){
        p <- empty_plot
      }else{
        if (!all(features_heatmap$features_current %in% Seurat::VariableFeatures(cds)) &
            input$HeatmapSlot == 'scale.data') {
          cds <- Seurat::ScaleData(object = cds,
                                   # use only one gene to scaledata() will throw an error
                                   features = unique(c(Seurat::VariableFeatures(cds),
                                                       features_heatmap$features_current)))
        }
        p <- Seurat::DoHeatmap(object = cds,
                               features = features_heatmap$features_current,
                               assay = input$HeatmapAssay,
                               slot = input$HeatmapSlot,
                               group.by = input$HeatmapClusterResolution,
                               size = input$HeatmapTextSize,
                               hjust = input$HeatmapTextHjust,
                               vjust = input$HeatmapTextVjust,
                               angle = input$HeatmapTextRatateAngle,
                               group.bar.height = input$HeatmapGroupBarHeight,
                               lines.width = input$HeatmapLineWidth) &
          ggplot2::theme(axis.text.y = ggplot2::element_text(size = input$HeatmapFeatureTextSize))
      }
    }
    ggplot2::ggsave(paste0(temp_dir,"/heatmap.pdf"),
                    p,
                    width = heatmap_width() * px2cm,
                    height = heatmap_width() * input$HeatmapPlotHWRatio * px2cm,
                    units = "cm",
                    limitsize = FALSE)
    return(p)
  }, height = function(){session$clientData$output_heatmap_width * input$HeatmapPlotHWRatio})
  # box plot: height = width default


  output$downloadheatmap <- downloadHandler(
    filename = function(){'heatmap.pdf'},
    content = function(file) {
      if (file.exists(paste0(temp_dir,"/heatmap.pdf"))) {
        file.copy(paste0(temp_dir,"/heatmap.pdf"), file, overwrite=TRUE)
      }
    })

  ################################ Group Averaged Heatmap
  # only render plot when the inputs are really changed
  features_heatmap_averaged <- reactiveValues(features_current = NA, features_last = NA)

  observeEvent(input$AveragedHeatmapGeneSymbol,{
    features_input <- CheckGene(InputGene = input$AveragedHeatmapGeneSymbol,
                                GeneLibrary =  rownames(data$obj@assays[[input$AveragedHeatmapAssay]]))
    if (!identical(sort(features_heatmap_averaged$features_current), sort(features_input))) {
      features_heatmap_averaged$features_last <- features_heatmap_averaged$features_current
      features_heatmap_averaged$features_current <- features_input
    }
  })

  output$AveragedHeatmaphints.UI <- renderUI({
    if(verbose){message("SeuratExplorer: preparing AveragedHeatmaphints.UI...")}
    helpText(strong("Tips: You can paste multiple genes from a column in excel."),
             style = "font-size:12px;")
  })

  # define the idents used
  output$AveragedHeatmapIdentsSelected.UI <- renderUI({
    req(input$AveragedHeatmapClusterResolution)
    if(verbose){message("SeuratExplorer: preparing AveragedHeatmapIdentsSelected.UI...")}
    shinyWidgets::pickerInput(inputId = "AveragedHeatmapIdentsSelected", label = "Clusters Used:",
                              choices = levels(data$obj@meta.data[,input$AveragedHeatmapClusterResolution]),
                              selected = levels(data$obj@meta.data[,input$AveragedHeatmapClusterResolution]),
                              options = shinyWidgets::pickerOptions(actionsBox = TRUE,
                                                                    size = 10,
                                                                    selectedTextFormat = "count > 3"),
                              multiple = TRUE)
  })

  # define Cluster order
  output$AveragedHeatmapClusterOrder.UI <- renderUI({
    if(verbose){message("SeuratExplorer: preparing AveragedHeatmapClusterOrder.UI...")}
    shinyjqui::orderInput(inputId = 'AveragedHeatmapClusterOrder',
                          label = 'Drag to order:',
                          # items = levels(data$obj@meta.data[,input$AveragedHeatmapClusterResolution]),
                          items = input$AveragedHeatmapIdentsSelected,
                          width = '100%')
  })

  observeEvent(input$AveragedHeatmapClusterResolution, ({
    if(verbose){message("SeuratExplorer: updateCollapse for AveragedcollapseHeatmap...")}
    shinyBS::updateCollapse(session, "AveragedcollapseHeatmap", open = "0")
  }))



  averagedheatmap_width  <- reactive({ session$clientData$output_averagedheatmap_width })

  output$averagedheatmap <- renderPlot({
    if(verbose){message("SeuratExplorer: preparing averagedheatmap...")}
    if (any(is.na(features_heatmap_averaged$features_current))) { # NA
      p <- empty_plot # when no symbol or wrong input, show a blank pic.
    }else{
      cds <- data$obj
      Seurat::DefaultAssay(cds) <- input$AveragedHeatmapAssay
      Seurat::Idents(cds) <- input$AveragedHeatmapClusterResolution
      cds <- subset_Seurat(cds, idents = input$AveragedHeatmapIdentsSelected)
      # cds@meta.data[,input$AveragedHeatmapClusterResolution] <- factor(cds@meta.data[,input$AveragedHeatmapClusterResolution],
      #                                                                  levels = input$AveragedHeatmapClusterOrder)
      Seurat::Idents(cds) <- factor(Seurat::Idents(cds), levels = input$AveragedHeatmapClusterOrder)
      # check gene again, if all the input symbols not exist in the selected assay, specially case: when switch assay!
      if(!any(features_heatmap_averaged$features_current %in% rownames(cds[[input$AveragedHeatmapAssay]]))){
        p <- empty_plot
      }else{
        p <- suppressMessages(AverageHeatmap(object = cds,
                                             markerGene = features_heatmap_averaged$features_current,
                                             group.by = input$AveragedHeatmapClusterResolution,
                                             feature.fontsize = input$AveragedHeatmapFeatureTextSize,
                                             cluster.fontsize = input$AveragedHeatmapClusterTextSize,
                                             assays = input$AveragedHeatmapAssay,
                                             column_names_rot = input$AveragedHeatmapClusterTextRatateAngle,
                                             cluster_columns = input$AveragedHeatmapClusterClusters,
                                             cluster_rows = input$AveragedHeatmapClusterFeatures))
      }
    }
    pdf(file = paste0(temp_dir,"/AveragedHeatmap.pdf"),
        width = (averagedheatmap_width() * px2cm)/2.54,
        height = (averagedheatmap_width() * input$AveragedHeatmapPlotHWRatio * px2cm)/2.54)
    print(p)
    dev.off()
    # ggplot2::ggsave(paste0(temp_dir,"/AveragedHeatmap.pdf"),
    # p,
    # width = averagedheatmap_width() * px2cm,
    # height = averagedheatmap_width() * input$AveragedHeatmapPlotHWRatio * px2cm, units = "cm", limitsize = FALSE)
    return(p)
  }, height = function(){session$clientData$output_averagedheatmap_width * input$AveragedHeatmapPlotHWRatio})
  # box plot: height = width default


  output$downloadaveragedheatmap <- downloadHandler(
    filename = function(){'AveragedHeatmap.pdf'},
    content = function(file) {
      if (file.exists(paste0(temp_dir,"/AveragedHeatmap.pdf"))) {
        file.copy(paste0(temp_dir,"/AveragedHeatmap.pdf"), file, overwrite=TRUE)
      }
    })

  ################################ Ridge Plot
  # define slot Choice UI
  output$RidgeplotAssaySlots.UI <- renderUI({
    req(input$RidgeplotAssay)
    if(verbose){message("SeuratExplorer: preparing RidgeplotAssaySlots.UI...")}
    slot_choices <- filter_slot(assay_info = data$assays_slots_options,
                                assay_selected = input$RidgeplotAssay,
                                allowed_slots = assay_allowed_slots[['RidgeplotAssay']])
    selectInput("RidgeplotSlot", "Slot:",
                choices = slot_choices,
                selected = ifelse('data' %in% slot_choices, 'data', slot_choices[1])) # default use data slot
  })

  # only render plot when the inputs are really changed
  features_ridgeplot <- reactiveValues(features_current = NA, features_last = NA)

  observeEvent(input$RidgeplotGeneSymbol,{
    features_input <- CheckGene(InputGene = input$RidgeplotGeneSymbol,
                                GeneLibrary = c(rownames(data$obj@assays[[input$RidgeplotAssay]]),
                                                data$extra_qc_options))
    if (!identical(sort(features_ridgeplot$features_current), sort(features_input))) {
      features_ridgeplot$features_last <- features_ridgeplot$features_current
      features_ridgeplot$features_current <- features_input
    }
  })


  output$Ridgeplothints.UI <- renderUI({
    if(verbose){message("SeuratExplorer: preparing Ridgeplothints.UI...")}
    helpText(strong(paste("Also supports: ", paste(data$extra_qc_options, collapse = " "),
                          ".",
                          sep = "")),
             br(),
             strong("Tips: You can paste multiple genes from a column in excel."),style = "font-size:12px;")
  })

  # define the idents used
  output$RidgeplotIdentsSelected.UI <- renderUI({
    req(input$RidgeplotClusterResolution)
    if(verbose){message("SeuratExplorer: preparing RidgeplotIdentsSelected.UI...")}
    shinyWidgets::pickerInput(inputId = "RidgeplotIdentsSelected", label = "Clusters Used:",
                              choices = levels(data$obj@meta.data[,input$RidgeplotClusterResolution]),
                              selected = levels(data$obj@meta.data[,input$RidgeplotClusterResolution]),
                              options = shinyWidgets::pickerOptions(actionsBox = TRUE,
                                                                    size = 10,
                                                                    selectedTextFormat = "count > 3"),
                              multiple = TRUE)
  })

  # define Cluster order
  output$RidgeplotClusterOrder.UI <- renderUI({
    if(verbose){message("SeuratExplorer: preparing RidgeplotClusterOrder.UI...")}
    shinyjqui::orderInput(inputId = 'RidgeplotClusterOrder',
                          label = 'Drag to order:',
                          # items = levels(data$obj@meta.data[,input$RidgeplotClusterResolution]),
                          items = input$RidgeplotIdentsSelected,
                          width = '100%')
  })

  observeEvent(input$RidgeplotClusterResolution, ({
    if(verbose){message("SeuratExplorer: updateCollapse for collapseRidgeplot...")}
    shinyBS::updateCollapse(session, "collapseRidgeplot", open = "0")
  }))



  # Conditional panel: show this panel when input multiple genes and stack is set to TRUE
  output$Ridgeplot_stack_show = reactive({
    req(input$RidgeplotGeneSymbol)
    if(verbose){message("SeuratExplorer: preparing Ridgeplot_stack_show...")}
    if (length(features_ridgeplot$features_current) > 1) {
      return(TRUE)
    }else{
      return(FALSE)
    }
  })

  outputOptions(output, 'Ridgeplot_stack_show', suspendWhenHidden = FALSE)

  # Conditional panel: show this panel when input multiple genes and stack is set to TRUE
  output$Ridgeplot_stack_NotSelected = reactive({
    req(input$RidgeplotStackPlot)
    if(verbose){message("SeuratExplorer: preparing Ridgeplot_stack_NotSelected...")}
    !input$RidgeplotStackPlot
  })

  outputOptions(output, 'Ridgeplot_stack_NotSelected', suspendWhenHidden = FALSE)

  # reset VlnSplitPlot value to FALSE when change the input gene symbols
  observe({
    req(input$RidgeplotGeneSymbol)
    if(verbose){message("SeuratExplorer: update RidgeplotStackPlot...")}
    updateCheckboxInput(session, "RidgeplotStackPlot", value = FALSE)
  })

  ridgeplot_width  <- reactive({ session$clientData$output_ridgeplot_width })

  output$ridgeplot <- renderPlot({
    if(verbose){message("SeuratExplorer: preparing ridgeplot...")}
    if (any(is.na(features_ridgeplot$features_current))) { # NA
      p <- empty_plot # when no symbol or wrong input, show a blank pic.
    }else{
      cds <- data$obj
      Seurat::DefaultAssay(cds) <- input$RidgeplotAssay
      Seurat::Idents(cds) <- input$RidgeplotClusterResolution
      cds <- subset_Seurat(cds, idents = input$RidgeplotIdentsSelected)
      Seurat::Idents(cds) <- factor(Seurat::Idents(cds), levels = input$RidgeplotClusterOrder)
      # Seurat::Idents(cds) <- input$RidgeplotClusterResolution
      # cds@meta.data[,input$RidgeplotClusterResolution] <- Seurat::Idents(cds)
      # check gene again, if all the input symbols not exist in the selected assay, specially case: when switch assay!
      if(!any(features_ridgeplot$features_current %in% c(rownames(cds[[input$RidgeplotAssay]]), data$extra_qc_options))){
        p <- empty_plot
      }else{
        p <- Seurat::RidgePlot(object = cds,
                               features = features_ridgeplot$features_current,
                               assay = input$RidgeplotAssay,
                               layer = input$RidgeplotSlot,
                               ncol = input$RidgeplotNumberOfColumns,
                               stack = input$RidgeplotStackPlot,
                               fill.by = input$RidgeplotFillBy
                               # not use group.by, use Idents(cds) <- input$RidgeplotClusterResolution
                               # because if only one level in existed in the Idents, will throw an error!
                               # group.by = input$RidgeplotClusterResolution
                               # idents = input$RidgeplotIdentsSelected
                               ) &
          ggplot2::theme(axis.text.x = ggplot2::element_text(size = input$RidgeplotXlabelSize),
                         axis.text.y = ggplot2::element_text(size = input$RidgeplotYlabelSize))
      }
    }
    ggplot2::ggsave(paste0(temp_dir,"/ridgeplot.pdf"),
                    p,
                    width = ridgeplot_width() * px2cm,
                    height = ridgeplot_width() * input$RidgeplotHWRatio * px2cm,
                    units = "cm",
                    limitsize = FALSE)
    return(p)
  }, height = function(){session$clientData$output_ridgeplot_width * input$RidgeplotHWRatio})
  # box plot: height = width default


  output$downloadridgeplot <- downloadHandler(
    filename = function(){'ridgeplot.pdf'},
    content = function(file) {
      if (file.exists(paste0(temp_dir,"/ridgeplot.pdf"))) {
        file.copy(paste0(temp_dir,"/ridgeplot.pdf"), file, overwrite=TRUE)
      }
    })

  ################################ Cell ratio Plot
  # define Fill choices
  output$CellratioFillChoice.UI <- renderUI({
    if(verbose){message("SeuratExplorer: preparing CellratioFillChoice.UI...")}
    selectInput("CellratioFillChoice","Fill in choice:",
                choices = data$cluster_options,
                selected = data$cluster_default)
  })

  # define the idents used
  output$CellratioIdentsSelected.UI <- renderUI({
    req(input$CellratioFillChoice)
    if(verbose){message("SeuratExplorer: CellratioIdentsSelected.UI...")}
    shinyWidgets::pickerInput(inputId = "CellratioIdentsSelected", label = "Clusters Used:",
                              choices = levels(data$obj@meta.data[,input$CellratioFillChoice]),
                              selected = levels(data$obj@meta.data[,input$CellratioFillChoice]),
                              options = shinyWidgets::pickerOptions(actionsBox = TRUE,
                                                                    size = 10,
                                                                    selectedTextFormat = "count > 3"),
                              multiple = TRUE)
  })

  # define Fill order
  output$CellratioplotFillOrder.UI <- renderUI({
    if(verbose){message("SeuratExplorer: preparing CellratioplotFillOrder.UI...")}
    shinyjqui::orderInput(inputId = 'CellratioFillOrder',
                          label = 'Drag to order:',
                          # items = levels(data$obj@meta.data[,input$CellratioFillChoice]),
                          items = input$CellratioIdentsSelected,
                          width = '100%')
  })

  # define X choices
  output$CellratioXChoice.UI <- renderUI({
    req(input$CellratioFillChoice)
    if(verbose){message("SeuratExplorer: preparing CellratioXChoice.UI...")}
    selectInput("CellratioXChoice","X axis choice:",
                choices = data$cluster_options[!data$cluster_options %in% input$CellratioFillChoice])
  })


  # define x choice order
  output$CellratioplotXOrder.UI <- renderUI({
    if(verbose){message("SeuratExplorer: preparing CellratioplotXOrder.UI...")}
    shinyjqui::orderInput(inputId = 'CellratioXOrder', label = 'Drag to order:',
                          items = levels(data$obj@meta.data[,input$CellratioXChoice]),
                          width = '100%')
  })



  # define Facet choices
  output$CellratioFacetChoice.UI <- renderUI({
    req(input$CellratioXChoice)
    if(verbose){message("SeuratExplorer: preparing CellratioFacetChoice.UI...")}
    selectInput("CellratioFacetChoice","Facet choice:",
                choices = c("None" = "None",
                            data$cluster_options[!data$cluster_options %in%
                                                   c(input$CellratioFillChoice, input$CellratioXChoice)]),
                selected = "None")
  })

  # Revise FacetChoice which will be appropriate for plot
  FacetChoice.Revised <- reactive({
    req(input$CellratioFacetChoice)
    if(verbose){message("SeuratExplorer: FacetChoice.Revised...")}
    # Revise the Split choice
    if(is.na(input$CellratioFacetChoice) | input$CellratioFacetChoice == "None") {
      return(NULL)
    }else{
      return(input$CellratioFacetChoice)
    }
  })

  # define Facet order
  output$CellratioplotFacetOrder.UI <- renderUI({
    if(verbose){message("SeuratExplorer: preparing CellratioplotFacetOrder.UI...")}
    if (!is.null(FacetChoice.Revised())) {
      shinyjqui::orderInput(inputId = 'CellratioFacetOrder',
                            label = 'Drag to order:',
                            items = levels(data$obj@meta.data[,input$CellratioFacetChoice]),
                            width = '100%')
    }else{

    }
  })

  cellratioplot_width  <- reactive({ session$clientData$output_cellratioplot_width})

  # plot
  output$cellratioplot <- renderPlot({
    req(input$CellratioXChoice)
    req(input$CellratioXOrder)
    req(input$CellratioFillChoice)
    req(input$CellratioFillOrder)
    req(input$CellratioIdentsSelected)
    if(verbose){message("SeuratExplorer: preparing cellratioplot...")}
    cds <- data$obj
    if (is.null(FacetChoice.Revised())) { # not facet
      p <- cellRatioPlot(object = cds,
                         idents = input$CellratioIdentsSelected,
                         sample.name = input$CellratioXChoice,
                         sample.order = input$CellratioXOrder,
                         celltype.name = input$CellratioFillChoice,
                         celltype.order = input$CellratioFillOrder,
                         facet.name = NULL,
                         facet.order = NULL,
                         col.width = input$CellratioColumnWidth,
                         flow.alpha = input$CellratioFlowAlpha,
                         flow.curve = input$CellratioFlowCurve,
                         color.choice = input$Cellratiofillcolorplatte)
    }else{
      p <- cellRatioPlot(object = cds,
                         idents = input$CellratioIdentsSelected,
                         sample.name = input$CellratioXChoice,
                         sample.order = input$CellratioXOrder,
                         celltype.name = input$CellratioFillChoice,
                         celltype.order = input$CellratioFillOrder,
                         facet.name = FacetChoice.Revised(),
                         facet.order = input$CellratioFacetOrder,
                         col.width = input$CellratioColumnWidth,
                         flow.alpha = input$CellratioFlowAlpha,
                         flow.curve = input$CellratioFlowCurve,
                         color.choice = input$Cellratiofillcolorplatte)
    }
    if (input$CellratioRotateAxis) {
      p <- p & ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 45,
                                                                  vjust = 1,
                                                                  hjust=1))
    }
    ggplot2::ggsave(paste0(temp_dir,"/cellratioplot.pdf"),
                    p,
                    width = cellratioplot_width() * px2cm,
                    height = cellratioplot_width() * input$CellratioplotHWRatio * px2cm,
                    units = "cm",
                    limitsize = FALSE)
    return(p)
  }, height = function(){session$clientData$output_cellratioplot_width * input$CellratioplotHWRatio})
  # box plot: height = width default

  # download
  output$downloadcellratioplot <- downloadHandler(
    filename = function(){'cellratioplot.pdf'},
    content = function(file) {
      if (file.exists(paste0(temp_dir,"/cellratioplot.pdf"))) {
        file.copy(paste0(temp_dir,"/cellratioplot.pdf"), file, overwrite=TRUE)
      }
    })

  output$cellratiodata <- renderTable({
    req(input$CellratioFillChoice)
    meta <- data$obj@meta.data
    # subset
    meta <- meta[meta[,input$CellratioFillChoice] %in% input$CellratioIdentsSelected,]
    meta[,input$CellratioFillChoice] <- as.character(meta[,input$CellratioFillChoice])
    if (is.null(FacetChoice.Revised())) {
      table(meta[,input$CellratioFillChoice], meta[,input$CellratioXChoice])
    }else{
      table(meta[,input$CellratioFacetChoice], meta[, input$CellratioFillChoice], meta[, input$CellratioXChoice])
    }
  })

  ################################ DEGs analysis
  # Warning
  output$degs_info = renderText({
    paste0('This usually takes longer, please wait patiently. Make sure to save current results before a new analysis!
  - FindMarkers for All Clusters: calculate markers for all groups.
  - Find DEGs for two groups: comparison between two groups, support subet cells before a comparison.')
  })

  DEGs <- reactiveValues(degs = NULL, degs_ready = FALSE)

  output$DEGs_ready <- reactive({
    return(DEGs$degs_ready)
  })

  outputOptions(output, 'DEGs_ready', suspendWhenHidden=FALSE)

  # Part-1: Cluster Markers

  observeEvent(input$DEGsClusterMarkersAnalysis, {
    if(verbose){message("SeuratExplorer: preparing DEGsClusterMarkersAnalysis...")}
    cds <- data$obj
    if (length(unique(as.character(Idents(cds)))) < 2) {
      showModal(modalDialog(title = "Error...",
                            "Please select a cluster resolution with more than one group!",
                            easyClose = TRUE,
                            footer = NULL,
                            size = "l"))
    }else{
      showModal(modalDialog(title = "Calculating Cluster Markers...",
                            "Please wait for a few minutes!",
                            footer= NULL,
                            size = "l"))
      cds <- check_SCT_assay(cds)
      cluster.markers <- Seurat::FindAllMarkers(cds,
                                                test.use = input$testuse,
                                                assay = input$DEGsAssay,
                                                logfc.threshold = input$logfcthreshold,
                                                group.by = input$ClusterMarkersClusterResolution,
                                                min.pct = input$minpct,
                                                min.diff.pct = ifelse(input$mindiffpct, input$mindiffpct, -Inf),
                                                only.pos = TRUE)
      removeModal()
      DEGs$degs <- cluster.markers
      DEGs$degs_ready <- TRUE
    }
  })

  # Part-2: Find DEGs for two groups
  # define Cluster Annotation choice
  output$IntraClusterDEGsCustomizedGroups.UI <- renderUI({
    if(verbose){message("SeuratExplorer: preparing IntraClusterDEGsCustomizedGroups.UI...")}
    selectInput("IntraClusterDEGsCustomizedGroups","Group Cells By:", choices = data$cluster_options)
  })

  # define the idents used
  output$IntraClusterDEGsCustomizedGroupsCase.UI <- renderUI({
    req(input$IntraClusterDEGsCustomizedGroups)
    if(verbose){message("SeuratExplorer: preparing IntraClusterDEGsCustomizedGroupsCase.UI...")}
    selectInput("IntraClusterDEGsCustomizedGroupsCase","Choose Case groups:",
                choices = levels(data$obj@meta.data[,input$IntraClusterDEGsCustomizedGroups]),
                multiple = TRUE)
  })

  # define the idents used
  output$IntraClusterDEGsCustomizedGroupsControl.UI <- renderUI({
    req(input$IntraClusterDEGsCustomizedGroups)
    req(input$IntraClusterDEGsCustomizedGroupsCase)
    if(verbose){message("SeuratExplorer: preparing IntraClusterDEGsCustomizedGroupsControl.UI...")}
    selectInput("IntraClusterDEGsCustomizedGroupsControl","Choose control groups:", multiple = TRUE,
                choices = setdiff(levels(data$obj@meta.data[,input$IntraClusterDEGsCustomizedGroups]),
                                  input$IntraClusterDEGsCustomizedGroupsCase))
  })

  # define Cluster Annotation choice
  output$IntraClusterDEGsSubsetCells.UI <- renderUI({
    req(input$IntraClusterDEGsCustomizedGroups)
    if(verbose){message("SeuratExplorer: preparing IntraClusterDEGsSubsetCells.UI...")}
    selectInput("IntraClusterDEGsSubsetCells","Filter Cells By:",
                choices = setdiff(data$cluster_options, input$IntraClusterDEGsCustomizedGroups))
  })

  # define Cluster Annotation choice
  output$IntraClusterDEGsSubsetCellsSelectedClusters.UI <- renderUI({
    req(input$IntraClusterDEGsCustomizedGroups)
    req(input$IntraClusterDEGsSubsetCells)
    if(verbose){message("SeuratExplorer: preparing IntraClusterDEGsSubsetCellsSelectedClusters.UI...")}
    shinyWidgets::pickerInput(inputId = "IntraClusterDEGsSubsetCellsSelectedClusters", label = "Cells to Keep:",
                              choices = levels(data$obj@meta.data[,input$IntraClusterDEGsSubsetCells]),
                              selected = levels(data$obj@meta.data[,input$IntraClusterDEGsSubsetCells]),
                              options = shinyWidgets::pickerOptions(actionsBox = TRUE,
                                                                    size = 10,
                                                                    selectedTextFormat = "count > 3"),
                              multiple = TRUE)
  })

  # compare two groups, support subset clusters before comparison
  observeEvent(input$IntraClusterDEGssAnalysis, {
    if(verbose){message("SeuratExplorer: calculate DEGs...")}
    if (any(is.null(input$IntraClusterDEGsCustomizedGroupsCase),
            is.null(input$IntraClusterDEGsCustomizedGroupsControl),
            is.null(input$IntraClusterDEGsSubsetCellsSelectedClusters))) {
      showModal(modalDialog(title = "Error:",
                            "Please specify the case & control samples and clusters used. Press ESC to close.",
                            easyClose = TRUE,
                            footer = NULL))
    }else{
      showModal(modalDialog(title = "Calculating DEGs...", "Please wait for a few minutes!",
                            footer= NULL,
                            size = "l"))
      cds <- data$obj
      Seurat::Idents(cds) <- input$IntraClusterDEGsSubsetCells
      cds <- subset_Seurat(cds, idents = input$IntraClusterDEGsSubsetCellsSelectedClusters)
      cds <- check_SCT_assay(cds)
      cluster.markers <- Seurat::FindMarkers(cds,
                                             ident.1 = input$IntraClusterDEGsCustomizedGroupsCase,
                                             ident.2 = input$IntraClusterDEGsCustomizedGroupsControl,
                                             assay = input$DEGsAssay,
                                             group.by = input$IntraClusterDEGsCustomizedGroups,
                                             test.use = input$testuse,
                                             logfc.threshold = input$logfcthreshold,
                                             min.pct = input$minpct,
                                             min.diff.pct = ifelse(input$mindiffpct, input$mindiffpct, -Inf))
      removeModal()
      DEGs$degs <- cluster.markers
      DEGs$degs_ready <- TRUE
    }
  })

  # part-4: reset parameters
  observeEvent(input$SetDefault, {
    if(verbose){message("SeuratExplorer: reset DEGs parameters...")}
    updateSelectInput(session = session, inputId = "DEGsAssay", selected = data$assay_default)
    updateSelectInput(session = session, inputId = "testuse", selected = "wilcox")
    updateSliderInput(session, "logfcthreshold", value = 0.1 )
    updateSliderInput(session, "minpct", value = 0.01 )
    updateSliderInput(session, "mindiffpct", value = 0 )
  })

  # part-5: output results
  output$dataset_degs <-  DT::renderDT(server=FALSE,{
    req(DEGs$degs)
    if(verbose){message("SeuratExplorer: preparing dataset_degs...")}
    # Show data
    if (nrow(DEGs$degs) == 0 | is.null(DEGs$degs)) {
      showModal(modalDialog(title = "Error",
                            "None of DEGs found, You may try change the default Assay in 'Custom Parameters' page, or contact technican for details!",
                            footer= modalButton("Dismiss"),
                            easyClose = TRUE,
                            size = "l"))
      return(NULL)
    }else{
      data_res <- DT::datatable(DEGs$degs,
                                extensions = 'Buttons',
                                selection = "single",
                                options = list(scrollX=TRUE,
                                               paging = TRUE,
                                               searching = TRUE,
                                               fixedColumns = TRUE,
                                               autoWidth = TRUE,
                                               ordering = TRUE,
                                               dom = 'Bfrtip',
                                               buttons = list('copy',
                                                              list(extend = 'csv', title = "DEGs"),
                                                              list(extend = 'excel', title = "DEGs"))))
      for (acolumn in c("p_val","p_val_adj")) {
        if (acolumn %in% colnames(DEGs$degs)) {
          data_res <- DT::formatSignif(data_res, columns = acolumn, digits = 3)
        }
      }
      for (acolumn in c("avg_log2FC", "avg_diff", "avg_logFC")) {
        if (acolumn %in% colnames(DEGs$degs)) {
          data_res <- DT::formatRound(data_res, columns = acolumn, digits = 3)
        }
      }
      return(data_res)
    }
  })


  output$DEGs_row_selected <- reactive({
    if (!DEGs$degs_ready) {
      return(FALSE)
    }else if(is.null(input$dataset_degs_rows_selected)){
      return(FALSE)
    }else{
      return(TRUE)
    }
  })

  outputOptions(output, 'DEGs_row_selected', suspendWhenHidden=FALSE)

  db <- SeuratExplorer::GenesDB

  output$ExternalLinks.UI <- renderUI({
    row_count <- input$dataset_degs_rows_selected
    if ('gene' %in% colnames(DEGs$degs)) {
      selected.gene <- DEGs$degs[row_count, 'gene']
    }else{
      selected.gene <- rownames(DEGs$degs)[row_count]
    }
    selected.db <- db[[input$selectspecies]]
    if (!selected.gene %in% selected.db[,input$selectsgenetype]) {
      return(renderText("Gene not found, please check parameters above, or this gene not existed in the database."))
    }

    external_links <- h4(paste0('Gene Selected: ', selected.gene))
    if (input$selectspecies == "human") {
      # GeneCards
      unique_ids <- unique(c(na.omit(selected.db[selected.db[,input$selectsgenetype] == selected.gene,][,'Symbol'])))
      for (id in unique_ids) {
        external_links <- paste0(external_links,
                                 shiny::a(h4("GeneCards", class = "btn btn-primary" , style = "fontweight:600"),
                                          target = "_blank",
                                          href = paste0("https://www.genecards.org/cgi-bin/carddisp.pl?gene=", id)))
      }
      # Ensembl
      unique_ids <- unique(c(na.omit(selected.db[selected.db[,input$selectsgenetype] == selected.gene,][,'Ensembl'])))
      for (id in unique_ids) {
        external_links <- paste0(external_links,
                                 shiny::a(h4("Ensembl", class = "btn btn-primary" , style = "fontweight:600"),
                                          target = "_blank",
                                          href = paste0("http://www.ensembl.org/Homo_sapiens/geneview?gene=", id)))
      }
      # HGNC
      unique_ids <- unique(c(na.omit(selected.db[selected.db[,input$selectsgenetype] == selected.gene,][,'HGNC'])))
      for (id in unique_ids) {
        external_links <- paste0(external_links,
                                 shiny::a(h4("HGNC", class = "btn btn-primary" , style = "fontweight:600"),
                                          target = "_blank",
                                          href = paste0("https://www.genenames.org/data/gene-symbol-report/#!/hgnc_id/", id)))
      }
    }else if(input$selectspecies == "mouse"){
      unique_ids <- unique(c(na.omit(selected.db[selected.db[,input$selectsgenetype] == selected.gene,][,'Ensembl'])))
      # MGI
      for (id in unique_ids) {
        external_links <- paste0(external_links,
                                 shiny::a(h4("MGI", class = "btn btn-primary" , style = "fontweight:600"),
                                          target = "_blank",
                                          href = paste0("https://www.informatics.jax.org/marker/", id)))
      }
      # Ensembl
      for (id in unique_ids) {
        external_links <- paste0(external_links,
                                 shiny::a(h4("Ensembl", class = "btn btn-primary" , style = "fontweight:600"),
                                          target = "_blank",
                                          href = paste0("http://www.ensembl.org/Mus_musculus/geneview?gene=", id)))
      }
    }else if (input$selectspecies == "fly") {
      unique_ids <- unique(c(na.omit(selected.db[selected.db[,input$selectsgenetype] == selected.gene,][,'Ensembl'])))
      # flybase
      for (id in unique_ids) {
        external_links <- paste0(external_links,
                                 shiny::a(h4("FlyBase", class = "btn btn-primary" , style = "fontweight:600"),
                                          target = "_blank",
                                          href = paste0("https://flybase.org/reports/", id)))
      }
      # Ensembl
      for (id in unique_ids) {
        external_links <- paste0(external_links,
                                 shiny::a(h4("Ensembl", class = "btn btn-primary" , style = "fontweight:600"),
                                          target = "_blank",
                                          href = paste0("https://www.ensembl.org/Drosophila_melanogaster/Gene/Summary?db=core;g=", id)))
      }
    }
    # NCBI EntrezID
    unique_ids <- unique(c(na.omit(selected.db[selected.db[,input$selectsgenetype] == selected.gene, 'EntrezID'])))
    for (id in unique_ids) {
      external_links <- paste0(external_links,
                               shiny::a(h4("NCBI", class = "btn btn-primary" , style = "fontweight:600"),
                                        target = "_blank",
                                        href = paste0("https://www.ncbi.nlm.nih.gov/gene/?term=", id)))
    }
    # NCBI EntrezID
    unique_ids <- unique(c(na.omit(selected.db[selected.db[,input$selectsgenetype] == selected.gene, 'UniProt'])))
    for (id in unique_ids) {
      external_links <- paste0(external_links,
                               shiny::a(h4("UniProt", class = "btn btn-primary" , style = "fontweight:600"),
                                        target = "_blank",
                                        href = paste0("https://www.uniprot.org/uniprotkb/", id, "/entry")))
    }
    HTML(external_links)
  })

  ################################ Top genes analysis
  # Warnings
  output$topgenes_info = renderText({
    paste0('This usually takes longer, please wait patiently. Save current results before a new analysis
      - Find Top Genes by Cell: firstly, for each cell, find genes that has high UMI percentage, then summary those genes for each cluster, details see About page.
      - Find Top Genes by mean UMI Counts: for each cluster, calculate the top n highly expressed genes by mean UMI counts.')
  })

  TopGenes <- reactiveValues(topgenes = NULL, topgenes_ready = FALSE)

  output$TopGenes_ready <- reactive({
    return(TopGenes$topgenes_ready)
  })

  outputOptions(output, 'TopGenes_ready', suspendWhenHidden=FALSE)

  # define Cluster Annotation choice
  output$TopGenesSelectedClusters.UI <- renderUI({
    req(input$TopGenesClusterResolution)
    if(verbose){message("SeuratExplorer: preparing TopGenesSelectedClusters.UI...")}
    shinyWidgets::pickerInput(inputId = "TopGenesSelectedClusters",
                              label = "Subset cells:",
                              choices = levels(data$obj@meta.data[,input$TopGenesClusterResolution]),
                              selected = levels(data$obj@meta.data[,input$TopGenesClusterResolution]),
                              options = shinyWidgets::pickerOptions(actionsBox = TRUE,
                                                                    size = 10,
                                                                    selectedTextFormat = "count > 3"),
                              multiple = TRUE)
  })

  observeEvent(input$TopGenesAnalysis, {
    if(verbose){message("SeuratExplorer: preparing TopGenesAnalysis...")}
    showModal(modalDialog(title = "Calculating Top Genes at Cell Level...",
                          "Please wait for a few minutes!",
                          footer= NULL,
                          size = "l"))
    cds <- data$obj
    Idents(cds) <- input$TopGenesClusterResolution
    cds <- subset_Seurat(cds, idents = input$TopGenesSelectedClusters)


    if (input$TopGenesClusterLevel) {
      TopGenes$topgenes <- top_genes(SeuratObj = cds,
                                     percent.cut = input$TopGenesTopPercent/100,
                                     group.by = input$TopGenesClusterResolution,
                                     assay = input$TopGenesAssay)
    }else{
      TopGenes$topgenes <- top_genes(SeuratObj = cds,
                                     percent.cut = input$TopGenesTopPercent/100,
                                     group.by = NULL,
                                     assay = input$TopGenesAssay)
    }
    removeModal()
    if (nrow(TopGenes$topgenes) > 0) {
      TopGenes$topgenes_ready <- TRUE
    }else{
      showModal(modalDialog(title = "Error",
                            "No genes found, please check the parameters.",
                            footer= modalButton("Dismiss"),
                            easyClose = TRUE,
                            size = "l"))
    }
  })

  observeEvent(input$TopAccumulatedGenesAnalysis, {
    if(verbose){message("SeuratExplorer: preparing TopAccumulatedGenesAnalysis...")}
    showModal(modalDialog(title = "Calculating Accumulated Top Genes...",
                          "Please wait for a few minutes!",
                          footer= NULL,
                          size = "l"))
    cds <- data$obj
    Idents(cds) <- input$TopGenesClusterResolution
    cds <- subset_Seurat(cds, idents = input$TopGenesSelectedClusters)
    if (input$TopGenesClusterLevel) {
      TopGenes$topgenes <- top_accumulated_genes(SeuratObj = cds,
                                                 top_n = input$TopGenesTopN,
                                                 group.by = input$TopGenesClusterResolution,
                                                 assay = input$TopGenesAssay)
    }else{
      TopGenes$topgenes <- top_accumulated_genes(SeuratObj = cds,
                                                 top_n = input$TopGenesTopN,
                                                 group.by = NULL,
                                                 assay = input$TopGenesAssay)
    }
    removeModal()
    if (nrow(TopGenes$topgenes) > 0) {
      TopGenes$topgenes_ready <- TRUE
    }else{
      showModal(modalDialog(title = "Error", "No genes found, please check the parameters.",
                            footer= modalButton("Dismiss"),
                            easyClose = TRUE,
                            size = "l"))
    }
  })

  output$dataset_topgenes <-  DT::renderDT(server=FALSE,{
    req(TopGenes$topgenes)
    if(verbose){message("SeuratExplorer: preparing topgenes...")}
    # Show data
    DT::datatable(TopGenes$topgenes, extensions = 'Buttons',
                  options = list(scrollX=TRUE,
                                 paging = TRUE, searching = TRUE,
                                 fixedColumns = TRUE, autoWidth = TRUE,
                                 ordering = TRUE, dom = 'Bfrtip',
                                 buttons = list('copy',
                                                list(extend = 'csv', title = "top-features"),
                                                list(extend = 'excel', title = "top-features"))))
  })

  ################################ Feature Summary
  # info
  output$featuresummary_info = renderText({
    paste0('Summary interested features by cluster, such as the percentage of positive cells, and mean/median expression level.
           Attention: Unmatched features will be automatically ignored.')
  })

  FeatureSummary <- reactiveValues(summary = NULL, summary_ready = FALSE)

  output$FeatureSummary_ready <- reactive({
    return(FeatureSummary$summary_ready)
  })

  outputOptions(output, 'FeatureSummary_ready', suspendWhenHidden=FALSE)

  # define Cluster Annotation choice
  output$FeatureSummarySelectedClusters.UI <- renderUI({
    req(input$FeatureSummaryClusterResolution)
    if(verbose){message("SeuratExplorer: preparing FeatureSummarySelectedClusters.UI...")}
    shinyWidgets::pickerInput(inputId = "FeatureSummarySelectedClusters", label = "Subset cells:",
                              choices = levels(data$obj@meta.data[,input$FeatureSummaryClusterResolution]),
                              selected = levels(data$obj@meta.data[,input$FeatureSummaryClusterResolution]),
                              options = shinyWidgets::pickerOptions(actionsBox = TRUE,
                                                                    size = 10,
                                                                    selectedTextFormat = "count > 3"),
                              multiple = TRUE)
  })

  observeEvent(input$FeatureSummaryAnalysis, {
    if(verbose){message("SeuratExplorer: preparing FeatureSummaryAnalysis...")}
    if(is.na(input$FeatureSummarySymbol)){
      GeneRevised <- NA
    }else{
      GeneRevised <- CheckGene(InputGene = input$FeatureSummarySymbol,
                               GeneLibrary =  rownames(data$obj[[input$FeatureSummaryAssay]]))
    }
    if (any(is.na(GeneRevised))) {
      showModal(modalDialog(title = "Error",
                            check_genes_error,
                            footer= modalButton("Dismiss"),
                            easyClose = TRUE,
                            size = "l"))
    }else{
      showModal(modalDialog(title = "Summarizing features...",
                            "Please wait for a few minutes!",
                            footer= NULL,
                            size = "l"))
      cds <- data$obj
      Idents(cds) <- input$FeatureSummaryClusterResolution
      cds <- subset_Seurat(cds, idents = input$FeatureSummarySelectedClusters)
      if (input$FeatureSummaryClusterLevel) {
        FeatureSummary$summary <- summary_features(SeuratObj = cds,
                                                   features = GeneRevised,
                                                   group.by = input$FeatureSummaryClusterResolution,
                                                   assay = input$FeatureSummaryAssay)
      }else{
        FeatureSummary$summary <- summary_features(SeuratObj = cds,
                                                   features = GeneRevised,
                                                   group.by = NULL,
                                                   assay = input$FeatureSummaryAssay)
      }
      removeModal()
      FeatureSummary$summary_ready <- TRUE
    }
  })

  output$dataset_featuresummary <-  DT::renderDT(server=FALSE,{
    req(FeatureSummary$summary)
    if(verbose){message("SeuratExplorer: preparing dataset_featuresummary...")}
    # Show data
    DT::datatable(FeatureSummary$summary, extensions = 'Buttons',
                  options = list(scrollX=TRUE,
                                 # lengthMenu = c(5,10,15),
                                 paging = TRUE,
                                 searching = TRUE,
                                 fixedColumns = TRUE,
                                 autoWidth = TRUE,
                                 ordering = TRUE,
                                 dom = 'Bfrtip',
                                 buttons = list('copy',
                                                list(extend = 'csv', title = "feature-summary"),
                                                list(extend = 'excel', title = "feature-summary"))))
  })

  ################################ Feature Correlation
  # Warning
  output$featurecorrelation_info = renderText({
    paste0('This usually takes longer, please wait patiently. Make sure to save current results before a new analysis!
      - Find Top Correlated Gene Pairs: find top 1000 correlated gene pairs.
      - Find Correlated Genes for A Gene: find the most correlated genes for input genes.
      - Calculate Correlation for A Gene List: calculate the correlation value for each pair of the input genes.
    if nothing return, this is caused by the low expression of the input genes, very low expressed genes will be removed before analysis.')
  })

  FeatureCorrelation <- reactiveValues(summary = NULL, summary_ready = FALSE)

  output$FeatureCorrelation_ready <- reactive({
    return(FeatureCorrelation$summary_ready)
  })

  outputOptions(output, 'FeatureCorrelation_ready', suspendWhenHidden=FALSE)

  # define the idents used
  output$FeatureCorrelationIdentsSelected.UI <- renderUI({
    req(input$FeatureCorrelationClusterResolution)
    if(verbose){message("SeuratExplorer: preparing FeatureCorrelationIdentsSelected.UI...")}
    shinyWidgets::pickerInput(inputId = "FeatureCorrelationIdentsSelected", label = "Clusters Used:",
                              choices = levels(data$obj@meta.data[,input$FeatureCorrelationClusterResolution]),
                              selected = levels(data$obj@meta.data[,input$FeatureCorrelationClusterResolution]),
                              options = shinyWidgets::pickerOptions(actionsBox = TRUE,
                                                                    size = 10,
                                                                    selectedTextFormat = "count > 3"),
                              multiple = TRUE)
  })



  observeEvent(input$TopCorrelationAnalysis, {
    if(verbose){message("SeuratExplorer: preparing TopCorrelationAnalysis...")}
    showModal(modalDialog(title = "Calculating",
                          "Calculate top correlated gene pairs, which usually takes longer...",
                          footer= NULL,
                          size = "l"))
    cds <- data$obj
    Seurat::Idents(cds) <- input$FeatureCorrelationClusterResolution
    cds <- subset_Seurat(cds, idents = input$FeatureCorrelationIdentsSelected)
    FeatureCorrelation$summary <- calculate_top_correlations(SeuratObj = cds,
                                                             method = input$correlationmethod,
                                                             assay = input$FeatureCorrelationAssay)
    removeModal()
    if (nrow(FeatureCorrelation$summary) > 0) {
      FeatureCorrelation$summary_ready <- TRUE
    }else{
      showModal(modalDialog(title = "Error",
                            "No gene paris found, probably for some genes has very low expression value.",
                            footer= modalButton("Dismiss"),
                            easyClose = TRUE, size = "l"))
    }
  })


  observeEvent(input$MostCorrelatedAnalysis, {
    if(verbose){message("SeuratExplorer: preparing MostCorrelatedAnalysis...")}
    feature.revised <- ReviseGene(Agene = trimws(input$MostCorrelatedAGene),
                                  GeneLibrary = rownames(data$obj[[input$FeatureCorrelationAssay]]))
    if(is.na(feature.revised)){
      showModal(modalDialog(title = "Error",
                            "the input gene can not be found, please check...",
                            footer= modalButton("Dismiss"),
                            easyClose = TRUE,
                            size = "l"))
    }else{
      showModal(modalDialog(title = "Calculating",
                            "Calculate the most correlated genes for the input gene, which usually takes longer...",
                            footer= NULL,
                            size = "l"))
      cds <- data$obj
      Seurat::Idents(cds) <- input$FeatureCorrelationClusterResolution
      cds <- subset_Seurat(cds, idents = input$FeatureCorrelationIdentsSelected)
      FeatureCorrelation$summary <- calculate_most_correlated(SeuratObj = cds,
                                                              feature = feature.revised,
                                                              method = input$correlationmethod,
                                                              assay = input$FeatureCorrelationAssay)
      removeModal()
      if (nrow(FeatureCorrelation$summary) > 0) {
        FeatureCorrelation$summary_ready <- TRUE
      }else{
        showModal(modalDialog(title = "Error",
                              "No gene paris are found, probably for some genes has very low expression value.",
                              footer= modalButton("Dismiss"),
                              easyClose = TRUE,
                              size = "l"))
      }
    }
  })

  observeEvent(input$calculatecorrelation, {
    if(verbose){message("SeuratExplorer: preparing calculatecorrelation...")}
    if(is.na(input$CorrelationGeneList)){
      GeneRevised <- NA
    }else{
      GeneRevised <- CheckGene(InputGene = input$CorrelationGeneList,
                               GeneLibrary =  rownames(data$obj[[input$FeatureCorrelationAssay]]))
    }
    if (any(is.na(GeneRevised))) {
      showModal(modalDialog(title = "Error",
                            check_genes_error,
                            footer= modalButton("Dismiss"),
                            easyClose = TRUE, size = "l"))
    }else if(length(GeneRevised) < 2){
      showModal(modalDialog(title = "Error",
                            "Please input at least two genes!",
                            footer= modalButton("Dismiss"),
                            easyClose = TRUE, size = "l"))
    }else{
      showModal(modalDialog(title = "Calculating",
                            "Calculate the correlation for the specified gene list...",
                            footer= NULL, size = "l"))
      cds <- data$obj
      Seurat::Idents(cds) <- input$FeatureCorrelationClusterResolution
      cds <- subset_Seurat(cds, idents = input$FeatureCorrelationIdentsSelected)
      FeatureCorrelation$summary <- calculate_correlation(SeuratObj = cds,
                                                          features = GeneRevised,
                                                          method = input$correlationmethod,
                                                          assay = input$FeatureCorrelationAssay)
      removeModal()
      if (nrow(FeatureCorrelation$summary) > 0) {
        FeatureCorrelation$summary_ready <- TRUE
      }else{
        showModal(modalDialog(title = "Error",
                              "No gene paris found, probably for some genes has very low expression value.",
                              footer= modalButton("Dismiss"),
                              easyClose = TRUE,
                              size = "l"))
      }
    }
  })

  output$dataset_correlation <-  DT::renderDT(server=FALSE,{
    req(FeatureCorrelation$summary)
    if(verbose){message("SeuratExplorer: preparing dataset_featuresummary...")}
    # Show data
    DT::datatable(FeatureCorrelation$summary, extensions = 'Buttons',
                  options = list(scrollX=TRUE,
                                 paging = TRUE, searching = TRUE,
                                 fixedColumns = TRUE, autoWidth = TRUE,
                                 ordering = TRUE, dom = 'Bfrtip',
                                 buttons = list('copy',
                                                list(extend = 'csv', title = "feature-correlation"),
                                                list(extend = 'excel', title = "feature-correlation"))))
  })

  ############################## Rename Clusters
  cell_annotation_df <- reactiveVal(data.frame())

  observe({
    req(input$renameclustersClusterResolution)
    cell_annotation_df(data.frame(Old_Name = levels(data$obj@meta.data[,input$renameclustersClusterResolution]),
                                  New_Name = rep('-', length(levels(data$obj@meta.data[,input$renameclustersClusterResolution])))))
  })

  output$cell_annotation <- DT::renderDataTable({
    req(input$renameclustersClusterResolution)
    DT::datatable(cell_annotation_df(),
                  editable = list(target = 'cell', disable = list(columns = 0)), # Disables columns 1
                  selection = "single",
                  options = list(dom = 'lrtip', lengthChange = FALSE, pageLength = -1,
                                 language = list(info = "Double click '-' to start edit, only support letters, numbers, - and _.")),
                  rownames = FALSE
                  )
  })

  observeEvent(input$cell_annotation_cell_edit, {
    info <- input$cell_annotation_cell_edit
    new_df <- cell_annotation_df()
    new_df[info$row, info$col + 1] <- info$value
    cell_annotation_df(new_df)
  })


  output$renameclusterscheck_OK <- reactive(FALSE)

  outputOptions(output, 'renameclusterscheck_OK', suspendWhenHidden = FALSE)

  new_anno_mapping <- reactiveValues(NewClusterName = '',
                                     OldClusterName = '',
                                     mapping = data.frame())

  observeEvent(input$renameclustersCheck, {
    # check input format
    if ('-' %in% cell_annotation_df()$New_Name) {
      showModal(modalDialog(title = "Error",
                            "'-' found, please edit all levels!",
                            footer= modalButton("Dismiss"),
                            easyClose = TRUE,
                            size = "l"))
      output$renameclusterscheck_OK <- reactive(FALSE)
    }else if('' %in% trimws(cell_annotation_df()$New_Name)){
      showModal(modalDialog(title = "Error",
                            "New cluster can not be empty!",
                            footer= modalButton("Dismiss"),
                            easyClose = TRUE,
                            size = "l"))
      output$renameclusterscheck_OK <- reactive(FALSE)
    }else if (!all(sapply(cell_annotation_df()$New_Name, check_allowed_chars))) {
      showModal(modalDialog(title = "Error",
                            "Unsupported character found! only support letters, numbers, - and _.",
                            footer= modalButton("Dismiss"),
                            easyClose = TRUE,
                            size = "l"))
      output$renameclusterscheck_OK <- reactive(FALSE)
    } else  if (!check_allowed_chars(input$renameclustersNewClusterName)) {
      showModal(modalDialog(title = "Error",
                            "Unsupported character found! only support letters, numbers, - and _.",
                            footer= modalButton("Dismiss"),
                            easyClose = TRUE,
                            size = "l"))
      output$renameclusterscheck_OK <- reactive(FALSE)
    }else{
      # check cluster name duplicates
      if (input$renameclustersNewClusterName %in% colnames(data$obj@meta.data)) {
          showModal(modalDialog(title = "Error",
                                "Duplicated cluster name found, please change the cluster name!",
                                footer= modalButton("Dismiss"),
                                easyClose = TRUE,
                                size = "l"))
      }else{
        # show dimension plot
        cds <- data$obj
        Seurat::Idents(cds) <- input$renameclustersClusterResolution
        new_names_mapping <- cell_annotation_df()$New_Name
        names(new_names_mapping) <- cell_annotation_df()$Old_Name
        cds <- Seurat::RenameIdents(cds, new_names_mapping)
        output$renameclusterdimplot <- renderPlot(Seurat::DimPlot(cds,
                                                                  reduction = input$renameclustersDimensionReduction,
                                                                  label = TRUE))
        new_anno_mapping$NewClusterName <- input$renameclustersNewClusterName
        new_anno_mapping$OldClusterName = input$renameclustersClusterResolution
        new_anno_mapping$mapping = cell_annotation_df()
        # output$updated_df_output <- renderPrint({ # for debug
        #   str(reactiveValuesToList(new_anno_mapping))
        # })
        output$renameclusterscheck_OK <- reactive(TRUE)
      }
    }
  })

  output$renameclustersNewClusterNamehints.UI <- renderUI({
    if(verbose){message("SeuratExplorer: preparing renameclustersNewClusterNamehints.UI...")}
    helpText(strong(paste("Avoid using: ",
                          paste(colnames(data$obj@meta.data), collapse = " "), ". Only support letters, numbers, - and _.",
                          sep = "")),style = "font-size:12px;")
  })

  observeEvent(input$renameclustersSubmit, {
    new_anno_mapping_list <- reactiveValuesToList(new_anno_mapping)
    need_update_data <- FALSE

    if (new_anno_mapping_list$NewClusterName == ''){
      showModal(modalDialog(title = "Error:",
                            "Please run Check before a submit!",
                            footer= modalButton("Dismiss"),
                            easyClose = TRUE,
                            size = "l"))
    }else if(new_anno_mapping_list$NewClusterName %in% colnames(data$obj@meta.data)){
      showModal(modalDialog(title = "Error",
                            "Duplicated labels found, do not resubmit!",
                            footer= modalButton("Dismiss"),
                            easyClose = TRUE,
                            size = "l"))
    }else{
      cds <- data$obj
      Seurat::Idents(cds) <- new_anno_mapping_list$OldClusterName
      new_names_mapping <- new_anno_mapping_list$mapping$New_Name
      names(new_names_mapping) <- new_anno_mapping_list$mapping$Old_Name
      cds <- Seurat::RenameIdents(cds, new_names_mapping)
      cds@meta.data[, new_anno_mapping_list$NewClusterName] <- Idents(cds)
      data$obj <- cds
      data$cluster_options <- prepare_cluster_options(df = data$obj@meta.data,
                                                      verbose = getOption('SeuratExplorerVerbose'))
      data$split_options <- prepare_split_options(df = data$obj@meta.data,
                                                  max.level = data$split_maxlevel,
                                                  verbose = getOption('SeuratExplorerVerbose'))
      data$version <- data$version + 1
      showModal(modalDialog(title = "Congratulations:",
                            "New annotation added!",
                            footer= modalButton("Dismiss"),
                            easyClose = TRUE,
                            size = "l"))
    }
  })

  output$renameclustersDownload <- downloadHandler(
    filename = function() {
      "new_annotation_mapping.csv"
    },
    content = function(file) {
      new_anno_mapping_list <- reactiveValuesToList(new_anno_mapping)
      df <- new_anno_mapping_list$mapping
      colnames(df) <- c(new_anno_mapping_list$OldClusterName, new_anno_mapping_list$NewClusterName)
      write.csv(df, file, row.names = FALSE)
    }
  )

  ############################## Search features
  # output the features dataset
  output$dataset_features <- DT::renderDT(server=TRUE,{
    req(input$FeaturesDataframeAssay)
    # Show data
    DT::datatable(data$gene_annotions_list[[input$FeaturesDataframeAssay]],
                  extensions = 'Buttons',
                  options = list(scrollX=TRUE,
                                 paging = TRUE, searching = TRUE,
                                 fixedColumns = TRUE, autoWidth = TRUE,
                                 ordering = TRUE, dom = 'Bfrtip',
                                 buttons = list('copy',
                                                list(extend = 'csv',
                                                     title = paste0("features-from-", input$FeaturesDataframeAssay)),
                                                list(extend = 'excel',
                                                     title = paste0("features-from-", input$FeaturesDataframeAssay)))))
  })

  ############################### Render metadata table
  # Server set to TRUE: https://stackoverflow.com/questions/50039186/add-download-buttons-in-dtrenderdatatable
  # when sever is set to TRUE, to download the whole data in DT button extensions.https://github.com/rstudio/DT/issues/267
  output$dataset_meta <- DT::renderDT(server=TRUE,{
    req(data$obj)
    # Show data
    DT::datatable(data$obj@meta.data,
                  callback = DT::JS("$('div.dwnld').append($('#download_meta_data'));"),
                  extensions = 'Buttons',
                  options = list(scrollX=TRUE,
                                 # lengthMenu = c(5,10,15),
                                 #paging = TRUE,
                                 #searching = TRUE,
                                 #fixedColumns = TRUE,
                                 #autoWidth = TRUE,
                                 ordering = TRUE,
                                 dom = 'B<"dwnld">frtip',
                                 buttons = list('copy')
                  ))
  })

  output$download_meta_data <- downloadHandler(
    filename = function() {
      "cell-metadata.csv"
    },
    content = function(file) {
      write.csv(data$obj@meta.data, file)
    }
  )

  ############################### Render Object structure
  output$object_structure <- renderPrint({
    req(data$obj)
    str(data$obj, max.level = input$ObjectStrutureLevel) # Display the structure of the data frame
  })

}


#' Server
#' @import shiny shinydashboard shinyWidgets
#' @import ggplot2 Seurat SeuratObject
#' @importFrom utils write.csv
#'
#' @param input Input from the UI
#' @param output Output to send back to UI
#' @param session from shiny server function
#'
#' @export
#' @return the server functions of shiny app
#'
server <- function(input, output, session) {
  ## Dataset tab ----
  # reactiveValues: Create an object for storing reactive values,similar to a list,
  # but with special capabilities for reactive programming.
  data = reactiveValues(obj = NULL,
                        loaded = FALSE,
                        Name = NULL,
                        Path = NULL,
                        species = NULL,
                        reduction_options = NULL,
                        reduction_default = NULL,
                        assay_default = 'RNA',
                        cluster_options = NULL,
                        cluster_default = NULL,
                        assay_slots = c('counts', 'data', 'scale.data'),
                        split_maxlevel = getOption("SeuratExplorerSplitOptionMaxLevel"),
                        split_options = NULL,
                        extra_qc_options = NULL,
                        version = 0)

  # reductions_options: xy axis coordinate
  # cluster_options/split_options/extra_qc_options all are column name from seurat object meta.data,
  # which will be used for later plot
  # load data after data selection
  observeEvent(input$dataset_file, {
    ext = tools::file_ext(input$dataset_file$datapath) # file_ext: returns the file (name) extensions
    # validate + need: check file name post-fix, in not rds or qs2, will throw an error
    validate(need(expr = ext %in% c("rds","qs2","Rds"),
                  message = "Please upload a .rds or a .qs2 file"))
    data$Path <- input$dataset_file$datapath

    data$obj <- prepare_seurat_object(obj = readSeurat(path = input$dataset_file$datapath, verbose = getOption('SeuratExplorerVerbose')),
                                      verbose = getOption('SeuratExplorerVerbose'))

    data$reduction_options <- prepare_reduction_options(obj = data$obj,
                                                        keywords = getOption("SeuratExplorerReductionKeyWords"),
                                                        verbose = getOption('SeuratExplorerVerbose'))

    data$assays_slots_options <- prepare_assays_slots(obj = data$obj,
                                                      data_slot = data$assay_slots,
                                                      verbose = getOption('SeuratExplorerVerbose'))

    data$assays_options <- prepare_assays_options(Alist = data$assays_slots_options,
                                                  verbose = getOption('SeuratExplorerVerbose'))

    data$assay_default <- ifelse(data$assay_default %in% data$assays_options,data$assay_default,
                                 data$assays_options[1]) # update the default assay

    data$cluster_options <- prepare_cluster_options(df = data$obj@meta.data,
                                                    verbose = getOption('SeuratExplorerVerbose'))

    data$gene_annotions_list <- prepare_gene_annotations(obj = data$obj,
                                                         verbose = getOption('SeuratExplorerVerbose'))

    data$split_options <- prepare_split_options(df = data$obj@meta.data,
                                                max.level = data$split_maxlevel,
                                                verbose = getOption('SeuratExplorerVerbose'))

    data$extra_qc_options <- prepare_qc_options(df = data$obj@meta.data,
                                                types = c("double","integer","numeric"),
                                                verbose = getOption('SeuratExplorerVerbose'))
  })

  # after data loaded,set loaded to TRUE
  observe({
    req(data$obj)
    data$loaded = !is.null(data$obj)
  })

  # Conditional panel control based on loaded obj, after loaded, show other UIs
  output$file_loaded = reactive({
    return(data$loaded)
  })

  outputOptions(output, 'file_loaded', suspendWhenHidden=FALSE)

  # Seurat Explorer functions
  explorer_server(input = input,
                  output = output,
                  session = session,
                  data = data,
                  verbose = getOption('SeuratExplorerVerbose'))

}
