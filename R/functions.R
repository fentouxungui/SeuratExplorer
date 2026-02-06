prepare_seurat_object <- function(obj, verbose = FALSE){
  requireNamespace("Seurat")
  # trans the none-factor columns in meta.data to factor
  # if the unique counts less than 1/20 of total cells, and not more than 50, in chr or num type columns，will be forced to factor type.
  # possible problem: unique_max_percent = 0.05 may not suitable for a data has 100 cells but more than 5 clusters.
  obj@meta.data <- modify_columns_types(df = obj@meta.data, types_to_check = c("numeric", "character"), unique_max_counts = 50, unique_max_percent = 0.05, verbose = verbose)
  # for splited object, join layers
  if (sum(grepl("^counts",Layers(object = obj))) > 1 | sum(grepl("^data",Layers(object = obj))) > 1) {
    obj <- SeuratObject::JoinLayers(object = obj)
  }
  if (verbose) {message("SeuratExplorer: prepare_seurat_object runs successfully!")}
  return(obj)
}

#' Helper function to join layers for Assay5 objects
#' @param SeuratObj A Seurat object
#' @param assay Assay name
#' @return Seurat object with joined layers
join_layers_if_needed <- function(SeuratObj, assay = 'RNA') {
  if (class(SeuratObj[[assay]])[1] == "Assay5") {
    SeuratObj <- JoinLayers(SeuratObj)
  }
  return(SeuratObj)
}

#' Helper function to extract counts matrix with proper handling
#' @param SeuratObj A Seurat object
#' @param assay Assay name
#' @return A counts matrix
get_counts_matrix <- function(SeuratObj, assay = 'RNA') {
  SeuratObj <- join_layers_if_needed(SeuratObj, assay)
  if (class(SeuratObj[[assay]])[1] == "Assay5") {
    counts <- as.matrix(SeuratObj[[assay]]@layers$counts)
  } else {
    counts <- as.matrix(SeuratObj[[assay]]$counts)
  }
  rownames(counts) <- rownames(SeuratObj[[assay]])
  colnames(counts) <- colnames(SeuratObj)
  return(counts)
}

#' Helper function to extract data matrix with proper handling
#' @param SeuratObj A Seurat object
#' @param assay Assay name
#' @return A data matrix
get_data_matrix <- function(SeuratObj, assay = 'RNA') {
  SeuratObj <- join_layers_if_needed(SeuratObj, assay)
  if (class(SeuratObj[[assay]])[1] == "Assay5") {
    data <- as.matrix(SeuratObj[[assay]]@layers$data)
  } else {
    data <- as.matrix(SeuratObj[[assay]]$data)
  }
  rownames(data) <- rownames(SeuratObj[[assay]])
  colnames(data) <- colnames(SeuratObj)
  return(data)
}


# Converts eligible non-factor columns to factor type, and converts strings that may be numbers to numbers.
modify_columns_types <- function(df, types_to_check = c("numeric", "character"), unique_max_counts = 50, unique_max_percent = 0.05, verbose = FALSE){
  # first, extract all columns in types_to_check types
  candidates.types.logic <- sapply(df, class) %in% types_to_check
  # then, for factor columns, check the levels, level counts should less than (total cells) * 0.1, if not trans to character
  factor_columns_names <- colnames(df)[sapply(df, class) %in% 'factor']
  factor_columns_names_not_ok <- factor_columns_names[sapply(df[,factor_columns_names], function(x)length(levels(x))) > nrow(df) * 0.1]
  if (length(factor_columns_names_not_ok) != 0) {
    df[,factor_columns_names_not_ok] <- lapply(df[,factor_columns_names_not_ok], as.character)
  }
  # then for types_to_check types
  # unique values counts should less than (total cells)*0.05, for 100 cells has 5 clusters at most. and for 10000 cells has 50 clusters at most.
  cutoff <- min(unique_max_counts, round(nrow(df) * unique_max_percent))
  candidates.unique.logic <- sapply(df, FUN = function(x)length(unique(x))) <= cutoff
  candidates.logic <- candidates.types.logic & candidates.unique.logic & !sapply(df, is.factor)
  # before trans to factor, trans numeric character vector to numeric vector: c('1','2','1') to c(1,2,1)
  char2numeric_columns.logic <- suppressWarnings(unlist(lapply(df[candidates.logic], function(x)any(!is.na(as.numeric(unique(x)))))))
  char2numeric_columns <- names(char2numeric_columns.logic)[char2numeric_columns.logic]
  df[char2numeric_columns] <- lapply(df[char2numeric_columns], as.numeric)
  # finally trans all char and numeric to factor
  df[candidates.logic] <- lapply(df[candidates.logic], as.factor)
  if(verbose){message("SeuratExplorer: modify_columns_types runs successfully!")}
  return(df)
}

# get reduction options by keywords: umap and tsne
prepare_reduction_options <- function(obj, keywords = c("umap","tsne"), verbose = FALSE){
  requireNamespace("Seurat")
  reduction.choice <- grep(paste0(paste0("(", keywords,")"),collapse = "|"), Seurat::Reductions(obj), value = TRUE, ignore.case = TRUE)
  names(reduction.choice) <- toupper(reduction.choice)
  if(verbose){message("SeuratExplorer: prepare_reduction_options runs successfully!")}
  return(reduction.choice)
}

# get assay and slots info
prepare_assays_slots <- function(obj, verbose = FALSE, data_slot =  c('counts', 'data', 'scale.data')){
  requireNamespace("Seurat")
  # assay at least has one slot of counts, data, scale.data
  assay_slot_list <- list()
  for (i in  Seurat::Assays(obj)) {
    slot_names <- slotNames(obj[[i]])
    if ('layers' %in% slot_names) {
      slot_names <- Layers(obj[[i]])
    }
    slot_names <- data_slot[data_slot %in% slot_names]
    if (length(slot_names) != 0) {
      assay_slot_list[[i]] <- slot_names
    }
  }
  if(verbose){message("SeuratExplorer: prepare_assays_slots runs successfully!")}
  return(assay_slot_list)
}

prepare_assays_options <-function(Alist, verbose = FALSE){
  assays.choice <- names(Alist)
  names(assays.choice) <- toupper(assays.choice)
  if(verbose){message("SeuratExplorer: prepare_assays_options runs successfully!")}
  return(assays.choice)
}

# get cluster resolution options from all factor type columns in meta.data
prepare_cluster_options <- function(df, verbose = FALSE){
  cluster.options <- colnames(df)[sapply(df, is.factor)]
  names(cluster.options) <- cluster.options
  if(verbose){message("SeuratExplorer: prepare_cluster_options runs successfully!")}
  return(cluster.options)
}

# get all genes or annotation from assays
#' extract the features from row names or annotation slot of assays
#'
#' @param obj a Seurat object
#' @param verbose whether output messages
#'
#' @importFrom methods slotNames is
#' @return a list with data.frames
#'
#' @examples
#' #NULL
prepare_gene_annotations <- function(obj, verbose = FALSE){
  anno_list <- list()
  for (aassay in Assays(obj)) {
    # the annotation in ATAC assay is not the real features of the assay! just annotations from genome.
    # use ClosestFeature to annotate peaks/features from ATAC assay
    if ('annotation' %in% slotNames(obj[[aassay]])) { # if exist annotation slot in assay
      # Check if Signac is available
      if (requireNamespace("Signac", quietly = TRUE)) {
        anno_df <- Signac::ClosestFeature(obj[[aassay]], regions = rownames(obj[[aassay]]))
        anno_list[[aassay]] <- anno_df
      } else {
        # If Signac is not available, fall back to using rownames
        if (verbose) message("Signac package not found. Using rownames instead for assay: ", aassay)
        anno_df <- data.frame(FeatureName = rownames(obj[[aassay]]))
        anno_list[[aassay]] <- anno_df
      }
    }else{ # if not exist annotation slot, use all rownames
      anno_df <- data.frame(FeatureName = rownames(obj[[aassay]]))
      anno_list[[aassay]] <- anno_df
    }
  }
  if(verbose){message("SeuratExplorer: prepare_gene_annotations runs successfully!")}
  return(anno_list)
}


# get split options from eligible factor columns in meta.data which has less levels than max_level
prepare_split_options <- function(df, max.level = 4, verbose = FALSE){
  cluster.options <- colnames(df)[sapply(df, is.factor)]
  leve.counts <- unname(sapply(df[cluster.options],FUN = function(x)length(levels(x))))
  split.options <- cluster.options[leve.counts <= max.level]
  names(split.options) <- split.options
  if(verbose){message("SeuratExplorer: prepare_split_options runs successfully!")}
  return(split.options)
}

# add extra columns from meta.data to qc options
prepare_qc_options <- function(df, types = c("double","integer","numeric"), verbose = FALSE){
  qc_options <- colnames(df)[sapply(df, class) %in% types]
  if(verbose){message("SeuratExplorer: prepare_qc_options runs successfully!")}
  return(qc_options)
}


# Check the input gene, return the revised gene, which can be used for FeaturePlot, Vlnplot ect.
CheckGene <- function(InputGene, GeneLibrary, verbose = FALSE){
  InputGenes <- trimws(unlist(strsplit(InputGene,split = "\n")))
  InputGenes <- InputGenes[InputGenes != ""]
  revised.genes <- sapply(InputGenes, FUN = function(x)ReviseGene(x, GeneLibrary = GeneLibrary))
  revised.genes <- unique(unname(revised.genes[!is.na(revised.genes)]))
  if(verbose){message("SeuratExplorer: CheckGene runs successfully!")}
  ifelse(length(revised.genes) == 0, yes = return(NA), no = return(revised.genes))
}

ReviseGene <- function(Agene, GeneLibrary){
  if (Agene %in% GeneLibrary) { # when input gene is absolutely right
    return(Agene)
  }else if(tolower(Agene) %in% tolower(GeneLibrary)){ # when not case sensitive
    return(GeneLibrary[tolower(GeneLibrary) %in% tolower(Agene)][1]) # gene list length can > 1
  }else{ # when not match
    return(NA)
  }
}


color_list <- list(stallion = c("#D51F26","#272E6A","#208A42","#89288F","#F47D2B",
                                "#FEE500","#8A9FD1","#C06CAB","#E6C2DC","#90D5E4",
                                "#89C75F","#F37B7D","#9983BD","#D24B27","#3BBCA8",
                                "#6E4B9E","#0C727C", "#7E1416","#D8A767","#3D3D3D"),

                   stallion2 = c("#D51F26","#272E6A","#208A42","#89288F","#F47D2B",
                                 "#FEE500","#8A9FD1","#C06CAB","#E6C2DC","#90D5E4",
                                 "#89C75F","#F37B7D","#9983BD","#D24B27","#3BBCA8",
                                 "#6E4B9E","#0C727C", "#7E1416","#D8A767"),

                   calm = c("#7DD06F", "#844081","#688EC1", "#C17E73", "#484125",
                            "#6CD3A7", "#597873","#7B6FD0", "#CF4A31", "#D0CD47",
                            "#722A2D", "#CBC594", "#D19EC4", "#5A7E36", "#D4477D",
                            "#403552", "#76D73C", "#96CED5", "#CE54D1", "#C48736"),

                   kelly = c("#FFB300", "#803E75", "#FF6800", "#A6BDD7", "#C10020",
                             "#CEA262", "#817066", "#007D34", "#F6768E", "#00538A",
                             "#FF7A5C", "#53377A", "#FF8E00", "#B32851", "#F4C800",
                             "#7F180D", "#93AA00", "#593315", "#F13A13", "#232C16"),

                   bear = c("#faa818", "#41a30d","#fbdf72", "#367d7d", "#d33502",
                            "#6ebcbc", "#37526d","#916848", "#f5b390", "#342739",
                            "#bed678","#a6d9ee", "#0d74b6",
                            "#60824f","#725ca5", "#e0598b"),

                   #15-colors
                   ironMan = c('#371377','#7700FF','#9E0142','#FF0080', '#DC494C',
                               "#F88D51","#FAD510","#FFFF5F",'#88CFA4','#238B45',
                               "#02401B", "#0AD7D3","#046C9A", "#A2A475", 'grey35'),

                   circus = c("#D52126","#88CCEE", "#FEE52C", "#117733", "#CC61B0",
                              "#99C945", "#2F8AC4", "#332288","#E68316", "#661101",
                              "#F97B72", "#DDCC77", "#11A579", "#89288F", "#E73F74"),

                   #12-colors
                   paired = c("#A6CDE2","#1E78B4","#74C476","#34A047","#F59899","#E11E26",
                              "#FCBF6E","#F47E1F","#CAB2D6","#6A3E98","#FAF39B","#B15928"),

                   #11-colors
                   grove = c("#1a1334","#01545a","#017351","#03c383","#aad962",
                             "#fbbf45","#ef6a32","#ed0345","#a12a5e","#710162","#3B9AB2"),

                   #7-colors
                   summerNight = c("#2a7185","#a64027","#fbdf72","#60824f","#9cdff0",
                                   "#022336","#725ca5"),

                   #5-colors
                   zissou = c("#3B9AB2", "#78B7C5", "#EBCC2A", "#E1AF00", "#F21A00"),
                   darjeeling = c("#FF0000", "#00A08A", "#F2AD00", "#F98400", "#5BBCD6"),
                   rushmore = c("#E1BD6D", "#EABE94", "#0B775E", "#35274A" , "#F2300F"),
                   captain = c("grey","#A1CDE1","#12477C","#EC9274","#67001E"),

                   #---------------------------------------------------------------
                   # Primarily Continuous Palettes
                   #---------------------------------------------------------------
                   #10-colors
                   horizon = c('#000075','#2E00FF', '#9408F7', '#C729D6', '#FA4AB5',
                               '#FF6A95', '#FF8B74', '#FFAC53', '#FFCD32', '#FFFF60'),

                   #9-colors
                   horizonExtra =c("#000436","#021EA9","#1632FB","#6E34FC","#C732D5",
                                   "#FD619D","#FF9965","#FFD32B","#FFFC5A"),

                   blueYellow = c("#352A86","#343DAE","#0262E0","#1389D2","#2DB7A3",
                                  "#A5BE6A","#F8BA43","#F6DA23","#F8FA0D"),

                   sambaNight = c('#1873CC','#1798E5','#00BFFF','#4AC596','#00CC00',
                                  '#A2E700','#FFFF00','#FFD200','#FFA500'),

                   solarExtra = c('#3361A5', '#248AF3', '#14B3FF', '#88CEEF', '#C1D5DC',
                                  '#EAD397', '#FDB31A', '#E42A2A', '#A31D1D'),

                   whitePurple = c('#f7fcfd','#e0ecf4','#bfd3e6','#9ebcda','#8c96c6',
                                   '#8c6bb1','#88419d','#810f7c','#4d004b'),

                   whiteBlue = c('#fff7fb','#ece7f2','#d0d1e6','#a6bddb','#74a9cf',
                                 '#3690c0','#0570b0','#045a8d','#023858'),


                   comet = c("#E6E7E8","#3A97FF","#8816A7","black"),

                   #7-colors
                   greenBlue = c('#e0f3db','#ccebc5','#a8ddb5','#4eb3d3','#2b8cbe',
                                 '#0868ac','#084081'),

                   #6-colors
                   beach = c("#87D2DB","#5BB1CB","#4F66AF","#F15F30",
                             "#F7962E","#FCEE2B"),

                   #5-colors
                   coolwarm = c("#4858A7", "#788FC8", "#D6DAE1", "#F49B7C", "#B51F29"),
                   fireworks = c("white","#2488F0","#7F3F98","#E22929","#FCB31A"),
                   greyMagma = c("grey", "#FB8861FF", "#B63679FF", "#51127CFF", "#000004FF"),
                   fireworks2 = c("black", "#2488F0","#7F3F98","#E22929","#FCB31A"),
                   purpleOrange = c("#581845", "#900C3F", "#C70039", "#FF5744", "#FFC30F"))



color_choice_vector <- names(color_list)
names(color_choice_vector) <- paste(names(color_list), unlist(lapply(color_list,length)),sep = "-")
color_choice_vector <- color_choice_vector[names(color_choice_vector)[order(unlist(lapply(color_list,length)),decreasing = TRUE)]]
color_choice_vector <- c(c("Default" = 'default'), color_choice_vector)

#' @title getColors
#'
#' @param color.platte predefined color list
#' @param choice color name
#' @param n how many colors to return
#'
#' @return a color list
#' @export
#'
#' @examples
#' # null
getColors <- function(color.platte = NULL,
                      choice = 'default',
                      n = NULL){
  if (choice == 'default') { # use default colors
    return(scales::hue_pal()(n))
  }else{
    return(color.platte[[choice]][1:n])
  }
}

globalVariables(c("num"))

#' @title plot cell percentage barplot
#' @description
#' support facet, codes refer to: https://github.com/junjunlab/scRNAtoolVis/blob/master/R/cellRatioPlot.R, with modification
#'
#' @param object an Seurat object
#' @param idents idents used, default all idents
#' @param sample.name x axis
#' @param sample.order order for x axis
#' @param celltype.name column fill by
#' @param celltype.order order for fill by
#' @param facet.name column name for facet
#' @param facet.order the order for facet
#' @param col.width column width, from 0-1
#' @param flow.alpha transparency for flow
#' @param flow.curve curve for flow
#' @param color.choice color choice for fill
#' @import dplyr
#' @importFrom dplyr %>%
#' @return a ggplot2 object
#' @export
#'
#' @examples
#' #NULL
cellRatioPlot <- function(object = NULL,
                          idents = NULL,
                          sample.name = NULL,
                          sample.order = NULL,
                          celltype.name = NULL,
                          celltype.order = NULL,
                          facet.name = NULL,
                          facet.order = NULL,
                          col.width = 0.7,
                          flow.alpha = 0.25,
                          flow.curve = 0,
                          color.choice = NULL) {
  requireNamespace("dplyr")
  # Input validation
  if (is.null(object)) stop("object cannot be NULL")
  if (is.null(idents)) stop("idents cannot be NULL")
  if (is.null(sample.name)) stop("sample.name cannot be NULL")
  if (is.null(celltype.name)) stop("celltype.name cannot be NULL")
  if (!sample.name %in% colnames(object@meta.data)) {
    stop(paste("sample.name", sample.name, "not found in meta.data"))
  }
  if (!celltype.name %in% colnames(object@meta.data)) {
    stop(paste("celltype.name", celltype.name, "not found in meta.data"))
  }

  # get meta info
  meta <- object@meta.data

  # subset
  meta <- meta[meta[,celltype.name] %in% idents,]

  # Check if we have data left after subsetting
  if (nrow(meta) == 0) {
    stop("No cells match the provided idents")
  }

  # fill order | y order
  if(!is.null(celltype.order)){
    meta[,celltype.name] <- factor(meta[,celltype.name], levels = celltype.order)
  }

  # x order
  if(!is.null(sample.order)){
    meta[,sample.name] <- factor(meta[,sample.name], levels = sample.order)
  }

   # facet order
  if(!is.null(facet.order)){
    meta[,facet.name] <- factor(meta[,facet.name], levels = facet.order)
  }

  # calculate percent ratio
  if (is.null(facet.name)) {
    ratio.info <- meta %>%
      dplyr::group_by(.data[[sample.name]], .data[[celltype.name]]) %>%
      dplyr::summarise(num = dplyr::n()) %>%
      dplyr::mutate(rel_num = num / sum(num))
  }else{
    ratio.info <- meta %>%
      dplyr::group_by(.data[[sample.name]], .data[[facet.name]],.data[[celltype.name]]) %>%
      dplyr::summarise(num = dplyr::n()) %>%
      dplyr::mutate(rel_num = num / sum(num))
  }

  # color
  # fill.col <- getColors(color.platte = color_list, choice = color.choice, n = length(unique(meta[, celltype.name])))
  fill.col <- getColors(color.platte = color_list, choice = color.choice, n = length(idents))


  # plot
  p <- ggplot2::ggplot(ratio.info, ggplot2::aes_string(x = sample.name, y = "rel_num")) +
    ggplot2::geom_col( ggplot2::aes_string(fill = celltype.name), width = col.width ) +
    ggalluvial::geom_flow( ggplot2::aes_string( stratum = celltype.name, alluvium = celltype.name, fill = celltype.name ),
      width = col.width,
      alpha = flow.alpha,
      knot.pos = flow.curve) +
    ggplot2::theme_bw() +
    ggplot2::coord_cartesian(expand = 0) +
    ggplot2::scale_y_continuous(labels = scales::label_percent()) +
    ggplot2::scale_fill_manual(values = fill.col,name = "Cell Type") +
    ggplot2::theme(panel.grid = ggplot2::element_blank(),
                   axis.text = ggplot2::element_text(size = ggplot2::rel(1.2), color = "black"),
                   axis.title = ggplot2::element_text(size = ggplot2::rel(1.5), color = "black"),
                   legend.text = ggplot2::element_text(size = ggplot2::rel(1.2), color = "black"),
                   legend.title = ggplot2::element_text(size = ggplot2::rel(1.5), color = "black")) +
    ggplot2::xlab("") +
    ggplot2::ylab("Cell percent ratio")
  if (!is.null(facet.name)) {
    p <- p +
      ggplot2::facet_grid(stats::as.formula(paste0(facet.name, "~ ."))) +
      ggplot2::theme(panel.spacing = grid::unit(0.8, "cm", data = NULL))
  }
  return(p)
}




#' @title Find Top Genes by Cell
#' @description
#'  for each cell, find genes that has high UMI percentage, for example, if a cell has 10000 UMIs, and the UMI percentage cutoff is set to 0.01,
#'  then all genes that has more than 10000 * 0.01 = 100 UMIs is thought to be the highly expressed genes for this cell.summary those genes for each cluster,
#'  firstly get all highly expressed genes in a cluster, some genes may has less cells, then for each gene, count cells in which this genes is highly expressed,
#'  and also calculate the mean and median UMI percentage in those highly expressed cells.
#'
#'
#' @param SeuratObj Seurat object
#' @param percent.cut UMI percentage cutoff, in a cell, if a gene with UMIs ratio more than this cutoff, this gene will be assigned to highly expressed gene for this cell
#' @param group.by how to group cells
#' @param assay which assay used
#' @import dplyr Seurat SeuratObject
#'
#' @return a data frame
#' @export
#'
top_genes <- function(SeuratObj, percent.cut = 0.01, group.by, assay = 'RNA') {
  #> https://stackoverflow.com/questions/76242926/using-data-table-in-package-development-undefined-global-functions-or-variables
  # to block R RMD check note: Undefined global functions or variables:
  Gene <- NULL
  Expr <- NULL
  DefaultAssay(SeuratObj) <- assay

  # Use helper function to get counts matrix
  counts.expr <- get_counts_matrix(SeuratObj, assay)

  if (!is.null(group.by)) {
    # calculate by cell type
    all.cell.types <- unique(SeuratObj@meta.data[,group.by])
    results.statics <- list()
    for (celltype in all.cell.types) {
      cells.sub <- colnames(SeuratObj)[as.character(SeuratObj@meta.data[,group.by]) == celltype]
      if (length(cells.sub) < 3) {
        next
      }
      results.statics[[celltype]] <- top_genes_core(expr_mat = counts.expr[,cells.sub], cutoff = percent.cut, celltype = celltype)
    }
    results.statics <- Reduce(rbind, results.statics)
  }else{
    results.statics <- top_genes_core(expr_mat = counts.expr, cutoff = percent.cut, celltype = 'AllSelectedCells')
  }
  rownames(results.statics) <- NULL
  return(results.statics)
}

top_genes_core <- function(expr_mat, cutoff = 0.01, celltype){
  # to block R RMD check note: Undefined global functions or variables:
  Expr <- NULL
  Gene <- NULL

  # Optimized version using matrix operations instead of loops
  # Calculate total UMI per cell
  cell_totals <- colSums(expr_mat)

  # Find genes above cutoff for each cell using vectorized operations
  gene_list <- list()
  for (i in 1:ncol(expr_mat)) {
    if (cell_totals[i] > 0) {
      rates <- expr_mat[, i] / cell_totals[i]
      top_genes_idx <- which(rates > cutoff)
      if (length(top_genes_idx) > 0) {
        gene_list[[i]] <- data.frame(
          'Gene' = rownames(expr_mat)[top_genes_idx],
          'Expr' = rates[top_genes_idx],
          stringsAsFactors = FALSE
        )
      }
    }
  }

  if (length(gene_list) == 0) {
    # Return empty data frame with correct structure
    return(data.frame(
      celltype = celltype,
      total.cells = ncol(expr_mat),
      Gene = character(0),
      total.pos.cells = integer(0),
      total.UMI.pct = numeric(0),
      cut.Cells = integer(0),
      cut.pct.mean = numeric(0),
      cut.pct.median = numeric(0)
    ))
  }

  res_cell_level <- do.call(rbind, gene_list)
  genes.statics <- dplyr::group_by(res_cell_level, Gene) %>%
    dplyr::summarise(cut.pct.mean = round(mean(Expr),digits = 4),
                     cut.pct.median = round(stats::median(Expr),digits = 4),
                     cut.Cells = length(Expr))
  genes.statics$total.pos.cells <- apply(expr_mat[genes.statics$Gene,,drop = FALSE] > 0, 1, sum)
  genes.statics$total.UMI.pct <- round(apply(expr_mat[genes.statics$Gene,,drop = FALSE], 1, sum)/sum(expr_mat),digits = 4)
  genes.statics$total.cells <- ncol(expr_mat)
  genes.statics$celltype <- celltype
  genes.statics <- genes.statics[,c("celltype", "total.cells", "Gene", "total.pos.cells", "total.UMI.pct", "cut.Cells", "cut.pct.mean", "cut.pct.median")]
  genes.statics <- genes.statics[order(genes.statics$total.UMI.pct, decreasing = TRUE),]
  return(genes.statics)
}


top_accumulated_genes <- function(SeuratObj, top_n = 100, group.by, assay = 'RNA'){
  requireNamespace("dplyr")
  requireNamespace("Seurat")

  # Use helper function to get counts matrix
  counts.expr <- get_counts_matrix(SeuratObj, assay)

  if (!is.null(group.by)) {
    all.cell.types <- unique(SeuratObj@meta.data[,group.by])
    all.cell.types <- all.cell.types[!is.na(all.cell.types)] # in case of some celltype has NA value
    results.statics <- list()
    for (celltype in all.cell.types) {
      cells.sub <- Cells(SeuratObj)[(as.character(SeuratObj@meta.data[, group.by]) == as.character(celltype)) & !is.na(SeuratObj@meta.data[, group.by])] # some celltype has NA value
      if (length(cells.sub) < 3) {
        next
      }
      results.statics[[celltype]] <- top_accumulated_genes_core(expr_mat = counts.expr[,cells.sub,drop = FALSE], top_n = top_n, celltype = celltype)
    }
    results.statics <- Reduce(rbind, results.statics)
  }else{
    results.statics <- top_accumulated_genes_core(expr_mat = counts.expr, top_n = top_n, celltype = 'AllSetectedCells')
  }
  rownames(results.statics) <- NULL
  return(results.statics)
}

top_accumulated_genes_core <- function(expr_mat, top_n, celltype){
  expr_sum <- sort(apply(expr_mat, 1, sum),decreasing = TRUE)
  if (length(expr_sum) > top_n) {
    expr_sum <- expr_sum[1:top_n]
  }
  res <- data.frame(Gene = names(expr_sum), MeanUMICounts = round(unname(expr_sum)/ncol(expr_mat),digits = 4), PCT = round(unname(expr_sum)/sum(expr_mat),digits = 4))
  res$total.pos.cells <- unname(apply(expr_mat[res$Gene,,drop = FALSE] > 0, 1, sum))
  res$total.cells <- ncol(expr_mat)
  res$celltype <- celltype
  res <- res[,c("celltype", "total.cells", "Gene", "total.pos.cells", "MeanUMICounts", "PCT")]
  res <- res[order(res$MeanUMICounts, decreasing = TRUE),]
  return(res)
}

summary_features <- function(SeuratObj, features, group.by, assay = 'RNA'){
  requireNamespace("dplyr")
  requireNamespace("Seurat")

  # Use helper function to get data matrix
  normalized.expr <- get_data_matrix(SeuratObj, assay)

  if (!is.null(group.by)) {
    all.cell.types <- unique(SeuratObj@meta.data[,group.by])
    res <- list()
    for (celltype in all.cell.types) {
      cells.sub <- colnames(SeuratObj)[as.character(SeuratObj@meta.data[,group.by]) == celltype]
      if (length(cells.sub) < 3) {
        next
      }
      res[[celltype]] <- summary_features_core(expr_mat=normalized.expr[,cells.sub], features=features, group = celltype)
    }
    res <- Reduce(rbind, res)
    rownames(res) <- NULL
  }else{
    res <- summary_features_core(expr_mat=normalized.expr, features=features, group = 'AllSelectedCells')
  }
  return(res)
}


summary_features_core <- function(expr_mat, features, group = 'merged'){
  mean.expr <- apply(expr_mat[features,,drop = FALSE], 1, mean)
  median.expr <- apply(expr_mat[features,,drop = FALSE], 1, stats::median)
  pct <- apply(expr_mat[features,,drop = FALSE] > 0, 1, mean)
  single.res <- data.frame(Gene = features, Expr.mean = round(mean.expr,digits = 4), Expr.median = round(median.expr, digits = 4), PCT = round(pct,digits = 4))
  single.res$CellType <- group
  single.res$TotalCells <- ncol(expr_mat)
  single.res <- single.res[,c("CellType", "TotalCells","Gene", "PCT", "Expr.mean", "Expr.median")]
  rownames(single.res) <- NULL
  return(single.res)
}



calculate_top_correlations <- function(SeuratObj, method, top = 1000, assay = 'RNA'){
  # Use helper function to get data matrix
  normalized.expr <- get_data_matrix(SeuratObj, assay)

  # Filter genes with low expression (mean expression > 0.1 across all cells)
  # This is an important filter to reduce computation time
  gene_means <- rowMeans(normalized.expr)
  normalized.expr <- normalized.expr[gene_means > 0.1, , drop = FALSE]
  cor.res = stats::cor(t(as.matrix(normalized.expr)), method = method)
  cor.res[lower.tri(cor.res, diag = TRUE)] <- 0
  cor.res <- reshape2::melt(cor.res)
  colnames(cor.res) <- c("GeneA","GeneB","correlation")
  cor.res <- cor.res[order(abs(cor.res$correlation),decreasing = TRUE),]
  cor.res <- cor.res[cor.res$correlation != 0,,drop = FALSE]
  cor.res$correlation <- round(cor.res$correlation, digits = 4)
  if (nrow(cor.res) > top) {
    cor.res <- cor.res[1:top, ]
  }
  rownames(cor.res) <- NULL
  return(cor.res)
}

calculate_most_correlated <- function(SeuratObj, feature, method, assay = 'RNA'){
  # Use helper function to get data matrix
  normalized.expr <- get_data_matrix(SeuratObj, assay)

  x <- normalized.expr[feature, ,drop = FALSE]
  y <- normalized.expr[rownames(normalized.expr)[rownames(normalized.expr) != feature],,drop = FALSE]
  # filter cells: remove genes with low expression, accumulated expression should more than 1/10 cell numbers.
  y <- y[apply(y, 1, sum) > 0.1 * ncol(SeuratObj),,drop = FALSE]
  cor.res = stats::cor(x = t(as.matrix(x)), y =  t(as.matrix(y)), method = method)
  cor.res <- reshape2::melt(cor.res)
  colnames(cor.res) <- c("GeneA","GeneB","correlation")
  cor.res <- cor.res[order(abs(cor.res$correlation),decreasing = TRUE),]
  cor.res$correlation <- round(cor.res$correlation, digits = 4)
  rownames(cor.res) <- NULL
  return(cor.res)
}

calculate_correlation <- function(SeuratObj, features, method, assay = 'RNA'){
  # Use helper function to get data matrix
  normalized.expr <- get_data_matrix(SeuratObj, assay)

  normalized.expr <- normalized.expr[rownames(normalized.expr) %in% features,,drop = FALSE]
  # filter cells: remove genes with low expression, accumulated expression should more than 1/10 cell numbers.
  normalized.expr <- normalized.expr[apply(normalized.expr, 1, sum) > 0.1 * ncol(SeuratObj),,drop = FALSE]
  cor.res = stats::cor(t(as.matrix(normalized.expr)), method = method)
  cor.res[lower.tri(cor.res, diag = TRUE)] <- 0
  cor.res <- reshape2::melt(cor.res)
  colnames(cor.res) <- c("GeneA","GeneB","correlation")
  cor.res <- cor.res[order(abs(cor.res$correlation),decreasing = TRUE),]
  cor.res <- cor.res[cor.res$correlation != 0, ,drop = FALSE]
  cor.res$correlation <- round(cor.res$correlation, digits = 4)
  rownames(cor.res) <- NULL
  return(cor.res)
}


# refer to: https://github.com/junjunlab/scRNAtoolVis/blob/master/R/averageHeatmap.R
AverageHeatmap <- function(
    object = NULL,
    markerGene = NULL,
    group.by = "ident",
    assays = "RNA",
    slot = "data",
    htCol = c("#0099CC", "white", "#CC0033"),
    colseed = 666,
    htRange = c(-2, 0, 2),
    annoCol = FALSE,
    myanCol = NULL,
    annoColType = "light",
    annoColTypeAlpha = 0,
    clusterAnnoName = TRUE,
    showRowNames = TRUE,
    row_names_side = "left",
    markGenes = NULL,
    border = FALSE,
    cluster.fontsize = 12,
    feature.fontsize = 10,
    column_names_rot = 45,
    width = NULL,
    height = NULL,
    cluster.order = NULL,
    cluster_columns = FALSE,
    cluster_rows = FALSE,
    gene.order = NULL,
    ...) {
  requireNamespace("ComplexHeatmap")

  # Input validation
  if (is.null(object)) stop("object cannot be NULL")
  if (is.null(markerGene)) stop("markerGene cannot be NULL")

  # get cells mean gene expression
  # check Seurat version first
  mean_gene_exp <- as.matrix(
      data.frame(
        Seurat::AverageExpression(object,
                                  features = markerGene,
                                  group.by = group.by,
                                  assays = assays,
                                  layer = slot
      )
    )
  )
  colnames(mean_gene_exp) <- levels(Seurat::Idents(object))

  # Z-score
  htdf <- t(scale(t(mean_gene_exp), scale = TRUE, center = TRUE))

  # cluster order
  if (!is.null(cluster.order)) {
    htdf <- htdf[, cluster.order]
  }

  # gene order
  if (!is.null(gene.order)) {
    htdf <- htdf[gene.order, ]
  }

  # color
  col_fun <- circlize::colorRamp2(htRange, htCol)

  # anno color
  if (annoCol == FALSE) {
    set.seed(colseed)
    anno_col <- circlize::rand_color(
      ncol(htdf),
      luminosity = annoColType,
      transparency = annoColTypeAlpha
    )
    # print(c("Your cluster annotation color is:", anno_col))
  } else if (annoCol == TRUE) {
    # give your own color vectors
    anno_col <- myanCol
  } else {
    stop("Give TRUE or FALSE paramters!")
  }
  names(anno_col) <- colnames(htdf)

  # top annotation
  column_ha <- ComplexHeatmap::HeatmapAnnotation(
    cluster = colnames(htdf),
    show_legend = FALSE,
    show_annotation_name = clusterAnnoName,
    col = list(cluster = anno_col)
  )

  # whether mark your genes on plot
  if (!is.null(markGenes)) {
    # all genes
    rowGene <- rownames(htdf)

    # tartget gene
    annoGene <- markGenes

    # get target gene index
    index <- match(annoGene, rowGene)

    # some genes annotation
    geneMark <- ComplexHeatmap::rowAnnotation(
      gene = ComplexHeatmap::anno_mark(
        at = index,
        labels = annoGene,
        labels_gp = grid::gpar(
          fontface = "italic",
          fontsize = feature.fontsize
        )
      ),
      ...
    )

    right_annotation <- geneMark
  } else {
    right_annotation <- NULL
  }

  # control heatmap width and height
  if (is.null(width) || is.null(height)) {
    # plot
    ComplexHeatmap::Heatmap(
      htdf,
      show_row_dend = TRUE,
      show_column_dend = TRUE,
      name = "Z-score",
      cluster_columns = cluster_columns,
      cluster_rows = cluster_rows,
      # column_title = "Clusters",
      right_annotation = right_annotation,
      show_row_names = showRowNames,
      column_names_gp = grid::gpar(fontsize = cluster.fontsize),
      row_names_gp = grid::gpar(
        fontface = "italic",
        fontsize = feature.fontsize
      ),
      row_names_side = row_names_side,
      border = border,
      column_names_side = "top",
      column_names_rot = column_names_rot,
      top_annotation = column_ha,
      col = col_fun,
      ...
    )
  } else {
    # plot
    ComplexHeatmap::Heatmap(
      htdf,
      show_row_dend = TRUE,
      show_column_dend = TRUE,
      name = "Z-score",
      cluster_columns = FALSE,
      cluster_rows = FALSE,
      # column_title = "Clusters",
      right_annotation = right_annotation,
      show_row_names = showRowNames,
      column_names_gp = grid::gpar(fontsize = cluster.fontsize),
      row_names_gp = grid::gpar(
        fontface = "italic",
        fontsize = feature.fontsize
      ),
      row_names_side = row_names_side,
      border = border,
      column_names_side = "top",
      column_names_rot = column_names_rot,
      top_annotation = column_ha,
      col = col_fun,
      width = ggplot2::unit(width, "cm"),
      height = ggplot2::unit(height, "cm"),
      ...
    )
  }
}

readSeurat <- function(path, verbose = FALSE){
  if(verbose){message("SeuratExplorer: Reading in data...")}
  # read data
  if (tools::file_ext(path) == 'qs2') {
    seu_obj <- qs2::qs_read(path)
  }else(
    seu_obj <- readRDS(path)
  )
  # update Seurat object
  if (class(seu_obj)[[1]] == 'seurat') { # for very old version: seurat object
    if(verbose){message('SeuratExplorer: prepare_seurat_object Update Seurat Object for very old versions!')}
    seu_obj <- suppressMessages(SeuratObject::UpdateSeuratObject(seu_obj))
  }else if(SeuratObject::Version(seu_obj) < utils::packageVersion('SeuratObject')) {
    if(verbose){message('SeuratExplorer: Update Seurat Object for old versions!')}
    seu_obj <- suppressMessages(SeuratObject::UpdateSeuratObject(seu_obj))
  }else{
    if(verbose){message('SeuratExplorer: Update Seurat Object escaped for it has been the latest version!')}
  }
  return(seu_obj)
}


# > SCT assay related bug:
# https://github.com/satijalab/seurat/issues/8235; 2025.03.26, may be Seurat Package will solve this bug in future.
# and: https://github.com/satijalab/seurat/pull/8203
# this bug can affect 'FindAllMarkers' function.
check_SCT_assay <- function(seu_obj){
  if (DefaultAssay(seu_obj) == "SCT") {
    if (length(seu_obj@assays$SCT@SCTModel.list) > 1) {
      SCT_first_umiassay <- seu_obj@assays$SCT@SCTModel.list[[1]]@umi.assay
      for (i in 2:length(seu_obj@assays$SCT@SCTModel.list)) {
        methods::slot(object = seu_obj@assays$SCT@SCTModel.list[[i]], name="umi.assay") <- SCT_first_umiassay
      }
      seu_obj <- Seurat::PrepSCTFindMarkers(object = seu_obj)
    }
  }
  return(seu_obj)
}

check_genes_error <- "None of the input genes can be found!"

# for plot features related functions when none of the input features can be recognized
empty_plot <- ggplot2::ggplot() +
  ggplot2::annotate('text', x = 0, y = 0, label = 'Please input correct features!\n Unrecognized features will be removed automatically.\n You can check the features in "Search Features" page.\n Or at leaset select one cluster.', color = 'darkgrey', size = 6)  +
  ggplot2::theme_bw() +
  ggplot2::geom_blank() +
  ggplot2::theme(axis.title = ggplot2::element_blank(),
                 axis.text = ggplot2::element_blank(),
                 axis.ticks = ggplot2::element_blank())



check_allowed_chars <- function(text_string) {
  # Returns TRUE if NO forbidden characters are found (meaning only allowed chars are present)
  !grepl("[^a-zA-Z0-9_-]", text_string)
}


# create_resizable_plot_ui is a function created by Claude code
create_resizable_plot_ui <- function(plot_id, initial_width = 800, initial_height = 720) {
  div(
    id = paste0(plot_id, "_wrapper"),
    style = "
        background-color: #f8f9fa;
        border: 2px dashed #dee2e6;
        border-radius: 8px;
        padding: 20px;
        position: relative;
        overflow: visible;
      ",
    shinyjqui::jqui_resizable(
      div(
        id = paste0(plot_id, "_inner"),
        style = paste0("
            width: ", initial_width, "px;
            height: ", initial_height, "px;
            overflow: hidden;
            position: relative;
          "),
        plotOutput(plot_id, width = "100%", height = "100%")
      ),
      options = list(
        minWidth = 100,
        maxWidth = 2000,
        minHeight = 100,
        maxHeight = 6000,
        handles = "s, e, se"  # Only enable south, east, and south-east handles
      )
    ),
    # Add visual indicators for the resizable handle
    tags$style(HTML(paste0("
        /* Ensure resizable container works properly */
        #", plot_id, "_inner.ui-resizable {
          position: relative !important;
        }

        /* Base handle styles - always visible */
        .ui-resizable-handle {
          position: absolute;
          z-index: 9999 !important;
          font-size: 0.1px;
          display: block;
        }

        /* Bottom edge handle */
        .ui-resizable-s {
          height: 20px !important;
          bottom: -10px !important;
          left: 0 !important;
          width: 100% !important;
          cursor: s-resize;
          background: rgba(0, 123, 255, 0.1) !important;
          border-bottom: 4px solid #007bff !important;
        }

        .ui-resizable-s:hover {
          background: rgba(0, 123, 255, 0.2) !important;
          border-bottom: 5px solid #0056b3 !important;
        }

        /* Right edge handle */
        .ui-resizable-e {
          width: 20px !important;
          right: -10px !important;
          top: 0 !important;
          height: 100% !important;
          cursor: e-resize;
          background: rgba(0, 123, 255, 0.1) !important;
          border-right: 4px solid #007bff !important;
        }

        .ui-resizable-e:hover {
          background: rgba(0, 123, 255, 0.2) !important;
          border-right: 5px solid #0056b3 !important;
        }

        /* Bottom-right corner handle */
        .ui-resizable-se {
          bottom: -10px !important;
          right: -10px !important;
          width: 30px !important;
          height: 30px !important;
          cursor: se-resize;
          background: conic-gradient(from 225deg, transparent 0deg, transparent 90deg, #007bff 90deg, #007bff 180deg, transparent 180deg) !important;
          border: 3px solid #007bff !important;
          border-radius: 4px !important;
        }

        .ui-resizable-se:hover {
          background: conic-gradient(from 225deg, transparent 0deg, transparent 90deg, #0056b3 90deg, #0056b3 180deg, transparent 180deg) !important;
          transform: scale(1.15);
        }

        /* Make plot canvas visible */
        #", plot_id," {
          display: block !important;
        }
      ")
      )
    ),
    # JavaScript to sync plot dimensions with resizable container
    tags$script(HTML(paste0("
        $(function() {
          var updateDimensions = function() {
            var container = $('#", plot_id, "_inner');
            var width = container.width();
            var height = container.height();
            Shiny.setInputValue('", plot_id, "_width', width, {priority: 'event'});
            Shiny.setInputValue('", plot_id, "_height', height, {priority: 'event'});
          };

          // Initial dimensions
          updateDimensions();

          // Update on resize
          var resizeTimer;
          $('#", plot_id, "_inner').on('resize', function(e, ui) {
            clearTimeout(resizeTimer);
            resizeTimer = setTimeout(function() {
              Shiny.setInputValue('", plot_id, "_width', ui.size.width, {priority: 'event'});
              Shiny.setInputValue('", plot_id, "_height', ui.size.height, {priority: 'event'});
            }, 100);
          });

          // Update periodically as a fallback
          setInterval(updateDimensions, 2000);
        });
      "))
    )
  )
}

