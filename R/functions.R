prepare_seurat_object <- function(obj){
  requireNamespace("Seurat")
  # 将meta.data中的部分非factor类型的列，转为factor类型
  # 如果unique数目少于细胞总数的1/20，并且不大于50种，并且数据类型为chr或num类型，会强制转为factor类型。
  # 可能的问题： unique_max_percent = 0.05可能不适合只有100个细胞但由大于5群的的数据
  obj@meta.data <- modify_columns_types(df = obj@meta.data, types_to_check = c("numeric", "character"), unique_max_counts = 50, unique_max_percent = 0.05)
  # for splited object, join layers
  if (sum(grepl("^counts",Layers(object = obj))) > 1 | sum(grepl("^data",Layers(object = obj))) > 1) {
    obj <- SeuratObject::JoinLayers(object = obj)
  }
  message("SeuratExplorer: prepare_seurat_object runs successfully!")
  return(obj)
}

# 把符合条件的非因子列，转为因子类型, 并且把可能是数字的字符串转为数字
modify_columns_types <- function(df, types_to_check = c("numeric", "character"), unique_max_counts = 50, unique_max_percent = 0.05){
  candidates.types.logic <- sapply(df, class) %in% types_to_check
  cutoff <- min(unique_max_counts, round(nrow(df) * unique_max_percent))
  candidates.unique.logic <- sapply(df, FUN = function(x)length(unique(x))) <= cutoff
  candidates.logic <- candidates.types.logic & candidates.unique.logic & !sapply(df, is.factor)
  # before trans to factor, trans numeric character vector to numeric vector: c('1','2','1') to c(1,2,1)
  char2numeric_columns.logic <- suppressWarnings(unlist(lapply(df[candidates.logic], function(x)any(!is.na(as.numeric(unique(x)))))))
  char2numeric_columns <- names(char2numeric_columns.logic)[char2numeric_columns.logic]
  df[char2numeric_columns] <- lapply(df[char2numeric_columns], as.numeric)
  # finally trans all char and numeric to factor
  df[candidates.logic] <- lapply(df[candidates.logic], as.factor)
  message("SeuratExplorer: modify_columns_types runs successfully!")
  return(df)
}

# 通过关键字换取reduction options
prepare_reduction_options <- function(obj, keywords = c("umap","tsne")){
  requireNamespace("Seurat")
  reduction.choice <- grep(paste0(paste0("(", keywords,")"),collapse = "|"), Seurat::Reductions(obj), value = TRUE, ignore.case = TRUE)
  names(reduction.choice) <- toupper(reduction.choice)
  message("SeuratExplorer: prepare_reduction_options runs successfully!")
  return(reduction.choice)
}


# 将所有meta.data中所有类型为因子的列名作为cluster options
prepare_cluster_options <- function(df){
  cluster.options <- colnames(df)[sapply(df, is.factor)]
  names(cluster.options) <- cluster.options
  message("SeuratExplorer: prepare_cluster_options runs successfully!")
  return(cluster.options)
}

# 将所有meta.data中level数目少于max_level的因子列作为split options
prepare_split_options <- function(df, max.level = 4){
  cluster.options <- colnames(df)[sapply(df, is.factor)]
  leve.counts <- unname(sapply(df[cluster.options],FUN = function(x)length(levels(x))))
  split.options <- cluster.options[leve.counts <= max.level]
  names(split.options) <- split.options
  message("SeuratExplorer: prepare_split_options runs successfully!")
  return(split.options)
}

# 添加额外的来自meta data的列名为qc options
prepare_qc_options <- function(df, types = c("double","integer","numeric")){
  qc_options <- colnames(df)[sapply(df, class) %in% types]
  message("SeuratExplorer: prepare_qc_options runs successfully!")
  return(qc_options)
}


# Check the input gene, return the revised gene, which can be used for FeaturePlot, Vlnplot ect.
CheckGene <- function(InputGene, GeneLibrary){
  InputGenes <- unlist(strsplit(InputGene,split = " "))
  InputGenes <- InputGenes[InputGenes != ""]
  revised.genes <- sapply(InputGenes, FUN = function(x)ReviseGene(x, GeneLibrary = GeneLibrary))
  revised.genes <- unique(unname(revised.genes[!is.na(revised.genes)]))
  message("SeuratExplorer: CheckGene runs successfully!")
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


# 检查差异分析时所需要的R包依赖
check_dependency <- function(test){
  if (test == "wilcox") { # 检查所需要的R包,暂时这样吧，其它几个test先不测试了
    if(!require(devtools, quietly = TRUE)){
      utils::install.packages("devtools")
    }
    if(!require(presto)){
      devtools::install_github('immunogenomics/presto', upgrade = "never")
    }
  }else if(test == "DESeq2"){
    if (!require("BiocManager", quietly = TRUE))
      utils::install.packages("BiocManager")
    if (!require("DESeq2", quietly = TRUE)) {
      BiocManager::install("DESeq2", update = FALSE, ask = FALSE)
    }
  } else if(test == "MAST"){
    if (!require("BiocManager", quietly = TRUE))
      utils::install.packages("BiocManager")
    if (!require("MAST", quietly = TRUE)) {
      BiocManager::install("MAST", update = FALSE, ask = FALSE)
    }
  }
}


