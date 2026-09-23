#' Launch shiny app
#'
#' @description
#' used to launch the shiny app in a web browser.
#'
#' @param verbose for debug use
#' @param ReductionKeyWords key words used for prepare Reduction options
#' @param SplitOptionMaxLevel max level cutoff for prepare Split options
#' @param MaxInputFileSize set the limited upload file size
#' @param DemoDataURL one or more URLs of the demo Seurat object downloaded by
#'   the "Download and Run a demo data" button. Multiple URLs are tried in
#'   order until one succeeds (primary, then backup)
#' @param DemoDataCacheDir directory used to cache the downloaded demo data;
#'   if NULL, a per-user cache directory is used
#' @param DemoDataMD5 expected md5 checksum of the downloaded demo data;
#'   set to NULL to skip the integrity check
#'
#' @import shiny
#' @return In-browser Shiny Application launch
#' @examples
#' if(interactive()){launchSeuratExplorer()}
#' @export
launchSeuratExplorer <- function(verbose = FALSE,
                                 ReductionKeyWords = c("umap","tsne","pca"),
                                 SplitOptionMaxLevel = 12,
                                 MaxInputFileSize = 20*1024^3, # default 20GB
                                 DemoDataURL = c("https://raw.githubusercontent.com/fentouxungui/SeuratExplorerServer/refs/heads/main/inst/extdata/source-data/fly/Rds-file/G101_PC20res04.rds",
                                                 "https://drive.usercontent.google.com/download?id=1iscfl4zyNbtjAol0bwndnBI08MDcZvYE&export=download&authuser=0&confirm=t&uuid=6352f610-9125-4551-afaa-e1eedd15701c&at=AMrWOn0YlTymrOPO6Rb0GlFtJ07o%3A1790127440033"),
                                 DemoDataCacheDir = NULL,
                                 DemoDataMD5 = "e144cabc42684f037e60a603f74ed4f1"
                                 ){
  options(SeuratExplorerVerbose = verbose)
  options(SeuratExplorerReductionKeyWords = ReductionKeyWords)
  options(SeuratExplorerSplitOptionMaxLevel = SplitOptionMaxLevel)
  options(shiny.maxRequestSize = MaxInputFileSize)
  options(SeuratExplorerDemoDataURL = DemoDataURL)
  options(SeuratExplorerDemoDataCacheDir = DemoDataCacheDir)
  options(SeuratExplorerDemoDataMD5 = DemoDataMD5)
  # Suppress the `as.list.reactivevalues()` deprecation warning emitted by
  # shinydashboard 0.7.3 (and other older deps) on newer Shiny versions.
  options(shiny.deprecation.messages = FALSE)

  shinyApp(ui, server)
}
