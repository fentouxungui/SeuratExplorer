#' Launch shiny app
#'
#' @description
#' used to launch the shiny app in a web browser.
#'
#' @param verbose for debug use
#' @param ReductionKeyWords key words used for prepare Reduction options
#' @param SplitOptionMaxLevel max level cutoff for prepare Split options
#' @param MaxInputFileSize set the limited upload file size
#'
#' @import shiny
#' @return In-browser Shiny Application launch
#' @examples
#' if(interactive()){launchSeuratExplorer()}
#' @export
launchSeuratExplorer <- function(verbose = FALSE,
                                 ReductionKeyWords = c("umap","tsne"),
                                 SplitOptionMaxLevel = 12,
                                 MaxInputFileSize = 20*1024^3 # default 20GB
                                 ){
  options(SeuratExplorerVerbose = verbose)
  options(SeuratExplorerReductionKeyWords = ReductionKeyWords)
  options(SeuratExplorerSplitOptionMaxLevel = SplitOptionMaxLevel)
  options(shiny.maxRequestSize = MaxInputFileSize)

  shinyApp(ui, server)
}
