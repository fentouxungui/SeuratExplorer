#!/usr/bin/env Rscript
# SeuratExplorer fix: "Operation not allowed without an active reactive context"
# in the cancelable DEG / Top-genes computations.
#
# Why it fails: later() callbacks are NOT reactive contexts, so step() must not
# read reactiveVal()/input$ values, and UI helpers need an explicit domain.
#
# Usage (either):
#   setwd("D:/GitHub_Res/SeuratExplorer"); source("fix_cancel_reactive.R")
#   Rscript fix_cancel_reactive.R "D:/GitHub_Res/SeuratExplorer"
#
# The script rewrites R/server.R in place, then parse()s it. It refuses to write
# anything unless every pattern matches the expected number of times.

args <- commandArgs(trailingOnly = TRUE)
repo <- if (length(args) >= 1L) args[1] else getwd()
f <- file.path(repo, "R", "server.R")
if (!file.exists(f)) {
  stop("Cannot find ", f, "\nSet the working directory to the SeuratExplorer repo root, ",
       "or pass the repo path as the first argument.", call. = FALSE)
}

lines <- readLines(f, warn = FALSE, encoding = "UTF-8")
crlf <- any(grepl("\r$", lines))
lines <- sub("\r$", "", lines)
txt <- paste(lines, collapse = "\n")

count_fixed <- function(text, pattern) {
  m <- gregexpr(pattern, text, fixed = TRUE)[[1]]
  if (length(m) == 1L && identical(m[1], -1L)) 0L else length(m)
}

repl <- function(text, old, new, label, times = 1L) {
  n <- count_fixed(text, old)
  if (n != times) {
    stop(sprintf("[%s] expected %d match(es), found %d. File NOT modified.",
                 label, times, n), call. = FALSE)
  }
  for (i in seq_len(times)) text <- sub(old, new, text, fixed = TRUE)
  cat(sprintf("  ok  %-28s (%d)\n", label, times))
  text
}

cat("Patching", f, "\n")

# 1. cancel flag: reactiveVal -> plain environment --------------------------
txt <- repl(txt,
  paste(
    "  # Long computations are executed in chunks (one cluster / one batch of cells",
    "  # per event-loop tick) so pending input is processed in between. A Cancel",
    "  # click therefore stops the run at the next chunk boundary.",
    "  long_task_cancelled <- reactiveVal(FALSE)",
    "  observeEvent(input$ClustermarkersCancel, { long_task_cancelled(TRUE) }, ignoreInit = TRUE)",
    "  observeEvent(input$TopGenesCancel,       { long_task_cancelled(TRUE) }, ignoreInit = TRUE)",
    sep = "\n"),
  paste(
    "  # Long computations are executed in chunks (one cluster / one batch of cells",
    "  # per event-loop tick) so pending input is processed in between. A Cancel",
    "  # click therefore stops the run at the next chunk boundary.",
    "  # NOTE: later() callbacks are not reactive contexts, so the flag is kept in a",
    "  # plain environment (a reactiveVal cannot be read from inside step()).",
    "  long_task_state <- new.env(parent = emptyenv())",
    "  long_task_state$cancelled <- FALSE",
    "  observeEvent(input$ClustermarkersCancel, { long_task_state$cancelled <- TRUE }, ignoreInit = TRUE)",
    "  observeEvent(input$TopGenesCancel,       { long_task_state$cancelled <- TRUE }, ignoreInit = TRUE)",
    sep = "\n"),
  "cancel flag -> environment")

# 2. DEGs: snapshot the inputs before later() ---------------------------------
txt <- repl(txt,
  paste(
    "      long_task_cancelled(FALSE)",
    "      parts <- vector(\"list\", length(idents_all))",
    sep = "\n"),
  paste(
    "      long_task_state$cancelled <- FALSE",
    "      deg_testuse <- input$testuse",
    "      deg_logfc   <- input$logfcthreshold",
    "      deg_minpct  <- input$minpct",
    "      deg_mindiff <- if (input$mindiffpct == 0) -Inf else input$mindiffpct",
    "      deg_assay   <- input$DEGsAssay",
    "      parts <- vector(\"list\", length(idents_all))",
    sep = "\n"),
  "DEGs input snapshot")

# 3. DEGs: step -> step_body (so return() still exits the body) ---------------
txt <- repl(txt,
  paste(
    "      # One cluster per tick: identical results to Seurat::FindAllMarkers,",
    "      # but the event loop stays free between clusters.",
    "      step <- function() {",
    "        if (long_task_cancelled()) {",
    sep = "\n"),
  paste(
    "      # One cluster per tick: identical results to Seurat::FindAllMarkers,",
    "      # but the event loop stays free between clusters.",
    "      step_body <- function() {",
    "        if (long_task_state$cancelled) {",
    sep = "\n"),
  "DEGs step header")

txt <- repl(txt,
  paste(
    "        later::later(step, 0)",
    "      }",
    "",
    "      later::later(step, 0)",
    sep = "\n"),
  paste(
    "        later::later(step, 0)",
    "      }",
    "      step <- function() shiny::withReactiveDomain(session, step_body())",
    "",
    "      later::later(step, 0)",
    sep = "\n"),
  "DEGs step wrapper")

# 4. DEGs: use the snapshots inside the step ----------------------------------
txt <- repl(txt,
  paste(
    "          find_markers_one_cluster(object = cds, cluster = cl,",
    "                                   cluster_col = cluster_res,",
    "                                   assay = input$DEGsAssay,",
    "                                   test.use = input$testuse,",
    "                                   logfc.threshold = input$logfcthreshold,",
    "                                   min.pct = input$minpct,",
    "                                   min.diff.pct = if (input$mindiffpct == 0) -Inf else input$mindiffpct,",
    "                                   only.pos = TRUE),",
    sep = "\n"),
  paste(
    "          find_markers_one_cluster(object = cds, cluster = cl,",
    "                                   cluster_col = cluster_res,",
    "                                   assay = deg_assay,",
    "                                   test.use = deg_testuse,",
    "                                   logfc.threshold = deg_logfc,",
    "                                   min.pct = deg_minpct,",
    "                                   min.diff.pct = deg_mindiff,",
    "                                   only.pos = TRUE),",
    sep = "\n"),
  "DEGs call arguments")

# 5. Top genes by cell: runs start -------------------------------------------
txt <- repl(txt,
  paste(
    "    long_task_cancelled(FALSE)",
    "    collected <- list()",
    sep = "\n"),
  paste(
    "    long_task_state$cancelled <- FALSE",
    "    collected <- list()",
    sep = "\n"),
  "TopGenes(cell) start")

# 6. Top genes by mean UMI: runs start ---------------------------------------
txt <- repl(txt,
  paste(
    "    long_task_cancelled(FALSE)",
    "    out_rows <- list()",
    "    gi <- 0L",
    sep = "\n"),
  paste(
    "    long_task_state$cancelled <- FALSE",
    "    top_n <- input$TopGenesTopN",
    "    out_rows <- list()",
    "    gi <- 0L",
    sep = "\n"),
  "TopGenes(UMI) start")

# 7. both Top-genes steps (identical text appears twice) ---------------------
txt <- repl(txt,
  paste(
    "    step <- function() {",
    "      if (long_task_cancelled()) {",
    sep = "\n"),
  paste(
    "    step_body <- function() {",
    "      if (long_task_state$cancelled) {",
    sep = "\n"),
  "TopGenes step headers", times = 2L)

txt <- repl(txt,
  paste(
    "      later::later(step, 0)",
    "    }",
    "",
    "    later::later(step, 0)",
    sep = "\n"),
  paste(
    "      later::later(step, 0)",
    "    }",
    "    step <- function() shiny::withReactiveDomain(session, step_body())",
    "",
    "    later::later(step, 0)",
    sep = "\n"),
  "TopGenes step wrappers", times = 2L)

# 8. TopGenes(UMI): use the snapshot -----------------------------------------
txt <- repl(txt,
  "        top_n = input$TopGenesTopN,",
  "        top_n = top_n,",
  "TopGenes(UMI) top_n")

# 9. sanity checks -----------------------------------------------------------
if (count_fixed(txt, "long_task_cancelled") != 0L) {
  stop("left-over 'long_task_cancelled' reference; file NOT modified.", call. = FALSE)
}
if (count_fixed(txt, "step_body <- function()") != 3L) {
  stop("expected 3 step_body definitions; file NOT modified.", call. = FALSE)
}
if (count_fixed(txt, "withReactiveDomain(session, step_body())") != 3L) {
  stop("expected 3 step wrappers; file NOT modified.", call. = FALSE)
}

out <- strsplit(txt, "\n", fixed = TRUE)[[1]]
con <- file(f, open = "wb")
writeLines(out, con, sep = if (crlf) "\r\n" else "\n", useBytes = TRUE)
close(con)

invisible(parse(file = f))
cat("\nDone.", f, "rewritten and parsed successfully.\n")
cat("Restart SeuratExplorer and test the Cancel button.\n")
