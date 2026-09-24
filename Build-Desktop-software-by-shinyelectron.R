# Build the SeuratExplorer desktop app with shinyelectron.
#
# Runs both locally (Rscript) and in GitHub Actions. It uses the *fork* of
# shinyelectron that supports `dependencies.r.local_packages` and `updates`:
#
#   pak::pak("fentouxungui/shinyelectron")
#
# Local use:  Rscript Build-Desktop-software-by-shinyelectron.R
# CI use:     GITHUB_WORKSPACE and APP_VERSION are set by the workflow.
#
# Repository layout expected:
#   DESCRIPTION, R/, ...                        (the R package)
#   dependency/presto-1.1.0.tar.gz              (committed)
#   dependency/SeuratExplorer_<version>.tar.gz  (built by the build-pkg job)
#   icons/ico/emerald.ico                       (optional)

library(shinyelectron)
# install_nodejs()

# ---- paths (CI-aware) -------------------------------------------------
ws <- Sys.getenv("GITHUB_WORKSPACE", unset = getwd())
app_dir    <- file.path(ws, "build-app")
output_dir <- file.path(ws, "build")
dep_dir    <- file.path(ws, "dependency")

dir.create(app_dir, recursive = TRUE, showWarnings = FALSE)

# ---- version: tag (CI) > DESCRIPTION > fallback -----------------------
se_version <- sub("^v", "", Sys.getenv("APP_VERSION", unset = ""))
if (!nzchar(se_version)) {
  desc <- file.path(ws, "DESCRIPTION")
  se_version <- if (file.exists(desc)) {
    as.character(read.dcf(desc, fields = "Version")[[1]])
  } else "0.0.0"
}

# ---- 1. Shiny entry point --------------------------------------------
app_code <- '
library(SeuratExplorer)

# shinyelectron runs shiny::runApp() itself; only return the app object.
options(shiny.launch.browser = FALSE)
options(shiny.deprecation.messages = FALSE)

launchSeuratExplorer(
  verbose = FALSE,
  ReductionKeyWords = c("umap", "tsne", "pca"),
  SplitOptionMaxLevel = 12,
  MaxInputFileSize = 30 * 1024^3   # unlocks the 30 GB single-cell upload limit
)
'
writeLines(app_code, file.path(app_dir, "app.R"))

# ---- 2. _shinyelectron.yml --------------------------------------------
# Both local archives are installed into the bundled R, so the app ships the
# exact SeuratExplorer build we tested plus the non-CRAN `presto`.  Absolute
# paths are used because the installer resolves them from the R process cwd.
local_se <- normalizePath(
  file.path(dep_dir, sprintf("SeuratExplorer_%s.tar.gz", se_version)),
  winslash = "/", mustWork = FALSE
)
local_presto <- normalizePath(
  file.path(dep_dir, "presto-1.1.0.tar.gz"),
  winslash = "/", mustWork = FALSE
)

config_code <- paste0('
app:
  version: "', se_version, '"
  slug: "seuratexplorer"
  log_level: "info"
  description: "An interactive R shiny application for exploring scRNAseq data processed in Seurat"
  author: "Zhang Yongchao"
  email: "zhangyongchao@nibs.ac.cn"
  homepage: "https://github.com/fentouxungui/SeuratExplorer"
  copyright: "Copyright (c) 2026 Zhang Yongchao. GPL (>= 3)."

build:
  runtime_strategy: "bundled"

window:
  width: 1400
  height: 900

menu:
  help_url: "https://github.com/fentouxungui/SeuratExplorer"

installer:
  app_id: "com.seuratexplorer.desktop"
  one_click: false
  allow_to_change_installation_directory: true

# No code-signing certificate yet; enable later with CSC_LINK / CSC_KEY_PASSWORD.
signing:
  sign: false

dependencies:
  extra_packages:
    - SeuratExplorer
    - Seurat
    - ggplot2
    - DT
  r:
    local_packages:
      - "', local_se, '"
      - "', local_presto, '"

updates:
  enabled: true
  provider: "github"
  check_on_startup: true
  auto_download: true      # download new builds in the background
  auto_install: true       # install silently on quit (false = prompt)
  github:
    owner: "fentouxungui"
    repo: "SeuratExplorer"

optimize:
  r_library: true
  r_runtime: true
')
writeLines(config_code, file.path(app_dir, "_shinyelectron.yml"))

# ---- 3. Build ----------------------------------------------------------
# The .ico works for Windows; for macOS provide an .icns via `icons.mac` (or
# an .png) if you want a custom dock icon.
icon <- file.path(ws, "icons", "ico", "emerald.ico")

options(timeout = 3600)   # bundled installs pull a lot of packages; be patient

export(
  appdir    = app_dir,
  destdir   = output_dir,
  app_name  = "SeuratExplorer",
  icon      = if (file.exists(icon)) icon else NULL,
  run_after = FALSE,
  overwrite = TRUE
)
