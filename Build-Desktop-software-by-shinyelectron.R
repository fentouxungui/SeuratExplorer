# # 安装 pak 管理工具（如果未安装）
# if(!require('pak')) install.packages('pak')

# # 从 GitHub 安装 shinyelectron
# pak::pak("coatless-rpkg/shinyelectron")
# or install a for version
# pak::pak("fentouxungui/shinyelectron")

# # 加载包
# library(shinyelectron)
# install_nodejs()

# # 【核心验证】检查你的电脑环境是否满足编译要求
# # 如果提示缺少工具，请根据输出的指引修复（Windows 需要 Visual Studio Build Tools）
# sitrep_shinyelectron()


library(shinyelectron)

# # 1. 建立打包工程目录（放在桌面）
app_dir <- "C:/Users/Xi_Lab/Desktop/SeuratExplorerApp"
if(!dir.exists(app_dir)) dir.create(app_dir, recursive = TRUE)

# 2. 写入 Shiny 启动脚本 app.R
# 已针对单细胞大数据解锁了 30GB 限制
app_code <- '
library(SeuratExplorer)

# shinyelectron 会自己执行 shiny::runApp(appDir, port=<它选定端口>)，
# 这里只需返回 app 对象，绝不能再调用 runApp()，也不要写死 host/port。
options(shiny.launch.browser = FALSE)
options(shiny.deprecation.messages = FALSE)

launchSeuratExplorer(
  verbose = FALSE,
  ReductionKeyWords = c("umap", "tsne", "pca"),
  SplitOptionMaxLevel = 12,
  MaxInputFileSize = 30 * 1024^3   # 它内部会设置 shiny.maxRequestSize
)
'

writeLines(app_code, con = file.path(app_dir, "app.R"))

# 3. 写入配置文件 _shinyelectron.yml（嵌套 schema；扁平键会被忽略并告警）
# 版本号与 SeuratExplorer 包联动（安装器版本 = 已安装/将打包的包版本）
# se_version <- tryCatch(
#   as.character(utils::packageVersion("SeuratExplorer")),
#   error = function(e) "1.0.0"
# )
# or specify the version 注意，这个地方需要根据实际情况进行修改。
se_version <- "0.1.8"

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
  help_url: "https://github.com/fentouxungui/SeuratExplorer/wiki"

icons:
  win: "./icons/ico/emerald.ico"

installer:
  app_id: "com.seuratexplorer.desktop"
  one_click: false
  allow_to_change_installation_directory: true

# 代码签名：暂无证书，保持关闭；拿到证书后改 sign: true 并配置
# CSC_LINK / CSC_KEY_PASSWORD 环境变量
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
      - "D:/GitHub_Res/presto-1.1.0.tar.gz"
updates:
  enabled: true
  provider: "github"
  check_on_startup: true
  auto_download: true      # 发现新版后台自动下载；想手动点下载就设 false
  auto_install: true       # 下载完退出时静默安装；想弹窗让用户选就设 false
  github:
    owner: "fentouxungui"
    repo: "SeuratExplorer"

optimize:
  r_library: true
  r_runtime: true
')
writeLines(config_code, con = file.path(app_dir, "_shinyelectron.yml"))



# 设定路径
app_dir <- "C:/Users/Xi_Lab/Desktop/SeuratExplorerApp"
output_dir <- "C:/Users/Xi_Lab/Desktop/SeuratExplorer-Desktop-Build"

# 2. runtime_strategy 已在 _shinyelectron.yml 的 build: 段设置
# 排查启动问题：可先运行 Sys.setenv(SHINYELECTRON_DEBUG = "1")，让后端打印 R 的 stdout/stderr 与实际端口
options(timeout = 3600)
export(
  appdir = app_dir,
  destdir = output_dir,
  app_name = 'SeuratExplorer',
  icon = './icons/ico/emerald.ico',
  run_after = FALSE,
  overwrite = TRUE
)

