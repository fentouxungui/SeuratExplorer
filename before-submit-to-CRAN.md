# SeuratExplorer：提交 CRAN 前的完整检查清单

> 适用对象：`SeuratExplorer` R 包（非首次提交）。
> 目标：生成一个只包含 `DESCRIPTION`、`NAMESPACE`、`R/`、`man/` 的 CRAN 源码包，并通过
> `R CMD check --as-cran` 的 ERROR / WARNING / NOTE 检查。
>
> **重要原则**
> 1. 不要直接在 GitHub 开发仓库里做 CRAN 裁剪；先复制一份独立的 CRAN 工作副本。
> 2. 不要复用已经被 CRAN 拒绝过的版本号；每次提交都必须递增。
> 3. 优先使用 `.Rbuildignore` 排除开发文件；如果手工压缩目录，则必须物理删除这些文件。
> 4. 本文件只保留在开发仓库中，不要放进 CRAN 源码包。

---

## 0. 准备 CRAN 工作副本

建议目录结构：

```text
D:\GitHub_Res\
├── SeuratExplorer\                         # GitHub 开发仓库（保留 README、wiki、inst 等）
└── CRAN\
    └── SeuratExplorer_<version>\
        └── SeuratExplorer\                 # 本次 CRAN 提交副本
```

操作步骤：

- [ ] 确认开发仓库工作区干净：

```bash
git status
```

- [ ] 查看 CRAN 当前版本，确定新版本号：

```text
https://cran.r-project.org/package=SeuratExplorer
```

- [ ] 新版本号必须大于 CRAN 当前版本，并避免使用被拒绝过的版本号。
- [ ] 复制开发仓库到 `CRAN/SeuratExplorer_<version>/SeuratExplorer`。
- [ ] 后续所有删除、修改、检查都在这个 CRAN 副本中进行。

---

## 1. 修改 `DESCRIPTION`

### 1.1 版本号

```dcf
Version: 0.1.10
```

- [ ] 版本号已递增（CRAN 当前为 0.1.8；本次目标 0.1.10）。
- [ ] 没有重复使用旧版本号。
- [ ] 下次提交时改成更大的版本号。

### 1.2 删除 CRAN 不接受的字段

删除以下字段：

```dcf
Remotes:
    bioc::release/ComplexHeatmap,
    bioc::release/MAST,
    bioc::release/limma,
    bioc::release/DESeq2,
    bioc::release/AUCell

LazyData: true
```

原因：

- `Remotes` 不是 CRAN 源码包字段，CRAN 构建时不会使用它。
- 包内已经删除 `data/`，因此 `LazyData` 不再需要。

### 1.3 调整 `Imports`

删除 About 页面后，不再需要：

```dcf
    knitr,
    markdown
```

- [ ] `Imports` 中已移除 `knitr`、`markdown`。
- [ ] `NAMESPACE` 中已移除 `import(knitr)`、`import(markdown)`。
- [ ] `R/` 中已移除 `knitr::`、`markdown::` 调用。
- [ ] roxygen 注释中的 `@import ... knitr` / `@import ... markdown` 已删除。

### 1.4 调整 `Suggests`

CRAN / Bioconductor 仓库中的包可以放在 `Suggests` 中；GitHub-only 包不要放。

- [ ] 保留 `presto`：`presto` 已被 CRAN 收录，可以放在 `Suggests` 中。
- [ ] 保留 Bioconductor 包：`ComplexHeatmap`、`MAST`、`limma`、`DESeq2`、`AUCell`、`Signac`。
- [ ] 如果还有其他 GitHub-only 依赖，从 `Suggests` 删除，并删除代码中的 `requireNamespace()` / `::` 调用。
- [ ] `presto` 的安装提示应使用 `install.packages("presto")`，不要再用 GitHub 安装方式。

> 注意：`presto` 已在 CRAN 后，`requireNamespace("presto")` 是合法条件依赖，
> 不需要删除 `presto_missing_ui()`；只要 `Suggests` 中包含 `presto` 即可。

### 1.5 补充 DOI 和链接

建议：

```dcf
URL: https://CRAN.R-project.org/package=SeuratExplorer,
     https://github.com/fentouxungui/SeuratExplorer,
     https://doi.org/10.32614/CRAN.package.SeuratExplorer
BugReports: https://github.com/fentouxungui/SeuratExplorer/issues
```

- [ ] `URL` 中包含 CRAN 页面、GitHub 和 CRAN DOI。
- [ ] `BugReports` 可访问。
- [ ] `Authors@R` 的邮箱、角色 `aut` / `cre` 正确。
- [ ] `License: GPL (>= 3)` 与仓库许可证一致。

### 1.6 最终 `DESCRIPTION` 参考

```dcf
Package: SeuratExplorer
Title: An 'Shiny' App for Exploring scRNA-seq Data Processed in 'Seurat'
Version: <new-version>
Authors@R:
  person("Yongchao", "Zhang", , "zhangyongchao@nibs.ac.cn",
         role = c("aut", "cre"),
         comment = c("https://github.com/fentouxungui"))
Description: A simple, one-command package which runs an interactive dashboard
    capable of common visualizations for single cell RNA-seq. 'SeuratExplorer'
    requires a processed 'Seurat' object, which is saved as 'rds' or 'qs2' file.
License: GPL (>= 3)
URL: https://CRAN.R-project.org/package=SeuratExplorer,
    https://github.com/fentouxungui/SeuratExplorer,
    https://doi.org/10.32614/CRAN.package.SeuratExplorer
BugReports: https://github.com/fentouxungui/SeuratExplorer/issues
Encoding: UTF-8
Roxygen: list(markdown = TRUE)
Imports:
    utils,
    tools,
    stats,
    methods,
    curl,
    Seurat (>= 5.4.0),
    SeuratObject (>= 5.3.0),
    ggplot2 (>= 4.0.1),
    shinydashboard,
    DT,
    shinycssloaders,
    colourpicker,
    shinyWidgets,
    scales,
    shinyjqui,
    shinyBS,
    ggalluvial,
    dplyr,
    qs2,
    circlize,
    reshape2,
    shinyjs,
    later,
    rlang
Depends:
    R (>= 4.1.0),
    shiny
Suggests:
    ComplexHeatmap,
    MAST,
    limma,
    DESeq2,
    AUCell,
    presto,
    Signac
Config/roxygen2/version: 8.1.0
```

> `Config/roxygen2/version` 可以保留；运行 `devtools::document()` 后 roxygen2 可能会补充
> `RoxygenNote`，这是正常的。

---

## 2. 删除 CRAN 源码包中不应出现的文件/目录

### 2.1 必须删除（或通过 `.Rbuildignore` 排除）

| 路径 | 原因 |
|---|---|
| `before-submit-to-CRAN.md` | 本检查清单，仅用于开发仓库 |
| `CITATION.cff` | GitHub 引用元数据，CRAN 不需要 |
| `README.md` | 包根目录 README，CRAN 源码包不需要 |
| `README.Rmd` | README 源文件，CRAN 源码包不需要 |
| `LICENSE.md` | `License: GPL (>= 3)` 已足够；CRAN 不需要完整 GPL 文本 |
| `inst/` | 主要包含 About 页面 README 和截图；About 删除后无用途 |
| `data/` | 包含 `GenesDB.rda`（约 2.7 MB），CRAN 包体积和外部链接功能不再需要 |
| `R/data.R` | `GenesDB` 数据文档 |
| `man/GenesDB.Rd` | `R/data.R` 对应的生成文档，必须一起删除 |
| `tutorials/` | `Prepare-GenesDB.Rmd` 等开发教程 |
| `Build-Desktop-software-by-shinyelectron.R` | Electron 桌面构建脚本 |
| `icons/` | 桌面应用图标资源 |
| `wiki/` | GitHub Wiki 源文件 |
| `dependency/` | 本地 `presto` tarball、桌面构建依赖 |
| `.github/` | GitHub Actions / CI 配置 |
| `.Rproj.user/` | RStudio 本地状态；RStudio 打开项目时会自动重建，删除前先关闭 RStudio |
| `SeuratExplorer.Rproj` | RStudio 项目文件；可保留，但必须通过 `.Rbuildignore` 排除 |
| `.gitattributes` | Git 配置 |
| `.gitignore` | Git 配置 |
| `.Rhistory` | R 历史记录（如果存在） |
| `.RData` / `.Ruserdata` | R 会话数据（如果存在） |
| `.git/` | 版本控制目录，绝不能进入源码包 |

### 2.2 可以保留

| 路径 | 说明 |
|---|---|
| `DESCRIPTION` | 必须保留 |
| `NAMESPACE` | 必须保留 |
| `R/` | 必须保留 |
| `man/` | 必须保留 |
| `.Rbuildignore` | 建议保留用于构建；它本身应被 `.Rbuildignore` 排除 |

### 2.3 推荐 `.Rbuildignore`

如果不想物理删除，至少保证 `.Rbuildignore` 包含：

```text
^SeuratExplorer\.Rproj$
^\.Rproj\.user$
^\.Rbuildignore$
^\.gitattributes$
^\.gitignore$
^\.github$
^wiki$
^dependency$
^icons$
^LICENSE\.md$
^README\.md$
^README\.Rmd$
^CITATION\.cff$
^before-submit-to-CRAN\.md$
^Build-Desktop-software-by-shinyelectron\.R$
^\.Rhistory$
^\.RData$
```

> `R CMD build` 会自动排除 `.git/`，不需要写进 `.Rbuildignore`。

### 2.4 最终源码包应只包含

```text
SeuratExplorer/
├── DESCRIPTION
├── NAMESPACE
├── R/
│   ├── functions.R
│   ├── launch.R
│   ├── server.R
│   └── ui.R
├── man/
│   └── *.Rd
└── MD5            # 由 R CMD build 自动生成
```

- [ ] `R CMD build` 生成的 tarball 中没有 `inst/`、`data/`、`README*`、`wiki/`、`dependency/`、`icons/`、`.github/`。
- [ ] tarball 中没有 `.git/`、`.Rproj.user/`、`SeuratExplorer.Rproj`。

---

## 3. 删除 About 页面和 GenesDB 外部链接功能

### 3.1 `R/ui.R`

- [ ] 删除侧边栏 About 菜单项：

```r
menuSubItem(text = "About", tabName = "about", icon = shiny::icon("angle-right"))
```

- [ ] 删除 `explorer_body_ui()` 末尾的 About 页面：

```r
tab_list[["about"]] = tabItem(
  tabName = "about",
  fluidRow(
    box(
      title = "About Seurat Explorer",
      solidHeader = TRUE,
      status = "primary",
      width = 12,
      withSpinner(uiOutput("about_page"))
    )
  )
)
```

- [ ] 删除 DEGs 页面右侧 External Links 面板，包括：
  - `selectInput("selectspecies", ...)`
  - `selectInput("selectsgenetype", ...)`
  - `uiOutput("ExternalLinks.UI")`
- [ ] 将 DEGs 结果区域从 `col-md-8` 改为 `col-md-12`，避免留下空白列。
- [ ] 删除 roxygen 中的 `knitr` / `markdown`：

```r
#' @import shiny
#' @import shinyBS
#' @import shiny shinyWidgets shinyBS
```

### 3.2 `R/server.R`

- [ ] 删除 About 页面渲染：

```r
output$about_page <- renderUI({ ... })
```

- [ ] 删除 GenesDB 数据和外部链接：

```r
db <- SeuratExplorer::GenesDB
output$ExternalLinks.UI <- renderUI({ ... })
```

- [ ] 删除只服务于 External Links 的 `DEGs_row_selected` 及其 `outputOptions()`。
- [ ] 更新 Top Genes 提示文字，删除 “see the About page for details” 之类引用。
- [ ] 确认 `R/` 中不再有 `GenesDB`、`ExternalLinks.UI`、`about_page`、`selectspecies`、`selectsgenetype`。

### 3.3 删除数据文件

```text
R/data.R
data/GenesDB.rda
man/GenesDB.Rd
```

- [ ] 三个文件都已删除。
- [ ] `DESCRIPTION` 中已删除 `LazyData: true`。
- [ ] `R/` 中不再有 `SeuratExplorer::GenesDB`。

### 3.4 处理 `presto` 提示

`presto` 已被 CRAN 收录，因此：

- [ ] `Suggests` 中保留 `presto`。
- [ ] 保留 `R/functions.R` 中的 `presto_missing_ui()` 和 `R/server.R` 中的调用。
- [ ] 安装提示使用 `install.packages("presto")`，不要再用 GitHub 安装方式。
- [ ] `grepl("presto", ...)` 仅用于屏蔽 Seurat 的控制台提示，可以保留。

---

## 4. 重新生成文档和 `NAMESPACE`

推荐先运行 roxygen2，避免手工修改 `NAMESPACE` 与 `man/` 不一致：

```r
devtools::document()
```

- [ ] `NAMESPACE` 已重新生成。
- [ ] `man/` 中没有 `GenesDB.Rd`。
- [ ] `man/` 中每个导出函数都有对应文档。
- [ ] `NAMESPACE` 中不再有 `import(knitr)`、`import(markdown)`。
- [ ] `DESCRIPTION` 中没有 `Remotes`、`LazyData`。
- [ ] `Suggests` 中包含 `presto`（presto 已在 CRAN）。

检查导出函数与文档是否一一对应：

```r
ns <- readLines("NAMESPACE")
exports <- sub("^export\\((.*)\\)$", "\\1", grep("^export\\(", ns, value = TRUE))
rd <- sub("\\.Rd$", "", list.files("man", pattern = "\\.Rd$"))
setdiff(exports, rd)   # 应为空
```

---

## 5. 构建和检查

### 5.1 构建源码包

在包目录（包含 `DESCRIPTION` 的目录）运行：

```bash
R CMD build .
```

或 RStudio：

```text
Build -> More -> Build Source Package
```

生成：

```text
SeuratExplorer_<new-version>.tar.gz
```

### 5.2 检查 tarball 内容

Windows 可用：

```bash
tar -tzf SeuratExplorer_<new-version>.tar.gz
```

确认只包含：

```text
SeuratExplorer/DESCRIPTION
SeuratExplorer/NAMESPACE
SeuratExplorer/R/...
SeuratExplorer/man/...
SeuratExplorer/MD5
```

如果出现以下内容，说明 `.Rbuildignore` 或删除步骤有问题：

```text
inst/
data/
README.md
README.Rmd
wiki/
dependency/
icons/
.github/
.Rproj.user/
SeuratExplorer.Rproj
```

### 5.3 运行 CRAN 检查

```bash
R CMD check --as-cran SeuratExplorer_<new-version>.tar.gz
```

或：

```r
devtools::check(manual = FALSE, vignettes = FALSE, cran = TRUE)
```

- [ ] 0 ERROR
- [ ] 0 WARNING
- [ ] 尽量 0 NOTE

### 5.4 常见 NOTE 及处理

| NOTE / 问题 | 处理 |
|---|---|
| `Non-standard file/directory found at top level` | 删除该文件/目录，或加入 `.Rbuildignore` |
| `Remotes` 字段 | 从 `DESCRIPTION` 删除 |
| `LazyData` 但无 `data/` | 删除 `LazyData` |
| `Namespace in Imports field not imported from` | 删除未使用依赖，或补充 `::` / `@import` |
| `'library' or 'require' call not declared from` | 将包加入 `Suggests`；CRAN 不可用的包则删除对应代码 |
| `Suggests or Enhances not in mainstream repositories` | 删除 GitHub-only `Suggests`；Bioconductor 包和已上 CRAN 的 `presto` 可保留 |
| `Undocumented code objects` | 运行 `devtools::document()`，补充 roxygen |
| `Undefined global functions or variables` | 检查 `utils::globalVariables()` 或函数内变量 |
| `no visible global function definition for 'ave'` | 在 roxygen 中加 `@importFrom stats ave`，再运行 `devtools::document()` |
| `no visible global function definition for 'object.size'` | 在 roxygen 中加 `@importFrom utils object.size`，再运行 `devtools::document()` |
| `code files for non-ASCII characters` | 用 `tools::showNonASCIIfile()` 定位；字符串中的 `·` 改为 `\u00b7` 转义，中文标点改为 ASCII |
| `Rd \usage lines wider than 90 characters` | 把长默认值移出函数签名（例如默认 `NULL`，在函数体内赋值） |
| Examples 运行失败 | 使用 `if (interactive())` 或 `\dontrun{}` 包裹需要交互/外部数据的示例 |
| 包体积过大 | 删除 `data/`、`inst/` 中的大文件；CRAN 通常限制 5 MB 左右 |

> 本地如果缺少 Bioconductor `Suggests`，可临时设置：
>
> ```bash
> _R_CHECK_FORCE_SUGGESTS_=false R CMD check --as-cran SeuratExplorer_<new-version>.tar.gz
> ```
>
> 但提交前应尽量在装有 Bioconductor 依赖的环境中再检查一次。

---

## 6. 提交 CRAN

### 6.1 提交方式

任选一种：

```r
devtools::submit_cran()
```

或：

```text
https://cran.r-project.org/submit.html
```

- [ ] 上传的是 `R CMD build` 生成的 `.tar.gz`，不是整个目录的 zip。
- [ ] 版本号高于 CRAN 当前版本。
- [ ] 提交说明中写明：
  - 本次主要变更；
  - 已通过 `R CMD check --as-cran`；
  - 如有 Bioconductor `Suggests`，说明已在相应环境测试；
  - 如有 NOTE，说明原因和处理方式。

### 6.2 CRAN 反馈

- [ ] 关注 maintainer 邮箱。
- [ ] 如果被拒绝，不要重复提交同一版本号；修复后递增版本号。
- [ ] 如果 CRAN 要求删除某些依赖/文件，按邮件要求处理并重新检查。

---

## 7. 接收后同步开发仓库

CRAN 接收后：

- [ ] 在 GitHub 打 tag：`v<new-version>`。
- [ ] 更新 `README.md` / `README.Rmd` 中的版本徽章。
- [ ] 更新 `CITATION.cff` 或引用信息。
- [ ] 确认 CRAN DOI 可访问：`https://doi.org/10.32614/CRAN.package.SeuratExplorer`。
- [ ] 将 CRAN 副本中的必要修复同步回 GitHub 开发仓库（但保留 README、wiki、inst、data 等开发文件）。
- [ ] 更新本检查清单，记录本次遇到的问题。

---

## 8. 一键检查命令（参考）

```bash
# 1. 进入 CRAN 副本
cd /d/GitHub_Res/CRAN/SeuratExplorer_<version>/SeuratExplorer

# 2. 查看不应进入包的文件
find . -maxdepth 1 -mindepth 1 | sort

# 3. 重新生成文档
Rscript -e "devtools::document()"

# 4. 构建
R CMD build .

# 5. 查看 tarball 内容
tar -tzf SeuratExplorer_<version>.tar.gz

# 6. CRAN 检查
R CMD check --as-cran SeuratExplorer_<version>.tar.gz
```

---

## 9. 提交前最终勾选

- [ ] `DESCRIPTION` 版本号已递增
- [ ] 无 `Remotes`
- [ ] 无 `LazyData`
- [ ] `Imports` 无 `knitr` / `markdown`
- [ ] `Suggests` 中的包均可在 CRAN / Bioconductor 获取（`presto` 已在 CRAN）
- [ ] `URL` 包含 CRAN DOI
- [ ] About 页面已删除
- [ ] `GenesDB` / External Links 已删除
- [ ] `R/data.R`、`data/`、`man/GenesDB.Rd` 已删除
- [ ] `inst/`、`README.md`、`README.Rmd`、`LICENSE.md`、`CITATION.cff` 已删除
- [ ] `icons/`、`wiki/`、`dependency/`、`.github/` 已删除或排除
- [ ] `.Rproj.user/`、`SeuratExplorer.Rproj`、`.Rhistory` 已删除或排除
- [ ] `devtools::document()` 已运行
- [ ] `R CMD build` 成功
- [ ] tarball 内容符合白名单
- [ ] `R CMD check --as-cran` 无 ERROR / WARNING
- [ ] 已通过 CRAN 网页或 `devtools::submit_cran()` 提交

---



---

## 10. 本次 0.1.10 的 R CMD check 经验（2026-11）

本次 `devtools::check()` 结果为：

- 0 ERROR
- 1 WARNING：`code files for non-ASCII characters`
- 2 NOTES：`ave` / `object.size` 未定义、`launchSeuratExplorer.Rd` 的 `\usage` 行过宽

### 10.1 非 ASCII 字符 WARNING

检查信息：

```text
Found the following files with non-ASCII characters:
  R/functions.R
  R/server.R
```

定位：

```r
for (f in list.files("R", pattern = "\\.R$", full.names = TRUE)) {
  tools::showNonASCIIfile(f)
}
```

本次问题：

- `R/functions.R` 注释中的中文逗号 `，`
- `R/functions.R` 和 `R/server.R` 字符串中的 `·`（U+00B7）

修复：

- 注释中的中文标点改为 ASCII：`，` -> `,`
- R 字符串中的 `·` 改为 ASCII 转义：

```r
"  \u00b7  "
```

> 注意：在 R 源码里要写成反斜杠 + `u00b7`，不是直接写 `·`。

修复后再次扫描，确认 `R/` 和 `NAMESPACE` 没有任何非 ASCII 字符。

### 10.2 `ave` / `object.size` NOTE

检查信息：

```text
explorer_server: no visible global function definition for 'ave'
explorer_server: no visible global function definition for 'object.size'
```

原因：`explorer_server()` 中使用了 `ave()` 和 `object.size()`，但没有在 NAMESPACE 中导入。

修复：在 `R/server.R` 的 `explorer_server` roxygen 块中加入：

```r
#' @importFrom stats na.omit setNames ave
#' @importFrom utils str head object.size
```

然后重新生成：

```r
devtools::document()
```

`NAMESPACE` 应出现：

```r
importFrom(stats,
  ave,
  na.omit,
  setNames
)
importFrom(utils,
  head,
  object.size,
  str,
  write.csv
)
```

> 只改 `NAMESPACE` 不够；必须同时改 roxygen 注释，否则下次 `devtools::document()` 会覆盖。

### 10.3 Rd `\usage` 行宽 NOTE

检查信息：

```text
Rd file 'launchSeuratExplorer.Rd':
  \usage lines wider than 90 characters
```

原因：`launchSeuratExplorer()` 的 `DemoDataURL` 默认值直接写在函数签名中，三个长 URL 导致 `\usage` 行超过 90 字符。

修复：

1. 函数签名改为：

```r
DemoDataURL = NULL,
```

2. 在函数体开头设置默认值，保留原来的默认行为：

```r
if (missing(DemoDataURL)) {
  DemoDataURL <- c(
    "https://raw.githubusercontent.com/...",
    "https://drive.usercontent.google.com/...",
    "https://www.daokedao.site/..."
  )
}
```

3. 重新生成文档：

```r
devtools::document()
```

4. 确认 `man/launchSeuratExplorer.Rd` 的 `\usage` 中只有：

```text
  DemoDataURL = NULL,
```

> 长 URL 放在函数体内，不要放在函数签名默认值中。

### 10.4 修复后复检

```r
devtools::check(manual = FALSE, vignettes = FALSE, cran = TRUE)
```

目标：

- 0 ERROR
- 0 WARNING
- 0 NOTE

如果仍有 NOTE，查看：

```text
SeuratExplorer.Rcheck/00check.log
```

### 10.5 本次经验总结

1. **先处理非 ASCII，再 build/check**：非 ASCII 是 WARNING，会直接让 `devtools::check()` 失败。
2. **`@importFrom` 与 NAMESPACE 必须同步**：修改 roxygen 后运行 `devtools::document()`。
3. **长默认值不要写在函数签名里**：默认值用 `NULL`，在函数体内赋值。
4. **`presto` 已上 CRAN**：保留在 `Suggests`，不需要 `Remotes`，也不需要在 CRAN 副本中删除 `presto_missing_ui()`。
5. **检查前先删除/排除开发文件**：`icons/`、`wiki/`、`dependency/`、`.github/`、`.Rproj.user/` 等不能进入源码包。

> 以上非 ASCII、`ave` / `object.size` 和 `launchSeuratExplorer.Rd` 行宽修复已同步到 GitHub 开发仓库；下次复制到 CRAN 副本时不需要重复修改。

**维护提醒**：建议每两个月检查一次 CRAN 版本和依赖状态；每次提交前复制本清单到 CRAN 副本的上一级目录，完成后从源码包中删除。
