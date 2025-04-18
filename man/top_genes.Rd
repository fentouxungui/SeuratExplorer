% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/functions.R
\name{top_genes}
\alias{top_genes}
\title{Find Top Genes by Cell}
\usage{
top_genes(SeuratObj, expr.cut = 0.01, group.by)
}
\arguments{
\item{SeuratObj}{Seurat object}

\item{expr.cut}{UMI percentage cutoff, in a cell, if a gene with UMIs ratio more than this cutoff, this gene will be assigned to highly expressed gene for this cell}

\item{group.by}{how to group cells}
}
\value{
a data frame
}
\description{
for each cell, find genes that has high UMI percentage, for example, if a cell has 10000 UMIs, and the UMI percentage cutoff is set to 0.01,
then all genes that has more than 10000 * 0.01 = 100 UMIs is thought to be the highly expressed genes for this cell.summary those genes for each cluster,
firstly get all highly expressed genes in a cluster, some genes may has less cells, then for each gene, count cells in which this genes is highly expressed,
and also calculate the mean and median UMI percentage in those highly expressed cells.
}
