# check if required packages are installed. If not install.
if(!require(shiny)){
  install.packages("shiny", dep=TRUE, quiet=TRUE)
  library(shiny, quietly=TRUE)
}
if(!require(shinyFiles)){
  install.packages("shinyFiles", dep=TRUE, quiet=TRUE)
  library(shinyFiles, quietly=TRUE)
}
if(!require(shinyBS)){
  install.packages("shinyBS", dep=TRUE, quiet=TRUE)
  library(shinyBS, quietly=TRUE)
}
if(!require(BiocParallel)){
  install.packages("BiocParallel", dep=TRUE, quiet=TRUE)
  library(BiocParallel, quietly=TRUE)
}
if(!require(xcms)){
  install.packages("xcms", dep=TRUE, quiet=TRUE)
  library(xcms, quietly=TRUE)
}
if(!require(DT)){
  install.packages("DT", dep=TRUE, quiet=TRUE)
  library(DT, quietly=TRUE)
}
if(!require(d3heatmap)){
  install.packages("d3heatmap", dep=TRUE, quiet=TRUE)
  library(d3heatmap, quietly=TRUE)
}
if(!require(shinyjs)){
  install.packages("shinyjs", dep=TRUE, quiet=TRUE)
  library(shinyjs, quietly=TRUE)
}
if(!require(rmarkdown)){
  install.packages("rmarkdown", dep=TRUE, quiet=TRUE)
  library(rmarkdown, quietly=TRUE)
}
if(!require(corrplot)){
  install.packages("corrplot", dep=TRUE, quiet=TRUE)
  library(corrplot, quietly=TRUE)
}
if(!require(calibrate)){
  install.packages("calibrate", dep=TRUE, quiet=TRUE)
  library(calibrate, quietly=TRUE)
}
if(!require(knitr)){
  install.packages("knitr", dep=TRUE, quiet=TRUE)
  library(knitr, quietly=TRUE)
}
if (!require(xMSannotator)) {
  install.packages("xMSannotator", dep = TRUE, quiet = TRUE)
  library(xMSannotator, quietly = TRUE)
}
if (!require(BiocParallel)) {
  install.packages("BiocParallel", dep = TRUE, quiet = TRUE)
  library(BiocParallel, quietly = TRUE)
}
if (!require(splitstackshape)) {
  install.packages("splitstackshape", dep = TRUE, quiet = TRUE)
  library(splitstackshape, quietly = TRUE)
}
if (!require(metabolomics)) {
  install.packages("metabolomics", dep = TRUE, quiet = TRUE)
  library(metabolomics, quietly = TRUE)
}
if (!require(lattice)) {
  install.packages("lattice", dep = TRUE, quiet = TRUE)
  library(lattice, quietly = TRUE)
}
if (!require(Heatplus)) {
  install.packages("Heatplus", dep = TRUE, quiet = TRUE)
  library(Heatplus, quietly = TRUE)
}
if (!require(mixOmics)) {
  install.packages("mixOmics", dep = TRUE, quiet = TRUE)
  library(mixOmics, quietly = TRUE)
}
if (!require(d3heatmap)) {
  install.packages("d3heatmap", dep = TRUE, quiet = TRUE)
  library(d3heatmap, quietly = TRUE)
}
if (!require(metabolomics)) {
  install.packages("metabolomics", dep = TRUE, quiet = TRUE)
  library(metabolomics, quietly = TRUE)
}
if (!require(lattice)) {
  install.packages("lattice", dep = TRUE, quiet = TRUE)
  library(lattice, quietly = TRUE)
}
if (!require(Heatplus)) {
  install.packages("Heatplus", dep = TRUE, quiet = TRUE)
  library(Heatplus, quietly = TRUE)
}
if (!require(mixOmics)) {
  install.packages("mixOmics", dep = TRUE, quiet = TRUE)
  library(mixOmics, quietly = TRUE)
}
if (!require(minet)) {
  install.packages("minet", dep = TRUE, quiet = TRUE)
  library(minet, quietly = TRUE)
}
if (!require(igraph)) {
  install.packages("igraph", dep = TRUE, quiet = TRUE)
  library(igraph, quietly = TRUE)
}
if (!require(data.table)) {
  install.packages("data.table", dep = TRUE, quiet = TRUE)
  library(data.table, quietly = TRUE)
}
if (!require(irlba)) {
    install.packages("irlba", dep = TRUE, quiet = TRUE)
    library(irlba, quietly = TRUE)
}
if (!require(qdap)) {
  install.packages("qdap", dep = TRUE, quiet = TRUE)
  library(qdap, quietly = TRUE)
}
if (!require(Matrix)) {
  install.packages("Matrix", dep = TRUE, quiet = TRUE)
  library(Matrix, quietly = TRUE)
}
