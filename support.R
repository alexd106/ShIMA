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
