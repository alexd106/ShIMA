---
title: "README"
output: html_document
---


# ShIMA 

## Shiny Interface for Metabolite Analysis


The app is hosted on Shinyapps.io here:

<Github>

To run this app locally on your machine, download R or RStudio and run the following commands once to set up the environment:
```
source("http://bioconductor.org/biocLite.R")
biocLite(c("xcms", "multtest", "mzR","AnnotationDbi", "impute", "GO.db", "preprocessCore",))
install.packages(c("rJava", "XML", "snow", "caTools", "bitops", "ptw", "gplots", "tcltk2","WGCNA", "shiny","shinyFiles","shinyBS","BiocParallel","DT","splitstackshape","mixOmics","metabolomics","lattice","Heatplus","minet","igraph","data.table","irlba","qdap","Matrix"))
install.packages("snow",repos="http://cran.r-project.org")
install.packages("doSNOW",repos="http://cran.r-project.org")
install.packages("parallel",repos="http://cran.r-project.org")
install.packages("e1071",repos="http://cran.r-project.org")
install.packages("XML",repos="http://cran.r-project.org")
install.packages("R2HTML",repos="http://cran.r-project.org")
install.packages("RCurl",repos="http://cran.r-project.org")
biocLite("Rdisop",suppressUpdates=TRUE)
biocLite("SSOAP",suppressUpdates=TRUE)
biocLite("KEGGREST",suppressUpdates=TRUE)
install.packages("pcaMethods",repos="http://cran.r-project.org")
install.packages("flashClust",repos="http://cran.r-project.org")
install.packages("plyr",repos="http://cran.r-project.org")
install.packages("png",repos="http://cran.r-project.org")
install.packages("rjson",repos="http://cran.r-project.org")


## installing xMSannotator
### Installation on Windows:
#### 1) Download the "xMSannotator_*.zip" file from the IT box under Rversion2.15.2 or Rversion3.0.3 depending on your R version
#### 2) Open R
#### 3) Click on "Packages" under the menu bar
#### 4) Click on "Install packages from local zip files"
#### 5) Browse to the download location of the "xMSannotator_*.zip" file
#### 6) Double click on the file and installation should begin
#### 7) Run "library(xMSannotator)" command from within R to make sure the package is successfully installed
### xMSannotator installation on Mac:
#### 1) Go to “Applications” 
#### 2) Open R
#### 3) Go to "Packages & Data” 
#### 4) Select "Local Source Package" option from the drop down menu 
#### 5) Click on "Install”
#### 6) Browse to the download location
#### 7) Click "Open"
### OR:
#### 1) Download "xMSannotator_*.tar.gz" file from the IT box under Rversion2.15.2 or Rversion3.0.3 depending on your R version
#### 2) Go to MAC “Utilities” 
#### 3) Click on "Terminal"
#### 4) Browse to the download location of xMSannotator_*.tar.gz
#### 5) Run "R CMD INSTALL xMSannotator_*.tar.gz" to install
#### 6) Run "library(xMSannotator)" command from within R to make sure the package is successfully installed
### xMSannotator installation on Linux:
#### 1) Download "xMSannotator_*.tar.gz" file from the IT box under Rversion2.15.2 or Rversion3.0.3 depending on your R version
#### 2) Browse to the download location of xMSannotator_*.tar.gz
#### 3) Run "R CMD INSTALL xMSannotator_*.tar.gz" to install
#### 4) Run "library(xMSannotator)" command from within R to make sure the package is successfully installed
```

You may now run the shiny app with just one command in R:

```
shiny::runGitHub("MIAP", "AccountName")
```



We would appreciate reports of any issues with the app via the issues option of 
[Github](https://github.com/AccountName/MIAP) or by emailing ---------------.

# Instructions

Instructions can be found here: <https://github.com/AccountName/MIAP/Documentation/Instructions.md> 

# Licensing

This shiny code is licensed under the GPLv3. Please see the file LICENSE.txt for
information.

    MIAP (Metabolites Identification and Annotation Pipeline) is a
    Shiny App for analysis and visualization of metabolomics data.
    Copyright (C) 2017  -----------------

    This program is free software: you can redistribute it and/or modify
    it under the terms of the GNU General Public License as published by
    the Free Software Foundation, either version 3 of the License, or
    (at your option) any later version.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU General Public License for more details.

    You should have received a copy of the GNU General Public License
    along with this program.  If not, see <http://www.gnu.org/licenses/>.

    You may contact the author of this code, --------------, at <--------------->
    



