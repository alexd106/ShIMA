---
title: "README"
output: html_document
---


# ShIMA 


<Github>

To run this app locally on your machine, download R or RStudio and install the following packages. While trying to run ShIMA the very first time, it will try to check and install all the packages needed for the analysis except xMSannotator. But there is a possiblity to have issues with installations. There are instructions below which can also be followed for simple installation of the packages needed. However, a few packages can be little tricky as all of the packages involved in this projects are not CRAN based. If any of the packages are not installed or loaded properly, there will be the basic error which can be seen in the R console and may need little troubleshooting. 

Run the following commands once to set up the environment:
```
source("http://bioconductor.org/biocLite.R")
biocLite(c("xcms", "multtest", "mzR","AnnotationDbi", "impute", "GO.db", "preprocessCore",
           "pcaMethods","Heatplus"))
install.packages(c("rJava", "XML", "snow", "caTools", "bitops", "ptw", "gplots", "tcltk2","WGCNA", 
                   "shiny","shinyFiles","shinyBS","BiocParallel","DT","splitstackshape","mixOmics",
                   "metabolomics","lattice","minet","igraph","data.table","irlba","qdap","Matrix"))
install.packages("snow",repos="http://cran.r-project.org")
install.packages("doSNOW",repos="http://cran.r-project.org")
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
install.packages("RANN",repos="http://cran.r-project.org")
```

For the installation of the package xMSannotator, you can go through the instructions below or you 
may want to have a look at 
<https://sourceforge.net/projects/xmsannotator/files/Installation_instructions_xMSannotator.txt/download>. 
We have also kept a zip versions of xMSannotator at <https://github.com/alexd106/ShIMA/tree/master/Install/> 
installation which is highly recommended for the version control in ShIMA.
The example dataset can be found at <https://github.com/alexd106/ShIMA/blob/master/Examples.zip>.

The installation procedures are little different in case of different operating systems. 
Go to <https://github.com/alexd106/ShIMA/tree/master/Install/> and download the appropriate file (zip for Windows and tar.gz for Linux and Mac). To download use the download button on Github.

On Windows:

In the R console, go to the option "Install packages from local zip files" under the "Packages" menu. 
Then browse the zip installation file which has been downloaded from Github and run.

On Mac:

In mac, open R and  under the "Packages & Data" there is "Local Source Package" which can be used 
to install with browsing the tar.gz installation file using the option "Install".

On Linux:

Linux also accepts the tar.gz installation file. It can be done by simply going to the directory 
which contains the installation file and then running "R CMD INSTALL xMSannotator_*.tar.gz" to install.


You may now run the shiny app with just one command in R:

shiny::runGitHub("ShIMA", "alexd106")


We would appreciate reports of any issues with the app via the issues option of 
Github <https://github.com/alexd106/ShIMA/issues>.

# Instructions

Instructions how to use can be found here: <https://github.com/alexd106/ShIMA/blob/master/Documentation.md>

A run through tutorial using example data can be found here: <https://github.com/alexd106/ShIMA/blob/master/Tutorial.md>

ShIMA has been tested to be working fine with the following:

### Operating systems: 

Linux and Windows

### R version: 

3.3.0 to 3.4.1

### Java version: 

The java version installed on the system should be the same version used by R.

### Extra: 

- Windows use Pandoc for rendering the markdown files, so it should be preinstalled.

- While browsing for the directories on ShIMA interface click the arrow just before the directory name showed to access the inside contents. However, they can always be visible on the right panel once any directory is selected in the left panel (right panel doesn't allow to select).

R package versions:

R version 3.4.1 (2017-06-30)
Platform: x86_64-pc-linux-gnu (64-bit)
Running under: Ubuntu 14.04.5 LTS

Matrix products: default
BLAS: /usr/lib/openblas-base/libblas.so.3
LAPACK: /usr/lib/lapack/liblapack.so.3.0

locale:
 [1] LC_CTYPE=en_GB.UTF-8          LC_NUMERIC=C                 
 [3] LC_TIME=en_GB.UTF-8           LC_COLLATE=en_GB.UTF-8       
 [5] LC_MONETARY=en_GB.UTF-8       LC_MESSAGES=en_GB.UTF-8      
 [7] LC_PAPER=en_GB.UTF-8          LC_NAME=en_GB.UTF-8          
 [9] LC_ADDRESS=en_GB.UTF-8        LC_TELEPHONE=en_GB.UTF-8     
[11] LC_MEASUREMENT=en_GB.UTF-8    LC_IDENTIFICATION=en_GB.UTF-8

attached base packages:
[1] parallel  stats     graphics  grDevices utils     datasets  methods  
[8] base     

other attached packages:
 [1] RANN_2.5.1             qdap_2.2.5             RColorBrewer_1.1-2    
 [4] qdapTools_1.3.3        qdapRegex_0.7.2        qdapDictionaries_1.0.6
 [7] irlba_2.2.1            Matrix_1.2-11          igraph_1.1.2          
[10] minet_3.34.0           mixOmics_6.2.0         ggplot2_2.2.1         
[13] Heatplus_2.22.0        lattice_0.20-35        metabolomics_0.1.4    
[16] gplots_3.0.1           crmn_0.0.20            limma_3.32.2          
[19] splitstackshape_1.4.2  data.table_1.10.4      xMSannotator_1.3.1    
[22] Rdisop_1.36.0          RcppClassic_0.9.6      pcaMethods_1.68.0     
[25] flashClust_1.01-2      KEGGREST_1.16.0        plyr_1.8.4            
[28] rjson_0.2.15           png_0.1-7              SSOAP_0.8-0           
[31] doSNOW_1.0.14          snow_0.4-2             iterators_1.0.8       
[34] foreach_1.4.3          RCurl_1.95-4.8         bitops_1.0-6          
[37] WGCNA_1.61             fastcluster_1.1.24     dynamicTreeCut_1.63-1 
[40] R2HTML_2.3.2           XML_3.98-1.9           knitr_1.17            
[43] calibrate_1.7.2        MASS_7.3-47            corrplot_0.77         
[46] rmarkdown_1.6          shinyjs_0.9.1          d3heatmap_0.6.1.1     
[49] DT_0.2                 xcms_1.52.0            MSnbase_2.2.0         
[52] ProtGenerics_1.8.0     mzR_2.10.0             Rcpp_0.12.12          
[55] Biobase_2.36.2         BiocGenerics_0.22.0    BiocParallel_1.10.1   
[58] shinyBS_0.61           shinyFiles_0.6.2       shiny_1.0.5           

loaded via a namespace (and not attached):
  [1] backports_1.1.0        Hmisc_4.0-3            lazyeval_0.2.0        
  [4] splines_3.4.1          openNLP_0.2-6          robust_0.4-18         
  [7] digest_0.6.12          BiocInstaller_1.26.1   htmltools_0.3.6       
 [10] gender_0.5.1           GO.db_3.4.1            gdata_2.18.0          
 [13] magrittr_1.5           checkmate_1.8.3        memoise_1.1.0         
 [16] xlsx_0.5.7             fit.models_0.5-14      tm_0.7-1              
 [19] cluster_2.0.6          doParallel_1.0.10      Biostrings_2.44.1     
 [22] wordcloud_2.5          matrixStats_0.52.2     colorspace_1.3-2      
 [25] blob_1.1.0             rrcov_1.4-3            dplyr_0.7.2           
 [28] jsonlite_1.5           bindr_0.1              impute_1.50.1         
 [31] glue_1.1.1             survival_2.41-3        gtable_0.2.0          
 [34] zlibbioc_1.22.0        XVector_0.16.0         XMLSchema_0.7-2       
 [37] DEoptimR_1.0-8         scales_0.5.0           vsn_3.44.0            
 [40] mvtnorm_1.0-6          DBI_0.7                miniUI_0.1.1          
 [43] plotrix_3.6-6          xtable_1.8-2           htmlTable_1.9         
 [46] foreign_0.8-69         bit_1.1-12             preprocessCore_1.38.1 
 [49] Formula_1.2-2          stats4_3.4.1           htmlwidgets_0.9       
 [52] httr_1.3.1             acepack_1.4.1          rJava_0.9-8           
 [55] openNLPdata_1.5.3-2    pkgconfig_2.0.1        venneuler_1.1-0       
 [58] nnet_7.3-12            RJSONIO_1.3-0          labeling_0.3          
 [61] reshape2_1.4.2         rlang_0.1.2            AnnotationDbi_1.38.1  
 [64] munsell_0.4.3          tools_3.4.1            RSQLite_2.0           
 [67] evaluate_0.10.1        stringr_1.2.0          yaml_2.1.14           
 [70] mzID_1.14.0            bit64_0.9-7            robustbase_0.92-7     
 [73] rgl_0.98.1             caTools_1.17.1         purrr_0.2.3           
 [76] bindrcpp_0.2           mime_0.5               slam_0.1-40           
 [79] compiler_3.4.1         affyio_1.46.0          MassSpecWavelet_1.42.0
 [82] tibble_1.3.4           pcaPP_1.9-72           stringi_1.1.5         
 [85] markdown_0.8           multtest_2.32.0        MALDIquant_1.16.4     
 [88] corpcor_1.6.9          httpuv_1.3.5           R6_2.2.2              
 [91] latticeExtra_0.6-28    affy_1.54.0            KernSmooth_2.23-15    
 [94] gridExtra_2.2.1        IRanges_2.10.2         codetools_0.2-15      
 [97] reports_0.1.4          assertthat_0.2.0       gtools_3.5.0          
[100] xlsxjars_0.6.1         chron_2.3-50           rprojroot_1.2         
[103] S4Vectors_0.14.3       grid_3.4.1             rpart_4.1-11          
[106] tidyr_0.7.1            Cairo_1.5-9            NLP_0.1-11            
[109] base64enc_0.1-3        ellipse_0.3-8  


# Licensing

This shiny code is licensed under the GPLv3. Please see the file LICENSE.txt for
information.

    ShIMA (Shiny Interface for Metabolite Analysis) is a
    Shiny App for analysis and visualization of metabolomics data.
    

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
    Please also acknoledge the Office of Naval Research (ONR) and 
    University of Aberdeen if you are using this pipeline.

    
    



