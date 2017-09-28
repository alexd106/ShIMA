---
title: "ShIMA Documentation"
runtime: shiny
output: html_document
---

This is an R Markdown document to provide a overview of the Metabolomics pipeline and how it works.  

# Introduction
The "Shiny Interface for Metabolite Analysis" (ShIMA) is developed in Shiny graphical user interface (GUI) environment using R. The basic idea is to provide a platform where the users can easily analyse the metabolomics datasets to identify and annotate metabolites. There is also provision for a few basic downstream analyses. The pipeline runs mainly on widely used packages such as XCMS, xMSannotator and metabolomics to perform the analysis. In-house scripts were written for automating and linking the individual analysis. The users do not need to be experts in programming in R.

<img src="Pipeline.png" alt="Pipeline_Home" style="width: 60%"/>

Figure 1: The Metabolomics Pipeline Home page

There are 2 main features of this pipeline; options to choose basic information regarding the data such as sample type, organism name from which the sample was taken and options in the form of a tabbed panel for submitting the input data. There are 4 tabs which are: *Autopipeline*, *Preprocessing*, *Annotation* and *StatisticalAnalysis*.

## Autopipeline

The *Autopipeline* doesn"t allow the users to change the value for the parameters and set as the default. It takes the raw metabolomics data as input and the users can select the output directory. It reads various types of raw data formats such as NetCDF, mzXML, mzData, etc.


<img src="Autopipeline.png" alt="Aotopipeline" style="width: 60%"/>

Figure 2: The Autopipeline tab

Once the Go button is pressed, the process starts running. It pre-processes the raw data and then matches against the databases such as Metlin, HMDB, KEGG and LipidMaps. The combination of *Preprocessing* and *Annotation* tabs is the same process as the *Autopipeline* tab with the default value of the parameters (for more details refer to *Preprocessing* and *Annotation* tab documentation). 


## Preprocessing

<img src="Preprocessing.png" alt="Preprocessing" style="width: 60%"/>

Figure 3: The Preprocessing tab

The pre-processing step mainly consists of steps such as reading the raw data, grouping, retention time (RT) correction, gap filling, etc. which is done using XCMS package. 

This takes the raw metabolomics data formats such as mzXML, NetCDF, mzData, etc. as input. Users can define different sample groups as well as the blank, standards and quality control as different group by keeping them in separate directories inside the parental directory which is selected using the "select directory" option on the "Preprocessing" tab. The output directory can also be selected to save the outputs. 

Once the "Go" button is clicked it starts reading the individual raw data files (also reads inside sub directories automatically), stores into a variable and tags according to their respective subdirectory names which are considered as the sample classes with the help of the function [`xcmsSet`](https://www.rdocumentation.org/packages/xcms/versions/1.48.0/topics/xcmsSet) from XCMS. We use the `centWave` algorithm for the peak detection method which is usually "matched filter" by default in XCMS. The "centWave" algorithm is preferred in case of high-resolution MS data. There are a few parameters such as ppm, Peakwidth, Signal to noise ratio cutoff, prefilter mass traces, Number of slaves/core, Minimum difference in m/z and integration method, which are needed to be optimized based upon the particular experimental conditions. The users can see the default values in the Parameters collapsible panel and they can change it according to their interest.

<img src="PreprocessingParam.png" alt="Preprocessing Parameters" style="width: 60%"/>

Figure 4: The Preprocessing tab Parameters option


RT correction is done using [`rector`](https://www.rdocumentation.org/packages/xcms/versions/1.48.0/topics/retcor-methods) function from XCMS package. After matching peaks into groups, those groups are used to identify and correct correlated drifts in retention time from run to run. Well-behaved peak groups are selected and the median retention time is calculated and then a deviation from that for each sample. The parameters for the "rector" function can be found in the panel named Parameters and the parameters are "method" which may be "obiwarp"or "normal", "profStep" which represents the step size (in m/z) to use for profile generation from the raw data files and center to set the index of the sample all others will be aligned to.


After RT correction is done, the peaks are grouped (maintaining the tags) by choosing the peaks that represent the same analytes across samples. The [`grouping algorithm`](https://www.rdocumentation.org/packages/xcms/versions/1.48.0/topics/group-methods) processes the peak lists in order of increasing mass and regularly creates logs for the mass, it is currently working on.
[`Gap filling`](https://www.rdocumentation.org/packages/xcms/versions/1.48.0/topics/fillPeaks-methods) process is performed for filling of the missing peaks (peaks that were missed during peak identification or may be an analyte is absent in a sample). It is then assigned to a new variable. This function compares the peaks with their respective raw peaks and fills up the missing parts.
Output files are generated for the significant analyte intensities across all the samples. The input for the Annotation and StatisticalAnalysis tabs are prepared from the output of Preprocessing tab with a few lines of R scripts.

## Annotation

<img src="Annotation.png" alt="Annotation" style="width: 60%"/>

Figure 5: The Annotation tab

The Annotation tab provides database matches against the processed metabolomics data to identify known metabolites with the help of xMSannotator package. It provides a function known as [`multilevelannotation`](https://rdrr.io/github/yufree/xMSannotator/man/multilevelannotation.html) which accepts user defined value for several parameters to search against databases efficiently. Several parameters are associated with the multilevelannotation which can be modified by the users: 

<img src="AnnotationParam.png" alt="Annotation Parameters" style="width: 60%"/>

Figure 6: The Annotation tab Parameters option


max.mz.diff - Mass tolerance in ppm for database matching.

max.rt.diff - Retention time (s) tolerance for finding peaks derived from the same parent metabolite.

corthresh - Minimum correlation threshold between peaks to qualify as adducts/isotopes of the same metabolite.

max_isp - Maximum number of expected isotopes.

mass_defect_window - Mass defect window in daltons for finding isotopes.

num_sets - How many subsets should the total number of metabolites in a database should be divided into to faciliate parallel processing or prevent memory overload?

num_nodes - Number of computing cores to be used for parallel processing.

Ionization mode - The ionization mode can be one of 3 values i.e. positive, negative or both depending upon the experiment.

Database - The database to be searched against. This can be HMDB/KEGG/LipidMaps.

Status - Used for HMDB. e.g.: NA, "Detected", "Detected and Quantified", "Detected and Not Quantified", "Expected and Not Quantified". In case of other databases, it will be automatically set to NA.

Adducts - Choose adducts of interest from the list to limit the identifications.

It takes the processed data from the output of Preprocessing tab. Users can also provide their data in the form of a comma-separated value (.csv) or tab-separated value (.txt) file as input where the first 2 columns are mz (mass) and time (retention time) respectively and all other columns contain the individual mz value for all the samples.

## StatisticalAnalysis
The statistical analysis focuses on the significant identifications of metabolites for further analysis. The very first interest is to find the differentially expressed metabolites across all the groups. There are 2 collapsible panels; Differential Analysis and Network Analysis. 

The input is prepared in such a way that the metabolite intensities (mz) will be representing the columns and the corresponding groups in the rows.


<img src="StatisticalAnalysis.png" alt="StatisticalAnalysis" style="width: 60%"/>

Figure 7: The StatisticalAnalysis tab

The Differential Analysis results in differential expression of the data. It provides 2 options  which are using Reference and Groups to chose the groups for comparsion of level of differential expressions. The reference can be selected and it will perform all the possible combinations of groups, keeping the chosen one as the control. The second option Groups provides all the possible combinations of the groups to choose one by one to customize the output. Except these options, there are a few other options to chose cutt-offs for the data based upon p-value, adjusted p-value and log fold change. The users can easily type in the value for these parameters and it will be suseted from the entire results and be presented.

<img src="DifferentialAnalysis.png" alt="DifferentialAnalysis" style="width: 60%"/>

Figure 8: The Differential Analysis panel on StatisticalAnalysis tab

Similarly in case of the Network Analysis panel, the user has to mention the group name for choosing the data to be subset and used as input. Then it constructs mutual information network on the selected data. The mutual information calculated here is actually the conditional mutual information (CMI) with the help of [`minet`](https://www.rdocumentation.org/packages/minet/versions/3.30.0/topics/minet) function from the package minet which is "Mutual Information NETworks". It calculates the mutual information between all pairs of variables in the provided input and creates the adjacency matrix. Then the adjacency matrix is converted to igraph object using [`graph_from_adjacency_matrix`](https://www.rdocumentation.org/packages/igraph/versions/1.1.2/topics/graph_from_adjacency_matrix) function from igraph. Further several network properties were calculated for the entire network. The network file can be saved in .graphml format which can be used further in graph visualization tools such as Gephi. A file will be generated containing the network properties from the statistical analysis of the network.

<img src="NetworkAnalysis.png" alt="NetworkAnalysis" style="width: 60%"/>

Figure 9: The Network Analysis panel on StatisticalAnalysis tab

## Visualization
The Visualization tab is simply for producing various graphs by using the output of the analysis. There are options to choose for plots to be output as .png files such as Heatmap, PCA, Level and RLA. 

<img src="Visualization.png" alt="Visualization" style="width: 60%"/>

Figure 10: The Visualization panel on StatisticalAnalysis tab

### Heatmap

The heatmap provides the expression intensities of the metabolites across the chosen groups. This helps in comparing the differential expression of metabolites in between 2 or more groups. It uses [`d3heatmap`](https://www.rdocumentation.org/packages/d3heatmap/versions/0.6.1.1/topics/d3heatmap) function from the package d3heatmap to draw an interactive heatmap. For saving the plot, it uses [`heatmap`](https://stat.ethz.ch/R-manual/R-devel/library/stats/html/heatmap.html).

### Principal Component Analysis (PCA) plots

These is basically ellipse plots representing the sample groups which are colored differently. Here the [`PLS-DA`](http://mixomics.org/methods/pls-da/) model from the package mixOmics has been used which is a supervised model. The plotting is done with the help of the function named [`plotIndiv`](http://mixomics.org/graphics/sample-plots/plotindiv/).

### Level plots

The level plot is basically similar to a heatmap but plotted on a matrix which represents the correlation between the sample groups. It uses the function [`levelplot`](https://www.rdocumentation.org/packages/lattice/versions/0.10-10/topics/levelplot) from the package lattice. This is basically a two-dimensional view of the suface data.

### Relative Log Abundance (RLA) plots

The RLA plot produces the relative log abundance plots for both within group and across group for the metabolite data matrix using the function [`RlaPlots`](https://rdrr.io/cran/metabolomics/src/R/RlaPlots.r).

## Reports

There is interesting options available to create reports in pdf/doc format for the analysis from Autopipeline, Preprocessing, Annotation and StatisticalAnalysis tabs. These reports summarize the meta data information for the input and output as well as inference into the analysis using various visualizations. Once the analysis is run, the "Generate Reprt" button will be activated and user can save the report.


# Suggestions
- Most of the packages will be installed automatically while running the app, if not then install them manually.
- To avoid unnecessary errors, it suggested that the pipeline should be run on a Linux server or a system with administrator privileges.





  


