---
title: "ShIMA Tutorial"
runtime: shiny
output: html_document
---

This tutorial contains the test raw metabolomics data in the .mzXML formats. The .mzXML format is the most common raw formats for LCMS data.  The format is very much similar to the basic XML format. It contains various meta-data tags such as "msRun", "parentFile", "msManufacturer", etc. and the real data inside the tag "can" such as "num", "scanType", "peaksCount", "polarity", "retentionTime", "lowMz", "highMz", etc. These are the input for "Autopipeline" and the "Preprocessing" tab but for all other tabs, the input is text files separated by comma/tab.

## Autopipeline


### Input

We have grouped the .mzXML files into "blank", "QC", "sample1", "sample2" and "sample3" and put them inside directories with the respective names (Fig-1a). Each of the directories contains 4 .mzXML files.

<img src="AutopipelineInpt.png" alt="Aotopipeline Input" style="width: 60%"/>

Fig-1a: Input for the Autopipeline tab

### Output

The Autopipeline results with the following files:-

#### Peak Table:

The peak table is a direct output of the package XCMS which basically provides the grouped and preprocessed peak intensities with the file name and the group they belong as the column names (Fig-1b). The mz/RT  can be found in the first column of the file under the column name "group". 

<img src="AutopipelinePeak.png" alt="Aotopipeline Output Peak" style="width: 60%"/>

Fig-1b: Peak table from the Autopipeline tab output

#### Report:

The report table is also a direct output of the package XCMS (Fig-1c). This acyually represents a differential report with fold change and p value and Metlin identification. ShIMA only consider the Metlin IDs as it provides the differential analysis in the StatisticalAnalysis tab.

<img src="AutopipelineReport.png" alt="Aotopipeline Output Report" style="width: 60%"/>

Fig-1c: Report table from the Autopipeline tab output

#### Metlin Identification:

This provides the identification of metabolite IDs in the metlin database provided by XCMS (Fig-1d). The file contains the mass charge ratio and retention time along with their respective matches in the metlin database.

<img src="MetlinRes.png" alt="Metlin Identification" style="width: 60%"/>

Fig-1d: Metlin identification table

#### Input for Annotation, StatisticalAnalysis and Visualization Tab:

Creates csv files which can be directly used as the inputs for the next tabs i.e. Annotation, StatisticalAnalysis and Visualization tabs. 

#### Identification and Annotation against various databases:

The Autopipeline also uses xMSannotator followed by XCMS to search against the databases such as "HMDB", "KEGG" and "LipidMaps". The basic output of [`multilevelannotation`](https://rdrr.io/github/yufree/xMSannotator/man/multilevelannotation.html) function of xMSannotator is saved in 3 different directories with respective database names. 

#### Merged Results

For a better idea about of the metabolites identified, the output contains 2 files with merged results which merges all the outputs based on the metabolite names (Fig-1e). 

<img src="MergedRes.png" alt="Merged table" style="width: 60%"/>

Fig-1e: Merged result table


## Preprocessing

The Preprocessing tab works exactly the first part of the Autopipeline. It uses XCMS but not xMSannotator. 
### Input
The input for Preprocessing tab is same as the Annotation tab; the raw metbolomics data (Fig-1a). It provides option to modify various parameters according to user interest (See Documentation).


### Output

The output from the Preprocessing tab contains the Peak table, Report table, Metlin table and inputs for Annotation and Visualization tabs.

## Annotation

The Annotation tabs performs the identification of the metabolites by matching the mz/RT against the "HMDB", "KEGG" and "LipidMaps" databses. 

### Input

The users can use the file generated from the Preprocessing or a file with similar structure which contains the first 2 columns as the mz and RT and then individual intensities accross the samples (Fig-3a). The "Parameters" panel can be used to modify the parameters as discussed in the Documentation which greatly influence the identification process.

<img src="AnnotationInpt.png" alt="Annotation Input" style="width: 60%"/>

Fig-3a: Input for the Annotation tab


### Output

The output of the Annotation tab is the basic output of the xMSannotator's multilevel annotation. The users has to choose a database from the option to get results for matches against the respective database.

## StatisticalAnalysis

The StatisticalAnalysis tab provides 2 panels. The first one provides the differential analysis and the second performs a conditional mutual information network analysis.

### Input

The input file for this tab is a csv file configured by the users similar to that of the output file of the Autopipeline which can also be directly used as the input (Fig-4a). The input file structure is same in case of both the panels under this tab.

The users should choose one of the option between "Reference" or "Groups" to find several options to again choose the groups to be compared for the Differential Analysis. There are options for choosing cut-offs for the p value , adjusted p value and the fold change. Just input as per the interest to subset the result data. 

Similarly, in case of Network Analysis, the users can select groups for subsetting from the parent input files. The users can also select checkboxes to allow the network statistics and graphml file to be included in the output.

<img src="StatisticalAnalysisInpt.png" alt="StatisticalAnalysis Input" style="width: 60%"/>

Fig-4a: Input for the StatisticalAnalysis tab

### Output

#### Differential Analysis

The output of the differential analysis creates a directory in the choosen output directory by the name, "runDiffAnalysisRes" containing 3 files. One of the files is the normalized logaritmic intensities for the metabolites. The other 2 files provides the output of the topTable function and actual differential report for the choosen groups after fitting the statistical model. 


#### Network Analysis

The Network analysis results in a matrix for all the metabolite-metabolite mutual information and another 3 files (if the checkboxes are selected) which are the graphml file and 2 files for the network statistics. One of this file suffixed with "NetStats"
contains "ID", "cluster", "NodeStrength", "EigenCentrality" and "NodeDegree" for each metabolites and the other one contains basic info such as the number of nodes and Modularity Coefficient and has been suffixed with "NetInfo".


#### Visualization

This tab allows the users to use the same input as that of the StatisticalAnalysis tab or log transformed intensities of the metabolites to visualize the data using plots such as Heatmap, PCA plot, Level plot and RLA plot (Fig-5).

<img src="heatmap.png" alt="StatisticalAnalysis Input" style="width: 60%"/>

Fig-5a: Heatmap

<img src="pca.png" alt="StatisticalAnalysis Input" style="width: 60%"/>

Fig-5b: PCA plot

<img src="levelplot.png" alt="StatisticalAnalysis Input" style="width: 60%"/>

Fig-5c: Level plot

<img src="rla.png" alt="StatisticalAnalysis Input" style="width: 60%"/>

Fig-5d: RLA plot










  


