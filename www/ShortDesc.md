---
title: "Short Description"

output: html_document
---

<span style="color:blue">

The “Shiny Interface for Metabolite Analysis” (ShIMA) is a Shiny application for analysing the raw metabolomics data to identify, annotate and perform various downstream analysis for the metabolites. This is a graphical user interface (GUI) which connects to the R scripts in the back-end. Users can pre-process the raw data which includes picking the peaks, retention time correction, grouping and report generation with the help of XCMS package and R scripts. The package xMSannotator is used for the identification and annotation of the metabolites from the processed data in the Annotation tab. The Autopipeline tab consists of both preprocessing and annotation with default parameters. The downstream analysis identifies the differential expression of the identified metabolites, provides various visualizations for the metabolites and the network analysis. 


The pipeline runs mainly on widely used packages such as XCMS, xMSannotator and metabolomics for the entire analysis. In-house scripts were written for automating and linking the individual analysis. The users do not need to be experts in programming in R.

</span>