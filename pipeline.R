
options(java.parameters = "-Xss2560k")

body(multilevelannotation)[[25]][[4]][[3]][[169]][[2]] <- substitute({
  c("outloc", "adduct_weights", "boostIDs", "pathwaycheckmode", "adduct_table", 
    "max_diff_rt", "corthresh", "filter.by", "max.rt.diff", "max_isp", "min_ions_perchem", 
    "max.mz.diff", "db_name", "allsteps", "redundancy_check", "num_nodes", "num_sets")
})
# body(multilevelannotation)[[25]][[4]][[3]][[170]][[2]][[2]]<-substitute({paste0('mchemdata',',','num_nodes')})
body(multilevelannotation)[[25]][[4]][[3]][[35]][[4]][[2]][[3]][[2]] <- substitute({
  data(keggCompMZ)
  names(keggCompMZ)[7] <- "AdductMass"
})

runAutopipeline <- function(inputDir, outputDir) {
  
  if(Sys.info()["sysname"] =="Linux"){
  xsg1 <- xcmsSet(files = inputDir, method = "centWave", ppm = 5, peakwidth = c(10, 100), snthresh = 5, prefilter = c(3, 1000), integrate = 1, mzdiff = 0.01, 
                  verbose.columns = TRUE, fitgauss = FALSE, BPPARAM = MulticoreParam(10))
  } 
  if(Sys.info()["sysname"] == "Windows"){
    xsg1 <- xcmsSet(files = inputDir, method = "centWave", ppm = 5, peakwidth = c(10, 100), snthresh = 5, prefilter = c(3, 1000), integrate = 1, mzdiff = 0.01, 
                    verbose.columns = TRUE, fitgauss = FALSE)
    
  } else{
    cat ("Mac implementation not available")
  }
    
  
  xsg1<-group.nearest(xsg1)
  xsg2 <- retcor(xsg1, method = "obiwarp", profStep = 0.01, center = 3)
  xsg3 <- group.nearest(xsg2)
  xsg4 <- fillPeaks(xsg3)
  dat <- groupval(xsg4, "medret", "into")
  
  dat <- rbind(group = as.character(phenoData(xsg4)$class), dat)
  
  report <- diffreport(xsg4, metlin = 0.15, h = 480, w = 640)
  
  setwd(outputDir)
  dir.create("runAutopipelineRes")
  outputDir<-paste0(outputDir,"/runAutopipelineRes")
  setwd(outputDir)
  write.table(dat, file = "MyPeakTable.txt", sep = "\t")
  write.table(report, file = "MyExpTable.txt", sep = "\t")
  dir.create("HMDB")
  dir.create("KEGG")
  dir.create("LipidMaps")
  #data <- read.table("MyExpTable.txt", sep = "\t", header = T)
  
  ###### 
  dataB<-report[,-c(which(names(report) %in% "fold"):which(names(report) %in% "anova"), which(names(report) %in% "mzmin"), which(names(report) %in% "mzmax"), which(names(report) %in% "rtmin"):which(names(report) %in% "metlin"))]
  names(dataB)[1] <- "Metlin_ID"
  names(dataB)[2] <- "mz"
  names(dataB)[3] <- "time"
  dataB$mz <- round(dataB$mz, digits = 4)
  dataB$time <- round(dataB$time, digits = 1)
  MetlinRes <- dataB[,c(which(names(dataB) %in% "Metlin_ID"),which(names(dataB) %in% "mz"),which(names(dataB) %in% "time"))]
  write.csv(MetlinRes, file = "MetlinRes.csv", row.names=F)
  dataC<-dataB[,-c(which(names(dataB) %in% "mz"),which(names(dataB) %in% "time"))]
  write.csv(dataC, file = "VizInpt.csv", row.names=F)
  dataC <- setNames(data.frame(t(dataC[, -1])), dataC[, 1])
  dataC$Sample <- rownames(dataC)
  dataC$Group <- paste(phenoData(xsg1)$class)
  dataC <- dataC[, c(ncol(dataC) - 1, ncol(dataC), 1:(ncol(dataC) - 2))]
  write.csv(dataC, file = "StatisticalAnalysInpt.csv", row.names=F)
  ######
  
  dataA <- dataB[,-which(names(dataB) %in% "Metlin_ID")]
  write.csv(dataA, file = "AnnotationInpt.csv", row.names=F)
  dataA <- data.frame(sapply(dataA, function(x) as.numeric(as.character(x))))
  
  data(example_data)
  data(adduct_table)
  data(adduct_weights)
  data(customIDs)
  ## data(customDB) data(hmdbAllinf) data(keggotherinf) data(t3dbotherinf)
  outloc1 <- paste0(outputDir, "/HMDB")
  outloc2 <- paste0(outputDir, "/KEGG")
  outloc3 <- paste0(outputDir, "/LipidMaps")
  max.mz.diff <- 10
  max.rt.diff <- 10
  corthresh <- 0.7
  max_isp <- 5
  mass_defect_window <- 0.01
  num_nodes <- 2
  
  db_name1 <- "HMDB"
  db_name2 <- "KEGG"
  db_name3 <- "LipidMaps"
  status <- "Detected and Quantified"
  num_sets <- 300
  
  mode <- "both"
  queryadductlist <- c("M+2H", "M+H+NH4", "M+ACN+2H", "M+2ACN+2H", "M+H", "M+NH4", 
                       "M+Na", "M+ACN+H", "M+ACN+Na", "M+2ACN+H", "2M+H", "2M+Na", "2M+ACN+H", "M+2Na-H", 
                       "M+H-H2O", "M+H-2H2O", "M-H", "M-H2O-H", "M+Na-2H", "M+Cl", "M+FA-H")  #other options: c('M-H','M-H2O-H','M+Na-2H','M+Cl','M+FA-H'); c('positive'); c('negative'); c('all');see data(adduct_table) for complete list
  
  
  customIDs <- NA
  
  
  dataA <- unique(dataA)
  print(dim(dataA))
  
  system.time(annotres1 <- multilevelannotation(dataA = dataA, max.mz.diff = max.mz.diff, 
                                                max.rt.diff = max.rt.diff, cormethod = "pearson", num_nodes = num_nodes, 
                                                queryadductlist = queryadductlist, mode = mode, outloc = outloc1, db_name = db_name1, 
                                                adduct_weights = adduct_weights, num_sets = num_sets, allsteps = TRUE, corthresh = corthresh, 
                                                NOPS_check = TRUE, customIDs = customIDs, missing.value = NA, deepsplit = 2, 
                                                networktype = "unsigned", minclustsize = 10, module.merge.dissimilarity = 0.2, 
                                                filter.by = c("M+H"), biofluid.location = NA, origin = NA, status = status, 
                                                boostIDs = NA, max_isp = max_isp, customDB = customDB, HMDBselect = "union", 
                                                mass_defect_window = mass_defect_window, pathwaycheckmode = "pm", mass_defect_mode = "pos"))
  
  system.time(annotres2 <- multilevelannotation(dataA = dataA, max.mz.diff = max.mz.diff, 
                                                max.rt.diff = max.rt.diff, cormethod = "pearson", num_nodes = num_nodes, 
                                                queryadductlist = queryadductlist, mode = mode, outloc = outloc2, db_name = db_name2, 
                                                adduct_weights = adduct_weights, num_sets = num_sets, allsteps = TRUE, corthresh = corthresh, 
                                                NOPS_check = TRUE, customIDs = customIDs, missing.value = NA, deepsplit = 2, 
                                                networktype = "unsigned", minclustsize = 10, module.merge.dissimilarity = 0.2, 
                                                filter.by = c("M+H"), biofluid.location = NA, origin = NA, status = status, 
                                                boostIDs = NA, max_isp = max_isp, customDB = customDB, HMDBselect = "union", 
                                                mass_defect_window = mass_defect_window, pathwaycheckmode = "pm", mass_defect_mode = "pos"))
  
  system.time(annotres3 <- multilevelannotation(dataA = dataA, max.mz.diff = max.mz.diff, 
                                                max.rt.diff = max.rt.diff, cormethod = "pearson", num_nodes = num_nodes, 
                                                queryadductlist = queryadductlist, mode = mode, outloc = outloc3, db_name = db_name3, 
                                                adduct_weights = adduct_weights, num_sets = num_sets, allsteps = TRUE, corthresh = corthresh, 
                                                NOPS_check = TRUE, customIDs = customIDs, missing.value = NA, deepsplit = 2, 
                                                networktype = "unsigned", minclustsize = 10, module.merge.dissimilarity = 0.2, 
                                                filter.by = c("M+H"), biofluid.location = NA, origin = NA, status = status, 
                                                boostIDs = NA, max_isp = max_isp, customDB = customDB, HMDBselect = "union", 
                                                mass_defect_window = mass_defect_window, pathwaycheckmode = "pm", mass_defect_mode = "pos"))
  
  ###### 
  hmdbRes <- read.csv(paste0(outputDir, "/HMDB/Stage5.csv"), header = T)
  hmdbRes$mz <- round(hmdbRes$mz, digits = 4)
  hmdbRes$time <- round(hmdbRes$time, digits = 1)
  hmdbRes$db_name <- "HMDB"
  keggRes <- read.csv(paste0(outputDir, "/KEGG/Stage5.csv"), header = T)
  keggRes$mz <- round(keggRes$mz, digits = 4)
  keggRes$time <- round(keggRes$time, digits = 1)
  keggRes$db_name <- "KEGG"
  LipidMapsRes <- read.csv(paste0(outputDir, "/LipidMaps/Stage5.csv"), header = T)
  LipidMapsRes$mz <- round(LipidMapsRes$mz, digits = 4)
  LipidMapsRes$time <- round(LipidMapsRes$time, digits = 1)
  LipidMapsRes$db_name <- "LipidMaps"
  
  hmdb_kegg_LipidMaps <- rbind(hmdbRes, keggRes, LipidMapsRes)
  hmdb_kegg_LipidMaps2 <- cSplit(hmdb_kegg_LipidMaps, "Name", sep = ";", direction = "long")
  setDF(hmdb_kegg_LipidMaps2)
  
  hmdb_kegg_LipidMaps3 <- aggregate(cbind(chemical_ID = as.character(chemical_ID), 
                                          Confidence, score, Module_RTclust, MatchCategory = as.character(MatchCategory), 
                                          theoretical.mz, delta_ppm, Formula = as.character(Formula), MonoisotopicMass, 
                                          Adduct = as.character(Adduct), ISgroup = as.character(ISgroup), mean_int_vec, 
                                          MD, db_name = as.character(db_name)) ~ mz + time + Name, hmdb_kegg_LipidMaps2, 
                                    paste, collapse = "|")
  
  setwd(outputDir)
  write.csv(hmdb_kegg_LipidMaps3, file = "hmdb_kegg_LipidMaps3.csv", row.names=F)
  
  MetlinRes_hmdb_kegg_LipidMaps3 <- merge(MetlinRes, hmdb_kegg_LipidMaps3, by = c("mz", "time"), all = T)
  write.csv(MetlinRes_hmdb_kegg_LipidMaps3, file = "AllMergdRes.csv", row.names=F)
  ###### 
  
  save(xsg1, dat,report,dataB, dataC, dataA, hmdbRes, keggRes, LipidMapsRes, MetlinRes_hmdb_kegg_LipidMaps3, file="runAutopipelineRes.RData")
}


runPreprocess <- function(inputDir,outputDir,ppm,peakwidth,snthresh,prefilter,integrate,mzdiff,nSlaves,retcorMethod,profStep,center) {
  
  xsg1 <- xcmsSet(files = inputDir, method = "centWave", ppm = as.numeric(ppm), peakwidth = peakwidth, snthresh = as.numeric(snthresh), prefilter = prefilter, integrate = as.numeric(integrate), mzdiff = as.numeric(mzdiff), 
                  verbose.columns = TRUE, fitgauss = FALSE, BPPARAM = MulticoreParam(as.numeric(nSlaves)))
  xsg1<-group.nearest(xsg1)
  xsg2 <- retcor(xsg1, method = retcorMethod, profStep = as.numeric(profStep), center = as.numeric(center))
  xsg3 <- group.nearest(xsg2)
  xsg4 <- fillPeaks(xsg3)
  dat <- groupval(xsg4, "medret", "into")
  dat <- rbind(group = as.character(phenoData(xsg4)$class), dat)
  report <- diffreport(xsg4, metlin = 0.15, h = 480, w = 640)
  setwd(outputDir)
  dir.create("runProcessAnalysisRes")
  outputDir<-paste0(outputDir,"/runProcessAnalysisRes")
  setwd(outputDir)
  write.table(dat, file = "MyPeakTable.txt", sep = "\t")
  write.table(report, file = "MyExpTable.txt", sep = "\t")
  
  ######
  dataB<-report[,-c(which(names(report) %in% "fold"):which(names(report) %in% "anova"), which(names(report) %in% "mzmin"), which(names(report) %in% "mzmax"), which(names(report) %in% "rtmin"):which(names(report) %in% "metlin"))]
  names(dataB)[1] <- "Metlin_ID"
  names(dataB)[2] <- "mz"
  names(dataB)[3] <- "time"
  dataB$mz <- round(dataB$mz, digits = 4)
  dataB$time <- round(dataB$time, digits = 1)
  write.csv(dataB[,c(which(names(dataB) %in% "Metlin_ID"),which(names(dataB) %in% "mz"),which(names(dataB) %in% "time"))], file = "MetlinRes.csv", row.names=F)
  dataC<-dataB[,-c(which(names(dataB) %in% "mz"),which(names(dataB) %in% "time"))]
  write.csv(dataC, file = "VizInpt.csv", row.names=F)
  dataC <- setNames(data.frame(t(dataC[, -1])), dataC[, 1])
  dataC$Sample <- rownames(dataC)
  dataC$Group <- paste(phenoData(xsg1)$class)
  dataC <- dataC[, c(ncol(dataC) - 1, ncol(dataC), 1:(ncol(dataC) - 2))]
  write.csv(dataC, file = "StatisticalAnalysInpt.csv", row.names=F)
  dataA <- dataB[,-which(names(dataB) %in% "Metlin_ID")]
  write.csv(dataA, file = "AnnotationInpt.csv", row.names=F)
  
  save(xsg1, dat,report,dataB, dataC, dataA, file="runProcessAnalysisRes.RData")
}



runAnnotation <- function(dataA, outDir, max.mz.diff, max.rt.diff, num_nodes, queryadductlist, 
                          mode, db_name, num_sets, corthresh, status, max_isp, mass_defect_window) {
  
  
  data(example_data)
  data(adduct_table)
  data(adduct_weights)
  data(customIDs)
  data(customDB)
  customIDs <- NA
  dataA <- unique(dataA)
  print(dim(dataA))
  print(format(Sys.time(), "%a %b %d %X %Y"))
  
  setwd(outDir)
  dir.create("runAnnotAnalysisRes")
  outputDir<-paste0(outDir,"/runAnnotAnalysisRes")
  setwd(outputDir)
  
  if (db_name == "HMDB") {
    dir.create("HMDB")
    outloc <- paste0(outputDir, "/HMDB")
  } else if (db_name == "KEGG") {
    dir.create("KEGG")
    outloc <- paste0(outputDir, "/KEGG")
  } else if (db_name == "LipidMaps") {
    dir.create("LipidMaps")
    outloc <- paste0(outputDir, "/LipidMaps")
  }
  
  
  
  system.time(annotres <- multilevelannotation(dataA = dataA, max.mz.diff = as.numeric(max.mz.diff), 
                                               max.rt.diff = as.numeric(max.rt.diff), cormethod = "pearson", num_nodes = as.numeric(num_nodes), 
                                               queryadductlist = queryadductlist, mode = mode, outloc = outloc, db_name = db_name, 
                                               adduct_weights = adduct_weights, num_sets = as.numeric(num_sets), allsteps = TRUE, 
                                               corthresh = as.numeric(corthresh), NOPS_check = TRUE, customIDs = customIDs, 
                                               missing.value = NA, deepsplit = 2, networktype = "unsigned", minclustsize = 10, 
                                               module.merge.dissimilarity = 0.2, filter.by = c("M+H"), biofluid.location = NA, 
                                               origin = NA, status = status, boostIDs = NA, max_isp = as.numeric(max_isp), 
                                               customDB = customDB, HMDBselect = "union", mass_defect_window = as.numeric(mass_defect_window), 
                                               pathwaycheckmode = "pm", mass_defect_mode = "pos"))
  
  
  
  setwd(outDir)
  save(db_name, outloc, annotres, file="runAnnotAnalysisRes.Rdata")
}

runDiffAnalysis <- function(met, grp4comp, FparamName, FparamValue, AllPvalue, AllFCvalue, file2name) {
  
  # Normalize and rescale data to mean=0 and sd=1 Call data frame, norm
  normalizeSample <- function(x) {
    xNorm <- x
    for (i in 1:ncol(x)) {
      temp <- x[, i]
      temp2 <- (temp - mean(temp, na.rm = TRUE))/sd(temp, na.rm = TRUE)
      xNorm[, i] <- temp2
    }
    return(xNorm)
  }
  
  normalizeFeature <- function(x) {
    xNorm <- x
    for (i in 1:nrow(x)) {
      temp <- x[i, ]
      temp2 <- (temp - mean(temp, na.rm = TRUE))/sd(temp, na.rm = TRUE)
      xNorm[i, ] <- temp2
    }
    return(xNorm)
  }
  
  
  ################################################ 
  
  met[met == 0] <- 0.0001
  
  met.1 <- as.matrix(met[2:ncol(met)])
  #rownames(met.1) <- rownames(met)
  met.1 <- log2(met.1)
  
  Tmet.1<-t(met.1)
  png("rawBoxplot.png")
  boxplot(Tmet.1, las=2, ylab="Raw intensities of Metabolites", col="sienna", pars=list(par(mar=c(15,5,2,2))), cex.axis=0.75, outline=FALSE)
  mtext("Samples", side = 1, line = 7)
  dev.off()
  
  norm.feat <- normalizeFeature(met.1)
  
  norm.samp <- normalizeSample(norm.feat)
  
  norm.samp <- data.frame(norm.samp)
  
  Tnorm.samp<-t(norm.samp)
  png("normBoxplot.png")
  boxplot(Tnorm.samp, las=2, ylab="Normalized intensities of Metabolites", col="sienna", pars=list(par(mar=c(15,5,2,2))), cex.axis=0.75, outline=FALSE)
  mtext("Samples", side = 1, line = 7)
  dev.off()
  
  Group <- met$Group
  
  met.2 <- cbind(Group, norm.samp)
  
  write.csv(met.2, paste0("norm_", file2name, ".csv"), row.names = F)
  
  met.log <- met.2
  
  
  meta<-as.matrix(met.log[,-1])
  meta[!is.finite(meta)] <- 0
  plsda.dol<-plsda(meta, Group, ncomp = 2, logratio = "none")
  png("PCA.png")
  plotIndiv(plsda.dol, ellipse=TRUE, legend=TRUE, title="Individual Group's score", legend.position="bottom")
  dev.off()
  
  met.3 <- as.matrix(met.log[, -1])
  #met.log$Group <- relevel(met.log$Group, ref)
  trans.met.log <- t(met.log[, -1])
  colnames(trans.met.log) <- met.log[, 1]
  cormat <- cor(trans.met.log, use = "complete.obs")
  # create design matrix
  design <- with(met.log, model.matrix(~0 + Group))
  
  # attr(design, 'dimnames')[[2]] <- c('BTAL12', 'BTAL24', 'BTCR10', 'BTCR20',
  # 'BTCR30', 'BTCR40')
  attr(design, "dimnames")[[2]] <- gsub("Group", "", attr(design, "dimnames")[[2]])
  
  if(any(grep("-VS-",grp4comp)) == TRUE){
    contrast.mat <- makeContrasts(contrasts = gsub("-VS-","-",grp4comp), levels = design)
  } else{
    ref <- grp4comp
    grpNames <- attr(design, "dimnames")[[2]]
    grpNamesWithoutRef <- grpNames[-which(ref %in% grpNames)]
    # contrast.mat <- makeContrasts( 'BTAL24-BTAL12', 'BTCR10-BTAL12', 'BTCR20-BTAL12', 'BTCR30-BTAL12', 'BTCR40-BTAL12', levels=design)
    makeContrastsVar <- c()
    for (i in 1:length(grpNamesWithoutRef)) {
      makeContrastsVar <- c(makeContrastsVar, paste0(grpNamesWithoutRef[i], "-", ref))
    }
    contrast.mat <- makeContrasts(contrasts = makeContrastsVar, levels = design)
  }
  
  
  
  #################### make a linear model #########################################
  
  
  mod1 <- LinearModelFit(datamat = met.log[, -1], contrastmat = contrast.mat, factormat = design, 
                         ruv2 = FALSE, k = NULL, nc = NULL, moderated = TRUE, padjmethod = "BH", saveoutput = FALSE, 
                         outputname = "results")
  
  
  # produce a table of significant results based on F statistic
  
  results_F <- topTable(mod1, number = Inf, adjust = "BH")
  
  colnames(results_F)[1:(which(colnames(results_F) %in% "AveExpr")-1)] <- gsub("\\.", "-VS-",colnames(results_F)[1:(which(colnames(results_F) %in% "AveExpr")-1)])
  results_F <- results_F[which(results_F[,which(names(results_F) %in% FparamName)] <= FparamValue),]
  write.csv(results_F, paste0("Fstats_", file2name, ".csv"))
  
  mod1$genes <- data.frame(metabolites = rownames(mod1))
  # write a file containing all the test statistic, pvalues etc for each contrast
  
  
  mod2 <- eBayes(mod1)
  results <- decideTests(mod2)
  
  
  write.fit(mod1, results=, file = paste0("AllStats_", file2name, ".csv"), adjust = "BH", F.adjust = "BH", method = "separate", 
            sep = ",")
  results_F1 <- read.csv(paste0("AllStats_", file2name, ".csv"), header=T)
  results_F1<-results_F1[,c(ncol(results_F1), 1:(ncol(results_F1) - 1))]
  colnames(results_F1)[3:(which(colnames(results_F1) %in% "F")-1)] <- gsub("(.*)\\.(.*)", "\\1-VS-\\2",colnames(results_F1)[3:(which(colnames(results_F1) %in% "F")-1)])
  results_F1 <- results_F1[which(results_F1[,which(names(results_F1) %in% "F.p.value")] <= AllPvalue),]
  
  print(dim(results_F1))
  if(AllFCvalue == "NULL"){
    results_F1 <- results_F1
  } else{
  for(j in 1:length(names(results_F1[grepl("Coef", names(results_F1))]))){
    results_F1<-results_F1[which(results_F1[,which(names(results_F1) %in% names(results_F1[grepl("Coef", names(results_F1))])[j])] <= AllFCvalue),]
  }
  }
  print(dim(results_F1))
  write.csv(results_F1, paste0("AllStats_", file2name, ".csv"), row.names=F)
  
  
  # For the Report
  save(Tmet.1, Tnorm.samp, mod2, plsda.dol, cormat, contrast.mat, results_F, results, results_F1, file="runDiffAnalysisRes.Rdata")
  
}

runCMI <- function(GrpMetMZ, irlba = FALSE, graphML, stats, prefix = file3name) {
  
  
  GrpMetMZ[GrpMetMZ == 0] <- 0.0001
  GrpMetMZ[GrpMetMZ == Inf] <- 0.0001
  
                           
  metMZ.mi <- minet(GrpMetMZ[, 2:length(GrpMetMZ)], method = "aracne", estimator = "mi.mm", 
                    disc = "equalwidth")
  write.csv(metMZ.mi, file = paste0(prefix, "_mi.csv"))
  AI <- metMZ.mi
  rownames(AI) <- colnames(AI)
  feature.names <- rownames(AI)
  num.nodes <- dim(AI)[1]
  gc()
  
  n <- dim(AI)[1]
  
  cat("Converting to igraph object", "\n")
  AI.graph <- graph_from_adjacency_matrix(AI, mode = "undirected", weighted = TRUE, 
                                          diag = FALSE, add.colnames = "label")
  
  cat("Optimising modularity ", "\n")
  AI.clust <- cluster_leading_eigen(AI.graph)
  clust.mem <- membership(AI.clust)
  Q <- AI.clust$modularity
  
  cat("Computing network statistics ", "\n")
  assocm <- AI - diag(diag(AI))  #diagonal==0
  strength <- round(rowSums(assocm), digits = 3)
  node.degree <- apply(assocm, 1, function(x) length(x[x > 0]))
  
  if (irlba == TRUE) {
    eigencent <- round(abs(irlba(assocm, nu = 0, nv = 1)$v[, 1]), digits = 3)  # use with irlba for very large networks
  } else {
    eigencent <- round(abs(eigen(assocm, symmetric = TRUE)$vectors[, 1]), digits = 3)
  }
  
  netStats <- list(ID = feature.names, cluster = clust.mem, NodeStrength = strength, 
                   EigenCentrality = eigencent, NodeDegree = node.degree)
  net.stats.name <- paste(prefix, "_", "metMZmi", "_NetStats.txt", sep = "")
  
  if (is.null(graphML) && is.null(stats)) {
    print("No output file selected...")
  } else if (graphML=="graphML" & stats=="stats"){
    net.graph.name <- paste(prefix, "_", "metMZmi", "_net.graphML", sep = "")
    cat("Creating graphML file", net.graph.name, "\n")
    D <- dim(AI)[1]
    sink(net.graph.name)
    cat("<?xml version=\"1.0\" encoding=\"UTF-8\"?>\n<graphml xmlns=\"http://graphml.graphdrawing.org/xmlns\"\nxmlns:xsi=\"http://www.w3.org/2001/XMLSchema-instance\"\nxsi:schemaLocation=\"http://graphml.graphdrawing.org/xmlns\nhttp://graphml.graphdrawing.org/xmlns/1.0/graphml.xsd\">\n")
    cat("<key id=\"d0\" for=\"node\" attr.name=\"cluster\" attr.type=\"double\"/>\n")
    cat("<key id=\"d1\" for=\"node\" attr.name=\"NodeStrength\" attr.type=\"double\"/>\n")
    cat("<key id=\"d2\" for=\"node\" attr.name=\"eigen\" attr.type=\"double\"/>\n")
    cat("<key id=\"d3\" for=\"node\" attr.name=\"NodeDegree\" attr.type=\"double\"/>\n")
    cat("<key id=\"d4\" for=\"edge\" attr.name=\"Weight\" attr.type=\"double\"/>\n")
    cat("<graph id=\"G\" edgedefault=\"undirected\">\n")
    
    for (i in 1:D) {
      cat(paste("\t<node id=\"", dimnames(AI)[[1]][i], "\">\n"))
      cat(paste("\t<data key=\"d0\">", clust.mem[i], "</data>\n"))
      cat(paste("\t<data key=\"d1\">", strength[i], "</data>\n"))
      cat(paste("\t<data key=\"d2\">", eigencent[i], "</data>\n"))
      cat(paste("\t<data key=\"d3\">", node.degree[i], "</data>\n"))
      cat(paste("\t</node>\n"))
    }
    
    n <- 1
    for (i in 1:length(dimnames(AI)[[1]])) {
      for (j in 1:length(dimnames(AI)[[2]])) {
        if(AI[i,j]!=0){
          cat(paste("\t<edge id=\"", n,"\" Source=\"", dimnames(AI)[[1]][i], "\" Target=\"", dimnames(AI)[[2]][j], "\">\n")) 
          cat(paste("\t<data key=\"d4\">", AI[i,j], "</data>\n"))	
          cat(paste("\t</edge>\n"))
          n <- n+1
        }
      }
    }
    
    cat("</graph>\n")
    cat("</graphml>")
    sink()
    
    cat("Exporting network statistics to file: ", net.stats.name, "\n")
    fwrite(netStats, file = net.stats.name, col.names = TRUE, sep = " ")
    net.info.name <- paste(prefix, "_", "metMZmi", "_NetInfo.txt", sep = "")
    cat("Exporting network info to file", net.info.name, "\n")
    sink(net.info.name)
    cat(paste("The number of nodes: ", num.nodes, "\n"))
    #cat(paste("The number of edges: ", num.int, "\n"))
    cat(paste("Modularity Coefficient: ", Q, "\n"))
    sink()
  } else if (graphML=="graphML"){
    net.graph.name <- paste(prefix, "_", "metMZmi", "_net.graphML", sep = "")
    cat("Creating graphML file", net.graph.name, "\n")
    D <- dim(AI)[1]
    sink(net.graph.name)
    cat("<?xml version=\"1.0\" encoding=\"UTF-8\"?>\n<graphml xmlns=\"http://graphml.graphdrawing.org/xmlns\"\nxmlns:xsi=\"http://www.w3.org/2001/XMLSchema-instance\"\nxsi:schemaLocation=\"http://graphml.graphdrawing.org/xmlns\nhttp://graphml.graphdrawing.org/xmlns/1.0/graphml.xsd\">\n")
    cat("<key id=\"d0\" for=\"node\" attr.name=\"cluster\" attr.type=\"double\"/>\n")
    cat("<key id=\"d1\" for=\"node\" attr.name=\"NodeStrength\" attr.type=\"double\"/>\n")
    cat("<key id=\"d2\" for=\"node\" attr.name=\"eigen\" attr.type=\"double\"/>\n")
    cat("<key id=\"d3\" for=\"node\" attr.name=\"NodeDegree\" attr.type=\"double\"/>\n")
    cat("<key id=\"d4\" for=\"edge\" attr.name=\"Weight\" attr.type=\"double\"/>\n")
    cat("<graph id=\"G\" edgedefault=\"undirected\">\n")
    
    for (i in 1:D) {
      cat(paste("\t<node id=\"", dimnames(AI)[[1]][i], "\">\n"))
      cat(paste("\t<data key=\"d0\">", clust.mem[i], "</data>\n"))
      cat(paste("\t<data key=\"d1\">", strength[i], "</data>\n"))
      cat(paste("\t<data key=\"d2\">", eigencent[i], "</data>\n"))
      cat(paste("\t<data key=\"d3\">", node.degree[i], "</data>\n"))
      cat(paste("\t</node>\n"))
    }
    
    
    
    n <- 1
    for (i in 1:length(dimnames(AI)[[1]])) {
      for (j in 1:length(dimnames(AI)[[2]])) {
        if(AI[i,j]!=0){
          cat(paste("\t<edge id=\"", n,"\" Source=\"", dimnames(AI)[[1]][i], "\" Target=\"", dimnames(AI)[[2]][j], "\">\n")) 
          cat(paste("\t<data key=\"d4\">", AI[i,j], "</data>\n"))	
          cat(paste("\t</edge>\n"))
          n <- n+1
        }
      }
    }
    
    
    
    cat("</graph>\n")
    cat("</graphml>")
    sink()
  } else if (stats == "stats") {
    cat("Exporting network statistics to file: ", net.stats.name, "\n")
    fwrite(netStats, file = net.stats.name, col.names = TRUE, sep = " ")
    net.info.name <- paste(prefix, "_", "metMZmi", "_NetInfo.txt", sep = "")
    cat("Exporting network info to file", net.info.name, "\n")
    sink(net.info.name)
    cat(paste("The number of nodes: ", num.nodes, "\n"))
    #cat(paste("The number of edges: ", num.int, "\n"))
    cat(paste("Modularity Coefficient: ", Q, "\n"))
    sink()
  }
  
  
  
  
  save(prefix, AI.graph, metMZ.mi, netStats, num.nodes, Q, file="runNetwrkAnalysisRes.Rdata")
  
 
  # write to graphML file
  
  rm(list=ls())
  gc()
}


runViz <- function(met.log, plots = F, file4name) {
  
  
  
  #met.log <- read.table("norm_BS.fil.imp.bat.csv", header=T, sep=",", row.names = 1)
  Group <- met.log$Group
  #met.log <- met.log[,-1]
  met.3 <- as.matrix(met.log[, -1])
  #met.log$Group <- relevel(met.log$Group, ref)
  trans.met.log <- t(met.log[, -1])
  colnames(trans.met.log) <- met.log[, 1]
  cormat <- cor(trans.met.log, use = "complete.obs")
  # create design matrix
  design <- with(met.log, model.matrix(~0 + Group))
  
  # attr(design, 'dimnames')[[2]] <- c('BTAL12', 'BTAL24', 'BTCR10', 'BTCR20',
  # 'BTCR30', 'BTCR40')
  attr(design, "dimnames")[[2]] <- gsub("Group", "", attr(design, "dimnames")[[2]])
 
  meta<-as.matrix(met.log[,-1])
  
  # replace all non-finite values with 0
  meta[!is.finite(meta)] <- 0
  
  plsda.dol<-plsda(meta, Group, ncomp = 2, logratio = "none")
  
  if (is.null(plots)) {
    print("No plot selected...")
  } else if (plots == "heatmap pca lvl rla") {
    png("heatmap.png")
    heatmap(met.3)
    dev.off()
    png("pca.png")
    plotIndiv(plsda.dol, ellipse=TRUE, legend=TRUE, title="Individual Group's score", legend.position="bottom")
    dev.off()
    png("level.png")
    levelplot(cormat, main = "Correlation between samples", scales = list(x = list(rot = 90)))
    dev.off()
    png("RLA_acrossGrp.png")
    RlaPlots(met.log, main = "RLA Plot Across Groups", type = "ag", ylim = c(5, 
                                                                             -5))  #across groups
    dev.off()
    png("RLA_withinGrp.png")
    RlaPlots(met.log, main = "RLA Plot Within Groups", type = "wg", ylim = c(5, 
                                                                             -5))  #within groups
    dev.off()
  } else if (plots == "heatmap pca lvl") {
    png("heatmap.png")
    heatmap(met.3)
    dev.off()
    png("pca.png")
    plotIndiv(plsda.dol, ellipse=TRUE, legend=TRUE, title="Individual Group's score", legend.position="bottom")
    dev.off()
    png("level.png")
    levelplot(cormat, main = "Correlation between samples", scales = list(x = list(rot = 90)))
    dev.off()
  } else if (plots == "heatmap pca rla") {
    png("heatmap.png")
    heatmap(met.3)
    dev.off()
    png("pca.png")
    plotIndiv(plsda.dol, ellipse=TRUE, legend=TRUE, title="Individual Group's score", legend.position="bottom")
    dev.off()
    png("RLA_acrossGrp.png")
    RlaPlots(met.log, main = "RLA Plot Across Groups", type = "ag", ylim = c(5, 
                                                                             -5))  #across groups
    dev.off()
    png("RLA_withinGrp.png")
    RlaPlots(met.log, main = "RLA Plot Within Groups", type = "wg", ylim = c(5, 
                                                                             -5))  #within groups
    dev.off()
  } else if (plots == "heatmap lvl rla") {
    png("heatmap.png")
    heatmap(met.3)
    dev.off()
    png("level.png")
    levelplot(cormat, main = "Correlation between samples", scales = list(x = list(rot = 90)))
    dev.off()
    png("RLA_acrossGrp.png")
    RlaPlots(met.log, main = "RLA Plot Across Groups", type = "ag", ylim = c(5, 
                                                                             -5))  #across groups
    dev.off()
    png("RLA_withinGrp.png")
    RlaPlots(met.log, main = "RLA Plot Within Groups", type = "wg", ylim = c(5, 
                                                                             -5))  #within groups
    dev.off()
  } else if (plots == "pca lvl rla") {
    png("pca.png")
    plotIndiv(plsda.dol, ellipse=TRUE, legend=TRUE, title="Individual Group's score", legend.position="bottom")
    dev.off()
    png("level.png")
    levelplot(cormat, main = "Correlation between samples", scales = list(x = list(rot = 90)))
    dev.off()
    png("RLA_acrossGrp.png")
    RlaPlots(met.log, main = "RLA Plot Across Groups", type = "ag", ylim = c(5, 
                                                                             -5))  #across groups
    dev.off()
    png("RLA_withinGrp.png")
    RlaPlots(met.log, main = "RLA Plot Within Groups", type = "wg", ylim = c(5, 
                                                                             -5))  #within groups
    dev.off()
  } else if (plots == "heatmap pca") {
    png("heatmap.png")
    heatmap(met.3)
    dev.off()
    png("pca.png")
    plotIndiv(plsda.dol, ellipse=TRUE, legend=TRUE, title="Individual Group's score", legend.position="bottom")
    dev.off()
  } else if (plots == "heatmap lvl") {
    png("heatmap.png")
    heatmap(met.3)
    dev.off()
    png("level.png")
    levelplot(cormat, main = "Correlation between samples", scales = list(x = list(rot = 90)))
    dev.off()
  } else if (plots == "heatmap rla") {
    png("heatmap.png")
    heatmap(met.3)
    dev.off()
    png("RLA_acrossGrp.png")
    RlaPlots(met.log, main = "RLA Plot Across Groups", type = "ag", ylim = c(5, 
                                                                             -5))  #across groups
    dev.off()
    png("RLA_withinGrp.png")
    RlaPlots(met.log, main = "RLA Plot Within Groups", type = "wg", ylim = c(5, 
                                                                             -5))  #within groups
    dev.off()
  } else if (plots == "pca lvl") {
    png("pca.png")
    plotIndiv(plsda.dol, ellipse=TRUE, legend=TRUE, title="Individual Group's score", legend.position="bottom")
    dev.off()
    png("level.png")
    levelplot(cormat, main = "Correlation between samples", scales = list(x = list(rot = 90)))
    dev.off()
  } else if (plots == "pca rla") {
    png("pca.png")
    plotIndiv(plsda.dol, ellipse=TRUE, legend=TRUE, title="Individual Group's score", legend.position="bottom")
    dev.off()
    png("RLA_acrossGrp.png")
    RlaPlots(met.log, main = "RLA Plot Across Groups", type = "ag", ylim = c(5, 
                                                                             -5))  #across groups
    dev.off()
    png("RLA_withinGrp.png")
    RlaPlots(met.log, main = "RLA Plot Within Groups", type = "wg", ylim = c(5, 
                                                                             -5))  #within groups
    dev.off()
  } else if (plots == "lvl rla") {
    png("level.png")
    levelplot(cormat, main = "Correlation between samples", scales = list(x = list(rot = 90)))
    dev.off()
    png("RLA_acrossGrp.png")
    RlaPlots(met.log, main = "RLA Plot Across Groups", type = "ag", ylim = c(5, 
                                                                             -5))  #across groups
    dev.off()
    png("RLA_withinGrp.png")
    RlaPlots(met.log, main = "RLA Plot Within Groups", type = "wg", ylim = c(5, 
                                                                             -5))  #within groups
    dev.off()
  } else if (plots == "heatmap") {
    png("heatmap.png")
    
    heatmap(met.3)
    dev.off()
  } else if (plots == "pca") {
    png("pca.png")
    plotIndiv(plsda.dol, ellipse=TRUE, legend=TRUE, title="Individual Group's score", legend.position="bottom")
    dev.off()
  } else if (plots == "lvl") {
    # plot to look at corellations between samples
    png("level.png")
    levelplot(cormat, main = "Correlation between samples", scales = list(x = list(rot = 90)))
    dev.off()
  } else if (plots == "rla") {
    # Produce within group and across group relative log abundance plots to visualise
    # a metabolomics data matrix.
    png("RLA_acrossGrp.png")
    RlaPlots(met.log, main = "RLA Plot Across Groups", type = "ag", ylim = c(5, 
                                                                             -5))  #across groups
    dev.off()
    png("RLA_withinGrp.png")
    RlaPlots(met.log, main = "RLA Plot Within Groups", type = "wg", ylim = c(5, 
                                                                             -5))  #within groups
    dev.off()
  } else if (plots == "pca") {
    # PCA plot of the metabolites and groups
    png("pca.png")
    plotIndiv(plsda.dol, ellipse=TRUE, legend=TRUE, title="Individual Group's score", legend.position="bottom")
    dev.off()
  }
  # else if(plots=='pca'){ png('PCA.png') PCA plot of the metabolites and groups
  # PcaPlots(met.log,varplot=F,multiplot=F) Plot PCA with different PCs
  # PcaPlots(met.log,varplot=F,multiplot=F, y.axis = 3, x.axis = 5 ) dev.off() }
  
  
  # for (i in 1:3) { png(file=paste0(i,'.png'))
  # PcaPlots(met.log,varplot=T,multiplot=T, n=2) dev.off() }
  
  
}

