
## datExpr: rows=samples, columns=genes
## dat: rows=samples, columns=genes

############################
## FUNCTION: geneFilter() ##
############################

geneFilter <- function(dat, mean.thred, sd.thred){ 
  gmean <- rowMeans(dat)
  gsd <- apply(dat,1,sd)
  
  i.keep <- which(gmean>mean.thred & gsd>sd.thred)
  i.keep
}


###############################
## FUNCTION: normalization() ## make column-wise unit vectors
###############################

normalization <- function(datExpr){
  datExpr.centered <- sweep(datExpr, 2, colMeans(datExpr), "-")
  norm <- sqrt(colSums(datExpr.centered^2))
  datExpr.normed <- sweep(datExpr.centered, 2, norm, "/")
  datExpr.normed
}

############################
## FUNCTION: METcorplot() ##
############################

METcorplot <- function(MEs, trait, ...){
  MET <- orderMEs(cbind(MEs, trait))
  plotEigengeneNetworks(MET, "Correlation between eigengenes and clinical traits",
                        marHeatmap = c(5,6,2,2), cex.lab = 0.8,
                        plotAdjacency = FALSE, printAdjacency = TRUE,
                        plotDendrograms = FALSE, xLabelsAngle = 90, ...)
}

#############################
## FUNCTION: METexprplot() ##
#############################

METexprplot <- function(MEs, trait, MEcolor, datExpr, moduleColors, ...){
  ME <- MEs[names(MEs)==paste0("ME",MEcolor)]
  annotation_col <- data.frame(ME,trait)
  annotation_colors <- list(colorRampPalette(rev(brewer.pal(n = 7, name ="RdYlBu")))(100))
  names(annotation_colors) <- paste0("ME",MEcolor)
  pheatmap(t(datExpr[order(ME),moduleColors==MEcolor]), scale="none", 
           cluster_rows=TRUE, show_rownames=TRUE, cluster_cols=FALSE, show_colnames=FALSE, 
           annotation_col=annotation_col, annotation_colors=annotation_colors, ...)
}

