
##Aggregated counts data from all featurecounts output and imported into R
##R code for EdgeR
## this is the code without filtering the genes based on expression counts manually. Used filterbyExpr instead
library(edgeR)
combined_counts <- as.data.frame(combined_counts_male)

x<-combined_counts
row.names(x)<-x$Geneid
x<-x[,-1]
group <- c(rep("A",ncol(x)))
# GETMM based normalization
rpk <- (x[,2:ncol(x)]/x[,1])
# remove length col in x
x <- x[,-1]
# for normalization purposes, no grouping of samples
group <- c(rep("A",ncol(x)))
rpk.norm <- DGEList(counts=rpk,group=group)
rpk.norm <- calcNormFactors(rpk.norm)
norm.counts.rpk_edger <- cpm(rpk.norm)
norm_data<-norm.counts.rpk_edger
View(norm_data)
group <- c(rep("Obp56h_KO_M",2),rep("CSB_M",2))
group<-factor(group)
y <- DGEList(norm_data,group = group,genes=combined_counts[,2,drop=FALSE])
options(digits = 3)
design <- model.matrix(~0+group)
colnames(design)<-levels(group)
keep <-filterByExpr(y,design)
table(keep)
x<-y[keep, ,keep.lib.sizes=FALSE]
y<-x

#Calculate dispersion for input parameter in model fitting.
y<-estimateDisp(y,design,robust = TRUE)
#Change the fit to any other available options. The number of DGE does change significantly based on the model fit.
fit <- glmFit(y,design,robust=TRUE)
#Change contasts based on the comparison you would like to make. This will be the first comparison.
KO_vs_CSB <-makeContrasts(Obp56h_KO_M-CSB_M,levels=design)
res <- glmLRT(fit,contrast = KO_vs_CSB)
is.de<-decideTestsDGE(res)
summary(is.de)
plotMD(res,status = is.de,values=c(1,-1),col=c("blue","red"),legend="topright", main="KO vs CSB (males)")
#Set topTags n to the full set to get all gene (statistically significant or otherwise). TopTags uses BH FDR correction by default.
KO_vs_CSB_output <- topTags(res,n=7823)
write.csv(KO_vs_CSB_output,file="KO_vs_CSB_male.csv")

##Input FB_IDs into flybase to get gene symbols.
#Heatmap visualziation based on the top DGE from this comparison. Heatmap also does hierarchical clustering of samples based on this set of genes.
logCPM <- cpm(y, prior.count=2, log=TRUE)
rownames(logCPM)<-rownames(y$genes)
o<-order(res$table$PValue)
logCPM<-logCPM[o[1:50],]
colnames(logCPM)<-c("KO_1","KO_2","CSB_1","CSB_2")
coolmap(logCPM,margins=c(7,7),lhei=c(1,6),lwid=c(1,3))
