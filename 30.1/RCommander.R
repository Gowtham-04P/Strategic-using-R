
Dataset <- read.table("D:/stategic for r/bikeshareanalysis/raw data/bikeshare.csv", header=TRUE, 
  stringsAsFactors=TRUE, sep="", na.strings="NA", dec=".", strip.white=TRUE)
Dataset <- read.table("D:/stategic for r/bikeshareanalysis/raw data/bikeshare.csv", header=TRUE, 
  stringsAsFactors=TRUE, sep=",", na.strings="NA", dec=".", strip.white=TRUE)
library(abind, pos=16)
library(e1071, pos=17)
numSummary(Dataset[,"rental", drop=FALSE], statistics=c("mean", "sd", "IQR", "quantiles"), quantiles=c(0,.25,.5,
  .75,1))
with(Dataset, Hist(rental, groups=season, scale="frequency", breaks="Sturges", col="darkgray"))
local({
  .Table <- with(Dataset, table(season))
  cat("\ncounts:\n")
  print(.Table)
  cat("\npercentages:\n")
  print(round(100*.Table/sum(.Table), 2))
})
library(mvtnorm, pos=18)
library(survival, pos=18)
library(MASS, pos=18)
library(TH.data, pos=18)
library(multcomp, pos=18)
AnovaModel.2 <- aov(rental ~ season, data = Dataset)
summary(AnovaModel.2)
with(Dataset, numSummary(rental, groups = season, statistics=c('mean', 'sd')))
Boxplot( ~ rental, data=Dataset, id=list(method="y"))

