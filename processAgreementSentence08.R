library(ggplot2)
library(plot.matrix) 
library(reshape2)
library(irr)
library(tidyr)
library(viridis)

#load table and anonymize
table4Judges = read.table("bigTableLanguages.txt", header = T)
table4Judges$judge_name[table4Judges$judge_name == "ferragne"] = "judge 1"
table4Judges$judge_name[table4Judges$judge_name == "guyotTalbot"] = "judge 2"
table4Judges$judge_name[table4Judges$judge_name == "king"] = "judge 3"
table4Judges$judge_name[table4Judges$judge_name == "navarro"] = "judge 4"

#convert to wide format
wideTab = spread(table4Judges, judge_name, british)

#compute percent agreement all features
resultsAgree = matrix(nrow = 4,ncol = 4)
rownames(resultsAgree) = unique(table4Judges$judge_name)
colnames(resultsAgree) = unique(table4Judges$judge_name)

for (idx in 5:8){
  data1 = wideTab[,idx]
  for (jdx in 5:8){
    data2 = wideTab[,jdx]
    currentData = cbind(data1,data2)
    resultsAgree[idx-4,jdx-4] = as.numeric(agree(currentData)$value)
  }
}

resultsAgree = round(resultsAgree, 2)
meltMat = melt(resultsAgree)
colnames(meltMat) = c("judge1", "judge2", "percent_agreement")
ggplot(data = meltMat, aes(x=judge1, y=judge2, 
                           fill=percent_agreement)) + geom_tile() + geom_text(aes(label=percent_agreement)) +
  scale_fill_gradientn(colours=heat.colors(5),na.value = "transparent",
                       breaks=c(80, 90, 100),
                       limits=c(80,100)) + ggtitle("All responses") +
  theme(axis.text.x= element_text(size = 18), axis.text.y= element_text(size = 18),
        axis.title.x = element_blank(), axis.title.y = element_blank(),
        plot.title = element_text(size = 24,hjust=0.5))

#same thing without zeros
noZeroTable = table4Judges[table4Judges$british != 0,]
wideTabNZ = spread(noZeroTable, judge_name, british)
resultsAgree = matrix(nrow = 4,ncol = 4)
rownames(resultsAgree) = unique(noZeroTable$judge_name)
colnames(resultsAgree) = unique(noZeroTable$judge_name)

for (idx in 5:8){
  data1 = wideTabNZ[,idx]
  for (jdx in 5:8){
    data2 = wideTabNZ[,jdx]
    currentData = cbind(data1,data2)
    resultsAgree[idx-4,jdx-4] = round(as.numeric(agree(currentData)$value),2)
  }
}

meltMat = melt(resultsAgree)
colnames(meltMat) = c("judge1", "judge2", "percent_agreement")
ggplot(data = meltMat, aes(x=judge1, y=judge2, 
                           fill=percent_agreement)) + geom_tile() + geom_text(aes(label=percent_agreement)) +
  scale_fill_gradientn(colours= heat.colors(5), na.value = "transparent",
                       breaks=c(85, 90, 100),
                       limits=c(85,100)) + ggtitle("Responses without '?'") +
  theme(axis.text.x= element_text(size = 18), axis.text.y= element_text(size = 18),
        axis.title.x = element_blank(), axis.title.y = element_blank(),
        plot.title = element_text(size = 24,hjust=0.5))

#now let's split by feature
byFeature = split(wideTab, wideTab$feature)
nbFeatures = length(byFeature)

byFeatureAgreements = unlist(lapply(byFeature, function (x) agree(x[,5:8])$value))
featureList = names(byFeature)
dfFeatureAgree = data.frame(featureList, as.vector(t(byFeatureAgreements)))
colnames(dfFeatureAgree) = c("feature", "percent")

ggplot(data=dfFeatureAgree, aes(x=reorder(feature, -percent), y=percent)) +
  geom_bar(stat="identity", fill="steelblue") +
  coord_flip() +
  ggtitle("by feature percent agreement") +
  theme(axis.text.x= element_text(size = 18), axis.text.y= element_text(size = 18),
        axis.title.x = element_blank(), axis.title.y = element_blank(),
        plot.title = element_text(size = 24,hjust=0.5))

zeroTable = table4Judges[table4Judges$british == 0,]
zeroByJudge = as.data.frame(xtabs(~judge_name, data = zeroTable))
zeroByFeature = as.data.frame(xtabs(~feature, data = zeroTable))
numberByFeature = as.data.frame(xtabs(~feature, data = table4Judges))
percentZeroByFeature = zeroByFeature$Freq/numberByFeature$Freq*100
zeroByFeature$percent = percentZeroByFeature

ggplot(zeroByJudge, aes(reorder(judge_name, -Freq), Freq)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  ggtitle("number of zeros") +
  theme(axis.text.x= element_text(size = 18), axis.text.y= element_text(size = 18),
        axis.title.x = element_blank(), axis.title.y = element_blank(),
        plot.title = element_text(size = 24,hjust=0.5))

ggplot(zeroByFeature, aes(reorder(feature, Freq), Freq)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  coord_flip() +
  ggtitle("zeros by feature") +
  theme(axis.text.x= element_text(size = 18), axis.text.y= element_text(size = 18),
        axis.title.x = element_blank(), axis.title.y = element_blank(),
        plot.title = element_text(size = 24,hjust=0.5))

ggplot(zeroByFeature, aes(reorder(feature, percent), percent)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  coord_flip() +
  ggtitle("percent zeros by feature") +
  theme(axis.text.x= element_text(size = 18), axis.text.y= element_text(size = 18),
        axis.title.x = element_blank(), axis.title.y = element_blank(),
        plot.title = element_text(size = 24,hjust=0.5))


