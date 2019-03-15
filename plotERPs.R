dPath = 'F:/CSL Choice/Data/EEG (raw data)/Visuals/'
fPrefix = 'N2pc'

conditions = c("epochInfo$Event==\"Search\" & epochInfo$LatStim==\"Target\" ",
               "epochInfo$Event==\"Search\" & epochInfo$LatStim==\"Distractor\" "
               )

rightHemCond = '& epochInfo$Hemifield==\"Right\" '
leftHemCond = '& epochInfo$Hemifield==\"Left\" '

#####
#Creates aggregate of all participant data (needs dPath and fPrefix)
eFilePattern = paste(fPrefix,"*_epochs.csv", sep="")
lFilePattern = paste(fPrefix,"*_LH.csv", sep="")
rFilePattern = paste(fPrefix,"*_RH.csv", sep="")
eFileList = list.files(dPath, pattern=glob2rx(eFilePattern))
lFileList = list.files(dPath, pattern=glob2rx(lFilePattern))
rFileList = list.files(dPath, pattern=glob2rx(rFilePattern))

#create variables using first dataset
epochInfo = read.csv(file = paste(dPath,eFileList[1], sep=""))
epochInfo$Subject = 1
lHemData = read.csv(file = paste(dPath,lFileList[1], sep=""), header = FALSE)
rHemData = read.csv(file = paste(dPath,rFileList[1], sep=""), header = FALSE)
#append the other datasets to the above variables
for (subj in 2:length(eFileList)) {
  curEpochInfo = read.csv(file = paste(dPath,eFileList[subj], sep=""))
  curEpochInfo$Subject = subj
  curLHemData = read.csv(file = paste(dPath,lFileList[subj], sep=""), header = FALSE)
  curRHemData = read.csv(file = paste(dPath,rFileList[subj], sep=""), header = FALSE)
  
  epochInfo = rbind(epochInfo, curEpochInfo)
  lHemData = rbind(lHemData, curLHemData)
  rHemData = rbind(rHemData, curRHemData)
}
epochInfo$Subject = as.factor(epochInfo$Subject)

#clear stuff that I don't need
rm(curEpochInfo,curLHemData,curRHemData, fPrefix, eFileList, eFilePattern, lFileList, lFilePattern, rFileList, rFilePattern, subj)
#####
#combine all the data together into one long table
library(tidyr)
library(dplyr)
gathercols = colnames(lHemData)
lHemData$Hem = "Left"
rHemData$Hem = "Right"
scalpData = rbind(lHemData,rHemData)
epochInfo = rbind(epochInfo,epochInfo)

allData <- cbind(epochInfo, scalpData)
allData <- gather(allData, "sample", "voltage", gathercols, factor_key = TRUE)

allData$sample <- as.integer(substring(allData$sample,2))

#clear stuff that I don't need
rm(epochInfo,lHemData,rHemData,scalpData, gathercols)
#####
library(ggplot2)
plot <- allData %>%
  group_by(LatStim,sample) %>%
  summarise(mean = mean(voltage)) %>%
  ggplot(., aes(sample, mean)) + geom_line(aes(linetype = LatStim))