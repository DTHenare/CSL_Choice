dPath = 'F:/CSL Choice/Data/EEG (raw data)/Visuals/'
fPrefix = 'N2pc'

#Load group data
groupdata = read.csv(file="F:/CSL Choice/Scripts/SophiaGroupAssignment_provisional.csv")
groupdata = groupdata %>% mutate(Group = as.factor(ifelse(Group == 1, "Colour", "Shape")))
groupdata$Reject = as.factor(groupdata$Reject)

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
epochInfo$Group = groupdata$Group[1]
epochInfo$Reject = groupdata$Reject[1]
lHemData = read.csv(file = paste(dPath,lFileList[1], sep=""), header = FALSE)
rHemData = read.csv(file = paste(dPath,rFileList[1], sep=""), header = FALSE)
#append the other datasets to the above variables
for (subj in 2:length(eFileList)) {
  curEpochInfo = read.csv(file = paste(dPath,eFileList[subj], sep=""))
  curEpochInfo$Subject = subj
  curEpochInfo$Group = groupdata$Group[subj]
  curEpochInfo$Reject = groupdata$Reject[subj]
  curLHemData = read.csv(file = paste(dPath,lFileList[subj], sep=""), header = FALSE)
  curRHemData = read.csv(file = paste(dPath,rFileList[subj], sep=""), header = FALSE)
  
  epochInfo = rbind(epochInfo, curEpochInfo)
  lHemData = rbind(lHemData, curLHemData)
  rHemData = rbind(rHemData, curRHemData)
}
epochInfo$Subject = as.factor(epochInfo$Subject)

#clear stuff that I don't need
rm(curEpochInfo,curLHemData,curRHemData, fPrefix, eFileList, eFilePattern, lFileList, lFilePattern, rFileList, rFilePattern, subj, groupdata)
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

allData <- allData %>% mutate(Contra = as.factor(ifelse(Hemifield==Hem, "Ipsilateral", "Contralateral")))
allData <- recode(allData, )
#clear stuff that I don't need
rm(epochInfo,lHemData,rHemData,scalpData, gathercols)
#####
library(ggplot2)
allData %>%
  filter(Event == "Search" & LatStim != "None" & Reject == 0) %>%
  group_by(LatStim,sample,Contra,Group) %>%
  summarise(mean = mean(voltage)) %>%
  ggplot(., aes(sample, mean)) +
    geom_line(aes(linetype = Contra)) +
    facet_grid(Group~LatStim) +
    scale_y_reverse()
  