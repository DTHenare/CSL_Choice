library(tidyr)
library(dplyr)
library(ggplot2)

dPath = 'CNV/'
fPrefix = 'CNV'

#Load group data
groupdata = read.csv(file="CNV/SophiaGroupAssignment_provisional.csv")
groupdata = groupdata %>% mutate(Group = ifelse(Group == 1, "Colour", "Shape"))
groupdata$Reject = as.factor(groupdata$Reject)

#####
#Creates aggregate of all participant data (needs dPath and fPrefix)
eFilePattern = paste(fPrefix,"*_epochs.csv", sep="")
fFilePattern = paste(fPrefix,"*_Fz.csv", sep="")
cFilePattern = paste(fPrefix,"*_Cz.csv", sep="")
pFilePattern = paste(fPrefix,"*_Pz.csv", sep="")
eFileList = list.files(dPath, pattern=glob2rx(eFilePattern))
fFileList = list.files(dPath, pattern=glob2rx(fFilePattern))
cFileList = list.files(dPath, pattern=glob2rx(cFilePattern))
pFileList = list.files(dPath, pattern=glob2rx(pFilePattern))

#create variables using first dataset
epochInfo = read.csv(file = paste(dPath,eFileList[1], sep=""))
epochInfo$Subject = 1
epochInfo$Group = groupdata$Group[1]
epochInfo$Reject = groupdata$Reject[1]
FzData = read.csv(file = paste(dPath,fFileList[1], sep=""), header = FALSE)
CzData = read.csv(file = paste(dPath,cFileList[1], sep=""), header = FALSE)
PzData = read.csv(file = paste(dPath,pFileList[1], sep=""), header = FALSE)
#append the other datasets to the above variables
for (subj in 2:length(eFileList)) {
  curEpochInfo = read.csv(file = paste(dPath,eFileList[subj], sep=""))
  curEpochInfo$Subject = subj
  curEpochInfo$Group = groupdata$Group[subj]
  curEpochInfo$Reject = groupdata$Reject[subj]
  curFzData = read.csv(file = paste(dPath,fFileList[subj], sep=""), header = FALSE)
  curCzData = read.csv(file = paste(dPath,cFileList[subj], sep=""), header = FALSE)
  curPzData = read.csv(file = paste(dPath,pFileList[subj], sep=""), header = FALSE)
  
  epochInfo = rbind(epochInfo, curEpochInfo)
  FzData = rbind(FzData, curFzData)
  CzData = rbind(CzData, curCzData)
  PzData = rbind(PzData, curPzData)
}
#Tidy the variables, remove unnecessary and convert to factors
epochInfo$Subject = as.factor(epochInfo$Subject)
epochInfo$Group = as.factor(epochInfo$Group)
epochInfo$VarName8 = NULL
epochInfo$VarName9 = NULL
epochInfo$VarName10 = NULL
epochInfo$VarName11 = NULL
epochInfo$LatStim=NULL
epochInfo$MidStim=NULL
epochInfo$TrialType=NULL
epochInfo$Timepoint=NULL
epochInfo$Hemifield=NULL

#clear stuff that I don't need
rm(curEpochInfo,curFzData,curCzData,curPzData, fPrefix, eFileList, eFilePattern, fFileList, fFilePattern, cFileList, cFilePattern, pFileList, pFilePattern, subj, groupdata)
#####
#combine all the data together into one long table
gathercols = colnames(FzData)
FzData$Chan = "Fz"
CzData$Chan = "Cz"
PzData$Chan = "Pz"
scalpData = rbind(FzData,CzData,PzData)
epochInfo = rbind(epochInfo,epochInfo,epochInfo)

allData <- cbind(epochInfo, scalpData)
rm(epochInfo,FzData,CzData,PzData,scalpData)
gc()
allData <- gather(allData, "sample", "voltage", gathercols, factor_key = TRUE)

#Tidy variable names etc. and create any necessary variables
allData$sample <- as.integer(substring(allData$sample,2))
allData <- allData %>% mutate(RepSwitch = substring(allData$Event,15))
allData <- allData %>% mutate(RepSwitch = ifelse((RepSwitch == "p" | RepSwitch == "ep"),"Repetition","Switch"))
allData <- allData %>% mutate(TaskChoice = substring(Event,1,5)) %>% mutate(TaskChoice = ifelse(TaskChoice=="Searc","Search","Learn"))

#clear stuff that I don't need
rm(gathercols)
allData$StimTrig = NULL
allData$Event = NULL
gc()
#####
baseline = 1000
plotWidth = 24
plotHeight = 9

allData %>%
  mutate(sample = sample-baseline) %>%
  group_by(RepSwitch, sample, Chan, Group) %>%
  summarise(mean = mean(voltage)) %>%
  ggplot(., aes(sample, mean)) +
  geom_line(aes(colour = RepSwitch),size=3) +
  scale_color_manual(values=c("#000000", "#CC0000")) +
  scale_x_continuous(name ="Latency (ms)", expand = c(0, 0)) +
  scale_y_reverse(name =expression(paste("Amplitude (",mu,"v)")), expand = c(0, 0)) +
  facet_grid(Chan~Group,scales = "free_y") +
  geom_vline(xintercept = -800,linetype = "dashed" )+
  geom_hline(yintercept = 0,linetype = "dashed") +
  theme_minimal() +
  theme(panel.spacing.y = unit(2, "lines"),text= element_text(size=60))
ggsave("CNVERPs.pdf",width = plotWidth, height = plotHeight*2)
