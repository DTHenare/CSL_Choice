library(tidyr)
library(dplyr)
library(ggplot2)

paths = c('CNV_StimOnsetWide/','CNV_HannaStimOnsetWide/')
groupInfos = c('SophiaGroupAssignment_provisional.csv','HannaGroupAssignment_provisional.csv')

for (ind in 1:2) {
dPath = paths[ind]
baseline = 800
groupdata = read.csv(file=groupInfos[ind])
rejCrit = 700

fPrefix = 'CNV'

#Organisevgroup data
groupdata = groupdata %>% mutate(Group = ifelse(Group == 1, "Colour", "Shape"))
groupdata$Reject = as.factor(groupdata$Reject)

#####
#Creates aggregate of all participant data (needs dPath and fPrefix)
eFilePattern = paste(fPrefix,"*_epochs.csv", sep="")
fFilePattern = paste(fPrefix,"*_Fz.csv", sep="")
pFilePattern = paste(fPrefix,"*_Pz.csv", sep="")
eFileList = list.files(dPath, pattern=glob2rx(eFilePattern))
fFileList = list.files(dPath, pattern=glob2rx(fFilePattern))
pFileList = list.files(dPath, pattern=glob2rx(pFilePattern))

#create variables using first dataset
epochInfo = read.csv(file = paste(dPath,eFileList[1], sep=""))
epochInfo$Subject = 1
epochInfo$Group = groupdata$Group[1]
epochInfo$Reject = groupdata$Reject[1]
FzData = read.csv(file = paste(dPath,fFileList[1], sep=""), header = FALSE)
PzData = read.csv(file = paste(dPath,pFileList[1], sep=""), header = FALSE)
#append the other datasets to the above variables
for (subj in 2:length(eFileList)) {
  curEpochInfo = read.csv(file = paste(dPath,eFileList[subj], sep=""))
  curEpochInfo$Subject = subj
  curEpochInfo$Group = groupdata$Group[subj]
  curEpochInfo$Reject = groupdata$Reject[subj]
  curFzData = read.csv(file = paste(dPath,fFileList[subj], sep=""), header = FALSE)
  curPzData = read.csv(file = paste(dPath,pFileList[subj], sep=""), header = FALSE)
  
  epochInfo = rbind(epochInfo, curEpochInfo)
  FzData = rbind(FzData, curFzData)
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
rm(curEpochInfo,curFzData,curPzData, fPrefix, eFileList, eFilePattern, fFileList, fFilePattern, pFileList, pFilePattern, subj, groupdata)
gc()
#####
#combine all the data together into one long table
gathercols = colnames(FzData)
FzData$Chan = "Fz"
PzData$Chan = "Pz"
scalpData = rbind(FzData,PzData)
epochInfo = rbind(epochInfo,epochInfo)

allData <- cbind(epochInfo, scalpData)
rm(epochInfo,FzData,PzData,scalpData)
gc()
allData <- gather(allData, "sample", "voltage", gathercols, factor_key = TRUE)

#Tidy variable names etc. and create any necessary variables
allData$sample <- as.integer(substring(allData$sample,2))
allData <- allData %>% mutate(RepSwitch = substring(allData$Event,15))
allData <- allData %>% mutate(RepSwitch = ifelse((RepSwitch == "p" | RepSwitch == "ep"),"Repetition","Switch"))
allData <- allData %>% mutate(TaskChoice = substring(Event,1,5)) %>% mutate(TaskChoice = ifelse(TaskChoice=="Searc","Search","Learn"))
allData$Chan <- factor(allData$Chan, levels=c('Fz', 'Pz'))

#clear stuff that I don't need
rm(gathercols)
allData$StimTrig = NULL
allData$Event = NULL
gc()
#####

#Remove bad participants
Keepers <- allData %>% filter(Chan =="Fz", sample == 1) %>% group_by(Subject) %>% summarise(n = n()) %>% filter(n>rejCrit) %>% select("Subject")
Keepers <- as.character(Keepers$Subject)

plotWidth = 24
plotHeight = 9

plotData <- allData %>%
  filter(Subject %in% Keepers) %>%
  mutate(sample = sample-baseline) %>%
  group_by(TaskChoice, sample, Chan) %>%
  summarise(mean = mean(voltage))
save(plotData, file = paste(dPath,"plotData_TaskChoice.RData",sep=""))

plotData <- allData %>%
  filter(Subject %in% Keepers) %>%
  mutate(sample = sample-baseline) %>%
  group_by(RepSwitch, sample, Chan) %>%
  summarise(mean = mean(voltage))
save(plotData, file = paste(dPath,"plotData_RepSwitch.RData",sep=""))

plotData <- allData %>%
  filter(Subject %in% Keepers) %>%
  mutate(sample = sample-baseline) %>%
  group_by(TaskChoice,RepSwitch, sample, Chan) %>%
  summarise(mean = mean(voltage))
save(plotData, file = paste(dPath,"plotData_TaskChoiceXrepswitch.RData",sep=""))

rm(allData)
gc()
}

load('CNV_StimOnsetWide/plotData_TaskChoice.RData')
choicePlot <- plotData
choicePlot$Exp = "Voluntary"
load('CNV_HannaStimOnsetWide/plotData_TaskChoice.RData')
plotData$Exp = "Cued"
plotData <- rbind(choicePlot,plotData)

plotData %>%
  group_by(sample, Chan, Exp) %>%
  summarise(mean = mean(mean)) %>%
  ggplot(., aes(sample, mean, colour = Exp)) +
  geom_line(size=3) +
  #scale_color_manual(values=c("#000000", "#CC0000")) +
  scale_x_continuous(name ="Latency (ms)", expand = c(0, 0)) +
  scale_y_reverse(name =expression(paste("Amplitude (",mu,"v)")), expand = c(0, 0)) +
  facet_grid(Chan~.,scales = "free_y") +
  geom_vline(xintercept = 0,linetype = "dashed" )+
  geom_hline(yintercept = 0,linetype = "dashed") +
  theme_minimal() +
  theme(panel.spacing.y = unit(2, "lines"),text= element_text(size=60))
ggsave(paste(dPath,"CNVERPs_Average.pdf",sep=""),width = plotWidth, height = plotHeight*2)

plotData %>%
  ggplot(., aes(sample, mean)) +
  geom_rect(xmin = -700, xmax=-500, ymin = -2, ymax = 2, size = 0, fill = "gray70", alpha = 0.05) +
  geom_line(aes( colour = Exp, linetype = TaskChoice), size = 1.5) +
  #scale_color_manual(values=c("#000000", "#CC0000")) +
  scale_x_continuous(name ="Latency (ms)", expand = c(0, 0)) +
  scale_y_reverse(name =expression(paste("Amplitude (",mu,"v)")), expand = c(0, 0)) +
  facet_grid(Chan~.) +
  #geom_rect(xmin = -700, xmax=-500, ymin = -Inf, ymax = Inf, size = 0, fill = "gray40", alpha = 0.1) +
  geom_vline(xintercept = 0,linetype = "dashed" )+
  geom_vline(xintercept = -500,linetype = "dashed" )+
  geom_hline(yintercept = 0,linetype = "dashed") +
  theme_minimal() +
theme(panel.spacing.y = unit(2, "lines"),text= element_text(size=60))
ggsave(paste(dPath,"CNVERPs_TaskChoice.pdf",sep=""),width = plotWidth, height = plotHeight*2)

load('CNV_StimOnsetWide/plotData_RepSwitch.RData')
choicePlot <- plotData
choicePlot$Exp = "Voluntary"
load('CNV_HannaStimOnsetWide/plotData_RepSwitch.RData')
plotData$Exp = "Cued"
plotData <- rbind(choicePlot,plotData)

plotData %>%
  ggplot(., aes(sample, mean)) +
  geom_rect(xmin = -700, xmax=-500, ymin = -2, ymax = 2, size = 0, fill = "gray70", alpha = 0.05) +
  geom_line(aes( colour = Exp, linetype = RepSwitch), size = 1.5) +
  #scale_color_manual(values=c("#000000", "#CC0000")) +
  scale_x_continuous(name ="Latency (ms)", expand = c(0, 0)) +
  scale_y_reverse(name =expression(paste("Amplitude (",mu,"v)")), expand = c(0, 0)) +
  facet_grid(Chan~.) +
  #geom_rect(xmin = -700, xmax=-500, ymin = -Inf, ymax = Inf, size = 0, fill = "gray40", alpha = 0.1) +
  geom_vline(xintercept = 0,linetype = "dashed" )+
  geom_vline(xintercept = -500,linetype = "dashed" )+
  geom_hline(yintercept = 0,linetype = "dashed") +
  theme_minimal() +
  theme(panel.spacing.y = unit(2, "lines"),text= element_text(size=60))
ggsave(paste(dPath,"CNVERPs_RepSwitch.pdf",sep=""),width = plotWidth, height = plotHeight*2)

load('CNV_StimOnsetWide/plotData_TaskChoiceXrepswitch.RData')
choicePlot <- plotData
choicePlot$Exp = "Voluntary"
load('CNV_HannaStimOnsetWide/plotData_TaskChoiceXrepswitch.RData')
plotData$Exp = "Cued"
plotData <- rbind(choicePlot,plotData)

plotData %>%
  ggplot(., aes(sample, mean)) +
  geom_rect(xmin = -700, xmax=-500, ymin = -2, ymax = 2, size = 0, fill = "gray70", alpha = 0.05) +
  geom_line(aes( colour = TaskChoice, linetype = RepSwitch), size = 1.5) +
  #scale_color_manual(values=c("#000000", "#CC0000")) +
  scale_x_continuous(name ="Latency (ms)", expand = c(0, 0)) +
  scale_y_reverse(name =expression(paste("Amplitude (",mu,"v)")), expand = c(0, 0)) +
  facet_grid(Chan~Exp) +
  #geom_rect(xmin = -700, xmax=-500, ymin = -Inf, ymax = Inf, size = 0, fill = "gray40", alpha = 0.1) +
  geom_vline(xintercept = 0,linetype = "dashed" )+
  geom_vline(xintercept = -500,linetype = "dashed" )+
  geom_hline(yintercept = 0,linetype = "dashed") +
  theme_minimal() +
  theme(panel.spacing.y = unit(2, "lines"),text= element_text(size=60))
ggsave(paste(dPath,"CNVERPs_TaskChoiceXrepswitch.pdf",sep=""),width = plotWidth, height = plotHeight*2)