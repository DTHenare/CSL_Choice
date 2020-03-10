library(tidyr)
library(dplyr)
library(ggplot2)

paths = c('CNV_StimOnsetWide/','CNV_HannaStimOnsetWide/')
groupInfos = c('SophiaGroupAssignment_provisional.csv','HannaGroupAssignment_provisional.csv')

for (ind in 1:length(paths)) {
dPath = paths[ind]
baseline = 800
groupdata = read.csv(file=groupInfos[ind])
rejCrit = 700

fPrefix = 'CNV'


rm(allData)
gc()
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
  group_by(TaskChoice,RepSwitch, sample, Chan, Subject) %>%
  summarise(mean = mean(voltage))
save(plotData, file = paste(dPath,"plotData_TaskChoiceXrepswitch.RData",sep=""))

}

plotWidth = 24
plotHeight = 9
dPath = 'F:/CSL_Choice/CNV_StimOnsetWide/'
load('CNV_StimOnsetWide/plotData_TaskChoiceXrepswitch.RData')
choicePlot <- plotData
choicePlot$Exp = "Voluntary"
choicePlot= choicePlot %>% mutate(Subject = paste("vol",as.character(Subject),sep =""))
load('CNV_HannaStimOnsetWide/plotData_TaskChoiceXrepswitch.RData')
plotData$Exp = "Cued"
plotData= plotData %>% mutate(Subject = paste("cue",as.character(Subject),sep =""))
plotData <- rbind(choicePlot,plotData)

x = c(-800,-500)
y=c(0,0)
lineData = data.frame(x=x,y=y)

plotData %>%
  group_by(sample, Chan, Exp) %>%
  summarise(mean = mean(mean)) %>%
  ggplot() +
  geom_rect(xmin = -300, xmax=-0, ymin = -Inf, ymax = Inf, size = 0, fill = "lemonchiffon") +
  #geom_rect(xmin = -700, xmax=-500, ymin = -Inf, ymax = Inf, size = 0, fill = "gray85") +
  geom_line(aes(sample, mean, colour = Exp),size=1) +
  scale_x_continuous(name ="Latency (ms)", expand = c(0, 0)) +
  scale_y_reverse(name =expression(paste("Amplitude (",mu,"v)")), expand = c(0, 0)) +
  facet_grid(Chan~.,scales = "free_y") +
  geom_vline(xintercept = 0,linetype = "dashed" )+
  geom_vline(xintercept = -500)+
  geom_hline(yintercept = 0,linetype = "dashed") +
  geom_line(data = lineData,aes(x=x,y=y)) +
  theme_minimal()
  theme(panel.spacing.y = unit(2, "lines"),text= element_text(size=60))
ggsave(paste(dPath,"CNVERPs_Average.png",sep=""),width = 10, height = 6.49)

plotData %>%
  group_by(sample, Chan, Exp, TaskChoice) %>%
  summarise(mean = mean(mean)) %>%
  ggplot() +
  geom_rect(xmin = -300, xmax=-0, ymin = -Inf, ymax = Inf, size = 0, fill = "lemonchiffon") +
  #geom_rect(xmin = -700, xmax=-500, ymin = -Inf, ymax = Inf, size = 0, fill = "gray85") +
  geom_line(aes(x = sample,y=mean, colour = Exp, linetype = TaskChoice), size = 1.5) +
  scale_x_continuous(name ="Latency (ms)", expand = c(0, 0)) +
  scale_y_reverse(name =expression(paste("Amplitude (",mu,"v)")), expand = c(0, 0)) +
  facet_grid(Chan~.) +
  geom_vline(xintercept = 0,linetype = "dashed" )+
  geom_vline(xintercept = -500)+
  geom_hline(yintercept = 0,linetype = "dashed") +
  geom_line(data = lineData,aes(x=x,y=y)) +
  theme_minimal() +
theme(panel.spacing.y = unit(2, "lines"),text= element_text(size=60))
ggsave(paste(dPath,"CNVERPs_TaskChoice.png",sep=""),width = plotWidth, height = plotHeight*2)

plotData %>%
  group_by(sample, Chan, Exp, RepSwitch) %>%
  summarise(mean = mean(mean)) %>%
  ggplot() +
  geom_rect(xmin = -300, xmax=-0, ymin = -Inf, ymax = Inf, size = 0, fill = "lemonchiffon") +
  #geom_rect(xmin = -700, xmax=-500, ymin = -Inf, ymax = Inf, size = 0, fill = "gray85") +
  geom_line(aes(x = sample,y=mean, colour = Exp, linetype = RepSwitch), size = 1.5) +
  scale_x_continuous(name ="Latency (ms)", expand = c(0, 0)) +
  scale_y_reverse(name =expression(paste("Amplitude (",mu,"v)")), expand = c(0, 0)) +
  facet_grid(Chan~.) +
  geom_vline(xintercept = 0,linetype = "dashed" )+
  geom_vline(xintercept = -500)+
  geom_hline(yintercept = 0,linetype = "dashed") +
  geom_line(data = lineData,aes(x=x,y=y)) +
  theme_minimal() +
  theme(panel.spacing.y = unit(2, "lines"),text= element_text(size=60))
ggsave(paste(dPath,"CNVERPs_RepSwitch.png",sep=""),width = plotWidth, height = plotHeight*2)

plotData %>%
  group_by(sample, Chan, Exp, RepSwitch, TaskChoice) %>%
  summarise(mean = mean(mean)) %>%
  ggplot() +
  geom_rect(xmin = -300, xmax=-0, ymin = -Inf, ymax = Inf, size = 0, fill = "lemonchiffon") +
  #geom_rect(xmin = -700, xmax=-500, ymin = -Inf, ymax = Inf, size = 0, fill = "gray85") +
  geom_line(aes( x = sample,y=mean,colour = TaskChoice, linetype = RepSwitch), size = 1) +
  scale_x_continuous(name ="Latency (ms)", expand = c(0, 0)) +
  scale_y_reverse(name =expression(paste("Amplitude (",mu,"v)")), expand = c(0, 0)) +
  facet_grid(Chan~Exp) +
  geom_vline(xintercept = 0,linetype = "dashed" )+
  geom_vline(xintercept = -500)+
  geom_hline(yintercept = 0,linetype = "dashed") +
  geom_line(data = lineData,aes(x=x,y=y)) +
  theme_minimal() 
  theme(panel.spacing.y = unit(2, "lines"),text= element_text(size=60))
ggsave(paste(dPath,"CNVERPs_TaskChoiceXrepswitch.png",sep=""),width = 10, height = 6.49)

frontalData <- plotData %>% filter(Chan == "Fz", sample > -300, sample<0)
posteriorData <- plotData %>% filter(Chan == "Pz", sample > -300, sample<0)
afex::aov_ez(
  data = frontalData,
  dv = "mean",
  id = "Subject", 
  within = c("RepSwitch", "TaskChoice"),
  between = "Exp"
)
afex::aov_ez(
  data = posteriorData,
  dv = "mean",
  id = "Subject",
  within = c("RepSwitch", "TaskChoice"),
  between = "Exp"
)

frontalData <- frontalData %>% group_by(sample, Exp) %>% summarise(mean = mean(mean))
t.test(frontalData$mean[frontalData$Exp == "Voluntary"],frontalData$mean[frontalData$Exp == "Cued"],paired = 0)
posteriorData <- posteriorData %>% group_by(sample, Exp) %>% summarise(mean = mean(mean))
t.test(posteriorData$mean[posteriorData$Exp == "Voluntary"],posteriorData$mean[posteriorData$Exp == "Cued"],paired = 0)
