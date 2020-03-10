library(tidyr)
library(dplyr)
library(ggplot2)
library(RColorBrewer)
library(scales)

dPath = 'CNV_ChoiceInitForward/'
baseline = 200
groupdata = read.csv(file='SophiaGroupAssignment_provisional.csv')
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

allData <- allData %>% filter(RT<1000)
allData$deciles = ntile(allData$RT,10)
allData <- allData %>% mutate(sample = sample-baseline)
rtLines <- allData %>%
  group_by(Group,Chan,deciles) %>%
  summarise(RT = mean(RT))
hmData <- allData %>%
  filter(Subject %in% Keepers) %>%
  group_by(Group,sample, Chan, deciles) %>%
  summarise(mean = mean(voltage))
ggplot() +
  geom_tile(data = hmData, aes(sample,deciles,fill = mean)) +
  geom_line(data = rtLines,aes(RT,deciles),size = 1.5) +
  scale_fill_gradient2(low = "blue", mid ="white", high = "red",limits = c(-2.5,2.5),oob=squish) + 
  facet_grid(Chan~Group) +
  scale_y_continuous(breaks = seq(1, 10, by = 1),name ="RT decile (1 = fastest, 10 = slowest)", expand = c(0, 0)) +
  scale_x_continuous(breaks = seq(-200, 1000, by = 200),name ="Latency (ms)", expand = c(0, 0)) +
  theme_minimal()

plotData <- allData %>%
  filter(Subject %in% Keepers, RT<1000) %>%
  group_by(TaskChoice,RepSwitch, sample, Chan) %>%
  summarise(mean = mean(voltage))
save(plotData, file = paste(dPath,"plotData_TaskChoiceXrepswitch.RData",sep=""))

load(paste(dPath,"plotData_TaskChoiceXrepswitch.RData",sep=""))
plotData %>%
  group_by(sample, Chan) %>%
  summarise(mean = mean(mean)) %>%
  ggplot() +
  geom_line(aes(sample,mean)) +
  scale_x_continuous(name ="Latency (ms)", expand = c(0, 0)) +
  scale_y_reverse(name =expression(paste("Amplitude (",mu,"v)")), expand = c(0, 0)) +
  facet_grid(Chan~.) +
  geom_vline(xintercept = 0)+
  geom_hline(yintercept = 0) +
  theme_minimal()
ggsave(paste(dPath,"CNVERPs_Average.png",sep=""),width = 10, height = 6.49)

plotData %>%
  group_by(sample, Chan, TaskChoice) %>%
  summarise(mean = mean(mean)) %>%
  ggplot() +
  geom_line(aes(sample,mean, linetype = TaskChoice)) +
  scale_x_continuous(name ="Latency (ms)", expand = c(0, 0)) +
  scale_y_reverse(name =expression(paste("Amplitude (",mu,"v)")), expand = c(0, 0)) +
  facet_grid(Chan~.) +
  geom_vline(xintercept = 0)+
  geom_hline(yintercept = 0) +
  theme_minimal()

plotData %>%
  group_by(sample, Chan, RepSwitch) %>%
  summarise(mean = mean(mean)) %>%
  ggplot() +
  geom_line(aes(sample,mean, colour = RepSwitch)) +scale_x_continuous(name ="Latency (ms)", expand = c(0, 0)) +
  scale_y_reverse(name =expression(paste("Amplitude (",mu,"v)")), expand = c(0, 0)) +
  facet_grid(Chan~.) +
  geom_vline(xintercept = 0)+
  geom_hline(yintercept = 0) +
  theme_minimal()

plotData %>%
  group_by(sample, Chan,TaskChoice,RepSwitch) %>%
  summarise(mean = mean(mean)) %>%
  ggplot() +
  geom_line(aes(sample,mean,linetype = TaskChoice,colour = RepSwitch)) +scale_x_continuous(name ="Latency (ms)", expand = c(0, 0)) +
  scale_y_reverse(name =expression(paste("Amplitude (",mu,"v)")), expand = c(0, 0)) +
  facet_grid(Chan~.) +
  geom_vline(xintercept = 0)+
  geom_hline(yintercept = 0) +
  theme_minimal()
ggsave(paste(dPath,"CNVERPs_TaskChoiceXRepSwtich.png",sep=""),width = 10, height = 6.49)
