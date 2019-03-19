library(tidyr)
library(dplyr)
library(ggplot2)

dPath = 'LRP/'
fPrefix = 'LRP'

#Load group data
groupdata = read.csv(file="LRP/SophiaGroupAssignment_provisional.csv")
groupdata = groupdata %>% mutate(Group = ifelse(Group == 1, "Colour", "Shape"))
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

#clear stuff that I don't need
rm(curEpochInfo,curLHemData,curRHemData, fPrefix, eFileList, eFilePattern, lFileList, lFilePattern, rFileList, rFilePattern, subj, groupdata)
#####
#combine all the data together into one long table
gathercols = colnames(lHemData)
lHemData$Hem = "Left"
rHemData$Hem = "Right"
scalpData = rbind(lHemData,rHemData)
epochInfo = rbind(epochInfo,epochInfo)

allData <- cbind(epochInfo, scalpData)
allData <- gather(allData, "sample", "voltage", gathercols, factor_key = TRUE)

#Tidy variable names etc. and create any necessary variables - could use unite
allData$sample <- as.integer(substring(allData$sample,2))
allData <- allData %>% mutate(Hemifield = ifelse(substring(Event,1,5)=="Learn", "Left", "Right"))
allData <- allData %>% mutate(Contra = ifelse(Hemifield==Hem, "Ipsilateral", "Contralateral"))
allData <- allData %>% mutate(Run = substring(allData$Event,15))
allData <- allData %>% mutate(Run = ifelse((Run == "p" | Run == "ep"),"Repetition","Switch"))

#clear stuff that I don't need
rm(epochInfo,lHemData,rHemData,scalpData, gathercols)
#####
baseline = 1000
plotWidth = 6
plotHeight = 2.25

allData %>%
  mutate(sample = sample-baseline) %>%
  group_by(Run, sample, Contra, Group) %>%
  summarise(mean = mean(voltage)) %>%
  ggplot(., aes(sample, mean)) +
  geom_line(aes(colour = Contra),size=0.5) +
  scale_color_manual(values=c("#000000", "#CC0000")) +
  scale_x_continuous(name ="Latency (ms)", expand = c(0, 0)) +
  scale_y_reverse(name =expression(paste("Amplitude (",mu,"v)")), expand = c(0, 0)) +
  facet_grid(Run~Group) +
  geom_vline(xintercept = -800,linetype = "dashed" )+
  geom_hline(yintercept = 0,linetype = "dashed") +
  theme_minimal() +
  theme(panel.spacing.y = unit(2, "lines"))
ggsave("LRPsUnsub.pdf",width = plotWidth, height = plotHeight*3)

allData %>%
  mutate(sample = sample-baseline) %>%
  group_by(Run, sample, Contra, Group) %>%
  summarise(mean = mean(voltage)) %>%
  spread(Contra, mean) %>% 
  mutate(diff = Contralateral - Ipsilateral) %>%
  ggplot(., aes(sample, diff)) +
  geom_line(aes(colour = Group),size=0.5) +
  scale_colour_brewer(palette = "Set1") +
  scale_x_continuous(name ="Latency (ms)", expand = c(0, 0)) +
  scale_y_reverse(name =expression(paste("Amplitude (",mu,"v)")), expand = c(0, 0)) +
  facet_grid(Run~.) +
  geom_vline(xintercept = -800,linetype = "dashed" )+
  geom_hline(yintercept = 0,linetype = "dashed") +
  theme_minimal() +
  theme(panel.spacing.y = unit(2, "lines"))
ggsave("LRPsSub.pdf",width = plotWidth, height = plotHeight*3)