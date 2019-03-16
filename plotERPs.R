library(tidyr)
library(dplyr)
library(ggplot2)

dPath = 'CSL_Visuals/'
fPrefix = 'N2pc'

#Load group data
groupdata = read.csv(file="CSL_Visuals/SophiaGroupAssignment_provisional.csv")
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
allData <- allData %>% mutate(Contra = ifelse(Hemifield==Hem, "Ipsilateral", "Contralateral"))
allData <- allData %>% mutate(Stimulus = paste(LatStim," (",MidStim," mid)"))

#clear stuff that I don't need
rm(epochInfo,lHemData,rHemData,scalpData, gathercols)
#####
baseline = 200
plotWidth = 6
plotHeight = 2.25
#Categorization ERPs
allData %>%
  filter(Event == "Learn" & Reject == 0) %>%
  mutate(sample = sample-baseline) %>%
  group_by(LatStim,sample,Contra,Group) %>%
  summarise(mean = mean(voltage)) %>%
  ggplot(., aes(sample, mean)) +
    geom_line(aes(colour = Contra),size=0.5) +
    scale_color_manual(values=c("#000000", "#CC0000")) +
    scale_x_continuous(name ="Latency (ms)", expand = c(0, 0)) +
    scale_y_reverse(name =expression(paste("Amplitude (",mu,"v)")), expand = c(0, 0)) +
    facet_grid(LatStim~Group) +
    geom_vline(xintercept = 0,linetype = "dashed" )+
    geom_hline(yintercept = 0,linetype = "dashed") +
    theme_minimal() +
    theme(panel.spacing.y = unit(2, "lines"))
ggsave("LearnERPs.pdf",width = plotWidth, height = plotHeight*2)
#Subtracted
allData %>%
  filter(Event == "Learn" & Reject == 0) %>%
  mutate(sample = sample-baseline) %>%
  group_by(LatStim,sample,Contra,Group) %>%
  summarise(mean = mean(voltage)) %>%
  spread(Contra, mean) %>% 
  mutate(diff = Contralateral - Ipsilateral) %>%
  ggplot(., aes(sample,diff)) +
    geom_line(aes(colour = Group),size=1) +
    scale_colour_brewer(palette = "Set1") +
    scale_x_continuous(name ="Latency (ms)", expand = c(0, 0)) +
    scale_y_reverse(name =expression(paste("Amplitude (",mu,"v)")), expand = c(0, 0)) +
    facet_grid(LatStim~.) +
    geom_vline(xintercept = 0,linetype = "dashed" )+
    geom_hline(yintercept = 0,linetype = "dashed") +
    theme_minimal() +
    theme(panel.spacing.y = unit(2, "lines"))
ggsave("LearnLRPs.pdf",width = plotWidth, height = plotHeight*2)

#Search ERPs
allData %>%
  filter(Event == "Search" & LatStim != "None" & Reject == 0) %>%
  mutate(sample = sample-baseline) %>%
  group_by(Stimulus,sample,Contra,Group) %>%
  summarise(mean = mean(voltage)) %>%
  ggplot(., aes(sample, mean)) +
    geom_line(aes(colour = Contra),size=0.5) +
    scale_color_manual(values=c("#000000", "#CC0000")) +
    scale_x_continuous(name ="Latency (ms)", expand = c(0, 0)) +
    scale_y_reverse(name =expression(paste("Amplitude (",mu,"v)")), expand = c(0, 0)) +
    facet_grid(Stimulus~Group) +
    geom_vline(xintercept = 0,linetype = "dashed" )+
    geom_hline(yintercept = 0,linetype = "dashed") +
    theme_minimal() +
    theme(panel.spacing.y = unit(2, "lines"))
ggsave("SearchERPs.pdf",width = plotWidth, height = plotHeight*3)
#Subtracted
allData %>%
  filter(Event == "Search" & LatStim != "None" & Reject == 0) %>%
  mutate(sample = sample-baseline) %>%
  group_by(Stimulus,sample,Contra,Group) %>%
  summarise(mean = mean(voltage)) %>%
  spread(Contra, mean) %>% 
  mutate(diff = Contralateral - Ipsilateral) %>%
  ggplot(., aes(sample,diff)) +
    geom_line(aes(colour = Group),size=1) +
    scale_colour_brewer(palette = "Set1") +
    scale_x_continuous(name ="Latency (ms)", expand = c(0, 0)) +
    scale_y_reverse(name =expression(paste("Amplitude (",mu,"v)")), expand = c(0, 0)) +
    facet_grid(Stimulus~.) +
    geom_vline(xintercept = 0,linetype = "dashed" )+
    geom_hline(yintercept = 0,linetype = "dashed") +
    theme_minimal() +
    theme(panel.spacing.y = unit(2, "lines"))
ggsave("SearchLRPs.pdf",width = plotWidth, height = plotHeight*3)