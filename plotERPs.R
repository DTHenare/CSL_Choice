library(tidyr)
library(dplyr)
library(ggplot2)
library(afex)

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
allData <- allData %>% mutate(Object = as.factor(paste(Group,LatStim, sep="")))
allData$Object <- recode(allData$Object, ShapePredictor = "Shape", ColourPredictor="Colour", ShapeNonPred = "Colour", ColourNonPred = "Shape")

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
    theme(panel.spacing.y = unit(2, "lines")) +
    coord_fixed(ratio = 10)
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
ggsave("LearnLERPs.pdf",width = plotWidth, height = plotHeight*2)
allData %>%
  filter(Event == "Learn" & Reject == 0) %>%
  mutate(sample = sample-baseline) %>%
  group_by(Object,sample,Contra,Group) %>%
  summarise(mean = mean(voltage)) %>%
  spread(Contra, mean) %>% 
  mutate(diff = Contralateral - Ipsilateral) %>%
  ggplot(., aes(sample,diff)) +
  geom_line(aes(colour = Group),size=1) +
  scale_colour_brewer(palette = "Set1") +
  scale_x_continuous(name ="Latency (ms)", expand = c(0, 0)) +
  scale_y_reverse(name =expression(paste("Amplitude (",mu,"v)")), expand = c(0, 0)) +
  facet_grid(Object~.) +
  geom_vline(xintercept = 0,linetype = "dashed" )+
  geom_hline(yintercept = 0,linetype = "dashed") +
  theme_minimal() +
  theme(panel.spacing.y = unit(2, "lines"))

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
ggsave("SearchLERPs.pdf",width = plotWidth, height = plotHeight*3)
#####
#Running stats
learnData <- allData %>%
  mutate(sample = sample-baseline) %>%
  filter(Event == "Learn" & Reject == 0 & (sample>200 | sample < 300)) %>%
  group_by(LatStim,Subject,Contra,Group) %>%
  summarise(mV = mean(voltage))
searchData <- allData %>%
  mutate(sample = sample-baseline) %>%
  filter(Event == "Search" & LatStim != "None" & Reject == 0 & (sample>200 | sample < 300)) %>%
  group_by(Stimulus,Subject,Contra,Group) %>%
  summarise(mV = mean(voltage))

learn.aov <- aov_ez(
  data = learnData,
  dv = "mV",
  id = "Subject", 
  within = c("Contra", "LatStim"),
  between = "Group"
)

search.aov <- aov_ez(
  data = searchData,
  dv = "mV",
  id = "Subject", 
  within = c("Contra", "Stimulus"),
  between = "Group"
)



dist.load.emmeans <- emmeans(output.aov, ~ Contra)
dist.component.emmeans <- emmeans(dist.aov, ~ Component)
dist.interaction.load.emmeans <- emmeans(dist.aov, ~Load|Component)
dist.load.posthoc <- pairs(dist.load.emmeans)
dist.component.posthoc <- pairs(dist.component.emmeans)
dist.interaction.load.posthoc <- pairs(dist.interaction.load.emmeans)