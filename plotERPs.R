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
plotWidth = 24
plotHeight = 9
#Categorization ERPs
allData %>%
  filter(Event == "Learn" & Reject == 0) %>%
  mutate(sample = sample-baseline) %>%
  group_by(LatStim,sample,Contra,Group) %>%
  summarise(mean = mean(voltage)) %>%
  ggplot(., aes(sample, mean)) +
    geom_line(aes(colour = Contra),size=0.25) +
    scale_color_manual(values=c("#000000", "#CC0000")) +
    scale_x_continuous(name ="Latency (ms)", expand = c(0, 0)) +
    scale_y_reverse(name =expression(paste("Amplitude (",mu,"v)")), expand = c(0, 0), limits=c(0,15)) +
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
ggsave("LearnLERPs.pdf",width = plotWidth, height = plotHeight*2)
allData %>%
  filter(Event == "Learn" & Reject == 0) %>%
  mutate(sample = sample-baseline) %>%
  group_by(Object,sample,Contra,Group) %>%
  summarise(mean = mean(voltage)) %>%
  spread(Contra, mean) %>% 
  mutate(diff = Contralateral - Ipsilateral) %>%
  ggplot(., aes(sample,diff)) +
  geom_line(aes(colour = Group),size=5) +
  scale_colour_brewer(palette = "Set1") +
  scale_x_continuous(name ="Latency (ms)", expand = c(0, 0)) +
  scale_y_reverse(name =expression(paste("Amplitude (",mu,"v)")), expand = c(0, 0)) +
  facet_grid(Object~.) +
  geom_vline(xintercept = 0,linetype = "dashed" )+
  geom_hline(yintercept = 0,linetype = "dashed") +
  theme_minimal() +
  theme(panel.spacing.y = unit(2, "lines"), text= element_text(size=60))
ggsave("LearnLERPs.pdf",width = plotWidth, height = plotHeight*2)
#Search ERPs
allData %>%
  filter(Event == "Search" & LatStim != "None" & Reject == 0) %>%
  mutate(sample = sample-baseline) %>%
  group_by(Stimulus,sample,Contra,Group) %>%
  summarise(mean = mean(voltage)) %>%
  ggplot(., aes(sample, mean)) +
    geom_line(aes(colour = Contra),size=2) +
    scale_color_manual(values=c("#000000", "#CC0000")) +
    scale_x_continuous(name ="Latency (ms)", expand = c(0, 0)) +
    scale_y_reverse(name =expression(paste("Amplitude (",mu,"v)")), expand = c(0, 0)) +
    facet_grid(Stimulus~Group) +
    geom_vline(xintercept = 0,linetype = "dashed" )+
    geom_hline(yintercept = 0,linetype = "dashed") +
    theme_minimal() +
    theme(panel.spacing.y = unit(2, "lines"),text= element_text(size=30))
ggsave("SearchERPs.pdf",width = plotWidth, height = plotHeight)
#Subtracted
allData %>%
  filter(Event == "Search" & LatStim != "None" & Reject == 0) %>%
  mutate(sample = sample-baseline) %>%
  group_by(Stimulus,sample,Contra,Group) %>%
  summarise(mean = mean(voltage)) %>%
  spread(Contra, mean) %>% 
  mutate(diff = Contralateral - Ipsilateral) %>%
  ggplot(., aes(sample,diff)) +
    geom_line(aes(colour = Group),size=5) +
    scale_colour_brewer(palette = "Set1") +
    scale_x_continuous(name ="Latency (ms)", expand = c(0, 0)) +
    scale_y_reverse(name =expression(paste("Amplitude (",mu,"v)")), expand = c(0, 0)) +
    facet_grid(Stimulus~.) +
    geom_vline(xintercept = 0,linetype = "dashed" )+
    geom_hline(yintercept = 0,linetype = "dashed") +
    theme_minimal() +
    theme(panel.spacing.y = unit(2, "lines"),text= element_text(size=60))
ggsave("SearchLERPs.pdf",width = plotWidth, height = plotHeight*3)
#####
#Running stats
learnData <- allData %>%
  mutate(sample = sample-baseline) %>%
  filter(Event == "Learn" & Reject == 0 & sample>200 & sample < 300 & Object == "Shape") %>%
  group_by(Object,Subject,Contra,Group) %>%
  summarise(mV = mean(voltage))
learn.aov <- aov_ez(
  data = learnData,
  dv = "mV",
  id = "Subject", 
  within = c("Contra"),
  between = "Group"
)
learnData <- allData %>%
  mutate(sample = sample-baseline) %>%
  filter(Event == "Learn" & Reject == 0 & sample>200 & sample < 300 & Object == "Shape") %>%
  group_by(Object,Subject,Contra,Group) %>%
  summarise(mV = mean(voltage))%>% 
  spread(Contra, mV) %>% 
  mutate(diff = Contralateral - Ipsilateral)
ggplot(learnData, aes(Group, diff, fill=Group)) +
  geom_flat_violin(position = position_nudge(x = .25, y = 0),adjust =2)+
  geom_point(position = position_jitter(width = .15), size = 2)+
  #note that here we need to set the x-variable to a numeric variable and bump it to get the boxplots to line up with the rainclouds. 
  geom_boxplot(aes(x = as.numeric(Group)+0.25, y = diff),outlier.shape = NA, alpha = 0.3, width = .1, colour = "BLACK") + 
  ylab('mV diff')+xlab('Group')+theme_cowplot()+guides(fill = FALSE, colour = FALSE)
learnData <- allData %>%
  mutate(sample = sample-baseline) %>%
  filter(Event == "Learn" & Reject == 0 & sample>200 & sample < 300) %>%
  group_by(Object,Subject,Contra,Group) %>%
  summarise(mV = mean(voltage))%>% 
  spread(Contra, mV) %>% 
  mutate(diff = Contralateral - Ipsilateral)
ggplot(learnData, aes(x = Object, y = diff, fill = Group)) +
  geom_flat_violin(aes(fill = Group),position = position_nudge(x = .1, y = 0), adjust = 1.5, trim = FALSE, alpha = .5, colour = NA)+
  geom_point(aes(x = Object, y = diff, colour = Group),position = position_jitter(width = .05), size = 1, shape = 20)+
  geom_boxplot(aes(x = Object, y = diff, fill = Group),outlier.shape = NA, alpha = .5, width = .1, colour = "black")+
  scale_colour_brewer(palette = "Dark2")+
  scale_fill_brewer(palette = "Dark2")+
  ggtitle("Figure R10: Repeated Measures Factorial Rainclouds")
ggsave('10repanvplot.png', width = w, height = h)

searchData <- allData %>%
  mutate(sample = sample-baseline) %>%
  filter(Event == "Search" & LatStim != "None" & Reject == 0 & sample>250 & sample < 350) %>%
  group_by(Stimulus,Subject,Contra,Group) %>%
  summarise(mV = mean(voltage))
searchData %>%
  spread(mV, c("Stimulus", "Contra"))
search.aov <- aov_ez(
  data = searchData,
  dv = "mV",
  id = "Subject", 
  within = c("Contra", "Stimulus"),
  between = "Group"
)

library(emmeans)

dist.load.emmeans <- emmeans(output.aov, ~ Contra)
dist.component.emmeans <- emmeans(dist.aov, ~ Component)
dist.interaction.load.emmeans <- emmeans(search.aov, ~Contra|Stimulus)
dist.load.posthoc <- pairs(dist.load.emmeans)
dist.component.posthoc <- pairs(dist.component.emmeans)
dist.interaction.load.posthoc <- pairs(dist.interaction.load.emmeans)
