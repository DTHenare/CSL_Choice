library(dplyr)
library(tidyr)
library(ggplot2)
#Need to run helpfulPlotFunctions for violins

#Load group data
behavdata = read.csv(file="BehaviouralData.csv")
behavdata = behavdata %>% mutate(GroupLabels = ifelse(Group == 1, "Colour", "Shape"))

#Calculate distractor interference effect
behavdata = behavdata %>% mutate(DistInt = SearchTDrt-SearchTOrt)

#Stats comparing distractor intereference between groups
t.test(filter(behavdata,GroupLabels=="Colour")$DistInt,filter(behavdata,GroupLabels=="Shape")$DistInt)

#PlotS
#First reproduce bar graph that has been used in past
behavdata %>%
  gather("TrialType", "RT", colnames(behavdata)[4:5]) %>%
  group_by(TrialType,Group,GroupLabels) %>%
  summarise(meanRT = mean(RT), seRT = sd(RT)/sqrt(n())) %>%
  ggplot(aes(x = TrialType, y=meanRT, fill = GroupLabels)) +
  geom_col(alpha = .5) +
  geom_errorbar(aes(ymin = meanRT - seRT, ymax = meanRT + seRT), width=0.2)+
  scale_colour_brewer(palette = "Set1")+
  scale_fill_brewer(palette = "Set1")+
  facet_grid(.~GroupLabels) +
  theme_minimal()

behavdata %>%
  filter(SwitchRate>0.2) %>%
ggplot( aes(x = Group, y = DistInt, fill = GroupLabels)) +
  geom_flat_violin(aes(fill = GroupLabels),position = position_nudge(x = .1, y = 0), adjust = 1.5, trim = FALSE, alpha = .5, colour = NA)+
  geom_point(aes(x = Group-0.25, y = DistInt, colour = GroupLabels),position = position_jitter(width = .05), size = 4, shape = 20)+
  geom_boxplot(aes(x = Group, y = DistInt, fill = GroupLabels),outlier.shape = NA, alpha = .5, width = .1, colour = "black")+
  scale_colour_brewer(palette = "Set1")+
  scale_fill_brewer(palette = "Set1")+
  labs(y = "Distractor Interference (ms)") +
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        axis.line = element_line(colour = "black"),
        axis.text.x = element_blank(),
        axis.title.x = element_blank(),
        axis.ticks = element_blank(),
        text= element_text(size=30))
ggsave('test.pdf', width = 15, height = 10)

#####
#Now let's look at switch rate!

#Stats comparing switch rate between groups
t.test(filter(behavdata,GroupLabels=="Colour"&SwitchRate>0.2)$SwitchRate,filter(behavdata,GroupLabels=="Shape"&SwitchRate>0.2)$SwitchRate)

behavdata %>%
  filter(SwitchRate>0.2) %>%
ggplot( aes(x = Group, y = SwitchRate, fill = GroupLabels)) +
  geom_flat_violin(aes(fill = GroupLabels),position = position_nudge(x = .1, y = 0), adjust = 1.5, trim = FALSE, alpha = .5, colour = NA)+
  geom_point(aes(x = Group-0.25, y = SwitchRate, colour = GroupLabels),position = position_jitter(width = .05), size = 4, shape = 20)+
  geom_boxplot(aes(x = Group, y = SwitchRate, fill = GroupLabels),outlier.shape = NA, alpha = .5, width = .1, colour = "black")+
  scale_colour_brewer(palette = "Set1")+
  scale_fill_brewer(palette = "Set1")+
  labs(y = "Switch Rate") +
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        axis.line = element_line(colour = "black"),
        axis.text.x = element_blank(),
        axis.title.x = element_blank(),
        axis.ticks = element_blank(),
        text= element_text(size=30))

#Correlations
behavdata %>%
ggplot(aes(x = DistInt, y = SwitchRate, fill = GroupLabels)) +
  geom_point()
library(Hmisc)
rcorr(behavdata$SwitchRate, behavdata$DistInt)
