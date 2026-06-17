### In the combined data of the three hypoxia experiments, the full glucose/glutamax traces are sustained for about 20-30hr before dropping. 
# I want to know, does the length of this sustained barrier vary in timing depending on the starting Rb position of the traces? Higher Rb = longer plateau?

# Testing this with a mixed effects model using starting Rb as fixed effect one. 


# Not a very valid test - ofc is start is lower, plateau and auc will be lower. 

#hypox 1
paired1hypox <- vascr_import("ECIS",
                             raw = "Grafton paired 131 ogd/ECIS_260521_MFT_1_CG_ogdpaired1hypoxrestart.abp",
                             model = "Grafton paired 131 ogd/ECIS_260521_MFT_1_CG_ogdpaired1hypoxrestart_RbAnew.csv", experiment = "exp1")

paired1hypoxkey <- tribble(
  ~SampleID, ~Row, ~Column, ~Sample,
  1, "A", "1 2 3",  "hypox 131 + glutamax + glucose", # A1 iffy in growth phase, excluded
  2, "B", "1 2 3", "hypox 131",
  3, "C", "1 2 3", "hypox 131 + 5mM glucose",
  4, "D", "1 2 3", "hypox 131 + 2mM glutamax")
p1hypox <- vascr:::vascr_apply_map(paired1hypox, paired1hypoxkey) 

# run2
paired2hypox <- vascr_import("ECIS",
                             raw = "Grafton paired 131 ogd/ECIS_260528_MFT_1_CG_ogdpaired2hypox.abp",
                             model = "Grafton paired 131 ogd/ECIS_260528_MFT_1_CG_ogdpaired2hypox_RbA.csv", experiment = "exp2")
paired2hypoxkey <- tribble(
  ~SampleID, ~Row, ~Column, ~Sample,
  1, "A", "7 8 9",  "hypox 131 + glutamax + glucose", # full
  3, "F", "7 8 9", "hypox 131 + 5mM glucose", # no glutamine, glucose only
  4, "A", "10 11 12",  "hypox 131 + 2mM glutamax", 
  2, "F", "10 11 12", "hypox 131") # neither

p2hypox <- vascr:::vascr_apply_map(paired2hypox, paired2hypoxkey) 

#3
paired3hypox <- vascr_import("ECIS",
                             raw = "Grafton paired 131 ogd/ECIS_260609_MFT_1_CG_ogdpaired3hypox.abp",
                             model = "Grafton paired 131 ogd/ECIS_260609_MFT_1_CG_ogdpaired3hypox_RbA.csv", experiment = "exp3")

paired3hypoxkey <- tribble(
  ~SampleID, ~Row, ~Column, ~Sample,
  2, "A", "1 2 3",  "hypox 131", # A1 iffy in growth phase, excluded
  4, "B", "1 2 3", "hypox 131 + 2mM glutamax",
  3, "C", "1 2 3", "hypox 131 + 5mM glucose",
  1, "D", "1 2 3", "hypox 131 + glutamax + glucose", # did glutamax first. PBS in first and third. 
  # extras
  5, "E", "1 2 3", "hypox 131 + 2mM glutamax + 5mM glucose + 5uL HEPES", # 15mM HEPES opposed to the usual 10mM
  6, "F", "1 2 3", "hypox 131 + 2mM glutamax + 2.5mM glucose",# doubled the glucose. 5uL glucose + 5uL
  7, "G", "1 2 3", "hypox 131 + 2mM glutamax + 10mM glucose") # 2.5uL glucose and another 2.5 of water

p3hypox <- vascr:::vascr_apply_map(paired3hypox, paired3hypoxkey) 




######## Need to get starting positions and Rb
p1raw<- p1hypox %>% vascr_zero_time(15.6) %>%  vascr_resample_time(500) %>% 
  vascr_subset(unit= "Rb", time= c(-4, 55), sampleid=c(1:4)) 

p2raw<- p2hypox %>% vascr_zero_time(65.764) %>%  vascr_resample_time(500) %>% 
  vascr_subset(unit= "Rb", time= c(-4, 55), sampleid=c(1:4)) 

p3raw<- p3hypox %>% vascr_zero_time(87.145) %>%  vascr_resample_time(500) %>% 
  vascr_subset(unit= "Rb", time= c(-4, 55), sampleid=c(1:4)) 

allraw<- rbind(p1raw, p2raw, p3raw)

allraw %>% vascr_summarise(level="experiment") %>% vascr_plot_line() + theme_bw()


# dataframe 1 gets starting Rb values, filter by time = 0
p1start<- p1hypox %>% vascr_zero_time(15.6) %>%  vascr_resample_time(500) %>% 
  vascr_subset(unit= "Rb", time= c(-1), sampleid=c(1)) 

p2start<- p2hypox %>% vascr_zero_time(65.764) %>%  vascr_resample_time(500) %>% 
  vascr_subset(unit= "Rb", time= c(-1), sampleid=c(1)) 

p3start<- p3hypox %>% vascr_zero_time(87.145) %>%  vascr_resample_time(500) %>% 
  vascr_subset(unit= "Rb", time= c(-1), sampleid=c(1))  

start<- rbind(p1start, p2start, p3start) %>% group_by(Experiment, Sample, Well) %>% 
  summarise(start=mean(Value)) # summarise mean isn't doing anything, all wells separately, but gives just the important columns

#something in time subsetting wont let me do all three experiments together without overridding/leaving out.





# dataframe 2 gets AUC using summary functions

auc<- allraw %>% vascr_subset(time=c(0, 55), sampleid=c(1)) %>% 
  group_by(Experiment, Sample, Well) %>% summarise(auc= sm_auc(Time, Value))

library(smplot2)
library(lme4)

lmedata<- full_join(auc, start)

auc<- lmer(auc ~ start + (1|Experiment), data = lmedata)
summary(auc)

anova(auc)


ggplot(lmedata, aes(x = start, y = auc, color = Experiment)) +
  geom_point(size = 3) +
  geom_smooth(method = "lm", se = FALSE) +
  theme_minimal()



