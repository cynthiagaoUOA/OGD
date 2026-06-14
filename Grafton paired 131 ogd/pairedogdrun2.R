# run 2, weird


library(vascr)
library(tidyverse)
library(ggplot2)

paired2hypox <- vascr_import("ECIS",
                             raw = "Grafton paired 131 ogd/ECIS_260528_MFT_1_CG_ogdpaired2hypox.abp",
                             model = "Grafton paired 131 ogd/ECIS_260528_MFT_1_CG_ogdpaired2hypox_RbA.csv", experiment = "hypox"
)

paired2normox <- vascr_import("ECIS",
                              raw = "Grafton paired 131 ogd/ECIS_260528_MFT_1_CG_ogdpaired2normox.abp",
                              model = "Grafton paired 131 ogd/ECIS_260528_MFT_1_CG_ogdpaired2normox_RbA.csv", experiment = "normox"
)

paired2hypoxkey <- tribble(
  ~SampleID, ~Row, ~Column, ~Sample,
  1, "A", "7 8 9",  "hypox 131 + 2mM glutamax + glucose", # full
  2, "B", "7 8 9", "hypox 131 + 1mM glutamax + glucose",
  3, "C", "7 8 9", "hypox 131 + 0.5mM glutamax + glucose",
  4, "D", "7 8 9", "hypox 131 + 0.1mM glutamax + glucose",
  5, "E", "7 8 9", "hypox 131 + 0.05mM glutamax + glucose", 
  6, "F", "7 8 9", "hypox 131 + glucose", # no glutamine, glucose only

  
  7, "A", "10 11 12",  "hypox 131 + 2mM glutamax", 
  8, "B", "10 11 12", "hypox 131 + 1mM glutamax",
  9, "C", "10 11 12", "hypox 131 + 0.5mM glutamax",
  10, "D", "10 11 12", "hypox 131 + 0.1mM glutamax",
  11, "E", "10 11 12", "hypox 131 + 0.05mM glutamax", 
  12, "F", "10 11 12", "hypox 131", # neither
  
  13, "H", "1 2 3", "131 growth changed to EGM at treatment"
  
  
)


p2hypox <- vascr:::vascr_apply_map(paired2hypox, paired2hypoxkey) 

p2hypox %>%  vascr_zero_time(65.764) %>%  vascr_resample_time(500) %>%  vascr_normalise(-2, divide = TRUE) %>% 
  vascr_subset(unit= "Rb", time= c(-5, 48)) %>% 
  vascr_subset(sampleid= c(1:6)) %>% 
  vascr_summarise(level="experiment") %>%  vascr_plot_line()

p2hypox %>%  vascr_zero_time(65.764) %>%  vascr_resample_time(500) %>%  vascr_normalise(-2, divide = TRUE) %>% 
  vascr_subset(unit= "Rb", time= c(-5, 48)) %>% 
  vascr_subset(sampleid= c(1, 6, 12, 7)) %>% 
  vascr_summarise(level="experiment") %>%  vascr_plot_line()

## normox

paired2normoxkey <- tribble(
  ~SampleID, ~Row, ~Column, ~Sample,
  101, "A", "7 8 9",  " 131 + 2mM glutamax + glucose", # full
  102, "B", "7 8 9", " 131 + 1mM glutamax + glucose",
  103, "C", "7 8 9", " 131 + 0.5mM glutamax + glucose",
  104, "D", "7 8 9", " 131 + 0.1mM glutamax + glucose",
  105, "E", "7 8 9", " 131 + 0.05mM glutamax + glucose", 
  106, "F", "7 8 9", " 131 + glucose", # no glutamine, glucose only
  
  
  107, "A", "10 11 12",  " 131 + 2mM glutamax", 
  108, "B", "10 11 12", " 131 + 1mM glutamax",
  109, "C", "10 11 12", " 131 + 0.5mM glutamax",
  110, "D", "10 11 12", " 131 + 0.1mM glutamax",
  111, "E", "10 11 12", " 131 + 0.05mM glutamax", 
  112, "F", "10 11 12", " 131", # neither
  
  113, "G", "4 5 6", "131 growth changed to EGM at treatment",
  
  114, "E F", "4 5 6", "EGM growth curve",
  
  200, "G", "10 11 12", "Full with CoCl2", 
  201, "H", "4 5 6", "neither CoCl2", 
  202, "H", "7 8 9", "no glucose CoCl2", 
  203, "H", "10 11 12", "no glutamax CoCl2"
  
  
)

p2norm <- vascr:::vascr_apply_map(paired2normox, paired2normoxkey) %>% drop_na()

paired<- vascr_combine(p2norm, p2hypox) 

paired %>% vascr_zero_time(65.764) %>%  vascr_resample_time(500) %>%  vascr_normalise(-2, divide = TRUE) %>% 
  vascr_subset(unit= "Rb", time= c(-5, 48)) %>% 
  vascr_subset(sampleid= c(1, 6, 7, 12, 101, 106, 107, 112)) %>% 
  vascr_summarise(level="experiment") %>%  vascr_plot_line()+ylim(0,1.5) + facet_wrap(~Experiment)


paired %>% vascr_zero_time(65.764) %>%  vascr_resample_time(500) %>%  vascr_normalise(-2, divide = TRUE) %>% 
  vascr_subset(unit= c("Cm", "alpha"), time= c(-5, 48)) %>% 
  vascr_subset(sampleid= c(1, 6, 7, 12, 101, 106, 107, 112)) %>% 
  vascr_summarise(level="experiment") %>%  vascr_plot_line()+ylim(0,1.5) + facet_wrap(~unit)

## EGM and 131 comparison
# Rb
Rbmedias<- paired %>% vascr_zero_time(0) %>%  vascr_resample_time(500) %>%  
  vascr_subset(unit= c("Rb"), time= c(0, 80)) %>% 
  vascr_subset(sampleid= c(114, 101)) %>% 
  vascr_summarise(level="experiment") %>%  vascr_plot_line()
# R
Rmedias<- paired %>% vascr_zero_time(0) %>%  vascr_resample_time(500) %>%  
  vascr_subset(unit= "R", frequency="4000", time= c(0, 80)) %>% 
  vascr_subset(sampleid= c(114, 101)) %>% 
  vascr_summarise(level="experiment") %>%  vascr_plot_line()

# Cm
Cmmedias<- paired %>% vascr_zero_time(0) %>%  vascr_resample_time(500) %>%  
  vascr_subset(unit= "Cm", time= c(1, 80)) %>% # starts really high, think artifact
  vascr_subset(sampleid= c(114, 101)) %>% 
  vascr_summarise(level="experiment") %>%  vascr_plot_line()
# alpha
alphamedias<- paired %>% vascr_zero_time(0) %>%  vascr_resample_time(500) %>%  
  vascr_subset(unit= "alpha", time= c(1, 80)) %>% # starts really high, think artifact
  vascr_subset(sampleid= c(114, 101)) %>% 
  vascr_summarise(level="experiment") %>%  vascr_plot_line()

library(patchwork)
Rbmedias +Rmedias+ Cmmedias+alphamedias + plot_layout(guides = "collect") & geom_vline(xintercept=65)


# barplot
Rbmediasbar<- paired %>% vascr_resample_time(500) %>%  
  vascr_subset(unit= c("Rb"), time= c(65)) %>% 
  vascr_subset(sampleid= c(114, 101)) %>% group_by(Sample) %>% summarise(mean=mean(Value), sd= sd(Value)) 

ggplot(data=Rbmediasbar, 
       aes(
         x= Sample, y=mean, fill = Sample))+
  geom_bar(stat="identity")+ geom_errorbar(aes(ymin= mean-sd, ymax=mean+sd), width = 0.5)


#### CoCl2
p2norm %>% vascr_zero_time(65.764) %>%  vascr_resample_time(500) %>%  
  vascr_normalise(-2, divide = TRUE) %>% 
  vascr_subset(unit= "Rb", time= c(-4, 40)) %>% 
  vascr_subset(sampleid= c(200:204)) %>% 
  vascr_summarise(level="experiment") %>%  vascr_plot_line() +ylim(0, 1.25)

p2norm %>% vascr_zero_time(65.764) %>%  vascr_resample_time(500) %>%  
  vascr_normalise(-2, divide = TRUE) %>% 
  vascr_subset(unit= "Rb", time= c(-4, 40)) %>% 
  vascr_subset(sampleid= c(101, 106, 107, 112)) %>% 
  vascr_summarise(level="experiment") %>%  vascr_plot_line() +ylim(0, 1.25)
