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
  
  114, "F", "4 5 6", "EGM growth curve"
)

p2norm <- vascr:::vascr_apply_map(paired2normox, paired2normoxkey) 


all<- vascr_combine(p1, p2) 

paired %>% vascr_zero_time(65.764) %>%  vascr_resample_time(500) %>%  vascr_normalise(-2, divide = TRUE) %>% 
  vascr_subset(unit= "Rb", time= c(-5, 48)) %>% 
  vascr_subset(sampleid= c(1, 6, 7, 12, 101, 106, 107, 112)) %>% 
  vascr_summarise(level="experiment") %>%  vascr_plot_line()+ylim(0,1.5) + facet_wrap(~Experiment)


paired %>% vascr_zero_time(65.764) %>%  vascr_resample_time(500) %>%  vascr_normalise(-2, divide = TRUE) %>% 
  vascr_subset(unit= c("Cm", "alpha"), time= c(-5, 48)) %>% 
  vascr_subset(sampleid= c(1, 6, 7, 12, 101, 106, 107, 112)) %>% 
  vascr_summarise(level="experiment") %>%  vascr_plot_line()+ylim(0,1.5) + facet_wrap(~unit)
