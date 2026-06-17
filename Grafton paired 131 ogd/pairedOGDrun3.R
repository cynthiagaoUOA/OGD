### Run 3 with extras



library(tidyverse)
library(vascr)
library(ggplot2)

paired3hypox <- vascr_import("ECIS",
                             raw = "Grafton paired 131 ogd/ECIS_260609_MFT_1_CG_ogdpaired3hypox.abp",
                             model = "Grafton paired 131 ogd/ECIS_260609_MFT_1_CG_ogdpaired3hypox_RbA.csv", experiment = "3")

#paired1normox <- vascr_import("ECIS",
#                              raw = "Grafton paired 131 ogd/",
#                              model = "Grafton paired 131 ogd/", experiment = "exp1")

paired3hypoxkey <- tribble(
  ~SampleID, ~Row, ~Column, ~Sample,
  1, "A", "1 2 3",  "hypox 131", # A1 iffy in growth phase, excluded
  2, "B", "1 2 3", "hypox 131+ 2mM glutamax",
  3, "C", "1 2 3", "hypox 131 + 5mM glucose",
  4, "D", "1 2 3", "hypox 131 + 2mM glutamax + 5mM glucose", # did glutamax first. PBS in first and third. 
# extras
  5, "E", "1 2 3", "hypox 131 + 2mM glutamax + 5mM glucose + 5uL HEPES", # 15mM HEPES opposed to the usual 10mM
  6, "F", "1 2 3", "hypox 131 + 2mM glutamax + 2.5mM glucose",# doubled the glucose. 5uL glucose + 5uL
  7, "G", "1 2 3", "hypox 131 + 2mM glutamax + 10mM glucose" # 2.5uL glucose and another 2.5 of water
   )


p3hypox <- vascr:::vascr_apply_map(paired3hypox, paired3hypoxkey) 




paired3<- p3hypox %>% vascr_zero_time(87.145) %>%  vascr_resample_time(500) %>%  vascr_normalise(-2, divide = TRUE) %>% 
  vascr_subset(unit= "Rb", time= c(-5, 48), sampleid=c(1:7)) 

paired3<- p3hypox %>% vascr_zero_time(87.145) %>%  vascr_resample_time(500) %>%  vascr_normalise(-2, divide = TRUE) %>% 
  vascr_subset(unit= c("Cm", "alpha"), time= c(-5, 48), sampleid=c(1:7)) 


paired3 %>% vascr_summarise(level= "experiment") %>%  vascr_plot_line()+ facet_wrap(~Unit) +ylim(-1, 2)


