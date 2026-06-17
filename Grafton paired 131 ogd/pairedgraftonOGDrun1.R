
library(vascr)
library(tidyverse)
library(ggplot2)

paired1hypox <- vascr_import("ECIS",
                           raw = "Grafton paired 131 ogd/ECIS_260521_MFT_1_CG_ogdpaired1hypoxrestart.abp",
                           model = "Grafton paired 131 ogd/ECIS_260521_MFT_1_CG_ogdpaired1hypoxrestart_RbAnew.csv", experiment = "exp1"
)

paired1normox <- vascr_import("ECIS",
                             raw = "Grafton paired 131 ogd/ECIS_260521_MFT_1_CG_ogdpaired1normoxrestart.abp",
                             model = "Grafton paired 131 ogd/ECIS_260521_MFT_1_CG_ogdpaired1normoxrestart_RbA.csv", experiment = "exp1"
)

paired1hypoxkey <- tribble(
  ~SampleID, ~Row, ~Column, ~Sample,
  1, "A", "1 2 3",  "hypox 131 + glutamax + glucose", # A1 lower from growth curve, unsure yet if I want to exclude. Does match up at treament time, comparable treatment traces
  2, "B", "1 2 3", "hypox 131",
  3, "C", "1 2 3", "hypox 131 + 5mM glucose",
  4, "D", "1 2 3", "hypox 131 + 2mM glutamax",
  
  5, "E", "1 2 3", "hypox DMEM + glutamax + glucose", 
  6, "F", "1 2 3", "hypox DMEM",
  7, "G", "1 2 3", "hypox DMEM + 5mM glucose",
  8, "H", "1 2 3", "hypox DMEM + 2mM glutamax"
  
  )


p1hypox <- vascr:::vascr_apply_map(paired1hypox, paired1hypoxkey) %>% vascr_subset(well="A01")

paired1normoxkey <- tribble(
  ~SampleID, ~Row, ~Column, ~Sample,
  11, "A", "7 8 9", "131 + glutamax + glucose", 
  12, "B", "7 8 9", "131",
  13, "C", "7 8 9", "131 + 5mM glucose",
  14, "D", "7 8 9", "131 + 2mM glutamax",
  
  15, "E", "7 8 9", "DMEM + glutamax + glucose", 
  16, "F", "7 8 9", "DMEM",
  17, "G", "7 8 9", "DMEM + 5mM glucose",
  18, "H", "7 8 9", "DMEM + 2mM glutamax",
  

   21, "A", "10 11 12", "CoCl2 131 + glutamax + glucose", 
   22, "B", "10 11 12", "CoCl2 131",
   23, "C", "10 11 12", "CoCl2 131 + 5mM glucose",
   24, "D", "10 11 12", "CoCl2 131 + 2mM glutamax",
    
   25, "E", "10 11 12", "CoCl2 DMEM + glutamax + glucose", 
   26, "F", "10 11 12", "CoCl2 DMEM",
   27, "G", "10 11 12", "CoCl2 DMEM + 5mM glucose",
   28, "H", "10 11 12", "CoCl2 DMEM + 2mM glutamax"
  )


p1normox <- vascr:::vascr_apply_map(paired1normox, paired1normoxkey) 


p1combined<- vascr_combine(p1hypox, p1normox) %>% drop_na() %>% 
  vascr_zero_time(15.62) %>% 
  vascr_resample_time(500) %>% 
  vascr_normalise(-2, divide = TRUE) %>% 
  vascr_subset(unit= "Rb", time= c(-5, 48))


# 131 hypox
p1combined  %>%  vascr_subset(sampleid = c(2, 1, 3,4)) %>% vascr_summarise(level = "well") %>%
  vascr_plot_line() + theme_bw()
# DMEM hypox
p1combined  %>%  vascr_subset(sampleid = c(6,5,7,8)) %>% vascr_summarise(level = "experiment") %>%
  vascr_plot_line() + theme_bw()

# 131 normox 
p1combined  %>%  vascr_subset(sampleid = c(12,14,13,11)) %>% vascr_summarise(level = "experiment") %>%
  vascr_plot_line()  + ylim(0, 1.2)
# DMEM normox
p1combined  %>%  vascr_subset(sampleid = c(16,15,17,18)) %>% vascr_exclude(well ="H8") %>% vascr_summarise(level = "experiment") %>%
  vascr_plot_line() + theme_bw()

# CoCl
p1combined  %>%  vascr_subset(sampleid = c(26, 25, 27,28)) %>% vascr_exclude(well ="H8") %>% vascr_summarise(level = "well") %>%
  vascr_plot_line() + theme_bw()

# CoCl
p1combined  %>%  vascr_subset(sampleid = c(21:24)) %>% vascr_exclude(well ="H8") %>% vascr_summarise(level = "experiment") %>%
  vascr_plot_line() + theme_bw()

#### how best to separate out hypox and normox? Have to combine all. Don't think I will need to facet. Don't have modeled data

p1combined  %>%  vascr_subset(sampleid = c(111,1)) %>% vascr_summarise(level = "experiment") %>%
  vascr_plot_line() + theme_bw()
