# combined data

library(tidyverse)
library(vascr)
library(ggplot2)

paired1hypox <- vascr_import("ECIS",
                             raw = "Grafton paired 131 ogd/ECIS_260521_MFT_1_CG_ogdpaired1hypoxrestart.abp",
                             model = "Grafton paired 131 ogd/ECIS_260521_MFT_1_CG_ogdpaired1hypoxrestart_RbA.csv", experiment = "exp1"
)

paired1normox <- vascr_import("ECIS",
                              raw = "Grafton paired 131 ogd/ECIS_260521_MFT_1_CG_ogdpaired1normoxrestart.abp",
                              model = "Grafton paired 131 ogd/ECIS_260521_MFT_1_CG_ogdpaired1normoxrestart_RbA.csv", experiment = "exp1"
)

paired1hypoxkey <- tribble(
  ~SampleID, ~Row, ~Column, ~Sample,
  1, "A", "2 3",  "hypox 131 + glutamax + glucose", # A1 iffy in growth phase, excluded
  2, "B", "1 2 3", "hypox 131",
  3, "C", "1 2 3", "hypox 131 + 5mM glucose",
  4, "D", "1 2 3", "hypox 131 + 2mM glutamax",
)


p1hypox <- vascr:::vascr_apply_map(paired1hypox, paired1hypoxkey) 

paired1normoxkey <- tribble(
  ~SampleID, ~Row, ~Column, ~Sample,
  11, "A", "7 8 9", "131 + glutamax + glucose", # A1 iffy in growth phase, excluded
  12, "B", "7 8 9", "131",
  13, "C", "7 8 9", "131 + 5mM glucose",
  14, "D", "7 8 9", "131 + 2mM glutamax",
)


p1normox <- vascr:::vascr_apply_map(paired1normox, paired1normoxkey) 


# run 2

paired2hypox <- vascr_import("ECIS",
                             raw = "Grafton paired 131 ogd/ECIS_260528_MFT_1_CG_ogdpaired2hypox.abp",
                             model = "Grafton paired 131 ogd/ECIS_260528_MFT_1_CG_ogdpaired2hypox_RbA.csv", experiment = "exp2"
)

paired2normox <- vascr_import("ECIS",
                              raw = "Grafton paired 131 ogd/ECIS_260528_MFT_1_CG_ogdpaired2normox.abp",
                              model = "Grafton paired 131 ogd/ECIS_260528_MFT_1_CG_ogdpaired2normox_RbA.csv", experiment = "exp2"
)

paired2hypoxkey <- tribble(
  ~SampleID, ~Row, ~Column, ~Sample,
  1, "A", "7 8 9",  "hypox 131 + glutamax + glucose", # full
  3, "F", "7 8 9", "hypox 131 + 5mM glucose", # no glutamine, glucose only
  4, "A", "10 11 12",  "hypox 131 + 2mM glutamax", 
  2, "F", "10 11 12", "hypox 131") # neither
  

paired2normoxkey <- tribble(
    ~SampleID, ~Row, ~Column, ~Sample, 
    11, "A", "7 8 9",  "131 + glutamax + glucose", # full
    13, "F", "7 8 9", "131 + 5mM glucose", # no glutamine, glucose only
    14, "A", "10 11 12", "131 + 2mM glutamax", 
    12, "F", "10 11 12", "131") # neither


p2hypox <- vascr:::vascr_apply_map(paired2hypox, paired2hypoxkey) 
p2normox<-  vascr:::vascr_apply_map(paired2normox, paired2normoxkey) 


p2<- vascr_combine(p2normox, p2hypox) %>% vascr_zero_time(65.764) %>%  vascr_resample_time(500) %>%  vascr_normalise(-2, divide = TRUE) %>% 
  vascr_subset(unit= "Rb", time= c(-5, 48)) 

p1<- vascr_combine (p1normox, p1hypox)%>% vascr_zero_time(15.6) %>%  vascr_resample_time(500) %>%  vascr_normalise(-2, divide = TRUE) %>% 
  vascr_subset(unit= "Rb", time= c(-5, 48)) 





combinedtworuns<- vascr_combine(p1,p2) %>% 
  vascr_subset(unit= "Rb", time= c(-5, 48)) %>% drop_na()

#hypoxic
combinedtworuns %>% vascr_subset(sampleid= c(2,4,3,1)) %>%  vascr_summarise(level="summary") %>%  
  vascr_plot_line() +facet_wrap(~Experiment)+ theme_bw()

#normoxic
combinedtworuns %>% vascr_subset(sampleid= c(12,14,13,11)) %>%  vascr_summarise(level="summary") %>%  
  vascr_plot_line() +facet_wrap(~Experiment)+ theme_bw()+ylim(0, 1.3)

# with glucose
combinedtworuns %>% vascr_subset(sampleid= c(1,4, 11, 12)) %>%  vascr_summarise(level="summary") %>%  
  vascr_plot_line() +facet_wrap(~Experiment)+ theme_bw()
