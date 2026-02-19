# OGD prelim


# Normoxia - CoCl2, glucosefree,  -----------------------------------------



library(vascr)
library(tidyverse)
library(ggplot2)

dmem_cocl1 <- vascr_import("ECIS",
                    raw = "Glucose-free DMEM and CoCl/ECIS_251029_MFT_1_CG_OGDprelim_1.abp",
                    model = "Glucose-free DMEM and CoCl/ECIS_251029_MFT_1_CG_OGDprelim_1_RbA.csv", experiment = "exp1"
)

dmem_cocl1key <- tribble(
  ~SampleID, ~Row, ~Column, ~Sample, #triplicate treatments, ordered numbers based on platemap rather than something logical
  1, "A B C", "7", "AIM-V with CoCl2",
  2, "A B C", "8", "AIM-V with vehicle",
  3, "A B C", "9", "GlucosefreeDMEM with CoCl2",
  4, "A B", "10", "GlucosefreeDMEM with CoCl2 and glucose",
  5, "A B C", "11", "GlucosefreeDMEM vehicle",
  6, "A B C", "12", "GlucosefreeDMEM with glucose",
  7, "D", "7 8 9 ", "GlucosefreeDMEM with glutamine",
  8, "D", "10 11 12", "EGM full media with serum CoCl2"
)


dmem_cocl1labeled <- vascr:::vascr_apply_map(dmem_cocl1, dmem_cocl1key)

dmem_cocl1plotdata <- dmem_cocl1labeled %>%
  vascr_subset(unit = "Rb") %>%
  vascr_zero_time(64.78) %>%
  vascr_resample_time(500) %>%
  vascr_normalise(-2, divide = TRUE) %>% 
  vascr_subset(time = c(-5, 48))

dmem_cocl1plotdata  %>%  vascr_subset(sampleid = c(1:8)) %>% vascr_summarise(level = "experiment") %>%
  vascr_plot_line() 

# all DMEM conditions
dmem_cocl1plotdata  %>%  vascr_subset(sampleid = c(3,4,5, 6,7)) %>% vascr_summarise(level = "experiment") %>%
  vascr_plot_line() +ylim(-0.001,1.3) +xlim(-4,50)+
  geom_vline(xintercept=0, linetype="dashed", color="cornflowerblue")


#AIMV with Cocl2 vs vehicle
dmem_cocl1plotdata  %>%  vascr_subset(sampleid = c(1,2)) %>% vascr_summarise(level = "experiment") %>%
  vascr_plot_line() +ylim(0,1.5) +xlim(-4,40)  


#glucosefree DMEM vhicle vs glucose vs glutamine
dmem_cocl1plotdata  %>%  vascr_subset(sampleid = c(5,6)) %>% vascr_summarise(level = "experiment") %>%
  vascr_plot_line() 



# repeat 2 ----------------------------------------------------------------


# Plasmin test, testing our plasmin against freshly made up EJ plasmin



dmem_cocl2 <- vascr_import("ECIS",
                                raw = "Glucose-free DMEM and CoCl/ECIS_251207_MFT_1_CG_plasmintestplusextra.abp",
                                model = "Glucose-free DMEM and CoCl/ECIS_251207_MFT_1_CG_plasmintestplusextra_RbA.csv", experiment = "exp2"
)

dmem_cocl2key <- tribble(
  ~SampleID, ~Row, ~Column, ~Sample, #triplicate treatments
  
  1, "E", "1 2 3", "AIM-V with CoCl2", 
  2, "D", "4 5 6", "AIM-V with vehicle", # messedup treatments of original D4/5 E4/5 wells, washed them and retreated with AIM, but iffy. Treated extra D6 E6 as insurence, but need to have a look at traces
  3, "A", "4 5 6", "GlucosefreeDMEM with CoCl2",
  4, "B", "4 5 6", "GlucosefreeDMEM with CoCl2 and glucose",
  5, "B", "1 2 3", "Glucosefree DMEM vehicle",  
  6, "C", "1 2 3", "GlucosefreeDMEM with glucose",
  7, "D", "1 2 3", "GlucosefreeDMEM with glutamine")

  

dmem_cocl2labeled <- vascr:::vascr_apply_map(dmem_cocl2, dmem_cocl2key)



dmem_cocl2plotdata <- dmem_cocl2labeled %>%
  vascr_subset(unit = "Rb") %>%
  vascr_zero_time(71.5658) %>%
  vascr_resample_time(500) %>%
  vascr_normalise(-2, divide = TRUE) %>% 
  vascr_subset(time = c(-5, 30))

dmem_cocl2plotdata  %>%  vascr_subset(sampleid = c(3:7, 2)) %>% vascr_summarise(level = "experiment") %>%
  vascr_plot_line() 
