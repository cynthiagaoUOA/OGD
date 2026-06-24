# DMEM run comparisons in runs 1 and 3
# run 2 i did glutamax titration, lot of conditions, deprioritised DMEM


# run 1 -------------
paired1normox <- vascr_import("ECIS",
                              raw = "Grafton paired 131 ogd/ECIS_260521_MFT_1_CG_ogdpaired1normoxrestart.abp",
                              model = "Grafton paired 131 ogd/ECIS_260521_MFT_1_CG_ogdpaired1normoxrestart_RbA.csv", experiment = "exp1")

paired1normoxkey <- tribble(
  ~SampleID, ~Row, ~Column, ~Sample,
  11, "A", "7 8 9", "131 + glutamax + glucose", 
  12, "B", "7 8 9", "131",
  13, "C", "7 8 9", "131 + 5mM glucose",
  14, "D", "7 8 9", "131 + 2mM glutamax",
  
  15, "E", "7 8 9", "DMEM + glutamax + glucose", 
  16, "F", "7 8 9", "DMEM",
  17, "G", "7 8 9", "DMEM + 5mM glucose",
  18, "H", "7 8 9", "DMEM + 2mM glutamax")
  
  
p1dmem <- vascr:::vascr_apply_map(paired1normox, paired1normoxkey) %>% 
  vascr_zero_time(15.62) %>% 
  vascr_resample_time(500) %>% 
  vascr_normalise(-2, divide = TRUE) %>% 
  vascr_subset(unit= "Rb", time= c(-5, 48), sampleid=c(11:18)) 
  


# run 3 -------------------

paired3normox <- vascr_import("ECIS",
                              raw = "Grafton paired 131 ogd/ECIS_260609_MFT_1_CG_ogdpaired3normox.abp",
                              model = "Grafton paired 131 ogd/ECIS_260609_MFT_1_CG_ogdpaired3normox_RbA.csv", experiment = "3")

paired3normoxkey <- tribble( # 4 wells per conditon in this one
  ~SampleID, ~Row, ~Column, ~Sample,
  12, "A", "1 2 3 4",  "131", # A1 iffy in growth phase, excluded
  14, "B", "1 2 3 4", "131 + 2mM glutamax",
  13, "C", "1 2 3 4", "131 + 5mM glucose",
  11, "D", "1 2 3 4", "131 + glutamax + glucose", # did glutamax first. PBS in first and third. 
  # extras
  # 15, "E", "1 2 3 4", "131 + 2mM glutamax + 5mM glucose + 5uL HEPES", # 15mM HEPES opposed to the usual 10mM
  # 16, "F", "1 2 3 4", "131 + 2mM glutamax + 2.5mM glucose",# doubled the glucose. 5uL glucose + 5uL
  # 17, "G", "1 2 3 4", "131 + 2mM glutamax + 10mM glucose" # 2.5uL glucose and another 2.5 of water
  # 
  # 
  
  
  16, "A E",  "5 6", "DMEM",
  18, "B F", "5 6", "DMEM + 2mM glutamax", 
  17, "C G", "5 6", "DMEM + 5mM glucose", 
  15, "D H", "5 6", "DMEM + glutamax + glucose"
  
  
)


p3dmem <- vascr:::vascr_apply_map(paired3normox, paired3normoxkey) %>% 
  vascr_zero_time(87.145) %>%  vascr_resample_time(500) %>%  vascr_normalise(-2, divide = TRUE) %>% 
  vascr_subset(unit= "Rb", time= c(-5, 48), sampleid=c(11:18)) 

# DMEM only
vascr_combine(p1dmem, p3dmem) %>% vascr_subset(sampleid=c(15: 18)) %>% vascr_summarise(level="summary") %>% vascr_plot_line()

# simplify glutamine story for DMEM comparison
vascr_combine(p1dmem, p3dmem) %>% vascr_subset(sampleid=c(11, 16, 15)) %>% vascr_summarise(level="summary") %>% vascr_plot_line()

# 131 vs DMEM
vascr_combine(p1dmem, p3dmem) %>% vascr_subset(sampleid=c( 11, 15)) %>% vascr_summarise(level="summary") %>% vascr_plot_line() + ylim(0, 1.3)


# 131 
vascr_combine(p1dmem, p3dmem) %>% vascr_subset(sampleid=c( 11: 14)) %>% vascr_summarise(level="summary") %>% vascr_plot_line() + ylim(0, 1.3)
