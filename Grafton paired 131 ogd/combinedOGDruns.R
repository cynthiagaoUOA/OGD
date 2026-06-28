# combined data

library(tidyverse)
library(vascr)
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
  1, "A", "1 2 3",  "hypox 131 + glutamax + glucose", # A1 iffy in growth phase, excluded
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




#hypoxic
combinedtworuns %>% vascr_subset(sampleid= c(2,4,3,1)) %>%  vascr_summarise(level="summary") %>%  
  vascr_plot_line() +facet_wrap(~Experiment)+ theme_bw()

#normoxic
combinedtworuns %>% vascr_subset(sampleid= c(12,14,13,11)) %>%  vascr_summarise(level="summary") %>%  
  vascr_plot_line() +facet_wrap(~Experiment)+ theme_bw()+ylim(0, 1.3)

# with glucose
combinedtworuns %>% vascr_subset(sampleid= c(1,2, 11, 12)) %>%  vascr_summarise(level="summary") %>%  
  vascr_plot_line() +facet_wrap(~Experiment)+ theme_bw()



# run 3 combined ----------------------------------------------------------

paired3hypox <- vascr_import("ECIS",
                             raw = "Grafton paired 131 ogd/ECIS_260609_MFT_1_CG_ogdpaired3hypox.abp",
                             model = "Grafton paired 131 ogd/ECIS_260609_MFT_1_CG_ogdpaired3hypox_RbA.csv", experiment = "3")

#paired1normox <- vascr_import("ECIS",
#                              raw = "Grafton paired 131 ogd/",
#                              model = "Grafton paired 131 ogd/", experiment = "exp1")

paired3hypoxkey <- tribble(
  ~SampleID, ~Row, ~Column, ~Sample,
  2, "A", "1 2 3",  "hypox 131", 
  4, "B", "1 2 3", "hypox 131 + 2mM glutamax",
  3, "C", "1 2 3", "hypox 131 + 5mM glucose",
  1, "D", "1 2 3", "hypox 131 + glutamax + glucose", # did glutamax first. PBS in first and third. 
  # extras
  5, "E", "1 2 3", "hypox 131 + 2mM glutamax + 5mM glucose + 5uL HEPES", # 15mM HEPES opposed to the usual 10mM
  6, "F", "1 2 3", "hypox 131 + 2mM glutamax + 2.5mM glucose",# doubled the glucose. 5uL glucose + 5uL
  7, "G", "1 2 3", "hypox 131 + 2mM glutamax + 10mM glucose" # 2.5uL glucose and another 2.5 of water
)


p3hypox <- vascr:::vascr_apply_map(paired3hypox, paired3hypoxkey) 


#norm
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
  15, "E", "1 2 3 4", "131 + 2mM glutamax + 5mM glucose + 5uL HEPES", # 15mM HEPES opposed to the usual 10mM
  16, "F", "1 2 3 4", "131 + 2mM glutamax + 2.5mM glucose",# doubled the glucose. 5uL glucose + 5uL
  17, "G", "1 2 3 4", "131 + 2mM glutamax + 10mM glucose" # 2.5uL glucose and another 2.5 of water
  )


p3normox <- vascr:::vascr_apply_map(paired3normox, paired3normoxkey) 





# combined plots n=3 ----------------------------------------------------------

p1<- vascr_combine (p1normox, p1hypox)%>% vascr_zero_time(15.6) %>%  vascr_resample_time(500) %>%  vascr_normalise(-1, divide = TRUE) %>% 
  vascr_subset(unit= "Rb", time= c(-5, 48)) 

p2<- vascr_combine(p2normox, p2hypox) %>% vascr_zero_time(65.764) %>%  vascr_resample_time(500) %>%  vascr_normalise(-2, divide = TRUE) %>% 
  vascr_subset(unit= "Rb", time= c(-5, 48)) 

p3<- vascr_combine(p3hypox, p3normox) %>% vascr_zero_time(87.145) %>%  vascr_resample_time(500) %>%  vascr_normalise(-2, divide = TRUE) %>% 
  vascr_subset(unit= "Rb", time= c(-5, 55), sampleid=c(1:14)) 



combinedthreeruns<- vascr_combine(p1,p2,p3) %>% drop_na %>% vascr_subset(unit="Rb") 

fullpaireddata<- combinedthreeruns %>% vascr_subset(sampleid=c(1:4, 11:14))

hypox <- combinedthreeruns %>% vascr_subset(sampleid= c(2,4,3,1)) %>% vascr_summarise(level="summary") %>% vascr_plot_line() +theme_bw() 
hypox


# norm vs hypox full glucose
combinedthreeruns %>%  vascr_subset(sampleid= c(1, 11)) %>% vascr_summarise(level="summary") %>% vascr_plot_line() +theme_bw() 





# four conditions in normox
normox<- combinedthreeruns %>% vascr_subset(sampleid= c(12, 14, 13, 11))  %>% vascr_summarise(level="summary") %>% vascr_plot_line() +theme_bw() 
normox + ylim(0, 1.3)  

# split normox by glucose/ no glucose
combinedthreeruns %>% vascr_subset(sampleid= c(12, 14))  %>% vascr_summarise(level="summary") %>% vascr_plot_line() +theme_bw() + ylim(0, 1.3)  

combinedthreeruns %>% vascr_subset(sampleid= c(2,3,12,13))  %>% vascr_summarise(level="summary") %>% vascr_plot_line() +theme_bw() + ylim(0, 1.3)  


# no glutamine
combinedthreeruns %>%  vascr_subset(sampleid= c(1,2, 11,12))  %>% vascr_summarise(level="summary") %>% vascr_plot_line() +theme_bw() + ylim(0, 1.3)  

combinedthreeruns %>%  vascr_subset(sampleid= c(11,12))  %>% vascr_summarise(level="summary") %>% vascr_plot_line() +theme_bw() + ylim(0, 1.3)  




# all
normox + hypox & ylim(0,1.3)



# Rb values ---------------------------------------------------------------

p1raw<- vascr_combine (p1normox, p1hypox)%>% vascr_zero_time(15.6) %>%  vascr_resample_time(500) %>%  
  vascr_subset(unit= "Rb", time= c(-5, 48))
p1raw$exp<- c(1)

p2raw<- vascr_combine(p2normox, p2hypox) %>% vascr_zero_time(65.764) %>%  vascr_resample_time(500) %>%  
  vascr_subset(unit= "Rb", time= c(-5, 48))
p2raw$exp<- c(2)

p3raw<- vascr_combine(p3hypox, p3normox) %>% vascr_zero_time(87.145) %>%  vascr_resample_time(500) %>%  
  vascr_subset(unit= "Rb", time= c(-5, 55), sampleid=c(1:14))
p3raw$exp<- c(3)




rb3rawrb<- rbind(p1raw,p2raw,p3raw) %>% drop_na() 

rb3rawrb$Time <- round(rb3rawrb$Time) 

rb3rawrb<- rb3rawrb %>% filter(Time== -1)

ggplot(data= rb3rawrb, aes(x= Time, y= Value)) + 
 geom_boxplot() + 
  geom_point(data=rb3rawrb, aes(x= , y=Value, colour= as.factor(exp)), position = "jitter") + ylim(0,5)

  
# numbers
rb3rawrb %>% summarise(mean=mean(Value), median = median(Value))



# Cross correlation -------------------------------------------------------

fullpaireddata %>% vascr_subset(sampleid=c(14, 11, 13, 12)) %>% vascr:::vascr_plot_cc_stats(., unit = "Rb", frequency = 0, reference = "none", points = FALSE, stars = TRUE, pval = FALSE)


norm <- fullpaireddata %>% vascr_subset(sampleid=c(14, 11, 13, 12))
summary(aov(Value~Sample, data = norm))
TukeyHSD(aov(Value~Sample, data = norm))
  

check_cc<- fullpaireddata %>% vascr_subset(sampleid=c(14, 11, 13, 12)) %>% vascr:::vascr_summarise_cc(level="experiments")
