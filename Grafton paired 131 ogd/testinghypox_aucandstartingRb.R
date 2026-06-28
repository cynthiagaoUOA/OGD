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



# Threshold for glucose in oxygen dropoff ---------------------------------

# Not dependent on starting Rb. 

# When is the mean drop-off time? Can I get a kaplan meier curve
library(segmented)


gluchypox<- combinedthreeruns %>%  vascr_subset(sampleid=c(1), time= c(10, 48)) %>%  filter(Value>0.2)

# find drop off point using linear models in two parts
wells <- unique(gluchypox$Well) 

dropofftime<- data.frame(Well=wells, time= NA) # making dataframe to store results. Column wells, column for breakpoint

for (i in seq_along(wells)) { # for each well of this new results dataframe, (i is each row), get entry through the process below:
  
  d <- gluchypox[gluchypox$Well == wells[i], ] # from the origianl dataframe, take indivudual rows corresponding to the looped well
  fit<- lm(Value ~ Time, data = d) # fit one lm. One line to work off
  
  seg_fit<- segmented(fit, seg.Z = ~ Time) # break into two parts. brokenline model, estimating breaktime
  
  bp<- summary(seg_fit)$psi # grap breakpoint time which is under column heading Est.
  dropofftime$time[i] <- bp[,"Est." ] # put that into corresponding well of results df
  
}


# sanity check

ggplot(gluchypox, aes(Time, Value)) +
  geom_point(alpha = 0.5) +
  geom_line(alpha = 0.5) +
  facet_wrap(~Well) +
  geom_vline(data = dropofftime,
             aes(xintercept = time),
             color = "red", linetype = "dashed",
             inherit.aes = FALSE) 


# mean
mean(dropofftime$time)
# sem
sd(dropofftime$time, na.rm = TRUE) / sqrt(sum(!is.na(dropofftime$time)))

# bootstrap CI
set.seed(1)

boot_means <- replicate(10000, {
  mean(sample(dropofftime$time, replace = TRUE), na.rm = TRUE)
})

 quantile(boot_means, c(0.025, 0.975)) #confidence interval
 
# checkigng

 fit_df <- data.frame()
 for (w in wells) {
  
   d <- gluchypox[gluchypox$Well == w, ]
   fit <- lm(Value ~ Time, data = d)
   seg_fit <- segmented(fit, seg.Z = ~ Time)
   d$fit <- fitted(seg_fit)
   tau <- summary(seg_fit)$psi[,"Est."]
   d$tau <- tau   # store breakpoint for that well
   fit_df <- rbind(fit_df, d)
 }

 ggplot(fit_df, aes(Time, Value)) +
   geom_point(alpha = 0.3, colour="red") +
   geom_line(alpha = 0.3) +
   geom_line(aes(y = fit), color = "blue", linewidth = 0.9) +
   geom_vline(aes(xintercept = tau),
              color = "red", linetype = "dashed") +
   facet_wrap(~Well) +
   ggtitle("Segmented model fit per well") 
 

 # kaplan meier
 library(survival)
 
 
 km_fit <- survfit(Surv(dropofftime$time, rep(1, nrow(dropofftime))) ~ 1)
 
 plot(km_fit,
      xlab = "Time",
      ylab = "Fraction not dropped",
      main = "Kaplan-Meier curve")
 
 
 
 # Method two, time where steepest slope. Not clear minimums
 # d <- d[order(d$Time), ]
 # 
 # slopes <- diff(d$Value) / diff(d$Time)
 # time_mid <- head(d$Time, -1)   # midpoint times
 # 
 # slope_df <- data.frame(Time = time_mid, slope = slopes)
 # 
 # drop_time <- slope_df$Time[which.min(slope_df$slope)]
 # 
 # 
 # slope_df <- gluchypox %>%
 #   arrange(Well, Time) %>%
 #   group_by(Well) %>%
 #   mutate(
 #     slope = (Value - lag(Value)) / (Time - lag(Time))
 #   ) %>%
 #   filter(!is.na(slope))
 # 
 # ggplot(slope_df, aes(Time, slope)) +
 #   geom_line() +
 #   facet_wrap(~Well) +
 #   geom_hline(yintercept = 0, col = "grey")
 # 