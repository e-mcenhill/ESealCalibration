
#00: Packages ============================================================
#This section ensures you have all the right packages installed, and if not, installs them then loads them

# what you need:
req_packages= c("tidyverse", "lme4", "MuMIn", "skimr", "patchwork", "here")

# if something's missing??
missing_packages= req_packages[!(req_packages %in% installed.packages()[, "Package"])]

if(length(missing_packages) > 0) {
  install.packages(missing_packages)
}

# Load all required packages
invisible(lapply(req_packages, library, character.only = TRUE))

#01: Custom Functions!   ============================================================

#Cleaning Functions
#Splitting Date: Drone Cleaning
split.date.drone = function(raw.dat) {
  raw.dat %>%
    mutate(
      class = relevel(as.factor(class), 'male', 'female', 'pup'),
      year = relevel(as.factor(year), '2016', '2017', '2018', '2019', '2020', '2021', '2022', '2023', '2024', '2025')
    ) %>% #i had to relevel stuff to make things work at first
    separate(date, into = c("year", "month", "day"), sep = "-", remove = FALSE) %>% #thplit day by -
    mutate(
      day = as.numeric(day),
      month = as.numeric(month),
      year = as.numeric(year),
      date = as.Date(date) #format things into numerics and dates for filtering reasons
    )
}


#Splitting Date: Procedure Cleaning

split.date.procedure= function(raw.dat) {
  raw.dat %>%
    separate(RecoverDate, into = c("RecoverYear", "RecoverMonth", "RecoverDay"), sep = "-", remove = FALSE) %>% #split the date by -
    filter(!is.na(RecoverYear), !is.na(RecoverMonth), !is.na(RecoverDay)) %>% #remove NA values (theres 2 or 3 dates in the wrong format, not matching our drone date range)
    mutate(
      RecoverDay = as.numeric(RecoverDay),
      RecoverMonth = as.numeric(RecoverMonth),
      RecoverYear = as.numeric(RecoverYear),
      RecoverDate = as.Date(RecoverDate)
    ) %>% #format things into numerics and dates for filtering reasons 
    filter(Season == "PM") #we are looking at post molt recoveries of instruments for an accurate time frame!
}

#Wrangling Functions 
#Calculating Weight on day of Calibration Flight (Pup)
wean.flight.weight= function(raw.dat) {
  raw.dat%>% 
    mutate(wean.mass=mass.act*abs(exp(0.00596 * days.since.wean)), #this is from Holsner et. al. 2021
           rate.mass.loss= 7.6-(0.02*wean.mass), #this is from Noren et. al. 2003
           days.to.flight = as.numeric(ymd(flight.date) - ymd(wean.date)), 
           flight.mass = wean.mass - (rate.mass.loss * days.to.flight * (wean.mass / 1000))) #this is using the rate combined with days between weaning and drone flight to calculate mass on flight day
}





# #Mean Per Year Females
# yearly.means.actual=function(est.data) {
#   group_by(year) %>%
#     summarise(mean.est.mass = mean(est.mass, na.rm = TRUE), 
#               mean.length= mean(length), na.rm=TRUE) %>%
#     mutate(year=as.Date(as.character(year), format = "%Y")) %>% 
#     filter(year != "2025-05-20")#filtering out the 2025 season of drone flights, since there's not an equivalent in the procedure data we currently have
# }
# 
# 
# #mean 2025 values Males and Pups 
# twofive.means.actual=function(est.data) {
#   group_by(year) %>%
#     summarise(mean.est.mass = mean(est.mass, na.rm = TRUE), 
#               mean.length= mean(length), na.rm=TRUE) %>%
#     filter(year == "2025")#filtering to only the 2025 season of drone flights, since there's not an equivalent in the procedure data we currently have
# }

#02: Data Load-In ============================================================
#this section loads in raw data! 

##Load raw drone data ----------------------------------------------------
drone_raw= 
  read_csv(here::here("data/raw/Universal Project Data/uasdata.csv"))

##Load procedure data ----------------------------------------------------
procedure_raw=
  read_csv(here::here("data/raw/ESEAL_FORAGING_2024_REVISED.v2.csv"))

##Load calibration data ----------------------------------------------------
calibration_raw=
  read_csv(here::here("data/raw/UAS_footprint_to_mass_Calibration_Data.csv"))

##Optional: Skim Data ----------------------------------------------------

#skim(drone_raw)
#skim(procedure_raw)
#skim(calibration_raw)
#03: Data Cleaning ============================================================
# this section is built to clean our three loaded in datasets into useable formats for each  set of analyses
# cleaning is split my class, as calibration and subsequent analysis will also be split by class group

## Female Class Data ============================================================

### Female Class Calibration Data ----------------------------------------------------
#this is specifically within 4 days of flight dates for more accurate mass measures to comapre against
  female_calibration_clean=
  {
    raw.dat=  procedure_raw #load in my raw data
    drone_dates = as.Date(c(
      "2016-01-25", "2017-01-26", "2018-01-31",
      "2019-01-29", "2020-01-31", "2021-01-29",
      "2022-01-27", "2023-01-21", "2023-01-27",
      "2023-03-02", "2024-01-16", "2024-01-28",
      "2024-02-15", "2025-01-17", "2025-01-30",
      "2025-02-16"
    )) #define my drone flight dates 
    
    clean.dat=  raw.dat %>%  
      filter(!is.na(RecoverMass), !is.na(RecoverDate)) %>%  #remove any animal without a recovery logged (died after deployment or otherwise not found)
      split.date.procedure() %>% 
      dplyr::select(ID,TOPPID, RecoverDate, RecoverYear, RecoverMonth, RecoverDay, RecoverMass, `Recover SL`, RecoverAdipose ) %>% #pulling relevant data
      rename(date=RecoverDate, year=RecoverYear, month=RecoverMonth, day=RecoverDay, 
             mass.act=RecoverMass, std.length=`Recover SL`, adipose=RecoverAdipose) %>% #rename stuff into shorter convention
      mutate('collection.type'= "exact recovery") %>% #add collection type
      mutate(date=as.Date(date)) %>%  #PLEASE MAKE SURE DATE IS DATE IM ADDING IT HERE TO DOUBLE CHECK
      rowwise() %>% #check the rows! treat them each as its own group
      filter(any(abs(difftime(date, drone_dates, units = "days")) <= 4)) %>% #find stuff within three days of my flight days (important for the wrangling AKA mutates I do in the next script)
      ungroup() #undo row wise
    clean.dat
    
    
  }

### Female Class Recovery (Actual) Data ----------------------------------------------------
#this is any actual data within the time frame of the general drone flight boundaries!
  female_recovery_clean=
  {
    raw.dat=  procedure_raw
    clean.dat=  raw.dat %>%
      #filter(!is.na(RecoverMass), !is.na(RecoverDate)) %>%  #remove any animal without a recovery logged (died after deployment or otherwise not found)
      split.date.procedure() %>% 
      filter(between(RecoverYear, 2016, 2024) & between(RecoverMonth, 1, 3)) %>% 
      dplyr::select(ID,TOPPID, RecoverDate, RecoverYear, RecoverMonth, RecoverDay, RecoverMass, `Recover SL`, RecoverAdipose ) %>% #pulling relevant data
      rename(date=RecoverDate, year=RecoverYear, month=RecoverMonth, day=RecoverDay, 
             mass.act=RecoverMass, std.length=`Recover SL`, adipose=RecoverAdipose) %>% #rename stuff into shorter convention
      mutate('collection.type'= "general recovery") %>% #add collection type
      mutate(date=as.Date(date)) #PLEASE MAKE SURE DATE IS DATE IM ADDING IT HERE TO DOUBLE CHECK
    clean.dat
  }

### Female Class Drone Data ----------------------------------------------------

  female_drone_clean=
  {
    raw.dat=  drone_raw
    clean.dat=  raw.dat %>% 
      split.date.drone() %>% 
      filter(class== "female") %>%   #we only want females here
      mutate(est.length=length*100) %>% #get to cm
      filter(length<=400) %>% 
      #      ^ this is filtering drone polygons with lengths above 400, since the largest females max out at aprox. 300cm, but we added some wiggle room here! this is mostly to cut off outliers, since the machine learning isn't super confident in said outliers
      dplyr::select(confidenc, date, year, month, day, class, est.length, width, area_m2) %>% 
      rename(area.m2=area_m2) %>% 
      filter(year!="2025") %>% 
      mutate('collection.type'= "drone")
    clean.dat
    
    
    
  }


## Male Class Data ============================================================
# 
# #note for this: 5/6 males with ground-truthed mass estimates have them completed within a day of the flight date, 
# #but 1 male has a weigh date 6 days before his calibration flight- this male may have lose ~12kg worth of weight and may contribute to higher error in the analysis

### Male Class Calibration Data ----------------------------------------------------

  male_calibration_clean=
  {
    raw.dat=  calibration_raw
    clean.dat=  raw.dat %>%
      rename(flight.date = `2025-04-11`, #csv didnt have this column named
             mass.act=Mass, 
             area.m2=area_m2) %>% 
      filter(Class == "AM") %>% #only males
      mutate('dorsal.mass.est'=(268.272*area.m2^1.5)+4.125, 
             'collection.type'= "calibration") #%>% 
      #filter(Mark!= "")
    clean.dat
  }

### Male Class Drone Data ----------------------------------------------------

  male_drone_clean=
  {
    raw.dat=  drone_raw
    clean.dat=  raw.dat %>%
      split.date.drone() %>%
      filter(class== "male") %>%  #only want males
      mutate(est.length=length*100) %>% #get to cm
      filter(length<=550) %>% 
      #      ^ this is filtering drone polygons with lengths above 550, since the largest males max out at aprox. 500cm, but we added some wiggle room here! this is mostly to cut off outliers, since the machine learning isn't super confident in said outliers
      dplyr::select(confidenc, date, year, month, day, class, est.length, width, area_m2) %>% 
      mutate('collection.type'= "drone")
    clean.dat
  }


## Pup Class Data ============================================================
# 
### Pup Class Calibration Data ----------------------------------------------------
# #this one is a little more involved- 
# #We run a set of mass-correction calculations to find the estimated mass of the weighed pups on the flight date, 
# #and then stick that stuff into my clean data set so we can actually analyze using the correct ground-derived weight, 
# # this is because some pups have quite some time between their weigh day and their calibration flight day 

  pup_calibration_clean=
  {
    raw.dat=  calibration_raw
    clean.dat=  raw.dat %>%
      rename(flight.date = `2025-04-11`, 
             days.since.wean=days.btwn, 
             mass.act=Mass, 
             wean.date=wean_date,
             area.m2=area_m2) %>%
      separate(flight.date, into = c("year", "month", "day"), sep = "-", remove = FALSE) %>%
      filter(Class == "wean") %>% 
      wean.flight.weight() %>% 
      mutate('collection.type'= "calibration")
    clean.dat
    
  }



### Pup Class Drone Data ----------------------------------------------------

  pup_drone_clean=
  {
    raw.dat=  drone_raw
    clean.dat=  raw.dat %>%
      split.date.drone() %>%
      filter(class== "pup") %>% #only want pups
      mutate(est.length=length*100) %>% #get to cm
      dplyr::select(confidenc, date, year, month, day, class, est.length, width, area_m2) %>% 
      mutate('collection.type'= "drone")
    
    clean.dat
    
  }








#04: Analysis: Adult Female============================================================
##New Equation Derivation: - Scrapped for insignificance --------------------------------------------------
##New Frame: Drone Footprint Means
female_drone_summary=female_drone_clean %>% 
  group_by(year) %>%
  summarise(mean.area.m2 = mean(area.m2, na.rm = TRUE), 
            mean.est.length= mean(est.length), na.rm=TRUE) %>%
  mutate(mean.area.scaled = mean.area.m2 ^ 1.5) %>% 
  mutate('collection.type'= "drone") %>%
  filter(year != "2025")#filtering out the 2025 season of drone flights, since there's not an equivalent in the procedure data we currently have

##New Frame: Calibration Mass Means
female_calibration_summary=female_calibration_clean %>% 
  group_by(year) %>%
  summarise(mean.mass.act = mean(mass.act, na.rm = TRUE), 
            mean.std.length= mean(std.length), na.rm=TRUE,) %>%
  mutate('collection.type'= "exact recovery")
  


##The Models
female.lm= lm(female_calibration_summary$mean.mass.act ~ female_drone_summary$mean.area.m2)
summary(female.lm)

female.scaled= lm(female_calibration_summary$mean.mass.act ~ female_drone_summary$mean.area.scaled)
summary(female.scaled)

female.aic=AIC(female.lm, female.scaled)
print(female.aic)


##Alvarado Equation Accuracy----------------------------------------------------
#Drone Footprint Means
female_drone_est=female_drone_clean %>% 
  #filter(est.length<=400) %>% 
 #calculate lateral mass estimates (alvarado et. al)
  mutate('dorsal.mass.est'=(268.272*area.m2^1.5)+4.125) 
  
#estimated drone mass summary
female_drone_est_summary= female_drone_est %>% 
  group_by(year) %>%
  summarise(mean.dorsal.mass.est = mean(dorsal.mass.est, na.rm = TRUE)) %>%
  mutate('collection.type'= "drone") %>%
  
  filter(year != "2025")#filtering out the 2025 season of drone flights, since there's not an equivalent in the procedure data we currently have

#Recovery summary 
female_recovery_summary=female_recovery_clean %>% 
  group_by(year) %>%
  summarise(mean.mass.act = mean(mass.act, na.rm = TRUE)) %>% 
  mutate('collection.type'= "general recovery")


###The Models
female.recovery.lm= lm(female_recovery_summary$mean.mass.act ~ female_drone_est_summary$mean.dorsal.mass.est)
summary(female.recovery.lm)

#Alvarado remains accurate! 
# Call:
#   lm(formula = female_recovery_summary$mean.mass.act ~ female_drone_est_summary$mean.dorsal.mass.est)
# 
# Residuals:
#   Min      1Q  Median      3Q     Max 
# -12.295  -8.453  -1.203   7.491  20.955 
# Coefficients:
#   Estimate Std. Error t value
# (Intercept)                                   -1164.8884   302.2037  -3.855
# female_drone_est_summary$mean.dorsal.mass.est     3.1322     0.5815   5.387
# Pr(>|t|)   
# (Intercept)                                    0.00625 **
#   female_drone_est_summary$mean.dorsal.mass.est  0.00102 **
#   ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Residual standard error: 11.62 on 7 degrees of freedom
# Multiple R-squared:  0.8057,	Adjusted R-squared:  0.7779 
# F-statistic: 29.02 on 1 and 7 DF,  p-value: 0.001023


##Error Estimations ----------------------------------------------------
#trying different way compared to other two here because of split data frames
residuals_dorsal_female = female_recovery_summary$mean.mass.act - predict(female.recovery.lm)



##Error Summaries ----------------------------------------------------
min_residual_female = min(residuals_dorsal_female)
max_residual_female = max(residuals_dorsal_female)

# Mean ± SD of residuals
mean_residual_female = mean(residuals_dorsal_female, na.rm = TRUE)
sd_residual_female = sd(residuals_dorsal_female, na.rm = TRUE)


mean_error_female = abs(mean_residual_female)

#Percent error
percent_error_female = (residuals_dorsal_female / female_recovery_summary$mean.mass.act) * 100
mean_percent_error_female = mean(percent_error_female, na.rm = TRUE)
mean_abs_percent_error_female <- mean(abs(percent_error_female), na.rm = TRUE)
sd_percent_error_female = sd(percent_error_female, na.rm = TRUE)

# Summary output (matched female format)
cat("Adult female residual error ranged from", round(min_residual_female, 1), "to", round(max_residual_female, 1), "kg.\n") %>% 
  cat("Adult female residual error (mean ± SD):", round(mean_residual_female, 2), "±", round(sd_residual_female, 2), "kg\n") %>% 
  cat("Adult female percent error (mean ± SD):", round(mean_percent_error_female, 2), "±", round(sd_percent_error_female, 2), "%\n") %>% 
  cat("Adult female class seal mass was estimated with a mean absolute error of", 
      round(mean_error_female, 1), "kg, or", 
      round(mean_percent_error_female, 2), "% of total body mass.\n")

# Adult female residual error ranged from -12.3 to 21 kg.
# Adult female residual error (mean ± SD): 0 ± 10.87 kg
# Adult female percent error (mean ± SD): -0.05 ± 2.32 %
# Adult female class seal mass was estimated with a mean absolute error of 0 kg, or -0.05 % of total body mass.


##Error Significance----------------------------------------------------

#residuals

t.test(residuals_dorsal_female, mu = 0)

# One Sample t-test
# 
# data:  female_error_summary$residual.err
# t = -9.1689, df = 8, p-value = 8.08e-06
# alternative hypothesis: true mean is less than 0
# 95 percent confidence interval:
#   -Inf -45.26725
# sample estimates:
#   mean of x 
# -56.78354 

#percent error 

t.test(female_error_summary$residual.err, mu = 0, alternative = "less")

# One Sample t-test
# 
# # data:  female_error_summary$residual.err
# # t = -9.1689, df = 8, p-value = 8.08e-06
# # alternative hypothesis: true mean is less than 0
# # 95 percent confidence interval:
# #   -Inf -45.26725
# sample estimates:
#   mean of x 
# -56.78354 


#05: Analysis: Pups===========================================================

##The Models----------------------------------------------------

# Linear model
pup.lm= lm(flight.mass ~ area.m2, data = pup_calibration_clean)
summary(pup.lm)

# Mixed Effects model
pup.mixed=  lmer(flight.mass ~ area.m2 + (1 | Tag), data = pup_calibration_clean)
summary(pup.mixed)

# Scaled Model area.m2^1.5  

pup_calibration_scaled= pup_calibration_clean %>% 
  mutate(area1.5 = area.m2^1.5)

pup.scaled= lmer(flight.mass ~ area1.5 + (1 | Tag), data = pup_calibration_scaled)

summary(pup.scaled)

# AIC comparison
pup.aic= AIC(pup.lm, pup.mixed, pup.scaled)
print(pup.aic)
# The table! 
#             df      AIC
# pup.lm      3     165.6559
# pup.mixed   4     152.7560
# pup.scaled  4     152.0598  = lowest by a small margin

##Equation Derivation----------------------------------------------------
summary(pup.scaled)
# Linear mixed model fit by REML ['lmerMod']
# Formula: flight.mass ~ area1.5 + (1 | Tag)
# Data: pup_calibration_scaled
# 
# REML criterion at convergence: 144.1
# 
# Scaled residuals: 
#   Min      1Q  Median      3Q     Max 
# -0.9604 -0.4024 -0.0673  0.3112  1.1535 
# 
# Random effects:
#   Groups   Name         Variance    Std.Dev.
# Tag      (Intercept)    158.02      12.571  
# Residual                43.74        6.614  
# Number of obs: 20, groups:  Tag, 17
# 
# Fixed effects:
#               Estimate   Std. Err  t value
# (Intercept)    52.83      15.58    3.39
# area1.5       171.14      45.63    3.75
# 
# Correlation of Fixed Effects:
#   (Intr)
# area1.5 -0.976

#T-Value above 2!! this suggests statistical significance 
#Equation pulled from best model following formatting fro. Alvarado et.al. :

#Estimated Mass (kg)= (171.14×(Footprint Area)^1.5) +52.83
#                    est.^.                          est^

#extract r square values 
r2 = r.squaredGLMM(pup.scaled)
print(r2)
r2[2]


##Estimations and Errors  ----------------------------------------------------
pup_calibration_full=
  {
    scale.dat=  pup_calibration_scaled
    est.dat=  scale.dat %>%
      mutate(predicted.mass = 52.83 + 171.14 * area1.5, 
              residual.err= flight.mass - predicted.mass, 
              percent.error = (residual.err / flight.mass * 100))
    est.dat
    
  }

##Error Summaries ----------------------------------------------------
min_residual_pup = min(pup_calibration_full$residual.err, na.rm = TRUE)
max_residual_pup = max(pup_calibration_full$residual.err, na.rm = TRUE)

# Mean ± SD of residuals
mean_residual_pup = mean(pup_calibration_full$residual.err, na.rm = TRUE)
sd_residual_pup = sd(pup_calibration_full$residual.err, na.rm = TRUE)

mean_error_pup = abs(mean_residual_pup)

# Mean ± SD of percent error
mean_percent_error_pup = mean(pup_calibration_full$percent.error, na.rm = TRUE)
sd_percent_error_pup = sd(pup_calibration_full$percent.error, na.rm = TRUE)

# Summary output (matched female format)
cat("Weanling residual error ranged from", round(min_residual_pup, 1), "to", round(max_residual_pup, 1), "kg.\n") %>% 
cat("Weanling residual error (mean ± SD):", round(mean_residual_pup, 2), "±", round(sd_residual_pup, 2), "kg\n") %>% 
cat("Weanling percent error (mean ± SD):", round(mean_percent_error_pup, 2), "±", round(sd_percent_error_pup, 2), "%\n") %>% 
cat("Weanling class seal mass was estimated with a mean absolute error of", 
    round(mean_error_pup, 1), "kg, or", 
    round(mean_percent_error_pup, 2), "% of total body mass.\n")

#statement: 
# Weanling residual error ranged from -25.4 to 29.2 kg.
# Weanling residual error (mean ± SD): -0.08 ± 13.14 kg
# Weanling percent error (mean ± SD): -1.39 ± 12.17 %
# Weanling class seal mass was estimated with a mean absolute error 
# of 0.1 kg, or -1.39 % of total body mass.

##Error Significance----------------------------------------------------

t.test(pup_calibration_full$residual.err, mu = 0)

# One Sample t-test
# 
# data:  pup_calibration_full$residual.err
# t = -0.02841, df = 19, p-value =
#   0.4888
# alternative hypothesis: true mean is less than 0
# 95 percent confidence interval:
#   -Inf 4.997887
# sample estimates:
#   mean of x 
# -0.08348704 








##New Frame: Drone Estimations---------------------------------------------------
pup_drone_full=pup_drone_clean %>% 
  rename(area.m2=area_m2) %>% 
  mutate(area1.5 = area.m2^1.5) %>% 
  mutate(predicted.mass = 52.83 + 171.14 * area1.5)

  
#06: Analysis: Adult Male============================================================
##The Models----------------------------------------------------


###attempting to derive a male-specific equation----------------------------------------------------
# Linear model


male.lm= lm(mass.act ~ area.m2, data = male_calibration_clean)
summary(male.lm)

#note: mixed effects will not work for males, there is no replication! 

# Scaled Model area.m2^1.5 
male_data_scaled= male_calibration_clean %>% mutate(area1.5 = area.m2^1.5)

male.scaled= lm(mass.act ~ area1.5, data = male_data_scaled)
summary(male.scaled)

#NOTE: neither of these are statistically significant! R values suggest less than ten percent explanations

###testing Alvarado dorsal equation as proxy ----------------------------------------------------
# Linear model

male.est.lm= lm(mass.act ~ dorsal.mass.est, data = male_calibration_clean)
summary(male.est.lm)


# AIC comparison
male.aic=AIC(male.lm, male.scaled, male.est.lm)
print(male.aic)
#              df      AIC
# male.lm      3     74.22494
# male.scaled  3     74.21811
# male.est.lm  3     74.21811
#theyre all similar and they all kinda suck 

  
#Error Estimations ----------------------------------------------------
male_calibration_full = male_calibration_clean %>%
  mutate(residual.err = mass.act - dorsal.mass.est,
         percent.error = (residual.err / mass.act) * 100)

## Error Summaries ----------------------------------------------------
min_residual_male = min(male_calibration_full$residual.err, na.rm = TRUE)
max_residual_male = max(male_calibration_full$residual.err, na.rm = TRUE)

# Mean ± SD of residuals
mean_residual_male = mean(male_calibration_full$residual.err, na.rm = TRUE)
sd_residual_male = sd(male_calibration_full$residual.err, na.rm = TRUE)

mean_error_male = abs(mean_residual_male)

# Mean ± SD of percent error
mean_percent_error_male = mean(male_calibration_full$percent.error, na.rm = TRUE)
sd_percent_error_male = sd(male_calibration_full$percent.error, na.rm = TRUE)

# Summary output
cat("Adult male residual error ranged from", round(min_residual_male, 1), "to", round(max_residual_male, 1), "kg.\n") %>% 
cat("Adult male residual error (mean ± SD):", round(mean_residual_male, 2), "±", round(sd_residual_male, 2), "kg\n") %>% 
cat("Adult male percent error (mean ± SD):", round(mean_percent_error_male, 2), "±", round(sd_percent_error_male, 2), "%\n") %>% 
cat("Adult male seal mass was estimated with a mean absolute error of", 
    round(mean_error_male, 1), "kg, or", 
    round(mean_percent_error_male, 2), "% of total body mass.\n")

# Adult male residual error ranged from -452.4 to 276.9 kg.
# Adult male residual error (mean ± SD): -106.04 ± 287.08 kg
# Adult male percent error (mean ± SD): -11.49 ± 25.62 %
# Adult male seal mass was estimated with a mean absolute error of 106 kg, or -11.49 % of total body mass.

## Error Significance ----------------------------------------------------

t.test(male_calibration_full$residual.err, mu = 0, alternative="less")


##New Frame: Drone Estimations---------------------------------------------------
male_drone_full=male_drone_clean %>% 
  rename(area.m2=area_m2) %>% 
  mutate(area1.5 = area.m2^1.5) %>% 
  mutate(predicted.mass = 52.83 + 171.14 * area1.5)


#07: Visualizations============================================================
##Figure 1: Female Estimations Across Years ----------------------------------------------------

###Figure 1a: Female ----------------------------------------------------

{
femalepalette = c(
    "Drone Estimate" = "#242C45",
    "General Recovery Procedure" = "#68C1DF",
    "Calibration Recovery Procedure \n (±4 Days from Flight)" = "#C7E8F3",
    "Drone Mean" = "#E0B639",     
    "Procedure Mean" = "#F6D55C"     
)


#2025 star
star_2025 <- female_error_summary %>% filter(year == 2025)

fig1a = ggplot() +
# Drone 
  geom_point(data = female_drone_est, 
             aes(x = date, y = dorsal.mass.est, color = "Drone Estimate"),
             alpha = 0.5, position = position_jitter(width = 100)) +
  
# Recovery
  geom_point(data = female_recovery_clean, 
             aes(x = date, y = mass.act, color = "General Recovery Procedure"),
             position = position_jitter(width = 100)) +
  
# Calibration 
  geom_point(data = female_calibration_clean, 
             aes(x = date, y = mass.act, color = "Calibration Recovery Procedure \n (±4 Days from Flight)"),
             shape = 17, size = 2.5,
             position = position_jitter(width = 100)) +
  
# Stars
  geom_point(data = female_error_summary %>% filter(year < 2025), 
             aes(x = as.Date(paste0(year, "-07-01")), y = mean.dorsal.mass.est, color = "Drone Mean"),
             shape = 8, size = 2, stroke = 2) +
  geom_point(data = female_error_summary %>% filter(year < 2025), 
             aes(x = as.Date(paste0(year, "-07-01")), y = mean.mass.act, color = "Procedure Mean"),
             shape = 8, size = 2, stroke = 2) +
  
  # Red Star (only drone estimate here as example)
  geom_point(data = star_2025,
             aes(x = as.Date("2025-07-01"), y = mean.dorsal.mass.est),
             shape = 8, size = 2, stroke = 2, color = "#823329")+
  
  scale_color_manual(values = femalepalette, name = "Collection Type") +
  scale_x_date(date_breaks = "year", date_labels = "%Y", limits = as.Date(c("2016-01-01", "2024-07-01"))) +
  theme_minimal() +
  ylim(300, 850) +
  labs(x = "Year", y = "Mass (kg)") +
  ggtitle("Female (2016–2025)")
}
fig1a

###Figure 1b: Pup  ----------------------------------------------------
{
  puppalette = c(
    "Drone Estimate" = "#1B3E2A",  
    "Calibration Actual Weight" = "#A7E6D0"  
  )
  fig1b = ggplot() +
    # Drone 
    geom_point(data = pup_drone_full, 
               aes(x = date, y =predicted.mass , color = "Drone Estimate"),
               alpha = 0.5, position = position_jitter(width = 100)) +
    
    # Calibration 
    geom_point(data = pup_calibration_clean, 
               aes(x = flight.date, y = flight.mass, color = "Calibration Actual Weight"),
               shape = 17, size = 2.5,
               position = position_jitter(width = 100)) +
    scale_color_manual(values = puppalette, name = "Collection Type") +
    scale_x_date(date_breaks = "year", date_labels = "%Y", limits = as.Date(c("2024-09-01", "2025-07-01"))) +
    theme_minimal() +
    ylim(0, 300) +
    labs(x = "Year", y = "Mass (kg)") +
    ggtitle("Weanling (2025)")
}
fig1b

###Figure 1c: Male  ----------------------------------------------------
{
  malepalette = c(
    "Drone Estimate" = "#242C45",  
    "Calibration Actual Weight" = "#C7E8F3"  
  )
  fig1c = ggplot() +
    # Drone 
    geom_point(data = male_drone_full, 
               aes(x = date, y =predicted.mass , color = "Drone Estimate"),
               alpha = 0.5, position = position_jitter(width = 100)) +
    
    # Calibration 
    geom_point(data = male_calibration_clean, 
               aes(x = flight.date, y = mass.act, color = "Calibration Actual Weight"),
               shape = 17, size = 2.5,
               position = position_jitter(width = 100)) +
    scale_color_manual(values = malepalette, name = "Collection Type") +
    scale_x_date(date_breaks = "year", date_labels = "%Y", limits = as.Date(c("2024-09-01", "2025-07-01"))) +
    theme_minimal() +
    ylim(300, 1800) +
    labs(x = "Year", y = "Mass (kg)") +
    ggtitle("Male (2025)")
}
fig1c


###Fig 1 Patchwork  ----------------------------------------------------
fig1= (fig1a)/(fig1b|fig1c)+ 
  plot_annotation(
    title = "Figure 1: Morphometrics Trends by Collection Type",
    tag_levels = 'A',
    caption = "Figure 1:\n
              Comparison of colony drone mass estimates (navy) of adult female elephant seals in the Año Nuevo breeding colony to measured mass (light blue circles and triangles) from instrument recovery procedures on \n
              adult females during the same time frame across years. Mean drone and procedure values shown per year (yellow star) B) Comparison of colony drone mass estimates of weanling elephant \n
               seals (dark green) in the Año Nuevo breeding colony to measured masses of weighed  calibration weanlings (light green) in the 2025 season.C) Comparison of colony mass estimates of weanling elephant seals\n
                in the Año Nuevo breeding colonyto measured masses of weighed calibration adult males in the 2025 season. \n
              ") & 
  theme(plot.caption = element_text(hjust = 0, face = "plain", size = 9))

fig1

ggsave("figure1(final).png", plot=fig1, device=png, path=here::here("images"), width=4000, height=2800, units="px", dpi=320)

##Figure 2:  ----------------------------------------------------
#Individual Accuracy Comparisons during Mass Estimation
skim(male_calibration_full)
skim(pup_calibration_full)
###PLOTS!!!! My outline is becoming ridiculous :  ----------------------------------------------------

#male!
#long time!!
male_long= male_calibration_full %>%
  select(Mark, mass.act, dorsal.mass.est) %>%
  pivot_longer(cols = c(mass.act, dorsal.mass.est),
               names_to = "Type", values_to = "Mass") %>%
  mutate(Type = dplyr::recode(Type,
                       "mass.act" = "Male Actual",
                       "dorsal.mass.est" = "Male Estimate"))
#plot! 
fig2b= ggplot(male_long, aes(x = Mark, y = Mass, fill = Type)) +
  geom_col(position = position_dodge(width = 0.8), width = 0.7) +
  scale_fill_manual(values = barpalette[c("Male Actual", "Male Estimate")]) +
  labs(title = "Adult Male",
       x = "Mark ID",
       y = "Mass (kg)",
       fill = "") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

fig2b    

#Pup! 
#long time 
pup_long = pup_calibration_full %>%
  select(Tag, flight.mass, predicted.mass) %>%
  pivot_longer(cols = c(flight.mass, predicted.mass),
               names_to = "Type", values_to = "Mass") %>%
  mutate(Type = dplyr::recode(Type,
                              "flight.mass" = "Pup Actual",
                              "predicted.mass" = "Pup Estimate"))

fig2a= ggplot(pup_long, aes(x = Tag, y = Mass, fill = Type)) +
  geom_col(position = position_dodge(width = 0.8), width = 0.7) +
  scale_fill_manual(values = barpalette[c("Pup Actual", "Pup Estimate")]) +
  labs(title = "Weanling (pup)",
       x = "Tag ID",
       y = "Mass (kg)",
       fill = "") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

fig2a 

##patchwork----------------------------------------------------
fig2= fig2a+fig2b +
plot_annotation(
  title = "Figure 2: Accuracy Comparisons during Mass Estimation",
  tag_levels = 'A',
  caption = "Figure 2:\n
              Comparison of ground-truthed mass to drone-estimate mass from calibration flights at the Año Nuevo elephant seal colony.\n
              A) Weanlings (n = 20). Mean absolute percent error (APE) was 1.39%, with residuals ranging from −25.4 to 29.2 kg (mean = −0.08 ± 13.14 kg), showing no bias (p = 0.978)\n
              B) Adult males (n = 6). Residuals ranged from −452.4 to 276.9 kg (mean = −106.04 ± 287.08 kg), with no significant underestimation (p = 0.20)."
              
              ) & 
  theme(plot.caption = element_text(hjust = 0, face = "plain", size = 9))
fig2

ggsave("figure2(final).png", plot=fig2, device=png, path=here::here("images"), width=4000, height=2000, units="px", dpi=320)
