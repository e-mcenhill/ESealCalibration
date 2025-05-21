##Packages!!====================================================================
library(tidyverse)
library(here)
library(patchwork)
library(lme4)
library(MuMIn)
#the files======================================================================
drone=read_csv(here::here("data/raw/Universal Project Data/uasdata.csv"))
procedure=read_csv(here::here("data/raw/ESEAL_FORAGING_2024_REVISED.v2.csv"))
calibration=read.csv(here::here("data/raw/UAS_footprint_to_mass_Calibration_Data.csv"))
#check out the loaded-in frames 
skimr::skim(drone)
skimr::skim(procedure)

summary(procedure$`Recover SL`)

#Female Data: Drone as Predictor for Procedure  ====================================================================== 
##cleaning- changing class/year to a factor ====================================================================== 

drone.1 <- drone %>%
  mutate(class = relevel(as.factor(class),
                         'male', 'female', 'pup' ), 
         year = relevel(as.factor(year), 
                        '2016', '2017', '2018', '2019', '2020', '2021', '2022', '2023', '2024', '2025'))
write_csv(drone.1,"./data/cleaned/uasv1.csv" )

#color palette 
classpalette = c(
  "male" = "#264653", 
  "female" = "#2a9d8f", 
  "pup" = "#9fe9e0")

#cleaning: female datasets  ======================================================================
#we need to match procedures to the right seasson/time frame- this would be post-molt recovery masses
#mainly in january but also including anything in febuary and march (when the UAS are flown)
####splitting "date" into year/month/day columns ======================================================================
drone.2=drone.1 %>% 
  separate(date, into= c("year", "month", "day"),
           sep= "-", remove=FALSE)

procedure.2=procedure %>% 
  separate(RecoverDate, into= c("RecoverYear", "RecoverMonth", "RecoverDay"), 
           sep= "-", remove=FALSE)

####removing the wonky dates:  ======================================================================
#note here: there are 2 data values that are in mm/dd/yy formatting, and won't properly split-
#these are thankfully outside the date range for procedures that we will be using alongside drone.2, so we can force them out 
procedure.2 <- procedure.2 %>%
  filter(!is.na(RecoverYear)) %>% 
  filter(!is.na(RecoverMonth)) %>%
  filter(!is.na(RecoverDay)) 


###new date columns as numerics ======================================================================
#to fix filtering issues 

#as.numeric for drone
drone.2= drone.2 %>% 
  mutate(day = as.numeric(day), 
         month = as.numeric(month),
         year = as.numeric(year), 
         date=as.Date(date))

#as.numeric for procedure
procedure.2=procedure.2 %>% 
  mutate(RecoverDay = as.numeric(RecoverDay), 
         RecoverMonth = as.numeric(RecoverMonth), 
         RecoverYear = as.numeric(RecoverYear),
         date=as.Date(RecoverDate))


###filtering procedure.2 data  ======================================================================
#to the correct year and month ranges to fit drone.2 time frame 

procedure.3=procedure.2 %>% 
  filter(between(RecoverYear, 2016, 2025) & between(RecoverMonth, 1, 3)) %>% 
  filter(Season== "PM")


###pivoting procedure.3- one consolidated "date" ======================================================================

#splitting into new dataframes by collection type: Recovery Procedure and Drone Estimate(s)


recover.1=procedure.3 %>% 
  dplyr::select(ID,TOPPID, RecoverDate, RecoverYear, RecoverMonth, RecoverDay, RecoverMass, `Recover SL`, RecoverAdipose ) %>% 
  rename(date=RecoverDate, year=RecoverYear, month=RecoverMonth, day=RecoverDay, 
         mass=RecoverMass, std.length=`Recover SL`, adipose=RecoverAdipose) %>%
  filter(between(year, 2016, 2025) & between(month, 1, 3)) %>%
  mutate('collection.type'= "recovery") %>% 
  mutate(date=as.Date(date))



drone.3=drone.2 %>% 
  dplyr::select(confidenc, date, year, month, day, class, length, width, area_m2) %>% 
  mutate(length=length*100) %>% 
  mutate('collection.type'= "drone")


#Modelling/Plotting ======================================================================
#i want to run a model to see how strongly/if drone standard length (FEMALES) can predict procedure values, and the same for mass values
#need to calculate mass estimates per drone 

###calculating drone mass estimates  ======================================================================

drone.female =drone.3 %>% 
  mutate('lateral.mass.est'=(255.43*area_m2^1.5)+4.238) %>% #calculate lateral mass estimates (alvarado et. al)
  mutate('dorsal.mass.est'=(268.272*area_m2^1.5)+4.125) %>% #Calculate dorsal mass estimates (alvarado et. al)
  filter(class== "female") %>% #filtering drone data to be female only 
  filter(length<=400) #this is filtering female drone polygons with lengths above 400, since the largest females max out at aprox. 300cm, but we added some wiggle room

skimr::skim(drone.female)


###modelling- gaussian linear model    ======================================================================
###need to collect means by year to properly perform a regression- this forces each frame to have an equal amount of values 
drone.summary=drone.female %>% 
  group_by(year) %>%
  summarise(mean.dorsal = mean(dorsal.mass.est, na.rm = TRUE), 
            mean.lateral = mean(lateral.mass.est, na.rm = TRUE), 
            mean.length= mean(length), na.rm=TRUE) %>%
  mutate(year=as.Date(as.character(year), format = "%Y")) #%>% 
  #filter(year != 2025)#filtering out the 2025 season of drone flights, since there's not an equivalent in the procedure data we currently have

recover.summary=recover.1 %>% 
  group_by(year) %>%
  summarise(mean.mass = mean(mass, na.rm = TRUE), 
            mean.length= mean(std.length), na.rm=TRUE) %>% 
  mutate(year=as.Date(as.character(year), format = "%Y"))

summary(recover.summary$mean.length)

##the models 
m1=lm(recover.summary$mean.mass~drone.summary$mean.dorsal)
m2=lm(recover.summary$mean.mass~drone.summary$mean.lateral)

summary(m1)
summary(m2)
#note that dorsal and lateral footprint predictor models have the same R^2 value!



###plotting some visualizations: est. vs actual  ======================================================================

####color palette  ======================================================================
typepalette = c(
  "Lateral Drone Estimate" = "#EBAA4B", 
  "Dorsal Drone Estimate" = "#EB4900",
  "Drone Length Estimate" = "#F0723A",
  "Drone Estimate"= "#EB4900",
  "Recovery Procedure" = "#9fe9e0")
#"#009483"

###Figure 1: Mass Estimations  ======================================================================
fig1a= ggplot()+
  geom_point(data=drone.female, 
             aes(x=date, y=lateral.mass.est, color= "Lateral Drone Estimate"), 
             alpha=0.7, 
             position=position_jitter(width=100))+
  geom_point(data=drone.female, 
             aes(x=date, y=dorsal.mass.est, color="Dorsal Drone Estimate"), 
             alpha=0.3, 
             position=position_jitter(width=100))+
  geom_point(data= recover.1, 
             aes(x=date, y=mass, color="Recovery Procedure" ), 
             position=position_jitter(width=100))+
#  geom_line(data=drone.summary, 
#            aes(x=year, y=mean.lateral, color= "Lateral Drone Estimate"))+
#  geom_line(data= recover.summary, 
#            aes(x=year, y=mean.mass, color="Recovery Procedure"))+
  scale_color_manual(values = typepalette, name= "Collection Type")+
  scale_x_date(date_breaks= "year", date_labels= "%Y")+
  theme_minimal()+
  ylim(300,850)+
  labs(x= "Year", y= "Mass (kg)")
fig1a

#Weanling Data: Calibration and Mass Estimates   ======================================================================
head(calibration)

###cleaning: weanling dataset   ======================================================================
weanling.1=calibration %>% 
  rename(flight.date = X2025.04.11, 
         days.since.wean=days.btwn) %>% 
  filter(Class == "wean") %>% 
  mutate(wean.mass=mass.act*abs(exp(0.00596 * days.since.wean)), #this is from Holsner et. al. 2021
         rate.mass.loss= 7.6-(0.02*wean.mass), #this is from Noren et. al. 2003
         days.to.flight = as.numeric(ymd(flight.date) - ymd(wean.date)), 
         flight.mass = wean.mass - (rate.mass.loss * days.to.flight * (wean.mass / 1000))) #this is using the rate combined with days between weaning and drone flight to calculate mass on flight day
head(weanling.1)  
write_csv(weanling.1,"./data/cleaned/weanling.calibration.csv" )

###linear modelling- footprint estimates ======================================================================
#preliminary modelling: 

#linear 
wean.linear= lm(flight.mass ~ area.m2, data = weanling.1)
summary(wean.linear)

#mixed effects with tag as a random effect?
wean.mixed= lmer(flight.mass ~ area.m2 + (1 | Tag), data = weanling.1)
summary(wean.mixed)

#using the 1.5 power scaleing 
weanling.2= weanling.1 %>% 
  mutate(area1.5= weanling.1$area.m2^1.5)
wean.model1.5= lmer(flight.mass ~ area1.5 + (1 | Tag), data = weanling.2)
summary(wean.model1.5)


AIC(wean.linear, wean.mixed, wean.model1.5)
#1.5 slightly better 

#Equation pulled from best model following alvarado formatting:

#Estimated Mass (kg)= (171.14×(Footprint Area)^1.5) +52.83
#                    est.^.                          est^

###errors! ======================================================================
#predicted masses 
weanling.2=weanling.2 %>% 
  mutate(predicted.mass = 52.83 + 171.14 * area1.5, 
         residual.err= flight.mass - predicted.mass, 
         percent.error = (residual.err / flight.mass * 100))

#mean +/- SD error
mean_residual= mean(weanling.2$residual)
sd_residual= sd(weanling.2$residual)

#summarize!!! 
cat("Residual error (mean ± SD): ", round(mean_residual, 2), " ± ", round(sd_residual, 2), " kg\n")
#Residual error (mean ± SD):  -0.08  ±  13.14  kg


##visualizing: trying to use Alvarado's formatting 
#predicted vs acutal: 
fig2= ggplot(weanling.1, aes(x = predicted.mass, y = flight.mass)) +
  geom_point(color = "lightblue", size = 3) +        # points for each observation
  geom_abline(slope = 1, intercept = 0,         # 1:1 line for perfect prediction
              color = "orangered", linetype = "dashed") +
  theme_minimal()+
  labs(title = "Predicted vs Actual Mass in Weanling Seals",
       x = "Predicted Mass (kg)",
       y = "Mass on Flight Day (kg)")
fig2 

#T tests- seeing if error is statistically significant 


