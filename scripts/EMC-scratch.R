##Packages!!====================================================================
library(tidyverse)
library(here)
#the files======================================================================
drone=read_csv(here::here("data/raw/Universal Project Data/uasdata.csv"))
#ocean=read_csv(here::here("data/raw/Universal Project Data/oce.indices.csv"))
procedure=read_csv(here::here("data/raw/ESEAL_FORAGING_2024_REVISED.v2.csv"))

skimr::skim(drone)
#skimr::skim(ocean)
skimr::skim(procedure)

#first look ploting ======================================================================
###Basic (in class) ======================================================================
basic=ggplot(data=drone, mapping = aes(x = width , y = length))+
  geom_point(alpha=0.2, aes(color=class))+
  theme_bw()+
  scale_color_viridis_d()
basic

###cleaning- changing class/year to a factor ====================================================================== 

drone.cleanv1 <- drone %>%
  mutate(class = relevel(as.factor(class),
                           'male', 'female', 'pup' ), 
         year = relevel(as.factor(year), 
                          '2016', '2017', '2018', '2019', '2020', '2021', '2022', '2023', '2024', '2025'))
write_csv(drone.cleanv1,"./data/cleaned/uascleanv1.csv" )

#color palette 
classpalette = c(
  "male" = "#264653", 
  "female" = "#2a9d8f", 
  "pup" = "#9fe9e0")

###plot: dodge by age class across years  ======================================================================

fun=ggplot(data=drone.cleanv1, mapping = aes(x = year , y = length, color=class))+
  geom_point(alpha = 0.2, position = position_dodge(width = 0.7)) +
  #facet_grid(class.f~.,switch='y')+
  theme_bw()+
  scale_color_manual(values = classpalette)+
  labs(y= "Polygon Length (m)", x= "Year", color="Class")
fun


#clean up: merged female dataset  ======================================================================
 #for comparisons/prediction work 

###splitting "date" into year/month/day columns ======================================================================
drone.2=drone.cleanv1 %>% 
  separate(date, into= c("year", "month", "day"),
           sep= "-", remove=FALSE)

procedure.2=procedure %>% 
  separate(DeployDate, into= c("DeployYear", "DeployMonth", "DeployDay"), 
           sep= "-", remove=FALSE) %>% 
  separate(RecoverDate, into= c("RecoverYear", "RecoverMonth", "RecoverDay"), 
           sep= "-", remove=FALSE)

####removing the wonky dates:  ======================================================================
#note here: there are 2 data values that are in mm/dd/yy formatting, and won't properly split-
#these are thankfully outside the date range for procedures that we will be using alongside drone.2, so we can force them out 
procedure.2 <- procedure.2 %>%
  filter(!is.na(DeployYear), !is.na(RecoverYear))


###new date columns as numerics ======================================================================
#to fix filtering issues 

#as.numeric for drone
drone.2= drone.2 %>% 
  mutate(day = as.numeric(day), 
         month = as.numeric(month),
         year = as.numeric(year))
#as.numeric for procedure
procedure.2=procedure.2 %>% 
  mutate(DeployDay = as.numeric(DeployDay),
         RecoverDay = as.numeric(RecoverDay), 
         DeployMonth = as.numeric(DeployMonth),
         RecoverMonth = as.numeric(RecoverMonth), 
         DeployYear = as.numeric(DeployYear),
         RecoverYear = as.numeric(RecoverYear))
  
  
###filtering procedure.2 data  ======================================================================
#to the correct year and month ranges to better fit drone.2

procedure.3=procedure.2 %>% 
  #if year is between 2016-2025 for deploy OR recover years, keep it 
  filter(Season== "PM" & between(RecoverYear, 2016, 2025) & between(RecoverMonth, 1, 3) ) 


###pivoting procedure.3- one consolidated "date" ======================================================================

#splitting into new dataframes by collection type

#deploy.1=procedure.3 %>% 
  #dplyr::select(ID,TOPPID, DeployDate, DeployYear, DeployMonth, DeployDay, DeployMass, `Deploy SL`, DeployAdipose) %>% 
  #rename(date=DeployDate, year=DeployYear, month=DeployMonth, day=DeployDay, 
   #      mass=DeployMass, std.length=`Deploy SL`, adipose=DeployAdipose) %>% 
  #filter(between(year, 2016, 2025) & between(month, 1, 3)) %>% 
  #mutate('collection type'= "deployment") 
 

recover.1=procedure.3 %>% 
  dplyr::select(ID,TOPPID, RecoverDate, RecoverYear, RecoverMonth, RecoverDay, RecoverMass, `Recover SL`, RecoverAdipose ) %>% 
  rename(date=RecoverDate, year=RecoverYear, month=RecoverMonth, day=RecoverDay, 
         mass=RecoverMass, std.length=`Recover SL`, adipose=RecoverAdipose) %>%
  filter(between(year, 2016, 2025) & between(month, 1, 3)) %>%
  mutate('collection.type'= "recovery") 
  


drone.3=drone.2 %>% 
  dplyr::select(confidenc, date, year, month, day, class, length, width, area_m2) %>% 
  mutate(length=length*100) %>% 
  mutate('collection.type'= "drone")
  
#Modelling/Plotting ======================================================================
#i want to run a model to see how strongly/if drone standard length (FEMALES) can predict procedure values, and the same for mass values
#need to calculate mass estimates per drone 

##calculating drone mass estimates  ======================================================================

drone.3 =drone.3 %>% 
  mutate('lateral.mass.est'=(255.43*area_m2^1.5)+4.238) %>% 
  mutate('dorsal.mass.est'=(268.272*area_m2^1.5)+4.125)



##filtering drone data to be female only   ======================================================================
drone.female=drone.3 %>% 
  filter(class.f== "female")

##modelling- gaussian linear model    ======================================================================
###need to collect means by year to properly perform a regression
drone.summary=drone.female %>% 
  group_by(year) %>%
  summarise(mean.dorsal = mean(dorsal.mass.est, na.rm = TRUE),
            mean.lateral = mean(lateral.mass.est, na.rm = TRUE)) %>%
  filter(year != 2025)

recover.summary=recover.1 %>% 
  group_by(year) %>%
  summarise(mean.mass = mean(mass, na.rm = TRUE))
  
deploy.summary=deploy.1 %>% 
  group_by(year) %>%
  summarise(mean.mass = mean(mass, na.rm = TRUE))

##the models 
m1=lm(recover.summary$mean.mass~drone.summary$mean.dorsal)
m2=lm(recover.summary$mean.mass~drone.summary$mean.lateral)
m3=lm(deploy.summary$mean.mass~drone.summary$mean.dorsal)
m4=lm(deploy.summary$mean.mass~drone.summary$mean.lateral)

summary(m1)
summary(m2)
summary(m3)
summary(m4)



##color palette  ======================================================================
typepalette = c(
  "Lateral Drone Estimate" = "#EBAA4B", 
  "Dorsal Drone Estimate" = "#EB4900",
  "Drone Length" = "#EBAA4B",
  "Recovery Procedure" = "#9fe9e0",
  "Deployment Procedure"= "#009483")


##plotting some visualizations: est. vs actual
#THESE ARE REALLY UGLY 
  ggplot()+
    geom_point(data=drone.female, aes(x=year, y=lateral.mass.est, color= "Lateral Drone Estimate",), alpha=0.7, position=position_jitter(width=0.3))+
    geom_point(data=drone.female, aes(x=year, y=dorsal.mass.est, color="Dorsal Drone Estimate" ), alpha=0.3, position=position_jitter(width=0.3))+
    geom_point(data= recover.1, aes(x=year, y=mass, color="Recovery Procedure" ), position=position_jitter(width=0.2))+
    #geom_point(data=deploy.1, aes(x=year, y=mass, color="Deployment Procedure" ), alpha=0.8, position=position_jitter(width=0.2))+
    scale_color_manual(values = typepalette, name= "Collection Type")+
    labs(title="Mass Trends by Collection Type, 2016-2025", x="Year", y= "Mass (kg)")
    
  
  
  ggplot()+
    geom_point(data=drone.female, aes(x=year, y=length, color="Drone Length" ), alpha=0.7, position=position_jitter(width=0.2))+
    geom_point(data= recover.1, aes(x=year, y=std.length, color="Recovery Procedure" ), alpha=0.7, position=position_jitter(width=0.2))+
    geom_point(data=deploy.1, aes(x=year, y=std.length, color="Deployment Procedure" ), alpha=0.7, position=position_jitter(width=0.2))+
    scale_color_manual(values = typepalette, name= "Collection Type")
  
  
  
