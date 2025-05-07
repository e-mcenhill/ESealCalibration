##Packages!!====================================================================
library(tidyverse)
library(here)
#the files======================================================================
drone=read_csv(here::here("data/raw/Universal Project Data/uasdata.csv"))
#ocean=read_csv(here::here("data/raw/Universal Project Data/oce.indices.csv"))
procedure=read_csv(here::here("data/raw/ESEAL_FORAGING_2024_REVISED.v2.csv"))

skimr::skim(drone)
skimr::skim(ocean)
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
  mutate(class.f = relevel(as.factor(class),
                           'male', 'female', 'pup' ), 
         year.f = relevel(as.factor(year), 
                          '2016', '2017', '2018', '2019', '2020', '2021', '2022', '2023', '2024', '2025'))
write_csv(drone.cleanv1,"./data/cleaned/uascleanv1.csv" )

#color palette 
classpalette = c(
  "male" = "#264653", 
  "female" = "#2a9d8f", 
  "pup" = "#9fe9e0")

###plot: dodge by age class across years  ======================================================================

fun=ggplot(data=drone.cleanv1, mapping = aes(x = year.f , y = length, color=class.f))+
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
  filter(between(DeployYear, 2016, 2025)| between(RecoverYear, 2016, 2025)) %>% 
  #if MONTH is between 1-3 for deploy OR recover months, keep it 
  filter(between(DeployMonth, 1, 3)|between(RecoverMonth, 1, 3))


###pivoting procedure.3- one consolidated "date" ======================================================================

next














