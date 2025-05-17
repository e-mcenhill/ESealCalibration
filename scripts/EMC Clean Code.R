##Packages!!====================================================================
library(tidyverse)
library(here)
library(patchwork)
#the files======================================================================
drone=read_csv(here::here("data/raw/Universal Project Data/uasdata.csv"))
procedure=read_csv(here::here("data/raw/ESEAL_FORAGING_2024_REVISED.v2.csv"))

#check out the loaded-in frames 
skimr::skim(drone)
skimr::skim(procedure)

summary(procedure$`Recover SL`)

###cleaning- changing class/year to a factor ====================================================================== 

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


###Drone Trends: dodge polygon length by age class across years  ======================================================================

fig1=ggplot(data=drone.1, mapping = aes(x = year , y = length, color=class))+
  geom_point(alpha = 0.2, position = position_dodge(width = 0.7)) +
  #facet_grid(class.f~.,switch='y')+
  theme_bw()+
  scale_color_manual(values = classpalette)+
  labs(y= "Polygon Length (m)", x= "Year", color="Class")
fig1

#clean up: merged female dataset  ======================================================================
#we need to match procedures to the right seasson/time frame- this would be post-molt recovery masses
#mainly in january but also including anything in febuary and march (when the UAS are flown)
###splitting "date" into year/month/day columns ======================================================================
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

##calculating drone mass estimates  ======================================================================

drone.female =drone.3 %>% 
  mutate('lateral.mass.est'=(255.43*area_m2^1.5)+4.238) %>% #calculate lateral mass estimates (alvarado et. al)
  mutate('dorsal.mass.est'=(268.272*area_m2^1.5)+4.125) %>% #Calculate dorsal mass estimates (alvarado et. al)
  filter(class== "female") %>% #filtering drone data to be female only 
  filter(length<=400) #this is filtering female drone polygons with lengths above 400, since the largest females max out at aprox. 300cm, but we added some wiggle room

skimr::skim(drone.female)


##modelling- gaussian linear model    ======================================================================
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




##color palette  ======================================================================
typepalette = c(
  "Lateral Drone Estimate" = "#EBAA4B", 
  "Dorsal Drone Estimate" = "#EB4900",
  "Drone Length Estimate" = "#F0723A",
  "Drone Estimate"= "#EB4900",
  "Recovery Procedure" = "#9fe9e0")
 #"#009483"


##plotting some visualizations: est. vs actual  ======================================================================

###Figure 2: Mass Estimations  ======================================================================
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

###Figure 3: Length Estimations  ======================================================================
fig1b= ggplot()+
  geom_point(data=drone.female, 
             aes(x=date, y=length, color="Drone Estimate" ), 
             alpha=0.7, 
             position=position_jitter(width=100))+
  geom_point(data= recover.1, 
             aes(x=date, y=std.length, color="Recovery Procedure" ), 
             position=position_jitter(width=100))+
#  geom_line(data=drone.summary, 
#            aes(x=year, y=mean.length, color= "Drone Estimate"))+
#  geom_line(data= recover.summary, 
#            aes(x=year, y=mean.length, color="Recovery Procedure"))+
  scale_color_manual(values = typepalette, name= "Collection Type")+
  scale_x_date(date_breaks= "year", date_labels= "%Y")+
  theme_minimal()+
  labs(x="Year", y= "Length (cm)")
fig1b

fig1ab=fig1a /plot_spacer() /fig1b + 
  plot_layout(heights =c(4,0.2,4), axes="collect")+ 
  plot_annotation(title="Morphometrics Trends by Collection Type", subtitle= "(2016-2025)", tag_levels = 'A')
fig1ab

ggsave("Figure 1.png", plot=fig1ab, device=png, path=here::here("images"), width=2200, height=1200, units="px", dpi=320)


#WIP: maybe separate patches for mean lines 
# fig2a= ggplot()+
#   geom_line(data=drone.summary, 
#           aes(x=year, y=mean.lateral, color= "Lateral Drone Estimate"))+
#   geom_line(data= recover.summary, 
#             aes(x=year, y=mean.mass, color="Recovery Procedure"))+
#   scale_color_manual(values = typepalette, name= "Collection Type")+
#   scale_x_date(date_breaks= "year", date_labels= "%Y")+
#   theme_minimal()+
#   ylim(300,850)+
#   theme(axis.title.x=element_blank(),
#         axis.text.x=element_blank(),
#         axis.ticks.x=element_blank()) #+
#   #labs(title="Morphometrics Trends by Collection Type, 2016-2025", y= "Mean Mass (kg)")
# fig2a
#   
# fig2b= ggplot()+
#   geom_line(data=drone.summary, 
#             aes(x=year, y=mean.length, color= "Drone Estimate" ))+
#   geom_line(data= recover.summary, 
#             aes(x=year, y=mean.length, color="Recovery Procedure"))+
#   scale_color_manual(values = typepalette, name= "Collection Type")+
#   scale_x_date(date_breaks= "year", date_labels= "%Y")+
#   theme_minimal()+
#   labs(x="Year", y= "Mean Length (cm)")
# fig2b
# 
# fig2ab=fig2a /fig2b
# fig2ab
# 
# fig2ab/fig1ab
