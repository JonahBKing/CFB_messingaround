rm(list = ls())
getwd()
setwd("~/Box Sync/Football stats")###
#install.packages("cfbfastR")
#install.packages("zoo")
#install.packages("ggimage")
#install.packages("gt")
library(cfbfastR)
library(zoo)
library(ggimage)
library(gt)
library(tidyverse)
library(plotly)
##CFB key 
Sys.setenv(CFBD_API_KEY = "6EurqDrc/VrqE63FxvrqvMHR8sFIUBixpN8+kyGiTD1j+nmdhFVp9MQRzCncl0r")



#can load multiple seasons with below data, but computer cant handle
#seasons <- 2019:cfbfastR:::most_recent_season()


newdf <- read.csv("2021cfb.csv")


summary(yards_gained[offense_play == "Ole Miss"])
summary(EPA [offense_score_play == 1 & offense_play == "Ole Miss"])
attach(newdf)


## have to filter out turnovers for yards gained graph
view(play_text [offense_play == "Ole Miss" & pass == 1 & success == 0 & yards_gained > 40])



##yards gained and epa graph
cols <- c("1" = "green", "0" = "red")
shape <- c("0" = "circle" , "1" = "triangle" )
ggplot(subset(newdf , offense_play %in% "Ole Miss" & turnover %in% "0" ) , aes(x= yards_gained , y = EPA,  color = as.factor(success) , shape = as.factor(pass) , alpha = .5) )+
  geom_point( size = 3 ) +
  geom_jitter()+
  ggtitle("2021 Ole Miss Offensive Plays")+
  ylab("Expected Points Added")+
  xlab("Yards Gained")+
  scale_color_manual(name = "Successful Play" , values = cols , breaks = c("0", "1"), labels = c("No", "Yes"))+
  scale_shape_manual(name = "Play Type" , values = shape , breaks = c("0", "1"), labels = c("Rush", "Pass"))+
  guides(alpha = FALSE )+
  theme (plot.title = element_text(hjust = 0.5, size = 18 , face = "bold") , axis.text=element_text(size=12), axis.title=element_text(size=14) , 
         panel.border = element_blank(), panel.grid.major = element_blank(),
         panel.grid.minor = element_blank(), panel.background = element_rect(fill = "white",colour = "black",size = 0.5, linetype = "solid"),
         axis.line = element_line(size = 0.5, linetype = "solid", colour = "black"),
         legend.key.size = unit(1, 'cm'), 
         legend.key.height = unit(1, 'cm'), 
         legend.key.width = unit(1, 'cm'), 
         legend.title = element_text(size=14),
         legend.text = element_text(size=10))

#make interactive graph for rush and pass average 
grouped_means <- newdf %>% 
  group_by(offense_play , offense_conference) %>% 
  summarise(
    pass = mean(yards_gained [pass == 1 &  turnover  == "0"]),
    rush = mean(yards_gained[rush == 1  &  turnover  == "0"]),
    success = mean (success , na.rm = TRUE), 
    epa = mean (EPA , na.rm = TRUE),
    ypp = mean(yards_gained))

attach(grouped_means)
test <- ggplot(data = grouped_means ,  aes(text = offense_play,  x=pass , y = rush  ))+
  geom_point(aes (color = offense_conference))+
  ylab ("Rush Average Per Play")+
  xlab("Pass Average Per Play")+
  ggtitle("2021 Average Yards Per Play by Play Type"  ) +
  scale_color_discrete(name = "Conference" ) +
  theme (plot.title = element_text(hjust = 0.5, size = 18 , face = "bold") , 
         axis.text=element_text(size=12), axis.title=element_text(size=14) , 
         panel.border = element_blank(),
         panel.grid.major = element_blank(),
         panel.grid.minor = element_blank(),
         panel.background = element_rect(fill = "white",
                                         colour = "black",
                                         size = 0.5, linetype = "solid"),
         axis.line = element_line(size = 0.5, linetype = "solid",
                                  colour = "black"),
         legend.key.size = unit(1, 'cm'), #change legend key size
         legend.key.height = unit(1, 'cm'), #change legend key height
         legend.key.width = unit(1, 'cm'), #change legend key width
         legend.title = element_text(size=14), #change legend title font size
         legend.text = element_text(size=10))

final <- ggplotly(test , tooltip = c("text" , "x" , "y"))
final

##

### epa graph. pretty good 

###output this data to csv so i dont have to do this everytime
view(play_text [EPA < -10 & down == "3" & pos_team == "Ole Miss" ])


cfbPalette <- c("#9E1B32" ,"#006BA6"  )

background <- rgb(130,87,54 ,max=255,  alpha = .25)

theme_football <- function (base_size = 11, base_family = "") {
  theme_minimal() %+replace% 
    theme(
      plot.title = element_text(hjust = 0.5, size = 24 , family = "mono" , face = "bold.italic" ) ,
      plot.subtitle = element_text(hjust = 0.5, size = 20 , family = "mono" , face = "bold.italic" ), 
      panel.grid.major.x = element_line(color = "white"),
      panel.grid.major.y = element_blank(),
      panel.grid.minor.y = element_blank(),
      panel.grid.minor.x = element_blank(),
      panel.background = element_rect(fill = "lightgreen"),
      plot.background = element_rect(fill = "#cb9279" ),
      panel.border = element_rect(color = "lightgreen", fill = NA),
      axis.line = element_line(color = "lightgreen"),
      axis.ticks = element_line(color = "lightgreen"),
      axis.text = element_text(size = 10 , color = "darkgreen" , face = "bold"), 
      strip.text = element_text(size=15 , vjust = 1),
      axis.title = element_text(size = 20),
      legend.text = element_text(size = 12),
      legend.title = element_text(size = 15),
#this isnt right but maybe in the right direction      legend.key.size = unit(2, "cm"),
      text = element_text(family = "mono" , face = "bold.italic"),
    )
}


newdf %>% 
  filter(offense_play %in% c('Ole Miss', "Alabama") & down %in% c(1, 2, 3, 4) ) %>% 
  ggplot(aes(x = distance, y = EPA)) +
  geom_point(aes(color = offense_play , alpha = .75 , size = 1.25) ) +
  geom_hline(linetype = "dashed", size = 1,   yintercept = 0)+
#  geom_smooth(aes(color = offense_play ), se=FALSE)+
#  stat_ellipse(aes(color = offense_play , alpha = .75 , fill= offense_play) , geom = "polygon") +
  facet_wrap(~down, labeller = labeller(down = 
                                          c("1" = "First Down",
                                            "2" = "Second Down",
                                            "3" = "Third Down",
                                            "4" = "Fourth Down"))) +
  scale_color_manual(name = "Team" , values =cfbPalette) +
  guides( alpha = FALSE , size = FALSE , color = guide_legend(override.aes = list(size = 5)))+
  labs(x        = 'Distance to Go',
       y        = 'Expexted Points Added',
       title    = 'Expected Points Added \nby Distance',
       ) +
  scale_x_continuous(breaks = c(0,5,10 ,20, 30 ), lim = c(0, 40))+
  theme_football()  -> bamaolemiss_epa_down_distance
  

bamaolemiss_epa_down_distance



playPalette <- c("yellow" ,"red" , "darkblue"  )

newdf %>% 
  filter(offense_play %in% 'Ole Miss' & down %in% "1" & distance %in% "10" & yards_to_goal >0 ) %>% 
  mutate(short_playtype = case_when(pass == 1 ~ 1,
                                    rush == 1 ~ 0,
                                    punt == 1 ~ -1,
                                    fg_inds == 1 ~ -1 ,
                                    TRUE ~ -99)) %>% 
  filter(short_playtype > -1) %>% 
  mutate(short_playtype = case_when(short_playtype == 1 ~ "Pass",
                                    short_playtype == 0 ~ "Rush" ,
                                    short_playtype == -1 ~ "Kick")) %>% 
  ggplot(aes(x = yards_to_goal, y = EPA)) +
  geom_point(aes(color = as.factor(short_playtype) , alpha = .5 , size = 1.25) )+
  geom_smooth(aes(color = as.factor(short_playtype) , size = 1) , se = FALSE)+
  geom_hline(linetype = "dashed", size = 1,   yintercept = 0)+
  scale_color_manual(name = "Play Type" , values =playPalette2) +
  guides( alpha = FALSE , size = FALSE , color = guide_legend(override.aes = list(size = 5)))+
  labs(x        = 'Yard Line',
       y        = 'Expexted Points Added',
       title    = 'What Should Ole Miss Do On First and 10?',
       subtitle = "By Field Position") +
 scale_x_reverse(breaks = seq(0, 100, by = 10), 
                 labels=c("Goal", "10", "20" , "30" , "40" , "50" , "40" , "30" , "20" , "10", "Goal"),
                 lim= c(100 , 0))+
  theme_football()  -> firstdown_plot 


firstdown_plot



  
 #facet_wrap(~pass, labeller = labeller(pass = 
 #                        c("0" = "Run",
 #                         "2" = "Pass",
 #                        "3" = "Third Down",
 #                       "4" = "Fourth Down"))) +
 

playPalette2 <- c("red" , "darkblue"  )

newdf %>% 
  filter(offense_play %in% 'Ole Miss' & down %in% "1" & distance %in% "10" & yards_to_goal >0 ) %>% 
  mutate(short_playtype = case_when(pass == 1 ~ 1,
                                    rush == 1 ~ 0,
                                    punt == 1 ~ -1,
                                    fg_inds == 1 ~ -1 ,
                                    TRUE ~ -99)) %>% 
  filter(short_playtype > -1) %>% 
  mutate(short_playtype = case_when(short_playtype == 1 ~ "Pass",
                                    short_playtype == 0 ~ "Rush" ,
                                    short_playtype == -1 ~ "Kick")) %>% 
  ggplot(aes(x = yards_to_goal, y = EPA)) +
  geom_point(aes(color = as.factor(short_playtype) , alpha = .5 , size = 1.25) )+
  geom_smooth(aes(color = as.factor(short_playtype) , size = 1) , se = FALSE)+
  geom_hline(linetype = "dashed", size = 1,   yintercept = 0)+
  facet_wrap(~ short_playtype , nrow = 2)+
  scale_color_manual(name = "Play Type" , values =playPalette2) +
  guides( alpha = FALSE , size = FALSE , color = guide_legend(override.aes = list(size = 5)))+
  labs(x        = 'Yard Line',
       y        = 'Expexted Points Added',
       title    = 'What Should Ole Miss Do On First and 10?',
       subtitle = "By Field Position") +
  scale_x_reverse(breaks = seq(0, 100, by = 10), 
                  labels=c("Goal", "10", "20" , "30" , "40" , "50" , "40" , "30" , "20" , "10", "Goal"),
                  lim= c(100 , 0))+
  theme_football()  -> wrapped_by_playtype

wrapped_by_playtype


#fourth down 

newdf %>% 
  filter(offense_play %in% 'Ole Miss' & down %in% "4"  & distance < 10 & yards_to_goal >0 ) %>% 
  mutate(short_playtype = case_when(pass == 1 ~ 1,
                                    rush == 1 ~ 0,
                                    punt == 1 ~ -1,
                                    fg_inds == 1 ~ -1 ,
                                    TRUE ~ -99)) %>% 
  filter(short_playtype > -2) %>% 
  mutate(short_playtype = case_when(short_playtype == 1 ~ "Pass",
                                    short_playtype == 0 ~ "Rush" ,
                                    short_playtype == -1 ~ "Kick")) %>% 
  ggplot(aes(x = yards_to_goal, y = EPA)) +
  geom_point(aes(color = as.factor(short_playtype) , alpha = .5 , size = 1.25) )+
#  geom_smooth(aes(color = as.factor(short_playtype) , size = 1) , se = FALSE)+
  geom_hline(linetype = "dashed", size = 1,   yintercept = 0)+
  scale_color_manual(name = "Play Type" , values =playPalette) +
  guides( alpha = FALSE , size = FALSE , color = guide_legend(override.aes = list(size = 5)))+
  labs(x        = 'Yard Line',
       y        = 'Expexted Points Added',
       title    = 'Did Ole Miss Do The Right \nThing On Fourth Down',
       subtitle = "By Field Position") +
  scale_x_reverse(breaks = seq(0, 100, by = 10), 
                  labels=c("Goal", "10", "20" , "30" , "40" , "50" , "40" , "30" , "20" , "10", "Goal"),
                  lim= c(100 , 0))+
  theme_football() -> fourthdownplot




### msu first down plot 

playPalettemsu <- c("#660000" , "white"  )


newdf %>% 
  filter(offense_play %in% "Mississippi State"  & down %in% "1" & distance %in% "10" & yards_to_goal >0 ) %>% 
  mutate(short_playtype = case_when(pass == 1 ~ 1,
                                    rush == 1 ~ 0,
                                    punt == 1 ~ -1,
                                    fg_inds == 1 ~ -1 ,
                                    TRUE ~ -99)) %>% 
  filter(short_playtype > -1) %>% 
  mutate(short_playtype = case_when(short_playtype == 1 ~ "Pass",
                                    short_playtype == 0 ~ "Rush" ,
                                    short_playtype == -1 ~ "Kick")) %>% 
  ggplot(aes(x = yards_to_goal, y = EPA)) +
  geom_point(aes(color = as.factor(short_playtype) , alpha = .25 , size = 1.25) )+
  geom_smooth(aes(color = as.factor(short_playtype) , size = 1) , se = FALSE)+
  geom_hline(linetype = "dashed", size = 1,   yintercept = 0)+
  scale_color_manual(name = "Play Type" , values =playPalettemsu) +
  guides( alpha = FALSE , size = FALSE , color = guide_legend(override.aes = list(size = 5)))+
  labs(x        = 'Yard Line',
       y        = 'Expexted Points Added',
       title    = 'What Should MSU Do On First and 10?',
       subtitle = "By Field Position") +
  scale_x_reverse(breaks = seq(0, 100, by = 10), 
                  labels=c("Goal", "10", "20" , "30" , "40" , "50" , "40" , "30" , "20" , "10", "Goal"),
                  lim= c(100 , 0))+
  theme_football()  -> firstdown_plot_msu


firstdown_plot_msu


##wrap
newdf %>% 
  filter(offense_play %in% 'Mississippi State' & down %in% "1" & distance %in% "10" & yards_to_goal >0 ) %>% 
  mutate(short_playtype = case_when(pass == 1 ~ 1,
                                    rush == 1 ~ 0,
                                    punt == 1 ~ -1,
                                    fg_inds == 1 ~ -1 ,
                                    TRUE ~ -99)) %>% 
  filter(short_playtype > -1) %>% 
  mutate(short_playtype = case_when(short_playtype == 1 ~ "Pass",
                                    short_playtype == 0 ~ "Rush" ,
                                    short_playtype == -1 ~ "Kick")) %>% 
  ggplot(aes(x = yards_to_goal, y = EPA)) +
  geom_point(aes(color = as.factor(short_playtype) , alpha = .5 , size = 1.25) )+
  geom_smooth(aes(color = as.factor(short_playtype) , size = 1) , se = FALSE)+
  geom_hline(linetype = "dashed", size = 1,   yintercept = 0)+
  facet_wrap(~ short_playtype , nrow = 2)+
  scale_color_manual(name = "Play Type" , values =playPalettemsu) +
  guides( alpha = FALSE , size = FALSE , color = guide_legend(override.aes = list(size = 5)))+
  labs(x        = 'Yard Line',
       y        = 'Expexted Points Added',
       title    = 'What Should MSU Do On First and 10?',
       subtitle = "By Field Position") +
  scale_x_reverse(breaks = seq(0, 100, by = 10), 
                  labels=c("Goal", "10", "20" , "30" , "40" , "50" , "40" , "30" , "20" , "10", "Goal"),
                  lim= c(100 , 0))+
  theme_football()  -> wrapped_by_playtypemsu

wrapped_by_playtypemsu




