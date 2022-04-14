rm(list = ls())
getwd()
setwd("~/Box Sync/POL 251 teach/Learning R")
###
#install.packages("cfbfastR")
#install.packages("zoo")
#install.packages("ggimage")
#install.packages("gt")
library(cfbfastR)
library(zoo)
library(ggimage)
library(gt)
library(tidyverse)
library(haven)
library(naniar)
library(tictoc)
library(progressr)
library(future)
library(plotly)
##CFB key 
Sys.setenv(CFBD_API_KEY = "6EurqDrc/VrqE63FxvrqvMHR8sFIUBixpN8+kyGiTD1j+nmdhFVp9MQRzCncl0r")

usethis::edit_r_environ()

pbp <- data.frame()
seasons <- 2019:cfbfastR:::most_recent_season()


pbp <- cfbfastR::load_cfb_pbp(2021)

summary(yards_gained[pos_team == "Ole Miss"])

attach(pbp)
unique(pos_team)

head(pbp)
unique(offense_play)


  
grouped_means <- pbp %>% 
  group_by(offense_play , offense_conference) %>% 
  summarise(
    pass = mean(yards_gained [pass == 1]),
    rush = mean(yards_gained[rush == 1]) , 
    ypp = mean(yards_gained))

attach(grouped_means)
unique(offense_play[offense_conference== "SEC"])

test <- ggplot(data = grouped_means ,  aes(text = offense_play,  x=pass , y = rush  ))+
  geom_point(aes (color = offense_conference))+
  ylab ("Rush Average Per Play")+
  xlab("Pass Average Per Play")+
  ggtitle("2021 Average Yards Per Play by Play Type"  ) +
  scale_color_discrete(name = "Conference") +
  theme (plot.title = element_text(hjust = 0.5) , axis.text=element_text(size=12), axis.title=element_text(size=14,face="bold"))

ggplotly(test , tooltip = c("text" , "x" , "y"))




