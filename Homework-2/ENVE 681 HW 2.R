# Homework 2 ------------------
# 10/18/17
#------------------------------

library(readxl)
library(tidyverse)
library(lubridate)

# Reading data into R Studio
setwd("F:/ENVE 681") # sets the working directory to specified file name
ppt <- read_excel("HW 2/ppt.xlsx") # reads in specified excel file
View(ppt) # view imported table

# creating a new data frame called xppt; ":" squences the numbers from 1980 to 2014 for row names
# in tidyverse 
# "%>% called the pipe and chains commands together (pipeline)

xppt <- ppt %>% # creates new data table xppt with ppt data
  mutate(year = 1980:2014) # creates a column in xppt from ppt data table with the years sequenced from 1980 to 2014
                           # number of rows in new column year should equal number of rows exisiting


#### Question 2 ####
answer_2 <- xppt %>% # assigns xppt to answer_2;  
  gather(key = "month", value = "value", Jan:Dec) %>% # pivots table month row into a column for all values Jan to Dec
  group_by(year) %>% #groups by year
  summarize(sum = sum(value)) # summarizes the data by sum of values for each grouping of years

ggplot(answer_2, aes(year, sum)) + #plots data from data table answer_2 with year as x and sum as y
  geom_line() + #plots a line
  scale_x_continuous(breaks = seq(1980, 2014, 2)) + #scales the x axis starting at 1980 to 2014 every 2 years
  theme_bw() + # a graph theme
  # adjusts the x axis labels by 90 degrees, removesthe minor grid lines
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5),
        panel.grid.minor.x = element_blank()) + 
  labs(x = "Year", y = "Precipitation\n(in)", title = "Total Annual Precipitation") # creates labels for the graph

#### Question 3 ####
xppt %>%
  gather(key = "month", value = "value", Jan:Dec) %>% # pivots table month row into a column for all values Jan to Dec
  filter(year == 1990) %>% # filters for specified year
  group_by(year) %>% # groups the year together
  # creates a summary table that calculates the mean, min, and max for column "values" for the grouping
  summarize(mean = mean(value), 
            min = min(value),
            max = max(value)) 

#### Question 4 ####
answer_4 <- xppt %>% # creates and assigns xppt to answer_4 table
  gather(key = "month", value = "value", Jan:Dec) %>% # pivots table month row into a column for all values Jan to Dec
  # converts month names into factors and orders them based on specified vector
  mutate(month = factor(month, levels = c("Jan", "Feb", "Mar", "April", "May", "June",
                                          "July", "Aug", "Sept", "Oct", "Nov", "Dec"))) %>% 
  group_by(month) %>% # creates groupings based on months
  # creates a summary table that calculates the mean and standard deviation based on month groupings
  summarize(mean = mean(value),
            std = sd(value))

ggplot(answer_4, aes(month, mean)) + # creates a plot with data from table answer_4 with x = month and y - mean
  geom_col(fill = "#9C9C9C") + # creates a bar/column graph with the color fill as #9C9C9C <- from addin colourpicker
  geom_errorbar(aes(x = month, ymin = mean - std, ymax = mean + std), color = "black") + # plots the errorbars
  theme_bw() + # a type of theme
  labs(x = "Month", y = "Average Precipitation\n(in)", 
       title = "Average Precipitation by Month", subtitle = "Data from 1980 to 2014") # creates labels for the graph

#### Question 5 ####
answer_5 <- xppt %>% # creates and assigns data from xppt to answer_5
  gather(key = "month", value = "value", Jan:Dec) %>% # pivots table month row into a column for all values Jan to Dec
  # converts month names into factors and orders them based on specified vector
  mutate(month = factor(month, levels = c("Jan", "Feb", "Mar", "April", "May", "June",
                                          "July", "Aug", "Sept", "Oct", "Nov", "Dec"))) %>%
  # creates a date column and merges year and month columns together and converts  into readable format (YYYY-mm-dd)
  mutate(date = ifelse(as.numeric(month) < 9, 
                       paste(as.character(year), "-0", as.numeric(month), "-01", sep = ""), 
                       paste(as.character(year), "-", as.numeric(month), "-01", sep = ""))) %>% 
  # converts date column into a date/time format for R
  mutate(date = as.POSIXct(strptime(date, format = "%Y-%m-%d")))

ggplot(answer_5, aes(date, value)) + # creates a plot with data from table answer_5 with x = date and y = value
  geom_line() + # creates a line graph
  scale_x_datetime(date_breaks = "5 years", date_labels = "%Y") + # sets the x-scale for 5 years with year as label
  theme_bw() + # plot theme
  labs(x = "Date", y = "Precipitation\n(in)", title = "Yearly Precipitation", subtitle = "Data from 1980 to 2014") # creates graph labels
  
