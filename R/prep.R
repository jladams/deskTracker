library(dplyr)
library(tidyr)
library(lubridate)
library(shiny)
library(flexdashboard)
library(ggplot2)
library(d3heatmap)

raw <- read.csv("../data/deskTracker.csv")

df1 <- raw %>%
  select(response_set,date_time,question,response) %>%
  spread(question, response) %>%
  select(id = response_set, date = date_time, mode = `*Type of Communication`, type = `*Type of Transaction`, referral = `Referral to:`)