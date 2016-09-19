
# Install and load packages
# pkgs <- c("dplyr", "plotly", "tidyr", "lubridate", "stringr", "shiny", "flexdashboard", "ggplot2", "d3heatmap")

# toInstall <- pkgs[!(pkgs %in% installed.packages()[,"Package"])]
# if(length(toInstall)) { install.packages(toInstall) }
# lapply(pkgs, library)

# if(!("sumar" %in% installed.packages())) { devtools::install_github("jladams/sumar")}
# library("sumar")

# rm(list = c("pkgs", "toInstall"))

library(d3heatmap)
library(dplyr)
library(flexdashboard)
library(ggplot2)
library(lubridate)
library(plotly)
library(shiny)
library(stringr)
library(tidyr)

# Turn off the PDF renderer from plotly (generates a file that breaks Shiny)
pdf(NULL)

# Compare dates and times to Dartmouth academic calendar, return Term name
getTerms <- function(date_time){
  ifelse(
    ymd(as_date(date_time)) %in% lubridate::ymd(20090625):lubridate::ymd(20090901) |
      ymd(as_date(date_time)) %in% lubridate::ymd(20100624):lubridate::ymd(20100831) |
      ymd(as_date(date_time)) %in% lubridate::ymd(20110623):lubridate::ymd(20110830) |
      ymd(as_date(date_time)) %in% lubridate::ymd(20120621):lubridate::ymd(20120828) |
      ymd(as_date(date_time)) %in% lubridate::ymd(20130620):lubridate::ymd(20130827) |
      ymd(as_date(date_time)) %in% lubridate::ymd(20140619):lubridate::ymd(20140826) |
      ymd(as_date(date_time)) %in% lubridate::ymd(20150625):lubridate::ymd(20150901) |
      ymd(as_date(date_time)) %in% lubridate::ymd(20160623):lubridate::ymd(20160830),
    "Summer",
    ifelse(
      ymd(as_date(date_time)) %in% lubridate::ymd(20090923):lubridate::ymd(20091209) |
        ymd(as_date(date_time)) %in% lubridate::ymd(20100922):lubridate::ymd(20101208) |
        ymd(as_date(date_time)) %in% lubridate::ymd(20110921):lubridate::ymd(20111207) |
        ymd(as_date(date_time)) %in% lubridate::ymd(20120910):lubridate::ymd(20121121) |
        ymd(as_date(date_time)) %in% lubridate::ymd(20130916):lubridate::ymd(20131127) |
        ymd(as_date(date_time)) %in% lubridate::ymd(20140915):lubridate::ymd(20141126) |
        ymd(as_date(date_time)) %in% lubridate::ymd(20150916):lubridate::ymd(20151125) |
        ymd(as_date(date_time)) %in% lubridate::ymd(20160912):lubridate::ymd(20161123),
      "Fall",
      ifelse(
        ymd(as_date(date_time)) %in% lubridate::ymd(20100104):lubridate::ymd(20100316) |
          ymd(as_date(date_time)) %in% lubridate::ymd(20110104):lubridate::ymd(20110316) |
          ymd(as_date(date_time)) %in% lubridate::ymd(20120104):lubridate::ymd(20120314) |
          ymd(as_date(date_time)) %in% lubridate::ymd(20130107):lubridate::ymd(20130325) |
          ymd(as_date(date_time)) %in% lubridate::ymd(20140106):lubridate::ymd(20140314) |
          ymd(as_date(date_time)) %in% lubridate::ymd(20150105):lubridate::ymd(20150317) |
          ymd(as_date(date_time)) %in% lubridate::ymd(20160104):lubridate::ymd(20160315) |
          ymd(as_date(date_time)) %in% lubridate::ymd(20170104):lubridate::ymd(20170315),
        "Winter",
        ifelse(
          ymd(as_date(date_time)) %in% lubridate::ymd(20100329):lubridate::ymd(20100613) |
            ymd(as_date(date_time)) %in% lubridate::ymd(20110328):lubridate::ymd(20110612) |
            ymd(as_date(date_time)) %in% lubridate::ymd(20120326):lubridate::ymd(20120610) |
            ymd(as_date(date_time)) %in% lubridate::ymd(20130325):lubridate::ymd(20130609) |
            ymd(as_date(date_time)) %in% lubridate::ymd(20140324):lubridate::ymd(20140608) |
            ymd(as_date(date_time)) %in% lubridate::ymd(20150330):lubridate::ymd(20150614) |
            ymd(as_date(date_time)) %in% lubridate::ymd(20160328):lubridate::ymd(20160612) |
            ymd(as_date(date_time)) %in% lubridate::ymd(20170327):lubridate::ymd(20170611),
          "Spring",
          "Intersession"
        )
      )
    )
  )
}

# Load and format deskTracker data
deskTracker <- read.csv("./data/deskTracker.csv", stringsAsFactors = FALSE)

deskTracker <- deskTracker %>%
  mutate(date_time = as.character.Date(floor_date(ymd_hms(deskTracker$date_time), "hour"))) %>%
  select(response_set, date_time, question, response) %>%
  spread(question, response)

deskTracker <- deskTracker %>%
  mutate(value = as.numeric(ifelse(!is.na(deskTracker$response_set), 1, 0)), term = as.character(getTerms(deskTracker$date_time))) %>%
  select(id = response_set, date_time, mode = `*Type of Communication`, type = `*Type of Transaction`, referral = `Referral to:`, term, value)

deskTracker$type <- replace(deskTracker$type, deskTracker$type == "In Depth Reference (teaches strategy, lengthier)", "In Depth Reference")
deskTracker$type <- replace(deskTracker$type, deskTracker$type == "Equipment (computing, copy/scan, printing, A/V)", "Technology Issues")

# Call to Suma API, save locally
# suma <- suma_from_api(initiativeId = 5, startDate = "2016-08-01", sepDates = FALSE) 
# write.csv(suma, "./data/suma.csv", row.names = FALSE)

# Load and format Suma Data
suma <- read.csv("./data/suma.csv", stringsAsFactors = FALSE)

suma <- suma %>% 
  mutate(date_time = as.character.Date(floor_date(ymd_hms(suma$sessionStart), "hour"))) %>%
  select(countId, date_time, actGroup, activities) %>%
  filter(actGroup != "No Activity") %>%
  spread(actGroup, activities)

suma <- suma %>%
  mutate(value = as.numeric(ifelse(!is.na(suma$countId), 1, 0)), term = as.character(getTerms(suma$date_time))) %>%
  select(id = countId, date_time, mode = `Communication Type`, type = `Transaction Type`, referral = `Referral To`, term, value)



# Combine and properly format deskTracker and Suma data
df1 <- deskTracker %>%
  rbind(suma) %>%
  filter((!is.na(mode)) & (!is.na(type))) %>%
  select(date_time, mode, type, referral, term, value) %>%
  complete(date_time, nesting(mode, type), fill = list(value = 0)) %>%
  fill(term) %>%
  replace_na(replace = list(`referral` = "None", `term` = "Summer"))



# Filling in missing values
# ts <- seq.POSIXt(as.POSIXct("2009-06-25 0:00"), as.POSIXct("2016-08-17 23:59"), by = "hour")
# ts <- data.frame(date_time = ts)

# raw <- full_join(ts, raw)
# raw <- raw %>%
#   mutate(id = response_set, value = as.numeric(ifelse(!is.na(raw$response_set), 1, 0)), term = as.character(getTerms(raw$date_time))) %>%
#   replace_na(replace = list(`*Type of Communication` = "NA", `*Type of Transaction` = "NA", `Referral to:` = "NA")) %>%
#   select(id, date_time, mode = `*Type of Communication`, type = `*Type of Transaction`, referral = `Referral to:`, term, value)