library(dplyr)
library(DT)
library(tidyr)
library(lubridate)
library(shiny)
library(flexdashboard)
library(ggplot2)
library(d3heatmap)

getTerms <- function(date_time){
  ifelse(
    as_date(date_time) %in% lubridate::ymd(20090625):lubridate::ymd(20090901) |
      as_date(date_time) %in% lubridate::ymd(20100624):lubridate::ymd(20100831) |
      as_date(date_time) %in% lubridate::ymd(20110623):lubridate::ymd(20110830) |
      as_date(date_time) %in% lubridate::ymd(20120621):lubridate::ymd(20120828) |
      as_date(date_time) %in% lubridate::ymd(20130620):lubridate::ymd(20130827) |
      as_date(date_time) %in% lubridate::ymd(20140619):lubridate::ymd(20140826) |
      as_date(date_time) %in% lubridate::ymd(20150625):lubridate::ymd(20150901) |
      as_date(date_time) %in% lubridate::ymd(20160623):lubridate::ymd(20160830),
    "Summer",
    ifelse(
      as_date(date_time) %in% lubridate::ymd(20090923):lubridate::ymd(20091209) |
        as_date(date_time) %in% lubridate::ymd(20100922):lubridate::ymd(20101208) |
        as_date(date_time) %in% lubridate::ymd(20110921):lubridate::ymd(20111207) |
        as_date(date_time) %in% lubridate::ymd(20120910):lubridate::ymd(20121121) |
        as_date(date_time) %in% lubridate::ymd(20130916):lubridate::ymd(20131127) |
        as_date(date_time) %in% lubridate::ymd(20140915):lubridate::ymd(20151126) |
        as_date(date_time) %in% lubridate::ymd(20150916):lubridate::ymd(20151125) |
        as_date(date_time) %in% lubridate::ymd(20160912):lubridate::ymd(20161123),
      "Fall",
      ifelse(
        as_date(date_time) %in% lubridate::ymd(20100104):lubridate::ymd(20100316) |
          as_date(date_time) %in% lubridate::ymd(20110104):lubridate::ymd(20110316) |
          as_date(date_time) %in% lubridate::ymd(20120104):lubridate::ymd(20120314) |
          as_date(date_time) %in% lubridate::ymd(20130107):lubridate::ymd(20130325) |
          as_date(date_time) %in% lubridate::ymd(20140106):lubridate::ymd(20140314) |
          as_date(date_time) %in% lubridate::ymd(20150105):lubridate::ymd(20150317) |
          as_date(date_time) %in% lubridate::ymd(20160104):lubridate::ymd(20160315) |
          as_date(date_time) %in% lubridate::ymd(20170104):lubridate::ymd(20170315),
        "Winter",
        ifelse(
          as_date(date_time) %in% lubridate::ymd(20100329):lubridate::ymd(20100613) |
            as_date(date_time) %in% lubridate::ymd(20110328):lubridate::ymd(20110612) |
            as_date(date_time) %in% lubridate::ymd(20120326):lubridate::ymd(20120610) |
            as_date(date_time) %in% lubridate::ymd(20130325):lubridate::ymd(20130609) |
            as_date(date_time) %in% lubridate::ymd(20140324):lubridate::ymd(20140608) |
            as_date(date_time) %in% lubridate::ymd(20150330):lubridate::ymd(20150614) |
            as_date(date_time) %in% lubridate::ymd(20160328):lubridate::ymd(20160612) |
            as_date(date_time) %in% lubridate::ymd(20170327):lubridate::ymd(20170611),
          "Spring",
          "Intersession"
        )
      )
    )
  )
}



raw <- read.csv("../data/deskTracker.csv", stringsAsFactors = FALSE)
raw <- raw %>%
  mutate(date_time = as.POSIXct(floor_date(ymd_hms(raw$date_time), "hour"))) %>%
  select(response_set, date_time, question, response) %>%
  spread(question, response)

raw <- raw %>%
  mutate(value = as.numeric(ifelse(!is.na(raw$response_set), 1, 0)), term = as.character(getTerms(raw$date_time))) %>%
  select(id = response_set, date_time, mode = `*Type of Communication`, type = `*Type of Transaction`, referral = `Referral to:`, term, value) %>%
  replace_na(replace = list(`mode` = "NA", `type` = "NA", `referral` = "NA"))

df1 <- raw %>%
  select(id, date_time, mode, type, referral, term, value)

# ts <- seq.POSIXt(as.POSIXct("2009-06-25 0:00"), as.POSIXct("2016-08-17 23:59"), by = "hour")
# ts <- data.frame(date_time = ts)

# raw <- full_join(ts, raw)
# raw <- raw %>%
#   mutate(id = response_set, value = as.numeric(ifelse(!is.na(raw$response_set), 1, 0)), term = as.character(getTerms(raw$date_time))) %>%
#   replace_na(replace = list(`*Type of Communication` = "NA", `*Type of Transaction` = "NA", `Referral to:` = "NA")) %>%
#   select(id, date_time, mode = `*Type of Communication`, type = `*Type of Transaction`, referral = `Referral to:`, term, value)
