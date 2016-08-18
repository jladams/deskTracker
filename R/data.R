raw <- read.csv("../data/deskTracker.csv")

df <- reactive({
  df <- raw %>%
  select(response_set,date_time,question,response) %>%
  spread(question, response) %>%
  select(id = response_set, date = date_time, type = `*Type of Communication`, transaction = `*Type of Transaction`, referral = `Referral to:`) %>%
  filter(as.Date(date) >= as.Date(input$dates[1]) & as.Date(date) <= as.Date(input$dates[2]))
})