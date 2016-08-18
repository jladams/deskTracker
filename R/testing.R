df <- raw %>%
  select(response_set,date_time,question,response) %>%
  spread(question, response) %>%
  select(id = response_set, date = date_time, mode = `*Type of Communication`, type = `*Type of Transaction`, referral = `Referral to:`) %>%
  mutate(value = 1)

test <- df %>%
  group_by(mode, day = date(date), weekday = wday(date, label = TRUE), hour = hour(date)) %>%
  summarize(value = sum(value)) %>%
  group_by(mode, weekday, hour) %>%
  summarize(value = mean(value))


df_mode <- reactive({
  df <- df() %>%
    group_by(mode, day = as.Date(date), weekday = wday(date, label = TRUE), hour = hour(date)) %>%
    summarize(value = sum(value)) %>%
    group_by(mode, weekday, hour) %>%
    summarize(max = max(value), mean = mean(value), median = median(value), min = min(value), sum = sum(value))
})
