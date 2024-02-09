library(tidyverse)
library(modelr)
options(na.action = na.warn)
library(nycflights13)
library(lubridate)

# # #   Exercises - Page 384   # # #
# # 1
# They represent the most common diamond sizes

# # 2
# log(price) = a_0 + a_1 * log(carat)
# As carat increases e [2.17] times, price will increase by e^(a_1) [2.17^(a_1)]
# As carat increases 2*e [4.34] times, price will increase by e^(2*a_1) [2.17^(2*(a_1))]



# # #   Exercises - Page 395   # # #
# # 1
# These are the Sundays before
# Monday holidays Martin Luther King Jr. Day, Memorial Day, and Labor Day

# # 2
# The top three days correspond to the Saturday after Thanksgiving (November 30th),
# the Sunday after Thanksgiving (December 1st), and the Saturday after Christmas (December 28th)

# # 3
daily <- flights %>%
  mutate(date = make_date(year, month, day)) %>%
  group_by(date) %>%
  summarize(n = n()) %>%
  mutate(wday = wday(date, label = TRUE, locale = "english"))

term <- function(date) {
  cut(date,
      breaks = ymd(20130101, 20130605, 20130825, 20140101),
      labels = c("spring", "summer", "fall")
  )
}

daily <- daily %>%
  mutate(term = term(date))

output <- vector(mode = "character", length = nrow(daily))
for (i in 1:nrow(daily)) {
  if (daily[i,3] == "Sat") {
    output[[i]] <- str_c("Sat", daily$term[i], sep = "-")
  } else {
    output[[i]] <- as.character(daily$wday[i])
  }
}

daily <- daily %>%
  mutate(newsplit = output)

mod1 <- lm(n ~ wday*term, data = daily)
mod2 <- lm(n ~ newsplit, data = daily)
summary(mod1) # R^2 = 0.74
summary(mod2) # R^2 = 0.73

# # 4
holidays_2013 <-
  tribble(
    ~holiday, ~date,
    "New Year's Day", 20130101,
    "Martin Luther King Jr. Day", 20130121,
    "Washington's Birthday", 20130218,
    "Memorial Day", 20130527,
    "Independence Day", 20130704,
    "Labor Day", 20130902,
    "Columbus Day", 20131028,
    "Veteran's Day", 20131111,
    "Thanksgiving", 20131128,
    "Christmas", 20131225
  ) %>%
  mutate(date = lubridate::ymd(date))

output <- vector(mode = "character")
for (i in 1:nrow(daily)) {
  if (any(daily$date[i] == holidays_2013$date)) {
    output[[i]] <- "Holidays"
  } else {
    output[[i]] <- daily$newsplit[daily$date == daily$date[i]]
  }
}

daily <- daily %>%
  mutate(newsplit2 = output)

mod4 <- lm(formula = n ~ newsplit2, data = daily)
summary(mod4) # R^2 = 0.77

quest4 <- gather_residuals(data = daily, mod1, mod2, mod4)
quest4 %>%
  group_by(model) %>%
  summarise(resid_sum = sum(abs(resid)))

# # 5
# Too many non-significant parameters as result of lack of statistical power

# # 6
# Doesn't include interaction, which occur

# # 7
flights %>%
  mutate(day_abr = wday(flights$time_hour, label = T, abbr = T, locale = "english")) %>%
  ggplot(mapping = aes(x = day_abr)) +
  geom_boxplot(mapping = aes(y = distance))
  
flights %>%
  mutate(day_abr = wday(flights$time_hour, label = T, abbr = T, locale = "english")) %>%
  ggplot(mapping = aes(x = day_abr)) +
  geom_boxplot(mapping = aes(y = air_time))

# # 8
flights %>%
  mutate(day_abr = factor(wday(flights$time_hour, label = T, abbr = T, locale = "english"),
         levels = c("Mon", "Tue", "Wed", "Thu", "Fri", "Sat", "Sun"))) %>%
  ggplot(mapping = aes(x = day_abr)) +
  geom_boxplot(mapping = aes(y = distance))