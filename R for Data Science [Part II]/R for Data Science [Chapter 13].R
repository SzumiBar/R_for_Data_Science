library(tidyverse)
library(lubridate)
library(nycflights13)

# # #   Exercises - Page 243   # # #
# # 1
ymd(c("2010-10-10", "bananas"))

# # 2
today(tzone = "UTC") # Universal time zone

# # 3
d1 <- "January 1, 2010"
d2 <- "2015-Mar-07"
d3 <- "06-Jun-2017"
d4 <- c("August 19 (2015)", "July 1 (2015)")
d5 <- "12/30/14"

date1 <- mdy(d1)
date2 <- ymd(d2)
date3 <- dmy(d3)
date4 <- mdy(d4)
date5 <- mdy(d5)



# # #   Exercises - Page 248   # # #
# # 1
flights %>%
  filter(!is.na(air_time)) %>%
  mutate(deptime_dt = make_datetime(year = year,
                                    month = month,
                                    day = day,
                                    hour = dep_time%/%100,
                                    min = dep_time%%100)) %>%
  relocate(dep_time, deptime_dt) %>%
  mutate(deptime_dt = update(deptime_dt, mday = 1)) %>%
  select(month, deptime_dt) %>%
  mutate(deptime_dt = round_date(x = deptime_dt, unit = "hour")) %>%
  count(month, deptime_dt) %>%
  ggplot(mapping = aes(x = hour(deptime_dt), y = n, col = factor(month))) +
  geom_line() +
  labs(x = "Departure time", y = "Count", col = "Month",
       title = "Distribution of flight times within a day\nover the course of the year") +
  theme(plot.title = element_text(hjust = 0.5))

# # 2
flights %>%
  filter(!is.na(air_time)) %>%
  mutate(deptime_dt = make_datetime(year = year,
                                    month = month,
                                    day = day,
                                    hour = dep_time%/%100,
                                    min = dep_time%%100),
         scheddt_dt = make_datetime(year = year,
                                    month = month,
                                    day = day,
                                    hour = sched_dep_time%/%100,
                                    min = sched_dep_time%%100)) %>%
  mutate(difference = as.double(deptime_dt - scheddt_dt),
         dep_del = dep_delay*60) %>%
  select(difference, dep_del, deptime_dt, scheddt_dt) %>%
  filter(difference == dep_del) # Difference matches departure delay as it should

# # 3
flights %>%
  filter(!is.na(air_time)) %>%
  mutate(deptime_dt = make_datetime(year = year,
                                    month = month,
                                    day = day,
                                    hour = dep_time%/%100,
                                    min = dep_time%%100),
         arrtime_dt = make_datetime(year = year,
                                    month = month,
                                    day = day,
                                    hour = arr_time%/%100,
                                    min = arr_time%%100)) %>%
  mutate(difference = as.double(arrtime_dt - deptime_dt)) %>%
  select(difference, air_time, deptime_dt, arrtime_dt) %>%
  filter(difference == air_time) # Difference does not match air time as it should

# # 4
flights %>%
  filter(!is.na(air_time)) %>%
  mutate(sched_dt = make_datetime(year = year,
                                  month = month,
                                  day = day,
                                  hour = sched_dep_time%/%100,
                                  min = sched_dep_time%%100)) %>%
  select(sched_dt, dep_delay) %>%
  mutate(sched_dt = update(sched_dt, mday = 1, month = 1)) %>%
  group_by(sched_dt) %>%
  summarise(avr_delay = mean(dep_delay)) %>%
  ggplot(mapping = aes(sched_dt, avr_delay)) +
  geom_line() +
  geom_smooth(col = "black") +
  labs(x = "Scheduled departure time", y = "Average departure delay",
       title = "Average departure delay over the course of the day") +
  theme(plot.title = element_text(hjust = 0.5))

# # 5
flights %>%
  filter(!is.na(air_time)) %>%
  mutate(week_day = wday(time_hour, label = T, abbr = F)) %>%
  group_by(week_day) %>%
  summarise(avr_arrdel = mean(arr_delay),
            avr_depdel = mean(dep_delay)) # To minimize chance of departure delay as well as
                                          # arrival delay, leave on saturday

# # 6
ggplot(data = diamonds, mapping = aes(x = carat)) +
  geom_histogram(binwidth = 0.01)

flights %>%
  filter(!is.na(sched_dep_time)) %>%
  ggplot(mapping = aes(x = sched_dep_time)) +
  geom_histogram(binwidth = 10) # Both distributions are multimodal

# # 7
flights %>%
  filter(!is.na(air_time)) %>%
  mutate(deptime_dt = make_datetime(dep_time,
                                    year = year,
                                    month = month,
                                    day = day,
                                    hour = dep_time%/%100,
                                    min = dep_time%%100),
         early = dep_delay < 0,
         mins = minute(deptime_dt)) %>%
  group_by(mins) %>%
  summarise(early_prop = mean(early, na.rm = T)) %>%
  ggplot(mapping = aes(x = mins, y = early_prop)) +
  geom_line() +
  labs(x = "Minute of departure time", y = "Proportion of earlier departures")
  


# # #   Exercises - Page 253   # # #
# # 1
months(1)
dmonths(1) == seconds(months(1))

# # 2
# In days(overnight * 1) it is not even necessary to multiply by 1 because
# overnight is logical and if equal to T one day will be added, otherwise not

# # 3
wektor_1a <- years(2015) + months(1:12) + days(1)
wektor_1b <- ymd(20150101) + months(0:11)
wday(x = wektor_1b, label = T, abbr = F)

wektor_2 <- floor_date(x = today(), unit = "year") + months(0:11)
wday(x = wektor_2, label = T, abbr = F)

# # 4
return_age <- function (bday) {
  if (!is.Date(bday)) {
    errorCondition(message = "argument 'bday' has to be date")
  } else {
    age <- year(today()) - year(bday)
    return(paste("Your age is", age, sep = " : "))
  }
}
return_age(bday = ymd(19990420))
return_age(bday = ymd_hms(19990420043030))

# # 5
(today() %--% (today() + years(1)) / months(1)) # Code was lacking last bracket