library(nycflights13)
library(tidyverse)
View(airlines)
View(airports)
View(flights)
View(planes)
View(weather)

# # #   Exercises - Page 174   # # #
# # 1
# Variables - destination, origin, longitude, lattitude
# Tables - flights, airports

# # 2
# Datasets weather and airports are related throught variable origin

# # 3
# Datasets would also be related through destination variable

# # 4
special <- tibble(day_name = c("Valentine's Day", "New Year", "Christmas"),
                  month = c(2,1,12),
                  day = c(14,1,25))
# Primary key - day_name
# Related to flights and weather datasets through month and day variables



# # #   Exercises - Page 177   # # #
# # 1
mutate(.data = flights,
       ID = row_number()) %>%
  relocate(ID, everything())

# # 2
# a)
library(Lahman)
View(Batting)
Batting %>%
  count(playerID, yearID, stint) %>%
  filter(n > 1)

# b)
library(babynames)
View(babynames)
babynames %>%
  count(year, sex, name) %>%
  filter(n > 1)

# c)
library(nasaweather)
View(atmos)
atmos %>%
  count(lat ,long, year, month) %>%
  filter(n > 1)

# d)
library(fueleconomy)
View(vehicles)
vehicles %>%
  count(id) %>%
  filter(n > 1)

# e)
View(diamonds) # No primary key

# # 3
View(Batting)
View(Pitching)
View(Fielding)
# All tables are related througn playerID, yearID and stint variables



# # #   Exercises - Page 186   # # #
# # 1
library(maps)

flights %>%
  filter(!is.na(arr_delay)) %>%
  group_by(dest) %>%
  summarise(avr_delay = mean(arr_delay)) %>%
  mutate(avr_delay = ifelse(test = avr_delay < 0, yes = 0, no = avr_delay)) -> quest_1

left_join(x = quest_1, y = airports, by = c("dest" = "faa")) -> quest_1.2

ggplot(data = quest_1.2, mapping = aes(x = lon, y = lat)) +
  borders("state") +
  geom_point(mapping = aes(col = avr_delay)) +
  coord_quickmap() +
  labs(x = "Longitude", y = "Lattitude", col = "Average delay", title = "Average arrival delays to destinations") +
  theme(plot.title = element_text(hjust = 0.5))

# # 2
to_join <- select(.data = airports, faa, lat, lon)

flights %>%
  select(year:day, tailnum, flight, origin, dest) %>%
  left_join(y = to_join, by = c("origin" = "faa")) %>%
  left_join(y = to_join, by = c("dest" = "faa")) %>%
  rename(lat_origin = lat.x, lon_origin = lon.x, lat_dest = lat.y, lon_dest = lon.y)

# # 3
f_short <- select(.data = flights, tailnum, arr_delay) %>%
  filter(!is.na(arr_delay)) %>%
  group_by(tailnum) %>%
  summarise(avr_delay = mean(arr_delay))

left_join(x = f_short, y = planes, by = "tailnum") %>%
  group_by(year) %>%
  summarise(average_delay = avr_delay) %>%
  ggplot(mapping = aes(x = year, y = average_delay)) +
  geom_jitter(alpha = 0.2) +
  geom_smooth(se = F) # No relationship

# # 4
f_quest4 <- select(.data = flights, year, month, day, hour, arr_delay) %>%
  filter(!is.na(arr_delay)) %>%
  group_by(year, month, day, hour) %>%
  summarise(avr_delay = mean(arr_delay))

f_quest4 %>%
  left_join(y = weather) %>%
  select(year:avr_delay, temp, humid, wind_speed, precip, visib) -> to_check

ggplot(data = to_check, mapping = aes(x = temp, y = avr_delay)) +
  geom_jitter(alpha = 0.1) +
  geom_smooth(se = F, col = "black") +
  labs(x = "Temperature", y = "Average delay") # No relationship

filter(.data = to_check, humid < 100) %>%
  ggplot(mapping = aes(x = humid, y = avr_delay)) +
  geom_jitter(alpha = 0.1) +
  geom_smooth(se = F, col = "black") +
  labs(x = "Humidity [%]", y = "Average delay") # Slightly positive relationship

filter(.data = to_check, wind_speed > 0) %>%
  ggplot(mapping = aes(x = wind_speed, y = avr_delay)) +
  geom_jitter(alpha = 0.1) +
  geom_smooth(se = F, col = "black") +
  labs(x = "Wind speed", y = "Average delay") # Slightly positive relationship

ggplot(data = to_check, mapping = aes(x = precip, y = avr_delay)) +
  geom_jitter(alpha = 0.1) +
  geom_smooth(se = F, col = "black") +
  labs(x = "Precipitation", y = "Average delay") +
  coord_cartesian(xlim = c(0, 0.25)) # Slightly positive relationship

ggplot(data = to_check, mapping = aes(x = visib, y = avr_delay)) +
  geom_jitter(alpha = 0.1) +
  geom_smooth(se = F, col = "black") +
  labs(x = "Visibility", y = "Average delay") # Slightly negative relationship

# # 5
flights %>%
  filter(year == 2013, month == 6, day == 13) %>%
  group_by(dest) %>%
  summarise(delay = mean(arr_delay, na.rm = TRUE)) %>%
  inner_join(airports, by = c("dest" = "faa")) %>%
  ggplot(mapping = aes(y = lat, x = lon, size = delay, col = delay)) +
  borders("state") +
  geom_point() +
  coord_quickmap()



# # #   Exercises - Page 191   # # #
# # 1
filter(.data = flights, is.na(tailnum)) %>%
  filter(!is.na(air_time)) # All flights were cancelled

# # 2
count(x = flights, tailnum) %>%
  filter(n > 99) -> planes_100

semi_join(x = flights, y = planes_100, by = "tailnum") %>%
  filter(!is.na(tailnum))

# # 3
library(fueleconomy)
View(vehicles)
View(common)

semi_join(x = vehicles, y = common, by = "model")

# # 4
flights %>%
  filter(!is.na(air_time)) %>%
  group_by(year, month, day, hour) %>%
  summarise(avr_delay = mean(arr_delay)) %>%
  arrange(desc(avr_delay)) %>%
  distinct() %>%
  head(n = 48) -> worst_48h

flights %>%
  filter(!is.na(air_time)) %>%
  group_by(time_hour) %>%
  summarise(avr_delay = mean(dep_delay)) %>%
  arrange(desc(avr_delay)) %>%
  distinct() %>%
  head(n = 48) -> worst_48h

left_join(x = worst_48h, y = weather, by = "time_hour") -> worst_weather
  
cor(worst_weather$temp, worst_weather$avr_delay) # -.37
cor(worst_weather$humid, worst_weather$avr_delay) # -.06
cor(worst_weather$wind_speed, worst_weather$avr_delay) # .07
cor(worst_weather$visib, worst_weather$avr_delay) # .09
cor(worst_weather$precip, worst_weather$avr_delay) # .05

# # 5
anti_join(x = flights, y = airports, by = c("dest" = "faa")) # Info about all flights with destinations not present in airports dataset
anti_join(x = airports, y = flights, by = c("faa" = "dest")) # Info about all airports which are not destinations in flights dataset

# # 6
distinct(.data = flights, tailnum) -> distinct_planes

flights %>%
  select(carrier, tailnum) %>%
  distinct() %>%
  count(carrier) -> pl_in_air # Number of planes in each airline
                              # This code allows for repetition of planes in different airlines

# To confirm hypothesis, number of distinct planes would have to match the sum of planes in all airlines
# If this is true, then each plane is flown by single airline

sum(pl_in_air$n) == nrow(distinct_planes) # FALSE ... 4067 != 4044