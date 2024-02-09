# # #   Exercises - Page 49   # # #
# # 1
# a)
library(nycflights13)
library(tidyverse)

filter(.data = flights, arr_delay > 120)

# b)
filter(.data = flights, dest == "IAH" | dest == "HOU")
filter(.data = flights, dest %in% c("IAH", "HOU"))

# c)
filter(.data = flights, carrier == "DL" | carrier == "AA" | carrier == "UA")
filter(.data = flights, carrier %in% c("DL", "AA", "UA"))

# d)
filter(.data = flights, month == 6 | month == 7 | month == 8)
filter(.data = flights, month %in% c(7:9))

# e)
filter(.data = flights, arr_delay > 120 & dep_delay <= 0)

# f)
filter(.data = flights, dep_delay >= 60 & (dep_delay - arr_delay) > 30)

# g)
filter(.data = flights, dep_time <= 600 | dep_time == 2400)

# # 2
filter(.data = flights, between(x = month, left = 7, right = 9))

# # 3
nrow(filter(.data = flights, is.na(dep_time)))

# # 4
NA ^ 0 == 1
NA | TRUE == TRUE
NA | FALSE == FALSE
is.na(NA * 0)



# # #   Exercises - Page 51   # # #
# # 1
arrange(.data = flights, desc(is.na(dep_time)))

# # 2
head(x = arrange(.data = flights, desc(dep_delay)), n = 10)
head(x = arrange(.data = flights, dep_delay), n = 10)

# # 3
head(x = arrange(.data = flights, desc(distance/air_time)), n = 10)

# # 4
head(x = arrange(.data = flights, desc(distance)), n = 10)
head(x = arrange(.data = flights, distance), n = 10)



# # #   Exercises - Page 54   # # #
# # 1
select(.data = flights, dep_time, dep_delay, arr_time, arr_delay)
select(.data = flights, "dep_time", "dep_delay", "arr_time", "arr_delay")
select(.data = flights, 4, 6, 7, 9)
select(.data = flights, (starts_with("dep") | starts_with("arr")) & (ends_with("time") | ends_with("delay")))

# # 2
select(.data = flights, dep_time, dep_time ,dep_time)

# # 3
one_of_vars <- c("year", "month", "day", "dep_delay", "arr_delay", "additional variable")
select(.data = flights, one_of(one_of_vars))

all_of_vars <- c("year", "month", "day", "additional variable")
select(.data = flights, all_of(all_of_vars))

# # 4
select(flights, contains(match = "TIME"))

select(flights, contains(match = "TIME", ignore.case = FALSE))



# # #   Exercises - Page 58   # # #
# # 1
hours_dt <- flights$dep_time %/% 100
hours_dt[which(hours_dt == 24)] <- 0
remain_min_dt <- flights$dep_time %% 100
range(hours_dt, na.rm = T)

hours_sdt <- flights$sched_dep_time %/% 100
remain_min_sdt <- flights$sched_dep_time %% 100
range(hours_sdt, na.rm = T)

mutate(.data = flights,
       dep_time_2 = (hours_dt*60) + remain_min_dt,
       sched_dep_time_2 = (hours_sdt*60) + remain_min_sdt) -> flights

select(.data = flights, dep_time, dep_time_2, sched_dep_time, sched_dep_time_2)

# # 2
wrong_diff <- flights$arr_time - flights$dep_time
head(x = cbind(flights$air_time, wrong_diff), n = 5)

hours_at <- flights$arr_time %/% 100
remain_min_at <- flights$arr_time %% 100

mutate(.data = flights,
       arr_time_2 = (hours_at*60) + remain_min_at) -> flights

should.be_proper.diff <- flights$arr_time_2 - flights$dep_time_2
head(cbind(flights$air_time, should.be_proper.diff), n = 5)

# # 3
flights$dep_delay == flights$dep_time - flights$sched_dep_time

# # 4
ranked <- min_rank(flights$dep_delay)

flights <- mutate(.data = flights,
                  rank = ranked)

arrange(.data = flights, desc(rank))

# # 5
1:3 + 1:10 == c(1+1, 2+2, 3+3, 1+4, 2+5, 3+6, 1+7, 2+8, 3+9, 1+10)

# # 6
cos(pi)
sin(pi/2)
tan(pi/4)

acos(-1)
asin(1)
atan()



# # #   Exercises - Page 73   # # #
# # 1
flights %>%
  filter(!is.na(air_time)) %>%
  group_by(tailnum) %>%
  summarise(arr_median = median(arr_delay)) %>%
  filter(arr_median >= 15) # Flights at least 15 min early 50% of time

flights %>%
  filter(!is.na(air_time)) %>%
  group_by(tailnum) %>%
  summarise(arr_median = median(arr_delay)) %>%
  filter(arr_median <= -15) # Flights at least 15 min late 50% of the time

flights %>%
  filter(!is.na(air_time)) %>%
  group_by(tailnum) %>%
  summarise(minimal_delay = min(arr_delay)) %>%
  filter(minimal_delay >= 10) # Flights which are always 10 minutes late

flights %>%
  filter(!is.na(air_time)) %>%
  group_by(tailnum) %>%
  summarise(arr_median = median(arr_delay)) %>%
  filter(arr_median >= 30) # Flights at least 30 min early 50% of time

flights %>%
  filter(!is.na(air_time)) %>%
  group_by(tailnum) %>%
  summarise(arr_median = median(arr_delay)) %>%
  filter(arr_median <= -30) # Flights at least 30 min late 50% of the time

flights %>%
  filter(!is.na(air_time)) %>%
  group_by(tailnum) %>%
  summarise(prop_on_time = mean(arr_delay <= 0)) %>%
  filter(prop_on_time >= 0.99) # Flights which are on time at least 99% of time

flights %>%
  filter(!is.na(air_time)) %>%
  group_by(tailnum) %>%
  summarise(prop_2h_late = mean(arr_delay >= 120)) %>%
  filter(prop_2h_late >= 0.01) # Flights which are at least 2 hours late at least 1% of time

# # 2
not_cancelled <- flights %>%
  filter(!is.na(dep_delay), !is.na(arr_delay))

not_cancelled %>%
  count(dest)

not_cancelled %>%
  count(tailnum, wt = distance)

not_cancelled %>%
  group_by(dest) %>%
  summarise(n = n())

not_cancelled %>%
  group_by(tailnum) %>%
  summarise(n = sum(distance))

# # 3
not_cancelled <- flights %>%
  filter(!is.na(air_time))

# # 4
cancelled <- flights %>%
  group_by(year, month, day) %>%
  summarise(no_cancelled = sum(is.na(air_time)))

ggplot(data = cancelled, mapping = aes(x = day, y = no_cancelled)) +
  geom_point() +
  geom_smooth(se = F) +
  facet_wrap(facets = ~ month, ncol = 4) +
  labs(x = "Day", y = "Number of cancelled flights")

# # 5
not_cancelled %>%
  group_by(carrier) %>%
  summarise(worst_delay = max(arr_delay)) %>%
  arrange(desc(worst_delay))

# # 6
not_cancelled %>%
  group_by(tailnum) %>%
  summarise(be4_1st_delay = sum(cumsum(arr_delay > 60) < 1))

# # 7
not_cancelled %>%
  count(tailnum, wt = distance)

not_cancelled %>%
  count(tailnum, wt = distance, sort = TRUE)



# # #   Exercises - Page 75   # # #
# # 1
# Mutate - arithmetics [No effect]
not_cancelled %>%
  mutate(new = log(year)) %>%
  relocate(new)

not_cancelled %>%
  group_by(tailnum) %>%
  mutate(new = log(year)) %>%
  relocate(new)

# Mutate - modular arithmetics [No effect]
not_cancelled %>%
  mutate(new = year %/% 500) %>%
  relocate(new)

not_cancelled %>%
  group_by(tailnum) %>%
  mutate(new = year %/% 500) %>%
  relocate(new)

# Mutate - offsets [Operations within groups]
not_cancelled %>%
  mutate(new = lead(dep_delay)) %>%
  relocate(new, dep_delay)

not_cancelled %>%
  group_by(carrier) %>%
  mutate(new = lead(dep_delay)) %>%
  relocate(new, dep_delay)

# Mutate - cumulative aggregates [Operations within groups]
not_cancelled %>%
  mutate(new = cumsum(month)) %>%
  relocate(new)

not_cancelled %>%
  group_by(month) %>%
  mutate(new = cumsum(month)) %>%
  relocate(new) %>%
  arrange(desc(month))

# Mutate - comparisons [No effect]
not_cancelled %>%
  mutate(new = month >= 6) %>%
  relocate(new)

not_cancelled %>%
  group_by(month) %>%
  mutate(new = month >= 6) %>%
  relocate(new)

# Mutate - summaries [Operations within groups]
not_cancelled %>%
  mutate(new = sd(dep_delay)) %>%
  relocate(new)

not_cancelled %>%
  group_by(carrier) %>%
  mutate(new = sd(dep_delay)) %>%
  relocate(new)

# Mutate - ranking [Operations within groups]
not_cancelled %>%
  mutate(new = dense_rank(air_time)) %>%
  relocate(new, air_time)

not_cancelled %>%
  group_by(carrier) %>%
  mutate(new = dense_rank(air_time)) %>%
  relocate(new, air_time)

# Filter - logicals [No effect]
not_cancelled %>%
  filter(dep_time == 1159 | dep_time == 1259)

not_cancelled %>%
  group_by(carrier) %>%
  filter(dep_time == 1159 | dep_time == 1259)

# Filter - comparisons [No effect]
not_cancelled %>%
  filter(dep_time > 1159)

not_cancelled %>%
  group_by(carrier) %>%
  filter(dep_time > 1159)

# Filter - missing values [No effect]
flights %>%
  filter(is.na(air_time))

flights %>%
  group_by(carrier) %>%
  filter(is.na(air_time))

# # 2
not_cancelled %>%
  group_by(tailnum) %>%
  summarise(count = n(),
            mean_delay = mean(arr_delay)) %>%
  filter(count > 30) %>%
  arrange(desc(mean_delay))

# # 3
not_cancelled %>%
  group_by(dep_time) %>%
  summarise(mean_delay = mean(arr_delay)) -> dane

ggplot(data = dane, mapping = aes(x = dep_time, y = mean_delay)) +
  geom_point(alpha = 0.25) +
  geom_smooth(se = F)

# # 4
not_cancelled %>%
  filter(arr_delay >= 0) %>%
  group_by(dest) %>%
  mutate(delay_total = sum(arr_delay)) %>%
  select(flight, dest, arr_delay, delay_total) %>%
  mutate(delay_prop = arr_delay/delay_total) %>%
  group_by(flight, dest) %>%
  summarise(delay_proportion = sum(delay_prop)) %>%
  arrange(dest) %>%
  transmute(flight, dest,
            delay_proportion = paste(round(x = (delay_proportion)*100, digits = 2), "%", sep = "")) 

# # 5
not_cancelled %>%
  select(origin, month, day, hour, dep_delay) %>%
  arrange(origin, month, day, hour, dep_delay) %>%
  group_by(origin, month, day, hour) %>%
  summarise(mean_delay = mean(dep_delay)) %>%
  mutate(preceeding_delay = lag(mean_delay)) %>%
  filter(preceeding_delay > 0) %>%
  ggplot(aes(x = preceeding_delay, y = mean_delay)) +
  geom_point(alpha = 0.1) +
  geom_smooth(method = "lm") +
  labs(x = "Departure delay of\npreceedding hour [min]", y = "Mean delay during specific hour [min]")

# # 6
not_cancelled %>%
  group_by(dest) %>%
  mutate(shortest_dest = min(air_time)) %>%
  select(dest, flight, air_time, shortest_dest) %>%
  group_by(dest, flight) %>%
  mutate(relative_at = air_time/shortest_dest) %>%
  arrange(air_time) # Suspiciously fast
  
not_cancelled %>%
  group_by(dest) %>%
  mutate(shortest_dest = min(air_time)) %>%
  select(dest, flight, air_time, shortest_dest) %>%
  group_by(dest, flight) %>%
  mutate(relative_at = air_time/shortest_dest) %>%
  arrange(desc(relative_at)) # Relatively most delayed

not_cancelled %>%
  group_by(dest) %>%
  mutate(shortest_dest = min(air_time)) %>%
  select(dest, flight, air_time, shortest_dest) %>%
  group_by(dest, flight) %>%
  mutate(relative_at = air_time/shortest_dest) %>%
  transmute(dest, flight,
            raw_air_delay = air_time - shortest_dest) %>%
  arrange(desc(raw_air_delay)) # Most delayed - raw values

# # 7
not_cancelled %>%
  select(dest, carrier) %>%
  count(dest, carrier) %>%
  transmute(dest, carrier) %>%
  arrange(dest, carrier) %>%
  group_by(dest) %>%
  mutate(no_carriers = n()) %>%
  filter(no_carriers >= 2) %>%
  count(dest, wt = mean(no_carriers)) # All destinations that are flown by at least two carriers

not_cancelled %>%
  select(dest, carrier) %>%
  count(dest, carrier) %>%
  transmute(dest, carrier) %>%
  arrange(dest, carrier) %>%
  group_by(carrier) %>%
  mutate(no_destinations = n()) %>%
  count(carrier, wt = mean(no_destinations)) %>%
  arrange(desc(n)) # Ranking of carriers with most destinations