# # #   Exercises - Page 90   # # #
# # 1
library(tidyverse)

ggplot(data = diamonds) +
  geom_histogram(mapping = aes(x = x), binwidth = 0.01) +
  coord_cartesian(xlim = c(0,10))

ggplot(data = diamonds) +
  geom_histogram(mapping = aes(x = y), binwidth = 0.01) +
  coord_cartesian(xlim = c(0,10))

ggplot(data = diamonds) +
  geom_histogram(mapping = aes(x = z), binwidth = 0.01) +
  coord_cartesian(xlim = c(0,10))

# # 2
ggplot(data = diamonds, mapping = aes(x = price)) +
  geom_histogram(binwidth = 1000)

ggplot(data = diamonds, mapping = aes(x = price)) +
  geom_histogram(binwidth = 100)

ggplot(data = diamonds, mapping = aes(x = price)) +
  geom_histogram(binwidth = 10) +
  coord_cartesian(xlim = c(1250, 1750)) # Lack of diamonds with price between 1450 and 1550

# # 3
nrow(filter(.data = diamonds, carat == 0.99))
nrow(filter(.data = diamonds, carat == 1))

# # 4
ggplot(data = mpg, mapping = aes(x = hwy)) +
  geom_histogram(binwidth = 2) +
  coord_cartesian(xlim = c(10,20), ylim = c(0,30))

ggplot(data = mpg, mapping = aes(x = hwy)) +
  geom_histogram(binwidth = 2) +
  xlim(c(10,20)) +
  ylim(c(0,30)) # Excludes values which fall outside off intervals

ggplot(data = mpg, mapping = aes(x = hwy)) +
  geom_histogram() +
  coord_cartesian(xlim = c(10,20), ylim = c(0,30)) # Binwidth is left unset

ggplot(data = mpg, mapping = aes(x = hwy)) +
  geom_histogram() +
  xlim(c(10,20)) +
  ylim(c(0,30)) # Binwidth is adjusted to axis scale



# # #   Exercises - Page 98   # # #
# # 1
ggplot(data = flights, mapping = aes(x = air_time)) +
  geom_histogram() # Missing values removed

flights %>%
  mutate(new = ifelse(test = is.na(air_time),
                      yes = NA, no = origin)) %>%
  select(new) -> some_data

ggplot(data = some_data, mapping = aes(x = new)) +
  geom_bar() +
  coord_flip() # Missing values are treated as another category

# # 2
wektor <- rep(x = c(1:5, NA), times = 5)
mean(x = wektor)
mean(x = wektor, na.rm = T)
sum(x = wektor)
sum(x = wektor, na.rm = T)



# # #   Exercises - Page 99   # # #
# # 1
nycflights13::flights %>%
  mutate(cancelled = is.na(dep_time),
         sched_hour = sched_dep_time %/% 100,
         sched_min = sched_dep_time %% 100,
         sched_dep_time = sched_hour + sched_min / 60) -> dane

ggplot(data = dane, mapping = aes(x = sched_dep_time, y = ..density..)) +
  geom_freqpoly(mapping = aes(col = cancelled))

ggplot(data = dane, mapping = aes(x = cancelled, y = sched_dep_time)) +
  geom_boxplot()

# # 2
glimpse(diamonds)

ggplot(data = diamonds, mapping = aes(x = carat, y = price)) +
  geom_point() +
  geom_smooth(se = F)

ggplot(data = diamonds, mapping = aes(x = cut, y = carat)) +
  geom_boxplot()

ggplot(data = diamonds, mapping = aes(x = carat, y = ..density.., col = cut)) +
  geom_freqpoly() +
  coord_cartesian(xlim = c(0,3)) # Higher values of carat lead to higher prices
                                 # Lower quality diamonds usually have higher carat values and therefore are more expensive

# # 3
library(ggstance)

ggplot(data = diamonds, mapping = aes(x = color, y = price)) +
  geom_boxplot()

ggplot(data = diamonds, mapping = aes(x = price, y = color)) +
  geom_boxplot()

ggplot(data = diamonds, mapping = aes(x = price, y = color)) +
  geom_boxploth()

# # 4
library(lvplot)

ggplot(data = diamonds, mapping = aes(x = cut, y = price)) +
  geom_lv()

# # 5
ggplot(data = diamonds, mapping = aes(x = price)) +
  geom_histogram(binwidth = 100) +
  facet_wrap(facets = ~ cut)

ggplot(data = diamonds, mapping = aes(x = price)) +
  geom_freqpoly(mapping = aes(col = cut))

ggplot(data = diamonds, mapping = aes(x = cut, y = price)) +
  geom_violin()

# # 6
library(ggbeeswarm)

ggplot(data = mpg, mapping = aes(x = class, y = hwy)) +
  geom_boxplot()

ggplot(data = mpg, mapping = aes(x = class, y = hwy)) +
  geom_jitter()

ggplot(data = mpg, mapping = aes(x = class, y = hwy)) +
  geom_beeswarm()

ggplot(data = mpg, mapping = aes(x = class, y = hwy)) +
  geom_quasirandom()



# # #   Exercises - Page 101   # # #
# # 1
diamonds %>%
  count(color, cut) %>%
  group_by(color) %>%
  mutate(color_total = sum(n),
         percent = n/color_total*100) %>%
  ggplot(mapping = aes(x = color, y = cut)) +
  geom_tile(mapping = aes(fill = percent))
  
# # 2
flights %>%
  filter(!is.na(arr_delay)) %>%
  group_by(dest, month) %>%
  summarise(avr_delay = mean(arr_delay)) %>%
  group_by(dest) %>%
  mutate(no_months = n()) %>%
  filter(no_months == 12) %>%
  mutate(avr_delay = ifelse(test = avr_delay < 0, yes = 0, no = avr_delay)) %>%
  ggplot(mapping = aes(x = factor(month), y = dest)) +
  geom_tile(mapping = aes(fill = avr_delay)) +
  labs(x = "Month", y = "Destination", fill = "Average delay")
  
# # 3
diamonds %>%
  count(color, cut) %>%
  group_by(color) %>%
  mutate(color_total = sum(n),
         percent = n/color_total*100) %>%
  ggplot(mapping = aes(x = cut, y = color)) +
  geom_tile(mapping = aes(fill = percent)) # Less intuitive to interpret proportions contained in rows than columns



# # #   Exercises - Page 104   # # #
# # 1
# Conditional distribution - boxplot
ggplot(data = diamonds, mapping = aes(x = carat, y = price)) +
  geom_boxplot(mapping = aes(group = cut_width(x = carat, width = 0.1)))

ggplot(data = diamonds, mapping = aes(x = carat, y = price)) +
  geom_boxplot(mapping = aes(group = cut_number(x = carat, n = 10)))

# Conditional distribution - frequency polygon
ggplot(data = diamonds, mapping = aes(x = price, y = ..density.., col = cut_width(x = carat, width = 1))) +
  geom_freqpoly() +
  labs(col = "Cut", x = "Price", y = "Kernel Density Estimation", title = "Plotted using 'cut_width'") +
  theme(plot.title = element_text(hjust = 0.5))

ggplot(data = diamonds, mapping = aes(x = price, y = ..density.., col = cut_number(x = carat, n = 5))) +
  geom_freqpoly() +
  labs(col = "Cut", x = "Price", y = "Kernel Density Estimation", title = "Plotted using 'cut_number'") +
  theme(plot.title = element_text(hjust = 0.5))

# # 2 & 3
ggplot(data = diamonds, mapping = aes(x = carat, y = ..density.., col = cut_number(x = price, n = 5))) +
  geom_freqpoly()

# # 4
# First possibility
library(hexbin)

dane4 <- diamonds %>%
  select(cut, carat, price) %>%
  mutate(carat = cut_width(x = carat, width = 0.5, boundary = 0)) %>%
  group_by(cut, carat) %>%
  summarise(mean_price = mean(price))

ggplot(data = dane4, mapping = aes(x = cut , y = carat)) +
  geom_bin2d(mapping = aes(fill = mean_price)) +
  labs(x = "Cut", y = "Carat", fill = "Mean price", title = "Tile plot") +
  theme(plot.title = element_text(hjust = 0.5))

# Second possibility
ggplot(data = diamonds, mapping = aes(x = cut, y = price)) +
  geom_violin(mapping = aes(fill = cut_number(x = carat, n = 3))) +
  labs(x = "Cut", y = "Price", fill = "Carat", title = "Violin plot") +
  theme(plot.title = element_text(hjust = 0.5))

# Third possibility
diamonds %>%
  ggplot(data = diamonds, mapping = aes(x = cut_number(x = carat, n = 5), y = price, col = cut)) +
  geom_boxplot() +
  labs(x = "Carat", y = "Price", col = "Cut", title = "Box plot") +
  theme(plot.title = element_text(hjust = 0.5))

# # 5
diamonds %>%
  mutate(arbitrary = T) %>%
  ggplot() +
  geom_boxplot(mapping = aes(x = arbitrary, y = x))

diamonds %>%
  mutate(arbitrary = T) %>%
  ggplot() +
  geom_boxplot(mapping = aes(x = arbitrary, y = y)) # Some insignificant outliers in variables themselves
                                                    # but there are more significant outliers in their correlation