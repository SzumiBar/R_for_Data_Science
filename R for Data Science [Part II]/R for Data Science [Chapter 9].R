# # #   Exercises - Page 151   # # #
# # 2
# a)
library(tidyverse) # tidyr
count(x = table1, country, year, wt = cases) -> TB_cases

# b)
count(x = table1, country, year,  wt = population) -> TB_population

# c)
rate <- (TB_cases$n / TB_population$n)*10000
names(rate) <- c("Afg99", "Afg00", "Bra99", "Bra00", "Chi99", "Chi00")

# d)
table2$rate <- rep(x = rate, each = 2)

table4a$rate_1999 <- rate[c(1,3,5)]
table4a$rate_2000 <- rate[c(2,4,6)]

table4b$rate_1999 <- rate[c(1,3,5)]
table4b$rate_2000 <- rate[c(2,4,6)]

# # 3
table2[table2$year == 1999, ]
table2[table2$year == 2000, ]

table2 %>%
  filter(type == "cases") %>%
  ggplot(mapping = aes(x = year, y = count, col = country)) +
  geom_point() +
  geom_line(mapping = aes(group = country), col = "grey")



# # #   Exercises - Page 156   # # #
# # 1
stocks <- tibble(year = c(2015, 2015, 2016, 2016),
                 half = c( 1, 2, 1, 2),
                 return = c(1.88, 0.59, 0.92, 0.17))
stocks %>%
  spread(key = "year", value = return) %>%
  gather(key = year, value = "return", `2015`:`2016`) # Variables and values are gathered and spreaded on the right side of tibble

stocks %>%
  pivot_wider(names_from  = "year", values_from = "return") %>%
  pivot_longer(cols = `2015`:`2016`, names_to = "Year", values_to = "Return", )

# # 2
table4a %>%
  gather(1999, 2000, key = "year", value = "cases") # Column names are non-syntactic variable names - it is necessary to use backticks

table4a %>%
  gather(`1999`, `2000`, key = "year", value = "cases")

table4a %>%
  gather(`1999`:`2000`, key = "year", value = "cases")

table4a %>% 
  pivot_longer(cols = c(1999, 2000), names_to = "year", values_to = "cases")

table4a %>% 
  pivot_longer(cols = c(`1999`, `2000`), names_to = "year", values_to = "cases")

table4a %>% 
  pivot_longer(cols = `1999`:`2000` , names_to = "year", values_to = "cases")

# # 3
people <- tribble(~name, ~key, ~value,
                  "Phillip Woods", "age", 45,
                  "Phillip Woods", "height", 186,
                  "Phillip Woods", "age", 50,
                  "Jessica Cordero", "age", 37,
                  "Jessica Cordero", "height", 156)
people$ID <- c(1,1,2,3,3)

spread(data = people, key = "key", value = "value")
pivot_wider(data = people, names_from = "key", values_from = "value")

# # 4
preg <- tribble(~pregnant, ~male, ~female,
                "yes", NA, 10,
                "no", 20, 12)

gather(data = preg, "male":"female", key = "sex", value = "count")
pivot_longer(data = preg, cols = "male":"female", names_to = "sex", values_to = "count")



# # #   Exercises - Page 160   # # #
# # 1
tb_1 <- tibble(x1 = c("a,b,c", "d,e,f,g", "h,i,j"))
tb_2 <- tibble(x2 = c("a,b,c", "d,e", "f,g,i"))

separate(data = tb_1, col = x1, into = c("one", "two", "three")) # Additional values discarded
separate(data = tb_2, col = x2, into = c("one", "two", "three")) # Missing values filled with NA

tb_1 %>%
  separate(col = x1, into = c("one", "two", "three"), extra = "warn") # Displays warning - default option
tb_1 %>%
  separate(col = x1, into = c("one", "two", "three"), extra = "drop") # No warning
tb_1 %>%
  separate(col = x1, into = c("one", "two", "three"), extra = "merge") # Includes all remaining values in last entry

tb_2 %>%
  separate(col = x2, into = c("one", "two", "three"), fill = "warn") # Displays warning
tb_2 %>%
  separate(col = x2, into = c("one", "two", "three"), fill = "left") # Fills missing values with NA on left
tb_2 %>%
  separate(col = x2, into = c("one", "two", "three"), fill = "right") # Fills missing values with NA on right - default option

# # 2
table5 %>%
  separate(col = rate, sep = "/", into = c("cases", "population"), remove = TRUE) # Default
table5 %>%
  separate(col = rate, sep = "/", into = c("cases", "population"), remove = FALSE) # Does not remove input column

table5 %>%
  unite(col = "date", "century", "year", sep = "", remove = TRUE) # Default
table5 %>%
  unite(col = "date", "century", "year", sep = "", remove = FALSE) # Does not remove input columns

# # 3
tibble(x = c("X_1", "X_2", "AA_1", "AA_2")) %>%
  separate(x, c("Zmienna", "Wartosc"), sep = "_")
tibble(x = c("XY11", "XZ21", "YZ12", "YZ223")) %>%
  separate(x, c("Zmienna", "Wartosc"), sep = 2)

tibble(x = c("X_1", "X_2", "AA_1", "AA_2")) %>%
  extract(x, c("Zmienna", "Wartosc"), regex = "([A-Z])_([0-9])")
tibble(x = c("X_1", "X_2", "AA_1", "AA_2")) %>%
  extract(x, c("Zmienna", "Wartosc"), regex = "([A-Z]+)_([0-9])")
tibble(x = c("XY11", "XZ21", "YZ12", "YZ223")) %>%
  extract(x, c("Zmienna", "Wartosc"), regex = "([A-Z]+)([0-9])")
tibble(x = c("XY11", "XZ21", "YZ12", "YZ223")) %>%
  extract(x, c("Zmienna", "Wartosc"), regex = "([A-Z])([0-9]+)")



# # #   Exercises - Page 163   # # #
# # 1
stocks <- tibble(year = c(2015, 2015, 2015, 2015, 2016, 2016, 2016),
                 qtr = c( 1, 2, 3, 4, 2, 3, 4),
                 return = c(1.88, 0.59, 0.35, NA, 0.92, 0.17, 2.66))

fill(data = stocks, return, .direction = "up")
complete(data = stocks, year, qtr, fill = list(return = "Filled"))

spread(data = stocks, key = year, value = return, fill = "Missing")
pivot_wider(data = stocks, names_from = year, values_from = return, values_fill = 1)

# # 2
fill(data = stocks, return, .direction = "down")
fill(data = stocks, return, .direction = "up")
fill(data = stocks, return, .direction = "downup")
fill(data = stocks, return, .direction = "updown")



# # #   Exercises - Page 168   # # #
# # 1
who1 <- who %>% 
  pivot_longer(cols = new_sp_m014:newrel_f65, 
               names_to = "key", 
               values_to = "cases", 
               values_drop_na = TRUE)
filter(.data = who1, cases == 0) # NA values represent missing data

# # 2
who %>%
  gather(code, value, new_sp_m014:newrel_f65, na.rm = TRUE) %>%
  # mutate(code = stringr::str_replace(code, "newrel", "new_rel")) %>%
  separate(code, c("new", "var", "sexage")) %>%
  select(-new, -iso2, -iso3) %>%
  separate(sexage, c("sex", "age"), sep = 1)

# # 3
who %>%
  select(country, iso2, iso3) %>%
  distinct() %>%
  group_by(country) %>%
  mutate(n = n()) %>%
  filter(n > 1)

# # 4
who %>%
  gather(code, value, new_sp_m014:newrel_f65, na.rm = TRUE) %>%
  mutate(code = stringr::str_replace(code, "newrel", "new_rel")) %>%
  separate(code, c("new", "var", "sexage")) %>%
  select(-new, -iso2, -iso3) %>%
  separate(sexage, c("sex", "age"), sep = 1) -> rdy_who

rdy_who %>%
  select(country, year, sex, value) %>%
  count(country, year, sex, wt = value) -> quest_4

ggplot(data = quest_4) +
  geom_jitter(aes(x = year, y = n, col = sex), alpha = 0.3, width = 0.2, length = 0.2) +
  labs(col = "Sex", x = "Year", y = "Cases")