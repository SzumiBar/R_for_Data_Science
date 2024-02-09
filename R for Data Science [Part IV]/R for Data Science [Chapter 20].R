library(modelr)
library(tidyverse)
library(gapminder)
library(ggbeeswarm)

# # #   Exercises - Page 409   # # #
# # 1
lmodel <- lm(formula = lifeExp ~ year, data = gapminder)
summary(lmodel)

pmodel <- lm(formula = lifeExp ~ year + I(year^2), data = gapminder)
summary(pmodel) # year is coefficient for linear term
                # I(year^2) is coefficient for quadratic term

pmodel_f <- function(df) {
  lm(formula = lifeExp ~ year + I(year^2), data = df)
}

quest1 <- gapminder %>%
  group_by(country, continent) %>%
  nest() %>%
  mutate(model = map(data, .f = pmodel_f),
         pred = map2(.x = data, .y = model, .f = add_predictions),
         resid = map2(.x = data, .y = model, .f = add_residuals))

output <- vector(mode = "numeric")
for(i in 1:nrow(quest1)) {
  output <- rbind(output, quest1$resid[i][[1]])
}

quest1 %>%
  select(country, continent, data) %>%
  unnest() %>%
  left_join(y = output) %>%
  ggplot(mapping = aes(x = year, y = resid, group = country)) +
  geom_line(alpha = 0.2) +
  geom_ref_line(h = 0, size = 1)

# # 2
output2 <- vector(mode = "numeric")
for(i in 1:nrow(quest1)) {
  out <- quest1$model[[i]]
  output2[[i]] <- summary(out)$adj.r.squared
}

quest1 %>%
  cbind(output2) %>%
  select(country, continent, `...7`) %>%
  rename("r.squared" = `...7`) %>%
  ggplot(mapping = aes(x = continent, y = r.squared)) +
  geom_beeswarm()

# # 3
gapminder %>%
  group_by(country, continent) %>%
  nest() %>%
  mutate(model = map(data, ~lm(lifeExp ~ year, .))) %>%
  mutate(glance = map(model, broom::glance)) %>%
  unnest(glance) %>%
  unnest(data) %>%
  filter(r.squared < 0.25) %>%
  ggplot(aes(year, lifeExp)) +
  geom_line(aes(color = country))



# # #   Exercises - Page 415   # # #
# # 1
x <- 1:5
as.list(x)
lapply(X = x, FUN = cumsum)
map(.x = x, .f = sum)

# # 2
cumprod(x)
range(x)
fivenum(x)

# # 3
mtcars %>%
  group_by(cyl) %>%
  summarize(q = list(quantile(mpg))) %>%
  unnest() # quantile cut points are missing

quantile(mpg$cyl) # quantile cut points are returned as names

mtcars %>%
  group_by(cyl) %>%
  summarize(q = list(quantile(mpg)),
            cut_point = list(names(quantile(mpg)))) %>%
  unnest()

# # 4
mtcars %>%
  group_by(cyl) %>%
  summarize_each(funs = list) # %>% unnest()
                              # groups observations by cyl values
                              # and nests all observations in one row
                              # for each value of cyl



# # #   Exercises - Page 419   # # #
# # 1
lengths(mtcars %>%
           group_by(cyl) %>%
           summarize_each(funs = list))

mtcars %>%
  group_by(cyl) %>%
  summarize_each(funs = list) %>%
  mutate(mpgLength = lengths(mpg),
         dispLength = lengths(disp),
         isEqual = all(c(lengths(mpg),
                         lengths(disp),
                         lengths(hp))))

# lengths() can be used e.g. to create vector specifying length of specific entry
# or to check if nested rows are all equal in length ... many uses

# # 2
# Most common types: numeric(double, integer), character, date, datetime, logical
# Lists can contain different types of vectors (data frames are lists)