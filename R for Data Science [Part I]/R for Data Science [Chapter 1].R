# # #   Exercises - Page 6   # # #
# # 1
library(tidyverse)
ggplot(data = mpg)

# # 2
nrow(mpg)
ncol(mpg)

# # 4
ggplot(data = mpg) +
  geom_point(mapping = aes(x = hwy, y = cyl))

# # 5
ggplot(data = mpg) +
  geom_point(mapping = aes(x = class, y = cyl))



# # #   Exercises - Page 12   # # #
# # 1
ggplot(data = mpg) +
  geom_point(mapping = aes(x = displ, y = hwy, color = "blue"))

ggplot(data = mpg) +
  geom_point(mapping = aes(x = displ, y = hwy), col = "blue")

# # 2
which(sapply(X = mpg, FUN = class) == "character")

# # 3
ggplot(data = mpg) +
  geom_point(mapping = aes(x = cty, y = hwy, col = displ))

# # 4
ggplot(data = mpg) +
  geom_point(mapping = aes(x = cty, y = displ, col = displ, size = displ))

# # 5
ggplot(data = mpg) +
  geom_point(mapping = aes(x = cty, y = hwy, stroke = displ), shape = 1)

# # 6
ggplot(data = mpg) +
  geom_point(mapping = aes(x = cty, y = hwy, col = displ < 5))



# # #   Exercises - Page 15   # # #
# # 1
ggplot(data = mpg, mapping = aes(x = cty, y = displ)) +
  facet_wrap(~ hwy)

# # 2
ggplot(data = mpg) +
  geom_point(mapping = aes(x = drv, y = cyl)) +
  facet_grid(drv ~ cyl)

# # 3
ggplot(data = mpg) +
  geom_point(mapping = aes(x = displ, y = hwy)) +
  facet_grid(drv ~ .)

ggplot(data = mpg) +
  geom_point(mapping = aes(x = displ, y = hwy)) +
  facet_grid(. ~ cyl)

# # 5
ggplot(data = mpg, mapping = aes(x = cty, y = displ)) +
  facet_wrap(~ manufacturer, nrow = 2)

ggplot(data = mpg, mapping = aes(x = cty, y = displ)) +
  facet_wrap(~ manufacturer, ncol = 2)



# # #   Exercises - Page 20   # # #
# # 1
ggplot(data = mpg, mapping = aes(x = displ, y = hwy)) +
  geom_line()

ggplot(data = mpg, mapping = aes(x = drv, y = hwy)) +
  geom_boxplot()

ggplot(data = mpg, mapping = aes(x = hwy)) +
  geom_histogram()

ggplot(data = mpg, mapping = aes(x = hwy, y = displ)) +
  geom_area()

# # 2
ggplot(data = mpg, mapping = aes(x = displ, y = hwy, color = drv)) +
  geom_point() +
  geom_smooth(se = FALSE)

# # 3
ggplot(data = mpg, mapping = aes(x = displ, y = hwy, color = drv)) +
  geom_point(show.legend = FALSE) +
  geom_smooth(se = FALSE)

ggplot(data = mpg, mapping = aes(x = displ, y = hwy, color = drv)) +
  geom_point() +
  geom_smooth(se = FALSE, show.legend = FALSE)

# # 4
ggplot(data = mpg, mapping = aes(x = displ, y = hwy, color = drv)) +
  geom_smooth(se = TRUE)

ggplot(data = mpg, mapping = aes(x = displ, y = hwy, color = drv)) +
  geom_smooth(se = FALSE)

# # 5
ggplot(data = mpg, mapping = aes(x = displ, y = hwy)) +
  geom_point() +
  geom_smooth()

ggplot() +
  geom_point(data = mpg, mapping = aes(x = displ, y = hwy)) +
  geom_smooth(data = mpg, mapping = aes(x = displ, y = hwy))

# # 6
ggplot(data = mpg, mapping = aes(x = displ, y = hwy)) +
  geom_point() +
  geom_smooth(se = FALSE)

ggplot(data = mpg, mapping = aes(x = displ, y = hwy)) +
  geom_point() +
  geom_smooth(mapping = aes(group = drv), se = FALSE)

ggplot(data = mpg, mapping = aes(x = displ, y = hwy, col = drv)) +
  geom_point() +
  geom_smooth(se = FALSE)

ggplot(data = mpg, mapping = aes(x = displ, y = hwy)) +
  geom_point(mapping = aes(col = drv)) +
  geom_smooth(se = FALSE)

ggplot(data = mpg, mapping = aes(x = displ, y = hwy)) +
  geom_point(mapping = aes(col = drv)) +
  geom_smooth(mapping = aes(linetype = drv), se = FALSE)

ggplot(data = mpg, mapping = aes(x = displ, y = hwy)) +
  geom_point(size = 4, col = "white") +
  geom_point(mapping = aes(col = drv))



# # #   Exercises - Page 26   # # #
# # 1
ggplot(data = diamonds, mapping = aes(x = cut, y = depth)) +
  geom_pointrange(stat = "summary", fun.min = min, fun.max = max, fun = median)

# # 2
ggplot(data = mpg, mapping = aes(x = class)) +
  geom_bar()

ggplot(data = mpg, mapping = aes(x = class)) +
  geom_col(mapping = aes(y = hwy))

# # 4
ggplot(data = mpg, mapping = aes(x = hwy)) +
  geom_point(mapping = aes(y = displ)) +
  stat_smooth(mapping = aes(y = displ), method = "lm", formula = y ~ x + I(x^2), se = FALSE)

# # 5
ggplot(data = diamonds) +
  geom_bar(mapping = aes(x = cut, y = ..prop..))

ggplot(data = diamonds) +
  geom_bar(mapping = aes(x = cut, y = ..prop.., group = 1))

ggplot(data = diamonds) +
  geom_bar(mapping = aes(x = cut, fill = color, y = ..prop..))

ggplot(data = diamonds) +
  geom_bar(mapping = aes(x = cut, fill = color, y = ..prop.., group = color))



# # #   Exercises - Page 31   # # #
# # 1
ggplot(data = mpg, mapping = aes(x = cty, y = hwy)) +
  geom_point(position = "jitter")

# # 2
ggplot(data = mpg, mapping = aes(x = cty, y = hwy)) +
  geom_jitter(width = 0.5, height = 0.5)

ggplot(data = mpg, mapping = aes(x = cty, y = hwy)) +
  geom_jitter(width = 2, height = 2)

# # 3
ggplot(data = mpg, mapping = aes(x = cty, y = hwy)) +
  geom_count(alpha = 0.25, show.legend = F) +
  geom_jitter(size = 0.5, width = 0.25, height = 0.25)

# # 4
ggplot(data = mpg, mapping = aes(x = drv, y = hwy, fill = factor(year))) +
  geom_boxplot()

ggplot(data = mpg, mapping = aes(x = drv, y = hwy, fill = factor(year))) +
  geom_boxplot(position = "dodge")

ggplot(data = mpg, mapping = aes(x = drv, y = hwy, fill = factor(year))) +
  geom_boxplot(position = "dodge2")

ggplot(data = mpg, mapping = aes(x = drv, y = hwy, fill = factor(year))) +
  geom_boxplot(position = "identity", alpha = 0.5)



# # #   Exercises - Page 33   # # #
# # 1
ggplot(data = mpg, mapping = aes(x = factor(1), fill = class)) +
  geom_bar()

ggplot(data = mpg, mapping = aes(x = factor(1), fill = class)) +
  geom_bar(position = "dodge") +
  coord_polar()

# # 2
ggplot(data = mpg, mapping = aes(x = factor(1), fill = class)) +
  geom_bar(position = "dodge") +
  coord_polar() +
  labs(x = "", y = "Count", title = "Exercise 2")

# # 4
ggplot(data = mpg, mapping = aes(x = cty, y = hwy)) +
  geom_point() +
  geom_abline()

ggplot(data = mpg, mapping = aes(x = cty, y = hwy)) +
  geom_point() +
  geom_abline() +
  coord_fixed()