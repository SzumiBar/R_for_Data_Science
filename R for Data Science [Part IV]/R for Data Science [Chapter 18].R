library(tidyverse)
library(modelr)
options(na.action = na.warn)

# # #   Exercises - Page 353   # # #
# # 1
sim1a <- tibble(x = rep(1:10, each = 3),
                y = x * 1.5 + 6 + rt(length(x), df = 2))
lm(formula = y ~ x, data = sim1a)

sim2a <- tibble(x = rep(1:10, each = 3),
                y = x * 1.5 + 6 + rt(length(x), df = 2))
lm(formula = y ~ x, data = sim2a)

sim3a <- tibble(x = rep(1:10, each = 3),
                y = x * 1.5 + 6 + rt(length(x), df = 2))
lm(formula = y ~ x, data = sim3a)

ggplot() +
  geom_point(data = sim1a, mapping = aes(x = x, y = y), col = "blue") +
  geom_smooth(data = sim1a, mapping = aes(x = x, y = y), method = "lm", col = "blue") +
  geom_point(data = sim2a, mapping = aes(x = x, y = y), col = "red") +
  geom_smooth(data = sim2a, mapping = aes(x = x, y = y), method = "lm", col = "red") +
  geom_point(data = sim3a, mapping = aes(x = x, y = y), col = "green") +
  geom_smooth(data = sim3a, mapping = aes(x = x, y = y), method = "lm", col = "green") #Extreme values affect slope

# # 2
make_prediction <- function(mod, data) {
  mod[1] + mod[2] * data$x
}

measure_distance <- function(mod, data) {
  diff <- data$y - make_prediction(mod, data)
  mean(abs(diff))
}

optim(par = c(0,0), fn = measure_distance, data = sim1a)$par
coefficients(lm(formula = y ~ x, data = sim1a))

# # 3
model1 <- function(a, data) {
  a[1] + data$x * a[2] + a[3]
} # a[1] and a[3] are linearly dependent and could be represented by single value



# # #   Exercises - Page 358   # # #
# # 1
model <- loess(formula = y ~ x, data = sim1)
model2 <- lm(formula = y ~ x, data = sim1)
grid1 <- sim1 %>% data_grid(x)
grid1 <- grid1 %>%
  add_predictions(model = model) %>%
  rename(pred_loess = pred) %>%
  add_predictions(model = model2)
ggplot(data = sim1, mapping = aes(x = x)) +
  geom_point(mapping = aes(y = y)) +
  geom_smooth(mapping = aes(y = y), col = "white", se = F) +
  geom_line(data = grid1, mapping = aes(y = pred), col = "green", alpha = 0.5) +
  geom_line(data = grid1, mapping = aes(y = pred_loess), col = "red", alpha = 0.5)

# # 2
df <- tibble::tibble(x = sort(runif(100)),
                     y = 5 * x + 0.5 * x ^ 2 + 3 + rnorm(length(x)))
plot(df)

m1 <- lm(y ~ x, data = df)
grid <- data.frame(x = seq(0, 1, length = 10))
grid %>% add_predictions(m1) # Adds predictions from one model

m2 <- lm(y ~ poly(x, 2), data = df)
grid %>% spread_predictions(m1, m2) # Predictions from multiple models into columns
grid %>% gather_predictions(m1, m2) # Predictions from multiple models into rows
  
# # 3
# geom_ref_line() comes from modelr package, it shows reference line
# displaying reference line in residual plot allows to visualize
# spread of residuals, homoscedasticity/heteroscedasticity etc.
# it helps to serve as diagnostic tool
  
# # 4
# Frequency polygon of absolute residuals allows to visualize their distribution
# Overlapping of raw residuals masks their count, only spread is possible to determine



# # #   Exercises - Page 371   # # #
# # 1
w_interc <- lm(formula = y ~ x, data = sim2)
wo_interc <- lm(formula = y ~ x - 1, data = sim2)
sim2 %>% data_grid(x) %>% spread_predictions(w_interc, wo_interc)

# # 2
mod1.3 <- lm(y ~ x1 + x2, data = sim3)
mod2.3 <- lm(y ~ x1 * x2, data = sim3)

mod1.4 <- lm(y ~ x1 + x2, data = sim4)
mod2.4 <- lm(y ~ x1 * x2, data = sim4)

View(model_matrix(data = sim3, mod1.3))
View(model_matrix(data = sim3, mod2.3))
View(model_matrix(data = sim3, mod1.4))
View(model_matrix(data = sim3, mod2.4))

# # 3

# # 4
mod1 <- lm(y ~ x1 + x2, data = sim4)
mod2 <- lm(y ~ x1 * x2, data = sim4)
quest4 <- gather_residuals(data = sim4, mod1, mod2)

ggplot(data = quest4, mapping = aes(x = y, y = resid)) +
  geom_point(mapping = aes(col = model), size = 0.9) +
  geom_smooth(mapping = aes(col = model), se = F, method = "lm", size = 0.5) +
  geom_ref_line(h = 0) +
  labs(x = NULL, y = "Residuals", col = "Model", title = "Residuals plot") +
  theme(plot.title = element_text(hjust = 0.5)) # mod2 is better