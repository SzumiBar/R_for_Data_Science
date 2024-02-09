# # #   Exercises - Page 273   # # #
# # 1
rescale01 <- function(x) {
  rng <- range(x, na.rm = FALSE, finite = TRUE)
  (x - rng[1]) / (rng[2] - rng[1])
}
rescale01(x = NA)

# # 2
rescale01_2 <- function(x) {
  rng <- range(x, na.rm = FALSE, finite = TRUE)
  y <- (x - rng[1]) / (rng[2] - rng[1])
  y[y == Inf] <- 1
  y[y == -Inf] <- 0
  return(y)
}
rescale01_2(x = c(1,2,5,4,Inf,-Inf))

# # 3
miss_prop <- function(x) {
  return(mean(x = is.na(x)))
}

sum_to_one <- function(x) {
  x / sum(x, na.rm = TRUE)
}

coef_of_var <- function(x) {
  sd(x, na.rm = TRUE) / mean(x, na.rm = TRUE)
}

# # 4
# Variance = SS / (n - 1)
my_variance <- function(x) {
  x_hat <- mean(x, na.rm = T)
  n <- length(x)
  variance <- sum((x - x_hat)^2) / (n - 1)
  return(variance)
}
var(iris$Sepal.Length)
my_variance(iris$Sepal.Length)

# Skewness = 3 * (Mean - Median) / SD
skewness <- function(x) {
  x_hat <- mean(x, na.rm = T)
  x_tilde <- median(x, na.rm = T)
  sigma <- sd(x, na.rm = T)
  skew <- 3 * (x_hat - x_tilde) / sigma
  return(skew)
}
skewness(iris$Sepal.Length)
plot(density(iris$Sepal.Length))

# # 5
both_na <- function(first, second) {
  first_NAs <- sum(is.na(first))
  second_NAs <- sum(is.na(second))
  to_print <- cat("First vector contains", first_NAs, "NA's\nSecond vector contains",
                second_NAs, "NA's\nBoth vectors contain", first_NAs + second_NAs, "NA's")
  return(to_print)
}
wektor_1 <- c(NA,3,1,4,3,NA,2,NA,2,NA,NA,NA)
wektor_2 <- c(9,9,2,4,2,3,NA,NA,32,NA)
both_na(first = wektor_1, second = wektor_2)

# # 6
is_directory <- function(x) file.info(x)$isdir # Checks whether file is in directory
is_readable <- function(x) file.access(x, 4) == 0 # Checks whether file exists and there is permission to open it



# # #   Exercises - Page 276   # # #
# # 1
prefix_check <- function(string, prefix) {
  substr(x = string, start = 1, stop = nchar(prefix)) == prefix
} # Checks whether prefix if present within string
ciagi <- c("pre-workout", "pre-mature", "log", "pre-existing")
prefix_check(string = ciagi, prefix = "pre-")

except_last <- function(x) {
  if (length(x) <= 1) return(NULL)
  x[-length(x)]
} # Returns all entries of object except last one
test <- 1:5
test2 <- c(c(1:5), c(6:10))
except_last(x = test)
except_last(x = test2)

repeater <- function(x, y) {
  rep(y, length.out = length(x))
} # Repeats second value number of times equivalent to the length of first value
first <- 1:10
second <- "To repeat"
repeater(x = first, y = second)

# # 3
library(MASS)

set.seed(123)
mvrnorm(n = 5, mu = 0, Sigma = 1)

set.seed(123)
as.matrix(rnorm(n = 5, mean = 0, sd = 1))

# # 4
# The same prefix for family of functions makes looking them all up easier



# # #   Exercises - Page 279   # # #
# # 1
# Control-flow constructs based on `if` allow to form simple or complex expression
# based on logical value of condition which is not vectorized
# ifelse() function conducts number of tests equivalent to number of values within
# test argument and returns equivalently long, reshaped object

wektor_1 <- 1:10
if(wektor_1[3] == 3) {
  wektor_1[3] <- "equal"
  wektor_1
}
ifelse(test = wektor_1 == 3, yes = "equal", no = wektor_1)

wektor_2 <- c(1,2,3,NA,5,6,7,NA,9,10)
if(wektor_2 < 5) {
  wektor_2 <- "less"
  wektor_2
}
ifelse(test = wektor_2 < 5, yes = "less", no = wektor_2)

if(wektor_2 > 5) {
  wektor_2 <- "more"
  wektor_2
}
ifelse(test = wektor_2 > 5, yes = "more", no = wektor_2)

# # 2
greeting <- function(dtime) {
  teraz <- now()
  godzina <- hour(teraz)
  if(between(x = godzina, left = 0, right = 4)) {
    print("Good night")
  }
  if(between(x = godzina, left = 5, right = 12)) {
    print("Good morning")
  }
  if(between(x = godzina, left = 13, right = 18)) {
    print("Good afternoon")
  }
  if(between(x = godzina, left = 19, right = 21)) {
    print("Good evening")
  }
  if(between(x = godzina, left = 22, right = 24)) {
    print("Good night")
  }
}
greeting()

# # 3
fizzbuzz <- function(number) {
  if(number %% 3 == 0 & number %% 5 == 0) {
    print("fizzbuzz")
  } else {
    if(number %% 3 == 0) {
      print("fizz")
    }
    if(number %% 5 == 0) {
      print("buzz")
    }
  }
  if(number %% 3 != 0 & number %% 5 != 0) {
    print(number)
  }
}
fizzbuzz(number = 3)
fizzbuzz(number = 5)
fizzbuzz(number = 15)
fizzbuzz(number = 17)

# # 4
temp_range <- -10:40
cut(x = temp_range,
    breaks = c(-Inf, 0, 10, 20, 30, Inf),
    labels = c("freezing", "cold", "cool", "warm", "hot"))

cut(x = temp_range,
    breaks = c(-Inf, 0, 10, 20, 30, Inf),
    labels = c("freezing", "cold", "cool", "warm", "hot"),
    right = F)

# # 5
switcher_1 <- function(x) {
  switch(EXPR = x,
         a = "Answer A", b = "Answer B", c = "Answer C", d = "Answer D")
}
switcher_1(x = "a")
switcher_1(x = 3)

# # 6
switcher_2 <- function(x) {
  switch(x,
         a = , b = "ab", c = , d = "cd")
}
switcher_2("a")
switcher_2("b")
switcher_2("c")
switcher_2("d")
switcher_2("e")



# # #   Exercises - Page 285   # # #
# # 1
commas <- function(...) stringr::str_c(..., collapse = ", ")
commas(letters, collapse = "-") # 'collapse' argument is treated as string to combine
                                # so it throws an error

commas2 <- function(...) stringr::str_c(...)
commas2(letters, collapse = "-")

# # 2
rule <- function(..., pad = "-") {
  title <- paste0(...)
  width <- getOption("width") - nchar(title) - 5
  cat(title, " ", stringr::str_dup(pad, width), "\n", sep = "")
}
rule("Important output", pad = "+-")

rule2 <- function(..., pad = "-") {
  title <- paste0(...)
  width <- getOption("width") - nchar(title) - 5
  fixed_width <- round(width/nchar(pad), digits = 0)
  cat(title, " ", stringr::str_dup(pad, fixed_width), "\n", sep = "")
}
rule2("Important output", pad = "+-")
rule2("Important output", pad = " d>.<b ")

# # 3
wektor <- c(1:100, 1000)
mean(x = wektor)
mean(x = wektor, trim = 0.01) # `trim` argument excludes fraction of values
                              # from both ends of distribution for mean calculation

# # 4
# Argument `method = c("pearson", "kendall", "spearman")` to cor() function
# takes only one of those three values, with "pearson" as default