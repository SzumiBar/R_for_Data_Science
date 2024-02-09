library(tidyverse)

# # #   Exercises - Page 316   # # #
# # 1
# a)
output <- vector(mode = "numeric", length = ncol(mtcars))
for(i in seq_along(mtcars)) {
  output[[i]] <- mean(mtcars[ ,i])
  names(output) <- paste("mean_", colnames(mtcars), sep = "")
}
output

# b)
output <- vector(mode = "character", length = ncol(nycflights13::flights))
for(i in seq_along(flights)) {
  output[[i]] <- typeof(nycflights13::flights[[i]])
  names(output) <- paste(colnames(nycflights13::flights), "_type", sep = "")
}
output

# c)
unique_n <- vector(mode = "numeric", length = length(iris))
for(i in seq_along(iris)) {
  unique_n[i] <- length(unique(iris[ ,i]))
  names(unique_n) <- names(iris)
}
unique_n

# d)
µ_vector <- c(-10,0,10,100)
rnorm_10 <- tibble(`µ = -10` = rep(x = NA, times = 10),
                   `µ = 0` = rep(x = NA, times = 10),
                   `µ = 10` = rep(x = NA, times = 10),
                   `µ = 100` = rep(x = NA, times = 10))
for(i in seq_along(rnorm_10)) {
  rnorm_10[[i]] <- rnorm(n = 10, mean = µ_vector[i])
}
rnorm_10

# # 2
out <- ""
for (x in letters) {
  out <- stringr::str_c(out, x)
} # Longer version
stringr::str_c(letters[seq_along(letters)], collapse = "") # Shorter version

x <- sample(100)
sd. <- 0
for (i in seq_along(x)) {
  sd. <- sd. + (x[i] - mean(x)) ^ 2
}
sd. <- sqrt(sd. / (length(x) - 1)) # Longer version
sd(x) # Shorter version

x <- runif(100)
out <- vector("numeric", length(x))
out[1] <- x[1]
for (i in 2:length(x)) {
  out[i] <- out[i - 1] + x[i]
} # Longer version
cumsum(x) # Shorter version

# # 3
# a)
countdown <- c("five", "four", "three", "two", "one", "no")
for(i in countdown) {
  a <- vector(mode = "character", length = 0L)
  b <- "So go Alice go, boom, boom, boom!\n"
  c <- "Because Alice is a horse of course!"
  if(i == "five") {
    a <- stringr::str_c(rep("Alice the camel has five humps\n", times = 3), collapse = "")
  }
  if(i == "four") {
    a <- stringr::str_c(rep("Alice the camel has four humps\n", times = 3), collapse = "")
  }
  if(i == "three") {
    a <- stringr::str_c(rep("Alice the camel has three humps\n", times = 3), collapse = "")
  }
  if(i == "two") {
    a <- stringr::str_c(rep("Alice the camel has two humps\n", times = 3), collapse = "")
  }
  if(i == "one") {
    a <- stringr::str_c(rep("Alice the camel has one hump\n", times = 3), collapse = "")
  }
  if(i == "no") {
    a <- stringr::str_c(rep("Alice the camel has no humps\n", times = 3), collapse = "")
    return(cat(a, c))
  }
  cat(a, b)
}

# b)
TenInTheBed <- function(number) {
  for(i in number:1) {
    if(i > 2) {
      a <- stringr::str_c(i, "\nThere were ", i,
                          " in the bed\nAnd the little one said\n\"Roll over, roll over\"\nSo they all rolled over and one fell out\n",
                          sep = "")
      cat(a)
    }
    if(i == 2) {
      b <- stringr::str_c(i, "\nThere were ", i,
                          " in the bed\nAnd the little one said\n\"Roll over, roll over\"\nSo they both rolled over and one fell out\n",
                          sep = "")
      cat(b)
    }
    if(i == 1) {
      c <- stringr::str_c(i, "\nThere was ", i,
                          " in the bed\nAnd the little one said\n\"I'm lonely\"",
                          sep = "")
      cat(c)
    }
  }
}
TenInTheBed(number = 15)

# c)
BottlesOfBeer <- function(number, vessel, liquid) {
  for(i in number:1) {
    if(i > 1) {
      a <- stringr::str_c(i, " ", vessel, "s of ", liquid, " on the wall, ",
                          i, " ", vessel, "s of ", liquid, ".\n",
                          "Take one down and pass it around, ", i - 1, " ",
                          vessel, "s of ", liquid, " on the wall.\n\n", sep = "")
      cat(a)
    }
    if(i == 1) {
      b <- stringr::str_c(i, " ", vessel, " of ", liquid, " on the wall, ",
                          i, " ", vessel, " of ", liquid, ".\n",
                          "Take one down and pass it around, no more ",
                          vessel, "s of ", liquid, " on the wall.\n\n", sep = "")
      c <- stringr::str_c("No more ", vessel, "s on the wall, no more ",
                          vessel, "s of ", liquid, ".\n",
                          "Go to the store and buy some more, ", number,
                          " ", vessel, "s of ", liquid, " on the wall.", sep = "")
      cat(b, c, sep = "")
    }
  }
}
BottlesOfBeer(number = 10, vessel = "bottle", liquid = "beer")
BottlesOfBeer(number = 100, vessel = "cup", liquid = "grape juice")

# # 4
output <- vector(mode = "integer", length = 0L)
x <- 1:10000000
for (i in seq_along(x)) {
  output[i] <- x[i]
}
print(output) # ~15 seconds

output <- vector(mode = "integer", length = 10000000L)
x <- 1:10000000
for (i in seq_along(x)) {
  output[i] <- x[i]
}
print(output) # ~1 second



# # #   Exercises - Page 321   # # #
# # 1
files <- dir("data/", pattern = "\\.csv$", full.names = TRUE)
file_list <- vector(mode = "list", length = length(files))
for(i in seq_along(files)) {
  file_list[[i]] <- read_csv(files[[i]])
}
dplyr::bind_rows(file_list)

# # 2
no_names <- 1:length(letters)
for(nm in names(no_names)) {
  print(nm)
}

some_names <- no_names
names(some_names) <- c(letters[1:5], rep(NULL, times = length(letters) - 5))
for(nm in names(some_names)) {
  print(nm)
}

all_names <- no_names
names(all_names) <- rep(letters[1:(length(letters)/2)], times = 2)
for(nm in names(all_names)) {
  print(nm)
}

# # 3
col_mean <- function(dataframe) {
  for(i in seq_along(dataframe)) {
    if(is.numeric(dataframe[[i]])) {
      avr <- mean(dataframe[[i]])
      nm <- colnames(dataframe[i])
      cat(nm,":\t", round(avr, digit = 2), "\n", sep = "")
    }
  }
}
col_mean(dataframe = iris)

# # 4
trans <- list(disp = function(x) x * 0.0163871,
              am = function(x) {
              factor(x, labels = c("auto", "manual"))
              })

for (var in names(trans)) {
  mtcars[[var]] <- trans[[var]](mtcars[[var]])
}

# List containing two functions is created. Each function is applied
# to the variable of dataframe with corresponding name, dataframe is transformed



# # #   Exercises - Page 324   # # #
# # 1
A <- matrix(data = 1:12, nrow = 3, byrow = T)

apply(X = A, MARGIN = 1, FUN = mean)
output_1 <- numeric()
for(i in seq_len(nrow(A))) {
  output_1[i] <- mean(A[i, ])
}
output_1

apply(X = A, MARGIN = 2, FUN = mean)
output_2 <- numeric()
for(i in seq_len(ncol(A))) {
  output_2[i] <- mean(A[ ,i])
}
output_2

# # 2
col_summary <- function(df, fun) {
  num_vec <- logical()
  out <- vector("double", length(df))
  for(i in seq_along(df)) {
    num_vec[i] <- is.numeric(df[[i]])
    if(num_vec[i]) {
      out[i] <- fun(df[[i]])
    }
  }
  out
}
col_summary(df = iris, fun = mean)
col_summary(df = mpg, fun = mean)



# # #   Exercises - Page 329   # # #
# # 1
# a)
mtcars %>%
  map_dbl(.f = mean)

# b)
nycflights13::flights %>%
  map_chr(.f = typeof)

# c)
iris %>%
  map_int(.f = n_distinct)

# d)
c(-10,0,10,100) %>%
  purrr::map(.f = ~rnorm(n = 10, mean = .))

# # 2
map_lgl(.x = CO2, .f = is.factor)

# # 3
purrr::map(.x = 1:10, .f = ~rnorm(n = ., mean = .))

purrr::map(1:5, runif)

# # 4
purrr::map(-2:2, rnorm, n = 5)

map_dbl(-2:2, rnorm, n = 5)

# # 5
mtcars %>%
  split(.$cyl) %>%
  purrr::map(~ lm(mpg ~ wt, data = .))



# # #   Exercises - Page 339   # # #
# # 1
own_every <- function(.x, .p) {
  lgl <- logical()
  for(i in seq_along(.x)) {
    lgl[[i]] <- .p(.x[[i]])
  }
  if(sum(lgl) == length(lgl)) {
    return(T)
  } else {
    return(F)
  }
}

y <- list(0:10, 5.5)
own_every(.x = y, .p = is.numeric)
purrr::every(.x = y, .p = is.numeric)

y2 <- list(0:10, 5.5, "character", NA, NaN)
own_every(.x = y2, .p = is.numeric)
purrr::every(.x = y2, .p = is.numeric)

# # 2
enh_cs <- function(df, f.) {
  podsumowanie <- numeric()
  for(i in seq_along(df)) {
    if(is.numeric(df[[i]])) {
      podsumowanie[[i]] <- f.(df[[i]])
    } else {
      podsumowanie[[i]] <- NA
    }
  }
  names(podsumowanie) <- names(df)
  podsumowanie
}

enh_cs(df = iris, f. = sum)
col_summary(df = iris, fun = sum)

enh_cs(df = mpg, f. = sd)
col_summary(df = mpg, fun = sd)

# # 3
# sapply() returns empty list when there is no output causing errors