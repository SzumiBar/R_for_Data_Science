library(tidyverse)

# # #   Exercises - Page 296   # # #
# # 1
#is.finite() checks if value is finite numeric
#!is.infinite() checks if value is not infinite, but it doesn't need to be numeric
is.finite(x = 5)
!is.infinite(x = 5)

is.finite(x = "string")
!is.infinite(x = "string")

# # 2
dplyr::near(x = sqrt(2) * sqrt(2),
            y = 2)

# # 3
logical_vec <- c(T, F, NA, NULL)
is.logical(logical_vec)

integer_vec <- c(1L, NA, NULL, T, F)
is.integer(integer_vec)

double_vec <- c(1, 1.1, NA, NaN, Inf, -Inf, NULL, T, F)
is.double(double_vec)

# # 4
dbl_to_int <- function(double, round = c("up", "down")) {
  rounded <- 0
  if(round == "up") {
    rounded <- ceiling(double)
  }
  if(round == "down"){
    rounded <- floor(double)
  }
  rounded <- as.integer(rounded)
  rounded
}
is.integer(dbl_to_int(double = 4.5))
dbl_to_int(double = 4.5, round = "up")
dbl_to_int(double = 4.5, round = "down")

# # 5
ciag <- "1"

parse_logical(ciag)
parse_integer(ciag)
parse_double(ciag)



# # #   Exercises - Page 302   # # #
# # 1
x <- rep(x = c(NA,1:3,Inf,NaN), times = 10, each = 3)
mean(is.na(x)) # Proportion of NA and NaN values in vector
sum(!is.finite(x)) # Number of not finite and numeric values

# # 2
# is.vector() returns TRUE if x is a vector of the specified mode
# having no attributes other than names. It returns FALSE otherwise
# is.atomic() cannot use lists and works whether or not values have attributes

# # 3
y <- 1:3

setNames(object = y, nm = c("raz", "dwa", "trzy"))
purrr::set_names(x = y, nm = c("raz", "dwa", "trzy"))

setNames(object = y)
purrr::set_names(x = y)

setNames(object = y, nm = letters[1:3])
purrr::set_names(x = y, nm = letters[1:3])

setNames(object = y, nm = "recycle")
purrr::set_names(x = y, nm = "recycle")

# # 4
quest_4 <- c("1st" = 1, "2nd" = 3, "3rd" = 6, "4th" = 1, "5th" = 9, "6th" = 10,
             "7th" = 7, "8th" = 9, "9th" = 5, "10th" = 11)
# a)
last_val <- function(wektor) {
  wektor[length(wektor)]
}
last_val(wektor = quest_4)

# b)
even_ind <- function(wektor) {
  wektor[seq(from = 2, to = length(wektor), by = 2)]
}
even_ind(wektor = quest_4)

# c)
allButLast <- function(wektor) {
  wektor[-length(wektor)]
}
allButLast(wektor = quest_4)

# d)
even_val <- function(wektor) {
  wektor[wektor %% 2 == 0]
}
even_val(wektor = quest_4)

# # 5
# x[-which(x > 0)] and x[x <= 0] differ only in the way they treat NaN's

x <- c(-3:3, NA, T, F, NaN, NULL, Inf)
x[-which(x > 0)]
x[x <= 0]

# # 6
y <- 1:10
y[11]
quest_4["11th"]



# # #   Exercises - Page 307   # # #
# # 2
# Tibble components, unlike list's, need to be of the same length
tibby <- tibble(x = 1:5, y = LETTERS[1:5], z = 6:10, w = letters[1:5])
tibby[2:3]
tibby[[4]]
tibby[[4]][2]



# # #   Exercises - Page 312   # # #
# # 1
d <- hms::hms(3600)
print(d)
typeof(d)
attributes(d)

# # 2
notibble <- tibble(x = 1:6, y = 1:2)
recytibble <- tibble(x = 1:6, y = "Recycle length 1")

# # 3
listibble <- tibble(x = 1:5,
                    y = list(1:5))
listibble[2]
listibble[[2]]