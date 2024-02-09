# # #   Exercises - Page 123   # # #
# # 1
library(tidyverse)

head(x = mtcars, n = 5)
head(x = as_tibble(x = mtcars), n = 5)

# # 2
df <- data.frame(abc = 1, xyz = "a")
df$x
df[, "xyz"]
df[, c("abc", "xyz")]

tdf <- as_tibble(df)
tdf$x
tdf[, "xyz"]
tdf[, c("abc", "xyz")]

# # 3
tmpg <- as_tibble(mpg)

tmpg[["manufacturer"]] # Vector form
tmpg$manufacturer # Vector form
tmpg %>% .$manufacturer # Vector form
tmpg[ ,"manufacturer"] # Data frame/Tibble form

# # 4
annoying <- tibble(`1` = 1:10,
                   `2` = `1` * 2 + rnorm(length(`1`)))

# a)
annoying$`1`
annoying[["1"]]
annoying[ ,"1"]
annnoying

# b)
plot(x = annoying$`1`, y = annoying$`2`)
plot(x = annoying[[1]], y = annoying[[2]])
plot(x = annoying[ ,"1"], y = annoying[ ,"2"]) # Error

ggplot(data = annoying) +
  geom_point(mapping = aes(x = `1`, y = `2`))

# c)
annoying$`3` <- annoying$`2` / annoying$`1`
annoying[ ,"3"] <- annoying[[2]] / annoying[[1]]
annoying[ ,"3"] <- annoying[ ,"2"] / annoying[ ,"1"]

# d)
colnames(annoying) <- c("one", "two", "three")
annoying <- rename(.data = annoying, "one" = `1`, "two" = `2`, "three" = `3`)
glimpse(annoying)

# # 5
wektor_1 <- 11:20

wektor_2 <- 21:30
names(wektor_2) <- paste("Nazwa_", 11:20, sep = "")

lista <- list(`1st` = wektor_1, `2nd` = wektor_2)

enframe(x = wektor_1, name = "ID") -> enframe_v1
enframe(x = wektor_2, name = "ID", value = "Values") -> enframe_v2
enframe(x = lista, name = "ID", value = "Objects") -> enframe_list

enframe_list[1] # Refer to tibble column
enframe_list[2] # Refer to tibble column
enframe_list[[1]] # Refer to entries in tibble column
enframe_list[[2]] # Refer to entries in tibble column
enframe_list[[2]][2] # Refer to specific entry in tibble column
enframe_list[[2]][[2]] # Refer to entries within specific entry in tibble column - in this case specific entry contains object(s)
enframe_list[[2]][[2]][2] # Refer to entry in object
enframe_list[[2]][[2]][[2]] # The same result as above, because entries are single values (not objects)

# # 6
library(nycflights13)
dfflights <- as.data.frame(flights)
print(x = dfflights, n = 6) # Does not work on data frames
print(x = flights, n = 6)
print(x = flights, n = 6, width = Inf)
print(x = flights, n = 6, max_extra_cols = 2)