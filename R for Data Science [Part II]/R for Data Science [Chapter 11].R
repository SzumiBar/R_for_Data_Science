library(tidyverse)
library(stringr)

# # #   Exercises - Page 199   # # #
# # 1
paste("To", "jest", "zdanie") # By default sep = " "
paste0("To", "jest", "zdanie") # By default sep = ""
str_c("To", "jest", "zdanie") # By default sep = ""

paste("To", NA, "jest", "zdanie")
paste0("To", NA, "jest", "zdanie")
str_c("To", NA, "jest", "zdanie")

# # 2
wektor_slow <- c("raz", "dwa", "trzy")

str_c("odliczam...", wektor_slow)
str_c("odliczam...", wektor_slow, sep = " ")
str_c("odliczam...", wektor_slow, collapse = ", ")

# # 3
slowo <- "Konstantynopolitanczykowianeczka"

str_length(string = slowo) -> dlugosc
str_sub(string = slowo, start = dlugosc/2, end = dlugosc/2 + 1)

# # 4
zdanie <- "To jest bardzo dlugi string, ktory trzeba porozdzielac.
Szerokosc to siedem, czyli zdanie bedzie rozdzielone do nowych linii,
a w kazdej moga byc dwa slowa jesli razem maja siedem znakow"

wrapped <- str_wrap(string = zdanie, width = 7)
writeLines(wrapped)

# # 5
zdanie2 <- "  To  jest  krotki  string  "

str_trim(string = zdanie2)
str_trim(string = zdanie2, side = "left")
str_trim(string = zdanie2, side = "right")

str_pad(string = slowo, width = 34, side = "both", pad = "-")
str_pad(string = slowo, width = 34, side = "left")
str_pad(string = slowo, width = 34, side = "right", pad = "+")

# # 6
stringer <- function(wektor) {
  if (length(wektor) >= 2) {
    pierwsza_cz <- str_c(wektor[-length(wektor)], collapse = ", ")
    druga_cz <- str_c(pierwsza_cz, ", and ", wektor[length(wektor)])
    return(druga_cz)
  } else {
    errorCondition(message = "Passed vector must have length of at least 2")
  }
}

litery <- c("a", "b", "c", "d", "e")
stringer(wektor = litery)
stringer(wektor = "a")



# # #   Exercises - Page 201   # # #
# # 1
# "\" - despacialize next character in string. In this case, this resolves to single quote in regexp.
writeLines("\"")
# "\\" - despacialize next character in string, which is \ itself. This resolves to \ in regexp.
writeLines("\\")
# "\\\" - 1st \\ will resolve to \ in regexp. 3rd \ despecialize next character, in this case quote mark.
writeLines("\\\"")
# "\\\\" - 1st \\ will resolve to \ in regexp, 2nd \\ also. This will resolve to \\ in regexp,
# where 1st \ will despecialize 2nd \ in regexp enabling to match explicit \ character in string.

# # 2
ciag <- c("\"'\\", "\\", "\\\\")
str_view(string = ciag, pattern = "\"'\\\\")

# # 3
ciag2 <- c(".a.b.c", ".1.2.3", "d.e.f.", "......")
reg_exp <- "\\..\\..\\.." 
writeLines(reg_exp) #\..\..\.. - \. is dot and . is any character in regexp
                    #so it will match triple sequence of dot followed by any character in string
str_view(string = ciag2, pattern = reg_exp)



# # #   Exercises - Page 203   # # #
# # 1
ciag3 <- c("$^$", "abc$^$def", "$^$$^$")
reg_exp2 <- "\\$\\^\\$"
reg_exp3 <- "^\\$\\^\\$$"
writeLines(reg_exp2)
writeLines(reg_exp3)

str_view(string = ciag3, pattern = reg_exp2)
str_view(string = ciag3, pattern = reg_exp3)

# # 2
# a)
str_view(string = stringr::words, pattern = "^y", match = T)

# b)
str_view(string = stringr::words, pattern = "x$", match = T)

# c)
str_view(string = stringr::words, pattern = "^...$", match = T)

# d)
str_view(string = stringr::words, pattern = ".......", match = T)



# # #   Exercises - Page 204   # # #
# # 1
# a)
str_view(string = stringr::words, pattern = "^[aeiou]", match = T)

# b)
str_detect(string = words, pattern = "[aeiou]") -> with_consonants
words[which(with_consonants == F)] -> without_consonants

# c)
str_view(string = stringr::words, pattern = "[^e]ed$", match = T)

# d)
str_view(string = stringr::words, pattern = "ing|ize$", match = T)

# # 2
str_view(string = stringr::words, pattern = "[^c]ie", match = T)

# # 3
str_subset(string = stringr::words, pattern = "q[^u]")

# # 4
str_view(string = stringr::words, pattern = "ise$", match = T)

# # 5
phone_nums <- c("473-203-294", "445 421 985", "748937728")

nr_reg1 <- "\\d\\d\\d-\\d\\d\\d-\\d\\d\\d"
nr_reg2 <- "[0-9][0-9][0-9]-[0-9][0-9][0-9]-[0-9][0-9][0-9]"
nr_reg3 <- "\\d\\d\\d\\s\\d\\d\\d\\s\\d\\d\\d"

str_subset(string = phone_nums, pattern = nr_reg1)
str_subset(string = phone_nums, pattern = nr_reg2)
str_subset(string = phone_nums, pattern = nr_reg3)



# # #   Exercises - Page 206   # # #
# # 1
# 0 or 1: ? == {,1}   |   ? == {0,1}
# 0 or more: * == {0,}
# 1 or more: + == {1,}

# # 2
# a)
matchings_1 <- c("w3iRd string", "9578", "..(-+2", "")
rexp_1 <- "^.*$"
writeLines(text = rexp_1) # Match is a string with 0 or more of any characters - any string will match
str_view(string = matchings_1, pattern = rexp_1, match = T)

# b)
matchings_2 <- c("{w3iRd string}", "{9578}", "{}", "no brackets")
rexp_2 <- "\\{.+\\}"
writeLines(text = rexp_2) # Match is a string of 1 or more of any characters within curly brackets
str_view(string = matchings_2, pattern = rexp_2, match = T)

# c)
matchings_3 <- c("4750-11-46", "132-462-950", "no digit", "3685-00-00")
rexp_3 <- "\\d{4}-\\d{2}-\\d{2}"
writeLines(text = rexp_3) # Match is a string of any 4, 2 and 2 digits sequence separated by hyphens
str_view(string = matchings_3, pattern = rexp_3, match = T)

# d)
matchings_4 <- c("\\\\", "cos", "\\", "\\\\\\\\")
rexp_4 <- "\\\\{4}"
writeLines(text = rexp_4) # Matches string with four backslashes
str_view(string = matchings_4, pattern = rexp_4, match = T)

# # 3
# a)
regexp_a <- "^[^aeiou]{3}"
str_view(string = stringr::words, pattern = regexp_a, match = T)

# b)
regexp_b <- "[aieou]{3,}"
str_view(string = stringr::words, pattern = regexp_b, match = T)

# c)
regexp_c <- "([aeiou][^aeiou]){2,}"
str_view(string = stringr::words, pattern = regexp_c, match = T)



# # #   Exercises - Page 207   # # #
# # 1
# a)
match_a <- c("dddd", "9999", "<<<<", "empg", "2668", "1d")
rexp_a <- "(.)\\1\\1"
writeLines(text = rexp_a) # Matches any character which repeats 3 times. \1 matches character which references
                   # first character in string - it is done two times in a row
str_view(string = match_a, pattern = rexp_a)

# b)
match_b <- c("abba", "9191", "0110", "<>><", "TTTT", "AbaabA")
rexp_b <- "(.)(.)\\2\\1"
writeLines(text = rexp_b) # Matches any first character and any second character followed by repeated second
                   # character and repeated first character
str_view(string = match_b, pattern = rexp_b)

# c)
match_c <- c("AbAb", ">4>4", "1001", "abcd", "ababab")
rexp_c <- "(..)\\1"
writeLines(text = rexp_c) # Matches any two characters repeated one time
str_view(string = match_c, pattern = rexp_c)

# d)
match_d <- c("121>1", "aaaaa", "abaca", "abbcb")
rexp_d <- "(.).\\1.\\1"
writeLines(text = rexp_d) # Matches any two characters followed by first character, followed by any character,
                          # followed by first character
str_view(string = match_d, pattern = rexp_d)

# e)
match_e <- c("abba", "1>1>1>1", "abcde939>hijcba", "1234567654123")
rexp_e <- "(.)(.)(.).*\\3\\2\\1"
writeLines(text = rexp_e) # Matches any three characters, followed by one or more any other characters,
                          # ended by repeated third character, followed by second character,
                          # followed by first character
str_view(string = match_e, pattern = rexp_e)

# # 2
# a)
regexp_2a <- "^(.).*\\1*$"
writeLines(text = regexp_2a)
test_a <- c("anb7ra", "<..920<", "1", "", "abc098")
str_subset(string = test_a, pattern = regexp_2a) # Starts and ends with the same character

# b)
regexp_2b <- "(..).*\\1.*"
writeLines(text = regexp_2b)
test_b <- c("church", "abrakadabra", "mama", "no match", "match match")
str_view(string = test_b, pattern = regexp_2b)

# c)
regexp_2c <- "(.).*\\1.*\\1.*"
writeLines(text = regexp_2c)
test_c <- c("Au4>ApppA...match on A, . and p", "B.B.will not match", "CCC will match", "eleven")
str_view(string = test_c, pattern = regexp_2c)



# # #   Exercises - Page 211   # # #
# # 1
# a)
str_subset(string = words, pattern = "^x|x$")
words[str_detect(string = words, pattern = "^x|x$")]

start_x <- str_detect(string = words, pattern = "^x")
end_x <- str_detect(string = words, pattern = "x$")
words[start_x | end_x]

# b)
str_subset(string = words, pattern = "^[aeiou].*[^aeiou]$")
words[str_detect(string = words, pattern = "^[aeiou].*[^aeiou]$")]

start_vowel <- str_detect(string = words, pattern = "^[aeiou]")
end_consonent <- str_detect(string = words, pattern = "[^aeiou]$")
words[start_vowel & end_consonent]

# c)
# It is too difficult to construct single regexp for such match
# as it has to take into account all possible orders of vowels

vow.1 <- str_detect(string = words, pattern = "a")
vow.2 <- str_detect(string = words, pattern = "e")
vow.3 <- str_detect(string = words, pattern = "i")
vow.4 <- str_detect(string = words, pattern = "o")
vow.5 <- str_detect(string = words, pattern = "u")
words[vow.1 & vow.2 & vow.3 & vow.4 & vow.5]

# d)
vowels_nr <- str_count(string = words, pattern = "[aeiou]")
words[vowels_nr == max(vowels_nr)] # Words with highest number of vowels

letters_nr <- str_count(string = words, pattern = ".")
vowels_prop <- vowels_nr/letters_nr
words[vowels_prop == max(vowels_prop)] # Words with highesst proportion of vowels



# # #   Exercises - Page 213   # # #
# # 1
color <- c("red", "orange", "yellow", "green", "blue", "purple")
color_1 <- str_c(color, collapse = "|")
color_2 <- str_c("^", color_1, "$", sep = "")

has_color <- str_subset(string = sentences, pattern = color_2)
has_color_extracted <- str_extract_all(string = has_color, color_1, simplify = T)

str_view_all(string = has_color, pattern = color_1)

# # 2
# a)
str_extract(string = sentences, pattern = "[A-Z][a-z]*")

# b)
str_extract_all(string = sentences, pattern = "[A-Za-z]*ing", simplify = T)

# c)
str_extract_all(string = sentences, pattern = "[A-Za-z]{4,}s\\s")
sentences[713]



# # #   Exercises - Page 215   # # #
# # 1
regexp <- "\\s(one|two|three|four|five|six|seven|eight|nine|ten)\\s[^ \\.]+"
str_extract_all(string = sentences, pattern = regexp, simplify = T)

matched_sent <- sentences[str_detect(string = sentences, pattern = regexp)]
extracted <- str_extract_all(string = matched_sent, pattern = regexp, simplify = T)
str_sub(string = extracted, start = 2)

# # 2
contractions <- str_detect(string = sentences, pattern = "[A-Za-z]+'[A-Za-z]")
with_contr <- sentences[contractions]

extracted_contr <- str_extract_all(string = with_contr, pattern = "[A-Za-z]+'[A-Za-z]", simplify = T)
str_split_fixed(string = extracted_contr, pattern = "'", n = 2)



# # #   Exercises - Page 216   # # #
# # 1
ciag_1.1 <- "83 / // 4nbm / ><///r/"
ciag_1.2 <- str_replace_all(string = ciag_1, pattern = "/", replacement = "\\\\")

writeLines(ciag_1.1)
writeLines(ciag_1.2)

# # 2
ciag_2 <- "LiTeRy O rOzNeJ wIeLkOsCi"
zamiana <- c("A" = "a", "B" = "b", "C" = "c", "D" = "d", "E" = "e",
             "F" = "f", "G" = "g", "H" = "h", "I" = "i", "J" = "j", 
             "K" = "k", "L" = "l", "M" = "m", "N" = "n", "O" = "o", 
             "P" = "p", "Q" = "q", "R" = "r", "S" = "s", "T" = "t", 
             "U" = "u", "V" = "v", "W" = "w", "X" = "x", "Y" = "y", 
             "Z" = "z")

str_replace_all(string = ciag_2, pattern = zamiana) == str_to_lower(string = ciag_2)

# # 3
str_replace_all(string = words, pattern = c("^(.)(.*)(.)$" = "\\3\\2\\1"))



# # #   Exercises - Page 217   # # #
# # 1
str_split(string = "apples, pears, and bananas", pattern = "[^A-Za-z]+")

# # 2
why_better <- "This  is    an example  why"
str_split(string = why_better, pattern = " ")
str_split(string = why_better, pattern = boundary("word"))
str_split(string = why_better, pattern = "(\\s)+") # Good alternative

# # 3
str_split(string = "This sentence is an example", pattern = "")



# # #   Exercises - Page 221   # # #
# # 1
str_detect(string = c("a\\b\\c\\\\d","ab","a\\b"), pattern = regex("\\\\"))
str_detect(string = c("a\\b\\c\\\\d","ab","a\\b"), pattern = fixed("\\"))

# # 2
slowa <- str_extract_all(string = sentences, pattern = "[A-Za-z]+", simplify = T)
wektor_slowa <- as.vector(slowa)
wektor_slowa <- str_to_lower(string = wektor_slowa)

tibble(Slowo = wektor_slowa) %>%
  mutate(Ilosc_liter = str_count(string = Slowo, pattern = "")) %>%
  filter(Ilosc_liter > 1) %>%
  group_by(Slowo) %>%
  summarise(Ilosc_slow = n()) %>%
  arrange(desc(Ilosc_slow)) %>%
  head(n = 5)

  
  
# # #   Exercises - Page 222   # # #
# # 1
# a)
stri_count_boundaries(str = sentences)

# b)
stri_duplicated(str = c(c("First", "Second", "Third", "First", "Fourth"),
                        c("Pierwszy", "Drugi", "Trzeci", "Pierwszy", "First")))

# c)
stri_rand_strings(n = 5, length = 5)

# # 2
# locale argument in str_sort() controls language that it uses for sorting