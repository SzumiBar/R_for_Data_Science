# # #   Exercises - Page 128   # # #
# # 1
library(readr)
read_delim(file = "...", delim = "|")

# # 2
intersect(names(formals(read_csv)), names(formals(read_tsv)))

# # 3
read_fwf(file = "...", col_position = fwf_empty(file, skip, n = guess_max))

# # 4
read_delim(file = "x,y\n1,'a,b'", delim = ",", quote = "'")

# # 5
read_csv("a,b\n1,2,3\n4,5,6")
read_csv("a,b,c\n1,2\n1,2,3,4")
read_csv("a,b\n\"1")
read_csv("a,b\n1,2\na,b")
read_csv("a;b\n1;3")



# # #   Exercises - Page 136   # # #
# # 1
locale(date_names = "en",
       date_format = "%Y%m%d",
       time_format = "%H%M%S",
       decimal_mark = ".",
       grouping_mark = ",",
       encoding = "UTF-8")

# # 2
options(digits = 7)
parse_number(x = "123,456.789", locale = locale(grouping_mark = ",", decimal_mark = ",")) # Error

parse_number(x = "123,456.789", locale = locale(grouping_mark = ".")) # Decimal mark by default changes to ","

parse_number(x = "123.456,789", locale = locale(decimal_mark = ",")) # Grouping mark be default changes to "."

# # 3
locale_custom <- locale(date_format = "Dzien %d Miesiac %m Rok %y",
                        time_format = "Sekunda %S Minuta %M Godzina %H")
date_custom <- c("Dzien 01 Miesiac 02 Rok 03", "Dzien 03 Miesiac 01 Rok 01")
time_custom <- c("Sekunda 01 Minuta 02 Godzina 03", "Sekunda 03 Minuta 02 Godzina 01")

parse_date(x = date_custom, locale = locale_custom)
parse_time(x = time_custom, locale = locale_custom)

# # 4
date_names_langs() # pl
my_locale <- locale(date_names = "pl",
                    date_format = "%d%m%Y",
                    time_format = "%S%M%H")

# # 5
read_csv() # Delimiter = ","
read_csv2() # Delimiter = ";"

# # 6
# UTF-8

# # 7
d1 <- "January 1, 2010"
d2 <- "2015-Mar-07"
d3 <- "06-Jun-2017"
d4 <- c("August 19 (2015)", "July 1 (2015)")
d5 <- "12/30/14"
t1 <- "1705"
t2 <- "11:15:10.12 PM"

parse_date(x = d1, format = "%B %d, %Y")
parse_date(x = d2, format = "%Y-%b-%d")
parse_date(x = d3, format = "%d-%b-%Y")
parse_date(x = d4, format = "%B %d (%Y)")
parse_date(x = d5, format = "%m/%d/%y")
parse_time(x = t1, format = "%H%M")
parse_time(x = t2, format = "%H:%M:%OS %p")