library(tidyverse)
library(forcats)

# # #   Exercises - Page 227   # # #
# # 1
View(gss_cat)
ggplot(data = gss_cat, mapping = aes(x = rincome)) +
  geom_bar() +
  coord_flip()

# # 2
count(x = gss_cat, relig) %>%
  arrange(desc(n)) %>% head(n = 1)
  
count(x = gss_cat, partyid) %>%
  arrange(desc(n)) %>% head(n = 1)

# # 3
select(.data = gss_cat, relig, denom) %>%
  filter(denom != "Not applicable" &
           denom != "Don't know" &
           denom != "No answer" &
           denom != "Other") %>%
  count(relig)

ggplot(data = gss_cat, mapping = aes(x = relig, y = denom)) +
  geom_point() +
  theme(axis.text.x = element_text(angle = 90))



# # #   Exercises - Page 232   # # #
# # 1
ggplot(data = gss_cat, mapping = aes(x = tvhours)) +
  geom_histogram(binwidth = 1)

gss_cat %>%
  filter(!is.na(tvhours) & tvhours < 12) %>%
  group_by(relig) %>%
  summarise(avr_hours = mean(tvhours)) %>%
  mutate(relig = fct_reorder(.f = relig, .x = avr_hours)) %>%
  ggplot(mapping = aes(x = avr_hours, y = relig)) +
  geom_point()
  
# # 2
glimpse(gss_cat)
levels(gss_cat$marital) # Arbitrary
levels(gss_cat$race) # Arbitrary
levels(gss_cat$rincome) # Principled
levels(gss_cat$partyid) # Arbitrary
levels(gss_cat$denom) # Arbitrary
levels(gss_cat$relig) # Arbitrary

# # 3
# Order of levels is interpreted in form of integers, so moving factor to first level
# gave it value of 1 and moved it to the bottom of y-axis on the plot



# # #   Exercises - Page 232   # # #
# # 1
unique(gss_cat$partyid)

gss_cat %>%
  mutate(partyid = fct_collapse(.f = partyid,
                                "Democrat" = c("Strong democrat", "Not str democrat"),
                                "Republican" = c("Strong republican", "Not str republican"),
                                "Independent" = c("Ind,near rep", "Independent", "Ind,near dem"),
                                "Other" = c("Other party", "No answer", "Don't know"))) %>%
  group_by(year) %>%
  mutate(yearsum = n()) %>%
  group_by(year, partyid) %>%
  mutate(partysum = n()) %>%
  select(year, partyid, partysum, yearsum) %>%
  distinct(year, partyid, .keep_all = T) %>%
  mutate(proportion = partysum / yearsum) %>%
  ggplot(mapping = aes(x = year, y = proportion, col = partyid)) +
  geom_line(lwd = 1.25) +
  labs(x = "Year", y = "Proportion", col = "Political affiliation",
       title = "Proportion of political system supporters\nchange over time") +
  theme(plot.title = element_text(hjust = 0.5))

# # 2
levels(gss_cat$rincome)
gss_cat %>%
  mutate(rincome = fct_collapse(.f = rincome,
                                "Other" = c("No answer", "Don't know", "Refused", "Not applicable"),
                                "0-4999$" = c("$4000 to 4999", "$3000 to 3999", "$1000 to 2999", "Lt $1000"),
                                "5000-9999$" = c("$8000 to 9999", "$7000 to 7999", "$6000 to 6999", "$5000 to 5999"),
                                "10000-19999$" = c("$15000 - 19999", "$10000 - 14999"),
                                "20000$+" = c("$25000 or more", "$20000 - 24999"))) %>%
  count(rincome) %>%
  ggplot(mapping = aes(x = rincome, y = n)) +
  geom_col() +
  coord_flip() +
  labs(y = "Count", x = "Reported income")