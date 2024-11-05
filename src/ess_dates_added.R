library(tidyverse)
library(foreign)
library(lubridate)

ESS9 <- read.spss("../sexcomp-leftright/data/ESS9e03_2.sav", to.data.frame = T, use.value.labels = F)
ESS10 <- read.spss("../sexcomp-leftright/data/ESS10.sav", to.data.frame = T, use.value.labels = F)
ESS11 <- read.spss("../sexcomp-leftright/data/ESS11.sav", to.data.frame = T, use.value.labels = F)
load("ess_election_dates.RData")

# Add missing data -------------------------------------------------------------
ess_election_dates$recent_election[ess_election_dates$cntry == "BG" & ess_election_dates$essround == 9] <- "2017-03-26"
ess_election_dates$recent_election[ess_election_dates$cntry == "EE" & ess_election_dates$essround == 5] <- "2015-03-01"
ess_election_dates$recent_election[ess_election_dates$cntry == "RS" & ess_election_dates$essround == 9] <- "2016-04-24"

dates_added <- data.frame(
  cntry = c("AT", "AT", "DK", "ES", "HR", "IS", "LT", "LV", "PT", "SE", "SK", 
            "BE", "BG", "CH", "CZ", "EE", "FI", "FR", "GB", "GR", "HR", "HU", "IE",
            "IS", "IT", "LT", "ME", "MK", "NL", "NO", "PT", "SI", "SK",
            "AT", "CH", "DE", "FI", "GB", "HR", "HU", "IE", "LT", "NL", "NO", "SI",
            "SK"),
  essround = c(4, 5, 9, 9, 9, 9, 9, 9, 9, 9, 9, rep(10, 22), rep(11, 13)),
  # manual imputation
  recent_election = as.Date(c("2008-09-28", "2008-09-28", "2015-06-18", "2019-11-10", 
                              "2016-09-11", "2017-10-28", "2016-10-09", "2018-10-06", 
                              "2015-10-04", "2018-09-09", "2016-03-05",
                              "2019-05-26", "2021-11-14", "2019-10-20", "2017-10-20",
                              "2019-03-03", "2019-04-14", "2017-04-23", "2019-12-12",
                              "2019-07-07", "2020-07-05", "2018-04-08", "2020-02-08",
                              "2021-03-23", "2018-03-04", "2020-10-11", "2020-08-30",
                              "2020-07-15", "2021-03-15", "2021-09-13", "2019-10-06",
                              "2018-06-03", "2020-02-29", "2019-09-29", "2019-10-20",
                              "2021-09-26", "2023-04-02", "2019-12-12", "2020-07-05",
                              "2018-04-08", "2020-02-08", "2020-10-11", "2021-03-15",
                              "2021-09-13", "2022-04-24", "2020-02-29")),
  split_wave = rep(NA, 46),
  recent_election_split1 = as.Date(rep(NA, 46)),
  max_days_since_election_split1 = rep(NA, 46)
)

field_date_9 <- ESS9 %>% 
  mutate(field_start = paste(inwyys, inwmms, inwdds, sep = "-"),
         field_end = paste(inwyye, inwmme, inwdde, sep = "-")) %>% 
  group_by(cntry) %>%
  summarise(field_start = min(field_start, na.rm = T),
            field_end = max(field_end, na.rm = T)) %>% 
  mutate(field_start = as.Date(field_start),
         field_end = as.Date(field_end),
         essround = 9)

ESS10$inwds <- as.POSIXct(ESS10$inwds, origin = "1582-10-14", tz = "UTC")
ESS10$inwde <- as.POSIXct(ESS10$inwde, origin = "1582-10-14", tz = "UTC")

field_date_10 <- ESS10 %>% 
  mutate(field_start = substr(inwds, start = 1, stop = 10),
         field_end = substr(inwde, start = 1, stop = 10)) %>% 
  group_by(cntry) %>%
  summarise(field_start = min(field_start, na.rm = T),
            field_end = max(field_end, na.rm = T)) %>% 
  mutate(field_start = as.Date(field_start),
         field_end = as.Date(field_end),
         essround = 10)

ESS11$inwds <- as.POSIXct(ESS11$inwds, origin = "1582-10-14", tz = "UTC")
ESS11$inwde <- as.POSIXct(ESS11$inwde, origin = "1582-10-14", tz = "UTC")

field_date_11 <- ESS11 %>% 
  mutate(field_start = substr(inwds, start = 1, stop = 10),
         field_end = substr(inwde, start = 1, stop = 10)) %>% 
  group_by(cntry) %>%
  summarise(field_start = min(field_start, na.rm = T),
            field_end = max(field_end, na.rm = T)) %>% 
  mutate(field_start = as.Date(field_start),
         field_end = as.Date(field_end),
         essround = 11)

field_date <- field_date_9 %>% 
  bind_rows(field_date_10) %>% 
  bind_rows(field_date_11)

dates_added <- dates_added %>% 
  left_join(field_date, by = c("cntry", "essround")) %>%
  mutate(max_days_since_election = round(difftime(field_end, recent_election, units = "days")))

ess_election_dates_added <- ess_election_dates %>% 
  bind_rows(dates_added) %>% 
  arrange(cntry, essround)
save(ess_election_dates_added, file = "ess_election_dates_added.RData")
write.csv(ess_election_dates_added, "ess_election_dates_added.csv")
