library(dplyr)
library(tidyr)
library(stringr)
library(ggplot2)
library(scales)
library(lubridate)
elec2010 <- ymd('2010-08-21')
elec2013 <- ymd('2013-09-07')
elec2016 <- ymd('2016-07-02')
elec2019 <- ymd('2019-05-18')
elec2022 <- ymd('2022-05-21')
pp2010 <- read.csv("https://www.aec.gov.au/Elections/Federal_Elections/2010/files/e2010-prepoll-stats-19-08.csv")
pp2013 <- read.csv("https://www.aec.gov.au/Elections/Federal_Elections/2013/files/statistics/e2013-prepoll-stats-07-09.csv")
pp2016 <- read.csv("https://www.aec.gov.au/Elections/Federal_Elections/2016/files/20160702_WEB_Prepoll_Report.csv")
pp2019 <- read.csv("https://www.aec.gov.au/Elections/federal_elections/2019/files/downloads/20190518_WEB_Pre-poll_Report_FE2019.csv")
pp2022 <- read.csv("https://www.aec.gov.au/election/files/downloads/20220518_WEB_Pre-poll_report_FE2022.csv")
pp2010[is.na(pp2010)] <- 0
pp2013[is.na(pp2013)] <- 0
pp2016[is.na(pp2016)] <- 0
pp2019[is.na(pp2019)] <- 0
pp2022[is.na(pp2022)] <- 0
dayscale <- data.frame(c(-19:-1))
names(dayscale) <- "daysuntil"
point <- format_format(big.mark = ",", scientific = FALSE)

# 2010 ------
pp2010 <- pp2010[-c(151,152), ]

dates10 <- names(pp2010)
dates10 <- dates10[3:20]

tidy10 <- pp2010 %>%
  gather(dates10, key = "date", value = "votes")

tidy10$date <- substring(tidy10$date,2,20)
tidy10$date <- dmy(tidy10$date)

totals10 <- tidy10 %>%
  group_by(date) %>%
  summarise(votesperday = sum(votes, na.rm = TRUE)) %>%
  mutate(daysuntil = as.integer(difftime(date, elec2010, units = "days"))) %>%
  merge(dayscale, all = TRUE) %>%
  mutate(year = as.factor(2010))

totals10 <- totals10[,-2]
totals10[is.na(totals10)] <- 0
totals10 <- mutate(totals10, totals = cumsum(votesperday))

# 2013 ------
dates13 <- names(pp2013)
dates13 <- dates13[4:19]

tidy13 <- pp2013 %>%
  gather(dates13, key = "date", value = "votes")

tidy13$date <- substring(tidy13$date,2,20)
tidy13$date <- dmy(tidy13$date)

totals13 <- tidy13 %>%
  group_by(date) %>%
  summarise(votesperday = sum(votes, na.rm = TRUE)) %>%
  mutate(daysuntil = as.integer(difftime(date, elec2013, units = "days"))) %>%
  merge(dayscale, all = TRUE) %>%
  mutate(totals = cumsum(votesperday)) %>%
  mutate(year = as.factor(2013))

totals13 <- totals13[,-2]
totals13[is.na(totals13)] <- 0
totals13 <- mutate(totals13, totals = cumsum(votesperday))

# 2016 ------
dates16 <- names(pp2016)
dates16 <- dates16[4:18]

tidy16 <- pp2016 %>%
  gather(dates16, key = "date", value = "votes")

tidy16$date <- substring(tidy16$date,2,20)
tidy16$date <- dmy(tidy16$date)

totals16 <- tidy16 %>%
  group_by(date) %>%
  summarise(votesperday = sum(votes, na.rm = TRUE)) %>%
  mutate(daysuntil = as.integer(difftime(date, elec2016, units = "days"))) %>%
  merge(dayscale, all = TRUE) %>%
  mutate(totals = cumsum(votesperday)) %>%
  mutate(year = as.factor(2016))

totals16 <- totals16[,-2]
totals16[is.na(totals16)] <- 0
totals16 <- mutate(totals16, totals = cumsum(votesperday))
  
# 2019 ------
dates19 <- names(pp2019)
dates19 <- dates19[5:length(dates19)]

tidy19 <- pp2019 %>%
  gather(dates19, key = "date", value = "votes")
  
tidy19$date <- substring(tidy19$date,2,20)
tidy19$date <- dmy(tidy19$date)

totals19 <- tidy19 %>%
  group_by(date) %>%
  summarise(votesperday = sum(votes, na.rm = TRUE)) %>%
  mutate(daysuntil = as.integer(difftime(date, elec2019, units = "days"))) %>%
  merge(dayscale, all = TRUE) %>%
  mutate(totals = cumsum(votesperday)) %>%
  mutate(year = as.factor(2019))

totals19 <- totals19[,-2]
totals19[is.na(totals19)] <- 0
totals19 <- mutate(totals19, totals = cumsum(votesperday))

# 2022 ------
dates22 <- names(pp2022)
dates22 <- dates22[5:length(dates22)]

tidy22 <- pp2022 %>%
  mutate(date = dmy(Issue.Date)) %>%
  mutate(votes = Total.Votes)

totals22 <- tidy22 %>%
  group_by(date) %>%
  summarise(votesperday = sum(votes, na.rm = TRUE)) %>%
  mutate(daysuntil = as.integer(difftime(date, elec2022, units = "days"))) %>%
  merge(dayscale, all = TRUE) %>%
  mutate(totals = cumsum(votesperday)) %>%
  mutate(year = as.factor(2022))

totals22 <- totals22[,-2]
totals22[is.na(totals22)] <- 0
daysleft <- as.integer(difftime(Sys.Date(), elec2022, units = "days"))
totals22 <- mutate(totals22, totals = cumsum(votesperday)) %>%
  filter(daysuntil < daysleft)

enrol <- read.csv("elector-count-fe-tidy-2022.csv")
tidyenrol <- gather(enrol, "age", "enrolled", 4:16)
stateenrol <- tidyenrol %>%
  filter(age == "total", gender == "Total") %>%
  group_by(state) %>%
  summarise(enrolment = sum(as.numeric(enrolled))) %>%
  select(state, enrolment) %>%
  arrange(desc(enrolment))
statetotals <- tidy19 %>%
  group_by(m_state_ab) %>%
  summarise(turnout = sum(votes)) %>%
  inner_join(stateenrol, c("m_state_ab" = "state")) %>%
  mutate(turnout.pc = round(turnout/enrolment*100,2)) %>%
  arrange(desc(turnout.pc))
statetotals
paste("Turnout:", round(sum(statetotals$turnout)/sum(statetotals$enrolment)*100,2),"%")

divtotals <- tidy19 %>%
  group_by(m_div_nm) %>%
  summarise(turnout = sum(votes))
divenrol <- tidyenrol %>%
  filter(age == "total", gender == "Total") %>%
  select(state, division, enrolled)
divenrol$enrolled <- as.numeric(divenrol$enrolled)
divtotals$m_div_nm <- as.character(divtotals$m_div_nm)
divenrol$division<- str_trim(as.character(divenrol$division),side = "right")
divenrol$division <- str_replace(divenrol$division, "Mcpherson", "McPherson")

divpercent <- divtotals %>% 
  inner_join(divenrol, c("m_div_nm" = "division")) %>%
  select(state, m_div_nm, enrolled, turnout) %>%
  mutate(percent = round(turnout/enrolled*100,2)) %>%
  arrange(desc(percent))

# Combine and plot ------
alldata <- bind_rows(totals10, totals13, totals16, totals19, totals22)
alldata$year <- as.factor(alldata$year)

ppv <- ggplot(alldata, aes(x = daysuntil, y = totals, colour = year)) +
  geom_line(size = 1) +
  geom_point(size = 2)
ppv + scale_y_continuous(labels = point) +
  labs(x = "Days before election", y = "Number of pre-poll votes", colour = "Election", title = "Pre-poll voting by days until election", subtitle = "2010–2022", caption = "Source: Australian Electoral Commission")