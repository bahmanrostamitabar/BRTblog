library(fpp3)
library(tidyverse)
library(lubridate)
ae_original <- readr::read_csv("data/ae_uk.csv",
                                  col_types = cols(
                                    arrival_time=col_datetime(format = "%d/%m/%Y %H:%M"),
                                    gender=col_character(),
                                    type_injury=col_character()))
ae_original %>%
  mutate(arrival_time=lubridate::force_tz(arrival_time,tz="GB")) -> ae
ae_tsb <- ae %>%
  as_tsibble(key = c(ID,gender,type_injury), index = arrival_time, regular=FALSE)

ae_half_hourly <- ae_tsb %>%
  index_by(time = lubridate::floor_date(arrival_time, "30 minutes")) %>%
  summarise(admission=n()) %>% fill_gaps(admission=0L) %>% ungroup()


admissions <- readr::read_csv("NHSR-WEBINAR-MSC/data/ae_uk.csv",
                              col_types = cols(
                                arrival_time=col_datetime(format = "%d/%m/%Y %H:%M"),
                                gender=col_character(),
                                type_injury=col_character()))%>%
  mutate(arrival_time=lubridate::force_tz(arrival_time,tz="GB")) %>% 
  select(-ID) # ID is a redundant row identifier 
print(admissions)

holidays <- read_csv("NHSR-WEBINAR-MSC/data/holiday.csv",
  col_types = cols(date=col_date(format = "%d/%m/%Y"))
) %>% 
  filter(!is.na(date)) %>%# Some extra empty rows exist that need removing %>% 
mutate(date=lubridate::force_tz(date,tz="GB"))

print(holidays)

temperatures <- read_csv("NHSR-WEBINAR-MSC/data/temp.csv",
  col_types = cols(date=col_date(format = "%d/%m/%Y"))) %>% 
    mutate(date=lubridate::force_tz(date,tz="GB"))
print(temperatures)

temperatures <- as_tsibble(temperatures, 
                           index = date, key = NULL)
holidays <- as_tsibble(holidays, 
                       index = date, key = NULL)
admissions <- as_tsibble(admissions, 
                         index = arrival_time, key = c(gender, type_injury))
duplicates(admissions, index = arrival_time, key = c(gender, type_injury))

admissions <- admissions %>% 
  count(gender, type_injury, arrival_time, name = "arrivals") %>% #<<
  as_tsibble(index = arrival_time, key = c(gender, type_injury))

interval(admissions)
interval(holidays)
interval(temperatures)

admissions %>% 
  filter(yearmonth(arrival_time) == yearmonth("2010 Jan")) %>% 
  summarise(arrivals = sum(arrivals)) %>% #<<
  fill_gaps(arrivals = 0) %>% #<<
  autoplot()

admissions %>% 
  filter(yearmonth(arrival_time) == yearmonth("2010 Jan")) %>% 
  index_by(time = floor_date(arrival_time, "30 minutes")) %>% #<<
  summarise(arrivals = sum(arrivals)) %>% 
  fill_gaps(arrivals = 0) %>% 
  autoplot()

admissions %>% 
  filter(yearmonth(arrival_time) == yearmonth("2010 Jan")) %>% 
  index_by(time = floor_date(arrival_time, "1 hour")) %>% #<<
  summarise(arrivals = sum(arrivals)) %>% 
  fill_gaps(arrivals = 0) %>% 
  autoplot()

admissions %>% 
  filter(year(arrival_time) == 2010) %>% 
  index_by(time = as.Date(floor_date(arrival_time, "1 day"))) %>% #<< 
  summarise(arrivals = sum(arrivals)) %>% 
  fill_gaps(arrivals = 0) %>% 
  autoplot()

admissions %>% 
  index_by(time = yearmonth(floor_date(arrival_time, "1 day"))) %>% #<<
  summarise(arrivals = sum(arrivals)) %>% 
  fill_gaps(arrivals = 0) %>%
  autoplot()

admissions <- admissions %>% 
  index_by(time = floor_date(arrival_time, "1 hour")) %>%
  summarise(arrivals = sum(arrivals)) %>% 
  ungroup() %>% 
  fill_gaps(arrivals = 0)

hospital <- admissions %>% 
  # Compute the common variable
  mutate(date = as.Date(time)) %>% 
  # Join in holiday data (keeping only 'public_holiday' for simplicity)
  left_join(holidays %>% select(public_holiday), by = "date") %>% 
  # Join in temperature data (keeping only 'actual_temp' for simplicity)
  left_join(temperatures %>% select(actual_temp), by = "date")

#Visualising multiple seasonality
hospital %>% 
  autoplot(arrivals)

library(dygraphs)
tsbox::ts_xts(hospital) %>% 
  dygraph() %>% 
  dyRangeSelector(dateWindow = c("2010-01-01", "2010-01-07"))

library(feasts)
hospital %>% 
  index_by(date = yearmonth(time)) %>% 
  summarise(arrivals = sum(arrivals)) %>% 
  gg_subseries(arrivals, period = "year")

hospital %>% 
  index_by(date = as.Date(time)) %>% 
  summarise(arrivals = sum(arrivals)) %>% 
  gg_subseries(arrivals, period = "week")

hospital %>% 
  gg_subseries(arrivals, period = "day")

hospital %>% 
  ggplot(aes(x = hour(time), y = arrivals)) + 
  geom_jitter(alpha = 0.01) +
  geom_smooth() + 
  facet_grid(cols = vars(wday(time, label = TRUE, week_start = 1)), scales = "free")

library(sugrrants)
p <- hospital %>% 
  mutate(date = as.Date(time)) %>% 
  filter(year(time) == 2012) %>% 
  frame_calendar(x = hour(time), y = arrivals, date = date) %>%
  ggplot(aes(x = `.hour(time)`, y = .arrivals, group = date)) + 
  geom_line()
prettify(p)

#forecast

library(fable.prophet)
hospital %>% 
  model(prophet(arrivals)) %>% 
  forecast(h = "2 weeks") %>% 
  autoplot(tail(hospital, 24*7*4))

hospital %>% 
  model(prophet(arrivals ~ season("day", 7, type = "mult") + season("week", 4))) %>% 
  forecast(h = "2 weeks") %>% 
  autoplot(tail(hospital, 24*7*4))

library(fasster)
ped_train <- pedestrian %>% 
  filter(Sensor == "Southern Cross Station",
         yearmonth(Date) == yearmonth("2015 July"))

```{r fasster-seasonality}
tibble::tribble(
  ~ `Seasonal term`, ~ Special,
  "Seasonal dummies", "y ~ season(p)",
  "Lag terms", "y ~ lag(y, p)",
  "Fourier terms", "y ~ fourier(p, K)",
  "Regressors", "y ~ x"
) %>% 
  gt::gt()
```

ped_train %>% 
  model(fasster(Count ~ trend(1) + fourier("week", 3) + (wday(Date_Time) %in% c(1,7)) %S% fourier("day", 10))) %>% 
  forecast(h = "2 weeks") %>% 
  autoplot(ped_train)

library(feasts)
hospital_dcmp <- hospital %>% 
  model(STL(arrivals)) 
components(hospital_dcmp) %>% autoplot()

components(hospital_dcmp) %>% 
  tail(24*7*5) %>% 
  autoplot()

hospital_dcmp <- hospital %>% 
  model(STL(arrivals ~ trend(window = 24*7*5) + season("year", window = Inf) +
              season("week", window = 24*7*5) + season("day", window = 24*7)))
components(hospital_dcmp) %>% autoplot()
components(hospital_dcmp) %>% 
  tail(24*7*5) %>% 
  autoplot()

components(hospital_dcmp)[-1] %>% 
  model(SNAIVE(season_week ~ lag("week"))) %>% 
  forecast(h = "2 weeks") %>% 
  autoplot(tail(components(hospital_dcmp)[-1], 24*7*4))

components(hospital_dcmp)[-1] %>% 
  model(SNAIVE(season_day ~ lag("day"))) %>% 
  forecast(h = "2 weeks") %>% 
  autoplot(tail(components(hospital_dcmp)[-1], 24*7*4))

components(hospital_dcmp)[-1] %>% 
  model(ARIMA(season_adjust ~ 0 + pdq(3,0,3) + PDQ(1,0,0))) %>% 
  forecast(h = "3 days") %>% 
  autoplot(tail(components(hospital_dcmp)[-1], 24*4))

dcmp_spec <- decomposition_model(
  dcmp = STL(arrivals ~ trend(window = 24*7*5) + season("year", window = Inf) +
               season("week", window = 24*7*5) + season("day", window = 24*7)),
  ARIMA(season_adjust ~ 0 + pdq(3,0,3) + PDQ(1,0,0)),
  SNAIVE(season_week ~ lag("week")),
  SNAIVE(season_day ~ lag("day"))
)

hospital %>% 
  model(dcmp_spec) %>% 
  forecast(h = "2 weeks") %>% 
  autoplot(tail(hospital, 24*7*4))

thetaf

nile.fcast <- thetaf(Nile)
plot(nile.fcast)