## ---- weekend -------------
confirmed_raw <- readxl::read_excel("Nottinghamshire_Cases.xlsx")
nhs111_raw <- readxl::read_excel("Nottinghamshire_111Calls.xlsx")
nhs111_date <- nhs111_raw %>% mutate(date=as_date(date)) %>% force_tz("GB")
confirmed <- confirmed_raw %>% mutate(date=as_date(date)) %>% force_tz("GB")
covid19 <- full_join(nhs111_date,confirmed, by="date")

covid19 %>%
  mutate(Weekend=wday(date, label = TRUE) %in% c("Sat","Sun")) %>%
 mutate(Weekend= factor(case_when(Weekend==TRUE ~ 1, TRUE ~ 0)))-> data_covid19

p <- data_covid19 %>% 
  ggplot(aes(x=date,y=confirmed_cases, colour=Weekend, group=1))+
  geom_line()+
  geom_point()+
  scale_color_colorblind()+
  scale_x_date(breaks = "10 days", date_labels = "%b %d")+
  theme_few()+
  theme(legend.position = "right")+
  labs(x= " Date", y="Confirmed case")+
  theme(axis.text.x = element_text(size = 9,angle = 90),
        axis.text.y = element_text(size = 9),
        axis.title.x = element_text(size = 9),
        axis.title.y = element_text(size = 9),
        legend.text =  element_text(size = 9),
        legend.title = element_text(size = 9))
p

## ---- casesoverlap -----

p_two <- data_covid19 %>% 
  ggplot(aes(x=date,y=confirmed_cases, colour=Weekend))+
  geom_line()+
  geom_point()+
  scale_color_colorblind()+
  scale_x_date(breaks = "10 days", date_labels = "%b %d")+
  theme_few()+
  theme(legend.position = "right")+
  labs(x= " Date", y="Confirmed case")+
  theme(axis.text.x = element_text(size = 9,angle = 90),
        axis.text.y = element_text(size = 9),
        axis.title.x = element_text(size = 9),
        axis.title.y = element_text(size = 9),
        legend.text =  element_text(size = 9),
        legend.title = element_text(size = 9))
p_two

## ---- casesoverlapless -----

filter(data_covid19, date < as.Date("2020-09-25")) %>% 
  ggplot(aes(x=date,y=confirmed_cases, colour=Weekend))+
  geom_line()+
  geom_point()+
  scale_color_colorblind()+
  scale_x_date(breaks = "10 days", date_labels = "%b %d")+
  theme_few()+
  theme(legend.position = "right")+
  labs(x= " Date", y="Confirmed case")+
  theme(axis.text.x = element_text(size = 9,angle = 90),
        axis.text.y = element_text(size = 9),
        axis.title.x = element_text(size = 9),
        axis.title.y = element_text(size = 9),
        legend.text =  element_text(size = 9),
        legend.title = element_text(size = 9))


## ---- NHS111 -----
pp <- data_covid19 %>% 
  ggplot(aes(x=date,y=nhs111_call, colour=Weekend, group=1))+
  geom_line()+
  geom_point()+
  scale_color_colorblind()+
  scale_x_date(breaks = "10 days", date_labels = "%b %d")+
  theme_few()+
  theme(legend.position = "right")+
  labs(x= " Date", y="NHS 111 call")+
  theme(axis.text.x = element_text(size = 9,angle = 90),
        axis.text.y = element_text(size = 9),
        axis.title.x = element_text(size = 9),
        axis.title.y = element_text(size = 9),
        legend.text =  element_text(size = 9),
        legend.title = element_text(size = 9))

pp

## ---- NHSCOVID -----

p1 <- data_covid19 %>% 
  ggplot(aes(x=date,y=confirmed_cases))+
  geom_line()+
  geom_point()+
  scale_color_colorblind()+
  scale_x_date(breaks = "10 days", date_labels = "%b %d")+
  theme_few()+
  theme(legend.position = "right")+
  labs(x= " Date", y="Confirmed case")+
  theme(axis.text.x = element_text(size = 9,angle = 90),
        axis.text.y = element_text(size = 9),
        axis.title.x = element_text(size = 9),
        axis.title.y = element_text(size = 9),
        legend.text =  element_text(size = 9),
        legend.title = element_text(size = 9))
## ---- acf --------
# p_acf <- data_covid19 %>% as_tsibble() %>% mutate(diff_conf=difference(confirmed_cases))%>%  ACF(diff_conf,lag_max = 21) %>% autoplot()+theme_few()
# p_pacf <- data_covid19 %>% as_tsibble() %>% mutate(diff_conf=difference(confirmed_cases)) %>% PACF(diff_conf,lag_max = 21) %>% autoplot()+theme_few()
# patchwork <- (p_acf | p_pacf)
# patchwork + plot_annotation(
#   tag_levels = 'A',
#   title = 'Autocorrelation and partial autocorrelation of confirmed cases',
# )

p_acf <- data_covid19 %>% as_tsibble() %>% 
  ACF(confirmed_cases,lag_max = 21) %>% autoplot()+theme_few()+labs(x="Lag", y="ACF")+
  theme(axis.text.x = element_text(size = 9),
        axis.text.y = element_text(size = 9),
        axis.title.x = element_text(size = 9),
        axis.title.y = element_text(size = 9),
        legend.text =  element_text(size = 9),
        legend.title = element_text(size = 9))
p_pacf <- data_covid19 %>% as_tsibble() %>% 
  PACF(confirmed_cases,lag_max = 21) %>% autoplot()+theme_few()+labs(x="Lag", y="PACF")+
  theme(axis.text.x = element_text(size = 9),
        axis.text.y = element_text(size = 9),
        axis.title.x = element_text(size = 9),
        axis.title.y = element_text(size = 9),
        legend.text =  element_text(size = 9),
        legend.title = element_text(size = 9))
patchwork <- (p_acf | p_pacf)
patchwork + plot_annotation(
  tag_levels = 'A',
)&theme(plot.tag = element_text(size = 8), plot.title = element_text(size = 8))

## ---- ccf ------

data_covid19 %>% as_tsibble() %>% filter_index(~"2020-09") %>% 
  CCF(nhs111_call,confirmed_cases,lag_max = 40) %>% 
  bind_cols(lag1=seq(-40,40,1)) %>% as_tibble() %>% 
  ggplot(aes(x=lag1, y=ccf))+
  geom_col(width =.1)+
  geom_hline(yintercept = .13, colour="blue", linetype=2)+
  geom_hline(yintercept = -.13, colour="blue", linetype=2)+
  geom_hline(yintercept = 0, colour="black")+
  theme_few()+
  scale_x_continuous(breaks=seq(-40,40,4),labels=seq(-40,40,4))+
  labs(x="Lag", y="CCF")+
  theme(axis.text.x = element_text(size = 9),
        axis.text.y = element_text(size = 9),
        axis.title.x = element_text(size = 9),
        axis.title.y = element_text(size = 9),
        legend.text =  element_text(size = 9),
        legend.title = element_text(size = 9))
