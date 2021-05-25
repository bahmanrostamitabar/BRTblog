## ---- point -------------
confirmed_raw <- readxl::read_excel("Nottinghamshire_Cases.xlsx")
nhs111_raw <- readxl::read_excel("Nottinghamshire_111Calls.xlsx")
nhs111_date <- nhs111_raw %>% mutate(date=as_date(date)) %>% force_tz("GB")
confirmed <- confirmed_raw %>% mutate(date=as_date(date)) %>% force_tz("GB")
# join two dataset, ready for analysis
covid19 <- full_join(nhs111_date,confirmed, by="date")
#function
jetlag <- function(data, variable, n=10){
  variable <- enquo(variable)
  
  indices <- seq_len(n)
  quosures <- map( indices, ~quo(lag(!!variable, !!.x)) ) %>% 
    set_names(sprintf("lag_nhs111_%02d", indices))
  
  mutate( data, !!!quosures )
  
}
jetlag1 <- function(data, variable, n=10){
  variable <- enquo(variable)
  
  indices <- seq_len(n)
  quosures <- map( indices, ~quo(lag(!!variable, !!.x)) ) %>% 
    set_names(sprintf("lag_fcst_nhs111_%02d", indices))
  
  mutate( data, !!!quosures )
}

covid19 %>%
  mutate(Weekend=wday(date, label = TRUE) %in% c("Sat","Sun")) %>%
  mutate(Weekend= factor(case_when(Weekend==TRUE ~ 1, TRUE ~ 0)))-> data_covid19

data_covid19 %>% jetlag(nhs111_call,40) %>% select(-c(5:8)) %>%
  mutate(lag1_confirmed_cases=lag(confirmed_cases),lag2_confirmed_cases=lag(confirmed_cases,2),
         lag3_confirmed_cases=lag(confirmed_cases,3),lag4_confirmed_cases=lag(confirmed_cases,4),
         lag5_confirmed_cases=lag(confirmed_cases,5),lag6_confirmed_cases=lag(confirmed_cases,6),
         lag7_confirmed_cases=lag(confirmed_cases,7),lag8_confirmed_cases=lag(confirmed_cases,8),
         lag9_confirmed_cases=lag(confirmed_cases,9),lag10_confirmed_cases=lag(confirmed_cases,10),
         lag11_confirmed_cases=lag(confirmed_cases,11),lag12_confirmed_cases=lag(confirmed_cases,12),
         lag13_confirmed_cases=lag(confirmed_cases,13),lag14_confirmed_cases=lag(confirmed_cases,14)) %>% 
  select(confirmed_cases,lag1_confirmed_cases,date,nhs111_call,Weekend,everything())->data_with_lags

data_for_forecast_final <- data_with_lags %>% 
  select(date,nhs111_call,lag1_confirmed_cases:lag14_confirmed_cases,confirmed_cases,lag_nhs111_05:lag_nhs111_37, Weekend) %>% 
  drop_na()

#cross validation

f_horizon <- 21
n_init <- nrow(filter(data_for_forecast_final, date <= lubridate::ymd("2020-08-18")))
ae_tscv <- data_for_forecast_final %>% as_tsibble(index = date) %>% slice(1:(n()-f_horizon)) %>% 
  stretch_tsibble(.init = n_init, .step = 1 )
ae_test <- data_for_forecast_final %>% as_tsibble(index = date) %>% filter(date > lubridate::ymd("2020-08-18")) %>% 
  slide_tsibble(.size = f_horizon, .step = 1)

fcs_ets_nhs111 <- ae_tscv %>% model(ets=ETS(nhs111_call),arima=ARIMA(nhs111_call)) %>% 
  mutate(mix=(ets+arima)/2) %>% 
  forecast(h=f_horizon)

bind_cols(ae_test,fcs_ets_nhs111 %>% 
            filter(.model=="arima") %>% 
            as_tibble() %>% select(.mean)) %>% 
  mutate(nhs111_call=.mean) %>% select(-.mean)->ae_test_forecast

mutate_cond <- function(.data, condition, ..., envir = parent.frame()) {
  condition <- eval(substitute(condition), .data, envir)
  .data[condition, ] <- .data[condition, ] %>% mutate(...)
  .data
}

fit_ets_cases <- ae_tscv %>% model(ets=ETS(confirmed_cases),arima=ARIMA(confirmed_cases)) %>% 
  mutate(mix1=(ets+arima)/2)

  fcs_ets_cases <- fit_ets_cases %>% 
  forecast(h=f_horizon) %>% filter(.model=="ets")

bind_cols(ae_test_forecast,fcs_ets_cases %>%
            as_tibble() %>% select(.mean)) %>%
  mutate(cases=.mean) %>% select(-.mean)->ae_test_forecast1

ae_test_forecast1 %>% mutate(lag1_cases=lag(cases),lag2_cases=lag(cases,2),
                             lag3_cases=lag(cases,3),lag4_cases=lag(cases,4),
                             lag5_cases=lag(cases,5),lag6_cases=lag(cases,6),
                             lag7_cases=lag(cases,7),lag8_cases=lag(cases,8),
                             lag9_cases=lag(cases,9),lag10_cases=lag(cases,10),
                             lag11_cases=lag(cases,11),lag12_cases=lag(cases,12),
                             lag13_cases=lag(cases,13),lag14_cases=lag(cases,14))->ae_test_forecast_fct_lag

#---replace the short term estimation of confimed cases for lag 1-7 by fill
if (f_horizon < 8) {
  ae_test_forecast_fct_lag %>% group_by(.id) %>% mutate(h=row_number()) %>%
    mutate_cond(h < 6 &h > 1, lag1_confirmed_cases=NA) %>%
    mutate_cond(h > 6, lag1_confirmed_cases=lag1_cases) %>%
    mutate_cond(h < 6 & h > 2, lag2_confirmed_cases=NA) %>%
    mutate_cond(h > 6 , lag2_confirmed_cases=lag2_cases) %>%
    mutate_cond(h < 6 & h > 3, lag3_confirmed_cases=NA) %>%
    mutate_cond(h > 6, lag3_confirmed_cases=lag3_cases) %>%
    mutate_cond(h == 5, lag4_confirmed_cases=NA) %>%
    mutate_cond(h> 5, lag5_confirmed_cases=lag5_cases) %>%
    mutate_cond(h> 6, lag6_confirmed_cases=lag6_cases) %>%
    ungroup() %>% select(-h)-> ae_test_1_filled
} else{
  ae_test_forecast_fct_lag %>% group_by(.id) %>% mutate(h=row_number()) %>%
    mutate_cond(h < 6 &h > 1, lag1_confirmed_cases=NA) %>%
    mutate_cond(h > 6, lag1_confirmed_cases=lag1_cases) %>%
    mutate_cond(h < 6 & h > 2, lag2_confirmed_cases=NA) %>%
    mutate_cond(h > 6 , lag2_confirmed_cases=lag2_cases) %>%
    mutate_cond(h < 6 & h > 3, lag3_confirmed_cases=NA) %>%
    mutate_cond(h > 6, lag3_confirmed_cases=lag3_cases) %>%
    mutate_cond(h == 5, lag4_confirmed_cases=NA) %>%
    mutate_cond(h> 5, lag5_confirmed_cases=lag5_cases) %>%
    mutate_cond(h> 6, lag6_confirmed_cases=lag6_cases) %>%
    mutate_cond(h > 7, lag7_confirmed_cases=lag7_cases) %>%
    mutate_cond(h > 8, lag8_confirmed_cases=lag8_cases) %>%
    mutate_cond(h > 9, lag9_confirmed_cases=lag9_cases) %>%
    mutate_cond(h > 10, lag10_confirmed_cases=lag10_cases) %>%
    mutate_cond(h > 11, lag11_confirmed_cases=lag11_cases) %>%
    mutate_cond(h > 12, lag12_confirmed_cases=lag12_cases) %>%
    mutate_cond(h > 13, lag13_confirmed_cases=lag13_cases) %>%
    mutate_cond(h > 14, lag14_confirmed_cases=lag14_cases) %>%
    ungroup() %>% select(-h)-> ae_test_1_filled
}

ae_test_1_filled %>% fill(lag1_confirmed_cases,lag2_confirmed_cases,lag3_confirmed_cases,lag4_confirmed_cases,
                          lag5_confirmed_cases,lag6_confirmed_cases)->ae_test_1_filled

ae_test_1_filled %>% select(-c(lag1_cases:lag14_cases))->ae_test_1_filled
ae_test_1_filled %>% jetlag1(nhs111_call,40) -> ae_test_1_filled_2

#replace lagnhs111 with na
if (f_horizon == 7) {
ae_test_1_nhs111 <- ae_test_1_filled_2
}
if (f_horizon == 14) {
  ae_test_1_filled_2 %>% group_by(.id) %>% mutate(h=row_number()) %>% 
    mutate_cond(h > 7, lag_nhs111_07=lag_fcst_nhs111_07) %>%
    mutate_cond(h > 8, lag_nhs111_08=lag_fcst_nhs111_08) %>%
    mutate_cond(h > 9, lag_nhs111_09=lag_fcst_nhs111_09) %>%
    mutate_cond(h > 10, lag_nhs111_10=lag_fcst_nhs111_10) %>%
    mutate_cond(h > 11, lag_nhs111_11=lag_fcst_nhs111_11) %>% 
    mutate_cond(h > 12, lag_nhs111_12=lag_fcst_nhs111_12) %>%
    mutate_cond(h > 13, lag_nhs111_13=lag_fcst_nhs111_13) %>% 
    ungroup() %>% select(-h)-> ae_test_1_nhs111
}

if (f_horizon == 21) {
  ae_test_1_filled_2 %>% group_by(.id) %>% mutate(h=row_number()) %>% 
    mutate_cond(h > 7, lag_nhs111_07=lag_fcst_nhs111_07) %>%
    mutate_cond(h > 8, lag_nhs111_08=lag_fcst_nhs111_08) %>%
    mutate_cond(h > 9, lag_nhs111_09=lag_fcst_nhs111_09) %>%
    mutate_cond(h > 10, lag_nhs111_10=lag_fcst_nhs111_10) %>%
    mutate_cond(h > 11, lag_nhs111_11=lag_fcst_nhs111_11) %>% 
    mutate_cond(h > 12, lag_nhs111_12=lag_fcst_nhs111_12) %>%
    mutate_cond(h > 13, lag_nhs111_13=lag_fcst_nhs111_13) %>%
    mutate_cond(h > 14, lag_nhs111_14=lag_fcst_nhs111_14) %>%
    mutate_cond(h > 15, lag_nhs111_15=lag_fcst_nhs111_15) %>%
    mutate_cond(h > 16, lag_nhs111_16=lag_fcst_nhs111_16) %>%
    mutate_cond(h > 17, lag_nhs111_17=lag_fcst_nhs111_17) %>%
    mutate_cond(h > 18, lag_nhs111_18=lag_fcst_nhs111_18) %>%
    mutate_cond(h > 19, lag_nhs111_19=lag_fcst_nhs111_19) %>% 
    mutate_cond(h > 20, lag_nhs111_20=lag_fcst_nhs111_20) %>% 
    ungroup() %>% select(-h)-> ae_test_1_nhs111
}

ae_test_1_nhs111 %>% select(-c(lag_fcst_nhs111_01:lag_fcst_nhs111_40)) %>% 
  as_tsibble(index = date,key=.id) ->ae_test_modified

fit <- ae_tscv %>% 
  model(ETS=ETS(log(confirmed_cases)), 
        ARIMA=ARIMA(log(confirmed_cases)),
        SNaive=SNAIVE(log(confirmed_cases)),
        Prophet=fable.prophet::prophet(log(confirmed_cases)),
        MLR_W=TSLM(log(confirmed_cases) ~ Weekend+
                   trend()+
                   log(lag1_confirmed_cases)+
                   log(lag4_confirmed_cases)+
                   log(lag8_confirmed_cases)+
                   log(lag13_confirmed_cases)),
        MLR_T=TSLM(log(confirmed_cases) ~ Weekend+
                     trend()+nhs111_call+
                     log(lag1_confirmed_cases)+
                     log(lag4_confirmed_cases)+
                     log(lag8_confirmed_cases)+
                     log(lag13_confirmed_cases)+
                     lag_nhs111_08+lag_nhs111_09+
                     lag_nhs111_12+lag_nhs111_11+
                     lag_nhs111_15+lag_nhs111_16+lag_nhs111_17+
                     lag_nhs111_18+lag_nhs111_19+lag_nhs111_21+
                     lag_nhs111_22+
                     lag_nhs111_24+lag_nhs111_25+ 
                     lag_nhs111_27+lag_nhs111_29))


# fit <- (ae_tscv %>% filter(.id==1) %>% select(date, confirmed_cases)) %>%
#   model(MLR=TSLM(log(confirmed_cases) ~ trend()),
#         MLR_step=TSLM(log(confirmed_cases) ~ trend(knots =c(as_date("2020-06-29")))
# 
#   ))
# fit %>% forecast(h=21)->fcst
# 
# (ae_tscv %>% filter(.id==1) %>% select(date, confirmed_cases)) %>%
#   autoplot(confirmed_cases) +
#   geom_line(aes(y=.fitted, colour=.model), data = fitted(fit)) +
#   autolayer(fcst, alpha = 0.5, level = 95)

fit %>% forecast(new_data=ae_test_modified)->fcst
fcst %>% group_by(.id,.model) %>% 
  mutate(h=row_number()) %>% ungroup()->fcst_h

#to generate an example
ae_tscv %>% as_tibble() %>% select(.id) %>% slice(n()) %>% unlist()->last_id

ae_tscv %>% filter(.id==last_id) %>%  model(MLR=TSLM(log(confirmed_cases) ~ Weekend+trend()+nhs111_call+
                                                  log(lag1_confirmed_cases)+
                                                  log(lag4_confirmed_cases)+
                                                  log(lag8_confirmed_cases)+
                                                  log(lag13_confirmed_cases)+
                                                  lag_nhs111_08+lag_nhs111_09+
                                                  lag_nhs111_12+lag_nhs111_11+
                                                  lag_nhs111_15+lag_nhs111_16+lag_nhs111_17+
                                                  lag_nhs111_18+lag_nhs111_19+lag_nhs111_21+
                                                  lag_nhs111_22+
                                                  lag_nhs111_24+lag_nhs111_25+
                                                  lag_nhs111_27+lag_nhs111_29))->fit_sim
fit_sim %>% forecast(ae_test_modified %>% filter(.id==last_id)) -> sim
example_fcst <- sim %>% as_tibble() %>%
  group_by(date) %>%
  summarise(qs = quantile(confirmed_cases, seq(.001,.999,.001)), prob = seq(.001,.999,.001)) %>% ungroup()

data1 <-  bind_rows(ae_tscv %>% filter(.id==last_id) %>% select(date,confirmed_cases) %>%
                      slice((n()-20):n()),(ae_test_modified %>% filter(.id==last_id) %>%
                                             select(date,confirmed_cases)))
data2 <- (ae_test_modified %>% filter(.id==last_id) %>%
            select(date,confirmed_cases))
#to generate an example end

fcst %>% accuracy((data_for_forecast_final %>%
                     as_tsibble(index = date)),
                  measures = list(point_accuracy_measures,interval_accuracy_measures, distribution_accuracy_measures)) ->fcst_accuracy

fcst_accuracy %>%
  mutate(Winkler=winkler, Model=.model, Percentile=percentile) %>% 
  select(Model,ME, RMSE,MAE,Winkler,Percentile, CRPS ) %>% arrange(desc(RMSE)) %>% 
  kableExtra::kbl(caption = "Forecast performance evaluation", booktabs = T,digits =2) %>%
  add_header_above(c(" ", "Accuracy measture" = 6)) %>%
  kable_styling(position = "center") %>%
  row_spec(6, bold = T) %>% 
  column_spec(1,bold = F)

## ---- pointh --------
fcst_accuracy_h <- fcst_h %>% accuracy((data_for_forecast_final %>%
                                          as_tsibble(index = date)),
                                       measures = list(point_accuracy_measures,interval_accuracy_measures, distribution_accuracy_measures),
                                       by = c(".model","h"))

p1 <- fcst_accuracy_h %>% select(.model,h,RMSE) %>% 
  ggplot(aes(x=h, y=RMSE, colour=.model))+
  geom_line()+
  geom_point(aes(shape=.model),size=1)+
  ggthemes::scale_color_colorblind()+
  labs(x=NULL, y = "RMSE")+
  scale_x_continuous(breaks= c(1,7,14,21), labels= c(1,7,14,21))+
  theme_few()+
  theme(axis.text.x = element_text(size = 9),
        axis.text.y = element_text(size = 9),  
        axis.title.x = element_text(size = 9),
        axis.title.y = element_text(size = 9),
        legend.text =  element_text(size = 9),
        legend.title = element_text(size = 9))+
  guides(colour = guide_legend("Model"),
         shape = guide_legend("Model"))+
  theme(legend.position = "right", plot.margin = margin(b = 0, unit = "pt"))


p2 <- fcst_accuracy_h %>% select(.model,h,MAE) %>% 
  ggplot(aes(x=h, y=MAE, colour=.model))+
  geom_line()+
  geom_point(aes(shape=.model),size=1)+
  ggthemes::scale_color_colorblind()+
  labs(x=NULL, y = "MAE")+
  scale_x_continuous(breaks= c(1,7,14,21), labels= c(1,7,14,21))+
  theme_few()+
  theme(axis.text.x = element_text(size = 9),
        axis.text.y = element_text(size = 9),  
        axis.title.x = element_text(size = 9),
        axis.title.y = element_text(size = 9),
        legend.text =  element_text(size = 9),
        legend.title = element_text(size = 9))+
  guides(colour = guide_legend("Model"),
         shape = guide_legend("Model"))+
  theme(legend.position = "right",plot.margin = margin(b = 0, unit = "pt"))



p22 <- fcst_accuracy_h %>% select(.model,h,ME) %>% 
  ggplot(aes(x=h, y=ME, colour=.model))+
  geom_line()+
  geom_point(aes(shape=.model),size=1)+
  ggthemes::scale_color_colorblind()+
  labs(x="Forecast horizon", y = "ME")+
  scale_x_continuous(breaks= c(1,7,14,21), labels= c(1,7,14,21))+
  theme_few()+
  theme(axis.text.x = element_text(size = 9),
        axis.text.y = element_text(size = 9),  
        axis.title.x = element_text(size = 9),
        axis.title.y = element_text(size = 9),
        legend.text =  element_text(size = 9),
        legend.title = element_text(size = 9))+
  guides(colour = guide_legend("Model"),
         shape = guide_legend("Model"))+
  theme(legend.position = "right", plot.margin = margin(b = 0, unit = "pt"))

p_all <- (p1 / p2 / p22) +  plot_annotation(
  tag_levels = 'A',
)+plot_layout(guides = "collect")&
  theme(legend.position = "right", plot.tag = element_text(size = 9), plot.title = element_text(size = 9))

p_all

## ---- uncertainty ---------------

p3 <- fcst_accuracy_h %>% select(.model,h,winkler) %>% 
  ggplot(aes(x=h, y=winkler, colour=.model))+
  geom_line()+
  geom_point(aes(shape=.model),size=1)+
  ggthemes::scale_color_colorblind()+
  labs(x=NULL, y = "Winkler score")+
  scale_x_continuous(breaks= c(1,7,14,21), labels= c(1,7,14,21))+
  theme_few()+
  theme(axis.text.x = element_text(size = 9),
        axis.text.y = element_text(size = 9),  
        axis.title.x = element_text(size = 9),
        axis.title.y = element_text(size = 9),
        legend.text =  element_text(size = 9),
        legend.title = element_text(size = 9))+
  guides(colour = guide_legend("Model"),
         shape = guide_legend("Model"))+
  theme(legend.position = "right",plot.margin = margin(b = 0, unit = "pt"))


p4 <- fcst_accuracy_h %>% select(.model,h,percentile) %>% 
  ggplot(aes(x=h, y=percentile, colour=.model))+
  geom_line()+
  geom_point(aes(shape=.model),size=1)+
  ggthemes::scale_color_colorblind()+
  labs(x=NULL, y = "Percentile score")+
  scale_x_continuous(breaks= c(1,7,14,21), labels= c(1,7,14,21))+
  theme_few()+
  theme(axis.text.x = element_text(size = 9),
        axis.text.y = element_text(size = 9),  
        axis.title.x = element_text(size = 9),
        axis.title.y = element_text(size = 9),
        legend.text =  element_text(size = 9),
        legend.title = element_text(size = 9))+
  guides(colour = guide_legend("Model"),
         shape = guide_legend("Model"))+
  theme(legend.position = "right",plot.margin = margin(b = 0, unit = "pt"))

p5 <- fcst_accuracy_h %>% select(.model,h,CRPS) %>% 
  ggplot(aes(x=h, y=CRPS, colour=.model))+
  geom_line()+
  geom_point(aes(shape=.model),size=1)+
  ggthemes::scale_color_colorblind()+
  labs(x="Forecast horizon", y = "CRPS")+
  scale_x_continuous(breaks= c(1,7,14,21), labels= c(1,7,14,21))+
  theme_few()+
  theme(axis.text.x = element_text(size = 9),
        axis.text.y = element_text(size = 9),  
        axis.title.x = element_text(size = 9),
        axis.title.y = element_text(size = 9),
        legend.text =  element_text(size = 9),
        legend.title = element_text(size = 9))+
  guides(colour = guide_legend("Model"),
         shape = guide_legend("Model"))+
  theme(legend.position = "right",plot.margin = margin(b = 0, unit = "pt"))

(p3 / p4 / p5)+
  plot_layout(guides = "collect")+
  plot_annotation(
    tag_levels = 'A',
  )&theme(legend.position = "right",plot.tag = element_text(size = 9), plot.title = element_text(size = 9))
# 
# # ## ---- samplefcst ---------------
# p_example <- example_fcst %>%
#   ggplot(aes(x=date,y=qs)) +
#   ggfan::geom_fan() + scale_fill_distiller() + ggfan::geom_interval()+
#   geom_line(data=data1,aes(x=date, y=confirmed_cases))+
#   geom_line(data= data2,aes(x=date, y=confirmed_cases,colour="Actual"), shape=22)+
#   scale_color_manual(name="",values=("Actual"="#E69F00"))+
#   geom_point(data= data2,aes(x=date, y=confirmed_cases),size=1,colour="#E69F00", fill="#E69F00",shape=22)+
#   scale_x_date(breaks = "1 week", date_labels = "%b %d")+
#   labs(x="Date", y= "Confirmed cases")+
#   theme_few()+
#   theme(axis.text.x = element_text(angle = 90,size = 9),
#         axis.text.y = element_text(size = 9),
#         axis.title.x = element_text(size = 9),
#         axis.title.y = element_text(size = 9),
#         legend.text =  element_text(size = 9),
#         legend.title = element_text(size = 9),
#         legend.position = "bottom")
# # p_example
# ggsave("p_example_fct.pdf",p_example, units = "in",dpi = 1200,width=6,height=4)
# 
