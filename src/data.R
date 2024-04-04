#Read in the data
source('https://raw.githubusercontent.com/zeyadissa/open_health_data/main/src/functions.R')
source('https://raw.githubusercontent.com/zeyadissa/open_health_data/main/const/global_var.R')
source('https://raw.githubusercontent.com/zeyadissa/open_health_data/main/src/ae.R')
source('https://raw.githubusercontent.com/zeyadissa/open_health_data/main/src/overnight_beds.R')

#remove anything unnecessary
rm(list=ls()[!grepl("FINAL_", ls())])

#Create regression data ----

#output data
FINAL_data <- FINAL_ae_data %>%
  #need it quarterly to be able to join
  dplyr::mutate(quarter_date = zoo::as.yearqtr(date)) %>%
  dplyr::ungroup() %>%
  dplyr::select(-c(period_year,period_month,code,date)) %>%
  dplyr::group_by(quarter_date,org_code)%>%
  #group by summarise
  dplyr::summarise(across(starts_with('remergency'), sum, .names = "{.col}"))  %>%
  dplyr::left_join(.,FINAL_night_bed_data,by=c('quarter_date','org_code')) %>%
  #create variables, covid_flag, time since covid, breaches, admit ratio and occupied
  dplyr::mutate(time_since_covid = quarter_date - zoo::as.yearqtr(lubridate::make_date(year=2020,month=3,day=1)),
                covid_flag = dplyr::case_when(
                  as.integer(time_since_covid) %in% c(0,1) ~ '2. Covid',
                  as.integer(time_since_covid) < 0 ~ '1. Pre-Covid',
                  as.integer(time_since_covid) > 1 ~ '3. Post-Covid'),
                time_flag = dplyr::case_when(
                  year(quarter_date) <= 2013 ~ 'Pre-2013',
                  year(quarter_date) > 2013 & year(quarter_date) <= 2016 ~ '2013 to 2016',
                  year(quarter_date) > 2016 & year(quarter_date) <= 2019 ~ '2016 to 2019',
                  year(quarter_date) > 2020 & year(quarter_date) <= 2021 ~ 'Covid',
                  TRUE ~ 'Post-Covid'),
                type_1_breaches = remergency_breaches_type_1/remergency_type_1,
                type_2_breaches = remergency_breaches_type_2/remergency_type_2,
                type_3_breaches = remergency_breaches_type_3/remergency_type_3,
                all_breaches = (remergency_breaches_type_1+remergency_breaches_type_2+remergency_breaches_type_3),
                all_attendances = (remergency_type_3+remergency_type_2+remergency_type_1),
                all_breaches_ratio = (remergency_breaches_type_1+remergency_breaches_type_2+remergency_breaches_type_3)/(remergency_type_3+remergency_type_2+remergency_type_1),
                type_1_admit_ratio = remergency_admissions_type_1/remergency_type_1,
                #note, multiply by 100 to enable easier interpretation. this is so confusing!!!
                occupied_ratio = 100*(occupied_general_acute_beds/general_acute_beds))

FINAL_regression_data <- FINAL_data %>%
  select(quarter_date,time_flag,covid_flag,org_code,all_breaches_ratio,remergency_type_1,occupied_ratio) %>%
  #remove nas: should i constrain this dataset more?
  tidyr::drop_na() %>%
  #remove small superflous places (1k seems a good limit?)
  dplyr::filter(remergency_type_1 >= 1000 & all_breaches_ratio < 0.5)

FINAL_reference_data <- FINAL_data %>%
  select(quarter_date,covid_flag,org_code,occupied_general_acute_beds,all_breaches,all_attendances,general_acute_beds) %>%
  #remove nas: should i constrain this dataset more?
  tidyr::drop_na() %>%
  dplyr::group_by(quarter_date,org_code,covid_flag) %>%
  dplyr::summarise(
    occupied_ratio = 100*sum(occupied_general_acute_beds,na.rm=T)/sum(general_acute_beds,na.rm=T),
    breach_ratio = sum(all_breaches,na.rm=T)/sum(all_attendances,na.rm=T),
    all_attendances = sum(all_attendances,na.rm=T)) %>%
  tidyr::drop_na()

EDA_1 <- FINAL_data %>%
  dplyr::mutate(occupied_ratio = as.integer(occupied_ratio)) %>%
  dplyr::select(covid_flag, quarter_date,org_code,occupied_ratio,type_1_breaches) %>%
  dplyr::group_by(covid_flag,occupied_ratio) %>%
  dplyr::summarise(breaches = mean(type_1_breaches,na.rm=T))

EDA_2 <- FINAL_data %>%
  dplyr::group_by(quarter_date) %>%
  dplyr::summarise(breaches = sum(remergency_breaches_type_1,na.rm=T),
            attendances = sum(remergency_type_1,na.rm=T),
            occupied = sum(occupied_general_acute_beds,na.rm=T),
            beds = sum(general_acute_beds,na.rm=T)) %>%
  dplyr:: mutate(ratio_breach = (breaches/attendances)*100,
         ratio_occupied = ((occupied/beds))*100,
         distance_target = 5 - ratio_breach)

