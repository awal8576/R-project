##file for pre-processing of CSV files
library(dplyr)
library(tidyr)
library(lubridate)

#list of all sites 
sites <- read.csv("./data/Sites.csv")

#create custom aggregation function
data.aggregation <- function(data, freq, summmary.function=mean, var) {
  require(dplyr)
  varname <- paste0(var)
  data %>% group_by(!!(sym(freq)), Site_Name) %>%        #here need to change string from list lookup to symbol            
    summarise({{var}}:=summmary.function(!!(sym(var)), na.rm = TRUE)) 
}

for (i in sites$Site_ID) {
  
  #load in each csv and clean/format
  read <- read.csv(paste0("data/Site_",i,".csv"))
  read$ob_time = parse_date_time(read$ob_time, orders = c('YmdHMS','dmYHM'))
    #change the format of the date to ensure consistent format.  Site4 date different
  
  #need to ensure all days are present for lag function
  read <- read %>% 
    mutate(date = date(ob_time)) %>%
    complete(date=seq.Date(as.Date("2020-01-01"), as.Date("2020-11-30"), by="day"), Site) %>% 
    #^^^^ insert rows so each dataset has a row for every days data (for lag function in Hutton Criteria calcs)
    #leave missing measurements with NA
    
    mutate(#ob_time = ifelse(is.na(ob_time), paste(date, "00:00:00"), ob_time),     
           hour = ifelse(is.na(hour), 0, hour),
           day = ifelse(is.na(day),day(date), day),
           month = ifelse(is.na(month),month(date), month), 
           #^^fill missing hour/day/time vars using date 
           ddate = decimal_date(date),
           monthly = make_date(year(date), month(date)),     #for monthly aggregation use first day of month 
           wday = wday(date) -1 ,
           week = week(date) ) %>%
           group_by(week) %>%
           mutate(hourofweek = (24*wday) + hour ) %>%  
           ungroup()
  
  #remove duplicated entries for the same day and hour.
  read <- read %>%
    arrange(Site, ob_time, 
            #sort to ensure duplicated rows with NA are last
            is.na(wind_speed), is.na(air_temperature), is.na(rltv_hum), is.na(visibility)) %>% 
    #remove duplicated rows
    distinct(Site, ob_time, .keep_all = TRUE)

  #add site details
  read <- read %>%  
    left_join(sites, by=c("Site"="Site_ID"))


  #calculate Hutton Criteria on datasets here:
  ##two prev days have min temp of 10 & atleast 6hrs of rel_humidity >90%
  hc1 <- data.aggregation(data=read, freq="date", summmary.function=min, var="air_temperature")   #daily min    
  ##^^need to put variable names in qutoes as had to change function above to use sym() as variables passed to this function from ui are strings from a list
  hc2 <- read %>%
    mutate(hc2_vars = ifelse(rltv_hum >= 90, 1, 0) )  %>%  #create boolean flag if the relative humidity is at least 90%
    group_by(date, Site_Name) %>%                          #group by date so can calculate the daily aggregation of criteria
    summarise(hc2 = sum(hc2_vars, na.rm = TRUE))                        #sum the number of hours that exceed 90% humidity
  
  hc <- hc1 %>% 
    full_join(hc2, by=c("date", "Site_Name")) %>%
    group_by(Site_Name) %>%
    mutate(hc1_l1 = lag(air_temperature, n=1, order_by=date),       #lag function errors if there is a missing date! 
           hc1_l2 = lag(air_temperature, n=2, order_by=date),
           hc2_l1 = lag(hc2, n=1, order_by=date), 
           hc2_l2 = lag(hc2, n=2, order_by=date),
           hc1_flag = ifelse(hc1_l1>=10 & hc1_l2>=10, 1, 0),    #min temp at least 10 degrees
           hc2_flag = ifelse(hc2_l1>=6 & hc2_l2>=6, 1, 0),
           hc_flag = ifelse(hc1_flag==1 & hc2_flag==1, TRUE, FALSE)) %>%
    ungroup() %>%
    ##Tidy output data
    select(Site_Name, date, hc_flag) %>%
    rename(Hutton_Criteria=hc_flag) %>%
    #replace missing values with FALSE as dont have earlier data to determine HC for 1st/2nd so assume FALSE
    mutate(Hutton_Criteria = ifelse(is.na(Hutton_Criteria), FALSE, Hutton_Criteria))
    
  
  #then merge hc flag onto dataset
  read <- read %>% 
    full_join(hc, by=c("date", "Site_Name"))
  
  #save each formatted/clean dataframe as Rdata file instead
  df <- paste0("sites_",i)
  assign(df,read)
  save(list = (df[1]), file=paste0("data/Site_",i,".RData"))
}


