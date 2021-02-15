#list of sites for user selection
sites <- read.csv("./data/Sites.csv")

#list of vars for user selection
#stations <- sites$Site_Name
weather_vars <- cbind(c("Wind Speed", "Air Temperature", "Relative Humidity", "Visibiity"), 
                      c("wind_speed", "air_temperature", "rltv_hum", "visibility"))
aggregation <- cbind(c("Hourly raw data", "Monthly Average", "Daily Average",  "Daily Minimum", "Daily Maximum"),
                     c("ob_time", "monthly", "date", "date", "date"))
#aggregation vars to correspond to groupby variable

##date_input
date_choice <- cbind(c("Calendar Time", "Day within the week", "Hour within the week", "Hour in the day"),
                     c("date", "wday", "hourofweek", "hour"))

##month input for Hutton Criteria
hc <- cbind(c(month.name), c(seq(1:12)))

#function to load in required site data
load.data <- function(site) {
  data <- NULL
  for (i in site) {
    load(file=paste0("data/Site_",i,".RData"))
    data <- rbind(data, eval(as.name(paste0("sites_",i))))
  }
  return(data)
}
  
#Pre-processing already done in above RData files --see 'Data Pre-Processing.R' code

#create custom aggregation function
data.aggregation <- function(data, freq, summmary.function=mean, var) {
  require(dplyr)
  varname <- paste0(var)
  data %>% group_by(!!(sym(freq)), Site_Name) %>%        #here need to change string from list lookup to symbol            
    summarise({{var}}:=summmary.function(!!(sym(var)), na.rm = TRUE))
}

#function for plots
time.plot <- function(plotdata, freq, xlab, plotvar, var, time_choice, agg) {
  ###axis labels depend on selected inputs
  ylabel <- paste0(agg, " of ", var)
  xlabel <- paste0(xlab) #date_choice from user input
  title <- paste0(var, " by ", xlab)
  
  #change data used for plot depending on aggregation var
  if (agg == "Daily Minimum" ){
    plotdata = data.aggregation(data=plotdata, freq=freq, 
                                  summmary.function=min, var=plotvar)
  } 
  if (agg == "Daily Maximum") {
    plotdata = data.aggregation(data=plotdata, freq=freq, 
                                  summmary.function=max, var=plotvar)
  }
  if (agg %in% c("Monthly Average", "Daily Average")) {  #daily/monthly average
    plotdata = data.aggregation(data=plotdata, freq=freq, 
                                  summmary.function=mean, var=plotvar)
  }
  
  #for calendar time
  if (time_choice == "Calendar Time") {
    
    ggplot(data = plotdata) +
      geom_line(aes(x=!!(sym(freq)), y = !!(sym(plotvar)), colour = Site_Name)) +
      xlab(xlabel) + ylab(ylabel) + ggtitle(title)
  } else {
    #for day/hour in week or hour in the day
    ggplot(data = plotdata) +
      geom_point(aes(x=!!(sym(freq)), y = !!(sym(plotvar)), colour = Site_Name), alpha = 0.5) +
      xlab(xlabel) + ylab(ylabel) + ggtitle(title)
  }
}
