##shiny app published at:
#https://awaluni.shinyapps.io/Project/

library(ggplot2)
library(dplyr, warn.conflicts = FALSE)
library(tidyr)
library(lubridate)
library(maps)
library(rmarkdown)

shinyServer(function(input, output, session) {
  
  # run every time data is updated 
  observe({
    #change input choices based on values selected
    if (input$agg == "Hourly raw data") {
      updateSelectInput(session, "time", 
          choices=c("Calendar Time", "Day within the week", 
                    "Hour within the week", "Hour in the day") # update choices
                 #selected = NULL # remove selection
      )}
    ##need to change below so month combined with calendar time
    if (input$agg == "Monthly Average") {
      updateSelectInput(session, "time", choices=c("Calendar Time") # update choices
      )}
    if (input$agg %in% c("Daily Average",  "Daily Minimum", "Daily Maximum")) {
      updateSelectInput(session, "time", choices=c("Calendar Time", "Day within the week") # update choices
                        #selected = NULL # remove selection
      )}
  })
  
  selected_sites <- reactive({
    sites$Site_ID[sites$Site_Name %in% input$site] #lookup site names to site id for ingest
  })
  
  #load in selected datasets using custom function   ###change to Rdata
  site_data <- reactive( {
    load.data(selected_sites())   
  })
  
  #what weather variable to plot
  weather_var <- reactive({ 
    weather_vars[which(weather_vars == input$weather_var),2]
  })
  
  #what aggregation to plot
  freq_var <- reactive({ 
    aggregation[which(aggregation == input$agg),2]
  })
  
  #date display for x_axis
  #calculate x_axis for (need to change for monthly aggregation)
  x_var <- reactive({
    if (input$time == "Calendar Time" & input$agg == "Monthly Average") {
      "monthly"
    } else date_choice[which(date_choice == input$time),2]
  })
  
  #selected month for hutton criteria
  hc_month <- reactive({ 
    hc[which(hc == input$hc),2]
  })
  
  summary_table <- reactive ({
    site_data () %>%
      filter(between(date, as.Date("2020-11-24"), as.Date("2020-11-30") ) ) %>%
      group_by(date, Site_Name) %>%
      summarise_at(vars(wind_speed:visibility), list(~ sprintf("%0.1f",mean(., na.rm=TRUE))) ) %>%  #funs()` is deprecated as of dplyr 0.8.0.
      gather("weather_vars", "daily_averages", -date, -Site_Name) %>%
      spread(date, daily_averages)
  })
  
  
  #create plots as reactive so can re-use in report
  time <- reactive({ 
    ##use time.plot function
    #time.plot(plotdata=site_data(), time=x_time(), plotvar=weather_var(), time_choice=input$time, agg=input$agg)
    time.plot(plotdata=site_data(), freq=x_var(), xlab=input$time,
              plotvar=weather_var(), var=input$weather_var,
              time_choice=input$time, agg=input$agg)
  })


  output$timeplot <- renderPlot( {
    time()
  })
  
  output$mapplot <- renderPlot( {
    maps::map("world",'UK')
    sel_coords <- sites %>%
      filter(Site_ID %in% selected_sites())
    points(sel_coords$Longitude, sel_coords$Latitude, pch=16, col="red")
  })
  
  output$summarytable <- renderDataTable( {
    summary_table()
  })

  output$download1 <- downloadHandler( 
    filename = "summary.csv", 
    content = function(file) { # Function to generate the download file
      write.csv(summary_table(), file, row.names=FALSE) 
    }
  )
  
  output$downloadReportButton <- downloadHandler( 
    filename = "report.docx",
    content = function(file) {
     render("report.Rmd", output_format="word_document", 
           output_file=file, params=list(summary_data=summary_table(),
                                         plot=time(),
                                         sites=input$site,
                                         sites_filter=selected_sites(),
                                         weather_var=input$weather_var,
                                         agg=input$agg
                                          ))
                                                  })
  
  output$hc_plot <- renderPlot ({
    hc_summary <- site_data() %>%
      filter(Site %in% selected_sites() & month(date) == hc_month()) %>%
      #group_by(month(date)) %>%
      select(date, Site_Name, Site, Hutton_Criteria) %>%
      unique() %>%
      group_by(Site_Name) %>%
      summarise(true = sum(Hutton_Criteria==TRUE, na.rm=TRUE),
                false = sum(Hutton_Criteria==FALSE, na.rm=TRUE)) %>%
      gather(HuttonCriteria, count, -Site_Name)
    
    #plot for how many days do/dont meet HC
    ggplot(data=hc_summary) + 
      #baes for how many days do/dont meet HC
      geom_bar(aes(fill = HuttonCriteria, y = count,  x=Site_Name,), position="dodge", stat="identity") +
      xlab("Selected Sites") + ylab("Number of Days") +
      ggtitle("Number of days in selected month where potato's arent/are at risk of blight forming on potato crops")
  })
  
  output$hctable <- renderDataTable( {
      month_hc <- site_data() %>%
        filter(month(date) == hc_month(), Site %in% selected_sites()) %>%
        select(date, Site_Name, Hutton_Criteria) %>%
        unique()
    
  })
  
})
