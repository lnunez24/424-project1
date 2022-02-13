library(lubridate)
library(DT)
threeStopTbl <- read.table(file="three-stops.csv",sep =",",header=TRUE)
# Double check
summary(threeStopTbl)
dim(threeStopTbl)
head(threeStopTbl)
# Change the date format into a yyyy-mm-dd format
# Add some more columns (year, month, weekday)
threeStopTbl$Date <- mdy(threeStopTbl$date)
threeStopTbl$Year <- year(mdy(threeStopTbl$date))
threeStopTbl$monthname <- month(mdy(threeStopTbl$date), label = TRUE, abbr = FALSE)
threeStopTbl$Month <- month(mdy(threeStopTbl$date))
threeStopTbl$dayname <- wday(mdy(threeStopTbl$date), label = TRUE, abbr = FALSE)
threeStopTbl$Day <- wday(mdy(threeStopTbl$date))
# Get individual stop data
uicHalsted <- subset(threeStopTbl,stationname=="UIC-Halsted")
ohare <- subset(threeStopTbl,stationname=="O'Hare Airport")
loyola <- subset(threeStopTbl,stationname=="Loyola")
# Specific year, month, stuff
ohare2019 <- subset(ohare,stationname=="O'Hare Airport" & Year == 2019)
uicHalsted2020 <- subset(uicHalsted,stationname=="UIC-Halsted" & Year == 2020)
# LABELS
monthSequence <- c("Jan","Feb","Mar","Apr","May","Jun","July","Aug","Sept","Oct","Nov","Dec")
weekdaySequence <-c("Sun","Mon","Tue","Wed","Thurs","Fri","Sat")
library(ggplot2)
library(scales)
library(shiny)
oldggplotbymonth <- ggplot(uicHalsted2020, aes(x=Month,y=rides)) + geom_col(fill="lightblue") + scale_x_continuous(breaks=seq(1,12),minor_breaks=NULL,labels=monthSequence)
uicEveryYear <- ggplot(uicHalsted, aes(x=Year,y=rides)) + geom_col(fill="lightblue") + scale_x_continuous(breaks=seq(2001,2021), minor_breaks = NULL) + labs(title="Rides from UIC-Halsted by Year") + theme(plot.title = element_text(hjust=0.5, face="bold")) + scale_y_continuous(labels=label_comma())
loyolaEveryYear <- ggplot(loyola, aes(x=Year,y=rides)) + geom_col(fill="lightblue") + scale_x_continuous(breaks=seq(2001,2021), minor_breaks = NULL)
ohareEveryYear <- ggplot(ohare, aes(x=Year,y=rides)) + geom_col(fill="lightblue") + scale_x_continuous(breaks=seq(2001,2021), minor_breaks = NULL)
# Default tables
byMonthTable <- aggregate(subset(uicHalsted, Year == 2020)$rides, by=list(subset(uicHalsted, Year == 2020)$monthname),FUN=sum)
colnames(byMonthTable) <-c("Month", "rides")

byDayTable <- aggregate(subset(uicHalsted, Year == 2020)$rides, by=list(subset(uicHalsted, Year == 2020)$Date),FUN=sum)
colnames(byDayTable) <-c("Day", "rides")

byWeekdayTable <- aggregate(subset(uicHalsted, Year == 2020)$rides, by=list(subset(uicHalsted, Year == 2020)$dayname),FUN=sum)
colnames(byWeekdayTable) <-c("Weekday", "rides")

byyear <- aggregate(uicHalsted$rides, by=list(uicHalsted$Year),FUN=sum)
colnames(byyear) <-c("Year", "rides")
ui <- fluidPage(
  titlePanel("CTA Stop Data"),
  sidebarLayout(
    sidebarPanel(
      width=2,
      h2("Navigation"),
      p("You may choose to view one stop or two."),
      p("When comparing two stops, you may select one of three stops per side."),
      selectInput(
        label="Number of Stops",
        inputId="sidebarChoice",
        choices = list("One Stop","Two Stops"),
        selected="One Stop",
        multiple=FALSE
      ),
      conditionalPanel(
        condition = "input.sidebarChoice == 'One Stop'",
        radioButtons(
          label="Stop chart choice",
          inputId="p1ChartChoice",
          choiceValues = (list("default","month","day","dayweek","all")),
          choiceNames = (list("Rides per year from 2001-2021 (default)","Rides by month","Rides by day","Rides by day of the week","All three (month, day, weekday)")),
          selected="default"
        ),
        sliderInput(
          label="Year",
          inputId="oneStopYear",
          min=2001,
          max=2021,
          value=2021,
          sep=""
        )
      ),
      conditionalPanel(
        condition ="input.sidebarChoice == 'Two Stops'",
        selectInput(
          label="1st Stop Choice",
          inputId="firststopchoice",
          choices = list("UIC-Halsted","O'Hare","Loyola"),
          selected="UIC-Halsted",
          multiple=FALSE
        ),
        radioButtons(
          label="1st stop chart choice",
          inputId="firstStopChartChoice",
          choiceValues =list("default","month","day","dayweek","all"),
          choiceNames = list("Rides per year from 2001-2021 (default)","Rides by month","Rides by day","Rides by day of the week","All three (month, day, weekday)"),
          selected="default"
        ),
        p("This stop will appear on the left"),
        sliderInput(
          inputId="firstyearinput",
          label="Year for First Stop",
          min=2001,
          value=2021,
          max=2021,
          sep=""
        ),
        selectInput(
          label="2nd Stop Choice",
          inputId="secondstopchoice",
          choices = list("UIC-Halsted","O'Hare","Loyola"),
          selected="O'Hare",
          multiple=FALSE,
        ),
        radioButtons(
          label="2nd stop chart choice",
          inputId="secondStopChartChoice",
          choiceValues =list("default","month","day","dayweek","all"),
          choiceNames = list("Rides per year from 2001-2021 (default)","Rides by month","Rides by day","Rides by day of the week","All three (month, day, weekday)"),
          selected="default"
        ),
        p("This stop will appear on the right"),
        sliderInput(
          inputId="secondyearinput",
          label="Year for Second Stop",
          min=2001,
          value=2021,
          max=2021,
          sep=""
        ),
      )
    ),
    mainPanel(
      tabsetPanel(
        tabPanel("Home",
                    conditionalPanel(
                      condition = "input.sidebarChoice == 'One Stop'",
                      conditionalPanel(
                        condition ="input.p1ChartChoice != 'all'",
                        plotOutput("oneStopSinglePlot"),
                        dataTableOutput("oneStopSingleTable")
                      ),
                      conditionalPanel(
                        condition = "input.p1ChartChoice == 'all'",
                        fluidRow(plotOutput("oneStop1")),
                        fluidRow(plotOutput("oneStop2")),
                        fluidRow(plotOutput("oneStop3"))
                      )
                    ),
                    conditionalPanel(
                      condition = "input.sidebarChoice == 'Two Stops'",
                      column(6,
                             conditionalPanel(
                               condition = "input.firstStopChartChoice != 'all'",
                               plotOutput("firstStopDefault"),
                               plotOutput("firstStopSinglePlot"),
                               dataTableOutput("firstStopSingleTable")
                             ),
                             conditionalPanel(
                               condition = "input.firstStopChartChoice == 'all'",
                               plotOutput("firstStop1"),
                               plotOutput("firstStop2"),
                               plotOutput("firstStop3")
                             )
                      ),
                      column(6,
                             conditionalPanel(
                               condition = "input.secondStopChartChoice != 'all'",
                               plotOutput("secondStopDefault"),
                               plotOutput("secondStopSinglePlot"),
                               dataTableOutput("secondStopSingleTable")
                             ),
                             conditionalPanel(
                               condition = "input.secondStopChartChoice == 'all'",
                               plotOutput("secondStop1"),
                               plotOutput("secondStop2"),
                               plotOutput("secondStop3")
                             )
                      )
                    )
                ),
        tabPanel("About",
                 h1("About"),
                 div(
                   h2("App Author"),
                   p("Luis Nunez, Gmail: Lnunez24@uic.edu"),
                   p("Published February 12th 2022"),
                   h2("Motivation"),
                   p("This app is meant to display the ridership data for the CTA transit 'El' system."),
                   p("Specifically, the number of entries at a station is recorded and plotted. Out of all of the stops available, three are chosen for the user: UIC-Halsted, O'Hare Airport, and Loyola"),
                   p("Plotting UIC-Halsted and O'Hare entry data, as well as the monthly, daily, and ridership-by-weekday was mandatory, but I chose Loyola because I wanted to see if there was anything that could be inferred about commuter students from UIC and Loyola."),
                   h2("Information and credits"),
                   h3("Data Owner"),
                   p("Chicago Transit Authority"),
                   p("https://data.cityofchicago.org/Transportation/CTA-Ridership-L-Station-Entries-Daily-Totals/5neh-572f"),
                   h3("Helpful Resources"),
                   h4("Charts:"),
                   p("ggplot https://ggplot2.tidyverse.org/"),
                   p("datatable https://www.rdocumentation.org/packages/DT/versions/0.20/topics/datatable"),
                   h4("Layout:"),
                   p("shiny https://shiny.rstudio.com/reference/shiny/latest/"),
                   h4("Data Operations and R"),
                   p("r documentation https://www.rdocumentation.org/"),
                   p("aggregate https://r-coder.com/aggregate-r/")
                 )
       )
    )
  )
)
)

server <- function(input, output) {
  oneStopYear <- reactive({
    sliderInput(
      label="Year",
      inputId="oneStopYear",
      min=2001,
      max=2021,
      value=input$oneStopYear
    )
  })
  
  
  ### SECTION FOR THE ONE STOP OPTION ###
  # Set the chart for the single stop option depending on the option (month, day, weekday, all three) 
  output$oneStopSinglePlot <- renderPlot({
    if (input$p1ChartChoice == 'default') {
      uicEveryYear
    }
    else if (input$p1ChartChoice == 'dayweek') {
      ggplot(subset(uicHalsted,Year==input$oneStopYear), aes(x=Day,y=rides)) + geom_col(fill="lightblue") + scale_x_continuous(breaks=seq(1,7),minor_breaks=NULL,labels=weekdaySequence) + labs(title="Rides from UIC-Halsted by Weekday") + theme(plot.title = element_text(hjust=0.5, face="bold")) + scale_y_continuous(labels=label_comma())
    }
    else if (input$p1ChartChoice == 'day') {
      ggplot(subset(uicHalsted,Year==input$oneStopYear), aes(x=Date,y=rides)) + geom_col(fill="lightblue") + labs(title="Rides from UIC-Halsted by Day") + theme(plot.title = element_text(hjust=0.5, face="bold")) + scale_y_continuous(labels=label_comma())
    }
    else if (input$p1ChartChoice == 'month') {
      ggplot(subset(uicHalsted,Year==input$oneStopYear),aes(x=Month,y=rides)) + geom_col(fill="lightblue") + scale_x_continuous(breaks=seq(1,12),minor_breaks=NULL,labels=monthSequence) + labs(title="Rides from UIC-Halsted by Month") + theme(plot.title = element_text(hjust=0.5, face="bold")) + scale_y_continuous(labels=label_comma())
    }
    else if (input$p1ChartChoice == 'all') {
      NULL
    }
  })
  # Set the table for the single stop option depending on the option (month, day, weekday, all three) 
  output$oneStopSingleTable <- renderDataTable({
    if (input$p1ChartChoice == 'default') {
      byyear <- aggregate(uicHalsted$rides, by=list(uicHalsted$Year),FUN=sum)
      colnames(byyear) <-c("Year", "rides")
      byyear
    }
    else if (input$p1ChartChoice == 'dayweek') {
      byWeekdayTable <- aggregate(subset(uicHalsted, Year == input$oneStopYear)$rides, by=list(subset(uicHalsted, Year == input$oneStopYear)$dayname),FUN=sum)
      colnames(byWeekdayTable) <-c("Weekday", "rides")
      byWeekdayTable
    }
    else if (input$p1ChartChoice == 'day') {
      byDayTable <- aggregate(subset(uicHalsted,Year==input$oneStopYear)$rides, by=list(subset(uicHalsted, Year==input$oneStopYear)$Date),FUN=sum)
      colnames(byDayTable) <-c("Day", "rides")
      byDayTable
    }
    else if (input$p1ChartChoice == 'month') {
      byMonthTable <- aggregate(subset(uicHalsted, Year == input$oneStopYear)$rides, by=list(subset(uicHalsted, Year == input$oneStopYear)$monthname),FUN=sum)
      colnames(byMonthTable) <-c("Month", "rides")
      byMonthTable
    }
  },rownames=FALSE)
  # Set the 3 plots for the single stop option.
  output$oneStop1 <- renderPlot({
    # Select the appropriate dataset, subset it by the year, get its rids, sum them, reorder the frame
    newTbl <- aggregate(subset(uicHalsted, Year == input$oneStopYear)$rides, by=list(subset(uicHalsted, Year == input$oneStopYear)$monthname),FUN=sum)
    # Rename the frame
    colnames(newTbl) <-c("Month", "rides")
    ggplot(newTbl, aes(x=Month, y=rides)) + geom_col(fill="lightblue") + labs(title="Rides from UIC-Halsted by Month") + theme(plot.title = element_text(hjust=0.5, face="bold")) + scale_y_continuous(labels=label_comma())
  })
  ## 1s BY WEEK DAY
  output$oneStop2 <- renderPlot({
    ggplot(subset(uicHalsted,Year==input$oneStopYear), aes(x=Day,y=rides)) + geom_col(fill="lightblue") + scale_x_continuous(breaks=seq(1,7),minor_breaks=NULL,labels=weekdaySequence) + labs(title="Rides from UIC-Halsted by Weekday") + theme(plot.title = element_text(hjust=0.5, face="bold")) + scale_y_continuous(labels=label_comma())
  })
  ## 1s BY DAY OF WEEK
  output$oneStop3 <- renderPlot({
    ggplot(subset(uicHalsted,Year==input$oneStopYear), aes(x=Date,y=rides)) + geom_col(fill="lightblue") + labs(title="Rides from UIC-Halsted by Day") + theme(plot.title = element_text(hjust=0.5, face="bold"))  + scale_y_continuous(labels=label_comma())
  })

  ### SECTION FOR THE ONE STOP OPTION ###
  # Set the yearly plot for the first stop depending on the stop
  output$firstStopDefault <- renderPlot({
    if (input$firststopchoice == "UIC-Halsted") {
      ggplot(uicHalsted, aes(x=Year,y=rides)) + geom_col(fill="lightblue") + scale_x_continuous(breaks=seq(2001,2021), minor_breaks = NULL) + labs(title="Rides from UIC-Halsted by Year") + theme(plot.title = element_text(hjust=0.5, face="bold")) + scale_y_continuous(labels=label_comma())
    }
    else if (input$firststopchoice == "O'Hare") {
      ggplot(ohare, aes(x=Year,y=rides)) + geom_col(fill="lightblue") + scale_x_continuous(breaks=seq(2001,2021), minor_breaks = NULL) + labs(title="Rides from O'Hare by Year") + theme(plot.title = element_text(hjust=0.5, face="bold"))  + scale_y_continuous(labels=label_comma())
    }
    else if (input$firststopchoice == "Loyola") {
      ggplot(loyola, aes(x=Year,y=rides)) + geom_col(fill="lightblue") + scale_x_continuous(breaks=seq(2001,2021), minor_breaks = NULL) + labs(title="Rides from Loyola by Year") + theme(plot.title = element_text(hjust=0.5, face="bold"))  + scale_y_continuous(labels=label_comma())
    }
  })
  # Set the yearly plot for the second stop depending on the stop
  output$secondStopDefault <- renderPlot({
    if (input$secondstopchoice == "UIC-Halsted") {
      ggplot(uicHalsted, aes(x=Year,y=rides)) + geom_col(fill="red") + scale_x_continuous(breaks=seq(2001,2021), minor_breaks = NULL) + labs(title="Rides from UIC-Halsted by Year") + theme(plot.title = element_text(hjust=0.5, face="bold")) + scale_y_continuous(labels=label_comma())
    }
    else if (input$secondstopchoice == "O'Hare") {
      ggplot(ohare, aes(x=Year,y=rides)) + geom_col(fill="red") + scale_x_continuous(breaks=seq(2001,2021), minor_breaks = NULL) + labs(title="Rides from O'Hare by Year") + theme(plot.title = element_text(hjust=0.5, face="bold")) + scale_y_continuous(labels=label_comma())
    }
    else if (input$secondstopchoice == "Loyola") {
      ggplot(loyola, aes(x=Year,y=rides)) + geom_col(fill="red") + scale_x_continuous(breaks=seq(2001,2021), minor_breaks = NULL) + labs(title="Rides from Loyola by Year") + theme(plot.title = element_text(hjust=0.5, face="bold")) + scale_y_continuous(labels=label_comma())
    }
  })
  # Set the three plots for the first stop (display all)
  output$firstStop1 <- renderPlot({
    if (input$firststopchoice == "UIC-Halsted") {
      ggplot(subset(uicHalsted,Year==input$firstyearinput),aes(x=Month,y=rides)) + geom_col(fill="lightblue") + scale_x_continuous(breaks=seq(1,12),minor_breaks=NULL,labels=monthSequence) + labs(title="Rides from UIC-Halsted by Month") + theme(plot.title = element_text(hjust=0.5, face="bold"))
    }
    else if (input$firststopchoice == "O'Hare") {
      ggplot(subset(ohare,Year==input$firstyearinput),aes(x=Month,y=rides)) + geom_col(fill="lightblue") + scale_x_continuous(breaks=seq(1,12),minor_breaks=NULL,labels=monthSequence) + labs(title="Rides from O'Hare by Month") + theme(plot.title = element_text(hjust=0.5, face="bold"))
    }
    else if (input$firststopchoice == "Loyola") {
      ggplot(subset(loyola,Year==input$firstyearinput),aes(x=Month,y=rides)) + geom_col(fill="lightblue") + scale_x_continuous(breaks=seq(1,12),minor_breaks=NULL,labels=monthSequence) + labs(title="Rides from Loyola by Month") + theme(plot.title = element_text(hjust=0.5, face="bold"))
    }
  })
  output$firstStop2 <- renderPlot({
    if (input$firststopchoice == "UIC-Halsted") {
      ggplot(subset(uicHalsted,Year==input$firstyearinput), aes(x=Day,y=rides)) + geom_col(fill="lightblue") + scale_x_continuous(breaks=seq(1,7),minor_breaks=NULL,labels=weekdaySequence) + labs(title="Rides from UIC-Halsted by Weekday") + theme(plot.title = element_text(hjust=0.5, face="bold"))  + scale_y_continuous(labels=label_comma())
    }
    else if (input$firststopchoice == "O'Hare") {
      ggplot(subset(ohare,Year==input$firstyearinput), aes(x=Day,y=rides)) + geom_col(fill="lightblue") + scale_x_continuous(breaks=seq(1,7),minor_breaks=NULL,labels=weekdaySequence) + labs(title="Rides from O'Hare by Weekday") + theme(plot.title = element_text(hjust=0.5, face="bold")) + scale_y_continuous(labels=label_comma())
    }
    else if (input$firststopchoice == "Loyola") {
      ggplot(subset(loyola,Year==input$firstyearinput), aes(x=Day,y=rides)) + geom_col(fill="lightblue") + scale_x_continuous(breaks=seq(1,7),minor_breaks=NULL,labels=weekdaySequence) + labs(title="Rides from Loyola by Weekday") + theme(plot.title = element_text(hjust=0.5, face="bold")) + scale_y_continuous(labels=label_comma())
    }
  })
    output$firstStop3 <- renderPlot({
    if (input$firststopchoice == "UIC-Halsted") {
      ggplot(subset(uicHalsted,Year==input$firstyearinput), aes(x=Date,y=rides)) + geom_col(fill="lightblue") + labs(title="Rides from UIC-Halsted by Day") + theme(plot.title = element_text(hjust=0.5, face="bold")) + scale_y_continuous(labels=label_comma())
    }
    else if (input$firststopchoice == "O'Hare") {
      ggplot(subset(ohare,Year==input$firstyearinput), aes(x=Date,y=rides)) + geom_col(fill="lightblue") + labs(title="Rides from O'Hare by Day") + theme(plot.title = element_text(hjust=0.5, face="bold")) + scale_y_continuous(labels=label_comma())
    }
    else if (input$firststopchoice == "Loyola") {
      ggplot(subset(loyola,Year==input$firstyearinput), aes(x=Date,y=rides)) + geom_col(fill="lightblue") + labs(title="Rides from Loyola by Day") + theme(plot.title = element_text(hjust=0.5, face="bold")) + scale_y_continuous(labels=label_comma())
    }
  })
  # Set the plot for the first stop (individual option)
  output$firstStopSinglePlot <- renderPlot({
    if (input$firstStopChartChoice == 'day') {
      if (input$firststopchoice == "UIC-Halsted") {
        ggplot(subset(uicHalsted,Year==input$firstyearinput), aes(x=Date,y=rides)) + geom_col(fill="lightblue") + labs(title="Rides from UIC-Halsted by Day") + theme(plot.title = element_text(hjust=0.5, face="bold")) + scale_y_continuous(labels=label_comma())
      }
      else if (input$firststopchoice == "O'Hare") {
        ggplot(subset(ohare,Year==input$firstyearinput), aes(x=Date,y=rides)) + geom_col(fill="lightblue") + labs(title="Rides from O'Hare by Day") + theme(plot.title = element_text(hjust=0.5, face="bold")) + scale_y_continuous(labels=label_comma())
      }
      else if (input$firststopchoice == "Loyola") {
        ggplot(subset(loyola,Year==input$firstyearinput), aes(x=Date,y=rides)) + geom_col(fill="lightblue") + labs(title="Rides from Loyola by Day") + theme(plot.title = element_text(hjust=0.5, face="bold")) + scale_y_continuous(labels=label_comma())
      }
    }
    else if (input$firstStopChartChoice == 'dayweek') {
      if (input$firststopchoice == "UIC-Halsted") {
        ggplot(subset(uicHalsted,Year==input$firstyearinput), aes(x=Day,y=rides)) + geom_col(fill="lightblue") + scale_x_continuous(breaks=seq(1,7),minor_breaks=NULL,labels=weekdaySequence) + labs(title="Rides from UIC-Halsted by Weekday") + theme(plot.title = element_text(hjust=0.5, face="bold")) + scale_y_continuous(labels=label_comma())
      }
      else if (input$firststopchoice == "O'Hare") {
        ggplot(subset(ohare,Year==input$firstyearinput), aes(x=Day,y=rides)) + geom_col(fill="lightblue") + scale_x_continuous(breaks=seq(1,7),minor_breaks=NULL,labels=weekdaySequence) + labs(title="Rides from O'Hare by Weekday") + theme(plot.title = element_text(hjust=0.5, face="bold")) + scale_y_continuous(labels=label_comma())
      }
      else if (input$firststopchoice == "Loyola") {
        ggplot(subset(loyola,Year==input$firstyearinput), aes(x=Day,y=rides)) + geom_col(fill="lightblue") + scale_x_continuous(breaks=seq(1,7),minor_breaks=NULL,labels=weekdaySequence) + labs(title="Rides from Loyola by Weekday") + theme(plot.title = element_text(hjust=0.5, face="bold")) + scale_y_continuous(labels=label_comma())
      }
    }
    else if (input$firstStopChartChoice == 'month') {
      if (input$firststopchoice == "UIC-Halsted") {
        ggplot(subset(uicHalsted,Year==input$firstyearinput),aes(x=Month,y=rides)) + geom_col(fill="lightblue") + scale_x_continuous(breaks=seq(1,12),minor_breaks=NULL,labels=monthSequence) + labs(title="Rides from UIC-Halsted by Month") + theme(plot.title = element_text(hjust=0.5, face="bold")) + scale_y_continuous(labels=label_comma())
      }
      else if (input$firststopchoice == "O'Hare") {
        ggplot(subset(ohare,Year==input$firstyearinput),aes(x=Month,y=rides)) + geom_col(fill="lightblue") + scale_x_continuous(breaks=seq(1,12),minor_breaks=NULL,labels=monthSequence) + labs(title="Rides from O'Hare by Month") + theme(plot.title = element_text(hjust=0.5, face="bold")) + scale_y_continuous(labels=label_comma())
      }
      else if (input$firststopchoice == "Loyola") {
        ggplot(subset(loyola,Year==input$firstyearinput),aes(x=Month,y=rides)) + geom_col(fill="lightblue") + scale_x_continuous(breaks=seq(1,12),minor_breaks=NULL,labels=monthSequence) + labs(title="Rides from Loyola by Month") + theme(plot.title = element_text(hjust=0.5, face="bold")) + scale_y_continuous(labels=label_comma())
      }      
    }
  })
  # Set the plot for the second stop (individual option)
  output$secondStopSinglePlot <- renderPlot({
    if (input$secondStopChartChoice == 'day') {
      if (input$secondstopchoice == "UIC-Halsted") {
        ggplot(subset(uicHalsted,Year==input$secondyearinput), aes(x=Date,y=rides)) + geom_col(fill="red") + labs(title="Rides from UIC-Halsted by Day") + theme(plot.title = element_text(hjust=0.5, face="bold")) + scale_y_continuous(labels=label_comma())
      }
      else if (input$secondstopchoice == "O'Hare") {
        ggplot(subset(ohare,Year==input$secondyearinput), aes(x=Date,y=rides)) + geom_col(fill="red") + labs(title="Rides from O'Hare by Day") + theme(plot.title = element_text(hjust=0.5, face="bold")) + scale_y_continuous(labels=label_comma())
      }
      else if (input$secondstopchoice == "Loyola") {
        ggplot(subset(loyola,Year==input$secondyearinput), aes(x=Date,y=rides)) + geom_col(fill="red") + labs(title="Rides from Loyola by Day") + theme(plot.title = element_text(hjust=0.5, face="bold")) + scale_y_continuous(labels=label_comma())
      }
    }
    else if (input$secondStopChartChoice == 'dayweek') {
      if (input$secondstopchoice == "UIC-Halsted") {
        ggplot(subset(uicHalsted,Year==input$secondyearinput), aes(x=Day,y=rides)) + geom_col(fill="red") + scale_x_continuous(breaks=seq(1,7),minor_breaks=NULL,labels=weekdaySequence) + labs(title="Rides from UIC-Halsted by Weekday") + theme(plot.title = element_text(hjust=0.5, face="bold")) + scale_y_continuous(labels=label_comma())
      }
      else if (input$secondstopchoice == "O'Hare") {
        ggplot(subset(ohare,Year==input$secondyearinput), aes(x=Day,y=rides)) + geom_col(fill="red") + scale_x_continuous(breaks=seq(1,7),minor_breaks=NULL,labels=weekdaySequence) + labs(title="Rides from O'Hare by Weekday") + theme(plot.title = element_text(hjust=0.5, face="bold")) + scale_y_continuous(labels=label_comma())
      }
      else if (input$secondstopchoice == "Loyola") {
        ggplot(subset(loyola,Year==input$secondyearinput), aes(x=Day,y=rides)) + geom_col(fill="red") + scale_x_continuous(breaks=seq(1,7),minor_breaks=NULL,labels=weekdaySequence) + labs(title="Rides from Loyola by Weekday") + theme(plot.title = element_text(hjust=0.5, face="bold")) + scale_y_continuous(labels=label_comma())
      }
    }
    else if (input$secondStopChartChoice == 'month') {
      if (input$secondstopchoice == "UIC-Halsted") {
        ggplot(subset(uicHalsted,Year==input$secondyearinput),aes(x=Month,y=rides)) + geom_col(fill="red") + scale_x_continuous(breaks=seq(1,12),minor_breaks=NULL,labels=monthSequence) + labs(title="Rides from UIC-Halsted by Month") + theme(plot.title = element_text(hjust=0.5, face="bold")) + scale_y_continuous(labels=label_comma())
      }
      else if (input$secondstopchoice == "O'Hare") {
        ggplot(subset(ohare,Year==input$secondyearinput),aes(x=Month,y=rides)) + geom_col(fill="red") + scale_x_continuous(breaks=seq(1,12),minor_breaks=NULL,labels=monthSequence) + labs(title="Rides from O'Hare by Month") + theme(plot.title = element_text(hjust=0.5, face="bold")) + scale_y_continuous(labels=label_comma())
      }
      else if (input$secondstopchoice == "Loyola") {
        ggplot(subset(loyola,Year==input$secondyearinput),aes(x=Month,y=rides)) + geom_col(fill="red") + scale_x_continuous(breaks=seq(1,12),minor_breaks=NULL,labels=monthSequence) + labs(title="Rides from Loyola by Month") + theme(plot.title = element_text(hjust=0.5, face="bold")) + scale_y_continuous(labels=label_comma())
      }      
    }
  })
  # Set the table for the first stop (individual option or default)
  output$firstStopSingleTable <- renderDataTable({
    if (input$firstStopChartChoice == 'day') {
      if (input$firststopchoice == "UIC-Halsted") {
        byDayTable <- aggregate(subset(uicHalsted,Year==input$firstyearinput)$rides, by=list(subset(uicHalsted, Year==input$firstyearinput)$Date),FUN=sum)
        colnames(byDayTable) <-c("Day", "rides")
        byDayTable
      }
      else if (input$firststopchoice == "O'Hare") {
        byDayTable <- aggregate(subset(ohare,Year==input$firstyearinput)$rides, by=list(subset(ohare, Year==input$firstyearinput)$Date),FUN=sum)
        colnames(byDayTable) <-c("Day", "rides")
        byDayTable
      }
      else if (input$firststopchoice == "Loyola") {
        byDayTable <- aggregate(subset(loyola,Year==input$firstyearinput)$rides, by=list(subset(loyola, Year==input$firstyearinput)$Date),FUN=sum)
        colnames(byDayTable) <-c("Day", "rides")
        byDayTable
      }
    }
    else if (input$firstStopChartChoice == 'dayweek') {
      if (input$firststopchoice == "UIC-Halsted") {
        byWeekdayTable <- aggregate(subset(uicHalsted, Year == input$firstyearinput)$rides, by=list(subset(uicHalsted, Year == input$firstyearinput)$dayname),FUN=sum)
        colnames(byWeekdayTable) <-c("Weekday", "rides")
        byWeekdayTable
      }
      else if (input$firststopchoice == "O'Hare") {
        byWeekdayTable <- aggregate(subset(ohare, Year == input$firstyearinput)$rides, by=list(subset(ohare, Year == input$firstyearinput)$dayname),FUN=sum)
        colnames(byWeekdayTable) <-c("Weekday", "rides")
        byWeekdayTable      
      }
      else if (input$firststopchoice == "Loyola") {
        byWeekdayTable <- aggregate(subset(loyola, Year == input$firstyearinput)$rides, by=list(subset(loyola, Year == input$firstyearinput)$dayname),FUN=sum)
        colnames(byWeekdayTable) <-c("Weekday", "rides")
        byWeekdayTable
      }
    }
    else if (input$firstStopChartChoice == 'month') {
      if (input$firststopchoice == "UIC-Halsted") {
        byMonthTable <- aggregate(subset(uicHalsted, Year == input$firstyearinput)$rides, by=list(subset(uicHalsted, Year == input$firstyearinput)$monthname),FUN=sum)
        colnames(byMonthTable) <-c("Month", "rides")
        byMonthTable
      }
      else if (input$firststopchoice == "O'Hare") {
        byMonthTable <- aggregate(subset(ohare, Year == input$firstyearinput)$rides, by=list(subset(ohare, Year == input$firstyearinput)$monthname),FUN=sum)
        colnames(byMonthTable) <-c("Month", "rides")
        byMonthTable
      }
      else if (input$firststopchoice == "Loyola") {
        byMonthTable <- aggregate(subset(loyola, Year == input$firstyearinput)$rides, by=list(subset(loyola, Year == input$firstyearinput)$monthname),FUN=sum)
        colnames(byMonthTable) <-c("Month", "rides")
        byMonthTable
      }      
    }
  },rownames=FALSE)
  # Set the table for the second stop (individual option or default)
  output$secondStopSingleTable <- renderDataTable({
    if (input$secondStopChartChoice == 'day') {
      if (input$secondstopchoice == "UIC-Halsted") {
        byDayTable <- aggregate(subset(uicHalsted,Year==input$secondyearinput)$rides, by=list(subset(uicHalsted, Year==input$secondyearinput)$Date),FUN=sum)
        colnames(byDayTable) <-c("Day", "rides")
        byDayTable
      }
      else if (input$secondstopchoice == "O'Hare") {
        byDayTable <- aggregate(subset(ohare,Year==input$secondyearinput)$rides, by=list(subset(ohare, Year==input$secondyearinput)$Date),FUN=sum)
        colnames(byDayTable) <-c("Day", "rides")
        byDayTable
      }
      else if (input$secondstopchoice == "Loyola") {
        byDayTable <- aggregate(subset(loyola,Year==input$secondyearinput)$rides, by=list(subset(loyola, Year==input$secondyearinput)$Date),FUN=sum)
        colnames(byDayTable) <-c("Day", "rides")
        byDayTable
      }
    }
    else if (input$secondStopChartChoice == 'dayweek') {
      if (input$secondstopchoice == "UIC-Halsted") {
        byWeekdayTable <- aggregate(subset(uicHalsted, Year == input$secondyearinput)$rides, by=list(subset(uicHalsted, Year == input$secondyearinput)$dayname),FUN=sum)
        colnames(byWeekdayTable) <-c("Weekday", "rides")
        byWeekdayTable
      }
      else if (input$secondstopchoice == "O'Hare") {
        byWeekdayTable <- aggregate(subset(ohare, Year == input$secondyearinput)$rides, by=list(subset(ohare, Year == input$secondyearinput)$dayname),FUN=sum)
        colnames(byWeekdayTable) <-c("Weekday", "rides")
        byWeekdayTable      
      }
      else if (input$secondstopchoice == "Loyola") {
        byWeekdayTable <- aggregate(subset(loyola, Year == input$secondyearinput)$rides, by=list(subset(loyola, Year == input$secondyearinput)$dayname),FUN=sum)
        colnames(byWeekdayTable) <-c("Weekday", "rides")
        byWeekdayTable
      }
    }
    else if (input$secondStopChartChoice == 'month') {
      if (input$secondstopchoice == "UIC-Halsted") {
        byMonthTable <- aggregate(subset(uicHalsted, Year == input$secondyearinput)$rides, by=list(subset(uicHalsted, Year == input$secondyearinput)$monthname),FUN=sum)
        colnames(byMonthTable) <-c("Month", "rides")
        byMonthTable
      }
      else if (input$secondstopchoice == "O'Hare") {
        byMonthTable <- aggregate(subset(ohare, Year == input$secondyearinput)$rides, by=list(subset(ohare, Year == input$secondyearinput)$monthname),FUN=sum)
        colnames(byMonthTable) <-c("Month", "rides")
        byMonthTable
      }
      else if (input$secondstopchoice == "Loyola") {
        byMonthTable <- aggregate(subset(loyola, Year == input$secondyearinput)$rides, by=list(subset(loyola, Year == input$secondyearinput)$monthname),FUN=sum)
        colnames(byMonthTable) <-c("Month", "rides")
        byMonthTable
      }
    }
    else if (input$secondstopchoice == "default") {
      if (input$secondstopchoice == "UIC-Halsted") {
        byyear <- aggregate(uicHalsted$rides, by=list(uicHalsted$Year),FUN=sum)
        colnames(byyear) <-c("Year", "rides")
        byyear
      }
      else if (input$secondstopchoice == "O'Hare") {
        byyear <- aggregate(ohare$rides, by=list(ohare$Year),FUN=sum)
        colnames(byyear) <-c("Year", "rides")
        byyear
      }
      else if (input$secondstopchoice == "Loyola") {
        byyear <- aggregate(loyola$rides, by=list(loyola$Year),FUN=sum)
        colnames(byyear) <-c("Year", "rides")
        byyear
      }
    }
  },rownames=FALSE)
  # Set the three plots for the seconod stop (display all)
  output$secondStop1 <- renderPlot({
    if (input$secondstopchoice == "UIC-Halsted") {
      ggplot(subset(uicHalsted,Year==input$secondyearinput),aes(x=Month,y=rides)) + geom_col(fill="red") + scale_x_continuous(breaks=seq(1,12),minor_breaks=NULL,labels=monthSequence) + labs(title="Rides from UIC-Halsted by Month") + theme(plot.title = element_text(hjust=0.5, face="bold")) + scale_y_continuous(labels=label_comma())
    }
    else if (input$secondstopchoice == "O'Hare") {
      ggplot(subset(ohare,Year==input$secondyearinput),aes(x=Month,y=rides)) + geom_col(fill="red") + scale_x_continuous(breaks=seq(1,12),minor_breaks=NULL,labels=monthSequence) + labs(title="Rides from O'Hare by Month") + theme(plot.title = element_text(hjust=0.5, face="bold")) + scale_y_continuous(labels=label_comma())
    }
    else if (input$secondstopchoice == "Loyola") {
      ggplot(subset(loyola,Year==input$secondyearinput),aes(x=Month,y=rides)) + geom_col(fill="red") + scale_x_continuous(breaks=seq(1,12),minor_breaks=NULL,labels=monthSequence) + labs(title="Rides from Loyola by Month") + theme(plot.title = element_text(hjust=0.5, face="bold")) + scale_y_continuous(labels=label_comma())
    }
  })
  output$secondStop2 <- renderPlot({
    if (input$secondstopchoice == "UIC-Halsted") {
      ggplot(subset(uicHalsted,Year==input$secondyearinput), aes(x=Day,y=rides)) + geom_col(fill="red") + scale_x_continuous(breaks=seq(1,7),minor_breaks=NULL,labels=weekdaySequence) + labs(title="Rides from UIC-Halsted by Weekday") + theme(plot.title = element_text(hjust=0.5, face="bold")) + scale_y_continuous(labels=label_comma())
    }
    else if (input$secondstopchoice == "O'Hare") {
      ggplot(subset(ohare,Year==input$secondyearinput), aes(x=Day,y=rides)) + geom_col(fill="red") + scale_x_continuous(breaks=seq(1,7),minor_breaks=NULL,labels=weekdaySequence) + labs(title="Rides from O'Hare by Weekday") + theme(plot.title = element_text(hjust=0.5, face="bold")) + scale_y_continuous(labels=label_comma())
    }
    else if (input$secondstopchoice == "Loyola") {
      ggplot(subset(loyola,Year==input$secondyearinput), aes(x=Day,y=rides)) + geom_col(fill="red") + scale_x_continuous(breaks=seq(1,7),minor_breaks=NULL,labels=weekdaySequence) + labs(title="Rides from Loyola by Weekday") + theme(plot.title = element_text(hjust=0.5, face="bold")) + scale_y_continuous(labels=label_comma())
    }
  })
  output$secondStop3 <- renderPlot({
    if (input$secondstopchoice == "UIC-Halsted") {
      ggplot(subset(uicHalsted,Year==input$secondyearinput), aes(x=Date,y=rides)) + geom_col(fill="red") + labs(title="Rides from UIC-Halsted Each Day") + labs(title="Rides from UIC-Halsted by Day") + theme(plot.title = element_text(hjust=0.5, face="bold")) + scale_y_continuous(labels=label_comma())
    }
    else if (input$secondstopchoice == "O'Hare") {
      ggplot(subset(ohare,Year==input$secondyearinput), aes(x=Date,y=rides)) + geom_col(fill="red") + labs(title="Rides from O'Hare Each Day") + labs(title="Rides from O'Hare by Day") + theme(plot.title = element_text(hjust=0.5, face="bold")) + scale_y_continuous(labels=label_comma())
    }
    else if (input$secondstopchoice == "Loyola") {
      ggplot(subset(loyola,Year==input$secondyearinput), aes(x=Date,y=rides)) + geom_col(fill="red") + labs(title="Rides from Loyola Each Day") + labs(title="Rides from Loyola by Day") + theme(plot.title = element_text(hjust=0.5, face="bold")) + scale_y_continuous(labels=label_comma())
    }
  })
}
shinyApp(ui=ui,server=server)
