library(shiny)
library(leaflet)
library(ggplot2)
library(DT)
library(bslib)
library(reshape2)
library(janitor)
library(scales)
library(lubridate)
library(readr)
library(geojsonio)
library(rsconnect)


theme <- bs_theme(bootswatch = "cosmo", primary = "#2C3E50", secondary = "#18BC9C")

population_df <- read.csv("02-population-by-governorate.csv") #getting data
population_df <- population_df[, c("Year", "Nationality", "Sex", "Governorate", "Population")]
population_df$Year <- substr(population_df$Year, 1, 4)

#for visitor, removing the weird arabic columns
visitor_df <- read.csv("05-inbound-visitors-by-countryregion-of-residence.csv")
visitor_df <- visitor_df[, !(names(visitor_df) %in% c("N", "الشهر"))]
visitor_df <- clean_names(visitor_df)
visitor_df$month <- trimws(visitor_df$month)
visitor_df$month_num <- parse_number(visitor_df$month)
visitor_df$year_month <- make_date(year = visitor_df$year, month = visitor_df$month_num, day = 1)

#for temp, also some cleaning,
temp_df <- read.csv("average-minimum-and-maximum-temperature.csv")
temp_df <- temp_df[, c("Year", "Month", "Indicator", "Sub.Indicator", "Value")]
colnames(temp_df) <- c("Year", "Month", "Indicator", "SubIndicator", "Value")
temp_df$SubIndicator <- trimws(temp_df$SubIndicator)
temp_df$MonthNum <- match(temp_df$Month, month.name)

ui <- navbarPage(
  title = span("🌍 Bahrain Island Dashboard", style = "font-weight: bold; font-size: 25px;"), #the emojis were HELL to get for some unknown reason on my laptop keyboard but looks clean,, could remove honestly
  theme = theme,
  
  tabPanel("Overview", #overview tab,, picture of skyline? im still a bit confused on what to put here// ask prof
           fluidPage(
             tags$style(HTML("
        .container-fluid { background-color: #007777; padding: 30px; }
        h2 { font-weight: 600; color: #000000; }
        h3 { font-weight: 600; color: #ffffff; }
        .well {
          background-color: #ffffff; border-radius: 10px; padding: 20px;
          box-shadow: 0 2px 8px rgba(0,0,0,0.1);
        }
        table.dataTable {
          font-weight: bold;
          color: #000000;
        }
        table.dataTable td, table.dataTable th {
          color: #000000;
        }
        label[for='tempType']::after {
          content: ' ℹ️';
          cursor: help;
        }
        label[for='tempType'] {
          title: 'Mean = daily average; Mean Min = avg daily lows; Mean Max = avg daily highs; Extreme Min = lowest recorded';
        }
      ")), #honestly asked chatgpt for the formating on this, not sure how to do the tags things well, then went to a hexcolour site to get the codes.
             div(class = "well",
                 h2("Welcome to the Bahrain Island Dashboard"),
                 p("Bahrain is an archipelago in the Persian Gulf known for its rich history, diverse culture, and thriving economy."),
                 imageOutput("overviewImage", height = "300px"),
                 br(),
                 p("Use the tabs above to navigate through maps, population statistics, visitor data, temperature, and a gallery of images and national hotspots.")
             )
           )
  ),
  
  tabPanel("Map",
           fluidPage(
             h3("🗺️ Location of Bahrain"),
             leafletOutput("bahrainMap", height = "500px")
           )
  ),
  
  tabPanel("Population",
           sidebarLayout(
             sidebarPanel(
               h4("📊 Population Filters"),
               selectInput("popYear", "Select Year:", choices = unique(population_df$Year)),
               selectInput("popNationality", "Nationality:", choices = unique(population_df$Nationality)),
               selectInput("popSex", "Sex:", choices = c("Male", "Female", "Both"), selected = "Both"),  #making both default is baseline
               checkboxInput("popShowTimeSeries", "Show time series trend", FALSE) #OMG im so proud of this lol, instead of having multiple plots why not have one but with ability to change to time series? idk i though it was so cool
             ),
             mainPanel(
               h3("Population Visualization"),
               plotOutput("popPlot"),
               br(),
               DTOutput("popTable")
             )
           )
  ),
  
  tabPanel("Visitors",
           sidebarLayout(
             sidebarPanel(
               h4("🌐 Visitor Filters"),
               selectInput("visYear", "Select Year:", choices = unique(visitor_df$year), selected = min(visitor_df$year)),
               uiOutput("visMonthUI"),
               checkboxGroupInput("visRegion", "Regions:",
                                  choices = c("America", "Asia", "Europe", "KSA", "Middle East", "Other GCC", "Other Countries"),
                                  selected = c("America", "Asia")), #ask professor: KSA is by far the most in all situations by a huge margin, is bar plot really the best visual if KSA is overrepresented.
               checkboxInput("visShowTimeSeries", "Show time series trend", FALSE),
               conditionalPanel(
                 condition = "input.visShowTimeSeries == true",
                 selectInput("visTimeType", "Trend Type:", choices = c("Yearly", "Average by Month"))
               )
             ),
             mainPanel(
               h3("Inbound Visitors"),
               plotOutput("visPlot"),
               br(),
               DTOutput("visTable")
             )
           )
  ),
  
  tabPanel("Temperature", #this was fairly clean,, i could add tooltips to define mean minimum, maximum, and extreme minimum maybe?
           sidebarLayout(
             sidebarPanel(
               h4("🌡️ Temperature Settings"),
               tagList(
                 tags$label("Temperature Type:",
                            tags$span(" ℹ️", title = "Mean = daily avg\nMean Minimum = avg lows\nMean Maximum = avg highs\nExtreme Minimum = record low", style = "cursor: help; margin-left: 5px;")
                 ),
                 selectInput("tempType", NULL,
                             choices = c("Mean", "Mean Minimum", "Mean Maximum", "Extreme Minimum"),
                             selected = "Mean")
               ),
               radioButtons("tempUnit", "Unit:", choices = c("Celsius", "Fahrenheit"), inline = TRUE), #to convert celsius to fehrenheit, also weirdly proud of this, 
               checkboxInput("tempShowAvgMonthly", "Show average by month", FALSE)
             ),
             mainPanel(
               h3("Temperature Over Time"),
               plotOutput("tempPlot"),
               DTOutput("tempTable")
             )
           )
  ),
  tabPanel("Gallery",
           fluidPage(
             h3("📸 Bahrain Gallery"),
             fluidRow(
               column(6,
                      tags$div(
                        imageOutput("gallery2", height = "440px"),
                        tags$p("UNESCO World Heritage Site – Ancient Dilmun capital and archaeological wonder.",
                               style = "text-align: center; font-style: italic; font-size: 14px; padding-top: 10px;")
                      )
               ),
               column(6,
                      tags$div(
                        imageOutput("gallery1", height = "440px"),
                        tags$p("A 400-year-old tree thriving in the desert – a natural mystery.",
                               style = "text-align: center; font-style: italic; font-size: 14px; padding-top: 10px;")
                      )
               )
             )
           )))
#now i do server part,,
server <- function(input, output, session) {
  output$overviewImage <- renderImage({
    list(src = "www/bahrain_overview.jpg", alt = "Bahrain overview", width = "60%", height = "323") #www folder in 415 folder
  }, deleteFile = FALSE)
  bahrain_geo <- geojson_read("www/bahrain.geojson", what = "sp") #im kinda proud of this, found a geojson that has the governorrates of Bahrain, although its kinda missing one? nonetheless works very well with the map tab
  output$bahrainMap <- renderLeaflet({
    leaflet() %>%
      addProviderTiles(providers$Esri.WorldImagery) %>%  #satellite map
      setView(lng = 50.5577, lat = 26.0667, zoom = 10) %>% #played around with zoom until it looks not bad
      addPolygons(data = bahrain_geo,
                  weight = 2, color = "#2C3E50", fillColor = "#0072B2", fillOpacity = 0.3,
                  label = ~as.character(shapeName), #got some of this code from kaggle, wasnt sure how to integrate geospatial stuff too well
                  highlightOptions = highlightOptions(weight = 3, color = "#000", fillOpacity = 0.5, bringToFront = TRUE),
                  labelOptions = labelOptions(direction = "auto", textsize = "13px", #this was a little annoying all together, i had to play around sizes of everything in most tabs until it worked
                                              style = list("font-weight" = "bold", "color" = "#333"))) %>%
      addMarkers(lng = 50.5577, lat = 26.0667, popup = "Bahrain")
  })
  
  popData <- reactive({  #subset,,
    df <- subset(population_df, Nationality == input$popNationality)
    if (input$popSex != "Both") df <- subset(df, Sex == input$popSex)
    df
  })
  output$popPlot <- renderPlot({ #rendering plot in server,,validating data, currently its box plot, seems fine for this
    df <- popData()
    validate(need(nrow(df) > 0, "No population data."))
    if (!input$popShowTimeSeries) {
      df_year <- subset(df, Year == input$popYear)
      ggplot(df_year, aes(x = Governorate, y = Population, fill = Governorate)) +
        geom_col() + scale_y_continuous(labels = comma) +  
        labs(title = paste("Population by Governorate -", input$popYear),
             x = "Governorate", y = "Population") + theme_minimal(base_size = 14)
    } else {
      df$Year <- as.numeric(df$Year)
      ggplot(df, aes(x = Year, y = Population, color = Governorate)) +
        stat_summary(fun = sum, geom = "line", size = 1) +
        stat_summary(fun = sum, geom = "point") +
        scale_y_continuous(labels = comma) +
        labs(title = "population trends by governorate", x = "year", y = "total population") +
        theme_minimal(base_size = 14)
    }
  })
  output$popTable <- renderDT({
    df <- popData()
    if (!input$popShowTimeSeries) df <- subset(df, Year == input$popYear)
    datatable(df)
  })
  output$visMonthUI <- renderUI({
    months_for_year <- visitor_df[visitor_df$year == input$visYear, "month_num"]
    unique_months <- sort(unique(months_for_year))
    selectInput("visMonth", "Select Month:",
                choices = setNames(unique_months, month.name[unique_months]),
                selected = unique_months[1])
  })
  visData <- reactive({
    selected_date <- make_date(year = input$visYear, month = as.numeric(input$visMonth), day = 1)
    subset(visitor_df, year_month == selected_date)
  })
  output$visPlot <- renderPlot({
    region_cols <- c("america", "asia", "europe", "ksa", "middle_east", "other_gcc", "other_countries")
    if (!input$visShowTimeSeries) {  #columns
      df <- visData()
      validate(need(nrow(df) > 0, "No visitor data for selected filters.")) #case
      df_regions <- df[, region_cols, drop = FALSE]
      df_long <- melt(df_regions, variable.name = "Region", value.name = "Visitors")
      df_long$Region <- gsub("_", " ", df_long$Region) #this section was EXHAUSTING, asked chatgpt for advice on how to appraoch cleaning and such
      selected_regions <- tolower(gsub(" ", "_", input$visRegion))
      df_long <- subset(df_long, tolower(gsub(" ", "_", df_long$Region)) %in% selected_regions)
      ggplot(df_long, aes(x = Region, y = Visitors, fill = Region)) +
        geom_col() + scale_y_continuous(labels = comma) +
        labs(title = paste("Inbound Visitors -", format(make_date(input$visYear, input$visMonth, 1), "%B %Y")),
             x = "Region", y = "Number of Visitors") + theme_minimal(base_size = 14)
    } else {
      df <- melt(visitor_df[, c("year", "month_num", region_cols)],
                 id.vars = c("year", "month_num"), variable.name = "Region", value.name = "Visitors")
      df$Region <- gsub("_", " ", df$Region)
      selected_regions <- tolower(gsub(" ", "_", input$visRegion))
      df <- subset(df, tolower(gsub(" ", "_", df$Region)) %in% selected_regions)
      if (input$visTimeType == "Yearly") {
        agg <- aggregate(Visitors ~ year + Region, df, sum)
        ggplot(agg, aes(x = year, y = Visitors, color = Region)) + #time series,, 
          geom_line(size = 1) + geom_point() + #ggplot line graph
          scale_y_continuous(labels = comma) +
          labs(title = "Yearly Visitor Trends", x = "Year", y = "Visitors") +
          theme_minimal(base_size = 14)
      } else {
        agg <- aggregate(Visitors ~ month_num + Region, df, mean) #aggregating so we can plot since data is terrrible on  its own to work with.
        ggplot(agg, aes(x = month_num, y = Visitors, color = Region)) +
          geom_line(size = 1) + geom_point() +
          scale_x_continuous(breaks = 1:12, labels = month.name) +
          scale_y_continuous(labels = comma) +
          labs(title = "Average Monthly Visitors Across Years", x = "Month", y = "Avg. Visitors") +
          theme_minimal(base_size = 14)
      }
    } #THIS ENTIRE CODE BLOCK ABOVE WITH CLEANING TOOK WAYYYY TOO LONG, MOST PROJECT WAS SPENT HERE.
  })
  
  output$visTable <- renderDT({
    df <- visData()
    if (nrow(df) == 0) datatable(data.frame(Message = "No visitor data")) else datatable(df)
  })
  tempData <- reactive({
    df <- temp_df[temp_df$SubIndicator == input$tempType, ] #temp data wasnt bad
    df <- df[!is.na(df$MonthNum), ]
    if (input$tempUnit == "Fahrenheit") df$Value <- df$Value * 9/5 + 32 #to convert to fahrebheit for the button to worl
    if (input$tempShowAvgMonthly) {
      df <- aggregate(Value ~ MonthNum, df, mean)
      df$Month <- factor(month.name[df$MonthNum], levels = month.name)
    }
    df
  })
  output$tempPlot <- renderPlot({ #this is the one im conflicted most one, not sure what visual would look nice,, ASK PROF
    df <- tempData()
    validate(need(nrow(df) > 0, "No temperature data"))
    if (!input$tempShowAvgMonthly) {
      ggplot(df, aes(x = as.numeric(Year), y = Value, color = Month)) +
        geom_line(aes(group = Month), alpha = 0.6) +
        geom_point(size = 1.5) +
        scale_y_continuous(labels = number_format(accuracy = 0.1)) +
        labs(title = paste(input$tempType, "Temperature Over Years"),
             x = "Year", y = paste("Temperature (", ifelse(input$tempUnit == "Celsius", "°C", "°F"), ")")) +
        theme_minimal(base_size = 14)
    } else {
      ggplot(df, aes(x = Month, y = Value, group = 1)) +
        geom_line(color = "#0072B2", size = 1) +
        geom_point(color = "#0072B2", size = 2) +
        scale_y_continuous(labels = number_format(accuracy = 0.1)) +
        labs(title = paste("Average", input$tempType, "by Month"),
             x = "Month", y = paste("Temperature (", ifelse(input$tempUnit == "Celsius", "°C", "°F"), ")")) +
        theme_minimal(base_size = 14)
    }
  })
  output$tempTable <- renderDT({ datatable(tempData()) })
  output$debugPopTable <- renderDT({ datatable(population_df) })
  output$debugVisTable <- renderDT({ datatable(visitor_df) })
  for (i in 1:2) { #idk what to show in gallery honestly but currently have some?
    local({
      idx <- i
      output[[paste0("gallery", idx)]] <- renderImage({
        list(src = paste0("www/gallery", idx, ".jpg"),
             alt = paste("Gallery image", idx),
             width = "100%")
      }, deleteFile = FALSE)
    })}}
shinyApp(ui, server)
#ask prof: Do i add more words: edit, i guess not needed since its dashboard and you dont want it to be report.