library(shiny)
library(ggplot2)
library(dplyr)
library(scales)

cp <- read.csv("cellPhonesStacked.csv", stringsAsFactors = FALSE)
colnames(cp) <- c("Country", "Region", "Income", "Year", "Subscriptions")

ui <- navbarPage("Mobile Cellular Subscriptions",
      # include navigation bar
      tabPanel("Introduction",
                    
               mainPanel(
                       h3("Welcome to my", a(href="http://shiny.rstudio.com/", "Shiny"), "App!"),
                       h4("Aside from mobile cellular subscription vendors 
                          profiting from the large increase in the number of 
                          subscriptions over the past quarter century, thus 
                          creating jobs, and for those of us who have discovered
                          long lost friends on our phones through social media, 
                          I wondered how mobile communication may otherwise be 
                          providing benefit to the world."),
                       p(h4("In the", a(href="http://rpubs.com/DMW29/181587", "accompanying interactive presentation"), "I 
                            highlight the results of a study conducted by the 
                            ICT policy division of the",
                            a(href="http://www.worldbank.org/en/topic/ict", "Global Information and 
                            Communications Department (GICT)"), "indicating the 
                            beneficial role of mobile phones in sustainable 
                            rural poverty reduction.")),
                       p(h4("Inside my Shiny application please find a visualization
                            of the data used by the GICT in their study. The", 
                            em("Visualize the Data"), "tab presents a histogram 
                            and time series plot of the population selected from 
                            the left-hand panel.")), 
                       p(h4("So it is clear how significant the growth in mobile
                            cellular subscriptions is around the world, the 
                            percent change is calculated. The percent change is 
                            calculated as the number of subscriptions for the 
                            selected income and region for the ending year minus
                            the number of subscriptions for the selected income 
                            and region for the beginning year all divided by the
                            selected income and region for the beginning year.")),
                       p(h4("For example, between the years 2006 and 2014, the 
                            number of mobile cellular subscriptions rose 135.3%
                            for the Latin America & Caribbean lower middle class
                            population")),
                       p(h4("The data set of the population selected can be 
                            viewed from the", em("View the Dataset"),
                            "tab. An extensive discription of the data taken directly
                            from the", 
                            a(href="http://data.worldbank.org/indicator/IT.CEL.SETS.P2", 
                              "data download website"), "is also presented.")),
                       p(h4("Please keep in mind the cellular subscription 
                            numbers are presented per each 100 people."))
                       ) 
      ), # End of intro tabset panel
 
      tabPanel(p(icon("line-chart"), "Visualize the Data"),
               sidebarPanel(
                        h4('Visualizations, calculations and the Dataset presented will depend upon the selected population. Please make your selections below.'),
                        sliderInput("yearInput",
                                    "Year",
                                    min = 1960,
                                    max = 2014,
                                    value = c(2006, 2014)),
                        radioButtons("regionInput", "Region",
                                     choices = c("Sub-Saharan Africa",
                                                 "South Asia",
                                                 "North America",
                                                 "Middle East & North Africa",
                                                 "Latin America & Caribbean",
                                                 "Europe & Central Asia",
                                                 "East Asia & Pacific"),
                                     selected = "Latin America & Caribbean"),
                        selectInput("incomeInput", "Income",
                                    choices = c("Upper middle income",
                                                "Lower middle income",
                                                "Low income",
                                                "High income: OECD",
                                                "High income: nonOECD"),
                                    selected = "Lower middle income")
                        ),
               mainPanel(
                       tabsetPanel(
                               # Percent Increase and Charts
                               tabPanel("Percent Change",
                                        h4("The percent change in the number of cell phone subscriptions for the selected population is displayed."),
                                        h5("Region:"),
                                        verbatimTextOutput("regionChoice"),
                                        h5("Income Group:"),
                                        verbatimTextOutput("incomeChoice"),
                                        h5('Beginning with the year:'),
                                        verbatimTextOutput("yearBegin"),
                                        h5("Ending in the year:"),
                                        verbatimTextOutput("yearEnd"),
                                        h5("Percent Change:"),
                                        verbatimTextOutput("perChange")
                                        ),
                               tabPanel("Histogram",
                                        h4("The cell phone subscription distribution for the selected population is displayed"),
                                        plotOutput("hist")),
                               tabPanel("Time Series",
                                        h4("The change in cell phone subscriptions over time for the selected population is displayed"),
                                        plotOutput("time"))
                               ) # End of charts tabset panel
                       )
               ), # End of visualize the data tab panel
      
      tabPanel(p(icon("table"), "View the Dataset"),
               sidebarPanel(
                       h3('Mobile cellular subscriptions (per 100 people)'),
                       h4('Mobile cellular telephone subscriptions are 
                          subscriptions to a public mobile telephone service 
                          that provide access to the PSTN using cellular 
                          technology. The indicator includes (and is split into)
                          the number of postpaid subscriptions, and the number 
                          of active prepaid accounts (i.e. that have been used 
                          during the last three months). The indicator applies 
                          to all mobile cellular subscriptions that offer voice 
                          communications. It excludes subscriptions via data 
                          cards or USB modems, subscriptions to public mobile 
                          data services, private trunked mobile radio, telepoint,
                          radio paging and telemetry services.'),
                       h5('International Telecommunication Union, World Telecommunication/ICT Development Report and database.')
               ),         
               mainPanel(
                       tabsetPanel(
                               # Data
                               tabPanel("Data Table", tableOutput("table"))
                       ) # End of data tabset panel
               )
               ) # End of view the data tab panel
      
        ) # End of navbarpanel

server <- function(input, output) {

  filtered <- reactive({
        cp %>%
        filter(Year >= input$yearInput[1],
                Year <= input$yearInput[2],
                Region == input$regionInput,
                Income == input$incomeInput
    )
  })
  
  output$regionChoice <- renderPrint({input$regionInput})
  output$incomeChoice <- renderPrint({input$incomeInput})
  output$yearBegin <- renderPrint({input$yearInput[1]})
  output$yearEnd <- renderPrint({input$yearInput[2]})
  output$perChange <- renderPrint({percentChange()})
  
  percentChange <- reactive({
          if(is.finite(mean(filtered()$Subscriptions[filtered()$Year == min(filtered()$Year)])) &&
             mean(filtered()$Subscriptions[filtered()$Year == min(filtered()$Year)]) >0)
          
          paste(((mean(filtered()$Subscriptions[filtered()$Year == max(filtered()$Year)]) -
                  mean(filtered()$Subscriptions[filtered()$Year == min(filtered()$Year)]))/
                  (mean(filtered()$Subscriptions[filtered()$Year == min(filtered()$Year)])))*
                  100,"%")
          else 'Percent change cannot be calculated due to missing data or zero subscriptions at either year selected'
          
  })
  
  output$hist <- renderPlot({
    
    ggplot(filtered(), aes(Subscriptions)) + 
           labs(x = "Subscriptions on Log Scale", 
           y = "Frequency") +
      geom_histogram(colour = "black", fill = "skyblue") +
      theme_bw() +
      scale_x_log10(breaks = trans_breaks("log10", function(x) 10^x),
                    labels = trans_format("log10", math_format(10^.x))) +
      ggtitle("Histogram of Cell Phone Subscriptions (per 100 people)") +
      theme(plot.title = element_text(lineheight = 1.0))
  
    })
  
  output$table <- renderTable({
    
    filtered()
    
  })
  
  output$time <- renderPlot({
    
    ggplot(filtered(), 
           aes(x = Year, y = Subscriptions, color = Country)) +
      geom_point(size = 3) +
      geom_smooth(se = FALSE) +
      theme_bw() +
      ggtitle("Cell Phone Subscriptions Growth (per 100 people)") +
      theme(plot.title = element_text(lineheight = 1.0))
  
    })
}

shinyApp(ui = ui, server = server)