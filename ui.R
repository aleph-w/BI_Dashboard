# ui.R
#library(shiny)


shinyUI(fluidPage(
  titlePanel("Intercity Waves"),
  
  sidebarLayout(
    sidebarPanel(
      helpText("Select Attributes of the Wavelength of Interest."),
      
      selectInput("wave", 
                  label = "Choose Wavelength Bandwidth",
                  choices = c("1G", "2.5G",
                              "10G"),
                  selected = "10G"),
      
      dateRangeInput("dateRange",
                     label = "Select Date Range",
                     start="2014-01-01",
                     end = Sys.Date(),
                     min="2013-04-01",
                     max=Sys.Date()),
      
      sliderInput("MRCrange", 
                  label = "MRC Range:",
                  min = 0, max = 20000, value = c(0, 15000)),
      
      sliderInput("Milerange", 
                  label = "Mileage Range:",
                  min = 0, max = 4000, value = c(50, 4000)),
      
      selectInput("subChannel", 
                  label = "Account Sub Channel",
                  choices = c("All","West GAM","National Service Providers Wholesale",
                              "Regional Pacific Enterprise","Strategic Pacific Enterprise",
                              "Regional Midwest Enterprise","Strategic Midwest Enterprise"),
                  selected = "All"),
      
      selectInput("tier", 
                  label = "Tier",
                  choices = c("All","SEAT","Tier 1","Tier 2",
                              "Tier 3","Gov & R/E", "Telecom"),
                  selected = "All"),
      
      selectInput("vertical", 
                  label = "Vertical",
                  choices = c("All",c(as.character(unique(wavesData$Vertical)))),
                  selected = "All")
      
    ),
    
    mainPanel(
      tabsetPanel(type = "tabs",
                  tabPanel("Average MRR / Wave", plotOutput("AvgMRR"))
                           #,tableOutput("testTable"))
                  ,tabPanel("Average MRR / Mile", plotOutput("AvgMRRMile"))
                  ,tabPanel("Average Discount", plotOutput("AvgDSC"),plotOutput("DscHist"))
                  ,tabPanel("Win Rate", plotOutput("WinRate"))
                  ,tabPanel("Deal Summary", tableOutput("summary"))
                  ,tabPanel("Volumetrics", plotOutput("totQuotes")
                                         , plotOutput("totWins")
                                         , plotOutput("mrrTS"))
                  ,tabPanel("Waves Pilot Summary", dataTableOutput("WavesInfo"))
                  )
      
      
    )
  )
))
