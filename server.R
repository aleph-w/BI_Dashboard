# server.R
library(shiny)
library(plyr)
library(ggplot2)

# This is where the data sits:
#wavesData <- read.csv("X:/Coursera/Shiny/WavesApp/data/wavesData.csv", header=T)
# define percent function:
percent <- function(x,digits=0) paste0(round(x*100,digits=digits),"%")


shinyServer(
  function(input, output) {
    
    # Create new data subset for each calculation
    NPInput <- reactive({
      wavesNP <- subset(wavesData, as.Date(QuoteCreateDate) >= input$dateRange[1]
                    & as.Date(QuoteCreateDate) <= input$dateRange[2]
                    & DiscountMRC_USD >= input$MRCrange[1]
                    & DiscountMRC_USD <= input$MRCrange[2]
                    & Miles >= input$Milerange[1]
                    & Miles <= input$Milerange[2]
                    & InPilot == 0)
    })
    
    #PilotInput <- reactive({
    #  wavesPilot <- subset(wavesData, as.Date(QuoteCreateDate) >= input$dateRange[1]
    #                    & as.Date(QuoteCreateDate) <= input$dateRange[2]
    #                    & DiscountMRC_USD >= input$MRCrange[1]
    #                    & DiscountMRC_USD <= input$MRCrange[2]
    #                    & Miles >= input$Milerange[1]
    #                    & Miles <= input$Milerange[2]
    #                    & InPilot == 1)
    #})
    
    AllWavesData <- reactive({
      wavesPilot <- subset(wavesData, as.Date(QuoteCreateDate) >= input$dateRange[1]
                           & as.Date(QuoteCreateDate) <= input$dateRange[2]
                           & DiscountMRC_USD >= input$MRCrange[1]
                           & DiscountMRC_USD <= input$MRCrange[2]
                           & Miles >= input$Milerange[1]
                           & Miles <= input$Milerange[2])
    })
    
    PilotInput <- reactive({
      wavesPilot <- subset(wavesData, as.Date(QuoteCreateDate) >= input$dateRange[1]
                           & as.Date(QuoteCreateDate) <= input$dateRange[2]
                           & DiscountMRC_USD >= input$MRCrange[1]
                           & DiscountMRC_USD <= input$MRCrange[2]
                           & Miles >= input$Milerange[1]
                           & Miles <= input$Milerange[2]
                           & InPilot == 1
                           & if (input$subChannel == "All") Account_Sub_Channel != input$subChannel
                             else Account_Sub_Channel == input$subChannel
                           & if (input$tier == "All") Tier != input$tier
                             else Tier == input$tier
                           & if (input$vertical == "All") Vertical != input$vertical
                             else Vertical == input$vertical )
    })
    
    output$AvgMRR <- renderPlot({ 
      
      # Reactive data inputs:
      wavesNP <- NPInput()
      wavesPilot <- PilotInput()
      
      # Fix non-pilot data
      wavesMRCMonth.np <- aggregate(DiscountMRC_USD ~ YearMonth, subset(wavesNP,Won==1), FUN = mean)
      wavesMRCMonth.np$YearMonth <- paste0(wavesMRCMonth.np$YearMonth,"-01")
      wavesMRCMonth.np$YearMonth <- as.Date(wavesMRCMonth.np$YearMonth,format="%Y-%m-%d")
      
      # Fix In-Pilot data
      wavesMRCMonth.pilot <- aggregate(DiscountMRC_USD ~ YearMonth, subset(wavesPilot,Won==1), FUN = mean)
      wavesMRCMonth.pilot$YearMonth <- paste0(wavesMRCMonth.pilot$YearMonth,"-01")
      wavesMRCMonth.pilot$YearMonth <- as.Date(wavesMRCMonth.pilot$YearMonth,format="%Y-%m-%d")
      
      # Plotting in ggplot2
      ggplot() +
        geom_line(data=wavesMRCMonth.np
                  , aes(x = YearMonth, y = DiscountMRC_USD,color='Non-Pilot'),size=1) +
        geom_line(data=wavesMRCMonth.pilot
                  , aes(x = YearMonth, y = DiscountMRC_USD,color='In Pilot'),size=1) +
        ylab("Average MRC") + xlab("Date") + ggtitle("Average MRR Over Time") +
        theme(text = element_text(size = 16)) + 
        scale_color_manual(name="Series",
                           breaks=c("Non-Pilot","In Pilot"),
                           values=c("Non-Pilot"='red2',"In Pilot"='blue4'))
        
    })
    
    
    #output$testTable <- renderTable({
    #  inputDataset <- elsePilotInput()
    #  inputSC <- input$subChannel
    #  inputTier <- input$tier
    #  inputVert <- input$vertical
    #  counts <- nrow(inputDataset)
    #  Descriptor <- c("Sub Channel","Tier","Vertical","Count")
    #  Value <- c(inputSC,inputTier,inputVert,counts)
    #  data.frame(cbind(Descriptor,Value))
    #  })
    
    
    output$AvgMRRMile <- renderPlot({ 
      wavesNP <- NPInput()
      wavesPilot <- PilotInput()
      
      # Fix non-pilot data
      wavesMRCMileMonth.np <- aggregate(MRCperMile ~ YearMonth, subset(wavesNP,Won==1), FUN = mean)
      wavesMRCMileMonth.np$YearMonth <- paste0(wavesMRCMileMonth.np$YearMonth,"-01")
      wavesMRCMileMonth.np$YearMonth <- as.Date(wavesMRCMileMonth.np$YearMonth,format="%Y-%m-%d")
      
      # Fix In-Pilot data
      wavesMRCMileMonth.pilot <- aggregate(MRCperMile ~ YearMonth, subset(wavesPilot,Won==1), FUN = mean)
      wavesMRCMileMonth.pilot$YearMonth <- paste0(wavesMRCMileMonth.pilot$YearMonth,"-01")
      wavesMRCMileMonth.pilot$YearMonth <- as.Date(wavesMRCMileMonth.pilot$YearMonth,format="%Y-%m-%d")
      
      # Plotting in ggplot2
      ggplot() +
        geom_line(data=wavesMRCMileMonth.np
                  , aes(x = YearMonth, y = MRCperMile,color='Non-Pilot'),size=1) +
        geom_line(data=wavesMRCMileMonth.pilot
                  , aes(x = YearMonth, y = MRCperMile,color='In Pilot'),size=1) +
        ylab("Average MRC / Mile") + xlab("Date") + ggtitle("Average MRR per Mile Over Time") +
        theme(text = element_text(size = 16)) +
        scale_color_manual(name="Series",
                           breaks=c("Non-Pilot","In Pilot"),
                           values=c("Non-Pilot"='red2',"In Pilot"='blue4'))
      
    })
    
    output$WinRate <- renderPlot({ 
      wavesNP <- NPInput()
      wavesPilot <- PilotInput()
      
      # Fix non-pilot data
      wavesWR.np <- aggregate(Won ~ YearMonth, wavesNP, FUN = mean)
      wavesWR.np$YearMonth <- paste0(wavesWR.np$YearMonth,"-01")
      wavesWR.np$YearMonth <- as.Date(wavesWR.np$YearMonth,format="%Y-%m-%d")
      
      # Fix In-Pilot data
      wavesWR.pilot <- aggregate(Won ~ YearMonth, wavesPilot, FUN = mean)
      wavesWR.pilot$YearMonth <- paste0(wavesWR.pilot$YearMonth,"-01")
      wavesWR.pilot$YearMonth <- as.Date(wavesWR.pilot$YearMonth,format="%Y-%m-%d")
      
      # Plotting in ggplot2
      ggplot() +
        geom_line(data=wavesWR.np
                  , aes(x = YearMonth, y = Won,color='Non-Pilot'),size=1) +
        geom_line(data=wavesWR.pilot
                  , aes(x = YearMonth, y = Won,color='In Pilot'),size=1) +
        ylab("Win Rate") + xlab("Date") + ggtitle("Average Win Rate Over Time") +
        ylim(0,1) + theme(text = element_text(size = 16)) +
        scale_color_manual(name="Series",
                           breaks=c("Non-Pilot","In Pilot"),
                           values=c("Non-Pilot"='red2',"In Pilot"='blue4'))
      
    })

    output$AvgDSC <- renderPlot({ 
      wavesNP <- NPInput()
      wavesPilot <- PilotInput()
      
      # Fix non-pilot data
      wavesDSC.np <- aggregate(PctDiscount ~ YearMonth, subset(wavesNP,Won==1), FUN = mean)
      wavesDSC.np$YearMonth <- paste0(wavesDSC.np$YearMonth,"-01")
      wavesDSC.np$YearMonth <- as.Date(wavesDSC.np$YearMonth,format="%Y-%m-%d")
      
      # Fix In-Pilot data
      wavesDSC.pilot <- aggregate(PctDiscount ~ YearMonth, subset(wavesPilot,Won==1), FUN = mean)
      wavesDSC.pilot$YearMonth <- paste0(wavesDSC.pilot$YearMonth,"-01")
      wavesDSC.pilot$YearMonth <- as.Date(wavesDSC.pilot$YearMonth,format="%Y-%m-%d")
      
      # Plotting in ggplot2
      ggplot() +
        geom_line(data=wavesDSC.np
                  , aes(x = YearMonth, y = PctDiscount,color='Non-Pilot'),size=1) +
        geom_line(data=wavesDSC.pilot
                  , aes(x = YearMonth, y = PctDiscount,color='In Pilot'),size=1) +
        ylab("Percent Discount") + xlab("Date") + ggtitle("Average % Discount Over Time") +
        ylim(0,1) + theme(text = element_text(size = 16)) +
        scale_color_manual(name="Series",
                           breaks=c("Non-Pilot","In Pilot"),
                           values=c("Non-Pilot"='red2',"In Pilot"='blue4'))
    })
    
    output$DscHist <- renderPlot({ 
      histData <- AllWavesData()
      noPilot <- data.frame(subset(histData, InPilot == 0)$PctDiscount)
      pilot <- data.frame(subset(histData, InPilot == 1)$PctDiscount)
      noPilot$Series <- rep('Non-Pilot',nrow(noPilot))
      pilot$Series <- rep('In Pilot',nrow(pilot))
      names(noPilot) <- c("Discount","Series")
      names(pilot) <- c("Discount","Series")
      discounts <- rbind(noPilot,pilot)
      
      
      # Plotting in ggplot2
      #ggplot(discounts, aes(Discount, fill = yn)) + geom_bar(pos="dodge") + xlim(0,1)
      
      ggplot(discounts, aes(Discount, fill = Series)) + geom_density(alpha = 0.2) + xlim(0,1) +
        theme(text = element_text(size = 16)) + 
        ylab("Discount Density") + xlab("Discount Level") + 
        ggtitle("Discount Distribution")
      
    })
    
    output$WavesInfo <- renderDataTable({
      wavesPilot <- PilotInput()
      dataTable <- wavesPilot[c("companyname","LineItemUpdate"
                              ,"ACity","ZCity","Miles"
                              ,"DiscountMRC_USD","MRC_USD"
                              ,"PctDiscount")]
      names(dataTable) <- c("Company Name","Date","City A","City Z"
                            ,"Mileage","Wave MRC","List MRC","Pct Dicsount")
      
      dataTable
    })
    
    output$summary <- renderTable({
      SummaryData <- AllWavesData()
      SummaryData$BelowCSG <- ifelse(SummaryData$PctDiscount <= 0.3,1,0)
      SummaryData$CSG <- ifelse((SummaryData$PctDiscount > 0.3 & SummaryData$PctDiscount <= 0.5)
                                ,1,0)
      SummaryData$Product <- ifelse(SummaryData$PctDiscount > 0.5,1,0)
      # Structure: AvgDiscount, Win Rates, within 0.3, Within CSG, product
      KPI <- c("Average Discount","Win Rate",
               "Pct Within Channel Head","Pct CSG","Pct Product")
      AvgDsc <- round(tapply(SummaryData$PctDiscount,SummaryData$InPilot,mean)[2],digits=2)
      TQ <- nrow(subset(SummaryData,InPilot==1))
      TW <- sum(subset(SummaryData,InPilot==1)$Won)
      NewMRR <- sum(subset(SummaryData,Won==1 & InPilot==1)$DiscountMRC_USD)
      NewMRR <- paste0("$",round(NewMRR,digits=0))
      WR <- round(mean(subset(SummaryData, InPilot==1)$Won),digits=2)
      CH <- round(mean(subset(SummaryData, InPilot==1)$BelowCSG),digits=2)
      CSGf <- round(mean(subset(SummaryData, InPilot==1)$CSG),digits=2)
      Prod <- round(mean(subset(SummaryData, InPilot==1)$Product),digits=2)
      Observed <- c(AvgDsc,WR,CH,CSGf,Prod)
      Target <- c(0.3,0.6,0.55,0.3,0.15)
      summaryTable <- data.frame(cbind(KPI,percent(Observed),percent(Target)))
      names(summaryTable) <- c("KPI","Observed","Target")
      summaryTable
      
      
    })
    
    output$totQuotes <- renderPlot({ 
      wavesNP <- NPInput()
      wavesPilot <- PilotInput()
      
      # Fix non-pilot data
      waves.np <- wavesNP
      waves.np$ones <- rep(1,nrow(wavesNP))
      waves.np <- aggregate(ones ~ YearMonth, waves.np, FUN = sum)
      waves.np$YearMonth <- paste0(waves.np$YearMonth,"-01")
      waves.np$YearMonth <- as.Date(waves.np$YearMonth,format="%Y-%m-%d")
      
      # Fix In-Pilot data
      waves.pilot <- wavesPilot
      waves.pilot$ones <- rep(1,nrow(wavesPilot))
      waves.pilot <- aggregate(ones ~ YearMonth, waves.pilot, FUN = sum)
      waves.pilot$YearMonth <- paste0(waves.pilot$YearMonth,"-01")
      waves.pilot$YearMonth <- as.Date(waves.pilot$YearMonth,format="%Y-%m-%d")
      
      # Plotting in ggplot2
      ggplot() +
        geom_line(data=waves.np
                  , aes(x = YearMonth, y = ones,color='Non-Pilot'),size=1) +
        geom_line(data=waves.pilot
                  , aes(x = YearMonth, y = ones,color='In Pilot'),size=1) +
        ylab("Number of Quotes") + xlab("Date") + ggtitle("Quote Volume Over Time") +
        theme(text = element_text(size = 16)) +
        scale_color_manual(name="Series",
                           breaks=c("Non-Pilot","In Pilot"),
                           values=c("Non-Pilot"='red2',"In Pilot"='blue4'))
    })
    
    output$totWins <- renderPlot({ 
      wavesNP <- NPInput()
      wavesPilot <- PilotInput()
      
      # Fix non-pilot data
      waves.np <- subset(wavesNP, Won == 1)
      waves.np$ones <- rep(1,nrow(waves.np))
      waves.np <- aggregate(ones ~ YearMonth, waves.np, FUN = sum)
      waves.np$YearMonth <- paste0(waves.np$YearMonth,"-01")
      waves.np$YearMonth <- as.Date(waves.np$YearMonth,format="%Y-%m-%d")
      
      # Fix In-Pilot data
      waves.pilot <- subset(wavesPilot, Won == 1)
      waves.pilot$ones <- rep(1,nrow(waves.pilot))
      waves.pilot <- aggregate(ones ~ YearMonth, waves.pilot, FUN = sum)
      waves.pilot$YearMonth <- paste0(waves.pilot$YearMonth,"-01")
      waves.pilot$YearMonth <- as.Date(waves.pilot$YearMonth,format="%Y-%m-%d")
      
      # Plotting in ggplot2
      ggplot() +
        geom_line(data=waves.np
                  , aes(x = YearMonth, y = ones,color='Non-Pilot'),size=1) +
        geom_line(data=waves.pilot
                  , aes(x = YearMonth, y = ones,color='In Pilot'),size=1) +
        ylab("Number of Wins") + xlab("Date") + ggtitle("Win Volume Over Time") +
        theme(text = element_text(size = 16)) +
        scale_color_manual(name="Series",
                           breaks=c("Non-Pilot","In Pilot"),
                           values=c("Non-Pilot"='red2',"In Pilot"='blue4'))
    })
    
    output$mrrTS <- renderPlot({ 
      wavesNP <- NPInput()
      wavesPilot <- PilotInput()
      
      # Fix non-pilot data
      waves.np <- subset(wavesNP, Won == 1)
      #waves.np$ones <- rep(1,nrow(waves.np))
      waves.np <- aggregate(DiscountMRC_USD ~ YearMonth, waves.np, FUN = sum)
      waves.np$YearMonth <- paste0(waves.np$YearMonth,"-01")
      waves.np$YearMonth <- as.Date(waves.np$YearMonth,format="%Y-%m-%d")
      
      # Fix In-Pilot data
      waves.pilot <- subset(wavesPilot, Won == 1)
      #waves.pilot$ones <- rep(1,nrow(waves.pilot))
      waves.pilot <- aggregate(DiscountMRC_USD ~ YearMonth, waves.pilot, FUN = sum)
      waves.pilot$YearMonth <- paste0(waves.pilot$YearMonth,"-01")
      waves.pilot$YearMonth <- as.Date(waves.pilot$YearMonth,format="%Y-%m-%d")
      
      # Plotting in ggplot2
      ggplot() +
        geom_line(data=waves.np
                  , aes(x = YearMonth, y = DiscountMRC_USD,color='Non-Pilot'),size=1) +
        geom_line(data=waves.pilot
                  , aes(x = YearMonth, y = DiscountMRC_USD,color='In Pilot'),size=1) +
        ylab("MRR") + xlab("Date") + ggtitle("MRR of New Sales") +
        theme(text = element_text(size = 16)) +
        scale_color_manual(name="Series",
                           breaks=c("Non-Pilot","In Pilot"),
                           values=c("Non-Pilot"='red2',"In Pilot"='blue4'))
    })
    
    
})
