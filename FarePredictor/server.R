setwd("/Users/homanaren/Documents/UI/Spring2017/CS504-DATA_SCIENCE/FinalProject/FarePredictor")
data_sets =  read.csv("AirportCodes.csv",header = TRUE,sep=",") 

data_sets

shinyServer(function(input, output) {
 
  output$choose_from <- renderUI({
    selectInput("fromAirport", "Choose from Airport", as.vector(data_sets["airport"]) )
  })
  
  output$choose_to <- renderUI({
    selectInput("toAirport", "Choose to Airport", as.vector(data_sets["airport"]) )
  })
  
  datasetInput <- reactive({
    dat <- input$fromAirport
    
  })
  datasetInput2 <- reactive({
    dat2 <- input$toAirport
  })
  
  output$result <- renderPlot({
    #airfare1()
    inFile <- input$file1
    if(is.null(inFile))
      return(NULL)
    test <- read.csv(inFile$datapath, header = TRUE, sep = ",")
    air = test[test$airport_1 == datasetInput() & test$airport_2 == datasetInput2(),]

    plotType <- input$plot
    if(datasetInput() == datasetInput2()) 
      return(NULL)
    if(plotType == "Histogram") {
    ggplot(air,aes(x=fare,group=quarter,fill=quarter))+
      geom_histogram(position="dodge",binwidth=5)+theme_bw()
    } else if(plotType == "Bar Chart") {
     
      ggplot(air, aes(x=Year, y=fare, fill=quarter)) +
        # plot the bars
        geom_bar(stat="identity", position="dodge") +
        # create the label, "dodged" to fit the bars
        geom_text(aes(label=fare), vjust=1.5, colour="white",
                  position=position_dodge(.9), size=3)
    } else {
      air1 <- test[test$airport1 == datasetInput(),]
      air2 <- names(air1) %in% "airport"
      
      aircode <- input$file2
      if(is.null(inFile))
        return(NULL)
      airportCode <- read.csv(aircode$datapath, header = TRUE, sep = ",")
      Lat1 <- airportCode[airportCode$airport == datasetInput(), ]$latitude
      Lon1 <- airportCode[airportCode$airport == datasetInput(), ]$longitude
      Lat2 <- airportCode[airportCode$airport == datasetInput2(), ]$latitude
      Lon2 <- airportCode[airportCode$airport == datasetInput2(), ]$longitude
      
      p1 = c(Lon1, Lon2)
      p2 = c(Lat1, Lat2)
  
      map('state', col = "cyan", fill = T, #xlim = xlim, 
          boundary = T, interior = TRUE)
      inter <- gcIntermediate(c(Lon1, Lat1), c(Lon2, Lat2), n=150, addStartEnd=TRUE)
      lines(inter, col = 'red', lwd = 2)
      points(x = p1, y = p2, col = "red", pch = 19,cex = 1.5)
      text(Lon1, Lat1, datasetInput(), col = 'blue', adj = c(-0.3, 1.25))
      text(Lon2, Lat2, datasetInput2(), col = 'blue', adj = c(-0.3, 1.25))
      
    } 
  })  
  
  #Plot from Airport to all airports
  output$result5 <- renderPlot({
    inFile <- input$file1
    aircode <- input$file2
    if(is.null(inFile))
      return(NULL)
    if(is.null(aircode))
      return(NULL)
    plotType <- input$plot
    test <- read.csv(inFile$datapath, header = TRUE, sep = ",")
    air1 <- subset(test,airport_1 == datasetInput() , 
                   select=c(airport_1,airport_2))
    air2 <- air1[!duplicated(air1$airport_2),]
    airportCode <- read.csv(aircode$datapath, header = TRUE, sep = ",")
    if(plotType == "Map") {
      map('state', col = "cyan", fill = T, #xlim = xlim, 
        boundary = T, interior = TRUE)
      Lat1 <- airportCode[airportCode$airport == datasetInput(), ]$latitude
      Lon1 <- airportCode[airportCode$airport == datasetInput(), ]$longitude
      text(Lon1, Lat1, datasetInput(), col = 'blue', adj = c(-0.3, 1.25), cex = 1.0)
      for(i in 1:nrow(air2)) {
        row <- air2[i,]

        Lat2 <- airportCode[airportCode$airport == factor(row[,2], levels = levels(airportCode[,1])), ]$latitude
        Lon2 <- airportCode[airportCode$airport == factor(row[,2], levels = levels(airportCode[,1])), ]$longitude
        
        p1 = c(Lon1, Lon2)
        p2 = c(Lat1, Lat2)
  
        inter <- gcIntermediate(c(Lon1, Lat1), c(Lon2, Lat2), n=50, addStartEnd=TRUE)
        lines(inter, col = 'red', lwd = 2)
        points(x = p1, y = p2, col = "red", pch = 19,cex = 1.5)
        text(Lon2, Lat2, row$airport_2, col = 'blue', adj = c(-0.3, 1.25), cex = 1.0)
      }
    }
  })
  
  #Output Table 
   output$result1 <- renderTable({
    inFile <- input$file1
    if(is.null(inFile))
      return(NULL)
    if(datasetInput() == datasetInput2()) 
      return(NULL)
    
    test <- read.csv(inFile$datapath, header = TRUE, sep = ",")
    air = test[test$airport_1 == datasetInput() & test$airport_2 == datasetInput2(),]
    fareSelect = input$fare
    if(fareSelect == "Show Data Table") {
      air
    }
    else if(fareSelect == "Average Fare") {
     setNames(aggregate(air[,5], list(air$quarter) , FUN = "mean"),c("Quarter","AverageFare"))
    }
  })
  output$result2 <- renderTable({
    inFile <- input$file1
    if(is.null(inFile))
      return(NULL)
    if(datasetInput() == datasetInput2()) 
      return(NULL)
    test <- read.csv(inFile$datapath, header = TRUE, sep = ",")
    air = test[test$airport_1 == datasetInput() & test$airport_2 == datasetInput2(),]
    fareSelect = input$fare
    if(fareSelect == "Average Fare") {
      setNames(aggregate(air[,5], list(air$quarter) , FUN = "min"),c("Quarter","Minimum Fare"))
    }
  })
  output$result3 <- renderTable({
    inFile <- input$file1
    if(is.null(inFile))
      return(NULL)
    if(datasetInput() == datasetInput2()) 
      return(NULL)
    test <- read.csv(inFile$datapath, header = TRUE, sep = ",")
    air = test[test$airport_1 == datasetInput() & test$airport_2 == datasetInput2(),]
    fareSelect = input$fare
    if(fareSelect == "Average Fare") {
      setNames(aggregate(air[,5], list(air$quarter) , FUN = "max"),c("Quarter","MaximumFare"))
    }
  })
  output$result4 <- renderPrint({
    inFile <- input$file1
    if(is.null(inFile))
      return(NULL)
    if(datasetInput() == datasetInput2()) 
      return(NULL)
    test <- read.csv(inFile$datapath, header = TRUE, sep = ",")
    air = test[test$airport_1 == datasetInput() & test$airport_2 == datasetInput2(),]
    air1 = air[5:9]
    fareSelect = input$fare
    if(fareSelect == "summary") {
      summary(air1)
    }
  })
  
  
})