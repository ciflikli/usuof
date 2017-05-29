source("global.R",local=TRUE)
ui <- fluidPage(
  titlePanel("US Power and Partnership 1816-2006"),
  sidebarLayout(
    sidebarPanel(
      h3("Filtering Data"),
      selectInput("dataset","Choose a data pre-processing method:", 
                  choices=c("Scaled", "Centered", "Standardised",
                            "Normalised","Yeo-Johnson Transformed")),
      h5(textOutput("caption",container=span)),
      h3("Scatterplot"),
      selectInput("zcol","Time Variable (4th-43rd Presidencies) [Fixed]","code"),
      selectInput("party","Party Subset",choices=c("Both","Democrat","Republican")),
      selectInput("xcol",label=HTML("Power Variable &#9675;"),""),
      selectInput("ycol",label=HTML("Partnership Variable &#9650;"),""),
      h3("K-Means"),
      numericInput("clusters","Cluster Count",3,min=1,max=9),
      numericInput("obs","Number of presidencies to be included on data table:",10,min=1,max=43)
    )
    ,
    mainPanel(
      tabsetPanel(
        tabPanel("Plot",h1("Power and Partnership Patterns"),
                 plotOutput("plot1")),
        tabPanel("Clustering",h1("K-Means Clustering"),textOutput("NbClust"),
                 plotOutput("plot2"),plotOutput("plot3")),
        tabPanel("Data Table", h1("Data Table"),textOutput("NbRows"),tableOutput("view")),
        tabPanel("Descriptive Statistics",h1("Summary Statistics"),verbatimTextOutput("summary"),
                 htmlOutput("Desc"))
      )
    )
  )
)
server <- function(input,output,session){
  datasetInput <- reactive({
    switch(input$dataset,
           "Scaled" = scaled,
           "Centered" = centered,
           "Standardised" = standardised,
           "Normalised" = normalised,
           "Yeo-Johnson Transformed" = yeojonhson)
  })
  caption <- c("calculates the standard deviation for an attribute and divides each value by that standard deviation",
               "calculates the mean for an attribute and subtracts it from each value",
               "combines scale and center; attributes will have a mean value of 0 and a standard deviation of 1",
               "scales the data into the range of [0,1]",
               "power-transforms like the Box-Cox, but it supports raw values that are equal to zero and negative")
  captionInput <- reactive({
    switch(input$dataset,
           "Scaled" = caption[1],
           "Centered" = caption[2],
           "Standardised" = caption[3],
           "Normalised" = caption[4],
           "Yeo-Johnson Transformed" = caption[5])
  })
  output$caption <- renderText({
    caption <- captionInput()
    print(caption)
  })
  observeEvent(input$dataset,{
    updateSelectInput(session,"xcol",choices=names(datasetInput()[,6:11]))
  })
  observeEvent(input$dataset,{
    updateSelectInput(session,"ycol",choices=names(datasetInput()[,12:17]))
  })
  selectedData <- reactive({
    dataset <- datasetInput()
    dataset[,c(input$xcol,input$ycol,input$zcol,"republican")]
  })
  selectedParty <- reactive({
    switch(input$party,
           "Both"=selectedData(),
           "Democrat"=selectedData()[selectedData()[,4]==0,],
           "Republican"=selectedData()[selectedData()[,4]==1,])
  })
  clusters <- reactive({
    kmeans(selectedData(),input$clusters)
  })
  output$plot1 <- renderPlot({
    palette(c("#377EB8","#E41A1C"))
    plot(x=selectedParty()[,3],y=selectedParty()[,2],
         pch=17,cex=2,
         col=ifelse(input$party=="Both","#4DAF4A",ifelse(input$party=="Democrat","#377EB8","#E41A1C")),
         xlab="Presidencies, Sorted Chronologically 4th-43rd",
         ylab="Parameter Values",
         ylim=c(min(selectedParty()[,1],selectedParty()[,2]),max(selectedParty()[,1],selectedParty()[,2])), 
         points(selectedParty()[,2],cex=2,
                col=ifelse(input$party=="Both","#4DAF4A",ifelse(input$party=="Democrat","#377EB8","#E41A1C"))))
  })
  output$summary <- renderPrint({
    dataset <- datasetInput()
    summary(dataset[,6:17],digits=4)
  })
  output$view <- renderTable({
    head(datasetInput()[,c(1,6:17)],n=input$obs)
  })
  output$NbRows <- renderText({ 
    paste("You have selected to display",input$obs,ifelse(input$obs==1,"presidency:","presidencies."))
  })
  output$NbClust <- renderText({ 
    paste("K-means clustering performed with ",input$clusters,ifelse(input$clusters==1,"cluster.","clusters.")," It aims to partition n observations
          into k clusters in which each observation belongs to the cluster with the nearest mean, serving as a 
          prototype of the cluster.")
  })
  output$Desc <- renderUI({
    str1 <- paste("mids: Raw count of all mids during a presidency")
    str2 <- paste("midsraw2: MIDS w/hostlev 3-5 and fatalities>0 (does not incl missing -9)")
    str3 <- paste("deploy: CRS data (Grimmett)")
    str4 <- paste("midsavg: MIDs average by tenure: all")
    str5 <- paste("midsavg2: MIDS average by tenure: w/hostlev 3-5 and fatalities>0 (incl missing -9)")
    str6 <- paste("deployavg: deploy / tenure")
    str7 <- paste("treaty1: Number of treaties. Source: Margolis, 1986 (Washington-Ford); Vital Statistics")
    str8 <- paste("treaty2: War, alliances, diplomacy, settlements only. Source: Axelrod 2000")
    str9 <- paste("execagr: Number of executive agreements. Source: Margolis, 1986 (Washington-Ford); Vital Statistics")
    str10 <- paste("treat1av: treaty1 average by tenure")
    str11 <- paste("treat2av: treaty2 average by tenure")
    str12 <- paste("execagrav: execagr / tenure")
    HTML(paste(str1,str2,str3,str4,str5,str6,
               str7,str8,str9,str10,str11,str12,sep="<br/>"))
  })
  output$plot2 <- renderPlot({
    palette(c("#377EB8","#E41A1C","#4DAF4A","#984EA3",
              "#FF7F00","#31FFF8","#A65628","#F781BF","#999999"))
    par(mar=c(5.1, 4.1, 0, 1))
    plot(selectedData()[,3],selectedData()[,1],ylab=(input$xcol),
         xlab=("Presidencies, Sorted Chronologically 4th-43rd"),
         ylim=c(min(selectedData()[,1]),max(selectedData()[,1])),
         col=clusters()$cluster,
         pch=1,cex=2)
  })
  output$plot3 <- renderPlot({
    palette(c("#377EB8","#E41A1C","#4DAF4A","#984EA3",
              "#FF7F00","#31FFF8","#A65628","#F781BF","#999999"))
    par(mar=c(5.1, 4.1, 0, 1))
    plot(selectedData()[,3],selectedData()[,2],ylab=(input$ycol),
         xlab=("Presidencies, Sorted Chronologically 4th-43rd"),
         ylim=c(min(selectedData()[,2]),max(selectedData()[,2])),
         col=clusters()$cluster,
         pch=17,cex=2)
  })
  }
shinyApp(ui = ui, server = server)