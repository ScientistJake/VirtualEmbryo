library(plotly)
library(shiny)
library(dplyr)

ui <- fluidPage(
  h1("Virtual Urchin!"),
  plotlyOutput("plot"),
  # verbatimTextOutput("hover"),
  verbatimTextOutput("click"),
  sliderInput("slicer","Slice Embryo!",min=-1, max=1,value = 1,step = 0.1),
  h4("This is where data will show up.")
)


server <- function(input, output, session) {
  
  #make a spherical embryo
  set.seed(101)
  n <- 2000
  theta <- runif(n,0,2*pi)
  u <- runif(n,-1,1)
  x <- sqrt(1-u^2)*cos(theta)
  y <- sqrt(1-u^2)*sin(theta)
  z <- u
  
  #Make some PMCs 'inside'
  nPMC <- 64
  thetaPMC <- runif(nPMC,0,2*pi)
  uPMC <- runif(nPMC,-1,-0.8)
  xPMC <- (sqrt(1-uPMC^2)*cos(thetaPMC))*0.5
  yPMC <- (sqrt(1-uPMC^2)*sin(thetaPMC))*0.5
  zPMC <- uPMC
  
  embryo <- data.frame(x=x,y=y,z=z)
  PMC <- data.frame(x=xPMC,y=yPMC,z=zPMC, territory = 4)
  
  #designate territories
  embryo$territory <- ifelse(embryo$z>0.9, 1,0)
  embryo$territory <- ifelse(embryo$z>-0.8 & embryo$z< -0.4, 2, embryo$territory)
  embryo$territory <- ifelse(embryo$z< -0.81 & embryo$x< -0.01,3,embryo$territory)
  
  #add the coordinates
  embryo <- rbind(embryo,PMC)
  
  #This will subset the coordinates based on the slicer
  embryoPlot <- reactive({
    embryo[which(embryo$x < input$slicer),]
  })

  output$plot <- renderPlotly({
    p <- plot_ly(embryoPlot(), 
                 x = ~x, y = ~y, z = ~z, 
                 color = ~territory, 
                 colors = c('#0C4B8E','#BF382A', '#FF7070','#4AC6B7','#F442EE'),
                 type = "scatter3d",
                 mode="markers") %>%
                add_markers()
  })
  
  output$click <- renderPrint({
    d <- event_data("plotly_click")
    if (!is.null(d)){
      territory <- embryoPlot()$territory[d$pointNumber + 1]
      if (territory == 0){
        return("You Clicked Ectoderm")
      } else if (territory == 1){
        return("You Clicked Neuro-ectoderm")
      } else if (territory == 2){
        return ("You Clicked Endoderm")
      } else if (territory == 3){
        return ("You Clicked a Pigment Cell")
      } else if (territory == 4){
        return ("You Clicked a PMC")
      }
    } else {
      return("Click a territory")
    }
  })
  
}

shinyApp(ui, server)
