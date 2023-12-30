library(shiny)
library(ggplot2)
library(tidyverse)
library(scales)
library(stringr)
# logifySlider javascript function
JS.logify <-
  "
// function to logify a sliderInput
function logifySlider (sliderId, sci = false) {
  if (sci) {
    // scientific style
    $('#'+sliderId).data('ionRangeSlider').update({
      'prettify': function (num) { return ('10<sup>'+num+'</sup>'); }
    })
  } else {
    // regular number style
    $('#'+sliderId).data('ionRangeSlider').update({
      'prettify': function (num) { return (Math.pow(10, num)); }
    })
  }
}"

# call logifySlider for each relevant sliderInput
JS.onload <-
  "
// execute upon document loading
$(document).ready(function() {
  // wait a few ms to allow other scripts to execute
  setTimeout(function() {
    // include call for each slider
    //logifySlider('log_slider', sci = false)
    logifySlider('num', sci = true)
  }, 5)})
"

# Define UI for app that draws a histogram ----
ui <- fluidPage(
  
  tags$head(tags$script(HTML(JS.logify))),
  tags$head(tags$script(HTML(JS.onload))),
  # App title ----
  titlePanel("Monte Carlo"),
  p('Ejemplo de la estimación del area de un círculo con el método Monte Carlo'),
  a("Código fuente", href="https://github.com/LecJackS/Ciencia-de-datos-con-R_Fundamentos-Estadisticos_2020/tree/master/clase25-extra-Monte-Carlo"),
  # Sidebar layout with input and output definitions ----
  sidebarLayout(
    # Sidebar panel for inputs ----
    sidebarPanel(
      
      # Input: Slider for the number of bins ----
      sliderInput(inputId = "num",
                  label = "Random points:",
                  min = 1,
                  max = 7,
                  value = c(2)),
      #numericInput('numHand', '', 327, min = 1, max = 1e7)
      withMathJax(h5("$$\\pi:$$", style="font-weight: bold;")),
      textOutput("readout1"),
      withMathJax(h5("$$Area_\\text{cuadrado} * \\frac {\\text{# puntos adentro}}{\\text{#puntos totales}} :$$", style="font-weight: bold;")),
      textOutput("readout2"),
     # fluidRow(
      #  column(6, actionButton("resample", "Re-sample"))
      #)
    ),
    
    
    # Main panel for displaying outputs ----
    mainPanel(
      
      # Output: Histogram ----
      plotOutput(outputId = "distPlot")
      
    )
  )
)

# Define server logic required to draw a histogram ----
server <- function(input, output) {
  
  output$readout1 <- reactive({
    paste0("", pi)
  })
  output$readout2 <- reactive({
    n <- 10^input$num
    xs <- runif(n, min=-1, max=1)
    ys <- runif(n, min=-1, max=1)
    paste0("", 4 * sum(sqrt(xs^2+ys^2) < 1 ) / n)
  })
  # Histogram of the Old Faithful Geyser Data ----
  # with requested number of bins
  # This expression that generates a histogram is wrapped in a call
  # to renderPlot to indicate that:
  #
  # 1. It is "reactive" and therefore should be automatically
  #    re-executed when inputs (input$bins) change
  # 2. Its output type is a plot

  output$distPlot <- renderPlot({
    n <- 10^input$num
    xs <- runif(n, min=-1, max=1)
    ys <- runif(n, min=-1, max=1)
    #x    <- faithful$waiting
    # bins <- seq(min(x), max(x), length.out = input$bins + 1)
    
   # hist(x, breaks = bins, col = "#75AADB", border = "white",
   #      xlab = "Waiting time to next eruption (in mins)",
   #      main = "Histogram of waiting times")
   get_circle_coords <- function(r = 1, ...) {
     data_frame(theta = seq(0, 2 * pi, ...),
                x     = cos(theta) * r,
                y     = sin(theta) * r)
   }
   
   circ <- get_circle_coords(length.out = 200)
  
   df <- data.frame(xs, ys)
   #plot(xs, ys, col='steelblue', pch=20)
   #draw.circle(2,4,c(1,0.66,0.33),border="purple",
   #           col=c("#ff00ff","#ff77ff","#ffccff"),lty=1,lwd=1)
   ggplot() +
     geom_point(df, mapping = aes(xs, ys, colour=(1 > sqrt(xs^2+ys^2)))) + 
     #geom_point(data.frame(x = 0, y = 0), mapping = aes(x, y), size = 154, pch = 1, , stroke = 1.5) + 
     geom_path(data.frame(x = circ$x, y = circ$y), mapping = aes(x, y) ) +
      coord_fixed()
   
   }
  )
  
}

shinyApp(ui, server)