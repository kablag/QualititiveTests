library(shiny)
library(epiR)

ui <- fluidPage(

  # Application title
  titlePanel("Qualitative Tests"),
  wellPanel(
  tags$table(
    tags$tr(
      tags$td(),
      tags$td("Gold P (M)"),
      tags$td("Gold N (Wt)"),
      tags$td()
    ),
    tags$tr(
      tags$td("Test P (M)"),
      tags$td(numericInput("tpNumber", "TP", 100)),
      tags$td(numericInput("fnNumber", "FN", 0)),
      tags$td(textOutput("tpfnTotal"))
    ),
    tags$tr(
      tags$td("Test N (Wt)"),
      tags$td(numericInput("fpNumber", "FP", 0)),
      tags$td(numericInput("tnNumber", "TN", 100)),
      tags$td(textOutput("fptnTotal"))
    ),
    tags$tr(
      tags$td(),
      tags$td(textOutput("tpfpTotal")),
      tags$td(textOutput("fntnTotal")),
      tags$td(tags$b(textOutput("grandTotal")))
    )
  )),
  numericInput("confLvl", "Confidence level, %", 95, max = 100),
  tags$h2("Result"),
  verbatimTextOutput("calcResult")
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  output$tpfnTotal <- renderText({
    input$tpNumber + input$fnNumber
  })
  output$fptnTotal <- renderText({
    input$fpNumber + input$tnNumber
  })
  output$tpfpTotal <- renderText({
    input$tpNumber + input$fpNumber
  })
  output$fntnTotal <- renderText({
    input$fnNumber + input$tnNumber
  })
  output$grandTotal <- renderText({
    input$tpNumber + input$fpNumber + input$fnNumber + input$tnNumber
  })

  output$calcResult <- renderPrint({
    dat <- 
      c(input$tpNumber, input$fnNumber,
        input$fpNumber, input$tnNumber)
    tryCatch(
      print(epi.tests(dat, conf.level = input$confLvl / 100)),
      error = function(e) print("Check input!")
    )
  })
}

shinyApp(ui = ui, server = server)
