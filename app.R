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
  fluidRow(
    column(width = 6,
           
    ),
    column(width = 6,
           fluidRow(
             column(6, numericInput("prevalence", "Disease Prevalence, %", 0, max = 100)),
             column(6, numericInput("populationN", "Population, N", 100000))
           )
    )
  ),
  fluidRow(
    column(width = 6,
           verbatimTextOutput("calcResult")
    ),
    
    column(width = 6,
           verbatimTextOutput("calcResult2")
    )
  )
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
  
  res <- reactiveVal()
  observe({
    dat <- 
      c(input$tpNumber, input$fnNumber,
        input$fpNumber, input$tnNumber)
    res(epi.tests(dat, conf.level = input$confLvl / 100))
  })
  
  output$calcResult <- renderPrint({
    tryCatch(
      print(res()),
      error = function(e) print("Check input!")
    )
  })
  
  output$calcResult2 <- renderPrint({
    
    tryCatch({
      Se <- res()$detail$est[3]
      Sp <- res()$detail$est[4]
      Prev <- input$prevalence/100
      
      N <- input$populationN
      
      # разбивка
      D <- N * Prev          # больные
      H <- N * (1 - Prev)    # здоровые
      
      # расчет ячеек
      TP <- Se * D
      FN <- (1 - Se) * D
      TN <- Sp * H
      FP <- (1 - Sp) * H
      
      # таблица
      tab <- matrix(c(TP, FP, FN, TN), nrow = 2, byrow = TRUE)
      print(epi.tests(tab))
      },
      error = function(e) print("Check input!")
    )
  })
  
  
}

shinyApp(ui = ui, server = server)
