library(shiny)
library(epiR)

epi.tests.bayes <- function(dat,
                            prev = NULL,
                            prev_ci = NULL,
                            conf.level = 0.95,
                            n_sim = 100000,
                            seed = 1) {
  
  # --- input ---
  if (is.vector(dat) && length(dat) == 4) {
    TP <- dat[1]; FP <- dat[2]; FN <- dat[3]; TN <- dat[4]
    tab <- matrix(c(TP, FP, FN, TN), nrow = 2, byrow = TRUE)
  } else if (is.matrix(dat) && all(dim(dat) == c(2,2))) {
    TP <- dat[1,1]; FP <- dat[1,2]; FN <- dat[2,1]; TN <- dat[2,2]
    tab <- dat
  } else {
    stop("dat is c(TP, FP, FN, TN) or matrix 2x2")
  }
  
  set.seed(seed)
  
  # --- Bayesian core ---
  Se_sim <- rbeta(n_sim, TP + 1, FN + 1)
  Sp_sim <- rbeta(n_sim, TN + 1, FP + 1)
  
  if (!is.null(prev_ci)) {
    m <- mean(prev_ci)
    sd <- (prev_ci[2] - prev_ci[1]) / (2 * 1.96)
    alpha <- ((1 - m) / sd^2 - 1 / m) * m^2
    beta <- alpha * (1 / m - 1)
    Prev_sim <- rbeta(n_sim, alpha, beta)
  } else if (!is.null(prev)) {
    Prev_sim <- rep(prev, n_sim)
  } else {
    Prev_sim <- rep((TP + FN) / sum(tab), n_sim)
  }
  
  # --- metrics ---
  AppPrev <- Se_sim * Prev_sim + (1 - Sp_sim) * (1 - Prev_sim)
  PPV <- (Se_sim * Prev_sim) / (Se_sim * Prev_sim + (1 - Sp_sim) * (1 - Prev_sim))
  NPV <- (Sp_sim * (1 - Prev_sim)) / ((1 - Se_sim) * Prev_sim + Sp_sim * (1 - Prev_sim))
  
  LR_pos <- Se_sim / (1 - Sp_sim)
  LR_neg <- (1 - Se_sim) / Sp_sim
  
  FPR_Dminus <- 1 - Sp_sim
  FNR_Dplus <- 1 - Se_sim
  FPR_Tplus <- 1 - PPV
  FNR_Tminus <- 1 - NPV
  
  Accuracy <- Se_sim * Prev_sim + Sp_sim * (1 - Prev_sim)
  
  alpha <- (1 - conf.level)/2
  
  sm <- function(x) {
    c(est = median(x),
      lower = unname(quantile(x, alpha)),
      upper = unname(quantile(x, 1 - alpha)))
  }
  
  detail <- list(
    ap = sm(AppPrev),
    tp = sm(Prev_sim),
    se = sm(Se_sim),
    sp = sm(Sp_sim),
    ppv = sm(PPV),
    npv = sm(NPV),
    lr.pos = sm(LR_pos),
    lr.neg = sm(LR_neg),
    pfp = sm(FPR_Dminus),
    pfn = sm(FNR_Dplus),
    pfp.pt = sm(FPR_Tplus),
    pfn.nt = sm(FNR_Tminus),
    acc = sm(Accuracy)
  )
  
  out <- list(
    tab = tab,
    detail = detail,
    method = "Bayesian diagnostic test evaluation",
    conf.level = conf.level,
    n_sim = n_sim
  )
  
  class(out) <- "epi.tests.bayes"
  return(out)
}

print.epi.tests.bayes <- function(x, digits = 2, ...) {
  tab <- x$tab
  
  TP <- tab[1,1]; FP <- tab[1,2]
  FN <- tab[2,1]; TN <- tab[2,2]
  
  total1 <- TP + FP
  total2 <- FN + TN
  grand  <- sum(tab)
  
  cat("          Outcome +    Outcome -      Total\n")
  cat(sprintf("Test + %12d %12d %10d\n", TP, FP, total1))
  cat(sprintf("Test - %12d %12d %10d\n", FN, TN, total2))
  cat(sprintf("Total  %12d %12d %10d\n", TP+FN, FP+TN, grand))
  
  cat("\nBayesian posterior summaries (median and ", x$conf.level * 100, "% CIs):\n", sep = "")
  cat("--------------------------------------------------------------\n")
  
  fmt <- function(v) {
    sprintf(paste0("%.", digits, "f (%.", digits, "f, %.", digits, "f)"),
            v["est"], v["lower"], v["upper"])
  }
  
  d <- x$detail
  
  lines <- c(
    sprintf("Apparent prevalence *                 %s", fmt(d$ap)),
    sprintf("True prevalence *                     %s", fmt(d$tp)),
    sprintf("Sensitivity *                         %s", fmt(d$se)),
    sprintf("Specificity *                         %s", fmt(d$sp)),
    sprintf("Positive predictive value *           %s", fmt(d$ppv)),
    sprintf("Negative predictive value *           %s", fmt(d$npv)),
    sprintf("Positive likelihood ratio             %s", fmt(d$lr.pos)),
    sprintf("Negative likelihood ratio             %s", fmt(d$lr.neg)),
    sprintf("False T+ proportion for true D- *     %s", fmt(d$pfp)),
    sprintf("False T- proportion for true D+ *     %s", fmt(d$pfn)),
    sprintf("False T+ proportion for T+ *          %s", fmt(d$pfp.pt)),
    sprintf("False T- proportion for T- *          %s", fmt(d$pfn.nt)),
    sprintf("Correctly classified proportion *     %s", fmt(d$acc))
  )
  
  cat(paste(lines, collapse = "\n"))
  cat("\n--------------------------------------------------------------\n")
  cat("* Bayesian credible interval (Beta posterior + Monte Carlo, not exact frequentist CIs!)\n\n")
}

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
        tags$td(numericInput("fpNumber", "FP", 20)),
        tags$td(textOutput("tpfpTotal"))
      ),
      tags$tr(
        tags$td("Test N (Wt)"),
        tags$td(numericInput("fnNumber", "FN", 50)),
        tags$td(numericInput("tnNumber", "TN", 200)),
        tags$td(textOutput("fntnTotal"))
      ),
      tags$tr(
        tags$td(),
        tags$td(textOutput("tpfnTotal")),
        tags$td(textOutput("fptnTotal")),
        tags$td(tags$b(textOutput("grandTotal")))
      )
    )),
  
  numericInput("confLvl", "Confidence level, %", 95, max = 100),
  
  
  
  tags$h2("Result"),
  fluidRow(
    column(width = 6,
           
    ),
    column(width = 6,
           HTML("Bayesian test with true population disease prevalence."),
           fluidRow(
             column(6, numericInput("prevalence", "Disease Prevalence, %", 0, max = 100)),
             column(3, numericInput("prevalenceCIlow", "CI lower, %", NULL)),
             column(3, numericInput("prevalenceCIupp", "CI upper, %", NULL))
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
  
  output$calcResult <- renderPrint({
    dat <- 
      c(input$tpNumber, input$fpNumber,
        input$fnNumber, input$tnNumber)
    tryCatch(
      epi.tests(dat, conf.level = input$confLvl / 100,
                digits = 4),
      error = function(e) print("Check input!")
    )
  })
  
  output$calcResult2 <- renderPrint({
    
    tryCatch({
      dat <- 
        c(input$tpNumber, input$fpNumber,
          input$fnNumber, input$tnNumber)
      print(epi.tests.bayes(dat, 
                            prev = input$prevalence / 100,
                            prev_ci = 
                              if (!is.na(input$prevalenceCIlow) && !is.na(input$prevalenceCIupp)) {
                                c(input$prevalenceCIlow / 100,
                                  input$prevalenceCIupp / 100)
                              } else {
                                NULL
                              },
                            conf.level = input$confLvl / 100),
            digits = 4)
    },
    error = function(e) print("Check input!")
    )
  })
  
  
}

shinyApp(ui = ui, server = server)
