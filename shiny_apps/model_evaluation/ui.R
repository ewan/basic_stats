library(shiny)
library(dplyr)

shinyUI(fluidPage(
  headerPanel("Understanding test statistics"),
  sidebarPanel(
    textInput('n_expts', "Number of experiments", 100),
    textInput('n_type1', "Size of group 1", nrow(dplyr::filter(tr_simple, text_type == "Type 1"))),
    textInput('n_type2', "Size of group 2", nrow(dplyr::filter(tr_simple, text_type == "Type 2"))),
    selectInput('distribution', "Type of distribution",
                list("Gaussian mixture", "Gaussian", "Reference density",
                     "Reference bootstrap", "Reference normal"),
                selected="Gaussian mixture"),    
    selectInput('test_stat', "Test statistic",
                list("# of s.d. difference in means",
                     "Absolute difference in means",
                     "Smoothed symmetric KL divergence",
                     "Absolute Welch's t statistic"),
                selected="# of s.d. difference in means"),
    selectInput('data', "Reference data", list("Simulated", "Actual")),
    textInput('seed', "Random seed", 1)
  ),
  mainPanel(
    verticalLayout(
    plotOutput("plot_stat"),
    plotOutput("plot_pstats"),
    splitLayout(cellWidths = c("50%", "50%"),
                plotOutput("plot_real", height="100%", width="100%"),
                verticalLayout(plotOutput("plot_fake", height="100%", width="100%"),
                               selectInput('select_fake', "Fake experiment",
                                list("Theory 1 (no difference)"="Same",
                                     "Theory 2 (different distribution)"="Different"),
                                selected="Same"),
                               selectInput('select_iter', "Iteration",
                                           list("Iter1"), selected="Iter1")))
    )
  )
))