library(shiny)

aboutUI <- function(id) {
  tabPanel(
    "About",
    
    fluidRow(
      column(12, align = "left",
             p("This is an interactive Shiny web application, powered by R."),
             p("If you have any suggestions, please let me know!"),
             p("Email: 2413667864@qq.com")
      )
    )
  )
}
