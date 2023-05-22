library(shiny)
library(slickR)

homeUI <- function(id) {
  tabPanel(
    "Home",
    
    fluidRow(
      column(12, align = "center",
             tags$b(style = "font-size:1.5em", "Relax your mind, have a cup of tea, and enjoy the scenery in front of you!")
      )
    ),
    
    hr(),
    
    fluidRow(
      column(12, align = "center",
             tags$b(style = "font-size:1.25em", "Goddess - Bai YueKui, a character in Ling Cage, made by YHKT Entertainment.")
      )
    ),
    
    fluidRow(
      column(12, align = "center",
             slickROutput(NS(id, "baiyuekui"), width = "100%", height = "100%")
      )
    )
  )
}

homeServer <- function(id) {
  moduleServer(id, function(input, output, session) {
    output$baiyuekui <- renderSlickR({
      imgs <- list.files("image/about", full.names = T)
      slickR(imgs) + settings(arrows = F, autoplay = T, autoplaySpeed = 3000)
    })
  })
}
