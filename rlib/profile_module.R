library(shiny)
library(png)

profileUI <- function(id) {
  tabPanel(
    "Profile",
    
    fluidRow(
      column(12, align = "left",
             tags$b(style = "font-size:2em", "Yang Rui")
      )
    ),
    
    hr(),
    
    fluidRow(
      column(5, align = "left",
             imageOutput(NS(id, "profile"), width = "100%", height = "100%")
      ),
      column(7, align = "left",
             tags$p(style = "text-indent:2em; text-align:justify", "I'm pursuing a Ph.D. degree in neuroscience at Kunming Institute of Zoology .CAS. In my eyes, a heathy body and excellent learning ability are the most important things in the world. Everything else can be expendable for me if needed. In addition to neuroscience, I'm also interested in programming, mathematics, and so on. I hope to use mathematical language and computer to describe the underlying logic of how the brain works. Then we can design and create the real artifical intelligence, based on the human brain, but far beyond it. I believe that this may be the next generation of human beings who can truly survive in the future universe."),
             tags$p(style = "text-indent:2em; text-align:justify", "Only the brave, there is no turning back!"),
             tags$p(style = "text-indent:2em; text-align:justify", "Forward is success, backward is death!")
      )
    )
  )
}

profileServer <- function(id) {
  moduleServer(id, function(input, output, session) {
    profile_size <- reactive({
      dim(readPNG(file.path("image/profile", "J20.png")))
    })
    
    output$profile <- renderImage({
      list(
        src = file.path("image/profile", "J20.png"),
        contentType = "image/png",
        width = profile_size()[2],
        height = profile_size()[1]
      )
    }, deleteFile = F)
  })
}
