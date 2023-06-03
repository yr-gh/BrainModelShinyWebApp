library(shiny)
library(rclipboard)

sequence_conversionUI <- function(id) {
  tabPanel(
    "Sequence Conversion",
    
    # Include clipboard.js
    rclipboardSetup(),
    
    # Wrap text within tag <pre></pre> automatically
    tags$style(paste0("#", NS(id, "display_sequence"), " {white-space: pre-wrap; word-wrap: break-word;}")),
    
    h4(style = "text-align:center", tags$b("Sequence Conversion Utility")),
    
    hr(),
    
    fluidRow(
      column(12, align = "left",
             textAreaInput(NS(id, "input_sequence"), "Please Paste Your Sequence Here:", "", placeholder = "ACGT", rows = 6, width = "100%")
      )
    ),
    
    fluidRow(
      column(6, align = "left",
             selectInput(NS(id, "input_sequence_type"), "Input Sequence Type:", c("DNA", "RNA", "DNA2RNA", "RNA2DNA"), selected = "DNA", multiple = F, width = "100%")
      ),
      column(6, align = "left",
             selectInput(NS(id, "sequence_operation"), "Sequence Operation Type:", c("Reverse", "Complemented", "Reverse-Complemented"), selected = "Reverse", multiple = F, width = "100%")
      )
    ),
    
    fluidRow(
      column(12, align = "left",
             verbatimTextOutput(NS(id, "display_sequence"))
      )
    ),
    
    fluidRow(
      column(12, align = "center",
             uiOutput(NS(id, "clip_output_sequence_btn"))
      )
    )
  )
}

sequence_conversionServer <- function(id, session_id) {
  moduleServer(id, function(input, output, session) {
    output_sequence <- reactive({
      req(input$input_sequence)
      
      if (input$sequence_operation == "Reverse") {
        seq_rev(input$input_sequence)
      } else if (input$sequence_operation == "Complemented") {
        seq_compm(input$input_sequence, input$input_sequence_type)
      } else if (input$sequence_operation == "Reverse-Complemented") {
        seq_rev_compm(input$input_sequence, input$input_sequence_type)
      }
    })
    
    output$display_sequence <- reactive({
      output_sequence()
    })
    
    output$clip_output_sequence_btn <- renderUI({
      rclipButton(
        inputId = NS(id, "clip_output_sequence"),
        label = "Copy the Sequence",
        clipText = output_sequence(),
        icon = icon("clipboard")
      )
    })
  })
}
