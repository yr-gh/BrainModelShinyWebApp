library(shiny)
library(rclipboard)
library(Biostrings)

pairwise_alignmentUI <- function(id) {
  tabPanel(
    "Pairwise Alignment",
    
    # Include clipboard.js
    rclipboardSetup(),
    
    # Wrap text within tag <pre></pre> automatically
    tags$style(paste0("#", NS(id, "display_alignment"), " {white-space: pre-wrap; word-wrap: break-word;}")),
    
    h4(style = "text-align:center", tags$b("Pairwise Alignment Utility")),
    
    hr(),
    
    h6("1. Introduction to Alignment Type"),
    tags$ul(
      tags$li("global: align whole strings with end gap penalties."),
      tags$li("local: align string fragment."),
      tags$li("overlap: align whole strings without end gap penalties."),
      tags$li("global-local: align whole strings in reference with consecutive subsequence of query."),
      tags$li("local-global: align consecutive subsequence of pattern with whole strings in subject.")
    ),
    
    h6("2. Introduction to Alignment Strategy"),
    tags$ul(
      tags$li("One-to-One: align the 1st query to the 1st reference; align the 2nd query to the 2nd reference; and so on."),
      tags$li("Many-to-One: align each query to all references.")
    ),
    
    fluidRow(
      column(12, align = "left",
             textAreaInput(NS(id, "reference_fa"), "Please Paste Your Reference Sequence(s) Here (FASTA format):", "", placeholder = ">chr1\nAGCTAGCT\n>chr2\nATGCATGC", rows = 10, width = "100%")
      )
    ),
    
    fluidRow(
      column(12, align = "left",
             textAreaInput(NS(id, "query_fa"), "Please Paste Your Query Sequence(s) Here (FASTA format):", "", placeholder = ">query1\nAGCT\n>query2\nATGC", rows = 10, width = "100%")
      )
    ),
    
    fluidRow(
      column(4, align = "left",
             selectInput(NS(id, "alignment_type"), "Alignment Type:", c("global", "local", "overlap", "global-local", "local-global"), selected = "global", multiple = F, width = "100%")
      ),
      column(4, align = "left",
             selectInput(NS(id, "alignment_strategy"), "Alignment Strategy:", c("One-to-One", "Many-to-One"), selected = "Many-to-One", multiple = F, width = "100%")
      ),
      column(4, align = "left",
             numericInput(NS(id, "block_width"), "Block Width (integer):", 50, width = "100%")
      )
    ),
    
    fluidRow(
      column(12, align = "left",
             verbatimTextOutput(NS(id, "display_alignment"))
      )
    ),
    
    fluidRow(
      column(12, align = "center",
             uiOutput(NS(id, "clip_output_alignment_btn"))
      )
    )
  )
}

pairwise_alignmentServer <- function(id, session_id) {
  moduleServer(id, function(input, output, session) {
    output_alignment <- reactive({
      req(input$reference_fa, input$query_fa)
      
      pairwise_alignment(ref_fa = input$reference_fa, query_fa = input$query_fa,
                         alignment_type = input$alignment_type,
                         alignment_strategy = input$alignment_strategy,
                         block_width = input$block_width)
    })
    
    output$display_alignment <- reactive({
      output_alignment()
    })
    
    output$clip_output_alignment_btn <- renderUI({
      rclipButton(
        inputId = NS(id, "clip_output_alignment"),
        label = "Copy the Alignment",
        clipText = output_alignment(),
        icon = icon("clipboard")
      )
    })
  })
}
