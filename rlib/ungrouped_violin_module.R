library(shiny)
library(dplyr)
library(magrittr)
library(stringr)
library(png)
library(pdftools)

ungrouped_violinUI <- function(id) {
  tabPanel(
    "Ungrouped Violin",
    
    h4("0. Introduction"),
    tags$p("Ungrouped violin is designed to draw violin with/without box inside it. It only supports Student's T-Test, comparing the control group with all other experimental groups; or ANOVA, to be developed if needed."),
    
    h4("1. Usage"),
    tags$ol(
      tags$li("Upload data file, whose columns must have headers and be separated by '\\t', or ',' (i.e., '.tsv', or '.csv' file)."),
      tags$li("Set plotting parameters."),
      tags$li("Adjust the width and height of the plot."),
      tags$li("If the plot is as expected, a PDF version of the plot is downloadable.")
    ),
    tags$b("Note: for axis titles and labels, the styles of Markdown or HTML are now supported. The followings are Markdown or HTML tags commonly used when drawing plot."),
    tags$ul(
      tags$li("Superscript: ", HTML(r"(<code>Ca&lt;sup&gt;2+&lt;/sup&gt;</code>&nbsp;&rarr;&nbsp;Ca<sup>2+</sup>.)")),
      tags$li("Subscript: ", HTML(r"(<code>H&lt;sub&gt;2&lt;/sub&gt;0</code>&nbsp;&rarr;&nbsp;H<sub>2</sub>0.)")),
      tags$li("Bold: ", HTML(r"(<code>A&lt;b&gt;BC&lt;/b&gt;D</code>&nbsp;&rarr;&nbsp;A<b>BC</b>D.)")),
      tags$li("Italic: ", HTML(r"(<code>A&lt;i&gt;BC&lt;/i&gt;D</code>&nbsp;&rarr;&nbsp;A<i>BC</i>D.)")),
      tags$li("Color: ", HTML(r"(<code>&lt;span style="color:red"&gt;Red&lt;/span&gt;</code>&nbsp;&rarr;&nbsp;<span style="color:red">Red</span>.)")),
      tags$li("Newline: ", HTML(r"(<code>&lt;br&gt;</code>.)")),
      tags$li("Tips: tags can be nested within each other, and most tags also contain attributes like ", HTML(r"(<code>style="color:red; font-size:12px; font-family: 'Microsoft Yahei'"</code>)"), ". For more tags of Markdown or HTML, and their attributes, you can search their usages in your favorite search engine. e.g., ", HTML(r"(<code>&lt;i style="color:red; font-size:12px; font-family: 'Times New Roman'"&gt;Times &lt;b&gt;New&lt;/b&gt; Roman&lt;/i&gt;</code>&nbsp;&rarr;&nbsp;<i style="color:red; font-size:12px; font-family: 'Times New Roman'">Times <b>New</b> Roman</i>.)"))
    ),
    
    h4("2. Upload Data File"),
    fileInput(NS(id, "file"), NULL, buttonLabel = "Upload", multiple = F),
    tags$b("Note: you can preview the first 4 lines of your data to see if it's read as expected."),
    fluidRow(
      column(12, align = "center",
        tableOutput(NS(id, "file_preview"))
      )
    ),
    tags$b("Note: please select two columns with which you want to plot."),
    fluidRow(
      column(6, align = "left",
             uiOutput(NS(id, "select_input_for_group_variable"), width = "100%")
      ),
      column(6, align = "left",
             uiOutput(NS(id, "select_input_for_value_variable"), width = "100%")
      )
    ),
    
    h4("3. Set Parameters"),
    fluidRow(
      column(4, align = "left",
             textInput(NS(id, "group_levels"), "Group Levels:", "", placeholder = "Separated by ,", width = "100%")
      ),
      column(4, align = "left",
             numericInput(NS(id, "max_data_value_scaling_factor"), "Scaling Factor on Maximum Data Value:", 0, width = "100%") 
      ),
      column(4, align = "left",
             numericInput(NS(id, "sig_level_scaling_factor"), "Significance Level Scaling Factor:", 100, width = "100%")
      )
    ),
    
    fluidRow(
      column(4, align = "left",
             textInput(NS(id, "x_axis_title"), "X-axis Title:", "Group", width = "100%")
      ),
      column(4, align = "left",
             textInput(NS(id, "y_axis_title"), "Y-axis Title:", "Value", width = "100%")
      ),
      column(4, align = "left",
             numericInput(NS(id, "x_axis_label_rotation"), "X-axis Label Rotation:", 45, width = "100%") 
      )
    ),
    
    fluidRow(
      column(4, align = "left",
             numericInput(NS(id, "sig_level_label_size"), "Significance Level Label Size:", 10, width = "100%")
      ),
      column(4, align = "left",
             numericInput(NS(id, "base_font_size"), "Global Base Font Size:", 18, width = "100%")
      ),
      column(4, align = "left",
             textInput(NS(id, "y_breaks"), "Y-axis Breaks:", "", placeholder = "Separated by ,", width = "100%")
      )
    ),
    
    fluidRow(
      column(4, align = "left",
             numericInput(NS(id, "violin_line_width"), "Violin Line Width:", 1, width = "100%")
      ),
      column(4, align = "left",
             selectInput(NS(id, "violin_scale"), "Violin Scale:", c("area", "count", "width"), selected = "area", multiple = F, width = "100%")
      ),
      column(4, align = "left",
             textInput(NS(id, "fill_color"), "Fill Colors:", "", placeholder = "Separated by ,", width = "100%")
      )
    ),
    
    fluidRow(
      column(4, align = "left",
             numericInput(NS(id, "boxplot_width"), "Box Width:", 0.2, width = "100%")
      ),
      column(4, align = "left",
             numericInput(NS(id, "boxplot_line_width"), "Box Line Width:", 1, width = "100%")
      ),
      column(4, align = "left",
             textInput(NS(id, "line_color"), "Line Colors:", "", placeholder = "Separated by ,", width = "100%")
      )
    ),
    
    fluidRow(
      column(12, align = "left",
             textInput(NS(id, "x_labels"), "Rename X-axis Labels (the same order as group levels):", "", placeholder = "Separated by &&&", width = "100%")
      )
    ),
    
    h4("4. Figure Preview"),
    fluidRow(
      column(6, align = "left",
             sliderInput(NS(id, "height"), "Figure Height:", min = 100, max = 4000, value = 400, width = "100%")
      ),
      column(6, align = "left",
             sliderInput(NS(id, "width"), "Figure Width:", min = 100, max = 4000, value = 400, width = "100%")
      )
    ),
    
    fluidRow(
      column(12, align = "center",
             plotOutput(NS(id, "ungrouped_violin_plot"), width = "100%", height = "100%")
      )
    ),
    
    h4("5. PDF Preview"),
    fluidRow(
      column(12, align = "left",
             tags$b("Note: if the figure generated above is as expected, you can select the following check box to preview and adjust the width and height of the PDF file.")
      )
    ),
    fluidRow(
      column(12, align = "left",
             checkboxInput(NS(id, "sure_to_preview_pdf"), "Preview the PDF File?", F)
      )
    ),
    fluidRow(
      column(6, align = "left",
             numericInput(NS(id, "pdf_height"), "PDF Height (ranging from 2cm to 200cm):", 10, min = 2, max = 200, width = "100%")
      ),
      column(6, align = "left",
             numericInput(NS(id, "pdf_width"), "PDF Width (ranging from 2cm to 200cm):", 10, min = 2, max = 200, width = "100%")
      )
    ),
    
    fluidRow(
      column(12, align = "center",
        imageOutput(NS(id, "pdf_preview"), width = "100%", height = "100%")
      )
    ),
    
    h4("6. Download PDF File"),
    fluidRow(
      column(12, align = "center",
             downloadButton(NS(id, "result_download"), "Download")
      )
    )
  )
}

ungrouped_violinServer <- function(id, session_id) {
  moduleServer(id, function(input, output, session) {
    raw_data <- reactive({
      req(input$file)
      
      vroom::vroom(input$file$datapath)
    })
    
    output$file_preview <- renderTable(head(raw_data(), n = 4))
    
    output$select_input_for_group_variable <- renderUI({
      selectInput(NS(id, "group_variable"), "Select Group Variable:", names(raw_data()), multiple = F, width = "100%")
    })
    
    output$select_input_for_value_variable <- renderUI({
      selectInput(NS(id, "value_variable"), "Select Value Variable:", names(raw_data()), multiple = F, width = "100%")
    })
    
    output_file_id <- reactive({
      paste0(input$value_variable, "_vs_", input$group_variable, "_", session_id)
    })
    
    group_levels <- reactive({
      if (input$group_levels == "") unique(raw_data()[[input$group_variable]]) else str_split(input$group_levels, ",")[[1]]
    })
    
    x_labels <- reactive({
      if (input$x_labels == "") setNames(group_levels(), group_levels()) else setNames(str_split(input$x_labels, "&&&")[[1]], group_levels())
    })
    
    clean_data <- reactive({
      raw_data() %>% 
        select(all_of(c(input$group_variable, input$value_variable))) %>% 
        set_colnames(c("group", "value")) %>% 
        filter(group %in% group_levels()) %>% 
        mutate(group = factor(group, levels = group_levels()))
    })
    
    y_breaks <- reactive({
      if (input$y_breaks == "") YBreaks(clean_data(), input$max_data_value_scaling_factor) else as.numeric(str_split(input$y_breaks, ",")[[1]])
    })
    
    y_range <- reactive({
      YRange(y_breaks())
    })
    
    sig_level <- reactive({
      TTestOverControl(clean_data(), group_levels(), input$sig_level_scaling_factor)
    })
    
    fig_args <- reactive({
      list("violin_line_width" = input$violin_line_width, "violin_scale" = input$violin_scale,
           "boxplot_width" = input$boxplot_width, "boxplot_line_width" = input$boxplot_line_width,
           "sig_level_label_size" = input$sig_level_label_size, "y_breaks" = y_breaks(),
           "y_range" = y_range(), "base_font_size" = input$base_font_size,
           "fill_color" = input$fill_color, "line_color" = input$line_color,
           "x_axis_title" = input$x_axis_title, "y_axis_title" = input$y_axis_title,
           "x_axis_label_rotation" = input$x_axis_label_rotation, "x_labels" = x_labels())
    })
    
    p <- reactive({
      UngroupedGGViolin(clean_data(), sig_level(), fig_args())
    })
    
    output$ungrouped_violin_plot <- renderPlot(
      width = function() input$width,
      height = function() input$height,
      res = 96,
      {
        p()
      }
    )
    
    pdf_file <- reactive({
      req(input$sure_to_preview_pdf)
      
      ggsave(file.path(".", paste0("ungrouped_violin_plot_", output_file_id(), ".pdf")),
             p(), device = "pdf", limitsize = F,
             width = unit(input$pdf_width, "cm"),
             height = unit(input$pdf_height, "cm"))
      file.path(".", paste0("ungrouped_violin_plot_", output_file_id(), ".pdf"))
    })
    
    pdf_preview_file <- reactive({
      pdf_convert(pdf_file(),
                  filenames = file.path(".", paste0("ungrouped_violin_plot_", output_file_id(), ".png")),
                  format = "png", pages = 1, dpi = 72)
      file.path(".", paste0("ungrouped_violin_plot_", output_file_id(), ".png"))
    })
    
    pdf_preview_size <- reactive({
      dim(readPNG(pdf_preview_file()))
    })
    
    output$pdf_preview <- renderImage({
      list(
        src = pdf_preview_file(),
        contentType = "image/png",
        width = pdf_preview_size()[2],
        height = pdf_preview_size()[1]
      )
    }, deleteFile = F)
    
    basic_stat_file <- reactive({
      vroom::vroom_write(BasicStatByGroup(clean_data()), file = file.path(".", paste0("ungrouped_violin_plot_basic_stat_", output_file_id(), ".tsv")), delim = "\t")
      file.path(".", paste0("ungrouped_violin_plot_basic_stat_", output_file_id(), ".tsv"))
    })
    
    t_test_res_file <- reactive({
      vroom::vroom_write(sig_level(), file = file.path(".", paste0("ungrouped_violin_plot_t_test_", output_file_id(), ".tsv")), delim = "\t")
      file.path(".", paste0("ungrouped_violin_plot_t_test_", output_file_id(), ".tsv"))
    })
    
    clean_data_file <- reactive({
      vroom::vroom_write(clean_data(), file = file.path(".", paste0("ungrouped_violin_plot_clean_data_", output_file_id(), ".tsv")), delim = "\t")
      file.path(".", paste0("ungrouped_violin_plot_clean_data_", output_file_id(), ".tsv"))
    })
    
    output$result_download <- downloadHandler(
      filename = function() {
        paste0("ungroupped_violin_plot_", output_file_id(), ".zip")
      },
      content = function(file) {
        zip(file, c(pdf_file(), basic_stat_file(), t_test_res_file(), clean_data_file()))
      },
      contentType = "application/zip"
    )
  })
}
