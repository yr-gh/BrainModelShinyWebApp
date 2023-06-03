library(shiny)
library(uuid)

# options(download.file.method = "auto")
# options(repos = c("CRAN" = "https://cran.rstudio.com", "BioCsoft" = "https://bioconductor.org/packages/3.16/bioc", "BioCann" = "https://bioconductor.org/packages/3.16/data/annotation"))
# getOption("repos")
# chooseBioCmirror(ind = 5)
# chooseCRANmirror(ind = 15)

# Allow users to upload files up to 100MB
options(shiny.maxRequestSize = 100 * 1024^2)

source("rlib/home_module.R")
source("rlib/profile_module.R")
source("rlib/about_module.R")
source("rlib/ungrouped_violin_module.R")
source("rlib/ungrouped_bar_module.R")
source("rlib/plot_func.R")
source("rlib/stat_func.R")
source("rlib/other_func.R")
source("rlib/sequence_conversion_module.R")
source("rlib/pairwise_alignment_module.R")
source("rlib/ngs_func.R")

ui <- navbarPage(
  theme = bslib::bs_theme(bootswatch = "flatly"),
  
  tags$b(style = "font-size:1.5em", "Brain Model"),
  
  homeUI("home"),
  
  tabPanel(
    "Stats",
    
    tags$b("To be developed if needed.")
  ),
  
  navbarMenu(
    "Tools",
    
    sequence_conversionUI("sequence_conversion"),
    
    pairwise_alignmentUI("pairwise_alignment")
  ),
  
  navbarMenu(
    "Graphs",
    
    ungrouped_violinUI("ungrouped_violin"),
    
    ungrouped_barUI("ungrouped_bar")
  ),
  
  profileUI("profile"),
  
  aboutUI("about")
)

server <- function(input, output, session) {
  session_id <- UUIDgenerate()
  
  homeServer("home")
  
  sequence_conversionServer("sequence_conversion", session_id = session_id)
  
  pairwise_alignmentServer("pairwise_alignment", session_id = session_id)
  
  ungrouped_violinServer("ungrouped_violin", session_id = session_id)
  
  ungrouped_barServer("ungrouped_bar", session_id = session_id)
  
  profileServer("profile")
  
  onSessionEnded(function() {
    file.remove(list.files(".", pattern = paste0(".+", session_id, ".+"), full.names = T))
  })
}

shinyApp(ui = ui, server = server)
