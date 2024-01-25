# Load libraries
# Install and load the pacman package
if (!require(pacman))
  install.packages("pacman")
pacman::p_load(
  shiny,
  shinythemes,
  shinydashboard,
  DT,
  magrittr,
  dplyr,
  readxl,
  janitor,
  stringr,
  parsedate,
  readr
)

# Define UI
ui <- fluidPage(
  theme = shinytheme("lumen"),
  sidebarLayout(
    sidebarPanel(
      fileInput(
        "file1",
        strong("Upload"),
        multiple = FALSE,
        accept = c(".xlsx", ".xls")
      ),
      radioButtons(
        "script",
        strong("Operator"),
        c("yardi", "aoperator", "boperator", "coperator")
      ),
      width = 2
    ),
    mainPanel(fluidRow(
      width = 12, height = 1500,
      tabsetPanel(tabPanel(strong("Preview"),
                           column(
                             12,
                             box(
                               height = 500,
                               width = 800,
                               solidHeader = FALSE,
                               status = "success",
                               DT::dataTableOutput("table1", height = 300)
                             )
                           )))
    ))
  )
)
  
  # Define server
  server <- shinyServer(function(input, output) {
    dat <- reactive({
      req(input$file1)
      path = input$file1$datapath
      path_source = paste0(input$script, ".R")
      source(path_source)
      res(path)
    })
    
    renderDT_custom <- function(data) {
      DT::renderDT({
        if (is.null(input$file1))
          return(NULL)
        data()
      },
      extensions = c("Buttons"),
      options = list(
        dom = 'Bfrtip',
        buttons = c('copy', 'excel', 'csv', 'print'),
        scrollX = TRUE,
        pageLength = 8
      ),
      server = FALSE)
    }
    
    output$table1 <- renderDT_custom(dat)
  })
  
  # Run the application
  shinyApp(ui = ui, server = server
  )