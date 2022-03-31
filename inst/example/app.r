
library(shiny)

# UI
ui <- fluidPage(
  includeCSS("style.css"),
  fluidPage(
    h1("shinyNewColumns", align = "center"),
    br(), br(),
    mod_gatherCols_ui(id = "gatherCols"), # just a button
    br(), br(),
    verbatimTextOutput("debug_expr"),
    wellPanel(dataTableOutput("debug_data"))
  )
)

# Server
server <- function(input, output) {
  data_joined <- reactiveValues( expr = "", data = iris )
  mod_gatherCols_srv(id = "gatherCols", dat = data_joined)
  output$debug_expr <- renderPrint(data_joined$expr)
  output$debug_data <- renderDataTable(data_joined$data)
}

shinyApp(ui = ui, server = server)
