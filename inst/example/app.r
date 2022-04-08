
library(shiny)
options(shiny.fullstacktrace = TRUE)
# UI
ui <- fluidPage(
  includeCSS("style.css"),
  fluidPage(
    h1("shinyNewColumns", align = "center"),
    br(), br(),
    mod_launchModal_ui(id = "launchModal"), # just a button
    br(), br(),
    verbatimTextOutput("display_expr"),
    wellPanel(dataTableOutput("display_data"))
  )
)

# Server
server <- function(input, output) {
  out <- mod_launchModal_srv(id = "launchModal", dat = iris)
  output$display_expr <- renderPrint(out$expr())
  output$display_data <- renderDataTable(out$data())
}

shinyApp(ui = ui, server = server)
