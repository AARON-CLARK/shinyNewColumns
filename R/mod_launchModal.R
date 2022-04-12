
mod_launchModal_ui <- function(id) {
  ns <- NS(id)
  fluidPage(
    shinyjs::useShinyjs(),
    tags$head(tags$style("#new_col_modal .modal-dialog {width:1000px;}")),
    # UI is just comprised of a button!
    actionButton(ns("createColBttn"),"Create Variable")
  )
}


mod_launchModal_srv <- function(id, dat) {
  moduleServer(id, function(input, output, session) {

    ns <- session$ns # get namespace context

    # initiate reactive values (rv) object to keep track of dplyr mutate()
    # expressions
    rv <- reactiveValues(data = dat, expr = NULL, all_mutates = NULL)

    # initialize modal
    observeEvent(input$createColBttn, {

      showModal(tags$div(id="new_col_modal", modalDialog(
        title = div(style="width:100%;padding-top:10px;",
          column(6, div(style="font-weight:bold;padding-top:5px;text-align:right", "New Column Type:")),
          column(6, selectInput(ns("createColType"), NULL,
                      choices = c("Range Variable","Yes/No Flag", "Custom"),
                      selected="Range Variable"))
        ),
        footer = tagList(
          modalButton("Cancel"),
          actionButton(ns("addCol"),"Add Variable")
        ),

        # Fill Content of the Modal with the 'newCol' UI
        mod_newCol_ui(ns("new"))

      )))
    })



    # run 'newCol' module upon 'Create Col' button click, passing data and col
    # type. Module returns dplyr mutate expression(s)
    observe({
      input$createColBttn
      newCol <- mod_newCol_srv(id = "new",
                     dat = reactive(rv$data),
                     colType = reactive(input$createColType)
      )
      rv$current_mutate <- newCol()$current_mutate
      print(paste("newCol()$allow_add():", newCol()$allow_add()))
      if(newCol()$allow_add()) shinyjs::enable(ns("addCol")) else shinyjs::disable(ns("addCol"))
    })


    # Upon clicking 'Add Variable' button in modal, combine and evaluate
    # dplyr mutate statements in order to modify data
    observeEvent(input$addCol, {
      rv$all_mutates <- c(rv$all_mutates, rv$current_mutate)

      # expressions to evaluate on data source
      rv$expr<- list(
        rlang::expr(rv$data),  # current data
        rv$all_mutates         # current + other mutates
      )

      # Create the new data frame with mutate(s) applied
      rv$data <- rlang::flatten(rv$expr) %>%
        purrr::reduce(~rlang::expr(!!.x %>% !!.y)) %>%
        eval()

      removeModal()
    })


  return(list(data = reactive(rv$data), expr = reactive(rv$expr)))
  })
}
