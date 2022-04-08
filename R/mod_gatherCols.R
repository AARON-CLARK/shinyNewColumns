
mod_launchModal_ui <- function(id) {
  ns <- NS(id)
  fluidPage(
    tags$head(tags$style("#new_col_modal .modal-dialog {width:1000px;}")),
    actionButton(ns("createColBttn"),"Create Variable")
  )
}

mod_launchModal_srv <- function(input, output, session, dat) {
  ns <- session$ns

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

      # Content of the Modal
      mod_newCol_ui(ns("new"))

    )))
  })

  all_mutates <- reactiveValues()


  # Why do we need both of these to work?
  # How do we condense this code?

  ##########################################
  observeEvent(input$createColBttn, {
     callModule(mod_newCol_srv, "new",
                dat = reactive(dat$data),
                colType = reactive(input$createColType))
  })

  # save the current mutate
  current_mutate <- eventReactive(input$createColBttn, {
      callModule(mod_newCol_srv, "new",
                 dat = reactive(dat$data),
                 colType = reactive(input$createColType))
    })
  ##########################################


  expressions_and_data <- reactive({
    if (length(all_mutates$mutate) == 0) return("")
    list(
      rlang::expr(dat$data),
      all_mutates$mutate
    )
  })

  new_dataset <- reactive({
    if (length(all_mutates$mutate) == 0) return(dat$data)
    rlang::flatten(expressions_and_data()) %>%
        purrr::reduce(~rlang::expr(!!.x %>% !!.y)) %>%
        eval()
  })

  observeEvent(input$addCol, {
    all_mutates$mutate <- c(all_mutates$mutate, current_mutate())
    dat$data <- new_dataset()
    dat$expr <- expressions_and_data()
    removeModal()
  })


}
