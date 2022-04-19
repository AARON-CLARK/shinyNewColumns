#' launchModal UI Function
#'
#' provides the interface to a single button
#'
#' @return a shiny \code{\link[shiny]{tagList}} containing a simple action
#'   button to launch the `shinyNewColumns` modal
#'
#' @param id standard parameter for {shiny modules}.
#'
#' @import shiny
#' @export
#'
mod_launchModal_ui <- function(id) {
  ns <- NS(id)
  fluidPage(
    # shinyjs::useShinyjs(),
    tags$head(tags$style("#new_col_modal .modal-dialog {width:1000px;}")),
    # UI is just comprised of a button!
    actionButton(ns("createColBttn"),"Add New Column")
  )
}

#' launchModal Server Function
#'
#' @description launchModal Server Function waits for a button click, and then
#'   launches a large modal containing a savvy user interface to help users
#'   create new columns on the fly
#'
#' @param id standard parameter for {shiny modules} used to create namespaces
#'   and connect the UI module to the server module.
#' @param dat a r data.frame you want users to  derive a new column from and to.
#'
#' @return a list of reactive objects. The first object is the original
#'   data.frame in a reactive state (named `data()`), which may contain a newly
#'   derived column if user successfully created derivation. Secondly, the list
#'   will also contain `expr()` containing the `dplyr::mutate()` expressions used
#'   to create said column(s).
#'
#' @import shiny
#' @importFrom rlang expr flatten
#' @importFrom purrr reduce
#'
#' @export
#'
mod_launchModal_srv <- function(id, dat) {
  moduleServer(id, function(input, output, session) {

    ns <- session$ns # get namespace context


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
        conditionalPanel("input.createColType != 'Range Variable'", ns = ns,
                         h3(div("This column type is under construction!",
                                icon("hand-o-down", verify_fa = FALSE),
                                style = "color:darkred;"))),
        # img(src="shinyNewColumnsv7_flat_wYellowStars.png",
        #     style=" padding-left:3px; height:100px;")
        mod_newCol_ui(ns("new"))

      )))
    })


    # initiate reactive values (rv) object to keep track of dplyr
    # mutate() expressions throughout the module
    rv <- reactiveValues(data = dat, all_mutates = NULL)


    # run 'newCol' module upon 'Create Col' button click, passing data
    # and col type. Later, Module returns dplyr mutate expression(s)
    observe({
      input$createColBttn
      rv$current_mutate <- mod_newCol_srv(id = "new",
                     dat = reactive(rv$data),
                     colType = reactive(input$createColType))
     })


    # Upon clicking 'Add Variable' button in modal, combine and evaluate
    # dplyr mutate statements in order to modify data
    observeEvent(input$addCol, {
      rv$all_mutates <- c(rv$all_mutates, rv$current_mutate)

      # expressions to evaluate on data source
      data_and_expr <- list(
        rlang::expr(rv$data),  # current data
        rv$all_mutates         # current + any other mutates
      )

      # Create the new data frame with mutate(s) applied
      rv$data <- rlang::flatten(data_and_expr) %>%
        purrr::reduce(~rlang::expr(!!.x %>% !!.y)) %>%
        eval()

      removeModal()
    })


  # return the original data, updated with the new
  # column. Return the dplyr::mutate() expression
  # for fun/ display in app.R.
  return( list(
      data = reactive(rv$data),
      expr = reactive(rv$all_mutates)
  ))

  })
}
