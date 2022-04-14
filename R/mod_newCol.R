#' User interface for newCol module
#'
#' Provides the interface for user to imput important characteristics about new
#' column the wish to create.
#'
#' @return a shiny \code{\link[shiny]{tagList}} containing a well panel of input
#'   widgets
#'
#' @param id standard parameter for {shiny modules}.
#'
#' @import shiny
#' @importFrom shinyFeedback useShinyFeedback
#'
#'
mod_newCol_ui <- function(id) {
  ns <- NS(id)
  fluidPage(
    shinyFeedback::useShinyFeedback(),
    wellPanel(
      fluidRow(
        column(4,
               textInput(ns("var_name"),"New Variable Name", placeholder = var_name_ph_util), #placeholder
               textInput(ns("var_label"), "New Variable Label", placeholder = lab_name_ph_util),
               ),
        column(4,
               fluidRow(
                 column(8,  uiOutput(ns("ref_var_ui"))),
               ),
               fluidRow(
                 column(8, sliderInput(ns("numGroups"), "Number of conditions/ groups", 1, 10, 1)),
                 column(4, br(), checkboxInput(ns("incl_else"), "Include an 'Else' group", value = FALSE))
                 ),
               ),
        column(4, plotOutput(ns("var_hist"), height=200))
      )),
    uiOutput(ns("cond_uis")),
    conditionalPanel("input.incl_else", ns = ns,
      wellPanel(
        textInput(ns("elseName"), "Else:", placeholder = else_ph_util)
      )
    )
  )
}

#' Server logic for newCol module
#'
#'
#' @param id standard parameter for {shiny modules} used to create namespaces
#'   and connect the UI module to the server module.
#' @param dat a r data.frame you want users to derive a new column from and to.
#' @param colType a text string, specifying a supported column type such as
#'   "Range Variable"
#'
#' @return an exprression containing the `dplyr::mutate()` expressions used to
#'   create new column
#'
#' @import shiny
#' @importFrom shinyFeedback showFeedbackDanger hideFeedback
#' @importFrom ggplot2 ggplot aes_string geom_histogram xlab
#' @importFrom purrr map
#' @importFrom rlang call2
#' @importFrom dplyr mutate case_when
#'
mod_newCol_srv <- function(id, dat, colType) {
  moduleServer(id, function(input, output, session) {

    ns <- session$ns

    # The reference variable available for selection depends on what type of
    # 'New Column Type' is chosen. For Ex, a "Range Variable" can only be
    # built on the data's numeric variables
    observeEvent(colType(), {
      output$ref_var_ui <- renderUI({
        selectInput(
          ns("reference_var"),
          label = switch(colType(),
                         Custom = "Plot Variable Distribution",
                         "Reference Variable"),
          choices = switch(colType(),
                      `Range Variable` = names(dat()[sapply(dat(), is.numeric)]),
                      names(dat()) ),
          selected = isolate(input$reference_var)
          )
      })
    })


    # create histogram of reference variable
    output$var_hist <- renderPlot({
      req(input$reference_var, dat())

      ggplot2::ggplot(dat(), ggplot2::aes_string(x = input$reference_var)) +
        ggplot2::geom_histogram(bins = 30) +
        ggplot2::xlab(glue::glue("n = {nrow(dat())}"))

    }, height=200)

    # validation for column name
    observeEvent(input$var_name, {
      if (grepl("[[:punct:]]", input$var_name)) {
        shinyFeedback::showFeedbackDanger(
          inputId = "var_name",
          text = "Cannot contain special characters"
        )
      } else {
        shinyFeedback::hideFeedback("var_name")

      }
    })

    # validation for column label
    observeEvent(input$var_label, {
      if (nchar(input$var_label) > 40) {
        shinyFeedback::showFeedbackDanger(
          inputId = "var_label",
          text = "40 Character max"
        )
      } else if (grepl("[[:punct:]]", input$var_label)) {
        shinyFeedback::showFeedbackDanger(
          inputId = "var_label",
          text = "Cannot contain special characters"
        )
      } else {
        shinyFeedback::hideFeedback("var_label")
      }
    })

    # labels for if-then conditional groups
    conds <- reactive(paste0("cond",seq_len(input$numGroups)))

    # just for screenshot
    # # generate numerous UI's for new var's new groups (as needed)
    # observeEvent(c(input$numGroups, colType()), {
    #   output$cond_uis <- renderUI(mod_rangeConditions_ui(ns("cond1")))
    # })

    # generate numerous UI's for new var's new groups (as needed)
    observeEvent(c(input$numGroups, colType()), {
      output$cond_uis <-
        if (colType() != "Custom") {
          renderUI(mod_rangeConditions_ui(ns("cond1")))
        } else {
          renderUI( purrr::map(conds(), ~ mod_advConditions_ui(ns(.x))))
        }
    })

    # initialize reactive values to monitor how many
    rv_cnts <- reactiveValues()

    # # just for screenshots
    # # When selected, call rangeConditions module, providing a number of inputs,
    # #  wrapping them in a reactive context. Save the output as a reactive
    # moduleExpr <- reactive({
    #   req(input$numGroups)
    #     mod_rangeConditions_srv(
    #       id = "cond1",
    #       dat = dat,
    #       grp = reactive(input$numGroups),
    #       reference_var = reactive(input$reference_var),
    #       else_group = reactive(input$incl_else),
    #       else_name = reactive(default_val(input$elseName, else_ph_util)))
    # })


    # When selected, call rangeConditions module, providing a number of inputs
    moduleExpr <- reactive({
      req(input$numGroups)
      if(colType() == "Range Variable") {
          mod_rangeConditions_srv(
             id = "cond1",
             dat = dat,
             grp = reactive(input$numGroups),
             reference_var = reactive(input$reference_var),
             else_group = reactive(input$incl_else),
             else_name = reactive(default_val(input$elseName, else_ph_util)))
      } else {
        purrr::map(conds(), ~ mod_advConditions_srv(id = .x, dat = dat, cnt = rv_cnts))
      }
    })

    # construct a call based on inputs (again) & return to parent module
    expr_call <-reactive({
      req(moduleExpr())
      colname <- default_val(input$var_name, var_name_ph_util)
      rlang::call2( quote(dplyr::mutate),
        !!colname := rlang::call2(quote(dplyr::case_when),!!!moduleExpr())
      )
    })

    # Not sure why, but when this is commented out, the cond UI will not display
    observe({
      req(expr_call())
      force(expr_call())
    })

    return(current_mutate = expr_call())
  })
}
