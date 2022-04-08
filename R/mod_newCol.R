#' User interface for newCol module
#'
#' @import shinyFeedback
mod_newCol_ui <- function(id) {
  ns <- NS(id)
  fluidPage(
    wellPanel(
      fluidRow(
        column(4,
               shinyFeedback::useShinyFeedback(),
               textInput(ns("var_name"),"Variable Name", placeholder = var_name_ph_util), #placeholder
               textInput(ns("var_label"), "Variable Label", placeholder = lab_name_ph_util),
               ),
        column(4,
               fluidRow(
                 column(6,  uiOutput(ns("ref_var_ui"))),
                 # column(6,  tags$div(style="padding-top:20px", checkboxInput(ns("live"), "Use Live Data?", value = TRUE))),
               ),
               fluidRow(
                 column(6, sliderInput(ns("numGroups"), "Number of groups", 1, 10, 1)),
                 column(6, br(), checkboxInput(ns("incl_else"), "Include an 'Else' group", value = FALSE))
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
#' @import shinyFeedback
mod_newCol_srv <- function(id, dat, colType) {
  moduleServer(id, function(input, output, session) {

    ns <- session$ns

    # The reference variable available for selection depends on what type of
    # variable is chosen.
    output$ref_var_ui <- renderUI({
      selectInput(
        ns("reference_var"), "Reference Variable",
        choices =
          if(colType() == "Range Variable") {
            names(dat()[sapply(dat(), is.numeric)])
          } else {
            names(dat())
          }
      )
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
        if (grepl("[[:punct:]]", input$var_name)) { # SAS var names can contain underscores...
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

    # generate numerous UI's for new var's new groups (as needed)
    observeEvent(c(input$numGroups, colType()), {
      output$cond_uis <-
        if (colType() != "Custom") {
          renderUI(mod_rangeConditions_ui(ns("cond1")))
        } else {
          renderUI( purrr::map(conds(), ~ mod_advConditions_ui(ns(.x))))
        }
    })

    rv_cnts <- reactiveValues()

    # initialize reactive values to monitor how many
    moduleExpr <- reactive({
      req(input$numGroups)
      if(colType() == "Range Variable") {
          moduleExpr <- mod_rangeConditions_srv(id = "cond1",
                                   dat = dat,
                                   grp = reactive(input$numGroups),
                                   response = reactive(input$reference_var),
                                   else_group = reactive(input$incl_else),
                                   else_name = reactive(default_val(input$elseName, else_ph_util)))
      } else {
        purrr::map(conds(), ~ mod_advConditions_srv(id = .x, dat = dat, cnt = rv_cnts))
      }
    })

    # construct a call based on inputs
    expr_call <-reactive({
      req(moduleExpr())
      colname <- default_val(input$var_name, var_name_ph_util)
      rlang::call2(
        quote(dplyr::mutate),
        !!colname := rlang::call2(
          quote(dplyr::case_when),
          !!!moduleExpr()
        )
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
