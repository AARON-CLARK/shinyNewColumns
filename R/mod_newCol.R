#' User interface for newCol module
#'
#' @import shinyFeedback
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
#' @import shinyFeedback
mod_newCol_srv <- function(id, dat, colType) {
  moduleServer(id, function(input, output, session) {

    # Initialize a reactiveValues object for enabling/ disabling addCol button
    # in parent module
    rv_newCol <- reactiveValues(addCol = TRUE)

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

    # validation for column name, no punctuation except . or _ allowed.
    # If violated, disable addCol button
    var_name_punct <- reactive(grepl("([._])|[[:punct:]]", input$var_name))
    observeEvent(input$var_name, {
      if (var_name_punct()) {
        shinyFeedback::showFeedbackDanger(
          inputId = "var_name",
          text = "Cannot contain special characters"
        )
      } else {
        shinyFeedback::hideFeedback("var_name")
      }
    })

    # validation for column label, same punctation rules as above, but also
    # impose a character max. If violated, then disable addCol button

    var_lab_max <- reactive(nchar(input$var_label) > 40)
    var_lab_punct <- reactive(grepl("([._])|[[:punct:]]", input$var_label))
    observeEvent(input$var_label, {
      if (var_lab_max()) {
        shinyFeedback::showFeedbackDanger(
          inputId = "var_label",
          text = "40 Character max"
        )
      } else if (var_lab_punct()) {
        shinyFeedback::showFeedbackDanger(
          inputId = "var_label",
          text = "Cannot contain special characters"
        )
      } else {
        shinyFeedback::hideFeedback("var_label")
      }
    })

    observe({
      # enable / disable addCol button in parent module
      rv_newCol$addCol <- !any(
        var_name_punct(), # T
        var_lab_max(), # T
        var_lab_punct() # F
      )
    # test <- c(T, T, T)
    # !any(test)
    print(paste("rv_newCol$addCol:", rv_newCol$addCol))
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

    return_list <- reactive({
      list(current_mutate = expr_call(),
           allow_add = reactive(rv_newCol$addCol))
    })


    return(return_list)
  })
}
