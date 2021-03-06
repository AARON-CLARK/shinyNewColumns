#' User interface for rangeConditions module
#'
#' Provides the interface for user to input important characteristics about new
#' column the wish to create.
#'
#' @return a shiny \code{\link[shiny]{tagList}} containing a well panel of input
#'   widgets
#'
#' @param id standard parameter for {shiny modules}.
#'
#' @import shiny
#'
mod_rangeConditions_ui <- function(id) {
  ns <- NS(id)
  wellPanel(
    uiOutput(ns("casewhens")),
    div(style = "float: right;", "Note: bounds are inclusive but executed in order shown."),
    div(style = "float: left; color: red", htmlOutput(ns("row_coverage_msg")))
    # ,fluidRow(column(4, DT::dataTableOutput(ns("preview_newCol")) ))
  )
}

#' Server logic for rangeConditions module
#'
#'
#' @return an expression containing the `dplyr::between()` expressions used to
#'   create new column
#'
#' @import shiny
#' @importFrom purrr map map2 reduce pmap
#' @importFrom rlang call2 expr
#' @importFrom glue glue
#' @importFrom dplyr between mutate case_when group_by summarize select filter pull
#'
mod_rangeConditions_srv <- function(id, dat, grp, reference_var, else_group, else_name) {
  moduleServer(id, function(input, output, session) {

    ns <- session$ns

    # return the reference_var as a vector
    ref_vtr <- reactive({
      req(!is.null(reference_var()))
      dat()[,reference_var()]
    })

    # "step" is an argument for numericInput and determines the number of decimal
    # places the user can enter into this field when using between() which is
    # inclusive on the low and high side, so if the column is full of integers, we
    # this arg to 1, else it has to be one more decimal past what exists in the data
    resp_step <- reactive({
      req(!is.null(reference_var()))
      if(is.integer(ref_vtr())){ 1 } else { 10 ^ -(max(cntDecV(ref_vtr())) + 1) }
    })

    # For ease of use, create somewhat equally spaced factor levels to split data
    # resp_lvl <- reactive({
    #   req(ref_vtr())
    #   lvls <- levels(ggplot2::cut_number(ref_vtr(), if(else_group()) grp() + 1 else grp()))[1:grp()]
    #   l <- as.numeric(substr(lvls,2,stringr::str_locate(lvls,",")[1]-1))
    #   u <- as.numeric(substr(lvls,stringr::str_locate(lvls,",")[1] + 1, nchar(lvls) - 1))
    #   return(list(lowBound = l, upBound = u))
    # })

    # idea?
    # create casewhens_reset to set min & max values to new values whenever reference variable changes


    # create several vectors of text strings that will be used as
    # UI id's that have the same length as there are groups
    low <- reactive(paste0("low", seq_len(grp())))
    high <- reactive(paste0("high", seq_len(grp())))
    then_names <- reactive(paste0("then", seq_len(grp())))
    grp_placeholders <- reactive(paste("Group", seq_len(grp())))


  output$casewhens <- renderUI({
    fluidRow(
      column(3, purrr::map(low(), ~ tags$div(class = "add_padding", glue::glue("When {reference_var()} is between")))),
      column(1, purrr::map(low(), ~ numericInput(ns(.x), NULL, value = isolate(input[[.x]]) %||% min(ref_vtr(), na.rm = T), step = resp_step()) )),
      column(1, purrr::map(low(), ~ tags$div(class = "add_padding", "and"))),
      column(1, purrr::map(high(), ~ numericInput(ns(.x), NULL, value = isolate(input[[.x]]) %||% max(ref_vtr(), na.rm = T), step = resp_step()) )),
      column(2, purrr::map(high(), ~ tags$div(class = "add_padding", "the value will be"))),
      column(2, purrr::map2(then_names(), grp_placeholders(), ~  textInput(ns(.x), NULL, value = isolate(input[[.x]]), placeholder = .y) )),
      column(1, purrr::map2(then_names(),  grp_placeholders(), ~ tags$div(class = "add_padding red",
        glue::glue("{newCol_n()$cnts[newCol_n()$newCol == default_val(isolate(input[[.x]]), .y)]}/{nrow(dat())}"))))
    )
  })



    # get all the range low & high values + string outputs
    range_low <- reactive(purrr::map_dbl(low(), ~ default_val(input[[.x]], NA_real_)))
    range_high <- reactive(purrr::map_dbl(high(), ~ default_val(input[[.x]], NA_real_)))
    range_names <- reactive(purrr::map2_chr(then_names(), grp_placeholders(), ~ default_val(input[[.x]], .y)))

    # create a list of between statements to use in case_when in this module AND parent module
    between_expr <- reactive({
      temp <- purrr::pmap(
        list("between", reference_var(), range_low(), range_high(), range_names()),
        build_case_when_formula)

      if (else_group()) append(temp, rlang::expr(TRUE ~ !!else_name())) else  append(temp, rlang::expr(TRUE ~ "NA"))
    })

    # Create an expression call using mutate and the between_expr() object above
    mutate_expr_call <- reactive({
      colname <- "newCol"
      rlang::call2(
        quote(dplyr::mutate),
        !!colname := rlang::call2(quote(dplyr::case_when),!!!between_expr())
      )
    })

    # Insert that ^^ into a list with the data, and a
    # dplyr::select() on our reference variable
    all_expressions <- reactive({
      list(
        rlang::expr(dat()),
        rlang::expr(dplyr::select(reference_var())),
        mutate_expr_call()
      )
    })

    # Create the new column and group by it so we have accurate row
    # counts to display next to each condition
    mutated_dat <- reactive({
      !any(is.na(range_names()))
      all_expressions() %>%
        purrr::reduce(~ rlang::expr(!!.x %>% !!.y)) %>%
        eval()
    })

    # calculate row counts
    newCol_n <- reactive({
      req(mutated_dat())
      mutated_dat() %>%
        dplyr::group_by(newCol) %>%
        dplyr::summarize(cnts = dplyr::n())
    })

    # output$preview_newCol <- DT::renderDataTable({
    #   DT::datatable(newCol_n())
    # })


    # Determine how many rows have been accounted for.
    # if 'else' group is created, answer should be 100% coverage
    row_cov_n <- reactive({
      req(newCol_n())
      purrr::walk2(low(), high(), ~ req(input[[.x]], input[[.y]]))
      newCol_n() %>%
        dplyr::filter(newCol != 'NA') %>%
        dplyr::pull(cnts) %>%
        sum()
    })
    row_cov_pct <- reactive({
      req(row_cov_n(), dat())
      round(100 * (row_cov_n() / nrow(dat())), 2)
      })





    # temporary output... just a place holder until we find a better way of displaying
    output$row_coverage_msg <- renderUI({
      req(row_cov_pct()) #, exists(else_group")
      if(row_cov_pct() < 100 & else_group() == FALSE){
        HTML(glue::glue("Accounted for {row_cov_n()}/{nrow(dat())} patients ({row_cov_pct()}%) with ranges provided. Consider adding 'Else' Group."))
      }
    })

    # # Trying to disable addCol button when lacking patient coverage from child
    # # module, but failing
    # observe({
    #   shinyjs::toggleState("snc-addCol", condition = row_cov_pct() != 100) #"addCol"
    #    if(row_cov_pct() != 100) {shinyjs::disable("snc-addCol")
    #    } else {shinyjs::enable("snc-addCol")}
    # })

    return(between_expr())
  })
}
