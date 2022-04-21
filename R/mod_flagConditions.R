#' User interface for flagConditions module
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
mod_flagConditions_ui <- function(id) {
  ns <- NS(id)
  wellPanel(
    uiOutput(ns("casewhens"))
    , div(style = "float: right;", "Note: observations not covered under these conditions or handled with an 'else' group will recieve an NA (or missing) value.")
    # , div(style = "float: left; color: red", htmlOutput(ns("row_coverage_msg")))
    # fluidRow(column(4, DT::dataTableOutput(ns("preview_newCol")) ))
  )
}

#' Server logic for flagConditions module
#'
#'
#' @return an expression containing the `dplyr::between()` expressions used to
#'   create new column
#'
#' @import shiny
#' @importFrom purrr map map2 reduce pmap map_dbl
#' @importFrom rlang call2 expr
#' @importFrom glue glue
#' @importFrom dplyr between mutate case_when group_by summarize select
#'
mod_flagConditions_srv <- function(id, dat, grp, reference_var, else_group, else_name, flg_typ) {
  moduleServer(id, function(input, output, session) {

    ns <- session$ns

    # # return the reference_var as a vector
    # ref_vtr <- reactive({
    #   req(!is.null(reference_var()))
    #   dat()[,reference_var()]
    # })
    #
    # # "step" is an argument for numericInput and determines the number of decimal
    # # places the user can enter into this field when using between() which is
    # # inclusive on the low and high side, so if the column is full of integers, we
    # # this arg to 1, else it has to be one more decimal past what exists in the data
    # resp_step <- reactive({
    #   req(!is.null(reference_var()))
    #   if(is.integer(ref_vtr())){ 1 } else { 10 ^ -(max(cntDecV(ref_vtr())) + 1) }
    # })
    #
    # # For ease of use, create somewhat equally spaced factor levels to split data
    # # resp_lvl <- reactive({
    # #   req(ref_vtr())
    # #   lvls <- levels(ggplot2::cut_number(ref_vtr(), if(else_group()) grp() + 1 else grp()))[1:grp()]
    # #   l <- as.numeric(substr(lvls,2,stringr::str_locate(lvls,",")[1]-1))
    # #   u <- as.numeric(substr(lvls,stringr::str_locate(lvls,",")[1] + 1, nchar(lvls) - 1))
    # #   return(list(lowBound = l, upBound = u))
    # # })

    # idea?
    # create casewhens_reset to set min & max values to new values whenever reference variable changes


    # create several vectors of text strings that will be used as
    # UI id's that have the same length as there are groups
    # low <- reactive(paste0("low", seq_len(grp())))
    # high <- reactive(paste0("high", seq_len(grp())))
    # then <- reactive(paste0("then", seq_len(grp())))
    # grp_ph <- reactive(paste("Group", seq_len(grp())))


    var <- reactive(paste0("var",seq_len(grp())))
    ops <- reactive(paste0("ops",seq_len(grp())))
    val <- reactive(paste0("val",seq_len(grp())))

    then <- reactive(paste0("then", seq_len(grp())))
    grp_ph <- reactive(paste("Group", seq_len(grp())))



    output$casewhens <- renderUI({
      req(flg_typ())
      flg_choices <- unlist(str_split(flg_typ(), "\\/"))
      if(flg_typ() == "TRUE/FALSE") flg_choices <- as.logical(flg_choices)

      fluidRow(
        column(1, purrr::map(var(), ~ tags$div(class = "add_padding", glue::glue("IF")))),

        column(3, purrr::map(var(),~ div(style = "text-align: center;",
          selectInput(ns(.x), NULL, choices = names(dat()), selected = isolate(input[[.x]])) ))),

        column(2, purrr::map2(ops(), var(), ~ div(style = "text-align: center;",
          selectInput(ns(.x), NULL, choices =
            if(all(sapply(dat()[input[[.y]]], is.numeric))) {c(eql_ops, num_ops)} else {c(eql_ops, chr_ops)},
            selected = isolate(input[[.x]])) ))),

        column(2, purrr::pmap(list(val(), var(), ops()), function(.x, .y, .z){ #, means() .m
          div(style = "text-align: center;",
              # if variable is numeric, value must be numeric
              if(all(sapply(dat()[input[[.y]]], is.numeric))) {
                div(style = "padding-bottom:5px;",
                    numericInput(ns(.x), NULL,
                      value = isolate(input[[.x]]) %||% 0) )
              } else {
                # --- Variable is not numeric ---
                # if operator is Contains or Doesn't Contain, user can enter free text
                if(input[[.z]] %in% c("CONTAINS", "DOESN'T CONTAIN")){
                  div(style = "padding-bottom:5px;",
                      textInput(ns(.x), NULL, value = isolate(input[[.x]])) )
                } else {
                  # operator is not contains
                  # and is = or !=
                  if(input[[.z]] %in% c("==", "!=")){ # looking for single value
                    selectInput(ns(.x), NULL, choices = unique(dat()[input[[.y]]]),
                                selected = isolate(input[[.x]]))
                  } else {
                    # else user can specify multiple values (operator is "in" or "not in")
                    selectInput(ns(.x), NULL, choices = unique(dat()[input[[.y]]]),
                                selected = isolate(input[[.x]]), multiple = TRUE)
                  }
                }
              }
          )
        }
        )),

        column(1, purrr::map(val(), ~ tags$div(class = "add_padding", "THEN"))),
        column(2, purrr::map(then(), ~
          if(flg_typ() != 'Custom'){
            selectInput(ns(.x), NULL, choices = flg_choices, selected = isolate(input[[.x]]))
          } else {
            textInput(ns(.x), NULL, value = isolate(input[[.x]]), placeholder = flg_typ())
          }
        ))
        ,column(1, purrr::map(then(), ~ tags$div(class = "add_padding red",
            glue::glue("120/150"))))
            # glue::glue("{newCol_n()$cnts[newCol_n()$newCol == default_val(isolate(input[[.x]]), .y)]}/{nrow(dat())}"))))

        # column(3, purrr::map(low(), ~ tags$div(class = "add_padding", glue::glue("When {reference_var()} is between")))),
        # column(1, purrr::map(low(), ~ numericInput(ns(.x), NULL, value = isolate(input[[.x]]) %||% min(ref_vtr(), na.rm = T), step = resp_step()) )),
        # column(1, purrr::map(low(), ~ tags$div(class = "add_padding", "and"))),
        # column(1, purrr::map(high(), ~ numericInput(ns(.x), NULL, value = isolate(input[[.x]]) %||% max(ref_vtr(), na.rm = T), step = resp_step()) )),
        # column(2, purrr::map(high(), ~ tags$div(class = "add_padding", "the value will be"))),
        # column(2, purrr::map2(then(), grp_ph(), ~  textInput(ns(.x), NULL, value = isolate(input[[.x]]), placeholder = .y) )),
        # column(1, purrr::map2(then(),  grp_ph(), ~ tags$div(class = "add_padding red", glue::glue("{newCol_n()$cnts[newCol_n()$newCol == default_val(isolate(input[[.x]]), .y)]}/{nrow(dat())}"))))
      )
    })

    # means <- reactive({
    #   req(var())
    #   purrr::map_dbl(var(), function(x) {
    #     req(input[[x]])
    #     if(all(sapply(dat()[input[[x]]], is.numeric))) {
    #       mean(dat()[,input[[x]]], na.rm = T)
    #     } else {
    #       NA_real_
    #     }
    #   })
    # })
    # observe({
    #   req(means())
    #   pmap(list(var(), val(), means()), function(c, v, m) {
    #     if(all(sapply(dat()[input[[c]]], is.numeric)) & is.null(input[[v]])){
    #       updateNumericInput(ns(input[[v]]), NULL, value = m)
    #     }
    #   })
    # })
    # observe({
    #   print("")
    #   print(paste(var(), means()))
    # })

    # get all the range low & high values + string outputs
    resp_var <- reactive(purrr::map_chr(var(), ~ default_val(input[[.x]], NA_character_)))
    resp_ops <- reactive(purrr::map_chr(ops(), ~ default_val(input[[.x]], NA_character_)))
    # resp_val <- reactive(purrr::map_chr(ops(), ~ default_val(input[[.x]], NA_character_)))
    # It doesn't really matter if we classify the value as numeric or character does it?
    resp_val <- reactive({
      purrr::map2(val(), var(), function(.x, .y){ #, means() .m
        # if variable is numeric, so value must be numeric
        if(all(sapply(dat()[input[[.y]]], is.numeric))) {
          default_val(input[[.x]], NA_real_)
        } else { # --- Variable is not numeric ---
          glue::glue("'{default_val(input[[.x]], NA_character_)}'")
        }
      }
      )
    })
    resp_flg <- reactive(purrr::map_chr(then(), ~ default_val(input[[.x]], NA_character_ )))

    observe({
      print("")
      print(
        paste(
          ifelse(any(stringr::str_detect(resp_ops(), c("NOT", "DOESN'T"))), "!(", ""),
          if(stringr::str_detect(resp_ops(), "CONTAIN")) {
            paste("stringr::str_detect(", resp_var(), ", ", unlist(resp_val()), ")")
          } else {
            paste(
              resp_var(),
              ifelse(resp_ops() == "IN", "%in%", resp_ops()),
              resp_val(),
            )
          },
          ifelse(any(stringr::str_detect(resp_ops(), c("NOT", "DOESN'T"))), ")", ""),
          "~",
          resp_flg()
        )
      )
    })
    # stringr::str_detect(resp_var(), resp_val())
    # stringr::str_detect(resp_ops(), c("NOT", "DOESN'T"))
    # stringr::str_detect(c("NOT IN", "DOESN'T CONTAIN"), c("NOT"))
    # c("NOT", "DOESN'T") %in% c("NOT IN", "DOESN'T CONTAIN")



    # create a list of between statements to use in case_when in this module AND parent module
    # flag_expr <- reactive({
    #   temp <- purrr::pmap(
    #     list(resp_var(), resp_ops(), resp_val(), resp_flg()),
    #       function(func, var, ops, val, flg) {
    #         rlang::expr(!!rlang::call2(func, rlang::sym(value), low, high, .ns = "dplyr") ~ !!string)
    #       }
    #     )
    #
    #   if (else_group()) append(temp, rlang::expr(TRUE ~ !!else_name())) else  append(temp, rlang::expr(TRUE ~ "NA"))
    # })
    #
    # # Create an expression call using mutate and the flag_expr() object above
    # mutate_expr_call <- reactive({
    #   colname <- "newCol"
    #   rlang::call2(
    #     quote(dplyr::mutate),
    #     !!colname := rlang::call2(quote(dplyr::case_when),!!!flag_expr())
    #   )
    # })

    # # Insert that ^^ into a list with the data, and a
    # # dplyr::select() on our reference variable
    # all_expressions <- reactive({
    #   list(
    #     rlang::expr(dat()),
    #     rlang::expr(dplyr::select(resp_var())),
    #     mutate_expr_call()
    #   )
    # })
    #
    # # Create the new column and group by it so we have accurate row
    # # counts to display next to each condition
    # mutated_dat <- reactive({
    #   !any(is.na(resp_flg()))
    #   all_expressions() %>%
    #     purrr::reduce(~ rlang::expr(!!.x %>% !!.y)) %>%
    #     eval()
    # })
    #
    # # calculate row counts
    # newCol_n <- reactive({
    #   mutated_dat() %>%
    #     dplyr::group_by(newCol) %>%
    #     dplyr::summarize(cnts = dplyr::n())
    # })
    #
    # # output$preview_newCol <- DT::renderDataTable({
    # #   DT::datatable(newCol_n())
    # # })
    #
    #
    # # Determine how many rows have been accounted for.
    # # if 'else' group is created, answer should be 100% coverage
    # row_cov_n <- reactive({
    #   req(ref_vtr())
    #   sum(purrr::map2_int(low(), high(), function(.x, .y) {
    #         req(input[[.x]], input[[.y]])
    #         sum(dplyr::between(ref_vtr(),input[[.x]], input[[.y]]), na.rm = T)
    #   }))
    # })
    # row_cov_pct <- reactive({
    #   req(row_cov_n(), dat())
    #   round(100 * (row_cov_n() / nrow(dat())), 2)
    #   })
    #
    # # temporary output... just a place holder until we find a better way of displaying
    # output$row_coverage_msg <- renderUI({
    #   req(row_cov_pct())
    #   if(row_cov_pct() < 100 & else_group() == FALSE){
    #     HTML(glue::glue("Accounted for {row_cov_n()}/{nrow(dat())} patients ({row_cov_pct()}%) with ranges provided. Consider adding 'Else' Group."))
    #   }
    # })
    #
    # # # Trying to disable addCol button when lacking patient coverage from child
    # # # module, but failing
    # # observe({
    # #   shinyjs::toggleState("snc-addCol", condition = row_cov_pct() != 100) #"addCol"
    # #    if(row_cov_pct() != 100) {shinyjs::disable("snc-addCol")
    # #    } else {shinyjs::enable("snc-addCol")}
    # # })
    #
    # return(flag_expr())
  })
}
