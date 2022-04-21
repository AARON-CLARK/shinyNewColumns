#' User interface for advConditions module
#'
#' Provides the interface for user to input important characteristics about new
#' column the wish to create.
#'
#' @return a shiny \code{\link[shiny]{tagList}} containing a well panels of input
#'   widgets
#'
#' @param id standard parameter for {shiny modules}.
#'
#' @import shiny
#'
mod_advConditions_ui <- function(id) {
  ns <- NS(id)
  wellPanel(
    h4(strong(paste0("#",substr(id,stringr::str_locate(id, "cond")[2] + 1,nchar(id))," If:"))),
    fluidRow(
      column(3, align="center", "Variable"),
      column(2, align="center", "Operator"),
      column(3, align="center", "Value"),
      column(2, align="center", "Next")
    ),
    uiOutput(ns("casewhens")),
    textInput(ns("then"), "Then Group Name:")
      # what if users want to supply an expression. For example, if
      # sepal.length > 0, then times sepal.length by -1? Work towards
      # supplying options like this.
  )
}

#' Server logic for advConditions module
#'
#'
#' @return nothing at the moment
#'
#' @import shiny
#' @importFrom purrr map map2 pmap
#' @importFrom rlang call2 expr
#' @importFrom glue glue
#' @importFrom dplyr between mutate case_when group_by summarize select
#' @importFrom stringr str_locate
#'
mod_advConditions_srv <- function(id, dat, cnt) {
  moduleServer(id, function(input, output, session) {

    ns <- session$ns

    observe({
      # if cond x doesn't exist in rv object, create it with initial value "1"

      # print("")
      # print(paste("initialize id:", id))
      # print(paste("cnt[[id]]:", cnt[[id]]))
      if(!(id %in% names(cnt))) cnt[[id]] <- 1
      if(cnt[[id]] <= 0) cnt[[id]] <- 1 # safety net
    })


    # Add to case counter upon button click
    observeEvent(input$add, {
      # print("")
      # print("Add")
      cnt[[paste0(id,"_chg")]] <-  1
    })

    # Subtract to case counter upon button click
    observeEvent(input$del, {
      # print("")
      # print("Minus")
      cnt[[paste0(id,"_chg")]] <-  (-1)
    })



    var_names <- reactive(paste0("var",seq_len(cnt[[id]])))
    ops_names <- reactive(paste0("ops",seq_len(cnt[[id]])))
    val_names <- reactive(paste0("val",seq_len(cnt[[id]])))
    nxt_names <- reactive(paste0("nxt",seq_len(cnt[[id]])))

    output$casewhens <- renderUI({
      num_ops <- c(">", "<", ">=", "<=")
      bth_ops <- c("=", "!=")
      chr_ops <- c("IN", "NOT IN", "CONTAINS", "DOESN'T CONTAIN")

      fluidRow(

        column(3, purrr::map(var_names(),
                             ~ div(style = "text-align: center;",
                                   selectInput(ns(.x), NULL, choices = names(dat()),
                                               selected = isolate(input[[.x]])) ))),

        column(2, purrr::map2(ops_names(), var_names(),
                              ~ div(style = "text-align: center;",
                                    selectInput(ns(.x), NULL,
                                                choices = if(all(sapply(dat()[input[[.y]]], is.numeric))) {c(bth_ops, num_ops)} else {c(bth_ops, chr_ops)},
                                                selected = isolate(input[[.x]])) ))),

        column(3, purrr::pmap(list(val_names(), var_names(), ops_names()), function(.x, .y, .z){
          div(style = "text-align: center;",
              # if variable is numeric, value must be numeric
              if(all(sapply(dat()[input[[.y]]], is.numeric))) {
                div(style = "padding-bottom:5px;",
                    numericInput(ns(.x), NULL, value = isolate(input[[.x]])) )
              } else {
                # Variable is not numeric
                # if operator is Contains or Doesn't Contain, user can enter free text
                if(input[[.z]] %in% c("CONTAINS", "DOESN'T CONTAIN")){
                  div(style = "padding-bottom:5px;",
                      textInput(ns(.x), NULL, value = isolate(input[[.x]])) )
                } else {
                  # operator is not contains
                  if(input[[.z]] %in% c("=", "!=")){ # looking for single value
                    selectInput(ns(.x), NULL, choices = unique(dat()[input[[.y]]]),
                                selected = isolate(input[[.x]]))
                  } else {
                    # user can specify multiple values (operator is "in" or "not in")
                    selectInput(ns(.x), NULL, choices = unique(dat()[input[[.y]]]),
                                selected = isolate(input[[.x]]), multiple = TRUE)
                  }
                }
              }
          )
        }
        )),

        column(2, if(cnt[[id]] != 1) purrr::map(nxt_names()[1:( length(nxt_names())-1)],
                                                 ~ selectInput(ns(.x), NULL,
                                                               choices = c("AND" = "&", "OR" = "|"),
                                                               selected = isolate(input[[.x]])) )),
        column(3,
               if(cnt[[id]] != 1) div(style = "display: inline-block;",
                                       actionButton(ns("del"), "Remove") ),
               div(style = "display: inline-block;",
                   actionButton(ns("add"), "Add Another") ))
      )

    })

    # module needs to return all the conditions for a single group as an expression
    # return()
  })
}
