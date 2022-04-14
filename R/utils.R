
#' Empty infix
#'
#' If empty string supply value otherwise return value
#'
#' @param x,y strings
#' @importFrom stringr str_length
#' @return a string
#'
`%empty%` <- function (x, y) {
  if (stringr::str_length(x) == 0) y else x
}

#' Null infix
#'
#' If first value is null then return second value
#'
#' @param x,y strings
#' @importFrom stringr str_length
#' @return a string
#'
`%||%` <- function (x, y) {
  if (is.null(x))
    y
  else x
}

#' Formula for dplyr::case_when
#'
#'
#' @param func func
#' @param value value
#' @param low low
#' @param high high
#' @param string string
#'
#' @importFrom rlang call2 expr sym
#' @return an expression
#'
build_case_when_formula <- function(func, value, low, high, string) {
  rlang::expr(!!rlang::call2(func, rlang::sym(value), low, high, .ns = "dplyr") ~ !!string)
}

#' Find the default value, if it exists
#'
#' test if first value Truthy, and if so use it, else use a back up value
#'
#' @param x,value values
#' @importFrom stringr str_length
#'
default_val <- function(x, value) {
  if (isTruthy(x)) {
    x
  } else {
    value
  }
}

#' cntDecV
#'
#' A vectorized decimal counting function
#'
#' @param x numeric
#' @return integer
#'
cntDecV <- Vectorize(function(x) {
  if ((x %% 1) != 0) {
    nchar(strsplit(sub('0+$', '', as.character(x)), ".", fixed=TRUE)[[1]][[2]])
  } else {
    return(0)
  }
})

# Define the placeholder for the "Else" Group
# here, as it is referenced in many places
else_ph_util <- "Other Group"
var_name_ph_util <- "ColName1"
lab_name_ph_util <- "LabName1"
