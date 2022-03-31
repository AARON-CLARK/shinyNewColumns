# infix if empty string supply value
# otherwise return value
`%empty%` <- function (x, y) {
  if (stringr::str_length(x) == 0) y else x
}

`%||%` <- function (x, y) {
  if (is.null(x))
    y
  else x
}

build_case_when_formula <- function(func, value, low, high, string) {
  rlang::expr(!!rlang::call2(func, rlang::sym(value), low, high, .ns = "dplyr") ~ !!string)
}

default_val <- function(x, value) {
  if (isTruthy(x)) {
    x
  } else {
    value
  }
}

cntDecV <- Vectorize(function(x) {
  if ((x %% 1) != 0) {
    nchar(strsplit(sub('0+$', '', as.character(x)), ".", fixed=TRUE)[[1]][[2]])
  } else {
    return(0)
  }
})

# define the placeholder for the "Else" Group here, as it is referenced in many
# places
else_ph_util <- "Other Group"
var_name_ph_util <- "ColName"
lab_name_ph_util <- "LabName"
