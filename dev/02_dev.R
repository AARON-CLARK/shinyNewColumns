# Building a Prod-Ready, Robust Shiny Application.
#
# README: each step of the dev files is optional, and you don't have to
# fill every dev scripts before getting started.
# 01_start.R should be filled at start.
# 02_dev.R should be used to keep track of your development during the project.
# 03_deploy.R should be used once you need to deploy your app.
#
#
###################################
#### CURRENT FILE: DEV SCRIPT #####
###################################

# Engineering

## Dependencies ---- ran
## Add one line by package you want to add as dependency
# usethis::use_package("shiny") # already added as a recommended package
# usethis::use_package("DT") # already added as a recommended package
# usethis::use_package("glue") # already added as a recommended package
usethis::use_pipe()
usethis::use_package("rlang")
usethis::use_package("ggplot2")
usethis::use_package("dplyr")
usethis::use_package("stringr")
usethis::use_package("purrr")
usethis::use_package("shinyFeedback")
usethis::use_package("magrittr")

# usethis::use_package("shinyjs")
# usethis::use_package("tibble")
# usethis::use_package("cicerone")
# usethis::use_package("shinyWidgets")




## Add modules ---- didn't run any ran
## Create a module infrastructure in R/
## Only argument is Name of the module

# golem::add_module( name = "dataUpload" ) # ran


## Add helper functions ---- didn't run
## Creates fct_* and utils_*
# golem::add_utils( "strObjs" )
# golem::add_utils( "helpers" )
#
# golem::add_fct( "helpers", module = "dataComply" )
# golem::add_fct( "helpers", module = "dataComplyRules" )




## External resources - ran
## Creates .js and .css files at inst/app/www
# golem::add_js_handler( "handlers" ) # ac golem: none
# golem::add_js_file( "script" )
# golem::add_js_file( "accordian" )
# golem::add_js_file( "analytics" )
# golem::add_js_file( "recipe" )
# golem::add_js_file( "detect_browser" )

# golem::add_css_file( "yeti" )
# golem::add_css_file( "styles" )

###################################################################
# ac golem: Aaron stopped here and pushed code to team on 6/3/2020
###################################################################

## Add internal datasets ---- not run
## If you have data in your package
# usethis::use_data_raw( name = "example", open = FALSE )

## Tests ---- not run yet
## Add one line by test you want to create
# usethis::use_test( "app" )

# Documentation

## Vignette ---- not run
# usethis::use_vignette("shinyNewColumns")
# usethis::use_vignette("x00_Data_Upload")

# devtools::build_vignettes() # don't use (takes too long), instead use...

# Before submitting a PR, run this code & update NEWS.md
usethis::use_version("patch") #choices: "dev", "patch", "minor", "major"

# Build pkg, including vignettes. Do this before updating documentation.
# devtools::build()

# update pkgdown site only if user needs refreshed documentation
# usethis::use_pkgdown() # Run once to configure your package to use pkgdown
# pkgdown::build_articles(pkg = ".")
# pkgdown::build_articles_index()
# pkgdown::build_reference_index(pkg = rprojroot::is_r_package$find_file())
# pkgdown::build_site(pkg = rprojroot::is_r_package$find_file()) # Run to build the website
# pkgdown::build_news()

# # GitHub Actions
# usethis::use_github_action()
#
# # Chose one of the three
# # See https://usethis.r-lib.org/reference/use_github_action.html
# usethis::use_github_action_check_standard()

# # Add action for PR
# usethis::use_github_action_pr_commands()


## Code coverage ---- not run
## (You'll need GitHub there)
# usethis::use_github() # don't need to do this. AC manually created a remote origin in terminal and pushed to github.
# usethis::use_travis()
# usethis::use_appveyor()

# # You're now set! ---- not run
# # go to dev/03_deploy.R
# rstudioapi::navigateToFile("dev/03_deploy.R") # no file

