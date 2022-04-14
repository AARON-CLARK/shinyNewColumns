# Building a Prod-Ready, Robust Shiny Application.
#
# README: each step of the dev files is optional, and you don't have to
# fill every dev scripts before getting started.
# 01_start.R should be filled at start.
# 02_dev.R should be used to keep track of your development during the project.
# 03_deploy.R should be used once you need to deploy your app.
#
#
########################################
#### CURRENT FILE: ON START SCRIPT #####
########################################

## Fill the DESCRIPTION ---- ran
## Add meta data about your application
golem::fill_desc(
  pkg_name = "shinyNewColumns", # The Name of the package containing the App
  pkg_title = "shinyNewColumns: a shiny module to derive custom columns in R data.frames on the fly", # The Title of the package containing the App
  pkg_description = "The shinyNewColumns module is simply a action button titled 'Create New Column' that can be placed anywhere in an existing shiny application. Upon clicking, a modal containing a user-friendly interface will launch & allow the user to build a new column off an existing data source. Specifically, the user will be able to select a new column type, name, label, and reference column(s) during deriviation. Along the way, the module helps the user visiualize existing column's distribution and summaries. Upon completion, the module returns the supplied data frame with the new column appended, and optional the dplyr::mutate() expressions used to create said column.",
  author_first_name = "Aaron", # Your First Name
  author_last_name = "Clark", # Your Last Name
  author_email = c("clark.aaronchris@gmail.com"), # Your Email
  repo_url = "https://github.com/AARON-CLARK/shinyNewColumns" # The URL of the GitHub Repo (optional)
)

## Set {golem} options ---- not ran
# golem::set_golem_options()

## Create Common Files ---- ran
## See ?usethis for more information
usethis::use_mit_license("Aaron Clark")
# usethis::use_agpl_license(version = 3, include_future = TRUE)

usethis::use_readme_rmd( open = TRUE )
usethis::use_code_of_conduct()
usethis::use_lifecycle_badge( "Experimental" ) #Experimental, Maturing, Stable, Superseded, Archived, Dormant, Questioning
# usethis::use_news_md( open = FALSE ) # Not ran... yet

## Use git ---- not ran. Already using git
# usethis::use_git("Initial Commit")

## Init Testing Infrastructure ---- not ran yet
## Create a template for tests
# Call `use_test()` to initialize a basic test file and open it for editing.
# golem::use_recommended_tests()

## Use Recommended Packages ---- ran slightly altered
# By Default, this will add “shiny”, “DT”, “attempt”, “glue”, “htmltools”, and “golem” as a dependency
# to our package. Since we don't need all of those, we'll adjusted the recommended vector...
golem::use_recommended_deps(recommended = c("shiny", "DT",  "glue", "golem")) #"attempt", "htmltools",

## Favicon ---- ran
# If you want to change the favicon (default is golem's one)
golem::remove_favicon()
golem::use_favicon("inst/app/www/shinyNewColumnsv7.ico") # path = "path/to/ico". Can be an online file.
# Favicon is automatically linked in app_ui via `golem_add_external_resources()`

## Add helper functions ---- not ran. Go checkout new files
# golem::use_utils_ui() # File created at ~tidyCDISC/R/golem_utils_ui.R
# golem::use_utils_server() # tidyCDISC/R/golem_utils_server.R

# You're now set! ----

# go to dev/02_dev.R
rstudioapi::navigateToFile( "dev/02_dev.R" )

