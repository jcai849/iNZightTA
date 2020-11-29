# Use packman package to check whether required packages are installed.
# Install missing packages, and load all packages afterwards

if(!require(pacman)) install.packages("pacman")

devtools::install_github('charlie86/spotifyr', quiet=TRUE)

# pacman::p_load(
#                spotifyr,
#                shiny, 
#                inzightta,
#                rlang,
#                gutenbergr,
#                pdftools,
#                stringr,
#                stringi,
#                dplyr,
#                tidytext,
#                tidyr,
#                genius,
#                tidyRSS, 
#                rtweet, 
#                shinyjs, 
#                shinybusy,
#                shinyWidgets, 
#                GuardianR, 
#                quanteda, 
#                jsonlite, 
#                ggplot2, 
#                data.table, 
#                DT, 
#                textclean, 
#                googleVis, 
#                shinyBS, 
#                purrr, 
#                ggstance, 
#                ggthemes, 
#                forcats, 
#                rvest, 
#                dplyr, 
#                httr,
#                lubridate, 
#                readr,
#                tibble)

pacman::p_load(
  inzightta,
  shiny, 
  ggplot2, 
  data.table, 
  purrr, 
  quanteda
 )

source("ui_app.R", local = TRUE)
source("server_app.R", local = TRUE)
shiny::runApp(shiny::shinyApp(ui, server))
