# Use packman package to check whether required packages are installed.
# Install missing packages, and load all packages afterwards

if(!require(pacman)) install.packages("pacman")

# library(inzightta)
# library(rlang)
# library(gutenbergr)
# library(pdftools)
# library(stringr)
# library(stringi)
# library(dplyr)
# library(tidytext)
# library(tidyr)
# library(DT)
# library(spotifyr)
# library(genius)
# library(tidyRSS)
# library(rtweet)
# library(shinyjs)
# library(shinybusy)
# library(shinyWidgets)
# library(GuardianR)
# library(quanteda)
# library(jsonlite)
# library(data.table)
# library(ggplot2)
# library(textclean)
# 
# library(googleVis)

# ##### Readability
# library(shiny)
# library(shinyBS)
# library(quanteda)
# library(purrr)
# library(ggplot2)
# library(ggstance)
# library(ggthemes)
# library(forcats)

devtools::install_github('charlie86/spotifyr')

pacman::p_load(
               spotifyr,
               shiny, 
               inzightta,
               rlang,
               gutenbergr,
               pdftools,
               stringr,
               stringi,
               dplyr,
               tidytext,
               tidyr,
               genius,
               tidyRSS, 
               rtweet, 
               shinyjs, 
               shinybusy,
               shinyWidgets, 
               GuardianR, 
               quanteda, 
               jsonlite, 
               ggplot2, 
               data.table, 
               DT, 
               textclean, 
               googleVis, 
               shinyBS, 
               purrr, 
               ggstance, 
               ggthemes, 
               forcats, 
               rvest, 
               dplyr, 
               httr, 
               lubridate, 
               readr,
               tibble)

source("ui_app.R", local = TRUE)$value
source("server_app.R", local = TRUE)$value
shinyApp(ui, server) # launch app