#' Launch app for iNZightTA
#'
#' @return void; launch app for iNZightTA
#'
#' @export
main <- function() {
	if (!require(pacman)) install.packages("pacman")
	devtools::install_github('charlie86/spotifyr', quiet=TRUE)
	pacman::p_load(
		       inzightta,
		       shiny,
		       ggplot2,
		       data.table,
		       purrr,
		       quanteda
		       )

    source(system.file("app", "ui.R", package = "inzightta"))
    source(system.file("app", "server.R", package = "inzightta"))
    shiny::runApp(shiny::shinyApp(ui, server))
}
