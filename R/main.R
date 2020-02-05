#' Launch app for iNZightTA
#'
#' @return void; launch app for iNZightTA
#'
#' @export
main <- function(){
    shiny::runApp(system.file("app", package = "inzightta"))
    }
