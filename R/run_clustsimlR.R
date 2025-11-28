# Shiny app launcher

#' Launch Shiny App for clustsimlR
#'
#' A function that launches the Shiny app for clustsimlR. Users should note
#'  that the Shiny app is intended for exploratory purposes, and that users
#'  who wish to customise their analyses using this package should use the
#'  functions in this package directly.
#'
#' @return No return value, but open up a Shiny page.
#'
#' @examples
#' \dontrun{
#' clustsimlR::run_clustsimlR()
#' }
#'
#' @references
#'
#' Attali, D., Edwards, T. (2024). _shinyalert: Easily Create Pretty Popup
#'  Messages (Modals) in 'Shiny'_. doi:10.32614/CRAN.package.shinyalert
#'  <https://doi.org/10.32614/CRAN.package.shinyalert>, R package version
#'  3.1.0, <https://CRAN.R-project.org/package=shinyalert>.
#'
#' Attali, D., Sali, A. (2024). _shinycssloaders: Add Loading Animations to a
#'   'shiny' Output While It's Recalculating_.
#'   doi:10.32614/CRAN.package.shinycssloaders
#'   <https://doi.org/10.32614/CRAN.package.shinycssloaders>, R package version
#'   1.1.0, <https://CRAN.R-project.org/package=shinycssloaders>.
#'
#' Chang, W., Cheng, J., Allaire, J., Sievert, C., Schloerke, B., Xie, Y.,
#'  Allen, J., McPherson, J., Dipert, A., Borges, B. (2025). _shiny: Web
#'  Application Framework for R_. doi:10.32614/CRAN.package.shiny
#'  <https://doi.org/10.32614/CRAN.package.shiny>, R package version 1.11.1,
#'  <https://CRAN.R-project.org/package=shiny>.
#'
#' Sievert, C., Cheng, J., Aden-Buie, G. (2025). _bslib: Custom 'Bootstrap'
#'  'Sass' Themes for 'shiny' and 'rmarkdown'_. doi:10.32614/CRAN.package.bslib
#'  <https://doi.org/10.32614/CRAN.package.bslib>, R package version 0.9.0,
#'  <https://CRAN.R-project.org/package=bslib>.
#'
#' @export
#' @importFrom shiny runApp

run_clustsimlR <- function() {
    app_dir <- system.file("shiny-scripts", package = "clustsimlR")
    shiny::runApp(appDir = app_dir,
                  display.mode = "normal")
    return(invisible(NULL))
}

# [END]
