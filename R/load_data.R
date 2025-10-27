# Functions for loading toy datasets

dasatinib <- function() {
    dasatinib <- readr::read_csv("data/nucleus 24/dasatinib_01.csv",
                             col_types = cols(...1 = col_integer()))
    colnames(dasatinib)[1] <- "Cell no."
    return(dasatinib)
}
