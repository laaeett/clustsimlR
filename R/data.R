#' Single-cell Immunofluorescence Data for MCF10A cells after 24h of Dasatinib
#' treatment, in the Nucleus
#'
#' 150 immunofluorescence measurements, for MCF10A cells, of the nuclear levels
#'  of five proteins involved in cell cycle regulation and apoptosis. Data was
#'  collected using the cyclic immunofluorescence protocol (CycIF). Cells were
#'  treated with dasatinib, a tyrosine kinase inhibitor, for 24 hours before
#'  measurement. Dataset contains 50 cells for each concentration of drug
#'  applied (0.01, 0.031623, 0.1 uM). All protein levels in the full dataset was
#'  log10-transformed and standardised before truncation to specific proteins.
#'
#' @source HMS LINCS Center, \url{https://lincs.hms.harvard.edu/mcf10a/}
#'
#' @usage data(dasatinib)
#'
#' @format A data frame with 150 rows each representing a single cell,
#'  and 6 columns as follows:
#' \describe{
#'  \item{Cell no.}{A unique identifier for each cell.}
#'  \item{Conc}{Concentration of dasatinib applied (uM).}
#'  \item{E2F-1}{Level of E2F-1 protein measured, log10-transformed and scaled
#'  by z-scoring.}
#'  \item{FOXO1a}{Level of FOXO1a protein measured, log10-transformed and scaled
#'  by z-scoring.}
#'  \item{Cyclin-E1}{Level of Cyclin-E1 protein measured, log10-transformed and
#'  scaled by z-scoring.}
#'  \item{Cyclin-D1}{Level of Cyclin-D1 protein measured, log10-transformed and
#'  scaled by z-scoring.}
#' }
#'
#' @examples
#' \dontrun{
#'     dasatinib
#' }
#'
#' @references
#' Koleti, A., Terryn, R., Stathias, V., Chung, C., Cooper, D. J.,
#' Turner, J. P., Vidović, D., Forlin, M., Kelley, T. T., D’Urso, A.,
#' Allen, B. K., Torre, D., Jagodnik, K. M., Wang, L., Jenkins, S. L.,
#' Mader, C., Niu, W., Fazel, M., Mahi, N., … Schürer, S. C. (2018).
#' Data Portal for the Library of Integrated Network-based Cellular
#' Signatures (LINCS) program: integrated access to diverse large-scale
#' cellular perturbation response data. \emph{Nucleic Acids Research},
#' 46(D1), D558–D566. https://doi.org/10.1093/nar/gkx1063
#'
#' Kantarjian, H., Jabbour, E., Grimley, J., & Kirkpatrick, P. (2006).
#' Dasatinib. \emph{Nature Reviews Drug Discovery}, 5(9), 717–718.
#' https://doi.org/10.1038/nrd2135
#'
#' Lin, J.-R., Fallahi-Sichani, M., & Sorger, P. K. (2015).
#' Highly multiplexed imaging of single cells using a high-throughput cyclic
#' immunofluorescence method. \emph{Nature Communications}, 6(1), 8390.
#' https://doi.org/10.1038/ncomms9390
#'
'dasatinib'
