# Functions to check data for various 'red flags'

#' Check for possible zero-inflation in the data
#'
#' Checks data for possible zero-inflation by comparing the number of zero
#'  values in the data to a threshold (default= 0.5), for which
#'  the data is considered zero-inflated if the number of zero values observed
#'  in the data exceeds the expected number as computed from the threshold. This
#'  function ignores NA or NaN values.
#'
#' @param data A data frame where each column represents a gene and each row
#'  represents a single cell.
#' @param overall_zero_threshold A double representing the maximum proportion of
#'  the data that can be zero before being considered zero-inflated. Default is
#'  0.5, indicating that we consider the data zero-inflated if over half of it
#'  are zeroes.
#' @returns A logical value representing whether the proportion of zeroes in the
#'  data is greater than the chosen threshold.
#'
#' @examples
#' \dontrun{
#' # load dasatinib dataset
#' data(dasatinib)
#' dframe <- dasatinib
#'
#' # check for zero-inflation with default threshold of 0.5
#' checkZeroInflation(dframe)
#'
#' # change some of the data to zero
#' dframe_zero_inflated <- data.frame(dframe)
#' dframe_zero_inflated[, 2:4] <- 0
#' checkZeroInflation(dframe_zero_inflated)
#' }
#'
checkZeroInflation <- function(data,
                               overall_zero_threshold = 0.5) {

    if (overall_zero_threshold > 1 || overall_zero_threshold < 0) {
        stop("overall_zero_threshold must be between 0 and 1.")
    }

    num_vals <- nrow(data) * ncol(data)

    # expected number of zeroes in the data
    exp_zeroes <- num_vals * overall_zero_threshold

    # observed number of zeroes
    num_zeroes <- sum(data == 0, na.rm = TRUE)

    return(num_zeroes > exp_zeroes)
}


#' Clean zero-inflated genes and cells from the data
#'
#' Cleans the data by removing zero-inflated genes (columns) and zero-inflated
#'  cells (rows). A gene or cell is considered zero-inflated if the number of
#'  zero values exceeds the number of zero values expected based on a threshold.
#'  This function ignores NA or NaN values.
#'
#' @param data A data frame where each column represents a gene and each row
#'  represents a single cell.
#' @param cell_zero_threshold A double representing the maximum proportion of
#'  the data for a single cell (row) that can be zero before being considered
#'  zero-inflated. Default is 0.5, indicating that we consider the data for that
#'  cell zero-inflated if over half of it are zeroes.
#' @param gene_zero_threshold A double representing the maximum proportion of
#'  the data for a single gene (column) that can be zero before being considered
#'  zero-inflated. Default is 0.5, indicating that we consider the data for that
#'  gene zero-inflated if over half of it are zeroes.
#'
#' @returns A 'cleaned' version of data, where zero-inflated columns and rows
#'  are removed.
#'
#' @examples
#' \dontrun{
#' # load dasatinib dataset
#' data(dasatinib)
#' dframe <- dasatinib
#'
#' # change some of the data to zero
#' dframe_zero_inflated <- data.frame(dframe)
#' dframe_zero_inflated[,2:4] <- 0
#' checkZeroInflation(dframe_zero_inflated)
#'
#' dframe_cleaned <- cleanZeroInflation(dframe_zero_inflated)
#' checkZeroInflation(dframe_cleaned)
#' }
#'
cleanZeroInflation <- function(data,
                               cell_zero_threshold = 0.5,
                               gene_zero_threshold = 0.5) {

    if (cell_zero_threshold > 1 || cell_zero_threshold < 0) {
        stop("cell_zero_threshold must be between 0 and 1.")
    }

    if (gene_zero_threshold > 1 || gene_zero_threshold < 0) {
        stop("gene_zero_threshold must be between 0 and 1.")
    }

    # check column-wise = gene
    rm_genes <- c()
    for (i in 1:ncol(data)) {
        curr_gene <- data[, i]
        exp_zeroes <- length(curr_gene) * gene_zero_threshold
        num_zeroes <- sum(curr_gene == 0, na.rm = TRUE)

        # mark for removal if no. of zeroes exceeds threshold
        if (num_zeroes > exp_zeroes) {
            rm_genes <- c(rm_genes, colnames(data)[i])
        }
    }
    data <- data[, !(colnames(data) %in% rm_genes), drop = FALSE]

    # if all genes were removed
    if (ncol(data) == 0) {
        stop("All genes were removed by zero-inflation filtering\n")
    }

    # check row-wise = cell
    rm_cells <- c()
    for (i in 1:nrow(data)) {
        curr_cell <- data[i, ]
        exp_zeroes <- length(curr_cell) * cell_zero_threshold
        num_zeroes <- sum(curr_cell == 0, na.rm = TRUE)

        #mark for removal if no. of zeroes exceed threshold
        if (num_zeroes > exp_zeroes) {
            rm_cells <- c(rm_cells, i)
        }
    }
    if (length(rm_cells) > 0){
        data <- data[-rm_cells, , drop = FALSE]
    }

    # inform what was removed
    message(sprintf("Removed %d zero-inflated genes: %s\n",
                    length(rm_genes),
                    paste(rm_genes, collapse = ", ")))
    message(sprintf("Removed %d zero-inflated cells: %s\n",
                    length(rm_cells),
                    paste(rm_cells, collapse = ", ")))

    return(data)
}


#' Clean NA or NaN values
#'
#' Cleans the data by imputing NA and NaN values in data using the mean
#'  expression.
#'
#' @param data A data frame where each column represents a gene and each row
#'  represents a single cell.
#' @returns A 'cleaned' version of the data, with imputed NA and NaN values.
#'
#' @examples
#' \dontrun{
#' # load dasatinib dataset
#' data(dasatinib)
#' dframe <- dasatinib
#'
#' # check for missing values
#' checkMissingData(dframe)
#'
#' # change one of the values to NA
#' dframe_one_NA <- data.frame(dframe)
#' dframe_one_NA[1,2] <- NA
#' dframe_NA_cleaned <- cleanMissingData(dframe_one_NA)
#' checkMissingData(dframe_NA_cleaned)
#'
#' #change one of the values to NaN
#' dframe_one_NaN <- data.frame(dframe)
#' dframe_one_NaN[1,2] <- 1/0
#' dframe_NaN_cleaned <- cleanMissingData(dframe_one_NaN)
#' checkMissingData(dframe_NaN_cleaned)
#' checkMissingData(dframe_one_NaN)
#' }
#' @importFrom Hmisc impute
#' @importFrom dplyr mutate_all
#' @importFrom magrittr %>%
#'
cleanMissingData <- function(data) {

    # changing NaN to NA for imputation
    data <- data %>% dplyr::mutate_all(~ifelse(is.nan(.), NA, .))

    # imputation based on column
    for (i in 1:ncol(data)) {
        # imputation from mean
        data[, i] <- Hmisc::impute(data[, i], fun = mean)

        # any remaining values, force into 0
        data[, i] <- Hmisc::impute(data[, i], fun = c(0))
    }

    return(data)
}


#' Check the data for zero-inflation or missing values
#'
#' Checks data for any NA or NaN values, and 'cleans' them by imputation based
#'  on mean gene expression, unless specified to not clean them. If missing
#'  data is found but not cleaned, function will stop execution and raise an
#'  error, because missing values will interfere with analyses. Also, if
#'  specified, can check data for possible zero-inflation by comparing
#'  the number of zero values in the data to a threshold (default=0.5), for which
#'  the data is considered zero-inflated if the number of zero values observed
#'  in the data exceeds the expected number as computed from the threshold.
#'  Zero-inflated genes and cells will then be removed unless specified not to.
#'
#' @inheritParams checkZeroInflation
#' @inheritParams cleanZeroInflation
#' @inheritParams checkMissingData
#' @inheritParams cleanMissingData
#' @param check_zero A logical indicating whether to check for zero-inflation.
#' Default is TRUE.
#' @param clean_zero A logical indicating whether to remove zero-inflated genes.
#' and cells. Default is TRUE.
#' @param clean_NA A logical indicating whether to impute missing values.
#' Default is TRUE. This can be changed to FALSE if the user wishes to handle
#' missing values on their own.
#'
#' @returns `NULL`.
#'
#' @examples
#' # load dasatinib dataset
#' data(dasatinib)
#' dframe <- dasatinib
#'
#' # change some of the data to zero
#' dframe_zero_inflated <- data.frame(dframe)
#' dframe_zero_inflated[,2:4] <- 0
#' checkEverything(dframe_zero_inflated)
#' head(dframe_zero_inflated)
#'
#' # change one of the values to NA
#' dframe_one_NA <- data.frame(dframe)
#' dframe_one_NA[1,2] <- NA
#' checkEverything(dframe_one_NA)
#' dframe_one_NA[1,2]
#'
#' #change one of the values to NaN
#' dframe_one_NaN <- data.frame(dframe)
#' dframe_one_NaN[1,2] <- 1/0
#' checkEverything(dframe_one_NaN)
#' dframe_one_NaN[1,2]
#'
#' @export
#'
checkEverything <- function(data,
                            check_zero = TRUE,
                            overall_zero_threshold = 0.5,
                            cell_zero_threshold = 0.5,
                            gene_zero_threshold = 0.5,
                            clean_zero = TRUE,
                            clean_NA = TRUE) {

    if (anyNA(data)) {
        message("Data contains missing values or NaN values.\n")

        if (clean_NA) {
            data <- cleanMissingData(data)
            message("Missing/NaN values imputed from mean gene expression.\n")
        }

        else {
            stop("Please impute or remove missing data before proceeding.\n")
        }
    }

    if (check_zero && checkZeroInflation(data, overall_zero_threshold)) {
        message("Data appears to be zero-inflated.\n")

        # user chooses to remove zero-inflated rows/columns
        if (clean_zero){
            data <- cleanZeroInflation(data,
                                       cell_zero_threshold,
                                       gene_zero_threshold)
            message("Zero-inflated genes/cells removed.\n")
        }

        # user chooses not to remove zero-inflated rows/columns
        # warn but run
        else {
            warning(
            "Consider running cleanZeroInflation(data) before proceeding.\n")
        }
    }

    return(NULL)
}

# [END]
