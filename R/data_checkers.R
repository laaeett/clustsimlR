# Functions to check data for various 'red flags'

#' Check for possible zero-inflation in the data
#'
#' @param data A data frame where each column represents a gene and each row
#' represents a single cell.
#' @param overall_zero_threshold A double representing the maximum proportion of
#' the data that can be zero before being considered zero-inflated. Default is
#' 0.5, indicating that we consider the data zero-inflated if over half of it
#' are zeroes.
#' @returns A boolean value representing whether the proportion of zeroes in the
#' data is greater than the chosen threshold.
#' @examples
#' # load dasatinib dataset
#' df <- dasatinib()
#' # first and last few rows of the dataset
#' head(df)
#' tail(df)
#' # check for zero-inflation with default threshold of 0.5
#' checkZeroInflation(df)
#'
#' # change 60% of the data to zero
#' df_zero_inflated <- data.frame(df)
#' df_zero_inflated[1:((nrow(df) * ncol(df) / 3) * 2)] <- 0
#' checkZeroInflation(df_zero_inflated)
#'
checkZeroInflation <- function(data,
                               overall_zero_threshold = 0.5) {

    if(overall_zero_threshold > 1 || overall_zero_threshold < 0) {
        stop("overall_zero_threshold must be between 0 and 1.")
    }

    num_vals <- nrow(data) * ncol(data)

    # expected number of zeroes in the data
    exp_zeroes <- num_vals * overall_zero_threshold

    # observed number of zeroes
    num_zeroes <- sum(data == 0)

    return(num_zeroes > exp_zeroes)
}


#' Clean zero-inflated genes and cells from the data
#'
#' @param data A data frame where each column represents a gene and each row
#' represents a single cell.
#' @param cell_zero_threshold A double representing the maximum proportion of
#' the data for a single cell (row) that can be zero before being considered
#' zero-inflated. Default is 0.5, indicating that we consider the data for that
#' cell zero-inflated if over half of it are zeroes.
#' @param gene_zero_threshold A double representing the maximum proportion of
#' the data for a single gene (column) that can be zero before being considered
#' zero-inflated. Default is 0.5, indicating that we consider the data for that
#' gene zero-inflated if over half of it are zeroes.
#' @param copy A boolean value indicating whether to work on a copy of the data.
#'
#' @returns A 'cleaned' version of data. If copy is TRUE, returns a copy of
#' data but cleaned.
#'
#' @examples
#' # load dasatinib dataset
#' df <- dasatinib()
#' # first and last few rows of the dataset
#' head(df)
#' tail(df)
#'
#' # change 60% of the data to zero
#' df_zero_inflated <- data.frame(df)
#' df_zero_inflated[1:((nrow(df) * ncol(df) / 3) * 2)] <- 0
#' checkZeroInflation(df_zero_inflated)
#'
#' df_cleaned <- cleanZeroInflation(df_zero_inflated, copy = TRUE)
#' checkZeroInflation(df_cleaned)
#' print(df_cleaned == df_zero_inflated)
#'
cleanZeroInflation <- function(data,
                               cell_zero_threshold,
                               gene_zero_threshold,
                               copy) {

    data_copy <- data

    # if TRUE, work on a copy of the data frame
    if(copy) {
        data_copy <- data.frame(data)
    }

    if(cell_zero_threshold > 1 || cell_zero_threshold < 0) {
        stop("cell_zero_threshold must be between 0 and 1.")
    }

    if(gene_zero_threshold > 1 || gene_zero_threshold < 0) {
        stop("gene_zero_threshold must be between 0 and 1.")
    }
    # check column-wise = gene
    for (i in 1:ncol(data_copy)) {
        curr_gene <- data_copy[, i]
        exp_zeroes <- length(curr_gene) * gene_zero_threshold
        num_zeroes <- sum(curr_gene == 0)

        # remove if no. of zeroes exceeds threshold
        if(num_zeroes > exp_zeroes) {
            data_copy <- data_copy[, -i]

        }
    }

    # check row-wise = cell
    for (i in 1:nrow(data_copy)) {
        curr_cell <- data_copy[i, ]
        exp_zeroes <- length(curr_cell) * cell_zero_threshold
        num_zeroes <- sum(curr_cell == 0)

        #remove if no. of zeroes exceed threshold
        if(num_zeroes > exp_zeroes) {
            data_copy <- data_copy[-i, ]
        }
    }

    # if copy, return a diff mem. address.
    # else, should return mem. address of the original df
    return(data_copy)
}


#' Check for NA or NaN values
#'
#' @param data A data frame where each column represents a gene and each row
#' represents a single cell.
#' @returns A boolean value representing whether they are any NA or NaN values
#' in data
#' @examples
#' # load dasatinib dataset
#' df <- dasatinib()
#' # first and last few rows of the dataset
#' head(df)
#' tail(df)
#' # check for missing values
#' checkMissingData(df)
#'
#' # change one of the values to NA
#' df_one_NA <- data.frame(df)
#' df_one_NA[1,1] <- NA
#' checkMissingData(df_one_NA)
#'
#' #c change one of the values to NaN
#' df_one_NaN <- data.frame(df)
#' df_one_NaN[1,1] <- 1/0
#' checkMissingData(df_one_NaN)
#'
checkMissingData <- function(data) {
    return(any(is.na(data) || is.nan(data)))
}


cleanMissingData <- function(data,
                             copy = FALSE) {
    data_copy <- data
    if(copy) {
        data_copy <- data.frame(data)
    }

    # imputation based on column
    for(i in 1:ncol(data_copy)) {
        curr_gene = data_copy[, i]

        # imputation from mean
        Hmisc::impute(curr_gene, fun = mean)
    }

    return(data_copy)
}


# function wrapper for the functions above
checkEverything <- function(data,
                            check_zero = TRUE,
                            overall_zero_threshold = 0.5,
                            cell_zero_threshold = 0.5,
                            gene_zero_threshold = 0.5,
                            clean_zero = TRUE,
                            clean_NA = TRUE,
                            copy = FALSE) {

    if(checkMissingData(data)) {
        message("Data contains missing values or NaN values.\n")

        if(clean_NA) {
            data <- cleanMissingData(data, copy)
            message("Missing/NaN values imputed from mean gene expression.\n")
        }

        else {
            stop("Please impute or remove missing data before proceeding.\n")
        }
    }

    if(check_zero && checkZeroInflation(data, overall_zero_threshold)) {
        message("Data appears to be zero-inflated.\n")

        # user chooses to remove zero-inflated rows/columns
        if(clean_zero){
            data <- cleanZeroInflation(data,
                                       cell_zero_threshold,
                                       gene_zero_threshold,
                                       copy)
            message("Zero-inflated genes/cells removed.\n")
        }

        # user chooses not to remove zero-inflated rows/columns
        # warn but run
        else {
            message(
            "Consider running cleanZeroInflation(data) before proceeding.\n")
        }
    }
}
