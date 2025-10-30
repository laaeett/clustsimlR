# Functions to check data for various 'red flags'

#' Check for possible zero-inflation in the data
#'
#' Checks data for possible zero-inflation by comparing the number of zero
#'  values in the data to a threshold (default= 0.5), for which
#'  the data is considered zero-inflated if the number of zero values observed
#'  in the data exceeds the expected number as computed from the threshold.
#'
#' @param data A data frame where each column represents a gene and each row
#'  represents a single cell.
#' @param overall_zero_threshold A double representing the maximum proportion of
#'  the data that can be zero before being considered zero-inflated. Default is
#'  0.5, indicating that we consider the data zero-inflated if over half of it
#'  are zeroes.
#' @returns A boolean value representing whether the proportion of zeroes in the
#'  data is greater than the chosen threshold.
#'
#' @examples
#' \dontrun{
#' # load dasatinib dataset
#' data(dasatinib)
#' df <- dasatinib
#'
#' # check for zero-inflation with default threshold of 0.5
#' checkZeroInflation(df)
#'
#' # change some of the data to zero
#' df_zero_inflated <- data.frame(df)
#' df_zero_inflated[, 2:4] <- 0
#' checkZeroInflation(df_zero_inflated)
#' }
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
#' Cleans the data by removing zero-inflated genes (columns) and zero-inflated
#'  cells (rows). A gene or cell is considered zero-inflated if the number of
#'  zero values exceeds the number of zero values expected based on a threshold.
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
#' @param copy A boolean value indicating whether to work on a copy of the data.
#'  Default is FALSE.
#'
#' @returns A 'cleaned' version of data, where zero-inflated columns and rows are
#'  removed. If copy is TRUE, returns a copy of
#'  data but cleaned.
#'
#' @examples
#' \dontrun{
#' # load dasatinib dataset
#' data(dasatinib)
#' df <- dasatinib
#'
#' # change some of the data to zero
#' df_zero_inflated <- data.frame(df)
#' df_zero_inflated[,2:4] <- 0
#' checkZeroInflation(df_zero_inflated)
#'
#' df_cleaned <- cleanZeroInflation(df_zero_inflated, copy = TRUE)
#' checkZeroInflation(df_cleaned)
#' }
#'
cleanZeroInflation <- function(data,
                               cell_zero_threshold = 0.5,
                               gene_zero_threshold = 0.5,
                               copy = FALSE) {

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
    rm_genes <- c()
    for (i in 1:ncol(data_copy)) {
        curr_gene <- data_copy[, i]
        exp_zeroes <- length(curr_gene) * gene_zero_threshold
        num_zeroes <- sum(curr_gene == 0)

        # mark for removal if no. of zeroes exceeds threshold
        if(num_zeroes > exp_zeroes) {
            rm_genes <- c(rm_genes, colnames(data_copy)[i])
        }
    }
    data_copy <- data_copy[, !(colnames(data_copy) %in% rm_genes)]

    # if all genes were removed
    if(ncol(data_copy) == 0) {
        stop("All genes were removed by zero-inflation filtering\n")
    }

    # check row-wise = cell
    rm_cells <- c()
    for (i in 1:nrow(data_copy)) {
        curr_cell <- data_copy[i, ]
        exp_zeroes <- length(curr_cell) * cell_zero_threshold
        num_zeroes <- sum(curr_cell == 0)

        #mark for removal if no. of zeroes exceed threshold
        if(num_zeroes > exp_zeroes) {
            rm_cells <- c(rm_cells, i)
        }
    }
    data_copy <- data_copy[!(data_copy[, 1] %in% rm_cells), ]

    # inform what was removed
    message(sprintf("Removed %d zero-inflated genes: %s\n",
                    length(rm_genes),
                    paste(rm_genes, collapse = ", ")))
    message(sprintf("Removed %d zero-inflated cells: %s\n",
                    length(rm_cells),
                    paste(rm_cells, collapse = ", ")))

    # if copy, return a diff mem. address
    # else, should return mem. address of the original df
    return(data_copy)
}


#' Clean NA or NaN values
#'
#' Cleans the data by imputing NA and NaN values in data using the mean
#'  expression.
#'
#' @param data A data frame where each column represents a gene and each row
#'  represents a single cell.
#' @param copy A boolean indicating whether to work on a copy of the data.
#' @returns A 'cleaned' version of the data, with imputed NA and NaN values.
#'  If copy is TRUE, returns a copy of data but cleaned. Default is FALSE.
#' @examples
#' \dontrun{
#' # load dasatinib dataset
#' data(dasatinib)
#' df <- dasatinib
#'
#' # check for missing values
#' checkMissingData(df)
#'
#' # change one of the values to NA
#' df_one_NA <- data.frame(df)
#' df_one_NA[1,2] <- NA
#' df_NA_cleaned <- cleanMissingData(df_one_NA)
#' checkMissingData(df_NA_cleaned)
#'
#' #change one of the values to NaN
#' df_one_NaN <- data.frame(df)
#' df_one_NaN[1,2] <- 1/0
#' df_NaN_cleaned <- cleanMissingData(df_one_NaN, copy = TRUE)
#' checkMissingData(df_NaN_cleaned)
#' checkMissingData(df_one_NaN)
#' }
#' @importFrom Hmisc impute
#' @importFrom dplyr mutate_all
#' @importFrom magrittr %>%
#'
cleanMissingData <- function(data,
                             copy = FALSE) {
    data_copy <- data
    if(copy) {
        data_copy <- data.frame(data)
    }

    # changing NaN to NA for imputation
    data_copy <- data_copy %>% dplyr::mutate_all(~ifelse(is.nan(.), NA, .))

    # imputation based on column
    for(i in 1:ncol(data_copy)) {
        # imputation from mean
        data_copy[, i] <- Hmisc::impute(data_copy[, i], fun = mean)
    }

    return(data_copy)
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
#' @param check_zero A boolean indicating whether to check for zero-inflation.
#' Default is TRUE.
#' @param clean_zero A boolean indicating whether to remove zero-inflated genes.
#' and cells. Default is TRUE.
#' @param clean_NA A boolean indicating whether to impute missing values.
#' Default is TRUE. This can be changed to FALSE if the user wishes to handle
#' missing values on their own.
#'
#' @returns `NULL`.
#'
#' @examples
#' # load dasatinib dataset
#' data(dasatinib)
#' df <- dasatinib
#'
#' # change some of the data to zero
#' df_zero_inflated <- data.frame(df)
#' df_zero_inflated[,2:4] <- 0
#' checkEverything(df_zero_inflated)
#' head(df_zero_inflated)
#'
#' # change one of the values to NA
#' df_one_NA <- data.frame(df)
#' df_one_NA[1,2] <- NA
#' checkEverything(df_one_NA)
#' df_one_NA[1,2]
#'
#' #change one of the values to NaN
#' df_one_NaN <- data.frame(df)
#' df_one_NaN[1,2] <- 1/0
#' checkEverything(df_one_NaN)
#' df_one_NaN[1,2]
#'
#' @export
#'
checkEverything <- function(data,
                            check_zero = TRUE,
                            overall_zero_threshold = 0.5,
                            cell_zero_threshold = 0.5,
                            gene_zero_threshold = 0.5,
                            clean_zero = TRUE,
                            clean_NA = TRUE,
                            copy = FALSE) {

    if(anyNA(data)) {
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
