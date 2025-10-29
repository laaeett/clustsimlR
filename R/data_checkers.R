# Functions to check data for various 'red flags'

# check for possible zero-inflation data
checkZeroInflation <- function(data,
                               overall_zero_threshold = 0.5) {

    num_vals <- nrow(data) * ncol(data)

    # expected number of zeroes in the data
    exp_zeroes <- num_vals * overall_zero_threshold

    # observed number of zeroes
    num_zeroes <- sum(data == 0)

    return(num_zeroes > exp_zeroes)
}

cleanZeroInflation <- function(data,
                               cell_zero_threshold,
                               gene_zero_threshold,
                               copy) {

    data_copy <- data

    # if TRUE, work on a copy of the data frame
    if(copy) {
        data_copy <- data.frame(data)
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

# check for missing data
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
