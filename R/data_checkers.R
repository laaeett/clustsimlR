# Functions to check data for various 'red flags'

checkZeroInflation <- function(data,
                               inflation_threshold = 0.5) {

    num_vals <- nrow(data) * ncol(data)

    # expected number of zeroes in the data
    exp_zeroes <- num_vals * inflation_threshold

    # observed number of zeroes
    num_zeroes <- sum(data == 0)

    return(num_zeroes > exp_zeroes)
}
