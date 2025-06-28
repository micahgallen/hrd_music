#' Bin Continuous Confidence Ratings into Quantile-Based Categories
#'
#' This function divides a vector of continuous confidence ratings into a specified number
#' of bins based on quantile thresholds. It computes quantile boundaries from the data and
#' assigns each rating to its corresponding bin. If the distribution is highly skewed—so that
#' the lowest or highest quantile boundaries collapse (i.e., are equal)—the function attempts
#' to recompute the bins by excluding the problematic extreme values. If both the lowest and
#' highest boundaries collapse, indicating an inability to form distinct bins, the function
#' returns a vector of NA values to signal a failure in binning.
#'
#' @param slidingCon A numeric vector of continuous confidence ratings.
#' @param Nbins An integer specifying the number of bins to create (e.g., 4 for quartiles).
#'
#' @return A numeric vector of the same length as \code{slidingCon} with bin assignments (1 to Nbins)
#'   for each element. If binning fails due to extreme clustering at both ends, the returned vector
#'   will consist entirely of NA values.
#'
#' @examples
#' # Typical usage:
#' ratings <- c(0, 25, 50, 75, 100)
#' bin_confidence_quantiles(ratings, 4)
#'
#' # Case with extreme clustering:
#' ratings_fail <- rep(100, 20)
#' bin_confidence_quantiles(ratings_fail, 4)  # Returns a vector of NAs
#'
#' @export
#' Micah G. Allen, 2025
bin_confidence_quantiles <- function(slidingCon, Nbins) {
  responseConf <- rep(NA, length(slidingCon))
  confBins <- quantile(slidingCon, probs = seq(0, 1, length.out = Nbins + 1), na.rm = TRUE)
  temp <- vector("list", Nbins)
  
  # Check for extreme clustering at both ends: 
  # if both the lowest and highest quantile boundaries collapse, binning fails.
  if (confBins[1] == confBins[2] && confBins[Nbins] == confBins[Nbins + 1]) {
    return(responseConf)  # Return vector of NAs to signal failure in binning.
  } else if (confBins[Nbins] == confBins[Nbins + 1]) {
    # High-end collapse: re-estimate quantiles excluding the highest ratings.
    hiConf <- confBins[Nbins]
    temp[[Nbins]] <- (slidingCon == hiConf)
    confBins <- quantile(slidingCon[!temp[[Nbins]]], probs = seq(0, 1, length.out = Nbins), na.rm = TRUE)
    for (b in seq_len(length(confBins) - 1)) {
      temp[[b]] <- slidingCon >= confBins[b] & slidingCon <= confBins[b + 1]
    }
  } else if (confBins[1] == confBins[2]) {
    # Low-end collapse: re-estimate quantiles excluding the lowest ratings.
    lowConf <- confBins[2]
    temp[[1]] <- (slidingCon == lowConf)
    confBins <- quantile(slidingCon[!temp[[1]]], probs = seq(0, 1, length.out = Nbins), na.rm = TRUE)
    for (b in seq(2, length(confBins))) {
      temp[[b]] <- slidingCon >= confBins[b - 1] & slidingCon <= confBins[b]
    }
  } else {
    # Normal case: assign ratings to bins based on quantile boundaries.
    for (b in seq_len(length(confBins) - 1)) {
      temp[[b]] <- slidingCon >= confBins[b] & slidingCon <= confBins[b + 1]
    }
  }
  
  for (b in seq_len(Nbins)) {
    responseConf[temp[[b]]] <- b
  }
  
  return(responseConf)
}
