#' Perform post hoc analysis based on residuals of Pearson's Chi-squared Test for Count Data.
#'
#' @param x A numeric vector passed on to the chisq.test function.
#' @param y A numeric vector passed on to the chisq.test function.
#' @param method The p adjustment method to be used. This is passed on to the p.adjust function.
#' @param alpha The significance level for indicating which results are significant.
#' @param ... Additional arguments passed on to the chisq.test function.
#'
#' @return A table with the adjusted p value for each x y combination.

chisq.posthoc.test <-
  function(x, y, method = "bonferroni", alpha = 0.05, ...) {
    # Perform the chi square test and save the residuals
    stdres <- chisq.test(x, y, ...)$stdres
    # Calculate the chi square values based on the residuls
    chi_square_values <- stdres ^ 2
    # Get the p values for the chi square values
    p_values <- pchisq(chi_square_values, 1, lower.tail = FALSE)
    # Adjust the p values with the chosen method
    adjusted_p_values <- p_values
    for (i in 1:nrow(adjusted_p_values)) {
      adjusted_p_values[i, ] <- p.adjust(
        adjusted_p_values[i, ],
        method = method,
        n = ncol(adjusted_p_values) * nrow(adjusted_p_values)
      )
    }
    # Indicate significant p values with a *
    for (i in 1:nrow(adjusted_p_values)) {
      for (j in 1:ncol(adjusted_p_values)) {
        if (adjusted_p_values[i, j] < alpha) {
          adjusted_p_values[i, j] <-
            paste(adjusted_p_values[i, j], "*", sep = "")
        }
      }
    }
    # Print the results
    print(adjusted_p_values)
  }
