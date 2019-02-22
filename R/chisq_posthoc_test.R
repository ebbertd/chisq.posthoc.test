#' Perform post hoc analysis based on residuals of Pearson's Chi-squared Test for Count Data.
#'
#' @param x A numeric vector passed on to the chisq.test function.
#' @param y A numeric vector passed on to the chisq.test function.
#' @param method The p adjustment method to be used. This is passed on to the p.adjust function.
#' @param round Number of digits to round the p.value to. Defaults to 6.
#' @param ... Additional arguments passed on to the chisq.test function.
#'
#' @return A table with the adjusted p value for each x y combination.
#' @export

chisq.posthoc.test <-
  function(x, y, method = "bonferroni", round = 6, ...) {
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
    adjusted_p_values <- round(adjusted_p_values, digits = round)
    # Print the results
    print(adjusted_p_values)
  }
