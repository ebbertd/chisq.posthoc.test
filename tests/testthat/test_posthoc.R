context("Test that calculations are performed correctly")

M <- as.table(rbind(c(762, 327, 468), c(484, 239, 477)))
dimnames(M) <- list(
  gender = c("F", "M"),
  party = c("Democrat", "Independent", "Republican")
)
post_hoc_results <- chisq.posthoc.test(M, alpha = 0.05)

test_that("Post hoc test is performed correctly", {
  expect_equal(post_hoc_results$Independent[2],
               1)
})
