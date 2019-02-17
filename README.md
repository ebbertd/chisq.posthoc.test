[![Build Status](https://travis-ci.com/ebbertd/chisq.posthoc.test.svg?branch=master)](https://travis-ci.com/ebbertd/chisq.posthoc.test)

# Overview

chisq.posthoc.test is a R package that is designed to run a post hoc analysis for Pearson's Chi-squared Test for Count Data.

## Installation

The easiest way to get chisq.posthoc.test is to install it using the devtools:

```R
install.packages("devtools")
devtools::install_github("ebbertd/chisq.posthoc.test")
```

## Usage

```R
chisq.posthoc.test(x, y, method = "bonferroni", alpha = 0.05, ...)
```

x and y correspond to x and y of the chisq.test function and are passed on to it. The method indicates the p adjustment method and defaults to the Bonferroni method. Alpha indicates the significane level which is used to mark the significant fields in the output with a *. Additional options can be given which are passed on to the chisq.test function.
