# DEMO: Imputating NAs in factor variables
library(dummy)
(dat <- data.frame(var1 = factor(c("a", "b", NA), exclude = NULL),
                  var2 = factor(c(1, NA, 3), exclude = NULL)))
dummy(dat)
(binarized_dat <- dummy(dat)[, -c(3, 6)]) # preserves (2, 1) and (3, 2)
