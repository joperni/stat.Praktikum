# function , that calculates the seven_day_inz of a numeric vector
# input: x: numeric vector
#        inhabitants: inhabitants in the current territory (default Germany)
# output: numeric vector with length == length(x)
library(checkmate)
seven_day_inz <- function(x, inhabitants = 82089780) {
  assert_numeric(x)
  # make sure we dont iterate outside the limits of x
  x_looping <- c(rep(NA, 6), x)
  # starting at 1 + 6, avoiding hitting these limits
  vapply(seq_along(x) + 6 , function(current_day) {
    seven_day_seq <- seq(current_day - 6, current_day)
    sum(x_looping[seven_day_seq], na.rm = TRUE) * 100000/inhabitants
  }, numeric(1))
}