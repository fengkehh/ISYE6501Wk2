# cusum_dec() detects a DECREASING change in x.
# Arguments:
# x: numerical vector of the quantity to undergo detection
# C: critical value (numeric)
# T: threshold value (numeric)
# 
# Returns:
# index of when the first change is detected.
# 0 otherwise.
cusum_dec <- function(x, C, T) {
  S_t <- 0
  mu = 0
  
  for (i in 1:length(x)) {
    mu = mean(x[1:i])
    S_t <- max(0, S_t + mu - x[i] - C)
    if (S_t >= T) {
      return(i)
    }
  }
  return(0)
}