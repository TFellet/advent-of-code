library(fastmatch)
a <- as.numeric(rfp('2020','9'))

# For each number, look at k precedents numbers and check if any 2 number can add to this number
findInvalid <- function(a, k = 25) {
  for (i in (k+1):length(a)) { # Starting at number k+1
    n <- a[i] # Current number = goal
    w <- a[(i-k):(i+1)] # Previous 25 numbers
    res <- w %fin% (n - w) & n/2!=w # Is one of the numbers goal-(previous 25 numbers)
    if (!any(res)) return(n) # If no number can add up to goal, stop and return goal
  }
}

goal <- findInvalid(a) # Part 1 (217430975): First number 'goal' where 2 numbers in previous 25 cannot add up to 'goal'
goal

findGoal <- \(a, goal) {
  fw <- cumsum(a) # Forward sum
  reve <- cumsum(rev(a)) # Reverse sum
  m <- fastmatch::fmatch(fw, (fw[length(fw)] - reve - goal), nomatch = 0L) # Find where forward sum can equal total sum - reverse sum - goal
  start <- which.max(m) # Sequence start
  end <- length(a) - m[start] # Sequence end
  sum(range(a[(start+1L):end])) # Sum of min and max of sequence
}
findGoal(a,goal) # Part 2 (28509180): Sum of min and max of sequence that add up to goal
