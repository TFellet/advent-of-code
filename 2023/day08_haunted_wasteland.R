a <- rfp("2023", "8")
b <- gsub("[^A-Z0-9]+", " ", a[3:length(a)])
b2 <- scan(text = b, what = as.list(rep("", 3)))
fact <- factor(b2[[1]], levels = b2[[1]])
ids <- as.integer(fact)
graph <- lapply(b2, \(x) as.integer(factor(x, levels = levels(fact))))

start <- which(b2[[1]] == "AAA")
end <- which(b2[[1]] == "ZZZ")

inst <- setNames(c(2, 3), c("L", "R"))[strsplit(a[1], "")[[1]]] |> unname()
countSteps <- \(line, end, inst) {
  i <- 1L
  steps <- 0L
  repeat {
    line <- graph[[inst[i]]][line]
    i <- if (i == length(inst)) 1L else i + 1L
    steps <- steps + 1L
    if (line %in% end) {
      return(steps)
    }
  }
}

countSteps(start, end, inst) # Part 1 (13939): Number of steps from start to end

starts <- which(substr(b2[[1]], 3, 3) == "A")
ends <- sort(which(substr(b2[[1]], 3, 3) == "Z"))

results <- sapply(starts, \(start) countSteps(start, ends, inst))

# Find the GCM of the results
gcd <- \(a, b) {
  while (b != 0) {
    t <- b
    b <- a %% b
    a <- t
  }
  a
} # Algorithm for gcd
lcm <- \(a, b) (a * b) / gcd(a, b) # Algorithm for lcm

Reduce(lcm, results, init = 1L) # Part 2 (8906539031197): Number of steps when all the ends are reached at the same time
