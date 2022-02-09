a <- strtoi(rfp('2020','10'))
a2 <- a[collapse::radixorder(a)] # Sort numbers

diffs <- c(a2, a2[length(a2)]+3L)-c(0L, a2) # Differences with previous number
prod(Rfast::Table(diffs)) # Part 1

# Fast run length
frl <- \(x) {
  n <- length(x)
  wd <- which(x[-1L] != x[-n]) # Detect ids where data change
  c(wd, n) - c(0L, wd) # Substract ids to find lenth of each sequence
}

groups <- frl(diffs) # Compute length of consecutive 1s and 3s
groups <- Rfast::Table(groups[seq.int(1L, length(groups), 2L)]) # Frequencies of sequences of 1s

res <- 2^groups['2'] * 4^groups['3'] * 7^groups['4'] # Number of paths for each type of group
res # Part 2
