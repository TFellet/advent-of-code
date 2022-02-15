a <- strtoi(rfp('2020','10'))
a2 <- radsort(a) # Sort numbers

diffs <- c(a2, a2[length(a2)]+3L)-c(0L, a2) # Differences with previous number
prod(kit::countOccur(diffs)$Count) # Part 1 (2738): Product of frequencies of 3's and 1's

# Fast run length
frl <- \(x) {
  n <- length(x)
  wd <- which(x[-1L] != x[-n]) # Detect ids where data change
  c(wd, n) - c(0L, wd) # Substract ids to find lenth of each sequence
}

groups <- frl(diffs) # Compute length of consecutive 1s and 3s
g <- groups[seq.int(1L, length(groups), 2L)] # Keep only lengths of 1's
# 2 1's in a row multiply possibilities by 2, 3 1's by 4 and 4 1's by 7
res <- 2^kit::count(g, 2L) * 4^kit::count(g, 3L) * 7^kit::count(g, 4L) # Multiply all number of possibilities 
res # Part 2 (74049191673856): All possibles ways to connect the adapters
