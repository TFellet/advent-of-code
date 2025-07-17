a <- rfp('2023','6')

times <- strsplit(a[1], ' +') |> unlist() |> _[-1] |> strtoi()
dists <- strsplit(a[2], ' +') |> unlist() |> _[-1] |> strtoi()

ways <- mapply(\(t, d) {
  n <- seq_len(t-1)
  sum((n*(t-n)) > d)
}, times, dists)
prod(ways) # 293046

times2 <- as.numeric(paste0(times, collapse = ''))
dists2 <- as.numeric(paste0(dists, collapse = ''))

n <- as.numeric(seq_len(times2-1)) 
# a bit slow but one liner
# Possible to optimise with binary search
sum((n*(times2-n)) > dists2) # 35150181
