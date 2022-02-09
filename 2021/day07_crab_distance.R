a <- strtoi(strsplit((rfp('2021','7')), ',', fixed=T)[[1]])

sum(abs(a-collapse::fmedian(a))) # Part 1: best meeting point is median
d <- \(n) n*(n+1)/2 # Distance function for part 2
sum(d(abs(a-floor(mean(a))))) # Min fuel used
# Check solution
# optimize(\(x) sum(d(abs(a-round(x)))), range(a),tol=1)$objective
