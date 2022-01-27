a <- scan(fp(2021,7),0L,sep=',',quiet=T)

sum(abs(a-median(a))) # 39.1 µs # Part 1: best meeting point is median
d <- \(n) n*(n+1)/2 # Distance function for part 2
sum(d(abs(a-floor(mean(a))))) # 14.9 µs # Min fuel used
# Check solution
# optimize(\(x) sum(d(abs(a-round(x)))), range(a),tol=1)$objective
