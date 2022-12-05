library(expm) # Matrix exponents

file <- rfp('2021','14')
temp <- strsplit(file[1],'',fixed=T)[[1]]
rulesl <- data.table::tstrsplit(file[-(1:2)], ' ', keep = c(1,3), fixed=T)
rules <- list(inp = rep(rulesl[[1]],2))
rules[['out']] <- c(stringi::stri_c(substr(rulesl[[1]],1,1),rulesl[[2]]), stringi::stri_c(rulesl[[2]], substr(rulesl[[1]],2,2)))

un <- radsort(kit::funique(c(rules$inp, rules$out))) # All unique pairs present
poly <- Rfast::Table(stringi::stri_c(temp[-length(temp)],temp[-1])) # Starting pairs
start <- `names<-`(rep(0,length(un)), un) # Empty vector with all combinasons of letters
start[names(poly)] <- poly # Assign numbers from input

### Matrix version 223 µs on 40 steps | 397 µs on 1000 steps
m <- array(0,rep(length(un),2), list(un,un)) # Matrix to keep track of associations
m[(fastmatch::fmatch(rules$inp, un)-1)*nrow(m) + fastmatch::fmatch(rules$out, un)] <- 1 # Add the rules to the matrix

countEnd <- \(end) {
  let <- c(substr(names(end),1L,1L),substr(names(end),2L,2L)) # All letters from pairs
  counts <- ceiling(collapse::fsum(rep(end,2L), g=let)/2L) # Sum number of letters present
  max(counts) - min(counts) # Most frequent - least frequent
}

# Part 1
end <- (m %^% 10 %*% start) # Find pairs presents after 10 steps
countEnd(end[,1])

# Part 2
end2 <- (m %^% 30 %*% end %*% start)[,1] # Find pairs presents after 40 steps
countEnd(end2)
