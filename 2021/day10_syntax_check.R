library(fastmatch)
a <- rfp('2021','10')
am <- strsplit(a, '',fixed=T) # List of single character vectors

parseLines <- \(am) {
  o <- c('(', '[', '{', '<') # Openings brackets
  f <- c(')', ']', '}', '>') # Closings brackets
  cid <- setNames(rep(seq_along(o),2), c(o, f)) # Id corresponding to both
  terr <- setNames(c(3, 57, 1197, 25137), f) # Scoring for part 1
  
  comps <- list() # List of symbols to complete
  errs <- c() # List of errors
  for (line in 1:length(am)) { # For each line
    seen <- c() # Keep track on seen brackets
    i <- 1 # Set pointer on seen to 1
    l <- am[[line]] # Get line
    # lop <- l %in% o # Find opening brackets
    lop <- l %fin% o # Find opening brackets
    lid <- cid[l] # Id of each bracket (same id for opening and closing bracket)
    
    bug <- F # No bug encountered
    for (char in seq_along(l)) { # For each char in line
      if(lop[[char]]) { # If the char is opening
        seen[i] <- lid[[char]] # Add id to seen brackets
        i <- i+1 # Advance pointer by 1
      } else { # Closing char
        i <- i-1 # Go to previous index
        if(lid[[char]] != seen[i]) { # If the current closing char doesn't close the previous opening char
          errs <- c(errs, terr[l[[char]]]) # Add error score to list
          bug <- T # Bug encountered
          break # End line parsing
        } 
        else { seen[i] <- 0 } # If the current char closes the previous bracket, remove previous bracket
      }
    }
    if(!bug) comps[[line]] <- rev(seen[seen != 0]) # If no bug found, add the list of symbols seen without closing in reverse order
  }
  list(comps=comps, errs=errs)
}
res <- parseLines(am)
# Part 1
sum(res$errs) # Sum of errors found

# Part 2
red <- \(x,y) x*5+y # Reduce function
vapply(res$comps[lengths(res$comps)>0L], Reduce, f=red, 0) |> median() # Compute all metrics with reduce and take median
