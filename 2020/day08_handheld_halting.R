library(fastmatch)
a <- sfp('2020','8', what=list('',0L))
vals <- a[[2]] # Values
acc <- a[[1]] == 'acc'
jmp <- a[[1]] == 'jmp'
visited2 <- rep(F, length(acc)) # Indicated if instruction has been visited
size <- length(acc)

loop <- \(vals, accl, jmp, p1=F) {
  visited <- visited2
  i <- 1L
  acc <- 0L
  end <- F
  repeat {
    if(i > size) {end <- T; break} # Stop after last instruction
    if(visited[i]) break # Stop if instruction has been visited (infinite loop)
    visited[i] <- T # Mark instruction as visited
    if (accl[i]) {acc <- acc + vals[i]; i <- i+1L} # Add to acc and go to next line
    else if (jmp[i]) i <- i + vals[i] # Jump instruction
    else i <- i + 1L # NOP = go to next line
  }
  if(p1) list(acc,which(visited)) else if(end) acc else NA
}
res <- loop(vals, acc, jmp, T)
path <- res[[2]] # Get list of visited values
res[[1]] # Part 1 (1782): Accumulator value before repeating an operation

findGoal <- \(jmp, vals, path) {
  # Only find jmp to nop changes
  exitsq <- collections::queue()
  target <- (max(which(jmp & vals <0))+1L):length(vals) # Final goal to exit program
  dest <- data.table::fifelse(jmp,vals,1L)+seq_along(vals) # Destinations of jumps
  exitsq$push(target) # Add list to exit possibilities
  nxt <- setdiff(which(dest %fin% target), target) # Next lines to check
  while(length(nxt)) {
    exitsq$push(nxt) # Add lines to exits
    nxt <- which(dest %fin% nxt) # Find next lines
  }
  exits <- unlist(exitsq$as_list()) # Full list of exits
  exits[exits %fin% (path+1L)]-1L # Find where a change from jmp to nop can bring path to exit
}
goal <- findGoal(jmp, vals, path)
  
jmp2 <- jmp
jmp2[goal] <- !jmp2[goal] # Change the jmp to nop

loop(vals, acc, jmp2) # Part 2 (797): Accumulator value when program ends
