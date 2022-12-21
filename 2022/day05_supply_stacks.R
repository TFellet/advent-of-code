a <- rfp('2022','5') # Read input

move_first <- which.max(substr(a,1,4)=='move') # Find first move
letters_id <- which(strsplit(a[move_first-2], '', fixed=T)[[1]] != ' ') # Where to find letters if present
stacks <- lapply(letters_id, \(x) substr(a[1:(move_first-3)], x, x)) # Fetch letters from each stack
moves <- stringi::stri_match_first_regex(a[move_first:length(a)], 'move (\\d+) from (\\d+) to (\\d+)')[,-1] |> # Extract numbers from moves list
  `storage.mode<-`('integer') |> collapse::mrtl() # Convert numbers to integer and list them by row
st <- lapply(stacks, \(x) collections::stack(items = rev(x[x!=' '])) ) # Create stacks for part 1
st2 <- lapply(stacks, \(x){ rev(x[x != ' ']) }) # Stacks as list for part 2
sizes <- lengths(st2)
for (move in moves) { # Go through each move
  for (i in seq_len(move[1])) { # For each crate to move
    st[[move[3]]]$push(st[[move[2]]]$pop()) # Put the first crate of stack "from" on top of stack "to"
  }
  
  # For part 2, move all elements at the same time
  idx_from <- (sizes[move[2]]-move[1]+1):sizes[move[2]] # Indices of starting crates
  idx_to <- seq.int(sizes[move[3]]+1, length.out = move[1]) # Indices of destination
  st2[[move[3]]][idx_to] <- st2[[move[2]]][idx_from] # Put crates at destination
  st2[[move[2]]][idx_from] <- '' # Remove crates from starting position
  sizes[move[2]] <- sizes[move[2]] - move[1] # Update "from" size
  sizes[move[3]] <- sizes[move[3]] + move[1] # Update "to" size
  
}
paste0(sapply(st, \(x) x$peek()), collapse = '') # Part 1 (JCMHLVGMG): Top crate of each stack
paste0(sapply(st2, \(x) flast(x[x!=''])), collapse = '') # Part 2 (LVMRWSSPZ): Top crate of each stack
