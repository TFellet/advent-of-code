a <- rfp('2021','24')
n2 <- stringi::stri_sub(a, 7L) # Number 2

w <- 1L:9L # Possible inputs
check <- n2[seq.int(6,by=18,length.out=14)] |> as.integer() # First useful line
offset <- n2[seq.int(16,by=18,length.out=14)] |> as.integer() # Second useful line
st <- collections::stack() 
digits <- vector('list',14)
for (i in 1:14) { # For each of the 14 inputs
  if (check[i]>0) { # If check is positive, add number to stack
    st$push(list(i, offset[i]))
  } else { # If check negative
    pop <- st$pop() # Get previous number from stack
    ind <- pop[[1]] # Digit index
    offind <- pop[[2]] # Previous offset
    inp_prev <- w+offind+check[i] # Previous possible inputs
    equal <- inp_prev %in% w # previous input == current input
    digits[[ind]] <- range(which(equal))
    digits[[i]] <- range(inp_prev[equal])
  }
}
stringi::stri_join_list(data.table::transpose(digits)) # Part 1 & 2: Min and max possible numbers 
