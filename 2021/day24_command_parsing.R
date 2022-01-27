library(collections)
library(purrr)

a <- rfp(2021,24)
n2 <- substr(a,7,nchar(a)) # Number 2

comnums <- ((seq_along(n2)-1L)%%18L) # Operation ids
w <- 1L:9L # Possible inputs
check <- n2[comnums == 5] |> as.integer() # First useful line
offset <- n2[comnums == 15] |> as.integer() # Second useful line
st <- stack() 
digits <- list()
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
map_chr(digits, 2) |> paste0(collapse = '')
map_chr(digits, 1) |> paste0(collapse = '')
