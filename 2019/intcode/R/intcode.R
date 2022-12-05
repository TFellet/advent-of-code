intcode <- \(code_raw, overflow = 0) {
  self <- environment()
  code <- strsplit(code_raw, ',', fixed=T)[[1]] |> as.numeric() # Parse raw code
  if(overflow) code[(length(code)+1):(length(code)*(1+overflow))] <- 0
  code0 <- code # Keep a copy of initial code for the reset
  pos <- 0
  end <- FALSE
  base <- 0
  # len <- length(code)
  lens <- c(4,4,2,2,3,3,4,4,2)
  out <- collections::queue()

  run <- \(input = c()) {
    # browser()
    n <- 0 # Index of input requested
    # Function to return the input when its requested. Exits the computer when no inputs are available
    out$clear() # Init output vector
    while(opc <- code[pos+1] != 99) {                        # 99: Exit
      # browser()
      opc <- code[pos+1] # Current operation code
      pm <- opc %/% c(100, 1000, 10000) %% 10 # Init parameters modes
      opc <- opc %% 100 # operation code is the 2 rightmost digits
      lenop <- lens[opc] # length of operations
      p2 <- p3 <- 0L
      # In mode 0, get value at adress of parameter
      # In mode 1, get parameter as value
      # In mode 2, get value at (current position + value of parameter + relative base)
      p1 <- switch(pm[1]+1, code[pos+2]+1, pos+2, code[pos+2]+1+base)# |> adjust()
      if(lenop > 2) p2 <- switch(pm[2]+1, code[pos+3]+1, pos+3, code[pos+3]+1+base)# |> adjust()
      if(lenop > 3) p3 <- switch(pm[3]+1, code[pos+4]+1, pos+4, code[pos+4]+1+base)# |> adjust()
      # switch (len, 0, adjust(p1), adjust(max(c(p1,p2))), adjust(max(c(p1,p2,p3))))

      # if((ma <- max(c(p1,p2,p3)))>len) adjust(ma)
      # adjust(ma)
      # pri('operation :', opc)
      # pri('parameters: ', p1, '(', p1m, '), ', p2, '(', p2m, '), ', p3, '(', p3m, ')')
      # pri('parameters dest: ', code[p1], '(', p1m, '), ', code[p2], '(', p2m, '), ', code[p3], '(', p3m, ')')
      switch (opc,
              code[p3] <<- code[p1] + code[p2],              # 1: Add
              code[p3] <<- code[p1] * code[p2],              # 2: Multiply
              code[p1] <<- {n<-n+1;                          # 3: Read input
              if(n>length(input)) return(unlist(out$as_list())); input[n]},
              out$push(code[p1]),                       # 4: Output value
              # if(n>length(input)) return(out); input[n]},
              # out <- c(out, code[p1]),                       # 4: Output value
              if(code[p1]!=0) {pos <<- code[p2]; next},      # 5: Jump if true
              if(code[p1]==0) {pos <<- code[p2]; next},      # 6: Jump if false
              code[p3] <<- as.integer(code[p1] < code[p2]),  # 7: ess than
              code[p3] <<- as.integer(code[p1] == code[p2]), # 8: Equal
              base <<- base + code[p1]                       # 9: Adjust base
      )
      pos <<- pos+lenop
    }
    end <<- TRUE
    # return(out)
    return(unlist(out$as_list()))
  }

  # Increase memory size as necessary
  # adjust <- \(x) {
  #   if(x > len) {
  #     # cat('mem increase\nlen: ', len, '\n')
  #     code[(len+1):(x+.2*len)] <<- 0
  #     len <<- length(code)
  #   }
  #   # x
  # }

  # Reset the computer to its initial state
  reset <- \() {
    code <<- code0
    pos <<- 0
    end <<- FALSE
    base <<- 0
    out$clear()
    self
  }

  self
}
