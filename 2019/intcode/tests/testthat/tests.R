oldwd <- getwd()
on.exit(setwd(oldwd))
setwd(here::here('..', '..'))
source('utils/utils.R')

test_that("day 5", {
  a <- rfp('2019','5') # Read input
  comp <- intcode(a) # Init computer
  comp$run(1L) |> flast() |> expect_equal(16348437)
  comp$reset()$run(5L) |> expect_equal(6959377)
})

test_that("day 7", {
  a <- rfp('2019', '7') # Read code
  comps <- replicate(5, intcode(a), F) # Init 5 intcode computers
  prev_ind <- c(5L, 1L:4L) # 1 is connected to 5, the others to the previous number

  tryPerm <- \(values, r = 0L) {
    # Run amplifier 5 times with sequence and previous result as input
    for (i in 1L:5L) r <- comps[[i]]$reset()$run(c(values[i], r));r
  }

  findMax <- \(range, func)
  Rfast::permutation(range) |> # Generate all permutations
    collapse::mrtl() |> # Convert them to list
    sapply(func) |> # 5th amplifier value
    max() # Return max value found

  findMax(0L:4L, tryPerm) |> expect_equal(101490) # Part 1 (101490): Max value after 5 amplify functions

  tryPermLoop <- \(values) {
    results <- list(NULL, NULL, NULL, NULL, 0L) # First signal is 0 (amplifier 1 is connected to 5)
    lapply(comps, \(x) x$reset()) # Reset all computers in each try
    first <- T # First loop
    while(!comps[[5]]$end) { # Exit when the last program has completed
      for (i in 1L:5L) { # For each sequence of 5 amplifiers
        prev <- prev_ind[i]
        # On first loop, feed sequence along with input
        if(first) results[[prev]] <- c(values[i], results[[prev]])
        # Run computer with previous state and output of previous amplifier
        results[[i]] <- comps[[i]]$run(results[[prev]])
      }
      first <- F
    }
    results[[5]]
  }

  findMax(5L:9L, tryPermLoop) |> expect_equal(61019896) # Part 2 (61019896): Max value after 5 looping amplify functions
})

test_that("day 9", {
  a <- rfp('2019','9') # Read input
  comp <- intcode(a) # Init computer

  comp$run(1) |> expect_equal(2738720997) # Part 1 (2738720997): Run computer with input 1
  comp$reset()$run(2) |> expect_equal(50894) # Part 2 (50894): Run computer with input 2
})

test_that("day 11", {
  a <- rfp('2019','11') # Read intcode program

  paint <- \(init_color = 0L) {
    comp <- intcode(a) # Init a computer
    pos <- 0+0i # Initial position
    dir <- 1i # Start facing up
    points <- collections::dict(init_color, pos) # Remember tiles visited
    repeat {
      color <- points$get(pos, default = 0L) # Color of current tile
      res <- comp$run(color) # Execute program
      if(is.null(res)) return(points) # Exit when program ends
      points$set(pos, res[1]) # Change color of current tile
      dir <- dir * (if(res[2]) -1i else 1i) # Change direction
      pos <- pos + dir # Advance 1 step forward
    }
  }

  paint()$size() |> expect_equal(2255) # Part 1 (2255): Number of points visited when starting on a black tile

  points <- paint(1L) # Points visited when starting on a white tile
  # Create a table with all tiles and colors corresponding
  dt <- fDT(x=Re(unlist(points$keys())), y=Im(unlist(points$keys())), v = unlist(points$values()))
})

test_that("day 13", {
  a <- rfp('2019','13')
  comp <- intcode(a) # Init a computer
  res <- comp$run() # Run code
  count <- sum(res[c(F,F,T)]==2)
  count |> expect_equal(301) # Part 1 (301): Number of block tiles on screen
  score <- mat <- 0L
  fillMat <- \(x) {
    mat <<- matrix(0L, nrow = max(x[c(T,F,F)])+1, ncol = max(x[c(F,T,F)])+1) # Init matrix
    for (i in seq.int(1,length(x),3)) # Go through output 3 by 3
      mat[x[i]+1,x[i+1]+1] <<- x[i+2] # Fill matrix with correct value
  }

  updateMat <- \(x) {
    for (i in seq.int(1,length(x),3)) { # Go through output 3 by 3
      if(x[i] == -1) {score <<- x[i+2];next} # When x = -1, update score
      if(x[i+2] == 4L) ball_y <<- x[i] # When 3rd value is 4, update ball position
      else if(x[i+2] == 3L) paddle_y <<- x[i] # When 3rd value is 3, update paddle position
      # When a block is destroyed (from 2 to 0) update block count
      else if(x[i+2] == 0L && mat[x[i]+1,x[i+1]+1] == 2L) {count <<- count -1L; mat[x[i]+1,x[i+1]+1] <<- 0L }
    }
  }

  comp2 <- intcode(`substr<-`(a,1,1,'2')) # Init a computer with different value at adress 0
  res2 <- comp2$run(0) # Run computer with neutral position
  fillMat(res2) # Init and fill matrix
  ball_y <- (collapse::whichv(mat, 4L)-1L) %% nrow(mat) + 1L # Init value of ball
  paddle_y <- (collapse::whichv(mat, 3L)-1L) %% nrow(mat) + 1L # Init value of paddle

  # While blocks are present, run computer with joystick input to follow ball
  while (count>0) updateMat(comp2$run(sign(ball_y-paddle_y)))
  score |> expect_equal(14096) # Part 2 (14096): Score when all blocks are destroyed
})

test_that("day 17", {
  a <- rfp('2019','17')
  comp <- intcode::intcode(a, 2)
  res <- comp$reset()$run()

  expect_equal(length(res), 2965L)
})

setwd(oldwd)
# library(testthat)
# library(intcode)
# setwd("~/repos/advent-of-code/2019/intcode/tests/testthat")
# system.time(source('/home/tony/repos/advent-of-code/2019/intcode/tests/testthat/tests.R'))
# 2.710
