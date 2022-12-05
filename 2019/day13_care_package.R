a <- rfp('2019','13')
comp <- intcode::intcode(a) # Init a computer
res <- comp$run() # Run code
count <- sum(res[c(F,F,T)]==2)
count # Part 1 (301): Number of block tiles on screen
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
    else if(x[i+2] == 0L && mat[x[i]+1,x[i+1]+1] == 2L) {
      count <<- count -1L; mat[x[i]+1,x[i+1]+1] <<- 0L
    }
  }
}

comp2 <- intcode::intcode(`substr<-`(a,1,1,'2')) # Init a computer with different value at adress 0
res2 <- comp2$run(0) # Run computer with neutral position
fillMat(res2) # Init and fill matrix
ball_y <- (collapse::whichv(mat, 4L)-1L) %% nrow(mat) + 1L # Init value of ball
paddle_y <- (collapse::whichv(mat, 3L)-1L) %% nrow(mat) + 1L # Init value of paddle

# While blocks are present, run computer with joystick input to follow ball
while (count>0) updateMat(comp2$run(sign(ball_y-paddle_y)))
score # Part 2 (14096): Score when all blocks are destroyed
