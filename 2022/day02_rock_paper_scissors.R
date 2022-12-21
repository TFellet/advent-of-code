a <- sfp('2022','2', what=list('','')) # Read input as 2 lists
b1 <- fastmatch::fmatch(a[[1]], c('A','B','C')) # Convert letters to numbers
b2 <- fastmatch::fmatch(a[[2]], c('X','Y','Z')) # Convert letters to numbers
# Substraction => Determine the outcome of the round
# Additions and modulo => Get the outcome between 1 and 3
# Then, get the score of outcome
# Add score of shape played to get final round score
score <- c(6L, 0L, 3L) # Score associated to each operation result
round_score <- (score[(b2 - b1 + 2L) %% 3L + 1L] + b2)
sum(round_score) # Part 1 (13009): Total score by following strategy guide 1 
# Addition => Determine the shape needed to get desired outcome
# Modulo / addition => Get shape between 1 and 3
# Add score of outcome to get final round score
ref_score <- c(0L, 3L, 6L) # Scores for a loose, draw and win
round_score2 <- ((b1 + b2) %% 3L + 1L + ref_score[b2])
sum(round_score2) # Part 2 (10398): Total score by following strategy guide 2
