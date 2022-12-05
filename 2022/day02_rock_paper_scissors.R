a <- rfp('2022','2') # Read input

ref1 <- c(A = 1L, B = 2L, C = 3L) # Letters to numbers correspondance
ref2 <- c(X = 1L, Y = 2L, Z = 3L) # Letters to numbers correspondance
score <- c(6L, 0L, 3L) # Score associated to each operation result
ref_score <- c(0L, 3L, 6L) # Scores for a loose, draw and win
a2 <- data.table::tstrsplit(a, ' ', fixed = T) # Convert list of 2 chars into 2 lists of chars
b1 <- ref1[a2[[1]]] # Convert letters to numbers
b2 <- ref2[a2[[2]]] # Convert letters to numbers
# Substraction => Determine the outcome of the round
# Additions and modulo => Get the outcome between 1 and 3
# Then, get the score of outcome
# Add score of shape played to get final round score
round_score <- (score[(b2 - b1 + 2L) %% 3L + 1L] + b2)
sum(round_score) # Part 1 (13009): Total score by following strategy guide 1 
# 413µs

# Addition => Determine the shape needed to get desired outcome
# Modulo / addition => Get shape between 1 and 3
# Add score of outcome to get final round score
round_score2 <- ((b1 + b2) %% 3L + 1L + ref_score[b2])
sum(round_score2) # Part 2 (10398): Total score by following strategy guide 2
