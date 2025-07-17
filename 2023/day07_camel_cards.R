a <- sfp('2023','7', what = list('', 1L))

# Rank poker hands : 
# 5 identical, 4 identical, 3+2 identical, 3 identical, 2+2 identical, 2 identical, 1 identical
# + card value from left to right
# 7 possibilities, 13 cards
cards_type <- c('A', 'K', 'Q', 'J', 'T', 9:2)
# r1 <- sapply(a[[1]], \(x) stringi::stri_count_fixed(x, cards_type)) |> ti()
r2 <- sapply(cards_type, \(x) stringi::stri_count_fixed(a[[1]], x))
# r2
maxs <- matrixStats::rowMaxs(r2)
m32 <- maxs == 3 | maxs == 2
ties <- apply(r2[m32,], 1, \(x) kit::topn(x, 2, index=F)[2])
score <- maxs*2
score[m32] <- score[m32] + (ties-1)
# Add individual score card value
# TODO: Substr a[[1]] and match each card to its value then mult by 13^(5:1)
cards_value <- strsplit(a[[1]], '') |> do.call(what = rbind) |> match(rev(cards_type)) |> matrix(ncol = 5)
cards_value2 <- cards_value %*% 13^(5:1)
score2 <- score * 13^6 + cards_value2
# sum(order(score2) * a[[2]])
sum(a[[2]][order(score2)] * seq_along(score2)) # 250957639

# New rules, J are now jokers but are the weakest cards individually
# compute a new max, without J
# new max 2 = new max + J
# create cards_type2 with J at the end
cards_type2 <- c('A', 'K', 'Q', 'T', 9:2, 'J')
notJ <- which(cards_type != 'J')
maxs2 <- matrixStats::rowMaxs(r2, cols = notJ)
maxs3 <- maxs2 + r2[, 'J']
m32_2 <- maxs3 == 3 | maxs3 == 2
ties2 <- apply(r2[m32_2,notJ], 1, \(x) kit::topn(x, 2, index=F)[2])
score_2 <- maxs3*2
score_2[m32_2] <- score_2[m32_2] + (ties2-1)
# Add individual score card value
cards_value_2 <- strsplit(a[[1]], '') |> do.call(what = rbind) |> match(rev(cards_type2)) |> matrix(ncol = 5)
cards_value2_2 <- cards_value_2 %*% 13^(5:1)
score2_2 <- score_2 * 13^6 + cards_value2_2
sum(a[[2]][order(score2_2)] * seq_along(score2_2)) # 251515496
