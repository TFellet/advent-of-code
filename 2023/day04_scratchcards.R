library(stringi)

`%fin%` <- fastmatch::`%fin%`
a <- rfp('2023','4')
dots <- stri_locate_first_fixed(a, ':')
pipe <- stri_locate_first_fixed(a, '|')
nums_left <- stri_extract_all_regex(stri_sub(a, dots[,1]+2, pipe[,1]-2), '\\d+') |> lapply(strtoi)
nums_right <- stri_extract_all_regex(stri_sub(a, pipe[,1]+2, nchar(a)), '\\d+') |> lapply(strtoi)

points <- vector('integer', length(nums_left))
cards <- rep(1, length(nums_left)*2)
for (i in seq_along(nums_left)) {
  points[i] <- sum(nums_right[i] %fin% nums_left[i])
  if(points[i] > 0) {
    adds <- (i+1):(i+points[i])
    cards[adds] <- cards[adds] + cards[i]
  }
}
sum(2^points %/% 2)
sum(cards[seq_along(nums_left)])
