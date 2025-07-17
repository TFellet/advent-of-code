library(stringi)
a <- rfp('2023','3')
symbols <- stri_locate_all_regex(a, '[^0-9\\.]')
nums <- stri_locate_all_regex(a, '[0-9]+')
stars <- stri_locate_all_fixed(a, '*')
all_nums <- strtoi(collapse::na_rm(unlist(stri_sub_all(a, nums))))

groupMat <- \(x) do.call(rbind, x) |> cbind(id = rep(seq_along(a), times = lengths(x)/2)) |> (\(x) x[!is.na(x[,'start']),])()
pos_nums <- groupMat(nums)

mult <- 1000L
findNei <- \(pos) {
  pos_valids <- pos[,'id'] * mult + pos[,'start']
  pos_valids2 <- c(pos_valids - 1L, pos_valids, pos_valids + 1L)
  c(pos_valids2 - mult, pos_valids2, pos_valids2 + mult)
}

pos_valids <- findNei(groupMat(symbols))
pos_stars_valids <- t(matrix(findNei(groupMat(stars)), ncol = 9))

stars_ids <- num_ids <- vector('integer', nrow(pos_nums))
found <- vector('logical', nrow(pos_nums))
c1 <- c2 <- 0L
for (i in seq_len(nrow(pos_nums))) {
  row <- pos_nums[i,]
  x <- c(row[1]:row[2]) + (row[3] * mult)
  found[[i]] <- any(fastmatch::fmatch(x, pos_valids, nomatch = FALSE))
  found_star <- fastmatch::fmatch(x, pos_stars_valids, nomatch = NA) |> collapse::na_rm()
  if(length(found_star) == 0) next
  stars_ids[c1 <- c1 + 1L] <- kit::funique((found_star - 1) %/% 9 + 1)
  num_ids[c2 <- c2 + 1L] <- i
}

sum(all_nums[found]) # 536576

gears <- which(tabulate(stars_ids)==2L)
collapse::fcount(stars_ids)
total <- 0L
for (i in gears) {
  nums_gear <- num_ids[stars_ids == i]
  total <- total + prod(all_nums[nums_gear])
}
total # 75741499
# 13.2ms
# 14.4ms
# 48.9ms
# 49.6ms
# 63ms
# 250ms
