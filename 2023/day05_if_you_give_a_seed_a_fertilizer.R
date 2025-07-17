library(data.table)
a <- rfp('2023','5')

seeds_init <- a[1L] |> strsplit(' ') |> unlist() |> as.numeric() |> collapse::na_rm()
seeds <- seeds_init
spaces <- c(which(a == ''), length(a))
maps <- vector('list', length(spaces)-1L)
for (i in seq_along(maps)) {
  m <- a[(spaces[i]+2):(spaces[i+1]-1)] |> strsplit(' ') |> unlist() |> as.numeric() |> matrix(ncol=3L, byrow=T)
  maps[[i]] <- m
  dest <- seeds
  for (j in seq_len(nrow(m))) {
    found <- seeds >= m[j,2L] & seeds < (m[j,2L] + m[j,3L])
    dest[found] <- dest[found] + m[j,1L] - m[j,2L]
  }
  seeds <- dest
}
min(seeds)

st <- seeds_init[c(TRUE, FALSE)]
en <- st + seeds_init[c(FALSE, TRUE)] - 1
dts <- data.table(start=st, end=en, id_seed = seq_along(st), key = c('start', 'end'))
for (i in seq_along(maps)) {
  dtm <- data.table(startm = maps[[i]][,2], 
                    endm = maps[[i]][,2] + maps[[i]][,3] - 1, 
                    id_map = seq_len(nrow(maps[[i]])),
                    move = maps[[i]][,1] - maps[[i]][,2],
                    key = c('startm', 'endm'))
  overlaps <- foverlaps(dts, dtm)
  overlaps[(start < startm) & ((shift(endm, fill = -Inf) + 1) != startm), 
           `:=`(start_left = pmax(start, shift(endm, fill = -Inf) + 1), end_left = startm - 1) ,by=id_seed]
  overlaps[(end > endm) & ((shift(startm, type = 'lead', fill = Inf) - 1) != endm), 
           `:=`(start_right = endm + 1, end_right = pmin(end, shift(startm, fill = Inf) - 1)) ,by=id_seed]
  overlaps[, `:=`(start_moved = pmax(start, startm) + move, end_moved = pmin(end, endm) + move)]
  
  dts <- rbindlist(use.names = FALSE, list(
    overlaps[, .(start_left, end_left, id_seed = id_seed)], 
    overlaps[, .(start_moved, end_moved, id_seed = id_seed)],
    overlaps[, .(start_right, end_right, id_seed = id_seed)]))[!is.na(start_left)]
  setnames(dts, c('start', 'end', 'id_seed'))
  setkey(dts, start, end)
}
min(dts$start) # 10834440

# 42.7ms

# foverlaps(
#   data.table(start=c(79, 55, 1), end=c(92,67, 10), id_seed = c(1,2,3), key = c('start', 'end')), 
#   data.table(startm = c(80, 90, 0), endm = c(89, 102, 30), id_map = c(1, 2, 3), key = c('startm', 'endm')))
# seed 55-67
# fake map 80-89 => 75-84
# maped 79-79 => 79-79, 80-89 => 75-84, 90-92 => 90-92
# st2 => 75
# en2 => 84
# map  50-97 => seed completly inside map
# seeds2 <- seeds_init
# st <- seeds2[c(TRUE, FALSE)]
# en <- st + seeds2[c(FALSE, TRUE)] - 1
# for (i in seq_along(maps)) {
#   i=0
#   i=i+1
#   maps[[i]]
#   st2 <- st
#   en2 <- en
#   j=0
#   for (j in seq_len(nrow(maps[[i]]))) {
#     j <- j+1
#     m <- maps[[i]][j,]
#     ms <- m[2]
#     me <- m[2] + m[3] - 1
#     if(any(cond <- st < ms & en > me)) {
#       st2 <- c(st2, ms[cond], me[cond] + 1)
#       en2 <- c(en2, ms[cond] - 1, me[cond])
#     }
#     if(any(cond <- st < ms & en == me)) {
#       st2 <- c(st2, ms[cond])
#       en2 <- c(en2, ms[cond] - 1)
#     }
#     if(any(cond <- st == ms & en > me)) {
#       st2 <- c(st2, me[cond] + 1)
#       en2 <- c(en2, ms[cond] - 1, me[cond])
#     }
#   }
# }


# Last exploration
# st2 <- st
# en2 <- en
# st2 <- en2 <- NULL
# j=1
# found <- rep(FALSE, length(st))
# for (j in seq_len(nrow(maps[[i]]))) {
#   j <- j+1
#   m <- maps[[i]][j,]
#   m <- c(75, 80, 10)
#   ms <- m[2]
#   me <- m[2] + m[3] - 1
#   move <- m[1] - m[2]
#   st
#   en
#   # If ms in (st, en), create 2 new ranges, (st, ms-1) and (ms, en)
#   # if me in (st, en), create 2 new ranges, (st, me) and (me+1, en)
#   cond_ms <- st < ms & ms < en
#   cond_me <- st < me & me < en
#   cond_inside <- st > ms & en < me
#   found <- found | cond_ms | cond_me | cond_inside
#   # When a range is found, split into 2 ranges, add the range to move into st2 and modify the other range inside st
#   # Move the overlap of seed and map to new range
#   st2 <- c(st2, ms[cond_ms] + move)
#   en2 <- c(en2, pmin(me[cond_ms], en[cond_ms]) + move)
#   me[cond_me]
#   # If map is completly inside seed, add a new range from map end to seed end
#   add_st <- me[cond_ms & cond_me] + 1
#   add_en <- en[cond_ms & cond_me]
#   
#   st
#   en
#   st[cond_ms]
#   en[cond_ms] <- ms[cond_ms]-1
#   
#   
#   st
#   en
#   # st2 <- c(st2, st[cond_ms], ms[cond_ms] + move)
#   # en2 <- c(en2, ms[cond_ms] - 1, en[cond_ms])
#   # st2 <- c(st2, st[cond_me] + move, me[cond_me] + 1)
#   # en2 <- c(en2, me[cond_me] + move, en[cond_me])
#   
# }
