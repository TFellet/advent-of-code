a <- strtoi(rfp('2021','1'))
(collapse::flag(a, c(-1L,-3L),fill=0L)>a) |> matrixStats::colSums2()
