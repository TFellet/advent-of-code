library(data.table)
library(bench)
library(ggplot2)

year <- '2021'
files <- list.files(paste0(year, '/benchs/times'), full.names = T, '^d\\d')
dates <- file.info(files)[['mtime']]

times <- mapply(\(f,d) {
  fread(f, select = c(1,3), col.names = c('day', 'time'))[,`:=`(file = basename(f), date = d)]
}, files, dates, SIMPLIFY = F) |> rbindlist()
times[,`:=`(time = as_bench_time(time), iter = as.integer(as.factor(date)))]

# ggplot(times, aes(date, time, group=day, color=day)) + geom_line() + geom_point()
ggplot(times, aes(iter, time, group=day, color=day)) + geom_line() + geom_point()

mintimes <- times[,.SD[which.min(time)],by=day]
pri('Total min times: ', sum(mintimes$time))
