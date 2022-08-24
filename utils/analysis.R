library(data.table)
library(bench)
library(ggplot2)

year <- '2020'
files <- list.files(paste0(year, '/benchs/times'), full.names = T, '^d\\d')
dates <- file.info(files)[['mtime']]

# Read benchmarks times
times <- mapply(\(f,d) {
  fread(f, select = c(1,3), col.names = c('day', 'time'))[,`:=`(file = basename(f), date = d)]
}, files, dates, SIMPLIFY = F) |> rbindlist()
times[,`:=`(time = as_bench_time(time), iter = as.integer(as.factor(date)))]
setkey(times, iter)

html_graphs()

# Plot all times on all days in log scale to see improvements
ggplot(times, aes(iter, time, group=day, color=day)) + geom_line() + geom_point() +
  theme(legend.text=element_text(size=6), legend.key.height = unit(5,'mm'), legend.position = 'bottom') + 
  guides(fill=guide_legend(ncol=1))

# Minimum recorded times for each day
mintimes <- times[,.SD[which.min(time)],by=day][,cumulative_time := as_bench_time(cumsum(time))]
pri('Total min times: ', sum(mintimes$time))

# Total time on each benchmark
times[,sum(time),by=file]

# Percentage improvement from previous benchmark
perc <- \(t) round((t[2]-t[1])/t[1]*100,2)
times[iter >= max(iter)-1,perc(as.numeric(time)),day]

# Create a isolated plot for each day
ggplot(times, aes(iter, time)) + geom_line() + geom_point() + facet_wrap(vars(day), scales = 'free')
