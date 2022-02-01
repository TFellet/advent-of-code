# Load all necessary librairies and setup parameters
lib_list <- c('microbenchmark', 'purrr', 'matrixStats', 'expm', 'ggplot2', 'collections', 'stringi', 'stringr', 
              'Rfast', 'data.table', 'bench', 'brio', 'fastmatch', 'collapse', 'kit')
for (lib in lib_list) suppressPackageStartupMessages(library(lib, character.only = T, quietly = T, warn.conflicts = FALSE))
F <- FALSE
options(scipen = 50)
setDTthreads(11L)


year <- '2021'
files <- list.files(year, '.R$', full.names = T)
exprs <- map(files, ~parse(file=.x)) # Read and parse all source code files
exp <- bquote(eval(f)) # Create an empty eval() instruction
exprs <- map(exprs, \(f){exp2<- exp;exp2[[2]] <- f;exp2}) # Insert each source code inside the eval instruction
names(exprs) <- substr(files, 6L, (nchar(files)-2L)) # Rename all expressions by filename

tictoc::tic()
# Run benchmark on all expressions in a new env each time, for at least 1s by expression 
times <- mark(exprs = exprs, check = F, filter_gc = F, memory = F, env = new.env(), min_time = 1)
tictoc::toc()

# Clean up columns
times2 <- as.data.table(times)
times2[,expression := attributes(expression)[['description']]]
times2[,c('result', 'memory', 'gc', 'time', 'gc/sec', 'n_gc') := NULL]

# Find next file name to add
# Pattern is d{pad]{max_day}_V{version number in max_day}
folder <- paste0(year, '/benchs/times')
pad <- if(length(exprs)>=10) '' else '0'
maxd <- paste0('d', pad,length(exprs))
iter <- length(list.files(folder, pattern = maxd))+1L
f <- paste0(folder, '/' ,maxd,'_V',iter,'.csv')

# Write time to keep track of improvements
fwrite(times2, f)

pri('Total time: ', sum(times2$median)) # Total execution time
