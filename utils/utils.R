#' Use internal paste without any checks to gain speed
#' @export
fp <- \(y, d) .Internal(paste0(list(y, '/inputs/day', d, '.txt'), NULL, FALSE))

#' Use brio instead of readLines
#' @export
rfp <- \(y, d,...) brio::read_lines(fp(y,d), ...)

#' Shortcut for scan without print
#' @export
sfp <- \(y, d,...) scan(fp(y,d),quiet=T, ...)

#' Convert a vector of strings to a matrix of strings or ints
#' @param a Input vector
#' @param w Desired width of the matrix
#' @param int Convert the matrix to int
#' @param sep Separator inside the strings
#' @return A matrix with dimensions length(a) * w
#' @export
toGrid <- \(a, w=0, int=T, sep='') {
  spl <- strsplit(a, sep, fixed = T) # Split at each separator, or each char
  if(!w) w <- length(spl[[1]]) # If width is not set, default is at length of first line
  spl <- unlist(spl,use.names = F) # Concat all lines together
  if(int) spl <- strtoi(spl) # Convert to int if necessary
  matrix(spl, ncol = w, byrow = T) # Create a matrix by row
}

#' Get last value of vector
#' @export
flast <- \(x) x[[length(x)]]

#' Fast internal radix sort without any checks
#' @export
radsort <- \(x, by=x) x[.Internal(radixsort(F, F, FALSE, TRUE, by))]

#' Fast internal radix sort without any checks
#' @export
radorder <- \(x) .Internal(radixsort(F, F, FALSE, TRUE, x))

#' Fast version of table()
#' @export
fTable <- \(x) attr(.Call(collapse:::C_group, x, F, T), 'group.sizes', T)

#' Repeat each element in x n times faster than rep(x, each=n)
#' @export
# frepEach <- \(x,n) {
#   matrix(x,nrow = n, ncol=length(x),byrow = T) |> `dim<-`(NULL)
# }

#' Repeat each element in x n times faster than rep(x, each=n)
#' @export
frepEach <- \(x,n) {
  rep(x, rep(n,length(x)))
}

#' Convert a matrix to integer
#' @export
matToInt <- \(mat) {
  d <- dim(mat)
  strtoi(mat) |> `dim<-`(d)
}

#' @export
`%=%` <- collapse::`%=%` # Multiple assign

#' Convert arguments to data table
#' @export
fDT <- \(...) collapse::qDT(list(...))

#' Count the number of times a value appears
#' @export
countFilter <- \(x,val) {
  tmp <- kit::countOccur(x) # Tabulate each value
  tmp$Variable[tmp$Count == val] # Filter on specified value
}

#' For a given matrix, compute indices where each of the 4/8/9 neighbours of each cell are
#' @export
adja <- \(mat, pad=NA, n=4L) {
  mat2 <- seq_along(mat)
  dim(mat2) <- dim(mat)
  mat.pad <- unname(rbind(pad, cbind(pad, mat2, pad), pad)) # Add padding arround matrix
  ind_row <- 2:(nrow(mat) + 1) # row indices of the "middle"
  ind_col <- 2:(ncol(mat) + 1) # column indices of the "middle"
  # Find neighbours in n directions, clockwise, starting from North or row by row for n = 9
  neigh <-
    if(n==4L)
      cbind(mat.pad[ind_row-1,ind_col  ], # N
            mat.pad[ind_row  ,ind_col+1], # E
            mat.pad[ind_row+1,ind_col  ], # S
            mat.pad[ind_row  ,ind_col-1]) # W
    else if (n==8L)
      cbind(mat.pad[ind_row-1,ind_col  ], # N
            mat.pad[ind_row-1,ind_col+1], # NE
            mat.pad[ind_row  ,ind_col+1], # E
            mat.pad[ind_row+1,ind_col+1], # SE
            mat.pad[ind_row+1,ind_col  ], # S
            mat.pad[ind_row+1,ind_col-1], # SW
            mat.pad[ind_row  ,ind_col-1], # W
            mat.pad[ind_row-1,ind_col-1]) # NW
    else if(n==9L)
      cbind(mat.pad[ind_row-1,ind_col-1], # NW
            mat.pad[ind_row-1,ind_col  ], # N
            mat.pad[ind_row-1,ind_col+1], # NE
            mat.pad[ind_row  ,ind_col-1], # W
            mat.pad[ind_row  ,ind_col  ], # Origin
            mat.pad[ind_row  ,ind_col+1], # E
            mat.pad[ind_row+1,ind_col-1], # SW
            mat.pad[ind_row+1,ind_col  ], # S
            mat.pad[ind_row+1,ind_col+1]) # SE
    else stop('Invalid n')

  dim(neigh) <- c(length(mat), n) # Reshape matrix to have (n points in original matrix) * (4 directions)
  neigh
}

#' Function to start a day of advent of code
#' @export
setup <- \(y,d) {
  p <- fp(y, d) # Path to input
  if(!dir.exists(dirname(p))) dir.create(dirname(p), recursive = T)
  session <- Sys.getenv('session') # Token on website
  if(!file.exists(p)) { # Input file not present
    url <- paste0('https://adventofcode.com/', y, '/day/', d, '/input') # Input url
    req <- httr::GET(url, httr::set_cookies(session = session)) # Get input
    httr::stop_for_status(req) # Stop on error
    txt <- httr::content(req, encoding = 'UTF-8') # Get content of request
    f <- base::file(p) # File object
    on.exit(try(close(f),silent=T)) # Close file connexion when function ends
    writeLines(txt, f, sep='') # Write input to file
    try(close(f),silent=T) # Close file if not already done
  }
  # Setup R file
  url <- paste0('https://adventofcode.com/', y, '/day/', d) # Problem url
  req <- httr::GET(url, httr::set_cookies(session = session)) # Get problem page
  httr::stop_for_status(req)
  txt <- as.character(httr::content(req, encoding = 'UTF-8')) # Content of page
  # Get title of problem to insert in script name
  title <- tolower(gsub(' ', '_', stringr::str_match(txt, '<h2>--- Day .*: (.*) ---</h2>')[,2]))
  # Create script file path with problem name
  p2 <- file.path(y, paste0('day',  stringi::stri_pad(d, 2, pad = 0), '_', title, '.R'))
  if(!file.exists(p2)) { # If script is not present
    file.create(p2) # Create the file
    code <- paste0("a <- rfp('", y, "','", d, "')") # Code to read input
    f2 <- base::file(p2) # Declare file object
    on.exit(try(close(f2),silent=T))
    writeLines(code, f2) # Write code to read input
    try(close(f2),silent=T)
  }
  rstudioapi::navigateToFile(p2) # Open the script file in Rstudio
  utils::browseURL(url) # Open the webpage of the problem inside browser
}

#' @export
#' Time the execution of a complete file
bench_file <- \(file, memory = F, min_time=1, ...) {
  f <- parse(file=file) # Parse file content
  exp <- bquote(eval(f)) # Add the eval around the code
  exp[[2]] <- f
  # Benchmark the file with bench package
  bench::mark(exprs = list(exp), memory = memory, min_time=min_time, filter_gc = FALSE, check=F, env=new.env(parent = parent.frame()))[,c(2:9)]
}

#' @export
#' Time the execution of a complete file
bench_file2 <- \(file, memory = F, min_time=1, ...) {
  # memory = F
  # min_time=1
  # file='2022/day09_rope_bridge.R'
  # f='2022/day01_calorie_counting.R'
  invisible(compiler::cmpfile(file, '/tmp/rcmp.Rc', options = list(optimize=3)))
  # compiler::loadcmp('/tmp/rcmp.Rc') |> ti()
  # file='/tmp/rcmp.Rc'
  exprs <- .Internal(load.from.file('/tmp/rcmp.Rc'))
  env <- new.env()
  ev <- \(exprs, envir) {
    for (i in exprs) {
      eval(i, envir)
    }
  }
  # ev(exprs, env) |> ti()
  # brio::read_file_raw(file)
  # f <- parse(file=file) # Parse file content
  # exp <- bquote(eval(f)) # Add the eval around the code
  # exp[[2]] <- f
  # eval(as.call(f))
  # exp <- bquote(eval(\(){f}())) # Add the eval around the code
  # exp[[2]][[3]][[1]][[2]] <- f
  # Benchmark the file with bench package
  bench::mark(ev(exprs, env), memory = memory, min_time=min_time, filter_gc = FALSE, check=F, env=new.env())[,c(2:9)]
}

#' When a string of Rust code is run with cargo, a .so file is created
#' This function copy this file to the specified path
#' @export
getRustSo <- \(name, ...) {
  folder <- file.path(tools::R_user_dir("cargo", "cache"), 'roxido', 'rustlib', 'target', 'release')
  files <- list.files(folder, 'so$', full.names = T) |> file.info()
  last_file <- rownames(files)[which.max(files$mtime)]
  file.copy(last_file, name, ...)
}

#' From a compiled file (.so or .dll) returns an R function to call it
#' @export
importDll <- \(lib_path, fun_name = 'func') {
  dll <- dyn.load(lib_path) # Load previously compiled code
  func <- getNativeSymbolInfo(fun_name, tools::file_path_sans_ext(basename(lib_path))) # Get function name in library
  \(...) .Call(func, ...) # Return R function
}

#' Source a file into current environment but faster than source
#' Causes problems with bench::mark
#' @export
load <- \(f) brio::read_file(f) |> str2lang() |> eval.parent()

#' @export
readTxt <- \(txt) { brio::write_file(txt, '/tmp/rtmp'); brio::read_lines('/tmp/rtmp') }
