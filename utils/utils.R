#' @export
#' Use internal paste without any checks to gain speed
fp <- \(y, d) .Internal(paste0(list(y, '/inputs/day', d, '.txt'), NULL, FALSE))

#' @export
#' Use brio instead of readLines
rfp <- \(y, d,...) brio::read_lines(fp(y,d), ...)

#' @export
#' Shortcut for scan without print
sfp <- \(y, d,...) scan(fp(y,d),quiet=T, ...)

#' @export
#' Convert a vector of strings to a matrix of strings or ints
toGrid <- \(a, w=0, int=T, sep='') {
  spl <- strsplit(a, sep, fixed = T) # Split at each separator, or each char
  if(!w) w <- length(spl[[1]]) # If width is not set, default is at length of first line
  spl <- unlist(spl,use.names = F) # Concat all lines together
  if(int) spl <- strtoi(spl) # Convert to int if necessary
  matrix(spl, ncol = w, byrow = T) # Create a matrix by row
}

#' @export
#' Shortcut for collaspe radixsort
radsort <- \(x, by=x, ...) x[collapse::radixorder(by, ...)]

#' @export
#' Fast internal radix sort without any checks
isort <- \(x, by=x) x[.Internal(radixsort(F, F, FALSE, TRUE, by))]

#' @export
#' Fast internal radix sort without any checks
iorder <- \(x) .Internal(radixsort(F, F, FALSE, TRUE, x))

#' @export
#' Fast internal sort without any checks
isort2 <- \(x, by=x) x[.Internal(order(F, F, by))]

#' @export
#' Fast version of table()
fTable <- \(x) attr(.Call(collapse:::C_group, x, F, T), 'group.sizes', T)

#' @export
#' Repeat each element in x n times faster than rep(x, each=n)
frepEach <- \(x,n) {
  matrix(x,nrow = n, ncol=length(x),byrow = T) |> `dim<-`(NULL)
}

#' @export
#' Convert a matrix to integer
matToInt <- \(mat) {
  d <- dim(mat)
  strtoi(mat) |> `dim<-`(d)
}

#' @export
`%=%` <- collapse::`%=%` # Multiple assign

#' @export
#' Convert arguments to data table
fDT <- \(...) collapse::qDT(list(...))

#' @export
#' Count the number of times a value appears
countFilter <- \(x,val) {
  tmp <- kit::countOccur(x) # Tabulate each value
  tmp$Variable[tmp$Count == val] # Filter on specified value
}

#' @export
#' For a given matrix, compute indices where each of the 4/8/9 neighbours of each cell are
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

#' @export
#' Function to start a day of advent of code
setup <- \(y,d) {
  p <- fp(y, d) # Path to input
  if(!file.exists(p)) dir.create(p, recursive = T)
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
  bench::mark(exprs = list(exp), memory = memory, min_time=min_time, env=new.env(parent = parent.frame()), ...)[,c(2:9)]
}

#' @export
#' When a string of Rust code is run with cargo, a .so file is created
#' This function copy this file to the specified path
getRustSo <- \(name, ...) {
  folder <- file.path(tools::R_user_dir("cargo", "cache"), 'roxido', 'rustlib', 'target', 'release')
  files <- list.files(folder, 'so$', full.names = T) |> file.info()
  last_file <- rownames(files)[which.max(files$mtime)]
  file.copy(last_file, name, ...)
}

#' @export
#' From a compiled file (.so or .dll) returns an R function to call it
importDll <- \(lib_path, fun_name = 'func') {
  dll <- dyn.load(lib_path) # Load previously compiled code
  func <- getNativeSymbolInfo(fun_name, tools::file_path_sans_ext(basename(lib_path))) # Get function name in library
  \(...) .Call(func, ...) # Return R function
}
