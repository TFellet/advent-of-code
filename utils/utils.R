#' @export
fp <- \(y, d) .Internal(paste0(list(y, '/inputs/day', d, '.txt'), NULL, FALSE))

#' @export
rfp <- \(y, d,...) brio::read_lines(fp(y,d), ...)
# rfp <- \(y, d,...) readLines(fp(y,d),warn=F, ...)

#' @export
sfp <- \(y, d,...) scan(fp(y,d),quiet=T, ...)

#' @export
toGrid <- \(a, w=0, int=T, sep='') {
  spl <- strsplit(a, sep, fixed = T)
  if(!w) w <- length(spl[[1]])
  spl <- unlist(spl,use.names = F)
  if(int) spl <- strtoi(spl)
  matrix(spl, ncol = w, byrow = T)
}

#' @export
radsort <- \(x, by=x, ...) x[collapse::radixorderv(by, ...)]

#' @export
frepEach <- \(x,n) {
  m <- matrix(x,nrow = n, ncol=length(x),byrow = T)
  dim(m) <- NULL
  m
}

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

#' @export
setup <- \(y,d) {
  p <- fp(y, d)
  session <- Sys.getenv('session')
  if(!file.exists(p)) {
    # Get input
    url <- paste0('https://adventofcode.com/', y, '/day/', d, '/input')
    req <- httr::GET(url, httr::set_cookies(session = session))
    httr::stop_for_status(req)
    txt <- httr::content(req, encoding = 'UTF-8')
    f <- base::file(p)
    on.exit(try(close(f),silent=T))
    writeLines(txt, f, sep='')
    try(close(f),silent=T)
  }
  # Setup R file
  url <- paste0('https://adventofcode.com/', y, '/day/', d)
  req <- httr::GET(url, httr::set_cookies(session = session))
  httr::stop_for_status(req)
  txt <- as.character(httr::content(req, encoding = 'UTF-8'))
  title <- tolower(gsub(' ', '_', stringr::str_match(txt, '<h2>--- Day .*: (.*) ---</h2>')[,2]))
  p2 <- file.path(y, paste0('day',  stringi::stri_pad(d, 2, pad = 0), '_', title, '.R'))
  if(!file.exists(p2)) {
    file.create(p2)
    code <- paste0("a <- rfp('", y, "','", d, "')")
    f2 <- base::file(p2)
    on.exit(try(close(f2),silent=T))
    writeLines(code, f2)
    try(close(f2),silent=T)
  }
  rstudioapi::navigateToFile(p2)
  utils::browseURL(url)
}

#' @export
bench_file <- \(file, memory = F, min_time=1, ...) {
  f <- parse(file=file)
  exp <- bquote(eval(f))
  exp[[2]] <- f
  bench::mark(exprs = list(exp), memory = memory, min_time=min_time, env=new.env(parent = parent.frame()), ...)[,c(2:9)]
}
