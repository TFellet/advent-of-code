#' @export
fp <- \(y, d) file.path(y, 'inputs', paste0('day', d, '.txt'))

#' @export
rfp <- \(y, d,...) readLines(fp(y,d),warn=F, ...)

#' @export
toGrid <- \(a, w=0, int=T, sep='') {
  spl <- strsplit(a, sep)
  if(!w) w <- length(spl[[1]])
  spl <- unlist(spl)
  if(int) spl <- strtoi(spl)
  matrix(spl, ncol = w, byrow = T)
}

#' @export
sfp <- \(y, d,...) scan(fp(y,d),quiet=T, ...)

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
    code <- paste0('a <- rfp(', y, ',', d, ')')
    f2 <- base::file(p2)
    on.exit(try(close(2),silent=T))
    writeLines(code, f2)
    try(close(2),silent=T)
  }
  rstudioapi::navigateToFile(p2)
  utils::browseURL(url)
}