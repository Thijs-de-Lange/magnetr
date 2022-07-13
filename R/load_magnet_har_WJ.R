#' @export
magnet_read_all_headers <- function(fullfilepath, header_as_valcolname = FALSE) {
  #Function returns all headers in a list of tidy data frames. Also works for sets.
  d1a <- HARr::read_har(file.path(fullfilepath))
  headers <- names(d1a)

  d1_headers_all <- list()
  for (header in headers) {
    d1_header <- reshape2::melt(d1a[[header]])
    cn = colnames(d1_header)
    if(length(cn) > 1) {
      for (n in 2:length(cn)){
        if (cn[n] == cn[n-1]){
          # This to avoid columns with identical names, kan slimmer
          cn[n] <- paste(cn[n], "_2",sep="")
        }
      }
      names(d1_header) <- cn
    }
    d1_header <- rename(d1_header, Value = value)
    if(header_as_valcolname){ # This just uses the header as the Value col name, sometimes useful.
      d1_header[[header]] <- d1_header$Value
      d1_header <- select(d1_header, -Value)
    }
    d1_headers_all[[header]] <- d1_header
  }
  return(d1_headers_all)
}
