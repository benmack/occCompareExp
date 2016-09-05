#' Title
#'
#' @param x 
#' @param fset 
#'
#' @return
#' @export
#'
#' @examples
subset_refset <- function(x, fset="re3a") {
    names_re <- c('re_062_0303_B', 're_062_0303_G', 're_062_0303_R', 're_062_0303_RE', 're_062_0303_IR', 're_092_0402_B', 're_092_0402_G', 're_092_0402_R', 're_092_0402_RE', 're_092_0402_IR', 're_178_0627_B', 're_178_0627_G', 're_178_0627_R', 're_178_0627_RE', 're_178_0627_IR', 're_268_0925_B', 're_268_0925_G', 're_268_0925_R', 're_268_0925_RE', 're_268_0925_IR')
  names_tsx=c('tsx139_133_0513_VH', 'tsx139_166_0615_VH', 'tsx139_199_0718_VH', 'tsx139_232_0820_VH', 'tsx139_254_0911_VH', 'tsx139_265_0922_VH', 'tsx139_133_0513_VV', 'tsx139_166_0615_VV', 'tsx139_199_0718_VV', 'tsx139_232_0820_VV', 'tsx139_254_0911_VV', 'tsx139_265_0922_VV')
  # cnames_exp <- c(names_re, names_tsx)
  cnames <- colnames(x)
  if (fset=="re3a") {
    stopifnot(all(names_re==cnames))
    idx <- !(1:length(names_re)) %in% grep("_062_", names_re)
    rtrn <- x[, idx, drop=F]
  } else if (fset=="re3b") {
    stopifnot(all(names_re==cnames))
    idx <- !(1:length(names_re)) %in% grep("_092_", names_re)
    rtrn <- x[, idx, drop=F]
  } else {
    stop(print("Invalid fset. Got: ", fset, "."))
  }
  
  }
