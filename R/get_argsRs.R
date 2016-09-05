#' Title
#'
#' @param dirData The data directory
#' @param idU The ids of the samples from which to sample 
#' @param idT The ids considered for the test data.
#' @param nTe 
#'
#' @return
#' @export
#'
#' @examples
get_argsRs <- function(dirData, 
                       idP=NULL, idU=NULL,
                       equalizeU=FALSE,
                       idT=NULL, 
                       nTe=100, pTr=0.5, minN.P=2557,
                       dirRefset=NULL,
                       classnames=NULL) {
  
  class_stats <- readRDS(paste0(dirData, "/class_stats.rds"))
  
  if (is.null(dirRefset))
    dirRefset <- paste0(dirData, "_refsets")

  if (is.null(idP)) {
    idP <- class_stats$id[class_stats$id!=0]
  }
  
  if (is.null(idU)) {
    idU <- class_stats$id
  }
  
  if (is.null(idT)) {
    idT <- class_stats$id[class_stats$id!=0]
  }
  
  if (is.null(classnames)) {
    classnames <- as.character(class_stats$name)
  }
  
  args.rs <- list(dirData=dirData,
                  dirRefsets=dirRefset,
                  pTr=pTr,
                  minN.P=minN.P,
                  idP=idP,  
                  idU=idU,
                  equalizeU=equalizeU,
                  idT=idT, 
                  nTe=nTe,
                  classnames=classnames)
  args.rs
}