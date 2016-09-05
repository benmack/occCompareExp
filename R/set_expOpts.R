#' Title
#'
#' @param type 
#'
#' @return
#' @export
#'
#' @examples
set_expOpts <- function(type="revision1", grid="b") {
  
  if (grid=="submission") {
    param <- list(maxent=list(fc=c("LQH", "LQHTP"),
                              beta=2^seq(-6, 6, by=2)),
                  bsvm=list(sigma=2^seq(-8, 0, by=2), 
                            cNeg=2^seq(-3, 7, 2),  
                            cMultiplier=2^seq(1, 10, 2)),
                  binsvm=list(sigma=2^seq(-2, 7, by=1), 
                              C=2^seq(-5, 15, 2)))
  } else if (grid == "a") {
    param <- list(maxent=list(fc=c("LQH", "LQHTP"),
                              beta=2^seq(-6, 6, by=2)),  
                  ocsvm=list(sigma=2^seq(-12, 0, by=2),
                             nu=c(.01, .05, .1, seq(.2, .9, by=0.1))),
                  bsvm=list(sigma=2^seq(-12, 0, by=2),
                            cNeg=2^seq(-3, 7, 2), 
                            cMultiplier=2^seq(1, 10, 2)),
                  binsvm=list(sigma=2^seq(-6 , 7, by=1), 
                              C=2^seq(-5, 17, 2)))
  } else if (grid == "b") {
    sigma <- 2^seq(-12, 0, by=2)
    C <- 2^seq(-7, 9, by=2)
    param <- list(maxent=list(fc=c("LQH", "LQHTP"),
                              beta=2^seq(-6, 6, by=2)),  
                  ocsvm=list(sigma=sigma,
                             nu=c(.01, .05, .1, seq(.2, .95, 
                                                    by=0.1))),
                  bsvm=list(sigma=sigma,
                            cNeg=C, 
                            cMultiplier=2^seq(1, 10, 2)),
                  binsvm=list(sigma=sigma, 
                              C=C))
    
  } else stop("Unknown grid.")
  valid_types = c("submission", 
                  "submission_nSeeds2", 
                  "ocsvmOnly4revision1", 
                  "ocsvm_seed99",
                  "revision1",
                  "revision1_oneExp"
  )
  if (!type %in% valid_types)
    stop(cat(sprintf("'%s', not defined. Defined are: \n  %s\n", 
                     type, paste0(valid_types, collapse=","))))
  nameResSum = paste0(type, grid) 
  stts = switch(type,
                submission = 
                  get_parcc(nP=c(50), nU=10000, idP=1:8, seed=1:10,
                            method=c("binsvm", "bsvm", "maxent"),
                            nameResSum=nameResSum, param=param),
                submission_nSeeds2 = 
                  get_parcc(nP=c(50), nU=10000, idP=1:8, seed=1:2,
                            method=c("binsvm", "bsvm", "maxent"),
                            nameResSum=nameResSum, param=param),
                ocsvmOnly4revision1 = 
                  get_parcc(nP=c(50), nU=10000, idP=1:8, seed=1:10,
                            method=c("ocsvm"),
                            nameResSum=nameResSum, param=param),
                ocsvm_seed99 = 
                  get_parcc(nP=c(50), nU=10000, idP=1:8, seed=99,
                            method=c("ocsvm"),
                            nameResSum=nameResSum, param=param),
                revision1 =
                  get_parcc(nP=c(50), nU=10000, idP=1:8, seed=1:10,
                            method=c("binsvm", "ocsvm", "bsvm", "maxent"),
                            nameResSum=nameResSum, param=param),
                revision1_oneExp =
                  get_parcc(nP=c(50), nU=10000, idP=1, seed=1, fset="re3b",
                            method=c("binsvm", "ocsvm", "bsvm", "maxent"),
                            nameResSum=nameResSum, param=param)
  )
  options(occc_settings=stts)
  parcc <- get_settings()
  return(parcc)
}