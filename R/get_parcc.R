#' Title
#'
#' @details 
#' @param seed A vector with the seed points.
#' @param fset A vector with the names of the predictor sets. 
#' (Passed to ) 
#' @param nP 
#' @param nU 
#' @param idP The ids considered as positive classes.
#' unlabeled samples.
#' @param method 
#' @param param 
#' @param n.seeds 
#' @param dirBase 
#' @param dirData 
#' @param dirRes 
#' @param resampling 
#' @param scaling 
#'
#' @return
#' @export
#'
#' @examples
get_parcc <-
  function(seed=NULL,
           fset=NULL,
           nP=NULL,
           nU=NULL,
           idP=NULL,
           method=NULL,
           param=NULL,
           n.seeds=10,
           dirBase="D:/Diss/occcomp",
           dset="agri6clUforest",
           dirData=paste0(dirBase, "/data/rdata_", dset),
           dirRes=NULL,
           dirResSum=NULL,
           nameResSum=NULL, 
           resampling=c("val"),
           scaling=c("ccs01"),
           get_refset=NULL,
           args.rs=NULL,
           nCores=2) {
    # from readRDS(paste0(parcc$bdir, "data/rdata_agriOnly/seeds.rds"))
    # :
    # seeds.bak <- c(14,16,49,56,63,64,76,102,104,111,124,131,136,155,178,180,188,191,201,206,219,239,247,254,296,302,353,359,361,391,422,440,443,452,499,536,542,562,585,592,606,628,670,671,682,688,696,713,737,748,767,784,815,829,854,857,886,888,904,914,922,941,947,953,957,964,966,973,994,995,1001,1004,1015,1023,1024,1027,1082,1083,1116,1150,1173,1174,1179,1268,1275,1295,1308,1313,1330,1372,1383,1391,1400,1425,1436,1460,1475,1477,1527,1554)
    
    class_stats_ignore <- FALSE
    if (file.exists(paste0(dirData, "/class_stats.rds")))
      class_stats <- readRDS(paste0(dirData, "/class_stats.rds"))
    else
      class_stats_ignore <- TRUE
    
    if (is.null(seed))
      seed <- readRDS(paste0(dirData, "/seeds.rds"))
    if (!is.null(n.seeds))
      seed <- seed[1:c(min(n.seeds, length(seed)))]
    
    # SETTINGS
    # loopElements <- c("seed", "fset", "nP", "nU", "idP", "method")
    parcc <- list()
    parcc$bdir <- dirBase
    parcc$seed <- seed
    parcc$resampling <- resampling
    parcc$scaling <- scaling
    
    if (is.null(dirRes)) {
      dirRes <- paste0(dirBase, "/results_", basename(dirData), "/raw")
    }
    if (is.null(dirResSum)) {
      if (!is.null(nameResSum)) {
        dirResSum <- paste0(dirBase, "/results_", basename(dirData), 
                            "/sum_", nameResSum)
      } else {
        dirResSum <- paste0(dirBase, "/results_", basename(dirData), 
                            "/sum")
      }
    }
    
    if (!is.null(fset)) {
      parcc$fset <- fset
    } else {
      parcc$fset <- c("re3b", "tsx", "re3bUtsx")
    }
    
    if (!is.null(nP)) {
      parcc$nP <- nP
    } else {
      # parcc$nP <- c(40, 60, 80)
      parcc$nP <- c(50) # , 100, 200
    }
    
    if (!is.null(nU)) {
      parcc$nU <- nU
    } else {
      # parcc$nU <- c(100, 250, 500)
      parcc$nU <- c(10000) # 1000, 5000, 
    }
    
    if (!is.null(idP)) {
      parcc$idP <- idP
    } else {
      exclude <- class_stats$name%in%c("unlabeled", "others") 
      parcc$idP <- class_stats$id[!exclude]
    }
    
    if (!is.null(method)) {
      parcc$method <- method
    } else {
      parcc$method <- c("ocsvm", "bsvm", "maxent", "binsvm") # with parameters in parcc$param
    }
    
    if (!is.null(param)) {
      parcc$param <- param
    } else {
      parcc$param$maxent <- list(fc=c("LQH", "LQHTP"),
                                 beta=2^seq(-6, 6, by=2))  #2^c(-2:5))
      parcc$param$ocsvm <- list(sigma=2^seq(-12, 0, by=2), #2^seq(-8, 0, by=2),
                                nu=c(.01, .05, .1, 
                                     seq(.2, .95, by=0.1)))
      parcc$param$bsvm <- list(sigma=2^seq(-12, 0, by=2), #2^seq(-8, 0, by=2)
                               cNeg=2^seq(-3, 7, 2),  # 2^seq(-3, 11, 2),
                               cMultiplier=2^seq(1, 10, 2))
      parcc$param$binsvm <- list(sigma=2^seq(-6, 7, by=1), # from Hsu before 2^seq(-2, 7, by=1)
                                 C=2^seq(-5, 17, 2)) # 2^seq(-5, 15, 2)
    }
    parcc$dn.res <- dirRes
    parcc$dn.ressum <- dirResSum
    parcc$get_fn_ressum <- eval(
      parse(text=
        sprintf("function(..., ext='.RDS') {
                 fname = paste('%s', '/', 
                 paste(..., sep='/'), ext, sep='')
                 dir.create(dirname(fname), 
                 showWarnings = F, recursive = T)
                 fname}", 
                dirResSum)))
    
    if (!class_stats_ignore)
      parcc$class_stats <- class_stats
    
    parcc$dset <- dset
    # The following ones are used in the user function
    # get_refset()
    
    parcc$get_refset <- occCompareExp::get_refset
    if (is.null(args.rs))
      parcc$args.rs <- get_argsRs(dirData=paste0(
        parcc$bdir, "/data/rdata_", parcc$dset))
    
    parcc$nCores = nCores
    parcc
  }
