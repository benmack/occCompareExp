#' Get the data for a experiment defined
#'
#' @param seed Seedpoint
#' @param fset Feature set
#' @param nP Number of positive samples
#' @param nU Number of unlabeled samples
#' @param args.rs Names list of Arguments required in the functions.
#' @param overwrite Overwrite the refset if it is already cached?
#' @param ... 
#'
#' @return data frame with columns set, y, and the feaatures.
#' @export
#'
#' @examples
get_refset <-
  function(seed, nP, nU, fset, args.rs, overwrite=FALSE, ...) {
    
    fn.refset <- function(what, n, seed, fset)
      paste0(args.rs$dirRefsets, "/", fset, "_n", n, 
             "_seed", seed, what, ".rds")
    
    #fnames <- fn.refset(nP, nU, seed, 
    #                             c("_Ptr", "_Utr", "_Te"))
    fnames <- c(fn.refset("_Ptr", nP, seed, fset),
                fn.refset("_Utr", nU, seed, fset),
                fn.refset("_Te", args.rs$nTe, seed, fset))
    
    fnData <- function(dirData, ...)
      paste0(dirData, ...)
    
    fn.y <- fnData(args.rs$dirData, "/ids.rds")
    fn.f <- fnData(args.rs$dirData, "/fids.rds")
    
    if (!all(file.exists(fnames)) | overwrite) {
      
      if (fset%in%c("re", "tsx")) {
        X <- readRDS(fnData(args.rs$dirData, "/", fset, ".rds"))
      } else if (fset=="reUtsx") {
        X <- cbind(readRDS(fnData(args.rs$dirData, "/", "re", ".rds")),
                   readRDS(fnData(args.rs$dirData, "/", "tsx", ".rds")))
      } else if (fset=="re3a" | fset=="re3aUtsx") {
        X <- readRDS(fnData(args.rs$dirData, "/", "re", ".rds"))
        X <- subset_refset(X, "re3a")
        if (fset=="re3aUtsx")
          X <- cbind(X, 
                     readRDS(fnData(args.rs$dirData, "/", "tsx", ".rds")))
      } else if (fset=="re3b" | fset=="re3bUtsx") {
        X <- readRDS(fnData(args.rs$dirData, "/", "re", ".rds"))
        X <- subset_refset(X, "re3b")
        if (fset=="re3bUtsx")
          X <- cbind(X, 
                     readRDS(fnData(args.rs$dirData, "/", "tsx", ".rds")))
      } else {
        stop(print("Invalid fset. Got: ", fset, "."))
      }
      
      y <- readRDS(fn.y)
      f <- readRDS(fn.f)
      
      if (any(!file.exists(fnames[c(1, 3)])) | overwrite)
        idx.fTr <- get_tr_fids_idx(y, f, args.rs$pTr, seed=seed)
      
      # The positive training data
      cat("Warning: New version with train/val/test sets.\n")
      refsetPTr <- doOrReadRDS({
        idxUy <- get_idxUy(y*idx.fTr, nP*2, args.rs$idP,     # TVT
                           seed=seed)
        set = factor(rep(rep(c("tr", "va"), each=nP), 
                         length(args.rs$idP)),
                     levels=c("tr", "va", "te"))              # TVT
        data.frame(set=set, y=idxUy$y, X[idxUy$idx, ])
      }, fnames[1], overwrite=overwrite)
      # The unlabeled training data
      refsetUTr <- doOrReadRDS({
        idxU <- get_idxUy_unlabeled(
          y, nU*2, args.rs$idU, args.rs$minN.P, seed=seed,
          equalizeU=args.rs$equalizeU)                        # TVT
        set = factor(rep(c("tr", "va"), each=nU),
                     levels=c("tr", "va", "te"))              # TVT
        df <- data.frame(set=set, y=0, X[idxU, ])
        attr(df, "y") <- y[idxU]
        df
      }, fnames[2], overwrite=overwrite)
      # The test data
      refsetTe <- doOrReadRDS({
        idxUy <- get_idxUy(y*!idx.fTr, args.rs$nTe, args.rs$idT, seed=seed)
        set = factor(rep("te", nrow(idxUy)),
                     levels=c("tr", "va", "te"))              # TVT
        data.frame(set=set, y=idxUy$y, X[idxUy$idx, ])
      }, fnames[3], overwrite=overwrite)
    } else {
      refsetPTr <- readRDS(fnames[1])
      refsetUTr <- readRDS(fnames[2])
      refsetTe <- readRDS(fnames[3])
    }
    rs = rbind(refsetPTr, refsetUTr, refsetTe)
    attr(rs, "y.un") <- attr(refsetUTr, "y")
    return(rs)
  }
