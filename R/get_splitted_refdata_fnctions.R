#' Title
#'
#' @param ids 
#' @param n 
#' @param uids 
#' @param seed 
#'
#' @return
#' @export
#'
#' @examples
get_idxUy <-
  function(ids, n, uids=NULL, seed=NULL) {
    if (is.null(uids))
      uids <- sort(unique(ids))
    idx <- lapply(as.list(uids), function(id, seed) {
      candis <- which(ids==id)
      if (!is.null(seed))
        set.seed(seed)
      idx <- sample(candis, min(n, length(candis)))
      return(idx)}, seed=seed)
    rtrn <- data.frame(idx=unlist(idx),
                       y=rep(uids, sapply(idx, length)))
    return(rtrn)
  }
#' Title
#'
#' @param ids 
#' @param n 
#' @param uids 
#' @param minN 
#' @param seed 
#' @param equalizeU If \code{TRUE}, first a subset 
#' of the data with equal class priors is created from which  
#' the unlabeled samples are taken. 
#'
#' @return
#' @export
#'
#' @examples
get_idxUy_unlabeled <-
  function(ids, n, uids=NULL, minN=NULL, seed=NULL,
           equalizeU=FALSE) {
    if (is.null(uids))
      uids <- sort(unique(ids))
    if (equalizeU) { 
      if (is.null(minN))
        stop("Default NULL, not implemented!") # WAS: minN <- min(uids)
      # Equal class priors
      idx <- sapply(uids, function(id, seed) {
        if (!is.null(seed))
          set.seed(seed)
        rtrn <- sample(which(ids==id), minN)
        return(rtrn)}, seed=seed)
      # CHECK: table(ids[idx])
      if (!is.null(seed))
        set.seed(seed)
      idx <- sample(idx, n)
    } else {
      idx <- which(ids%in%uids)
      idx <- sample(idx, n)
    }
    return(idx)
  }

#' Title
#'
#' @param ids 
#' @param fids 
#' @param pTr 
#' @param seed 
#'
#' @return
#' @export
#'
#' @examples
get_tr_fids_idx <-
  function(ids, fids, pTr, seed=NULL) {
    tr.fids <- get_tr_fids(ids, fids, pTr, seed)
    len <- sapply(tr.fids, length)
    tr.fids <- unlist(tr.fids)
    idx <- fids %in% tr.fids
    attr(idx, "len") <- len
    return(idx)
  }

#' Title
#'
#' @param ids 
#' @param fids 
#' @param pTr 
#' @param seed 
#'
#' @return
#' @export
#'
#' @examples
get_tr_fids <-
  function(ids, fids, pTr, seed=NULL) {
    uids <- unique(ids); uids <- sort(uids[!is.na(uids)])
    fids_tr <- lapply(uids, function(id, ids, fids, pTr, seed)
      get_tr_fids_oneId(id, ids=ids, fids=fids, pTr=pTr, seed=seed),
      ids=ids, fids=fids, pTr=pTr, seed=seed)
    return(fids_tr)
  }

#' Title
#'
#' @param id 
#' @param ids 
#' @param fids 
#' @param pTr 
#' @param seed 
#'
#' @return
#' @export
#'
#' @examples
get_tr_fids_oneId <-
  function(id, ids, fids, pTr, seed=NULL) {
    idx.id <- ids==id
    fids.id <- fids[idx.id]
    ufids <- sort(unique(fids.id))
    if (!is.null(seed))
      set.seed(seed)
    fids.id.tr <- sample(ufids, floor(length(ufids)*pTr))
    return(fids.id.tr)
  }
