dPrep_agri_cult1_to_L1_class_mapping <- 
  list(barley=c("Winterfuttergerste",
                "Winterbraugerste"),
       corn=c("Koernermais", 
              "Silomais"),
       f.legumes=c("SaatgutFutterleguminosen", 
                   "Futterleguminosen"), 
       p.grassland=c("WeideOderMaehweideBeweidet", 
                     "WieseNichtBeweidet", 
                     "Wiese/Weiden"),
       peas=c("Erbsen"),
       potatoes=c("Speisekartoffeln", "Pflanzkartoffeln"), 
       rapeseed=c("WinterrapsRruebsen"),
       raygrass=c("Raygrass"),
       rye=c("Winterfutterroggen",
             "Winterbrotroggen"),
       s.barley=c("Sommerbraugerste",
                  "Sommerfuttergerste"),
       s.oat=c("Sommerhafer"),
       s.triticale=c("Sommertriticale"),
       s.wheat=c("Sommerweizen"),
       spelt=c("SpelzDinkel"),
       triticale=c("Wintertriticale"),
       wheat=c("Winterfutterweizen",
               "Winterbrotweizen")
  )

#' Title
#'
#' @param ids 
#' @param fids 
#' @param uids 
#' @param rm4perc 
#'
#' @return
#' @export
#'
#' @examples
dPrep_pixel_poly_stats <- function(ids, fids, uids=NULL, rm4perc=NULL) {
  if (is.null(uids))
    uids <- sort(unique(ids))
  ufids <- unique(fids)
  
  ans <- numeric(length(uids))
  restab <- data.frame(id=uids, nSmpls=ans, nFlds=ans, 
                       prcSmpls=ans, prcFlds=ans)
  #ans <- numeric(length(ufids))
  #restab.smplsPerField <- data.frame(ID=ans, FID=ans, nSmpls=ans)
  
  for (i in 1:nrow(restab)) {
    idmask <- ids==restab$id[i]
    restab$nSmpls[i] <- sum(idmask)
    restab$nFlds[i] <- length(unique(fids[idmask]))
  }
  if (is.null(rm4perc)) {
    idx <- 1:nrow(restab)
  } else {
    idx <- restab$id %in% rm4perc
  }
  
  restab$prcSmpls <- restab$nSmpls/sum(restab$nSmpls[idx])*100
  restab$prcFlds <- restab$nFlds/sum(restab$nFlds[idx])*100
  return(restab)
}

#' Title
#'
#' @param idsOld 
#' @param namesOld 
#' @param newNamesList 
#'
#' @return
#' @export
#'
#' @examples
dPrep_get_newUold_ids <- function(idsOld, namesOld, newNamesList) {
  newnames <- sort(names(newNamesList))
  lst <- vector(length(newnames), mode="list")
  names(lst) <- newnames
  for (n in newnames)
    lst[[n]] <- idsOld[namesOld %in% newNamesList[[n]]]
  
  df <- data.frame(id=1:length(lst), name=newnames, 
                   oldId=sapply(lst, function(x) paste(x, collapse=",")),
                   stringsAsFactors=FALSE)
  rownames(df) <- NULL
  return(df)
}

 
#' Title
#'
#' @param oldIds 
#' @param lut 
#'
#' @return
#' @export
#'
#' @examples
dPrep_reclassify <- function(oldIds, lut) {
  newIds <- oldIds*0
  for (i in 1:nrow(lut)) {
    from <- as.integer(strsplit(lut$oldId[i], ",")[[1]])
    to <- lut$id[i]
    newIds[oldIds %in% from] <- to
  }
  return(newIds)
}

#' Title
#'
#' @param x 
#' @param rng.in 
#' @param rng.out 
#' @param percentiles 
#' @param cut.tails 
#'
#' @return
#' @export
#'
#' @examples
dPrep_scale_minmax <- function(x, rng.in=c(.01, .99), 
                               rng.out=c(0, 1), percentiles=T,
                               cut.tails=T) {
  mima <- quantile(x, probs=c(rng.in[1], rng.in[2]))
  df <- data.frame(x=mima, y=rng.out)
  if (cut.tails) {
    vals[x<mima[1]] <- mima[1]
    vals[x>mima[2]] <- mima[2]
  }
  fit <- lm(y~x, data=df)
  x.scaled <- predict(fit, data.frame(x=x) )
  return(x.scaled)
}

#' Title
#'
#' @param dir 
#' @param buf 
#' @param rmUn 
#'
#' @return
#' @export
#'
#' @examples
dPrep_get_ids_orig <- function(dir="data/raster", buf=20, 
                               rmUn=FALSE, addForest=F) {
  cells <- which(raster(file.path(dir, "/re_tsx139_mask"))[]==1)
  ids_orig <- raster(file.path(dir, "agri_cult1"))[cells]
  if (addForest) {
    ids_forest <- raster(file.path(dir, "noAgri_forest_cid.tif"))[cells]
    idx_forest <- ids_forest%in%c(1,2)
    ids_forest[idx_forest] <- ids_forest[idx_forest]+240
  }
  ids_corr <- raster(file.path(dir, "agri_corrected"))[cells]
  idx.corr <- which(!ids_corr%in%c(-1,0))
  ids_orig[idx.corr] = ids_corr[idx.corr]
  table(ids_orig)
  if (!is.null(buffer)) {
    mask_buf <- raster(paste0(dir, "/agri_buffer", buf, "m"))[cells]==1
    ids_orig[ids_orig>0 & !mask_buf] <- 0
  }
  if (addForest)
    ids_orig[idx_forest] <- ids_forest[idx_forest]

  if (rmUn) {
    idx.rm <- ids_orig==0
    cells <- cells[idx.rm]
    ids_orig <- ids_orig[idx.rm]
  }
  lut <- read.csv(file.path(dir, "KulturenUndIdsUmainTypes.csv"), header=T,
                  row.names = "X")
  if (addForest)
    lut <- rbind(lut, data.frame(Nutzungen=c("Laubwald", "Nadelwald"),
                                 Code=c(241, 242),
                                 type=c("bforest", "cforest")))
  orig <- list(ids=ids_orig, cells=cells, lut=lut)
  
  orig$freq <- table(orig$ids)
  orig$lut=lut
  rm(ids_orig, cells)
  
  orig$lut$nPix <- 0
  idx.ans <- match(as.numeric(names(orig$freq)), orig$lut$Code)
  ids_orig_not_in_lut_orig <- orig$freq[is.na(idx.ans)]
  
  ids_orig_not_in_lut_orig
  
  idx.ans.nNa.in_lut_orig <- idx.ans[!is.na(idx.ans)]
  idx.ans.nNa.in_ids_orig_freq <- !is.na(idx.ans)
  
  orig$lut$nPix[idx.ans.nNa.in_lut_orig] <- orig$freq[idx.ans.nNa.in_ids_orig_freq]
  write.csv(orig$lut, file=paste0(dir, "/Kulturen_Ids_nPix_mainTypes.txt"), 
            quote=FALSE)
  
  return(orig)
}