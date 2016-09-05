#' Title
#'
#' @param type 
#'
#' @return
#' @export
get_fVals4plot <- function (type="B") {
  
  method=c("binsvm"="binSVM",
           "bsvm"="bSVM",
           "maxent"="MaxEnt",
           "maxentDef"="MaxEntD",
           "ocsvm"="ocSVM"
           )
  idP=c("1"="corn", 
        "2"="oilseeds", 
        "3"="pgrass",
        "4"="root", 
        "5"="scereals", 
        "6"="wcereals",
        "7"="bforest", 
        "8"="cforest")
  if (type == "A") {
    ps=c("k"="K",
         "FLEE"="F'-Lee", 
         "FLI"="F'-Li",
         "AUC"="AUC'",
         "DEF"="-")
    ts=c("k"="K",
         "FLEE"="F'-Lee", 
         "FLI"="F'-Li",
         "mxSSSP"="maxSS'",
         "eqSSSP"="equSS'",
         "tenTP"="tenP",
         "at0"="0")
  } else if (type == "B") {
    ps=c("k"="K",
         "FLEE"="Lee", 
         "FLI"="Li",
         "AUC"="AUC",
         "DEF"="-")
    ts=c("k"="K",
         "FLEE"="Lee", 
         "FLI"="Li",
         "mxSSSP"="mxSeSp",
         "eqSSSP"="eqSeSp",
         "tenTP"="tenP",
         "at0"="0")
  } else {stop("Wrong type.")}
  
  return(list(method=method, 
              idP=idP,
              ps=ps,
              ts=ts))  
}
