# devtools::load_all("../occCompaReExp")
# devtools::load_all(".")
devtools::load_all("../occCompaRe")
devtools::load_all(".")

# ----
parcc <- occCompareExp::set_expOpts("revision1", grid = "b")

# ----
res <- load_results(c(".trainUtest", ".resampling"), 
                    verbosity=1, overwrite=FALSE)
lapply(res, colnames)

# ----
ms <- matrix(c(
  "FLEE", "FLEE", NA,
  "FLEE", "at0", "bsvm",
  "FLEE", "at0", "ocsvm",
  #"FLEE", "minTP", NA,
  "FLEE", "tenTP", NA,
  "FLEE", "mxSSSP", NA,
  "FLEE", "eqSSSP", NA,
  "FLI", "FLI", NA,
  "FLI", "at0", "bsvm",
  "FLI", "at0", "ocsvm",
  #"FLI", "minTP", NA,
  "FLI", "tenTP", NA,
  "FLI", "mxSSSP", NA,
  "FLI", "eqSSSP", NA,
  "AUC", "FLEE", NA, 
  "AUC", "FLI", NA, 
  "AUC", "at0", "bsvm", 
  "AUC", "at0", "ocsvm", 
  #"AUC", "minTP", NA, 
  "AUC", "tenTP", NA, 
  "AUC", "mxSSSP", NA,
  "AUC", "eqSSSP", NA), ncol=3, byrow=TRUE) %>%
  as.data.frame(); colnames(ms) <- c("ps", "ts", "cl")

devtools::load_all(".")
AC <- model_selection(res, ms=ms, acc_metric="mxK", 
                      verbose=T, overwrite=TRUE)

PL <- ddp_perfLoss(AC, metric="K", best="binsvm.k.k")

fVals4plot <- get_fVals4plot()
fVals4plot$method[] <- LETTERS[c(26, 1:4)]

devtools::load_all("../occCompaRe")
devtools::load_all(".")

ggp <- ggp_perfloss(PL,
                    ignore_methods="star",
                    factorVals4plot=fVals4plot,
                    cols4plot=get_cols4plot(),
                    box_lwd=.2, 
                    box_outlier_size=.2,
                    axis.text.size=8,
                    strip.text.size=8,
                    rotate_axis.title.y=TRUE,
                    panel.margin.y=0,
                    panel.margin.x=.2
)
g <- ggplotGrob(ggp)
g$heights[[3]] = unit(.4,"cm")
g$heights[[4]] = unit(.4,"cm")
grid.draw(g)
