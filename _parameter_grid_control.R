devtools::load_all("../occCompaRe")
devtools::load_all(".")

# ----

parcc <- set_expOpts("revision1", "b")
res <- load_results(c(".trainUtest", ".resampling"), verbosity=1, overwrite=FALSE)
sapply(res, dim)

# ----
# find the parameters of the maximum values
res_max = lapply(res, function(x) 
  x %>% slice(which.max(mxK)))

devtools::load_all("../occCompaRe")
param_at_limit <- check_parameter_grids(method="binsvm", res=res)
param_at_limit <- check_parameter_grids(method="bsvm", res=res)
param_at_limit <- check_parameter_grids(method="ocsvm", res=res)
param_at_limit <- check_parameter_grids(method="maxent", res=res)

# ----------------------------------------------------------------
# plot grids of border cases 

# BINARYSVM
ggp <- ggp_parameter_grids(method="binsvm", x="C", y="sigma", facet_x=NULL, 
                           res=res, grid_boder_cases_only=TRUE, 
                           print_single_grids=TRUE, verbose=TRUE)

# ----
# BSVM
ggp <- ggp_parameter_grids(method="bsvm", 
                           x="cNeg", y="sigma", facet_x="cMultiplier", 
                           res=res, grid_boder_cases_only=TRUE,
                           print_single_grids=TRUE, 
                           verbose=TRUE)

# ----
# OCSVM
ggp <- ggp_parameter_grids(method="ocsvm", x="nu", y="sigma", facet_x=NULL, 
                           res=res, grid_boder_cases_only=TRUE,
                           ignore=list("nu"="lower"), print_single_grids=TRUE, 
                           verbose=TRUE)

# ----
# MAXENT
ggp <- ggp_parameter_grids(method="maxent", x="beta", y="fc", facet_x=NULL, 
                           res=res, grid_boder_cases_only=TRUE, verbose=TRUE,
                           print_single_grids=TRUE, 
                           ignore=list(fc=c("lower", "upper")))
