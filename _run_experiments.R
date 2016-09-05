# devtools::load_all("../occCompaRe")
# devtools::load_all(".")

devtools::load_all(".")
devtools::load_all("../occCompaReExp")

# require(occCompare)
# require(occCompareExp)

parcc <- occCompareExp::set_expOpts("revision1", grid="b")

run_experiments(parcc=parcc, 
                fun=c(".trainUtest", 
                      ".resampling-val"), doPar=FALSE)


# ----
add_thVals2resTable()

res <- load_results(c(".trainUtest", ".resampling"),
                    verbosity=1, overwrite=FALSE)
lapply(res, colnames)
