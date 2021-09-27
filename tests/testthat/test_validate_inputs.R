## NOTE about testing: "throws_error" takes as its argument a REGULAR EXPRESSION,
## not a simple string, so need to escape special regex characters e.g "\\$"
## instead of "$".

## NOTE that the testthat testing commands will automatically run the code in
## tests/testthat/helper-functions.R before running any of the tests.

test_that("clustglm fails for an invalid formula", {
    expect_error(clustglm(), "'formula' object must be a formula, or can be coerced to class formula.")
    expect_error(clustglm(2), "'formula' object must be a formula, or can be coerced to class formula.")
    expect_error(clustglm("test"), "'formula' object must be a formula, or can be coerced to class formula.")
    expect_error(clustglm(3:19), "'formula' object must be a formula, or can be coerced to class formula.")
    expect_error(clustglm(-2.2:-6.2), "'formula' object must be a formula, or can be coerced to class formula.")
    expect_error(clustglm(matrix(1:9, nrow=3)), "'formula' object must be a formula, or can be coerced to class formula.")
    expect_error(clustglm(list(a=1, b="c")), "'formula' object must be a formula, or can be coerced to class formula.")
    expect_error(clustglm(TRUE), "'formula' object must be a formula, or can be coerced to class formula.")

    # Note that I haven't included a test for 'formula' being a data frame because a data frame can be coerced into class formula.

    # expect_silent(clustglm(counts ~ site,family="poisson",data=whit.df))
})

test_that("clustglm fails for an invalid family", {
    expect_error(clustglm(counts ~ species, "exponential"), "The 'family' argument must be 'binomial', 'gaussian' or 'poisson'.")
    expect_error(clustglm(counts ~ species, c("binomial","gaussian")), "The 'family' argument must be 'binomial', 'gaussian' or 'poisson'.")
    expect_error(clustglm(counts ~ species, 2), "The 'family' argument must be 'binomial', 'gaussian' or 'poisson'.")
    expect_error(clustglm(counts ~ species, 3:19), "The 'family' argument must be 'binomial', 'gaussian' or 'poisson'.")
    expect_error(clustglm(counts ~ species, -2.2:-6.2), "The 'family' argument must be 'binomial', 'gaussian' or 'poisson'.")
    expect_error(clustglm(counts ~ species, matrix(1:9, nrow=3)), "The 'family' argument must be 'binomial', 'gaussian' or 'poisson'.")
    expect_error(clustglm(counts ~ species, data.frame(x=1:5,y=1:5)), "The 'family' argument must be 'binomial', 'gaussian' or 'poisson'.")
    expect_error(clustglm(counts ~ species, list(a=1, b="c")), "The 'family' argument must be 'binomial', 'gaussian' or 'poisson'.")
    expect_error(clustglm(counts ~ species, TRUE), "The 'family' argument must be 'binomial', 'gaussian' or 'poisson'.")
    expect_error(clustglm(counts ~ species), "The 'family' argument must be 'binomial', 'gaussian' or 'poisson'.")
})

test_that("clustglm fails for an invalid data object", {
    expect_error(clustglm(counts ~ species, "poisson"), 'argument "data" is missing, with no default')

    expect_error(clustglm(counts ~ species, "poisson", "test"), "'data' object must be a data frame with at least 2 named columns, and at least 2 rows.")
    expect_error(clustglm(counts ~ species, "poisson", c("test","try")), "'data' object must be a data frame with at least 2 named columns, and at least 2 rows.")
    expect_error(clustglm(counts ~ species, "poisson", 2), "'data' object must be a data frame with at least 2 named columns, and at least 2 rows.")
    expect_error(clustglm(counts ~ species, "poisson", 3:19), "'data' object must be a data frame with at least 2 named columns, and at least 2 rows.")
    expect_error(clustglm(counts ~ species, "poisson", -2.2:-6.2), "'data' object must be a data frame with at least 2 named columns, and at least 2 rows.")
    expect_error(clustglm(counts ~ species, "poisson", matrix(1:9, nrow=3)), "'data' object must be a data frame with at least 2 named columns, and at least 2 rows.")
    expect_error(clustglm(counts ~ species, "poisson", list(a=1, b="c")), "'data' object must be a data frame with at least 2 named columns, and at least 2 rows.")
    expect_error(clustglm(counts ~ species, "poisson", TRUE), "'data' object must be a data frame with at least 2 named columns, and at least 2 rows.")

    expect_error(clustglm(counts ~ species, "poisson", data.frame(x=1,y=1)), "'data' object must be a data frame with at least 2 named columns, and at least 2 rows.")
})

test_that("clustglm fails for an invalid control object", {
    expect_error(clustglm(counts ~ species, "poisson", whit.df, control=list(x=1,epsilon=2)), "The 'control' argument must be a list, with entries from 'epsilon', 'maxit' and 'trace'.")

    expect_error(clustglm(counts ~ species, "poisson", whit.df, control="test"), "The 'control' argument must be a list, with entries from 'epsilon', 'maxit' and 'trace'.")
    expect_error(clustglm(counts ~ species, "poisson", whit.df, control=c("test","try")), "The 'control' argument must be a list, with entries from 'epsilon', 'maxit' and 'trace'.")
    expect_error(clustglm(counts ~ species, "poisson", whit.df, control=2), "The 'control' argument must be a list, with entries from 'epsilon', 'maxit' and 'trace'.")
    expect_error(clustglm(counts ~ species, "poisson", whit.df, control=3:19), "The 'control' argument must be a list, with entries from 'epsilon', 'maxit' and 'trace'.")
    expect_error(clustglm(counts ~ species, "poisson", whit.df, control=-2.2:-6.2), "The 'control' argument must be a list, with entries from 'epsilon', 'maxit' and 'trace'.")
    expect_error(clustglm(counts ~ species, "poisson", whit.df, control=matrix(1:9, nrow=3)), "The 'control' argument must be a list, with entries from 'epsilon', 'maxit' and 'trace'.")
    expect_error(clustglm(counts ~ species, "poisson", whit.df, control=data.frame(x=1:5,y=1:5)), "The 'control' argument must be a list, with entries from 'epsilon', 'maxit' and 'trace'.")
    expect_error(clustglm(counts ~ species, "poisson", whit.df, control=TRUE), "The 'control' argument must be a list, with entries from 'epsilon', 'maxit' and 'trace'.")

    # Use 'fixed=TRUE' argument to expect_error() so that the test doesn't misread 'control$...'

    expect_error(clustglm(counts ~ species, "poisson", whit.df, control=list(epsilon=-2e-5)), "control$epsilon must be a single positive value.", fixed=TRUE)
    expect_error(clustglm(counts ~ species, "poisson", whit.df, control=list(epsilon=c(1e-4,1e-5))), "control$epsilon must be a single positive value.", fixed=TRUE)
    expect_error(clustglm(counts ~ species, "poisson", whit.df, control=list(epsilon="test")), "control$epsilon must be a single positive value.", fixed=TRUE)
    expect_error(clustglm(counts ~ species, "poisson", whit.df, control=list(epsilon=c("test","try"))), "control$epsilon must be a single positive value.", fixed=TRUE)
    expect_error(clustglm(counts ~ species, "poisson", whit.df, control=list(epsilon=3:19)), "control$epsilon must be a single positive value.", fixed=TRUE)
    expect_error(clustglm(counts ~ species, "poisson", whit.df, control=list(epsilon=-2.2:-6.2)), "control$epsilon must be a single positive value.", fixed=TRUE)
    expect_error(clustglm(counts ~ species, "poisson", whit.df, control=list(epsilon=matrix(1:9, nrow=3))), "control$epsilon must be a single positive value.", fixed=TRUE)
    expect_error(clustglm(counts ~ species, "poisson", whit.df, control=list(epsilon=data.frame(x=1:5,y=1:5))), "control$epsilon must be a single positive value.", fixed=TRUE)
    expect_error(clustglm(counts ~ species, "poisson", whit.df, control=list(epsilon=list(a=1, b="c"))), "control$epsilon must be a single positive value.", fixed=TRUE)
    expect_error(clustglm(counts ~ species, "poisson", whit.df, control=list(epsilon=TRUE)), "control$epsilon must be a single positive value.", fixed=TRUE)

    expect_error(clustglm(counts ~ species, "poisson", whit.df, control=list(maxit=1.1)), "control$maxit must be a positive integer.", fixed=TRUE)
    expect_error(clustglm(counts ~ species, "poisson", whit.df, control=list(maxit=-2)), "control$maxit must be a positive integer.", fixed=TRUE)
    expect_error(clustglm(counts ~ species, "poisson", whit.df, control=list(maxit=c(1e-4,1e-5))), "control$maxit must be a positive integer.", fixed=TRUE)
    expect_error(clustglm(counts ~ species, "poisson", whit.df, control=list(maxit="test")), "control$maxit must be a positive integer.", fixed=TRUE)
    expect_error(clustglm(counts ~ species, "poisson", whit.df, control=list(maxit=c("test","try"))), "control$maxit must be a positive integer.", fixed=TRUE)
    expect_error(clustglm(counts ~ species, "poisson", whit.df, control=list(maxit=3:19)), "control$maxit must be a positive integer.", fixed=TRUE)
    expect_error(clustglm(counts ~ species, "poisson", whit.df, control=list(maxit=-2.2:-6.2)), "control$maxit must be a positive integer.", fixed=TRUE)
    expect_error(clustglm(counts ~ species, "poisson", whit.df, control=list(maxit=matrix(1:9, nrow=3))), "control$maxit must be a positive integer.", fixed=TRUE)
    expect_error(clustglm(counts ~ species, "poisson", whit.df, control=list(maxit=data.frame(x=1:5,y=1:5))), "control$maxit must be a positive integer.", fixed=TRUE)
    expect_error(clustglm(counts ~ species, "poisson", whit.df, control=list(maxit=list(a=1, b="c"))), "control$maxit must be a positive integer.", fixed=TRUE)
    expect_error(clustglm(counts ~ species, "poisson", whit.df, control=list(maxit=TRUE)), "control$maxit must be a positive integer.", fixed=TRUE)

    expect_error(clustglm(counts ~ species, "poisson", whit.df, control=list(trace=-2)), "control$trace must be TRUE or FALSE, or a non-negative integer (0 corresponds to FALSE, positive values correspond to TRUE).", fixed=TRUE)
    expect_error(clustglm(counts ~ species, "poisson", whit.df, control=list(trace=-2e-5)), "control$trace must be TRUE or FALSE, or a non-negative integer (0 corresponds to FALSE, positive values correspond to TRUE).", fixed=TRUE)
    expect_error(clustglm(counts ~ species, "poisson", whit.df, control=list(trace=c(1e-4,1e-5))), "control$trace must be TRUE or FALSE, or a non-negative integer (0 corresponds to FALSE, positive values correspond to TRUE).", fixed=TRUE)
    expect_error(clustglm(counts ~ species, "poisson", whit.df, control=list(trace="test")), "control$trace must be TRUE or FALSE, or a non-negative integer (0 corresponds to FALSE, positive values correspond to TRUE).", fixed=TRUE)
    expect_error(clustglm(counts ~ species, "poisson", whit.df, control=list(trace=c("test","try"))), "control$trace must be TRUE or FALSE, or a non-negative integer (0 corresponds to FALSE, positive values correspond to TRUE).", fixed=TRUE)
    expect_error(clustglm(counts ~ species, "poisson", whit.df, control=list(trace=3:19)), "control$trace must be TRUE or FALSE, or a non-negative integer (0 corresponds to FALSE, positive values correspond to TRUE).", fixed=TRUE)
    expect_error(clustglm(counts ~ species, "poisson", whit.df, control=list(trace=-2.2:-6.2)), "control$trace must be TRUE or FALSE, or a non-negative integer (0 corresponds to FALSE, positive values correspond to TRUE).", fixed=TRUE)
    expect_error(clustglm(counts ~ species, "poisson", whit.df, control=list(trace=matrix(1:9, nrow=3))), "control$trace must be TRUE or FALSE, or a non-negative integer (0 corresponds to FALSE, positive values correspond to TRUE).", fixed=TRUE)
    expect_error(clustglm(counts ~ species, "poisson", whit.df, control=list(trace=data.frame(x=1:5,y=1:5))), "control$trace must be TRUE or FALSE, or a non-negative integer (0 corresponds to FALSE, positive values correspond to TRUE).", fixed=TRUE)
    expect_error(clustglm(counts ~ species, "poisson", whit.df, control=list(trace=list(a=1, b="c"))), "control$trace must be TRUE or FALSE, or a non-negative integer (0 corresponds to FALSE, positive values correspond to TRUE).", fixed=TRUE)
})

test_that("clustglm fails for an invalid fact4clust", {
    expect_error(clustglm(counts ~ species, "poisson", whit.df, fact4clust=2), "'fact4clust' must be a vector of strings giving the names of variables in the dataset that should be clustered.")
    expect_error(clustglm(counts ~ species, "poisson", whit.df, fact4clust=3:19), "'fact4clust' must be a vector of strings giving the names of variables in the dataset that should be clustered.")
    expect_error(clustglm(counts ~ species, "poisson", whit.df, fact4clust=-2.2:-6.2), "'fact4clust' must be a vector of strings giving the names of variables in the dataset that should be clustered.")
    expect_error(clustglm(counts ~ species, "poisson", whit.df, fact4clust=matrix(1:9, nrow=3)), "'fact4clust' must be a vector of strings giving the names of variables in the dataset that should be clustered.")
    expect_error(clustglm(counts ~ species, "poisson", whit.df, fact4clust=data.frame(x=1:5,y=1:5)), "'fact4clust' must be a vector of strings giving the names of variables in the dataset that should be clustered.")
    expect_error(clustglm(counts ~ species, "poisson", whit.df, fact4clust=list(a=1, b="c")), "'fact4clust' must be a vector of strings giving the names of variables in the dataset that should be clustered.")
    expect_error(clustglm(counts ~ species, "poisson", whit.df, fact4clust=TRUE), "'fact4clust' must be a vector of strings giving the names of variables in the dataset that should be clustered.")
})

test_that("clustglm fails for an invalid nclust", {
    expect_error(clustglm(counts ~ species, "poisson", whit.df, fact4clust="species", nclust=1), "nclust must be a vector of positive integers (2 or larger).", fixed=TRUE)
    expect_error(clustglm(counts ~ species, "poisson", whit.df, fact4clust="species", nclust=0), "nclust must be a vector of positive integers (2 or larger).", fixed=TRUE)
    expect_error(clustglm(counts ~ species, "poisson", whit.df, fact4clust="species", nclust=-2), "nclust must be a vector of positive integers (2 or larger).", fixed=TRUE)
    expect_error(clustglm(counts ~ species, "poisson", whit.df, fact4clust="species", nclust=2.4), "nclust must be a vector of positive integers (2 or larger).", fixed=TRUE)
    expect_error(clustglm(counts ~ species, "poisson", whit.df, fact4clust="species", nclust=-2.2:-6.2), "nclust must be a vector of positive integers (2 or larger).", fixed=TRUE)
    expect_error(clustglm(counts ~ species, "poisson", whit.df, fact4clust="species", nclust=matrix(1:9, nrow=3)), "nclust must be a vector of positive integers (2 or larger).", fixed=TRUE)
    expect_error(clustglm(counts ~ species, "poisson", whit.df, fact4clust="species", nclust=data.frame(x=1:5,y=1:5)), "nclust must be a vector of positive integers (2 or larger).", fixed=TRUE)
    expect_error(clustglm(counts ~ species, "poisson", whit.df, fact4clust="species", nclust=list(a=1, b="c")), "nclust must be a vector of positive integers (2 or larger).", fixed=TRUE)
    expect_error(clustglm(counts ~ species, "poisson", whit.df, fact4clust="species", nclust=TRUE), "nclust must be a vector of positive integers (2 or larger).", fixed=TRUE)
    
    expect_error(clustglm(counts ~ species, "poisson", whit.df, fact4clust="species", nclust=3:19), "nclust must be the same length as 'fact4clust'.", fixed=TRUE)
})


test_that("clustglm fails for an invalid clustfactnames", {
    expect_error(clustglm(counts ~ species, "poisson", whit.df, fact4clust="species", nclust=2, clustfactnames=c("test","try")), "'clustfactnames' must be a vector of strings the same length as 'fact4clust'.")
    expect_error(clustglm(counts ~ species, "poisson", whit.df, fact4clust="species", nclust=2, clustfactnames=2), "'clustfactnames' must be a vector of strings the same length as 'fact4clust'.")
    expect_error(clustglm(counts ~ species, "poisson", whit.df, fact4clust="species", nclust=2, clustfactnames=3:19), "'clustfactnames' must be a vector of strings the same length as 'fact4clust'.")
    expect_error(clustglm(counts ~ species, "poisson", whit.df, fact4clust="species", nclust=2, clustfactnames=-2.2:-6.2), "'clustfactnames' must be a vector of strings the same length as 'fact4clust'.")
    expect_error(clustglm(counts ~ species, "poisson", whit.df, fact4clust="species", nclust=2, clustfactnames=matrix(1:9, nrow=3)), "'clustfactnames' must be a vector of strings the same length as 'fact4clust'.")
    expect_error(clustglm(counts ~ species, "poisson", whit.df, fact4clust="species", nclust=2, clustfactnames=data.frame(x=1:5,y=1:5)), "'clustfactnames' must be a vector of strings the same length as 'fact4clust'.")
    expect_error(clustglm(counts ~ species, "poisson", whit.df, fact4clust="species", nclust=2, clustfactnames=list(a=1, b="c")), "'clustfactnames' must be a vector of strings the same length as 'fact4clust'.")
    expect_error(clustglm(counts ~ species, "poisson", whit.df, fact4clust="species", nclust=2, clustfactnames=TRUE), "'clustfactnames' must be a vector of strings the same length as 'fact4clust'.")
})
#
# test_that("clustglm fails for an invalid start.control", {
#
# })
# test_that("clustglm fails for an invalid EM.control", {
#
# })
test_that("clustglm fails for an invalid save.long", {
    expect_error(clustglm(counts ~ species, "poisson", whit.df, fact4clust="species", 
                          nclust=2, clustfactnames="spClust", save.long="try"), 
                 "save.long must be TRUE or FALSE.")
    expect_error(clustglm(counts ~ species, "poisson", whit.df, fact4clust="species", 
                          nclust=2, clustfactnames="spClust", save.long=c("a","b")), 
                 "save.long must be TRUE or FALSE.")
    expect_error(clustglm(counts ~ species, "poisson", whit.df, fact4clust="species", 
                          nclust=2, clustfactnames="spClust", save.long=2), 
                 "save.long must be TRUE or FALSE.")
    expect_error(clustglm(counts ~ species, "poisson", whit.df, fact4clust="species", 
                          nclust=2, clustfactnames="spClust", save.long=-1), 
                 "save.long must be TRUE or FALSE.")
    expect_error(clustglm(counts ~ species, "poisson", whit.df, fact4clust="species", 
                          nclust=2, clustfactnames="spClust", save.long=0.5), 
                 "save.long must be TRUE or FALSE.")
    expect_error(clustglm(counts ~ species, "poisson", whit.df, fact4clust="species", 
                          nclust=2, clustfactnames="spClust", save.long=3:19), 
                 "save.long must be TRUE or FALSE.")
    expect_error(clustglm(counts ~ species, "poisson", whit.df, fact4clust="species", 
                          nclust=2, clustfactnames="spClust", save.long=-2.2:-6.2), 
                 "save.long must be TRUE or FALSE.")
    expect_error(clustglm(counts ~ species, "poisson", whit.df, fact4clust="species", 
                          nclust=2, clustfactnames="spClust", save.long=matrix(1:9, nrow=3)), 
                 "save.long must be TRUE or FALSE.")
    expect_error(clustglm(counts ~ species, "poisson", whit.df, fact4clust="species", 
                          nclust=2, clustfactnames="spClust", save.long=data.frame(x=1:5,y=1:5)), 
                 "save.long must be TRUE or FALSE.")
    expect_error(clustglm(counts ~ species, "poisson", whit.df, fact4clust="species", 
                          nclust=2, clustfactnames="spClust", save.long=list(a=1, b="c")), 
                 "save.long must be TRUE or FALSE.")
    expect_error(clustglm(counts ~ species, "poisson", whit.df, fact4clust="species", 
                          nclust=2, clustfactnames="spClust", save.long=list(a=TRUE,b=FALSE)), 
                 "save.long must be TRUE or FALSE.")
})
test_that("clustglm fails for an invalid save.ests", {
    expect_error(clustglm(counts ~ species, "poisson", whit.df, fact4clust="species", 
                          nclust=2, clustfactnames="spClust", save.ests="try"), 
                 "save.ests must be TRUE or FALSE.")
    expect_error(clustglm(counts ~ species, "poisson", whit.df, fact4clust="species", 
                          nclust=2, clustfactnames="spClust", save.ests=c("a","b")), 
                 "save.ests must be TRUE or FALSE.")
    expect_error(clustglm(counts ~ species, "poisson", whit.df, fact4clust="species", 
                          nclust=2, clustfactnames="spClust", save.ests=2), 
                 "save.ests must be TRUE or FALSE.")
    expect_error(clustglm(counts ~ species, "poisson", whit.df, fact4clust="species", 
                          nclust=2, clustfactnames="spClust", save.ests=-1), 
                 "save.ests must be TRUE or FALSE.")
    expect_error(clustglm(counts ~ species, "poisson", whit.df, fact4clust="species", 
                          nclust=2, clustfactnames="spClust", save.ests=0.5), 
                 "save.ests must be TRUE or FALSE.")
    expect_error(clustglm(counts ~ species, "poisson", whit.df, fact4clust="species", 
                          nclust=2, clustfactnames="spClust", save.ests=3:19), 
                 "save.ests must be TRUE or FALSE.")
    expect_error(clustglm(counts ~ species, "poisson", whit.df, fact4clust="species", 
                          nclust=2, clustfactnames="spClust", save.ests=-2.2:-6.2), 
                 "save.ests must be TRUE or FALSE.")
    expect_error(clustglm(counts ~ species, "poisson", whit.df, fact4clust="species", 
                          nclust=2, clustfactnames="spClust", save.ests=matrix(1:9, nrow=3)), 
                 "save.ests must be TRUE or FALSE.")
    expect_error(clustglm(counts ~ species, "poisson", whit.df, fact4clust="species", 
                          nclust=2, clustfactnames="spClust", save.ests=data.frame(x=1:5,y=1:5)), 
                 "save.ests must be TRUE or FALSE.")
    expect_error(clustglm(counts ~ species, "poisson", whit.df, fact4clust="species", 
                          nclust=2, clustfactnames="spClust", save.ests=list(a=1, b="c")), 
                 "save.ests must be TRUE or FALSE.")
    expect_error(clustglm(counts ~ species, "poisson", whit.df, fact4clust="species", 
                          nclust=2, clustfactnames="spClust", save.ests=list(a=TRUE,b=FALSE)), 
                 "save.ests must be TRUE or FALSE.")
})
test_that("clustglm fails for an invalid save.Qres", {
    expect_error(clustglm(counts ~ species, "poisson", whit.df, fact4clust="species", 
                          nclust=2, clustfactnames="spClust", save.Qres="try"), 
                 "save.Qres must be TRUE or FALSE.")
    expect_error(clustglm(counts ~ species, "poisson", whit.df, fact4clust="species", 
                          nclust=2, clustfactnames="spClust", save.Qres=c("a","b")), 
                 "save.Qres must be TRUE or FALSE.")
    expect_error(clustglm(counts ~ species, "poisson", whit.df, fact4clust="species", 
                          nclust=2, clustfactnames="spClust", save.Qres=2), 
                 "save.Qres must be TRUE or FALSE.")
    expect_error(clustglm(counts ~ species, "poisson", whit.df, fact4clust="species", 
                          nclust=2, clustfactnames="spClust", save.Qres=-1), 
                 "save.Qres must be TRUE or FALSE.")
    expect_error(clustglm(counts ~ species, "poisson", whit.df, fact4clust="species", 
                          nclust=2, clustfactnames="spClust", save.Qres=0.5), 
                 "save.Qres must be TRUE or FALSE.")
    expect_error(clustglm(counts ~ species, "poisson", whit.df, fact4clust="species", 
                          nclust=2, clustfactnames="spClust", save.Qres=3:19), 
                 "save.Qres must be TRUE or FALSE.")
    expect_error(clustglm(counts ~ species, "poisson", whit.df, fact4clust="species", 
                          nclust=2, clustfactnames="spClust", save.Qres=-2.2:-6.2), 
                 "save.Qres must be TRUE or FALSE.")
    expect_error(clustglm(counts ~ species, "poisson", whit.df, fact4clust="species", 
                          nclust=2, clustfactnames="spClust", save.Qres=matrix(1:9, nrow=3)), 
                 "save.Qres must be TRUE or FALSE.")
    expect_error(clustglm(counts ~ species, "poisson", whit.df, fact4clust="species", 
                          nclust=2, clustfactnames="spClust", save.Qres=data.frame(x=1:5,y=1:5)), 
                 "save.Qres must be TRUE or FALSE.")
    expect_error(clustglm(counts ~ species, "poisson", whit.df, fact4clust="species", 
                          nclust=2, clustfactnames="spClust", save.Qres=list(a=1, b="c")), 
                 "save.Qres must be TRUE or FALSE.")
    expect_error(clustglm(counts ~ species, "poisson", whit.df, fact4clust="species", 
                          nclust=2, clustfactnames="spClust", save.Qres=list(a=TRUE,b=FALSE)), 
                 "save.Qres must be TRUE or FALSE.")
})
test_that("clustglm fails for an invalid save.RQres", {
    expect_error(clustglm(counts ~ species, "poisson", whit.df, fact4clust="species", 
                          nclust=2, clustfactnames="spClust", save.RQres="try"), 
                 "save.RQres must be TRUE or FALSE.")
    expect_error(clustglm(counts ~ species, "poisson", whit.df, fact4clust="species", 
                          nclust=2, clustfactnames="spClust", save.RQres=c("a","b")), 
                 "save.RQres must be TRUE or FALSE.")
    expect_error(clustglm(counts ~ species, "poisson", whit.df, fact4clust="species", 
                          nclust=2, clustfactnames="spClust", save.RQres=2), 
                 "save.RQres must be TRUE or FALSE.")
    expect_error(clustglm(counts ~ species, "poisson", whit.df, fact4clust="species", 
                          nclust=2, clustfactnames="spClust", save.RQres=-1), 
                 "save.RQres must be TRUE or FALSE.")
    expect_error(clustglm(counts ~ species, "poisson", whit.df, fact4clust="species", 
                          nclust=2, clustfactnames="spClust", save.RQres=0.5), 
                 "save.RQres must be TRUE or FALSE.")
    expect_error(clustglm(counts ~ species, "poisson", whit.df, fact4clust="species", 
                          nclust=2, clustfactnames="spClust", save.RQres=3:19), 
                 "save.RQres must be TRUE or FALSE.")
    expect_error(clustglm(counts ~ species, "poisson", whit.df, fact4clust="species", 
                          nclust=2, clustfactnames="spClust", save.RQres=-2.2:-6.2), 
                 "save.RQres must be TRUE or FALSE.")
    expect_error(clustglm(counts ~ species, "poisson", whit.df, fact4clust="species", 
                          nclust=2, clustfactnames="spClust", save.RQres=matrix(1:9, nrow=3)), 
                 "save.RQres must be TRUE or FALSE.")
    expect_error(clustglm(counts ~ species, "poisson", whit.df, fact4clust="species", 
                          nclust=2, clustfactnames="spClust", save.RQres=data.frame(x=1:5,y=1:5)), 
                 "save.RQres must be TRUE or FALSE.")
    expect_error(clustglm(counts ~ species, "poisson", whit.df, fact4clust="species", 
                          nclust=2, clustfactnames="spClust", save.RQres=list(a=1, b="c")), 
                 "save.RQres must be TRUE or FALSE.")
    expect_error(clustglm(counts ~ species, "poisson", whit.df, fact4clust="species", 
                          nclust=2, clustfactnames="spClust", save.RQres=list(a=TRUE,b=FALSE)), 
                 "save.RQres must be TRUE or FALSE.")
})
test_that("clustglm fails for an invalid save.EQres", {
    expect_error(clustglm(counts ~ species, "poisson", whit.df, fact4clust="species", 
                          nclust=2, clustfactnames="spClust", save.EQres="try"), 
                 "save.EQres must be TRUE or FALSE.")
    expect_error(clustglm(counts ~ species, "poisson", whit.df, fact4clust="species", 
                          nclust=2, clustfactnames="spClust", save.EQres=c("a","b")), 
                 "save.EQres must be TRUE or FALSE.")
    expect_error(clustglm(counts ~ species, "poisson", whit.df, fact4clust="species", 
                          nclust=2, clustfactnames="spClust", save.EQres=2), 
                 "save.EQres must be TRUE or FALSE.")
    expect_error(clustglm(counts ~ species, "poisson", whit.df, fact4clust="species", 
                          nclust=2, clustfactnames="spClust", save.EQres=-1), 
                 "save.EQres must be TRUE or FALSE.")
    expect_error(clustglm(counts ~ species, "poisson", whit.df, fact4clust="species", 
                          nclust=2, clustfactnames="spClust", save.EQres=0.5), 
                 "save.EQres must be TRUE or FALSE.")
    expect_error(clustglm(counts ~ species, "poisson", whit.df, fact4clust="species", 
                          nclust=2, clustfactnames="spClust", save.EQres=3:19), 
                 "save.EQres must be TRUE or FALSE.")
    expect_error(clustglm(counts ~ species, "poisson", whit.df, fact4clust="species", 
                          nclust=2, clustfactnames="spClust", save.EQres=-2.2:-6.2), 
                 "save.EQres must be TRUE or FALSE.")
    expect_error(clustglm(counts ~ species, "poisson", whit.df, fact4clust="species", 
                          nclust=2, clustfactnames="spClust", save.EQres=matrix(1:9, nrow=3)), 
                 "save.EQres must be TRUE or FALSE.")
    expect_error(clustglm(counts ~ species, "poisson", whit.df, fact4clust="species", 
                          nclust=2, clustfactnames="spClust", save.EQres=data.frame(x=1:5,y=1:5)), 
                 "save.EQres must be TRUE or FALSE.")
    expect_error(clustglm(counts ~ species, "poisson", whit.df, fact4clust="species", 
                          nclust=2, clustfactnames="spClust", save.EQres=list(a=1, b="c")), 
                 "save.EQres must be TRUE or FALSE.")
    expect_error(clustglm(counts ~ species, "poisson", whit.df, fact4clust="species", 
                          nclust=2, clustfactnames="spClust", save.EQres=list(a=TRUE,b=FALSE)), 
                 "save.EQres must be TRUE or FALSE.")
})

