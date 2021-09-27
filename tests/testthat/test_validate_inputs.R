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
    expect_error(clustglm(counts ~ species, "poisson", whit.df, control=list()), "The 'control' argument must be a list, with entries from 'epsilon', 'maxit' and 'trace'.")
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
    expect_error(clustglm(counts ~ species, "poisson", whit.df, fact4clust="species", nclust=1), "nclust must be a vector of positive integers (2 or larger).")
    expect_error(clustglm(counts ~ species, "poisson", whit.df, fact4clust="species", nclust=0), "nclust must be a vector of positive integers (2 or larger).")
    expect_error(clustglm(counts ~ species, "poisson", whit.df, fact4clust="species", nclust=-2), "nclust must be a vector of positive integers (2 or larger).")
    expect_error(clustglm(counts ~ species, "poisson", whit.df, fact4clust="species", nclust=2.4), "nclust must be a vector of positive integers (2 or larger).")
    expect_error(clustglm(counts ~ species, "poisson", whit.df, fact4clust="species", nclust=3:19), "nclust must be a vector of positive integers (2 or larger).")
    expect_error(clustglm(counts ~ species, "poisson", whit.df, fact4clust="species", nclust=-2.2:-6.2), "nclust must be a vector of positive integers (2 or larger).")
    expect_error(clustglm(counts ~ species, "poisson", whit.df, fact4clust="species", nclust=matrix(1:9, nrow=3)), "nclust must be a vector of positive integers (2 or larger).")
    expect_error(clustglm(counts ~ species, "poisson", whit.df, fact4clust="species", nclust=data.frame(x=1:5,y=1:5)), "nclust must be a vector of positive integers (2 or larger).")
    expect_error(clustglm(counts ~ species, "poisson", whit.df, fact4clust="species", nclust=list(a=1, b="c")), "nclust must be a vector of positive integers (2 or larger).")
    expect_error(clustglm(counts ~ species, "poisson", whit.df, fact4clust="species", nclust=TRUE), "nclust must be a vector of positive integers (2 or larger).")
})

test_that("clustglm fails for an invalid clustfactnames", {
    expect_error(clustglm(counts ~ species, "poisson", whit.df, fact4clust="species", clustfactnames=c("test","try")), "'clustfactnames' must be a vector of strings the same length as 'fact4clust'.")
    expect_error(clustglm(counts ~ species, "poisson", whit.df, fact4clust="species", clustfactnames=2), "'clustfactnames' must be a vector of strings the same length as 'fact4clust'.")
    expect_error(clustglm(counts ~ species, "poisson", whit.df, fact4clust="species", clustfactnames=3:19), "'clustfactnames' must be a vector of strings the same length as 'fact4clust'.")
    expect_error(clustglm(counts ~ species, "poisson", whit.df, fact4clust="species", clustfactnames=-2.2:-6.2), "'clustfactnames' must be a vector of strings the same length as 'fact4clust'.")
    expect_error(clustglm(counts ~ species, "poisson", whit.df, fact4clust="species", clustfactnames=matrix(1:9, nrow=3)), "'clustfactnames' must be a vector of strings the same length as 'fact4clust'.")
    expect_error(clustglm(counts ~ species, "poisson", whit.df, fact4clust="species", clustfactnames=data.frame(x=1:5,y=1:5)), "'clustfactnames' must be a vector of strings the same length as 'fact4clust'.")
    expect_error(clustglm(counts ~ species, "poisson", whit.df, fact4clust="species", clustfactnames=list(a=1, b="c")), "'clustfactnames' must be a vector of strings the same length as 'fact4clust'.")
    expect_error(clustglm(counts ~ species, "poisson", whit.df, fact4clust="species", clustfactnames=TRUE), "'clustfactnames' must be a vector of strings the same length as 'fact4clust'.")
})
#
# test_that("clustglm fails for an invalid start.control", {
#
# })
#
# test_that("clustglm fails for an invalid EM.control", {
#
# })
#
# test_that("clustglm fails for an invalid save.long", {
#
# })
#
# test_that("clustglm fails for an invalid save.ests", {
#
# })
#
# test_that("clustglm fails for an invalid save.Qres", {
#
# })
#
# test_that("clustglm fails for an invalid save.RQres", {
#
# })
#
# test_that("clustglm fails for an invalid save.EQres", {
#
#
# })
#
# test_that("clustglm fails for an invalid verbose", {
#
# })
