test_that("clustglm poisson count-data clustering works as expected from specified inputs", {
    ## Difficult to test clustglm if it is allowed to generate random starts,
    ## so instead testing whether if it's given the cluster membership allocation
    ## from a previous run, it still gets back to the same allocation
    set.seed(1)
    r2.out <- clustglm(formula = counts ~ species + site + speciesClust:site,
                       family = "poisson",
                       data = whit.df,
                       fact4clust = "species", nclus = 2,
                       clustfactnames = "speciesClust",
                       start.control = list(randstarts = 100),
                       verbose = 1)
    
    alloc <- round(r2.out$pp)
    
    r2.out2 <- clustglm(formula = counts ~ species + site + speciesClust:site,
                        family = "poisson",
                        data = whit.df,
                        fact4clust = "species", nclus = 2,
                        clustfactnames = "speciesClust",
                        start.control = list(alloc=alloc),
                        verbose = 1)

    expect_equal(round(r2.out2$pp),alloc)
})