validate_inputs <- function(formula, family, data, weights, offset, control,
                            fact4clust, nclust, clustfactnames,
                            start.control, EM.control,
                            save.long, save.ests, save.Qres, save.RQres, save.EQres,
                            verbose) {
    # Check formula
    if (!"formula" %in% class(tryCatch(as.formula(formula), error = function(e) e))) {
        stop("'formula' object must be a formula, or can be coerced to class formula.")
    }

    # Check family
    if (!is.vector(family) || !is.character(family) || length(family) != 1 ||
        !(family %in% c("binomial","gaussian","poisson"))) {
        stop("The 'family' argument must be 'binomial', 'gaussian' or 'poisson'.")
    }

    # Check data
    if (!is.data.frame(data) || ncol(data) < 2 || nrow(data) < 2 || is.null(names(data))) {
        stop("'data' object must be a data frame with at least 2 named columns, and at least 2 rows.")
    }

    # Check control
    if (!is.null(control)) {
        if (!is.list(control)) stop("The 'control' argument must be a list, with entries from 'epsilon', 'maxit' and 'trace'.")
        if (length(control) > 1 && !all(names(control) %in% c("epsilon","maxit","trace"))) {
            stop("The 'control' argument must be a list, with entries from 'epsilon', 'maxit' and 'trace'.")
        }
        if (!is.null(control$epsilon) &&
            (!is.numeric(control$epsilon) || !is.vector(control$epsilon) ||
             length(control$epsilon) != 1 || control$epsilon <= 0)) {
            stop("control$epsilon must be a single positive value.")
        }
        if (!is.null(control$maxit) &&
            (!is.numeric(control$maxit) || !is.vector(control$maxit) ||
             length(control$maxit) != 1 || control$maxit %% 1 != 0 ||
             control$maxit < 1))
            stop("control$maxit must be a positive integer.")
        if (!is.null(control$trace) &&
            (!is.numeric(control$trace) || !is.vector(control$trace) ||
             length(control$trace) != 1 || control$trace %% 1 != 0 ||
             control$trace < 0))
            stop("control$trace must be TRUE or FALSE, or a non-negative integer (0 corresponds to FALSE, positive values correspond to TRUE).")
    }

    # Check clustering specification
    if (!is.null(fact4clust) &&
        (!is.vector(fact4clust) || !is.character(fact4clust) || length(family) <= 0 )) {
        stop("'fact4clust' must be a vector of strings giving the names of variables in the dataset that should be clustered.")
    }
    if (!is.null(nclust) &&
        (!is.numeric(nclust) || !is.vector(nclust) || length(nclust) < 1 ||
         !all(nclust %% 1 == 0) || !all(nclust >= 2)))
        stop("nclust must be a vector of positive integers (2 or larger).")
    if (length(nclust) != length(fact4clust)) stop("nclust must be the same length as 'fact4clust'.")
    if (!is.null(clustfactnames) &&
        (!is.vector(clustfactnames) || !is.character(clustfactnames) ||
         length(clustfactnames) != length(fact4clust) )) {
        stop("'clustfactnames' must be a vector of strings the same length as 'fact4clust'.")
    }

    # Check start.control
    if (!is.null(start.control)) {
        if (!is.list(start.control) || is.null(names(start.control)) ||
            !(all(names(start.control) %in% c("randstarts","selfstarts","alloc")))) {
            stop("The 'start.control' argument must be a list, with entries from 'randstarts', 'selfstarts' and 'alloc'. Please see the documentation for more info.")
        }
        if (!is.null(start.control$randstarts) &&
            (!is.numeric(start.control$randstarts) || !is.vector(start.control$randstarts) ||
             length(start.control$randstarts) != 1 || start.control$randstarts %% 1 != 0 ||
             start.control$randstarts < 0))
            stop("start.control$randstarts must be a non-negative integer.")
        if (!is.null(start.control$selfstarts) &&
            (!is.numeric(start.control$selfstarts) || !is.vector(start.control$selfstarts) ||
             length(start.control$selfstarts) != 1 || start.control$selfstarts %% 1 != 0 ||
             start.control$selfstarts < 0))
            stop("start.control$selfstarts must be a non-negative integer.")
        if (!is.null(start.control$alloc) & !is.matrix(start.control$alloc) & !is.list(start.control$alloc)) {
            stop("start.control$alloc must be a list (for row/column clustering) or a matrix (for biclustering).")
        }
    }

    # Check EM.control
    if (!is.null(EM.control)) {
        if (!is.list(EM.control) || is.null(names(EM.control)) ||
            !all((names(EM.control) %in% c("maxEMcycles","EMstoppingpar")))) {
            stop("The 'EM.control' argument must be a list, with entries from 'maxEMcycles' and 'EMstoppingpar.")
        }
        if (!is.null(EM.control$maxEMcycles) &&
            (!is.numeric(EM.control$maxEMcycles) || !is.vector(EM.control$maxEMcycles) ||
             length(EM.control$maxEMcycles) != 1 || EM.control$maxEMcycles %% 1 != 0 ||
             EM.control$maxEMcycles < 1))
            stop("EM.control$maxEMcycles must be a positive integer.")
        if (!is.null(EM.control$EMstoppingpar) &&
            (!is.numeric(EM.control$EMstoppingpar) || !is.vector(EM.control$EMstoppingpar) ||
             length(EM.control$EMstoppingpar) != 1 || EM.control$EMstoppingpar <= 0))
            stop("EM.control$EMstoppingpar must be a single positive value.")
    }

    # Check SAVE options
    if (!is.vector(save.long) || length(save.long) != 1 || !(save.long %in% c(TRUE,FALSE))) {
        stop("save.long must be TRUE or FALSE.")
    }
    if (!is.vector(save.ests) || length(save.ests) != 1 || !(save.ests %in% c(TRUE,FALSE))) {
        stop("save.ests must be TRUE or FALSE.")
    }
    if (!is.vector(save.Qres) || length(save.Qres) != 1 || !(save.Qres %in% c(TRUE,FALSE))) {
        stop("save.Qres must be TRUE or FALSE.")
    }
    if (!is.vector(save.RQres) || length(save.RQres) != 1 || !(save.RQres %in% c(TRUE,FALSE))) {
        stop("save.RQres must be TRUE or FALSE.")
    }
    if (!is.vector(save.EQres) || length(save.EQres) != 1 || !(save.EQres %in% c(TRUE,FALSE))) {
        stop("save.EQres must be TRUE or FALSE.")
    }
    if (!is.numeric(verbose) || !is.vector(verbose) || length(verbose) != 1 ||
        verbose %% 1 != 0 || verbose < 0) {
        stop("verbose must be an integer, 0 or 1 or 2.")
    }
}