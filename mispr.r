function (x, x.select = FALSE, pen = FALSE, maxit = 5, m = 5, 
    track = FALSE, init.method = "random", L2.fix = NULL, cv = TRUE, 
    maxL2 = 2^10) 
{
    if (!is.data.frame(x)) {
        if (is.matrix(x)) 
            x <- as.data.frame(x)
        else stop("data frame must be provided")
    }
    if (!is.null(L2.fix)) {
        if (L2.fix < 0 | !is.numeric(L2.fix)) 
            stop("L2 penalty must be non.negative value")
    }
    if (!is.null(L2.fix) & cv == TRUE) {
        cat("cv option is set automatically to FALSE  since fixed value of L2 penalty is given")
        cv <- FALSE
    }
    types <- lapply(x, class1)
    attributes(types)$names <- NULL
    types <- unlist(types)
    if (any(types == "character")) {
        chrInd <- which(types == "character")
        warning("At least one character variable is converted into a factor")
        for (ind in chrInd) {
            x[, ind] <- as.factor(x[, ind])
            types[ind] <- "factor"
        }
    }
    indFac <- which(types == "factor")
    for (ind in indFac) {
        if (length(levels(x[, ind])) == 2) 
            types[ind] <- "binary"
        else if (length(levels(x[, ind])) > 2) 
            stop("factor with more than 2 levels detected and has not yet implemented!")
        else stop("factor with less than 2 levels detected!")
    }
    missingSummary <- data.frame(types, apply(x, 2, function(x) sum(is.na(x))))
    colnames(missingSummary) <- c("type", "#missing")
    N <- n <- dim(x)[1]
    P <- dim(x)[2]
    if (dim(x)[2] < 2) 
        stop("Less than 2 variables included in x.")
    if (!any(is.na(x))) 
        print("No missings in x. Nothing to impute")
    if (any(apply(x, 1, function(x) all(is.na(x))))) 
        stop("Unit non-responses included in x.")
    factors <- vector()
    for (i in 1:ncol(x)) {
        factors <- c(factors, is.factor(x[, i]))
    }
    x.na <- vector()
    for (i in seq(P)) {
        if (na.exist(x[, i])) 
            x.na <- c(x.na, i)
    }
    w2 <- is.na(x)
    if (requireNamespace("e1071")) 
        if (requireNamespace("MASS")) 
            if (requireNamespace("penalized")) 
                res = list()
    res$m = m
    res$iteration = maxit
    res$imputation <- list()
    res$missingSummary <- missingSummary
    if (m > 0) {
        data.input = x
        for (imp in 1:m) {
            x = data.input
            if (init.method == "median") {
                x.init <- initialise(x, method = "median")
            }
            else if (init.method == "random") {
                x.init <- initialise(x, method = "random")
            }
            else {
                stop("The value of init.method is misspecified, please choose any of two options i.e., random or median")
            }
            x = x.init
            rm(x.init)
            if (track) 
                print(head(x))
            for (iteration in 1:maxit) {
                for (i in x.na) {
                  if (track) {
                    print(paste("iteration ", iteration, "; imputed dataset", 
                      imp))
                  }
                  yPart <- x[, i, drop = FALSE]
                  wy <- which(w2[, i])
                  xPart <- x[, -i, drop = FALSE]
                  dataForReg <- data.frame(yPart, xPart)
                  if (types[i] == "numeric") {
                    meth = "numeric"
                  }
                  else if (types[i] == "binary") {
                    meth = "bin"
                  }
                  if (length(wy) > 0) {
                    if (track) {
                      if (meth == "bin") 
                        print(paste("fitting model with ", colnames(dataForReg)[1], 
                          " as a binary response y.imp"))
                      else print(paste("fitting model with ", 
                        colnames(dataForReg)[1], " as a continuous response y.imp"))
                    }
                    colnames(dataForReg)[1] <- "y.imp"
                    x[wy, i] <- lm.glm(xReg = dataForReg, x.select = x.select, 
                      pen = pen, index = wy, type = meth, L2.fix = L2.fix, 
                      cv = cv, maxL2 = maxL2, track = track)
                  }
                }
            }
            res$imputation[[imp]] <- x
        }
    }
    else {
        stop("value of m should be positive")
    }
    return(res)
}