wordfish <- function(wfm,
                     dir=c(1, 10),
                     control=list(tol=1e-06, sigma=3,
                     startparams=NULL), verbose=FALSE){

    thecall <- match.call()

    if (!is.wfm(wfm))
        stop("First argument must be an object of type wfm")

    ## strip out any words that for whatever reason do not occur in any document
    wm <- wordmargin(wfm)
    origV <- length(words(wfm))
    uninformative <- apply(wfm, wm, sum) == 0
    if (sum(uninformative)>0 & verbose)
        cat('Ejecting words that never occur: ', paste((1:origV)[uninformative], sep=','), '\n')

    good.words <- words(wfm)[!uninformative] ## not indices, words
    if (wm==1)
        wfm <- wfm[good.words,]
    else
        wfm <- wfm[,good.words]

    Y <- as.worddoc(wfm)
    tY <- as.docword(wfm)
    D  <- NROW(tY)
    V  <- NCOL(tY)
    wfm <- NULL   ## two copies are quite enough

    require(numDeriv) ## for the standard errors

    ## enforce control defaults
    tol <- ifelse(is.null(control$tol), 1e-06, control$tol)
    sigma <- ifelse(is.null(control$sigma), 3, control$sigma)

    optimfail <- "Warning: optim failed to converge"

    ## the actual likelihood function
    LL <- function(params, tY){
        logexpected <- outer(params$theta, params$beta) +
            outer(params$alpha, params$psi, "+")
        sum(sum(tY * logexpected - exp(logexpected)))  - sum(0.5*(params$beta^2/sigma^2))
    }

    ## Objective functions are _minus_ log likelihoods (for optim)

    ## estimate all psi and beta
    LL.psi.beta <- function(p, y, theta, alpha, sigma) {
        beta <- p[1]
        psi  <- p[2]
        eta  <- psi + beta*theta + alpha
        ll <- sum(eta*y - exp(eta)) - 0.5*(beta^2/sigma^2)
        return(-ll)
    }

    DLL.psi.beta <- function(p, y, theta, alpha, sigma){
        beta <- p[1]
        psi <- p[2]
        mu <- exp(psi + beta*theta + alpha)
        dll <- c(sum(theta * (y-mu)) - beta/sigma^2,
                 sum(y-mu))
        return(-dll)
    }

    ## estimate theta[1]
    LL.first.theta <- function(p, y, beta, psi) {
        theta <- p[1]
      	eta <- psi + beta*theta   # alpha=0
      	ll <- sum(eta*y - exp(eta))
    	return(-ll)
    }

    ## estimate remaining thetas and alphas
    LL.alpha.theta <- function(p, y, beta, psi) {
        theta <- p[1]
        alpha <- p[2]
        eta <- psi + beta*theta + alpha
        ll <- sum(eta*y - exp(eta))
    	return(-ll)
    }

    DLL.alpha.theta <- function(p, y, beta, psi){
    	theta <- p[1]
        alpha <- p[2]
        mu <- exp(psi + beta*theta + alpha)
        dll <- c(sum(beta * (y-mu)),
                 sum(y-mu))
        return(-dll)
    }

    if (!is.null(control$startparams)) {
        inc <- control$startparams
        pars <- coef(inc, form='poisson')

	## fill in known word parameters if they're available
        inter <- intersect(good.words, rownames(pars$words)) 
	
	med.beta <- median(pars$words$beta)
	med.psi <- median(pars$words$psi)
        newpars <- matrix(rep(c(med.beta, med.psi), each=length(good.words)), 
                          ncol=2, dimnames=list(good.words, c('beta', 'psi')))  
        
        newpars[inter,'beta'] <- pars$words[inter,'beta']
        newpars[inter,'psi'] <- pars$words[inter,'psi']
	
        params <- list(beta=newpars[,'beta'],
                       psi=newpars[,'psi'],
                       alpha=pars$docs$alpha,
                       theta=inc$theta)

    } else {
        params <- initialize.urfish(tY)
    }

    ll <- LL(params, tY)
    if (verbose)
        cat("0:\tLL:", ll, "\n")

    iter <- 1
    diffll <- Inf
    while (diffll > tol) {
        oldll <- ll
        if (verbose)
            cat(iter, "\t")

        ## Estimate first theta (alpha = 0)
        resa <- optim(p=c(params$theta[1]),
                      fn=LL.first.theta,
                      gr=NULL,
                      y=as.numeric(Y[,1]),
                      beta=params$beta,
                      psi=params$psi,
                      method=c("BFGS")
                      )
        params$theta[1] <- resa$par[1]
        params$alpha[1] <- 0  ## just in case
        if (resa$convergence!=0)
            cat(optimfail, "while estimating theta[ 1 ]\n")

      	for (i in 2:D) {
            resa <- optim(par=c(params$theta[i], params$alpha[i]),
                          fn=LL.alpha.theta,
                          gr=DLL.alpha.theta,
                          y=as.numeric(Y[,i]),
                          beta=params$beta,
                          psi=params$psi,
                          method=c('BFGS')
                         )
            params$theta[i] <- resa$par[1]
            params$alpha[i] <- resa$par[2]
            if (resa$convergence!=0)
                cat(optimfail, "while estimating theta[", i, "] and alpha[", i, "]\n")

        }

        ## Z-score transformation of estimates for theta
        thetabar     <- mean(params$theta)
        params$theta <- (params$theta - thetabar) / sd(params$theta)

        ## oldbeta      <- params$beta
        ## params$beta  <- params$beta * sd(params$theta)
        ## params$psi   <- params$psi + oldbeta*thetabar

        ## global identification
        if (params$theta[dir[1]] > params$theta[dir[2]])
            params$theta <- params$theta*(-1)

        for (j in 1:V) {
            resa <- optim(par=c(params$beta[j], params$psi[j]),
                          fn=LL.psi.beta,
                          gr=DLL.psi.beta,
                          y=tY[,j],
                          theta=params$theta,
                          alpha=params$alpha,
                          sigma=sigma,
                          method=c('BFGS')
                          )
            params$beta[j] <- resa$par[1]
            params$psi[j] <- resa$par[2]
            if (resa$convergence!=0)
                cat(optimfail, "while estimating beta[", j, "] and psi[", j, "]\n")
        }

        ll <- LL(params, tY)
        diffll <- (ll - oldll)/ll
        if (verbose) {
            cat("LL:", ll, "\t(inc:", diffll, ")\n")
            flush.console()
        }
        iter <- iter + 1
    }

    nms <- words(tY)
    model <- list(dir=dir,
                  theta=params$theta,
                  alpha=params$alpha,
                  beta=params$beta,
                  psi=params$psi,
                  docs=docs(tY),
                  words=words(tY),
                  sigma=sigma,
                  ll=ll,
                  data=Y,
                  call=thecall)

    ## asymptotic standard errors (in multinomial form)
    mnll <- function(tt, b, p, y){
        linpred <- c(0, tt*b + p)
        dmultinom(y, size=sum(y), exp(linpred), log=TRUE)
    }
    model$se.theta <- rep(NA, D)
    for (i in 1:D){
        ## one pass maximum posterior calculation
        new.beta <- (model$beta[2:V] - model$beta[1])
        new.psi <- (model$psi[2:V] - model$psi[1])
        model$theta[i] <- optimize(mnll, interval=c(-6,6), new.beta, new.psi,
                                   tY[i,], maximum=TRUE)$maximum
        ## numerical hessian
        neghess <- -hessian(mnll, model$theta[i], b=new.beta, p=new.psi, y=tY[i,])
        invneghess <- solve(neghess)
        model$se.theta[i] <- sqrt(invneghess)
    }

    class(model) <- c("wordfish", class(model))
    return(model)
}

print.wordfish <- function(x,
                           digits=max(3,getOption('digits')-3),
                           ...){
    cat("Call:\n\t")
    print(x$call)
    cat("\nDocument Positions:\n")
    pp <- predict(x, se.fit=TRUE, interval='confidence')
    colnames(pp) <- c("Estimate", "Std. Error", "Lower", "Upper")
    print(pp, digits=digits)
}


coef.wordfish <- function(object, form=c('multinomial', 'poisson'), ...){
    m <- object
    fm <- match.arg(form)
    if (fm == 'multinomial'){
        V <- length(m$beta)
        word.params <- data.frame(beta=(m$beta[2:V] - m$beta[1]),
                                  psi=(m$psi[2:V] - m$psi[1]))
        rownames(word.params) <- paste(m$words[2:V], "/", m$words[1], sep='')
        d <- list(words=word.params)
    } else {
        word.params <- data.frame(beta=m$beta, psi=m$psi)
        rownames(word.params) <- m$words
        docs <- data.frame(alpha=m$alpha)
        rownames(docs) <- m$docs
        d <- list(words=word.params, docs=docs)
    }

    class(d) <- c('coef.wordfish', class(d))
    return(d)
}

print.coef.wordfish <- function(x,
                                digits=max(3,getOption('digits')-3),
                                ...){
    print(x$words, digits=digits)
    if (!is.null(x$docs))
        print(x$docs, digits=digits)
}

plot.coef.wordfish <- function(x, pch=20, psi=TRUE, ...){

    if (!is(x, "coef.wordfish"))
        stop("First argument must be coefficients from a Wordfish model")

    if (is.null(x$docs))
        stop(paste("Plotting word parameters in the multinomial parameterization",
                   "probably won't\n  be very informative.  Try plotting the value",
                   "of coef(model, form='poisson')"))

    ord <- order(x$words$beta)

    if (!psi){
        dotchart(x$words$beta[ord],
                 labels=row.names(x$words)[ord],
                 pch=pch, ...)
    } else {
        plot(x$words$beta[ord],
             x$words$psi[ord],
             type='n',
             xlab="Beta",
             ylab="Psi", ...)
        text(x$words$beta[ord],
             x$words$psi[ord],
             row.names(x$words)[ord])
    }
}

summary.wordfish <- function(object, level=0.95, ...){
    m <- object
    pp <- predict(m, se.fit=TRUE, interval='confidence')
    colnames(pp) <- c("Estimate", "Std. Error", "Lower", "Upper")
    rownames(pp) <- m$docs
    ret <- list(model=m, scores=pp)
    class(ret) <- c('summary.wordfish', class(ret))
    return(ret)
}

print.summary.wordfish <- function(x,
                          digits=max(3,getOption('digits')-3),
                                   ...){
    cat("Call:\n\t")
    print(x$model$call)
    cat("\nDocument Positions:\n")
    print(x$scores, digits=digits)
}

predict.wordfish <- function(object,
                             newdata=NULL,
                             se.fit=FALSE,
                             interval=c("none", "confidence"),
                             level=0.95,
                             ...){

    m <- object
    if (is.null(newdata)){
        ## use the original data stored in the m
        newd <- as.docword(m$data)
    } else {
        if (is.wfm(newdata))
            newd <- as.docword(newdata)
        else
            stop("Use as.wfm to convert newdata into a suitable format")
    }

    interval <- match.arg(interval)
    if (interval != "none")
        se.fit=TRUE

    ## asymptotic standard errors
    mnll <- function(tt, b, p, y){
        linpred <- c(0, tt*b + p)
        dmultinom(y, size=sum(y), exp(linpred), log=TRUE)
    }

    V <- length(m$beta)
    new.beta <- (m$beta[2:V] - m$beta[1])
    new.psi <- (m$psi[2:V] - m$psi[1])

    preds <- rep(NA, NROW(newd))
    preds.se <- rep(NA, NROW(newd))
    for (i in 1:NROW(newd)){
        preds[i] <- optimize(mnll, interval=c(-6,6), new.beta, new.psi, newd[i,], maximum=TRUE)$maximum
        if (se.fit){
            neghess <- -hessian(mnll, preds[i], b=new.beta, p=new.psi, y=newd[i,])
            invneghess <- solve(neghess)
            preds.se[i] <- sqrt(invneghess)
        }
    }

    if (se.fit){
        res <- data.frame(fit=preds, se.fit=preds.se)
        rownames(res) <- rownames(newd)
        if (interval == "confidence"){
            z <- qnorm(1-(1-level)/2)
            res$lwr <- preds - z * preds.se
            res$upr <- preds + z * preds.se
        }
    } else {
        res <- preds
        names(res) <- rownames(newd) ## we can at least label it
    }
    return(res)
}

plot.wordfish <- function(x,
                          truevals=NULL,
                          level=0.95,
                          pch=20,
                          ...){

    if (!is(x, "wordfish"))
        stop("First argument must be a wordfish model")

    vv       <- as.data.frame(predict(x, se.fit=TRUE, interval='conf', level=level))
    ord      <- order(vv$fit)
    theta    <- vv$fit[ord]
    se.theta <- vv$se.fit[ord]
    upper    <- vv$upr[ord]
    lower    <- vv$lwr[ord]

    name.theta <- x$docs[ord]

    if (!is.null(truevals)){
        truevals <- truevals[ord]
        lims <- c(min(c(theta, truevals, lower)), max(c(theta, truevals, upper)))
        dotchart(theta, labels=name.theta, xlim=lims, pch=pch, ...)
        segments(lower, 1:length(theta), upper, 1:length(theta))
        points(truevals, 1:length(theta), col=rgb(139/255,0,0,0.75), pch=pch)
        title(paste('r =', format(cor(truevals, x$theta), digits=4)))
    } else {
        lims <- c(min(c(theta, lower)), max(c(theta, upper)))
        dotchart(theta, labels=name.theta, xlim=lims, pch=pch, ...)
        segments(lower, 1:length(theta), upper, 1:length(theta))
    }
}

##if (grp){
##    group.med <- aggregate(x$theta, by=list(groups), median)
##    groups <- reorder(groups, order(group.med))
##}
##if (scale){
##    sdt <- sd(theta)
##    theta <- (theta - mean(theta)) / sdt
##    if (se){ se.theta <- se.theta / sdt }
##}

sim.wordfish <- function(docs=10,
                         vocab=20,
                         doclen=500,
                         dist=c("spaced", "normal"),
                         scaled=TRUE){

    if ((vocab %% 4)>0)
        stop("This function assumes you have vocab divisible by 4")

    tdist <- match.arg(dist)

    if (tdist=="spaced")
        theta <- seq(1/docs, 1, 1/docs)
    else if (tdist=="normal")
        theta <- rnorm(docs)
    else
        stop("Unrecognized dist parameter")

    if (scaled){ theta <- scale(theta) }

    theta <- theta[order(theta)]  ## sorted for convenience

    if (length(doclen)==1)
        doclen <- rep(doclen, docs)

    b0 <- c(0, rep(0, vocab/2-1),
            rep(1, vocab/2))
    b1 <- c(0, rep(0, vocab/4-1),
            rep(1, vocab/4),
            rep(0, vocab/4),
            rep(1, vocab/4))

    data <- matrix(data=0, nrow=vocab, ncol=docs)
    mu <- matrix(data=0, nrow=vocab, ncol=docs)
    for (d in 1:docs){
	mu[,d] <- exp(b1 * theta[d] + b0)
  	data[,d] <- rmultinom(1, doclen[d], mu[,d]) # unnormalized probs
    }

    zfill <- function(vals){
        ## assumes vals is an int vector
        v <- as.character(vals)
        wid <- max(apply(as.matrix(v) , 1, nchar))
        formatstr <- paste("%", wid, ".f", sep='')
        newv <- gsub(' ', '0', sprintf(formatstr, vals))
        return(newv)
    }

    rownames(data) <- paste("W", zfill(1:vocab), sep="")
    colnames(data) <- paste("D", zfill(1:docs), sep="")
    Y <- wfm(data, word.margin=1)

    val <- list(Y=Y,
                theta=theta,
                doclen=colSums(Y),
                psi=b0,
                beta=b1)
    class(val) <- c('wordfish.simdata', class(val))

    return(val)
}

fitted.wordfish <- function(object, ...){
    m <- object
    n <- as.numeric(colSums(m$data))
    eta <- outer(m$theta, m$beta) + outer(m$alpha, m$psi, "+")
    mm <- apply(exp(eta), 1, function(x){ x / sum(x) })
    mm <- mm * outer(rep(1,length(m$beta)), n)
    colnames(mm) <- m$docs
    rownames(mm) <- m$words
    return(t(mm))
}

initialize.urfish <- function(tY){
    ## mostly the initialization routine from S&P

    D             <- nrow(tY)
    V             <- ncol(tY)
    numword       <- rep(1:V, each=D)
    numdoc        <- rep(1:D, V)

    dat           <- matrix(1, nrow=V*D, ncol=3)
    dat[,1]       <- as.vector(as.matrix(tY))
    dat[,2]       <- as.vector(numword)
    dat[,3]       <- as.vector(numdoc)
    dat           <- data.frame(dat)

    colnames(dat) <- c("y", "word", "doc")
    dat$word      <- factor(dat$word)
    dat$doc       <- factor(dat$doc)

    b0            <- log(colMeans(tY))

    rmeans        <- rowMeans(tY)
    alpha         <- log(rmeans/rmeans[1])

    ystar         <- log(dat$y+0.1) - alpha[dat$doc] - b0[dat$word]
    ystarm        <- matrix(ystar, D, V, byrow=FALSE)
    res           <- svd(ystarm, nu=1)
    b             <- as.numeric(res$v[,1]*res$d[1])

    ## normalize
    th            <- as.vector(res$u)-res$u[1,1]
    theta         <- th/sd(th)
    b1            <- b*sd(th)

    params        <- list(alpha=as.numeric(alpha),
                          psi=as.numeric(b0),
                          beta=b1,
                          theta=theta)
    return(params)
}

bootstrap.se <- function(object, L=50, verbose=FALSE, ...){
    if (is(object, 'wordfish')){
        m <- object
        b <- m$beta
        p <- m$psi
        a <- m$alpha
        th <- m$theta

        D <- length(th)
        V <- length(b)

        mtheta <- matrix(0, nrow=D, ncol=L)
        logexpected <- outer(th, b) + outer(a, p, "+")
        lambda <- exp(logexpected)
        for (l in 1:L){
            if (verbose)
                cat(paste("iter:", l, "\n"))

            mat <- matrix(rpois(rep(1, D*V), as.vector(lambda)), nrow=D)
            rownames(mat) <- m$docs
            colnames(mat) <- m$words
            newY <- wfm(mat, word.margin=2)
            newparams <- wordfish(newY, dir=m$dir,
                                  control=list(sigma=m$sigma,
                                  startparams=m,
                                  tol=m$tol),
                                  verbose=FALSE)
            mtheta[,l] <- newparams$theta
        }
    } else {
        stop("Parametric bootstrap is only implemented for wordfish models")
    }

    se <- data.frame(boot.se=apply(mtheta, 1, sd))
    return(se)
}
