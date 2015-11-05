#' Estimate a Wordfish Model
#' 
#' Estimates a Wordfish model using Conditional Maximum Likelihood.
#' 
#' Fits a Wordfish model with document ideal points constrained to mean zero
#' and unit standard deviation.
#' 
#' The \code{control} list specifies options for the estimation process.
#' \code{conv.check} is either 'll' which stops when the difference
#' in log likelihood between iterations is less than \code{tol}, or 'cor'
#' which stops when one minus the correlation between the \code{theta}s
#' from the current and the previous iterations is less  
#' than \code{tol}. \code{sigma} is the standard deviation for the beta 
#' prior in poisson form. \code{startparams} is a list of starting values
#' (\code{theta}, \code{beta}, \code{psi} and \code{alpha}).  A previously 
#' fitted Wordfish model is sufficient.
#' \code{verbose} generates a running commentary during estimation
#' 
#' The model has two equivalent forms: a poisson model with two sets of
#' document and two sets of word parameters, and a multinomial with two sets of
#' word parameters and document ideal points.  The first form is used for
#' estimation, the second for summarizing and prediction.
#' 
#' The model is regularized by assuming a prior on beta with mean zero and
#' standard deviation sigma (in poisson form).  If you don't want to
#' regularize, set beta to a large number.
#' 
#' @param wfm a word frequency matrix
#' @param dir set global identification by forcing \code{theta[dir[1]]} <
#' \code{theta[dir[2]]} (defaults to first and last document)
#' @param control list of estimation options
#' @param verbose produce a running commentary
#' @return An object of class wordfish.  This is a list containing:
#' 
#' \item{dir}{global identification of the dimension} 
#' \item{theta}{document
#' positions} 
#' \item{alpha}{document fixed effects} 
#' \item{beta}{word slope
#' parameters} 
#' \item{psi}{word fixed effects} 
#' \item{docs}{names of the documents} 
#' \item{words}{names of words} 
#' \item{sigma}{regularization parameter for betas in poisson form}
#' \item{ll}{final log likelihood} 
#' \item{se.theta}{standard errors for document position} 
#' \item{data}{the original data}
#' 
#' @author Will Lowe
#' @importFrom numDeriv hessian
#' @seealso \code{\link{plot.wordfish}}, \code{\link{summary.wordfish}},
#' \code{\link{coef.wordfish}}, \code{\link{fitted.wordfish}},
#' \code{\link{predict.wordfish}}, \code{\link{sim.wordfish}}
#' @references Slapin and Proksch (2008) 'A Scaling Model for Estimating
#' Time-Series Party Positions from Texts.' American Journal of Political
#' Science 52(3):705-772.
#' @examples
#' 
#' dd <- sim.wordfish()
#' wf <- wordfish(dd$Y)
#' summary(wf)
#' 
#' @export
wordfish <- function(wfm,
                     dir=c(1, length(docs(wfm))),
                     control=list(tol=1e-06, sigma=3,
                                  startparams=NULL), verbose=FALSE,
                     conv.check=c('ll', 'cor')){
  thecall <- match.call()
  dir <- dir
  
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
  
  #Y <- as.worddoc(wfm)
  tY <- as.docword(wfm)
  D  <- NROW(tY)
  V  <- NCOL(tY)
  wfm <- NULL   ## one version is quite enough
  
  ## enforce control defaults
  tol <- ifelse(is.null(control$tol), 1e-06, control$tol)
  sigma <- ifelse(is.null(control$sigma), 3, control$sigma)
  conv.check <- ifelse(is.null(control$conv.check), 'll', control$conv.check)
  
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
  diff <- Inf ## the quantity we're checking is less than tol
  while (diff > tol) {
    if (conv.check=='cor') { old.theta <- params$theta }
    else { old.ll <- ll }
    
    if (verbose) cat(iter, "\t")
    
    ## Estimate first theta (alpha = 0)
    resa <- optim(par=c(params$theta[1]),
                  fn=LL.first.theta,
                  gr=NULL,
                  y=as.numeric(tY[1,]),
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
                    y=as.numeric(tY[i,]),
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
    if (conv.check=='ll'){
      diff <- (ll - old.ll)/ll
      if (verbose) {
        cat("LL:", ll, "\n")
        flush.console()
      }
    } else {
      diff <- 1-cor(params$theta, old.theta) 
      if (verbose) {
        cat("1-cor:", diff, "\n")
        flush.console()
      }
    }
    
    iter <- iter + 1
  }
  
  model <- list(dir=dir,
                theta=params$theta,
                alpha=params$alpha,
                beta=params$beta,
                psi=params$psi,
                docs=docs(tY),
                words=words(tY),
                sigma=sigma,
                ll=ll,
                data=t(tY),
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

##' @method print wordfish
##' @export
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

#' Extract Word Parameters
#' 
#' Extract word parameters beta and psi in an appropriate model
#' parameterization
#' 
#' Slope parameters and intercepts are labelled beta and psi respectively.  In
#' multinomial form the coefficient names reflect the fact that the
#' first-listed word is taken as the reference category.  In poisson form, the
#' coefficients are labeled by the words the correspond to.
#' 
#' Note that in both forms there will be beta and psi parameters, so make sure
#' they are the ones you want.
#' 
#' @param object an object of class wordfish
#' @param form which parameterization of the model to return parameters for
#' @param ... extra arguments
#' @return A data.frame of word parameters from a wordfish model in one or
#' other parameterization.
#' @author Will Lowe
#' @export
#' @method coef wordfish
#' @seealso \code{\link{wordfish}}
coef.wordfish <- function(object, form=c('poisson', 'multinomial'), ...){
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

#' Plot the Word Parameters From a Wordfish Model
#' 
#' Plots sorted beta and optionally also psi parameters from a Wordfish model
#' 
#' 
#' @param x a fitted Wordfish model
#' @param pch Default is to use small dots to plot positions
#' @param psi whether to plot word fixed effects
#' @param ... Any extra graphics parameters to pass in
#' @return A plot of sorted beta and optionally psi parameters.
#' @author Will Lowe
#' @seealso \code{\link{wordfish}}
#' @export
#' @method plot coef.wordfish
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

#' Summarize a Wordfish Model
#' 
#' Summarises estimated document positions from a fitted Wordfish model
#' 
#' if `level' is passed to the function, e.g. 0.95 for 95 percent confidence,
#' this generates the appropriate width intervals.
#' 
#' @param object fitted wordfish model
#' @param level confidence interval coverage
#' @param ... extra arguments, e.g. level
#' @return A data.frame containing estimated document position with standard
#' errors and confidence intervals.
#' @author Will Lowe
#' @seealso \code{\link{wordfish}}
#' @export
#' @method summary wordfish
summary.wordfish <- function(object, level=0.95, ...){
  m <- object
  pp <- predict(m, se.fit=TRUE, interval='confidence')
  colnames(pp) <- c("Estimate", "Std. Error", "Lower", "Upper")
  rownames(pp) <- m$docs
  ret <- list(model=m, scores=pp)
  class(ret) <- c('summary.wordfish', class(ret))
  return(ret)
}

#' @method print summary.wordfish
#' @export
print.summary.wordfish <- function(x,
                                   digits=max(3,getOption('digits')-3),
                                   ...){
  cat("Call:\n\t")
  print(x$model$call)
  cat("\nDocument Positions:\n")
  print(x$scores, digits=digits)
}

#' Predict Method for Wordfish
#' 
#' Predicts positions of new documents using a fitted Wordfish model
#' 
#' Standard errors for document positions are generated by numerically
#' inverting the relevant Hessians from the profile likelihood of the
#' multinomial form of the model.
#' 
#' @param object A fitted wordfish model
#' @param newdata An optional data frame or object of class wfm in which to
#' look for word counts to predict document ideal points which to predict.  If
#' omitted, the fitted values are used.
#' @param se.fit A switch indicating if standard errors are required.
#' @param interval Type of interval calculation
#' @param level Tolerance/confidence level
#' @param ... further arguments passed to or from other methods.
#' @return \code{predict.wordfish} produces a vector of predictions or a matrix
#' of predictions and bounds with column names `fit' and `se.fit', and with
#' `lwr', and `upr' if `interval' is also set.
#' @author Will Lowe
#' @importFrom numDeriv hessian
#' @seealso \code{\link{wordfish}}
#' @export
#' @method predict wordfish
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

#' Plot a Wordfish Model
#' 
#' Plots a fitted Wordfish model with confidence intervals
#' 
#' 
#' @param x a fitted Wordfish model
#' @param truevals True document positions if known
#' @param level Intended coverage of confidence intervals
#' @param pch Default is to use small dots to plot positions
#' @param ... Any extra graphics parameters to pass in
#' @return A plot of sorted estimated document positions, with confidence
#' intervals and true document positions, if these are available.
#' @author Will Lowe
#' @export
#' @seealso \code{\link{wordfish}}
#' @method plot wordfish
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



#' Simulate data and parameters for a Wordfish model
#' 
#' Simulates data and returns parameter values using Wordfish model
#' assumptions: Counts are sampled under the assumption of independent Poisson
#' draws with log expected means linearly related to a lattice of document
#' positions.
#' 
#' This function draws `docs' document positions from a Normal distribution, or
#' regularly spaced between 1/`docs' and 1.
#' 
#' `vocab'/2 word slopes are 1, the rest -1.  All word intercepts are 0.
#' `doclen' words are then sampled from a multinomial with these parameters.
#' 
#' Document position (theta) is sorted in increasing size across the documents.
#' If `scaled' is true it is normalized to mean zero, unit standard deviation.
#' This is most helpful when dist=normal.
#' 
#' @param docs How many `documents' should be generated
#' @param vocab How many `word' types should be generated
#' @param doclen A scalar `document' length or vector of lengths
#' @param dist the distribution of `document' positions
#' @param scaled whether the document positions should be mean 0, unit sd
#' @return \item{Y}{A sample word-document matrix} \item{theta}{The `document'
#' positions} \item{doclen}{The `document' lengths} \item{beta}{`Word'
#' intercepts} \item{psi}{`Word' slopes}
#' @author Will Lowe
#' @export sim.wordfish
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

#' Get Fitted Values from a Wordfish Model
#' 
#' Extracts the estimated word rates from a fitted Wordfish model
#' 
#' 
#' @param object a fitted Wordfish model
#' @param ... Unused
#' @return Expected counts in the word frequency matrix
#' @author Will Lowe
#' @export
#' @method fitted wordfish
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

#' initialize.urfish
#' 
#' Get cheap starting values for a Wordfish model
#' 
#' This function is only called by model fitting routines and does therefore
#' not take a wfm classes.  tY is assumed to be in document by term form.
#' 
#' In the poisson form of the model incidental parameters (alpha) are set to
#' log(rowmeans/rowmeans[1]).  intercept (psi) values are set to log(colmeans)
#' These are subtracted from a the data matrix, which is logged and decomposed
#' by SVD.  Word slope (beta) and document position (theta) are estimated by
#' rescaling SVD output.
#' 
#' @param tY a document by word matrix of counts
#' @return List with elements: \item{alpha}{starting values of alpha
#' parameters} \item{psi}{starting values of psi parameters}
#' \item{beta}{starting values of beta parameters} \item{theta}{starting values
#' for document positions}
#' @author Will Lowe
#' @references This is substantially the method used by Slapin and Proksch's
#' original code.
#' @export initialize.urfish
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

#' Compute Bootstrap Standard Errors
#' 
#' Computes bootstrap standard errors for document positions from a fitted
#' Wordfish model
#' 
#' This function computes a parametric bootstrap by resampling counts from the
#' fitted word counts, refitting the model, and storing the document positions.
#' The standard deviations for each resampled document position are returned.
#' 
#' @param object a fitted Wordfish model
#' @param L how many replications
#' @param verbose Give progress updates
#' @param ... Unused
#' @return Standard errors for document positions
#' @author Will Lowe
#' @export bootstrap.se
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
