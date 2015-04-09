rescale <- function(object, ident=c(1,-1,10,1)){
    val1 <- object$theta[ident[1]]
    val2 <- object$theta[ident[3]]
    ab <- as.numeric(coef(lm(c(ident[2], ident[4]) ~ c(val1, val2))))

    if (is(object, 'wordfish')){
        d <- data.frame(theta=(object$theta*ab[2] + ab[1]), se=object$se.theta*ab[2])
        names(d) <- c('Estimate', 'Std. Error')
    } else if (is(object, 'wordscores')){
        d <- data.frame(Estimate=(object$theta*ab[2] + ab[1]))
    }
    return(d)
}

