#' Rescale Estimated Document Positions
#' 
#' Linearly rescales estimated document positions on the basis of two control
#' points.
#' 
#' The rescaled positions set document with index ident[1] to position ident[2]
#' and docuemnt with index ident[3] to position ident[4].  The fitted model
#' passed as the first argument is not affected.
#' 
#' @param object fitted wordfish or wordscores object
#' @param ident two documents indexes and and their desired new positions
#' @return A data frame containing the rescaled document positions with
#' standard errors if available.
#' @author Will Lowe
#' @importFrom stats coef lm
#' @export 
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

