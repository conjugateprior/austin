#' austin: Do things with words
#' 
#' Austin helps you see what people, usually politicians, do with words.
#' Currently that means how positions on a presumed underlying
#' policy scale are taken by manipulating word occurrence counts.  
#' The models implemented here try to
#' try to recover those positions using only this information, plus
#' some heroic assumptions about language generation, e.g.
#' unidimensionality, conditional independence of words given ideal point
#' and Poisson-distributed word counts.
#' 
#' The package currently implements Wordfish (Slapin and Proksch, 2008) and 
#' Wordscores (Laver, Benoit and Garry, 2003).  See references for details.
#' 
#' @references 
#'   J. Slapin and S.-O. Proksch (2008) 'A scaling model for
#'   estimating time-series party positions from texts' American Journal of
#'   Political Science 52(3), 705-722.
#'   
#'   Laver, Benoit and Garry (2003) `Estimating policy positions from
#'   political text using words as data' American Political Science Review 97(2).
#'   
#' @docType package
#' @name austin
NULL

#' The Irish No-Confidence debate
#' 
#' Irish No-Confidence Debate
#' 
#' This are word counts from the no-confidence debate in Ireland.
#' 
#' \code{daildata} is a word frequency object.
#' 
#' @name daildata
#' @docType data
#' @references Benoit and Laver's Irish Political Studies piece. (fixme!)
#' @keywords datasets
NULL





#' Economics sections of German Party Manifestos
#' 
#' A word frequency matrix from the economic sections of German political party
#' manifestos from 1990-2005.
#' 
#' demanif.econ is word frequency matrix
#' 
#' @name demanif.econ
#' @docType data
#' @references J. Slapin and S.-O. Proksch (2008) 'A scaling model for
#' estimating time-series party positions from texts' American Journal of
#' Political Science 52(3), 705-722.
#' @source These data courtesy are of S.-O. Proksch.
#' @keywords datasets
NULL





#' Foreign Policy Sections of German Party Manifestos
#' 
#' A word frequency matrix from the foreign policy sections of German political
#' party manifestos from 1990-2005.
#' 
#' demanif.foreign is word frequency matrix
#' 
#' @name demanif.foreign
#' @docType data
#' @references J. Slapin and S.-O. Proksch (2008) 'A scaling model for
#' estimating time-series party positions from texts' American Journal of
#' Political Science 52(3), 705-722.
#' @source These data courtesy are of S.-O. Proksch.
#' @keywords datasets
NULL





#' German Party Manifesto Data
#' 
#' A random sample of words and their frequency in German political party
#' manifestos from 1990-2005.
#' 
#' demanif is word frequency matrix
#' 
#' @name demanif
#' @docType data
#' @references J. Slapin and S.-O. Proksch (2008) 'A scaling model for
#' estimating time-series party positions from texts' American Journal of
#' Political Science 52(3), 705-722.
#' @source Wordfish website (http://www.wordfish.org)
#' @keywords datasets
NULL





#' Societal sections of German Party Manifestos
#' 
#' A word frequency matrix from the societal sections of German political party
#' manifestos from 1990-2005.
#' 
#' demanif.soc is word frequency matrix
#' 
#' @name demanif.soc
#' @docType data
#' @references J. Slapin and S.-O. Proksch (2008) 'A scaling model for
#' estimating time-series party positions from texts' American Journal of
#' Political Science 52(3), 705-722.
#' @source These data courtesy are of S.-O. Proksch.
#' @keywords datasets
NULL





#' Irish Budget Debate Data 2009
#' 
#' Irish budget debate 2009
#' 
#' This are word counts from the 2009 Budget debate in Ireland.
#' 
#' This is a word frequency nmatrix.  Loading this data also makes available
#' \code{iebudget2009cov} which contains covariates for the speakers.
#' 
#' @name iebudget2009
#' @aliases iebudget2009 iebudget2009cov
#' @docType data
#' @keywords datasets
NULL





#' Interest Groups
#' 
#' Interest Groups and the European Commission
#' 
#' Word counts from interest groups and a European commission proposal to
#' reduce CO2 emmissions in 2007.
#' 
#' \code{comm1} and \code{comm2} are the commission's proposal before and after
#' the proposals of the interest groups.
#' 
#' @name interestgroups
#' @docType data
#' @references H. Kluever (2009) 'Measuring influence group influence using
#' quantitative text analysis' European Union Politics 11:1.
#' @keywords datasets
NULL





#' Example Data
#' 
#' Example data from Laver Benoit and Garry (2003)
#' 
#' This is the example word count data from Laver, Benoit and Garry's (2000)
#' article on Wordscores.  Documents R1 to R5 are assumed to have known
#' positions: -1.5, -0.75, 0, 0.75, 1.5.  Document V1 is assumed unknown.  The
#' `correct' position for V1 is presumed to be -0.45.
#' \code{\link{classic.wordscores}} generates approximately -0.45.
#' 
#' To replicate the analysis in the paper, use the wordscores function either
#' with identification fixing the first 5 document positions and leaving
#' position of V1 to be predicted.
#' 
#' @name lbg
#' @docType data
#' @references Laver, Benoit and Garry (2003) `Estimating policy positions from
#' political text using words as data' American Political Science Review 97(2).
#' @keywords datasets
NULL





#' UK Manifesto Data
#' 
#' UK manifesto data from Laver et al.
#' 
#' This are word counts from the manifestos of the three main UK parties for
#' the 1992 and 1997 elections.
#' 
#' ukmanif is a word frequency object.
#' 
#' @name ukmanif
#' @docType data
#' @references Laver, Benoit and Garry (2003) `Estimating policy positions from
#' political text using words as data' American Political Science Review 97(2).
#' @keywords datasets
NULL



