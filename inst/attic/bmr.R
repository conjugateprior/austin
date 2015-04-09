bmr.locate.training.executable <- function(hint=NULL){
    if (!is.null(hint))
        exec <- hint
    else {
        if (.Platform$OS.type=="windows")
            exec <- system.file("exec/BMRtrain.exe", package="austin")
        else
            exec <- system.file("exec/BMRtrain_osx", package="austin")
            ## exec <- system.file("exec/BMRtrain4.0", package="austin")
        }
    if (is.null(exec) || exec == "")
        stop("Could not find executable")
    return(exec)
}

bmr.locate.test.executable <- function(hint=NULL){
    if (!is.null(hint))
        exec <- hint
    else {
        if (.Platform$OS.type=="windows")
            exec <- system.file("exec/BMRclassify.exe", package="austin")
        else
            exec <- system.file("exec/BMRclassify_osx", package="austin")
            ##exec <- system.file("exec/BMRclassify4.0", package="austin")
    }
    if (is.null(exec) || exec == "")
        stop("Could not find executable")
    return(exec)
}

bmr.classify <- function(datafile, modelfile, resultfile,
                         verbose=2,
                         test.exec=NULL){

    exec <- bmr.locate.test.executable(test.exec)

    comm <- paste(shQuote(exec),
                  "-r", shQuote(resultfile),
                  "-l", shQuote(verbose),
                  shQuote(datafile),
                  shQuote(modelfile))
    print(paste("Running command:\n", comm))

    system(comm)
}

bmr.help <- function(train.exec=NULL){
    exec <- bmr.locate.training.executable(train.exec)
    system(paste(shQuote(exec), "--h"))
}

bmr.train <- function(datafile, modelfile, resultfile,
                      prior=c("laplace", "gaussian"),
                      verbose=2,
                      train.exec=NULL,
                      pars=NULL){

    pr <- match.arg(prior)
    if (pr == "laplace")
        pchoice <- 1
    else
        pchoice <- 2

    exec <- bmr.locate.training.executable(train.exec)

    comm <- paste(shQuote(exec),
                  "-p", pchoice,
                  "-r", shQuote(resultfile),
                  "-l", verbose,
                  pars,
                  shQuote(datafile),
                  shQuote(modelfile))

    print(paste("Running the command:\n", comm))

    system(comm)
}

bmr.results <- function(filename){
    f <- file(filename, 'r')
    cv <- read.table(filename)
    colnames(cv) <- c("True", rep("Prob.", colnames(cv)-2), "Predicted")
    cv
}

bmr.clinton <- function(ctrain.y, ctrain.x, ctest.y, ctest.x){
    wfm2bmr(ctrain.y+2, ctrain.x, "bmr.clinton.data.txt")
    bmr.train("bmr.clinton.data.txt",
              "bmr.clinton.model.txt",
              "bmr.clinton.results.txt",
              prior="laplace",
              verbose=2)
    wfm2bmr(ctest.y+2, ctest.x, "bmr.clinton.testdata.txt")

    bmr.classify("bmr.clinton.testdata.txt",
                 "bmr.clinton.model.txt",
                 "bmr.test.testresults.txt",
                 verbose=2)

}
