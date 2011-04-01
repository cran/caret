cubistType <- function (x, ...) UseMethod("cubistType")
cubistType.numeric <- function(x, ...) "continuous."
cubistType.factor <- function(x, ...) paste(paste(levels(x), collapse = ","), ".", sep = "")
cubistType.character <- function(x, ...) paste(paste(unique(x), collapse = ","), ".", sep = "")
cubistType.ordered <- function(x, ...) paste("[ordered]", paste(levels(x), collapse = ","), ".", sep = "")
cubistType.matrix <- function(x, ...)
{
  if(is.numeric(x)) out <- rep("continuous.", ncol(x))
  if(is.character(x)) out <- apply(x, 2, cubistType)
  out
}

cubistType.data.frame <- function(x, ...) unlist(lapply(x, cubistType))


makeCubistFiles2 <- function(x, y,
                            testX = NULL, testY = NULL,
                            ylab = "outcome",
                            prefix = NULL,
                            cubist = "cubist",
                            numCom = 1, nn = 5, rules = 5)
  {
    if(is.null(prefix)) prefix <- paste("data", format(Sys.time(), "%Y%m%d%H%M%S"), sep = "_")
    ## check for consistent train and test sizes
    ## check for commas in rownames
    
    #if(!is.numeric(y)) stop("outcome variable must be numeric")
    call <- match.call()
    ## See http://www.rulequest.com/cubist-win.html
    ## for file sepcifications
    
    if(nrow(x) != length(y)) stop("the number of samples in x and y must be the same")

  fileHeader <- paste("| autmatically created using", R.version.string,
                            "on",  format(Sys.time(), "%a %b %d %H:%M:%S %Y"),
                      "\n| Call: ", paste(deparse(call), collapse = ""), "\n")
    
    #########################################################
    ## Setup and write names attributes


    dataTypes <- cubistType(x)
    dataTypes <- c(dataTypes, "continuous.")
    if(any(names(dataTypes) == ylab)) stop("the outcome label is in the 'x' data. please pick another name")
    names(dataTypes)[length(dataTypes)] <- ylab                            
    nameFileData <- paste(names(dataTypes), dataTypes, sep = "\t")
    nameFileData <- paste(nameFileData, collapse = "\n")
    nameFileData <- paste(fileHeader, nameFileData, sep = "\n")
    cat(nameFileData, file = paste(prefix, ".names", sep = ""))
    
    if(!file.exists(paste(prefix, ".names", sep = "")))
      stop(paste("error creating", paste(prefix, ".names", sep = "")))

    
    #########################################################
    ## Setup and write training data

    if(!is.data.frame(x)) x <- as.data.frame(x)
     train <- x
    train$.outcome <- y
 
    write.table(train,
                sep = ",",
                na = "?",
                file = paste(prefix, ".data", sep = ""),
                quote = FALSE,
                row.names = FALSE,
                col.names = FALSE)
    
    if(!file.exists(paste(prefix, ".data", sep = "")))
      stop(paste("error creating", paste(prefix, ".data", sep = "")))

     #########################################################
    ## Optionally write test data

    if(!is.null(testX) & !is.null(testY))
      {
        if(!is.data.frame(testX)) testX <- as.data.frame(testX)
        test <- testX
        test$.outcome <- testY
        
        write.table(test,
                    sep = ",",
                    na = "?",
                    file = paste(prefix, ".test", sep = ""),
                    quote = FALSE,
                    row.names = FALSE,
                    col.names = FALSE)
        
        if(!file.exists(paste(prefix, ".test", sep = "")))
          stop(paste("error creating", paste(prefix, ".test", sep = "")))
      }
     #########################################################
    ## Write out system commands
        run <- paste(cubist,
                     "-f", prefix,
                     "-C", numCom,
                     "-n", nn,
                     "-a",
                     "-r", rules)
    cat("To run the cubist model,  use:\n\n\t", run, "\n")
    
    
  }

fitCubist <- function(path = NULL, prefix = "model", numCom = 1, nn = 5, rules = 5)
  {
    if(is.null(path)) path <- "~/Downloads/Cubist/cubist"
    fitCall <- paste(path,
                     "-f", prefix,
                     "-C", numCom,
                     "-n", nn,
                     "-a",
                     "-r", rules)
    doFit <- system(fitCall,
                    wait = TRUE,
                    intern = FALSE)
    if(doFit == 0)
      {
        if(!file.exists(paste(prefix, ".model", sep = "")))
          stop("model file was not created")
      } else stop("system call returned an error")
    cat("  Cubist model was fit\n\n")
    invisible(fitCall)
  }

cubistPred <- function(path = NULL, prefix = "model", cleanup = FALSE)
  {
    if(is.null(path)) path <- "~/Downloads/Cubist/cubist"
    
    predCall <- paste(path,"-f", prefix,
                      ">", paste(prefix, ".csv", sep = ""))
    doPred <- system(predCall,
                     wait = TRUE,
                     intern = FALSE)
    if(doPred == 0)
      {
        if(!file.exists(paste(prefix, ".pred", sep = "")))
          stop(".pred file was not created")
      } else stop("system call returned an error")

   # tmp <- read.delim(paste(prefix, ".pred", sep = ""), sep = "\n", header = FALSE, stringsAsFactors = FALSE)
   # startRow <- grep("--", tmp$V1) - 1
    ret <- read.delim(paste(prefix, ".pred", sep = ""),
                      skip = 6,
                      sep = "\n",
                      stringsAsFactors = FALSE,
                      header = FALSE)[,1]
    splitUp <- strsplit(ret, " ")
    obs <- as.numeric(unlist(lapply(splitUp, function(x)x[x != ""][1])))
    pred <- as.numeric(unlist(lapply(splitUp, function(x)x[x != ""][2])))
    if(cleanup)
      {
        try(unlink(paste(prefix, ".data", sep = "")), silent = TRUE)
        try(unlink(paste(prefix, ".names", sep = "")), silent = TRUE)
        try(unlink(paste(prefix, ".dw", sep = "")), silent = TRUE)
        try(unlink(paste(prefix, ".model", sep = "")), silent = TRUE)
        try(unlink(paste(prefix, ".csv", sep = "")), silent = TRUE)
        try(unlink(paste(prefix, ".cases", sep = "")), silent = TRUE)
      }
    data.frame(obs = obs, pred = pred)
  }



QuinlanAttributes <- function (x, ...) UseMethod("QuinlanAttributes")
QuinlanAttributes.numeric <- function(x, ...) "continuous."
QuinlanAttributes.factor <- function(x, ...) paste(paste(levels(x), collapse = ","), ".", sep = "")
QuinlanAttributes.character <- function(x, ...) paste(paste(unique(x), collapse = ","), ".", sep = "")
QuinlanAttributes.ordered <- function(x, ...) paste("[ordered]", paste(levels(x), collapse = ","), ".", sep = "")
QuinlanAttributes.matrix <- function(x, ...) apply(x, 2, QuinlanDescription)
QuinlanAttributes.data.frame <- function(x, ...) unlist(lapply(x,  QuinlanDescription))


formatAttributes <- function(x)
  {
    ## gsub special chars with escapes
    x
  }

makeNamesFile <- function(x, y, label = "outcome", comments = TRUE)
  {
    if(comments)
      {
        call <- match.call()
        out <- paste("| Generated using ", R.version.string, "\n",
                     "| on ", format(Sys.time(), "%a %b %d %H:%M:%S %Y"), "\n",
                     "| function call: ", paste(deparse(call)),
                     sep = "")
      } else out <- ""

    out <- paste(out,
                 "\n", label, ".\n",
                 "\n", label, ": continuous.",
                 sep = "")
    varData <- QuinlanAttributes(x)
    varData <- paste(names(varData), ": ", varData, sep = "", collapse = "\n")
    out <- paste(out, "\n", varData, sep = "")
    out


  }


makeDataFile <- function(x, y)
  {
    if(!is.data.frame(x)) x <- as.data.frame(x)
    x <- cbind(y, x)
    out <- capture.output(
                          write.table(x,
                                      sep = ",",
                                      na = "?",
                                      file = "",
                                      quote = FALSE,
                                      row.names = FALSE,
                                      col.names = FALSE))
    paste(out, collapse = "\n")
  }
