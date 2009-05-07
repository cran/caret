makeCubistFiles <- function(x, y,
                            testX = NULL, testY = NULL,
                            ylab = "outcome",
                            prefix = NULL)
  {
    if(is.null(prefix)) prefix <- paste("data", format(Sys.time(), "%Y%m%d_%H%M%S"), sep = "_")
    ## check for consistent train and test sizes
    ## check for commas in rownames
    
    if(!is.numeric(y)) stop("outcome variable must be numeric")
    call <- match.call()
    ## See http://www.rulequest.com/cubist-win.html
    ## for file sepcifications
    
    if(nrow(x) != length(y)) stop("the number of samples in x and y must be the same")


    #########################################################
    ## Determine variable types
    
    if(is.matrix(x))
      {
        if(is.numeric(x))
          {
            varType <- rep("continuous", ncol(x))
          } else {
            varType <- rep("discrete", ncol(x))
          }
      } else {
        foo <- function(x) if(is.numeric(x)) "continuous" else "discrete"
        varType <- unlist(lapply(x, foo))
      }

    #########################################################
    ## Setup and write names attributes
    
    nameData <- c(ylab, "sample_ids:label")
    nameData <- c(nameData,
                  paste(ylab, "continuous", sep = ":")) ## can also use "target" ?
    nameData <- c(nameData,
                  paste(names(varType),
                        varType,
                        sep = ":"))
    nameData <- as.matrix(nameData, ncol = 1)

    cat(paste(nameData, collapse = "\n"),
        file = paste(prefix, ".names", sep = ""))
    
    if(!file.exists(paste(prefix, ".names", sep = "")))
      stop(paste("error creating", paste(prefix, ".names", sep = "")))

    #########################################################
    ## Setup and write training data

    train.ids <- rownames(x)
    if(is.null(train.ids)) train.ids <- paste("sample", 1:nrow(x), sep = "")

    if(any(duplicated(train.ids))) stop("please use unique row names in training set")

    train.data <- data.frame(sample = train.ids,
                             .outcome = y)
    train.data <- cbind(train.data, x)
    if(length(nameData) -1 != ncol(train.data)) stop("problem creating the files - incompatible dimensions")

    write.table(train.data,
                sep = ",",
                file = paste(prefix, ".data", sep = ""),
                quote = FALSE,
                row.names = FALSE,
                col.names = FALSE)
    
    if(!file.exists(paste(prefix, ".data", sep = "")))
      stop(paste("error creating", paste(prefix, ".data", sep = "")))
    
    if(!is.null(testX))
      {

        #########################################################
        ## Setup and write training data

        test.ids <- rownames(testX)
        if(is.null(test.ids)) test.ids <- paste("sample", nrow(x) + (1:nrow(testX)), sep = "")

        if(any(duplicated(test.ids))) stop("please use unique row names in test set")

        if(is.null(testY)) testY <- rep(0, nrow(testX))
        
        test.data <- data.frame(sample = test.ids,
                                .outcome = testY)
        test.data <- cbind(test.data, testX)
        if(length(nameData) - 1 != ncol(test.data))
          stop("problem creating the files - incompatible dimensions between training and test")

        write.table(test.data,
                    sep = ",",
                    file = paste(prefix, ".cases", sep = ""),
                    quote = FALSE,
                    row.names = FALSE,
                    col.names = FALSE)
        
        if(!file.exists(paste(prefix, ".cases", sep = "")))
          stop(paste("error creating", paste(prefix, ".cases", sep = "")))
      }

    
    #########################################################
    ##     
  }

fitCubist <- function(path = NULL, prefix = "model", numCom = 1, nn = 5, rules = 5)
  {
    if(is.null(path)) path <- "/grid/gro/vol/ccdev/cscoe/Cubist/PfeCubistModel/bin/PfeCubistModel64"
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

cubistPred <- function(path = NULL, prefix = "model", cleanup = TRUE)
  {
    if(is.null(path)) path <- "/grid/gro/vol/ccdev/cscoe/Cubist/PfeCubistPredict/bin/PfeCubistPredict64"
    
    predCall <- paste(path,"-f", prefix,
                      ">", paste(prefix, ".csv", sep = ""))
    doPred <- system(predCall,
                     wait = TRUE,
                     intern = FALSE)
    if(doPred == 0)
      {
        if(!file.exists(paste(prefix, ".csv", sep = "")))
          stop("csv file was not created")
      } else stop("system call returned an error")

    ret <- read.csv(paste(prefix, ".csv", sep = ""))
    if(cleanup)
      {
        try(unlink(paste(prefix, ".data", sep = "")), silent = TRUE)
        try(unlink(paste(prefix, ".names", sep = "")), silent = TRUE)
        try(unlink(paste(prefix, ".dw", sep = "")), silent = TRUE)
        try(unlink(paste(prefix, ".model", sep = "")), silent = TRUE)
        try(unlink(paste(prefix, ".csv", sep = "")), silent = TRUE)
        try(unlink(paste(prefix, ".cases", sep = "")), silent = TRUE)
      }
    ret
  }


