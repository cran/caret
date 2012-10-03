"varImp" <-
function(object, ...){
   UseMethod("varImp")
}



varImp.dsa <- function(object, cuts = NULL, ...)
  {
    if(is.null(cuts) & !is.null(object$tuneValue))
      {
        cuts <- object$tuneValue$.cut.off.growth[1]
      } else {
        if(is.null(cuts)) stop("please supply a value for 'cuts'")
      }
    tmp <- object$var.importance[,cuts]
    out <- data.frame(Overall = tmp)
    rownames(out) <- names(tmp)
    out
  }

varImp.multinom <- function(object, ...)
  {
    library(nnet)
    out <- abs(coef(object))
    if(is.vector(out))
      {
        out <- data.frame(Overall = out)
        rownames(out) <- names(coef(object))
      } else {
        out <- as.data.frame(apply(out, 2, sum))
        names(out)[1] <- "Overall"
      }
    subset(out, rownames(out) != "(Intercept)")
  }


varImp.gam <- function(object, ...)
  {

    if(any(names(object) %in% c("edf", "mgcv.conv", "gcv.ubre")))
      {
        library(mgcv)
        tmp <- mgcv:::anova.gam(object)
        smoothed <- data.frame(Overall = tmp$s.table[, 4])

        if(nrow(tmp$p.table) > 1)
          {
            linear <- data.frame(Overall = tmp$p.table[, 4])
            out <- rbind(linear, smoothed)
          } else out <- smoothed
        out$Overall[!is.na(out$Overall)] <- -log10(out$Overall[!is.na(out$Overall)])

        out$Overall[is.na(out$Overall)] <- 0

        nms <- strsplit(rownames(out), "[()]")
        nms <- unlist(lapply(nms, function(x) x[length(x)]))
        rownames(out) <- nms
        out <- subset(out, rownames(out) != "Intercept")
      } else {
        library(gam)
        trms <- attr(object$terms, "term.labels")

        vars <- all.vars(object$terms)[-1]
        out <- data.frame(Overall = rep(NA, length(vars)))
        rownames(out) <- vars
        for(i in seq(along = trms))
          {
            reduced <- update(object, as.formula(paste("~.-", trms[i])))
            out[i,1] <- -log10(gam:::anova.gam(object, reduced)[2, "P(>|Chi|)"])
          }
      }
    out
  }


varImp.cubist <- function(object, weights = c(0.5, 0.5), ...)
  {
    if(length(weights) != 2) stop("two weights must be given")
    weights <- weights/sum(weights)
    out <- data.frame(Overall = object$usage$Conditions*weights[1] + object$usage$Model*weights[2])
    rownames(out) <- object$usage$Variable
    out
  }


varImp.RRF <- function(object, ...)
  {
    library(RRF)
    varImp <- RRF:::importance(object, ...)
    if(object$type == "regression")
      varImp <- data.frame(Overall = varImp[,"%IncMSE"])
    else {
      retainNames <- levels(object$y)
      varImp <- varImp[, retainNames]
    }
    
    out <- as.data.frame(varImp)
    if(dim(out)[2] == 2)
      {
        tmp <- apply(out, 1, mean)
        out[,1] <- out[,2] <- tmp  
      }
    out
  }

varImp.JRip <- function(object, ...)
  {
    dat <- ripperRuleSummary(object)
    out <- dat$varUsage[,"Overall", drop = FALSE]
    rownames(out) <- dat$varUsage$Var
    out
  }

varImp.PART <- function(object, ...)
  {
    dat <- partRuleSummary(object)
    out <- dat$varUsage[,"Overall", drop = FALSE]
    rownames(out) <- dat$varUsage$Var
    out
  }


varImp.glm <- function(object, ...)
  {
    sObj <- summary(object)$coef
    sObj <- sObj[rownames(sObj) != "(Intercept)",]
    sObj <- abs(sObj[, "z value", drop = FALSE])
    colnames(sObj) <- "Overall"
    sObj <- as.data.frame(sObj)
    sObj
  }



varImp.C5.0 <- function(object, ...)
  {
    library(C50)
    C5imp(object, ...)
  }

GarsonWeights <- function(object)
  {
    beta <- coef(object)
    abeta <- abs(beta)
    nms <- names(beta)
    i2h <- array(NA, dim = object$n[2:1])
    h2o <- array(NA, dim = object$n[2:3])

    for(hidden in 1:object$n[2])
      {
        for(input in 1:object$n[1])
          {
            label <- paste("i", input, "->h", hidden, sep = "")
            i2h[hidden, input] <- abeta[grep(label, nms, fixed = TRUE)]
          }
      }
    for(hidden in 1:object$n[2])
      {
        for(output in 1:object$n[3])
          {
            label <- paste("h", hidden, "->o",
                           ifelse(object$n[3] == 1, "", output),
                           sep = "")
            h2o[hidden,output] <- abeta[grep(label, nms, fixed = TRUE)]
          }
      }    

    if(FALSE)
      {
        ## Test case from Gevrey, M., Dimopoulos, I., & Lek,
        ## S. (2003). Review and comparison of methods to study the
        ## contribution of variables in artificial neural network
        ## models. ecological modelling, 160(3), 249–264.
        i2h <- matrix(c(-1.67624,  3.29022,  1.32466, 
                        -0.51874, -0.22921, -0.25526,
                        -4.01764,  2.12486, -0.08168,
                        -1.75691, -1.44702,  0.58286),
                      ncol = 3, byrow = TRUE)
        h2o <- matrix(c(4.57857, -0.48815, -5.73901, -2.65221),
                      ncol = 1)
      }

    ##  From Gevrey et al (2003): "For each hidden neuron i, multiply
    ##  the absolute value of the hidden-output layer connection
    ##  weight by the absolute value of the hidden-input layer
    ##  connection weight. Do this for each input variable j. The
    ##  following products Pij are obtained"


    ## We'll do this one response at a time. Gevrey et al (2003) do
    ## not discuss multiple outputs, but the results are the same (at
    ## least in the case of classification).

    imp <- matrix(NA, nrow = object$n[1], ncol = object$n[3])
    

    for(output in 1:object$n[3])
      {
        Pij <- i2h * NA
        for(hidden in 1:object$n[2]) Pij[hidden,] <- i2h[hidden,] * h2o[hidden,output]

        ## "For each hidden neuron, divide Pij by the sum for all the
        ## input variables to obtain Qij. For example for Hidden 1, Q11 =
        ## P11/(P11+P12+P13).
        
        Qij <- Pij * NA
        for(hidden in 1:object$n[2]) Qij[hidden,] <- Pij[hidden,] / sum(Pij[hidden,])
        

        ## "For each input neuron, sum the product Sj formed from the
        ## previous computations of Qij. For example, S1 =
        ## Q11+Q21+Q31+Q41."

        Sj <- apply(Qij, 2, sum)

        ## "Divide Sj by the sum for all the input variables. Expressed as
        ## a percentage, this gives the relative importance or
        ## distribution of all output weights attributable to the given
        ## input variable. For example, for the input neuron 1, the
        ## relative importance is equal to (S1/100)/(S1+S2+S3)"

        imp[,output] <- Sj/sum(Sj)*100
        rm(Pij, Qij, Sj)
      }
    
    colnames(imp) <- if(!is.null(colnames(object$residuals))) colnames(object$residuals) else paste("Y", 1:object$n[3], sep = "")
    rownames(imp) <- if(!is.null(object$coefnames)) object$coefnames else  paste("X", 1:object$n[1], sep = "")
    imp
  }

varImp.nnet <- function(object, ...)
  {
    imp <- GarsonWeights(object, ...)
    if(ncol(imp) > 1)
      {
        imp <- cbind(apply(imp, 1, mean), imp)
        colnames(imp)[1] <- "Overall"
      } else {
        imp <- as.data.frame(imp)
        names(imp) <- "Overall"
      }
    if(!is.null(object$xNames)) rownames(imp) <- object$xNames
    imp
  }



varImp.glmnet <- function(object, lambda = NULL, ...)
{
  library(glmnet)
  if(is.null(lambda))
    {
      if(length(lambda) > 1) stop("Only one value of lambda is allowed right now")
      if(!is.null(object$lambdaOpt))
        {
          lambda <- object$lambdaOpt
        } else stop("must supply a vaue of lambda")
    }
  beta <- predict(object, s = lambda, type = "coef")
  if(is.list(beta))
    {
      out <- do.call("cbind", lapply(beta, function(x) x[,1]))
      out <- as.data.frame(out)
    } else out <- data.frame(Overall = beta[,1])
  out <- out[rownames(out) != "(Intercept)",,drop = FALSE]

  out
}
