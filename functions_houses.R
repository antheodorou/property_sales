#check the assumptions
assum_fun <- function(model2){
  #Normality
  library(nortest)
  library(car)
  library(randtests)
  par(mfrow = c(2,2))
  plot(model2, which = 2)
  plot(model2, which = 3)
  residualPlot(model2, type='rstudent')
  plot(rstudent(model2), type='l')
  
  res <- model2$residuals
  
  normality.pvalues <- matrix( nrow=3,ncol=2)
  row.names(normality.pvalues) <- c( 'Unstandardized','Standardized', 'Ext. Studentized' )
  colnames(normality.pvalues) <- c( 'Lillie KS', 'SW' )
  allres <- list()
  allres[[1]] <- res
  allres[[2]] <- rstandard(model2)
  allres[[3]] <- rstudent(model2)
  for (i in 1:3){
    res <- allres[[i]]
    normality.pvalues[i,1]<-lillie.test(res)$p.value
    normality.pvalues[i,2]<-shapiro.test(res)$p.value
  }
  print(normality.pvalues)
  #Costant variance 
  Stud.residuals <- rstudent(model2)
  yhat <- fitted(model2)
  par(mfrow=c(1,2))
  plot(yhat, Stud.residuals)
  abline(h=c(-2,2), col=2, lty=2)
  plot(yhat, Stud.residuals^2)
  abline(h=4, col=2, lty=2)
  print(ncvTest(model2))
  yhat.quantiles<-cut(yhat, breaks=quantile(yhat, probs=seq(0,1,0.25)), dig.lab=6)
  table(yhat.quantiles)
  print(leveneTest(rstudent(model2)~yhat.quantiles))
  par(mfrow=c(1,1))
  boxplot(rstudent(model2)~yhat.quantiles)
  qyhat<- cut( yhat, breaks= 4 )
  #print(bartlett.test( rstandard(model2)~qyhat ))
  print(fligner.test( rstandard(model2)~qyhat ))
  #Non linearity
  print(residualPlots(model2, plot=F, type = "rstudent"))
  print("Independence\n")
  #Independence
  print(runs.test(model2$res))
  print(durbinWatsonTest(model2))
}

#outliers
out_fun <- function(model1, houses){
  print(outlierTest(model1))
  cooks<- cooks.distance(model1)
  critical.value <- 4/(nrow(houses)- length(model1$coef))
  plot(cooks, main='Original data',ylab="Cook's distance", ylim=range(c(0,cooks, critical.value )) )
  abline(h=critical.value,col=2,lty=2)
}
#show each level of the factors
debug_contr_error <- function (dat, subset_vec = NULL) {
  if (!is.null(subset_vec)) {
    ## step 0
    if (mode(subset_vec) == "logical") {
      if (length(subset_vec) != nrow(dat)) {
        stop("'logical' `subset_vec` provided but length does not match `nrow(dat)`")
      }
      subset_log_vec <- subset_vec
    } else if (mode(subset_vec) == "numeric") {
      ## check range
      ran <- range(subset_vec)
      if (ran[1] < 1 || ran[2] > nrow(dat)) {
        stop("'numeric' `subset_vec` provided but values are out of bound")
      } else {
        subset_log_vec <- logical(nrow(dat))
        subset_log_vec[as.integer(subset_vec)] <- TRUE
      } 
    } else {
      stop("`subset_vec` must be either 'logical' or 'numeric'")
    }
    dat <- base::subset(dat, subset = subset_log_vec)
  } else {
    ## step 1
    dat <- stats::na.omit(dat)
  }
  if (nrow(dat) == 0L) warning("no complete cases")
  ## step 2
  
  var_mode <- sapply(dat, mode)
  if (any(var_mode %in% c("complex", "raw"))) stop("complex or raw not allowed!")
  var_class <- sapply(dat, class)
  if (any(var_mode[var_class == "AsIs"] %in% c("logical", "character"))) {
    stop("matrix variables with 'AsIs' class must be 'numeric'")
  }
  ind1 <- which(var_mode %in% c("logical", "character"))
  dat[ind1] <- lapply(dat[ind1], as.factor)
  ## step 3
  fctr <- which(sapply(dat, is.factor))
  if (length(fctr) == 0L) warning("no factor variables to summary")
  ind2 <- if (length(ind1) > 0L) fctr[-ind1] else fctr
  dat[ind2] <- lapply(dat[ind2], base::droplevels.factor)
  ## step 4
  lev <- lapply(dat[fctr], base::levels.default)
  nl <- lengths(lev)
  ## return
  list(nlevels = nl, levels = lev)
}