# Code for "Community Vibrancy and Its Relationship
# Authors: Wichinpong Park Sinchaisri and Shane T Jensen
# Last updated: 20210915
# Set up

# Load relevant libraries
{ # Import libraries
  library(data.table)
  library(corrplot)
  library(stargazer)
  library(xtable)
  library(rgdal) # reading a shape file
  library(sp)
  library(lubridate) # to deal with difftime for a vector
  library(gplots) # plotmeans
  library(ggplot2)
  library(plyr)
  library(dplyr)
  library(sp)
  library(sf)
  library(Hmisc) # cut2 + rcorr
  library(tidyverse) # %>%
  library(rgeos) # for gContains
  library(plm) # fixed effects lm
  library(betareg) # beta regression
  library(lme4)
  library(MASS)
  library(foreign)
  library(pscl) # for vuong
  library(lmtest) # for lrtest
  library(tableone) # for absolte standardized diff
  library(optmatch)
  library(MASS)
  library(RItools)
  library(MatchIt)
  
  options(scipen = 999)
  
  savepdf <- function(file, width=16, height=10)
  {
    fname <- paste("figures/",file,".pdf",sep="")
    pdf(fname, width=width/2.54, height=height/2.54,
        pointsize=10)
    par(mgp=c(2.5,1,0), tcl=-0.4, mar=c(4,5,2.5,1.1))
    # par(mgp=c(2.2,0.45,0), tcl=-0.4, mar=c(3.3,3.6,1.1,1.1))
  }
  
  savepdf3 <- function(file, width=16, height=12)
  {
    fname <- paste("figures/",file,".pdf",sep="")
    pdf(fname, width=width/2.54, height=height/2.54,
        pointsize=10)
    par(mgp=c(3.25,1.25,0), tcl=-0.4, mar=c(5,5,3,1.1))
    # par(mgp=c(2.2,0.45,0), tcl=-0.4, mar=c(3.3,3.6,1.1,1.1))
  }
  
  savepdfwide <- function(file, width=35, height=10) {
    fname <- paste("figures/", file,".pdf",sep="")
    pdf(fname, width=width/2.54, height=height/2.54,
        pointsize=10)
    par(mgp=c(2.5,1,0), tcl=-0.4, mar=c(4,5,1.1,1.1))
    # par(mgp=c(2.2,0.45,0), tcl=-0.4, mar=c(3.3,3.6,1.1,1.1))
  }
  
  # flattenCorrMatrix
  # cormat : matrix of the correlation coefficients
  # pmat : matrix of the correlation p-values
  flattenCorrMatrix <- function(cormat, pmat) {
    ut <- upper.tri(cormat)
    data.frame(
      row = rownames(cormat)[row(cormat)[ut]],
      column = rownames(cormat)[col(cormat)[ut]],
      cor  =(cormat)[ut],
      p = pmat[ut]
    )
  }
  
  # Mahalanobis function
  # Prevents an outlier from inflating the variance for a variable, thereby decreasing its importance.
  # z is a vector, length(z)=n, with z=1 for treated, z=0 for control
  # X is a matrix with n rows containing variables in the distance
  
  smahal = function(z,X){
    
    X <- as.matrix(X)
    n <- dim(X)[1] # number of all observations
    rownames(X) <- 1:n
    k <- dim(X)[2]
    m <- sum(z) # = number of treatment
    # for each column, update to rank of itself
    for (j in 1:k) X[,j] <- rank(X[,j]) # returns the sample ranks, 1 is smallest, 10 is the biggest
    
    cv <- cov(X) # covariance of X
    vuntied <- var(1:n)
    rat <- sqrt(vuntied/diag(cv))
    cv <- diag(rat)%*%cv%*%diag(rat)
    
    out <- matrix(NA,m,n-m)
    Xc <- X[z==0,] # control 
    Xt <- X[z==1,] # treated
    
    rownames(out)<-rownames(X)[z==1]
    colnames(out)<-rownames(X)[z==0]
    
    icov <- ginv(cv) # generalized inverse, Moore-Penrose
    
    # for each of the control variable, compute mahalanobis distance between 
    for (i in 1:m) out[i,] <- mahalanobis(Xc, Xt[i,], icov,inverted=T) # return squared MH distance of all rows in x AND the vector mu = center with respect fo sigma = cov -> D^2 = (x - mu)' inverse sigma (x-mu)
    # for each treated unit.
    return(out)
  }
  
  # Use the actual values rather than rank
  smahal2 = function(z,X){
    
    X <- as.matrix(X)
    n <- dim(X)[1] # number of all observations
    rownames(X) <- 1:n
    k <- dim(X)[2]
    m <- sum(z) # = number of treatment
    
    cv <- cov(X) # covariance of X
    vuntied <- var(1:n)
    rat <- sqrt(vuntied/diag(cv))
    cv <- diag(rat)%*%cv%*%diag(rat)
    
    out <- matrix(NA,m,n-m)
    Xc <- X[z==0,] # control 
    Xt <- X[z==1,] # treated
    
    rownames(out)<-rownames(X)[z==1]
    colnames(out)<-rownames(X)[z==0]
    
    icov <- ginv(cv) # generalized inverse, Moore-Penrose
    
    # for each of the control variable, compute mahalanobis distance between 
    for (i in 1:m) out[i,] <- mahalanobis(Xc, Xt[i,], icov,inverted=T) # return squared MH distance of all rows in x AND the vector mu = center with respect fo sigma = cov -> D^2 = (x - mu)' inverse sigma (x-mu)
    # for each treated unit.
    return(out)
  }
  
  # Function for adding a propensity score caliper to a distance matrix dmat
  # calipersd is the caliper in terms of standard deviation of the logit propensity scoe
  
  addcaliper <- function(dmat, z, logitp, calipersd, penalty){
    # Pooled within group standard deviation
    
    sd.logitp <- sqrt((sd(logitp[z==1])^2+sd(logitp[z==0])^2)/2)
    adif <- abs(outer(logitp[z==1],logitp[z==0],"-"))
    adif <- (adif - (calipersd*sd.logitp)) * (adif > (calipersd*sd.logitp)) # if adif is within limit, then it's 0, otherwise, it will be the difference between absolute difference and our caliper * sd.logit
    dmat <- dmat + adif*penalty
    return(dmat)
    
  }
  
  # Drop observations with missing values from the calculations
  # stratum.myindex should contain strata for each subject, 0 means a unit was not 
  # matched
  standardized.diff.func=function(x,treatment,stratum.myindex,missing=rep(0,length(x))){
    xtreated=x[treatment==1 & missing==0];
    xcontrol=x[treatment==0 & missing==0];
    var.xtreated=var(xtreated);
    var.xcontrol=var(xcontrol);
    combinedsd=sqrt(.5*(var.xtreated+var.xcontrol));
    std.diff.before.matching=(mean(xtreated)-mean(xcontrol))/combinedsd;
    nostratum=length(unique(stratum.myindex))-1*max(stratum.myindex==0);
    if(max(stratum.myindex==0)==0){
      stratumlist=sort(unique(stratum.myindex))
    }
    if(max(stratum.myindex==0)==1){
      templist=sort(unique(stratum.myindex))
      stratumlist=templist[-1]
    }
    diff.in.stratum=rep(0,nostratum);
    number.in.stratum=rep(0,nostratum);
    for(i in 1:nostratum){
      if(sum(stratum.myindex==stratumlist[i] & treatment==1 & missing==0)==0 | sum(stratum.myindex==stratumlist[i] & treatment==0 & missing==0)==0){
        number.in.stratum[i]=0
      }
      if(sum(stratum.myindex==stratumlist[i] & treatment==1 & missing==0)>0 & sum(stratum.myindex==stratumlist[i] & treatment==0 & missing==0)>0){
        diff.in.stratum[i]=mean(x[stratum.myindex==stratumlist[i] & treatment==1 & missing==0])-mean(x[stratum.myindex==stratumlist[i] & treatment==0 & missing==0]);
        number.in.stratum[i]=sum(stratum.myindex==stratumlist[i])
      }
    }
    std.diff.after.matching=(sum(number.in.stratum*diff.in.stratum)/sum(number.in.stratum))/combinedsd;
    list(std.diff.before.matching=std.diff.before.matching,std.diff.after.matching=std.diff.after.matching);
  }
  
}