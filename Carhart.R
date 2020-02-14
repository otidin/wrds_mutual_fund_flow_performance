# install.packages("quantmod")
library(devtools)
# install_github("braverock/FactorAnalytics")
# rm(list=ls())
# install.packages("robust")
# install.packages("PerformanceAnalytics")
# install.packages("lars")
library(PerformanceAnalytics)
library(robust)
library(lars)
library(quantmod)
library(factorAnalytics)
options(digits=3)
library(factorAnalytics)

########################Carhart 4-Factor Model######################################
# Carhart 4-Factor Model
########################### Compute Excess Returns#########################################
# Compute Excess Returns
returns.z <- read.zoo("timeSeriesReturns.csv",FUN=as.yearmon,header=T,sep=",")
returns.z <- returns.z[which(index(returns.z)>="Jan 2000"),]
ret.z <- returns.z[,1:30]-returns.z[,34] #return -rf
fact.z <- returns.z[,c(31,32,33,35)]

# Fit Carhart 4-Factor Model by Linear Regression
carhart <- lm(ret.z~fact.z)
carhart$coefficients
carhart$residuals
carhart$effects
carhart$rank
carhart$fitted.values

a <- summary(carhart)
rsq <- as.numeric(sapply(a,FUN="[","r.squared"))
png(file="carhart-r2.png",width=7,height=5,units="in",res=300)
par(las=2)
barplot(rsq,main="R-Squared Values for Carhart Model",names.arg=colnames(ret.z),col=4)
dev.off()

# Compute Factor Model Covariance Matrix Estimate
B <- coef(carhart)[-1,]
sigma.F <- cov(fact.z)
rerrv <- as.numeric(sapply(a,FUN="[",i=6))^2
sigma.eps <- diag(rerrv)
sigma <- t(B)%*%sigma.F%*%B+sigma.eps

# Fit Carhart 4-Factor Model via fitTimeSeriesFactorModel
carhart.betas <- fitTimeSeriesFactorModel(colnames(ret.z),colnames(fact.z),returns.z,fit.method="OLS")
png(file="carhartBetas.png",width=7,height=5,units="in",res=300)
par(list(mfrow=c(2,2),las=2))
barplot(carhart.betas$beta[,1],main="Factor Beta for Market Return",names.arg=colnames(ret.z),cex.names=0.6,col=4)
barplot(carhart.betas$beta[,2],main="Factor Beta for Small-minus-Big",names.arg=colnames(ret.z),cex.names=0.6,col=4)
barplot(carhart.betas$beta[,3],main="Factor Beta for High-minus-Low",names.arg=colnames(ret.z),cex.names=0.6,col=4)
barplot(carhart.betas$beta[,4],main="Factor Beta for Up-minus-Down",names.arg=colnames(ret.z),cex.names=0.6,col=4)
dev.off()

# barplot(pca$r2,main="R-Squared Values for PCA Model",names.arg=colnames(ret.z),col=4)
dev.off()

# time series factor analysis ---------------------------------------------

# time series factor model ------------------------------------------------fitTimeSeriesFactorModel <-
function(assets.names, factors.names, data=data, num.factor.subset = 1,
         fit.method=c("OLS","DLS","Robust"),
         variable.selection="none",
         decay.factor = 0.95,nvmax=8,force.in=NULL,
         subsets.method = c("exhaustive", "backward", "forward", "seqrep"),
         lars.criteria = "Cp",add.up.market.returns = FALSE,add.quadratic.term = FALSE,
         excess.market.returns.name ) {

  require(PerformanceAnalytics)
  require(leaps)
  require(lars)
  require(robust)
  require(MASS)
  this.call <- match.call()

  # convert data into xts and hereafter compute in xts
  data.xts <- checkData(data)
  reg.xts <- merge(data.xts[,assets.names],data.xts[,factors.names])
  if (add.up.market.returns == TRUE || add.quadratic.term == TRUE ) {
    reg.xts <- merge(reg.xts,data.xts[,excess.market.returns.name])
  }
  # initialize list object to hold regression objects
  reg.list = list()


  # initialize matrices and vectors to hold estimated betas,
  # residual variances, and R-square values from
  # fitted factor models

  Alphas = ResidVars = R2values = rep(NA, length(assets.names))
  names(Alphas) = names(ResidVars) = names(R2values) = assets.names
  Betas = matrix(NA, length(assets.names), length(factors.names))
  colnames(Betas) = factors.names
  rownames(Betas) = assets.names

  if(add.up.market.returns == TRUE ) {
    Betas <- cbind(Betas,rep(NA,length(assets.names)))
    colnames(Betas)[dim(Betas)[2]] <- "up.beta"
  }

  if(add.quadratic.term == TRUE ) {
    Betas <- cbind(Betas,rep(NA,length(assets.names)))
    colnames(Betas)[dim(Betas)[2]] <- "quadratic.term"
  }

  #
  ### plain vanila method
  #
  if (variable.selection == "none") {
    if (fit.method == "OLS") {
      for (i in assets.names) {
        reg.df = na.omit(reg.xts[, c(i, factors.names)])
        if(add.up.market.returns == TRUE) {
          reg.df$up.beta = reg.df[,excess.market.returns.name]
          reg.df$up.beta[reg.df$up.beta <0] <- rep(0,sum(reg.df$up.beta<0))
        }
        if(add.quadratic.term == TRUE) {
          quadratic.term <- reg.xts[,excess.market.returns.name]^2
          reg.df = merge(reg.df,quadratic.term)
          colnames(reg.df)[dim(reg.df)[2]] <- "quadratic.term"
        }
        fm.formula = as.formula(paste(i,"~", ".", sep=" "))
        fm.fit = lm(fm.formula, data=reg.df)
        fm.summary = summary(fm.fit)
        reg.list[[i]] = fm.fit
        Alphas[i] = coef(fm.fit)[1]
        Betas.names = names(coef(fm.fit)[-1])
        Betas[i,Betas.names] = coef(fm.fit)[-1]
        ResidVars[i] = fm.summary$sigma^2
        R2values[i] =  fm.summary$r.squared
      }
    } else if (fit.method == "DLS") {
      for (i in assets.names) {
        reg.df = na.omit(reg.xts[, c(i, factors.names)])
        if(add.up.market.returns == TRUE) {
          up.beta <- apply(reg.xts[,excess.market.returns.name],1,max,0)
          reg.df = merge(reg.df,up.beta)
        }
        if(add.quadratic.term == TRUE) {
          quadratic.term <- reg.xts[,excess.market.returns.name]^2
          reg.df = merge(reg.df,quadratic.term)
          colnames(reg.df)[dim(reg.df)[2]] <- "quadratic.term"
        }
        t.length <- nrow(reg.df)
        w <- rep(decay.factor^(t.length-1),t.length)
        for (k in 2:t.length) {
          w[k] = w[k-1]/decay.factor
        }
        # sum weigth to unitary
        w <- w/sum(w)
        fm.formula = as.formula(paste(i,"~", ".", sep=""))
        fm.fit = lm(fm.formula, data=reg.df,weights=w)
        fm.summary = summary(fm.fit)
        reg.list[[i]] = fm.fit
        Alphas[i] = coef(fm.fit)[1]
        Betas.names = names(coef(fm.fit)[-1])
        Betas[i,Betas.names] = coef(fm.fit)[-1]
        ResidVars[i] = fm.summary$sigma^2
        R2values[i] =  fm.summary$r.squared
      }
    } else if (fit.method=="Robust") {
      for (i in assets.names) {
        reg.df = na.omit(reg.xts[, c(i, factors.names)])
        if(add.up.market.returns == TRUE) {
          up.beta <- apply(reg.xts[,excess.market.returns.name],1,max,0)
          reg.df = merge(reg.df,up.beta)
        }
        if(add.quadratic.term == TRUE) {
          quadratic.term <- reg.xts[,excess.market.returns.name]^2
          reg.df = merge(reg.df,quadratic.term)
          colnames(reg.df)[dim(reg.df)[2]] <- "quadratic.term"
        }
        fm.formula = as.formula(paste(i,"~", ".", sep=" "))
        fm.fit = lmRob(fm.formula, data=reg.df)
        fm.summary = summary(fm.fit)
        reg.list[[i]] = fm.fit
        Alphas[i] = coef(fm.fit)[1]
        Betas[i, ] = coef(fm.fit)[-1]
        ResidVars[i] = fm.summary$sigma^2
        R2values[i] =  fm.summary$r.squared
      }

    }  else {
      stop("invalid method")
    }

    #
    ### subset methods
    #
  }
  else if (variable.selection == "all subsets") {
    # estimate multiple factor model using loop b/c of unequal histories for the hedge funds

    if (fit.method == "OLS") {

      if (num.factor.subset == length(force.in)) {
        for (i in assets.names) {
          reg.df = na.omit(reg.xts[, c(i, force.in)])
          if(add.up.market.returns == TRUE) {
            up.beta <- apply(reg.xts[,excess.market.returns.name],1,max,0)
            reg.df = merge(reg.df,up.beta)
          }
          if(add.quadratic.term == TRUE) {
            quadratic.term <- reg.xts[,excess.market.returns.name]^2
            reg.df = merge(reg.df,quadratic.term)
            colnames(reg.df)[dim(reg.df)[2]] <- "quadratic.term"
          }
          fm.formula = as.formula(paste(i,"~", ".", sep=" "))
          fm.fit = lm(fm.formula, data=reg.df)
          fm.summary = summary(fm.fit)
          reg.list[[i]] = fm.fit
          Alphas[i] = coef(fm.fit)[1]
          Betas.names = names(coef(fm.fit)[-1])
          Betas[i,Betas.names] = coef(fm.fit)[-1]
          ResidVars[i] = fm.summary$sigma^2
          R2values[i] =  fm.summary$r.squared
        }
      }  else if (num.factor.subset > length(force.in)) {

        for (i in assets.names) {
          reg.df = na.omit(reg.xts[, c(i, factors.names)])
          fm.formula = as.formula(paste(i,"~", ".", sep=" "))
          fm.subsets <- regsubsets(fm.formula,data=reg.df,nvmax=nvmax,force.in=force.in,
                                   method=subsets.method)
          sum.sub <- summary(fm.subsets)
          reg.df <- na.omit(reg.xts[,c(i,names(which(sum.sub$which[as.character(num.factor.subset),-1]==TRUE))  )])
          if(add.up.market.returns == TRUE) {
            up.beta <- apply(reg.xts[,excess.market.returns.name],1,max,0)
            reg.df = merge(reg.df,up.beta)
          }
          if(add.quadratic.term == TRUE) {
            quadratic.term <- reg.xts[,excess.market.returns.name]^2
            reg.df = merge(reg.df,quadratic.term)
            colnames(reg.df)[dim(reg.df)[2]] <- "quadratic.term"
          }
          fm.fit = lm(fm.formula, data=reg.df)
          fm.summary = summary(fm.fit)
          reg.list[[i]] = fm.fit
          Alphas[i] = coef(fm.fit)[1]
          Betas.names = names(coef(fm.fit)[-1])
          Betas[i,Betas.names] = coef(fm.fit)[-1]
          ResidVars[i] = fm.summary$sigma^2
          R2values[i] =  fm.summary$r.squared
        }
      } else {
        stop("ERROR! number of force.in should less or equal to num.factor.subset")
      }




    }
    else if (fit.method == "DLS"){


      if (num.factor.subset == length(force.in)) {
        # define weight matrix
        for (i in assets.names) {
          reg.df = na.omit(reg.xts[, c(i, force.in)])
          if(add.up.market.returns == TRUE) {
            up.beta <- apply(reg.xts[,excess.market.returns.name],1,max,0)
            reg.df = merge(reg.df,up.beta)
          }
          if(add.quadratic.term == TRUE) {
            quadratic.term <- reg.xts[,excess.market.returns.name]^2
            reg.df = merge(reg.df,quadratic.term)
            colnames(reg.df)[dim(reg.df)[2]] <- "quadratic.term"
          }
          t.length <- nrow(reg.df)
          w <- rep(decay.factor^(t.length-1),t.length)
          for (k in 2:t.length) {
            w[k] = w[k-1]/decay.factor
          }
          # sum weigth to unitary
          w <- w/sum(w)
          fm.formula = as.formula(paste(i,"~", ".", sep=""))
          fm.fit = lm(fm.formula, data=reg.df,weights=w)
          fm.summary = summary(fm.fit)
          reg.list[[i]] = fm.fit
          Alphas[i] = coef(fm.fit)[1]
          Betas.names = names(coef(fm.fit)[-1])
          Betas[i,Betas.names] = coef(fm.fit)[-1]
          ResidVars[i] = fm.summary$sigma^2
          R2values[i] =  fm.summary$r.squared
        }
      } else if  (num.factor.subset > length(force.in)) {
        for (i in assets.names) {
          reg.df = na.omit(reg.xts[, c(i, factors.names)])
          t.length <- nrow(reg.df)
          w <- rep(decay.factor^(t.length-1),t.length)
          for (k in 2:t.length) {
            w[k] = w[k-1]/decay.factor
          }
          w <- w/sum(w)
          fm.formula = as.formula(paste(i,"~", ".", sep=""))
          fm.subsets <- regsubsets(fm.formula,data=reg.df,nvmax=nvmax,force.in=force.in,
                                   method=subsets.method,weights=w) # w is called from global envio
          sum.sub <- summary(fm.subsets)
          reg.df <- na.omit(reg.xts[,c(i,names(which(sum.sub$which[as.character(num.factor.subset),-1]==TRUE))  )])
          if(add.up.market.returns == TRUE) {
            up.beta <- apply(reg.xts[,excess.market.returns.name],1,max,0)
            reg.df = merge(reg.df,up.beta)
          }
          if(add.quadratic.term == TRUE) {
            quadratic.term <- reg.xts[,excess.market.returns.name]^2
            reg.df = merge(reg.df,quadratic.term)
            colnames(reg.df)[dim(reg.df)[2]] <- "quadratic.term"
          }
          fm.fit = lm(fm.formula, data=reg.df,weights=w)
          fm.summary = summary(fm.fit)
          reg.list[[i]] = fm.fit
          Alphas[i] = coef(fm.fit)[1]
          Betas.names = names(coef(fm.fit)[-1])
          Betas[i,Betas.names] = coef(fm.fit)[-1]
          ResidVars[i] = fm.summary$sigma^2
          R2values[i] =  fm.summary$r.squared
        }
      } else {
        stop("ERROR! number of force.in should less or equal to num.factor.subset")
      }


    }
    else if (fit.method=="Robust") {
      for (i in assets.names) {
        reg.df = na.omit(reg.xts[, c(i, factors.names)])
        if(add.up.market.returns == TRUE) {
          up.beta <- apply(reg.xts[,excess.market.returns.name],1,max,0)
          reg.df = merge(reg.df,up.beta)
        }
        if(add.quadratic.term == TRUE) {
          quadratic.term <- reg.xts[,excess.market.returns.name]^2
          reg.df = merge(reg.df,quadratic.term)
          colnames(reg.df)[dim(reg.df)[2]] <- "quadratic.term"
        }
        fm.formula = as.formula(paste(i,"~", ".", sep=" "))
        fm.fit = lmRob(fm.formula, data=reg.df)
        fm.summary = summary(fm.fit)
        reg.list[[i]] = fm.fit
        Alphas[i] = coef(fm.fit)[1]
        Betas[i, ] = coef(fm.fit)[-1]
        ResidVars[i] = fm.summary$sigma^2
        R2values[i] =  fm.summary$r.squared
      }

    }  else {
      stop("invalid method")
    }


  }
  else if (variable.selection == "stepwise") {

    if (fit.method == "OLS") {
      # loop over all assets and estimate time series regression
      for (i in assets.names) {
        reg.df = na.omit(reg.xts[, c(i, factors.names)])
        if(add.up.market.returns == TRUE) {
          up.beta <- apply(reg.xts[,excess.market.returns.name],1,max,0)
          reg.df = merge(reg.df,up.beta)
        }
        if(add.quadratic.term == TRUE) {
          quadratic.term <- reg.xts[,excess.market.returns.name]^2
          reg.df = merge(reg.df,quadratic.term)
          colnames(reg.df)[dim(reg.df)[2]] <- "quadratic.term"
        }
        fm.formula = as.formula(paste(i,"~", ".", sep=" "))
        fm.fit = step(lm(fm.formula, data=reg.df),trace=0)
        fm.summary = summary(fm.fit)
        reg.list[[i]] = fm.fit
        Alphas[i] = coef(fm.fit)[1]
        Betas.names = names(coef(fm.fit)[-1])
        Betas[i,Betas.names] = coef(fm.fit)[-1]
        ResidVars[i] = fm.summary$sigma^2
        R2values[i] =  fm.summary$r.squared
      }


    }
    else if (fit.method == "DLS"){
      # define weight matrix
      for (i in assets.names) {
        reg.df = na.omit(reg.xts[, c(i, factors.names)])
        if(add.up.market.returns == TRUE) {
          up.beta <- apply(reg.xts[,excess.market.returns.name],1,max,0)
          reg.df = merge(reg.df,up.beta)
        }
        if(add.quadratic.term == TRUE) {
          quadratic.term <- reg.xts[,excess.market.returns.name]^2
          reg.df = merge(reg.df,quadratic.term)
          colnames(reg.df)[dim(reg.df)[2]] <- "quadratic.term"
        }
        t.length <- nrow(reg.df)
        w <- rep(decay.factor^(t.length-1),t.length)
        for (k in 2:t.length) {
          w[k] = w[k-1]/decay.factor
        }
        # sum weigth to unitary
        w <- w/sum(w)
        fm.formula = as.formula(paste(i,"~", ".", sep=""))
        fm.fit = step(lm(fm.formula, data=reg.df,weights=w),trace=0)
        fm.summary = summary(fm.fit)
        reg.list[[i]] = fm.fit
        Alphas[i] = coef(fm.fit)[1]
        Betas.names = names(coef(fm.fit)[-1])
        Betas[i,Betas.names] = coef(fm.fit)[-1]
        ResidVars[i] = fm.summary$sigma^2
        R2values[i] =  fm.summary$r.squared
      }

    }
    else if (fit.method =="Robust") {
      for (i in assets.names) {
        assign("reg.df" , na.omit(reg.xts[, c(i, factors.names)]),envir = .GlobalEnv )
        #       reg.df = na.omit(reg.xts[, c(i, factors.names)],envir = .GlobalEnv)
        if(add.up.market.returns == TRUE) {
          stop("This function does not support add.up.market.returns and stepwise variable.selection
               together Please choose either one.")
          up.beta <- apply(reg.xts[,excess.market.returns.name],1,max,0)
          reg.df = merge(reg.df,up.beta)
        }
        if(add.quadratic.term == TRUE) {
          stop("This function does not support add.up.market.returns and stepwise variable.selection
               together. Please choose either one.")
          quadratic.term <- reg.xts[,excess.market.returns.name]^2
          reg.df = merge(reg.df,quadratic.term)
          colnames(reg.df)[dim(reg.df)[2]] <- "quadratic.term"
        }
        fm.formula = as.formula(paste(i,"~", ".", sep=" "))
        lmRob.obj <- lmRob(fm.formula, data=reg.df)
        fm.fit = step.lmRob(lmRob.obj,trace=FALSE)
        fm.summary = summary(fm.fit)
        reg.list[[i]] = fm.fit
        Alphas[i] = coef(fm.fit)[1]
        Betas.names = names(coef(fm.fit)[-1])
        Betas[i,Betas.names] = coef(fm.fit)[-1]
        ResidVars[i] = fm.summary$sigma^2
        R2values[i] =  fm.summary$r.squared
      }

    }

    } else if (variable.selection == "lar" | variable.selection == "lasso") {
      # use min Cp as criteria to choose predictors

      for (i in assets.names) {
        reg.df = na.omit(reg.xts[, c(i, factors.names)])
        if(add.up.market.returns == TRUE) {
          up.beta <- apply(reg.xts[,excess.market.returns.name],1,max,0)
          reg.df = merge(reg.df,up.beta)
        }
        if(add.quadratic.term == TRUE) {
          quadratic.term <- reg.xts[,excess.market.returns.name]^2
          reg.df = merge(reg.df,quadratic.term)
          colnames(reg.df)[dim(reg.df)[2]] <- "quadratic.term"
        }
        reg.df = as.matrix(na.omit(reg.df))
        lars.fit = lars(reg.df[,factors.names],reg.df[,i],type=variable.selection,trace=FALSE)
        sum.lars <- summary(lars.fit)
        if (lars.criteria == "cp") {
          s<- which.min(sum.lars$Cp)
        } else {
          lars.cv <- cv.lars(reg.df[,factors.names],reg.df[,i],trace=FALSE,
                             type=variable.selection,mode="step",plot.it=FALSE)
          s<- which.min(lars.cv$cv)
        }
        coef.lars <- predict(lars.fit,s=s,type="coef",mode="step")
        reg.list[[i]] = lars.fit
        fitted <- predict(lars.fit,reg.df[,factors.names],s=s,type="fit",mode="step")
        Alphas[i] = (fitted$fit - reg.df[,factors.names]%*%coef.lars$coefficients)[1]
        Betas.names = names(coef.lars$coefficients)
        Betas[i,Betas.names] = coef.lars$coefficients
        ResidVars[i] = sum.lars$Rss[s]/(nrow(reg.df)-s)
        R2values[i] =  lars.fit$R2[s]
      }

    }  else  {
      stop("wrong method")
    }





  # return results
  # add option to return list
  ans = list (asset.fit = reg.list,
              alpha = Alphas,
              beta  = Betas,
              r2    = R2values,
              resid.variance = ResidVars,
              call      = this.call,
              data = data,
              factors.names = factors.names,
              variable.selection = variable.selection,
              assets.names = assets.names)
  class(ans) = "TimeSeriesFactorModel"
  return(ans)
}
