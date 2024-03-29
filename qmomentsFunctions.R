# # # # # # # # # # # # # # # # # # # # # # #
#   Functions to run urchins_q_moments.R    #
#  and urchins_tortuosity_classic.R scripts #
#                                           #
#                Jordi F. Pagès             #
#                September 2020             #
# # # # # # # # # # # # # # # # # # # # # # # 


# Function to calculate long-range correlations - q moments (following methods by Seuront & Stanley, PNAS) ----
qmom <- function(granMAT){
  noms <- as.character(unique(granMAT$ID))
  llistaExponents <- matrix(nrow = 1, ncol = 4)
  llistaExponents <- as.data.frame(llistaExponents)
  names(llistaExponents) <-  c("ID", "exponents", "num", "conf.int")
  # This first loop selects a single sea urchin every time it loops.
  for(m in 1:length(noms)){
    graella <- NULL
    MAT <- subset(granMAT, ID == noms[m])
    # In this second for, we select a value for tau
    for(j in 4:(length(MAT$x)-1)){
      tau <- j
      matriu <- matrix(nrow = 1, ncol = 1)
      matriu <- as.data.frame(matriu)
      names(matriu) <-  c("tau")
      #  And in this 3rd loop, we calculate the distance between increments (see Seuront and Stanley PNAS) 
      for(i in 1:(length(MAT$x)-j)){
        if(i+tau <= length(MAT$x)){
          d <- sqrt((MAT$x[i+tau]-MAT$x[i])^2+(MAT$y[i+tau]-MAT$y[i])^2)
          matriu[i, 1] <- tau
          # Finally, in this 4th loop, we calculate the qth moment (q being between 0 and 8) for each d
          for(q in 0:8){
            matriu[i, q+2] <- d^(q)
            names(matriu)[q+2] <- paste("q", q, sep = "")
          }
        }
      }
      graella <- rbind(graella, matriu)
    }
    
    # We now calculate the exponents of the regression between mean.dist and tau
    tot.expon <- matrix(nrow = 1, ncol = 4)
    tot.expon <- as.data.frame(tot.expon)
    names(tot.expon) <-  c("ID", "exponents", "num", "conf.int")
    for(r in 2:length(names(graella))){
      mean.dist <- tapply(graella[, r], graella$tau, mean)
      mean.dist <- as.numeric(mean.dist)
      a <- lm(log(mean.dist)~log(unique(graella$tau)))
      exponent <- as.numeric(coef(a)[2])
      conf.int <- as.numeric(summary(a)$coefficients[4])
      tot.expon[r-1,1] <- noms[m]
      tot.expon[r-1,2] <- exponent
      tot.expon[r-1,3] <- r-2
      tot.expon[r-1,4] <- conf.int
    }
    llistaExponents <- rbind(llistaExponents, tot.expon)
  }
  
  llistaExponents <- llistaExponents[-1,] # We get rid of the first row, which is always NA.
  llistaExponents
}

###

loglogdata <- function(granMAT, nom){
  graella <- NULL
  MAT <- filter(granMAT, ID == nom)
    # In this for, we select a value for tau
    for(j in 4:(length(MAT$x)-1)){
      tau <- j
      matriu <- matrix(nrow = 1, ncol = 1)
      matriu <- as.data.frame(matriu)
      names(matriu) <-  c("tau")
      #  And in this loop, we calculate the distance between increments (see Seuront and Stanley PNAS) 
      for(i in 1:(length(MAT$x)-j)){
        if(i+tau <= length(MAT$x)){
          d <- sqrt((MAT$x[i+tau]-MAT$x[i])^2+(MAT$y[i+tau]-MAT$y[i])^2)
          matriu[i, 1] <- tau
          # Finally, in this loop, we calculate the qth moment (q being between 0 and 8) for each d
          for(q in 0:8){
            matriu[i, q+2] <- d^(q)
            names(matriu)[q+2] <- paste("q", q, sep = "")
          }
        }
      }
      graella <- rbind(graella, matriu)
    }
  graella
  }


####

qmom1 <- function(granMAT, nom){
  llistaExponents <- matrix(nrow = 1, ncol = 4)
  llistaExponents <- as.data.frame(llistaExponents)
  names(llistaExponents) <-  c("ID", "exponents", "num", "conf.int")
  # This first loop selects a single sea urchin every time it loops.
    graella <- NULL
    MAT <- filter(granMAT, ID == nom)
    # In this second for, we select a value for tau
    for(j in 4:(length(MAT$x)-1)){
      tau <- j
      matriu <- matrix(nrow = 1, ncol = 1)
      matriu <- as.data.frame(matriu)
      names(matriu) <-  c("tau")
      #  And in this 3rd loop, we calculate the distance between increments (see Seuront and Stanley PNAS) 
      for(i in 1:(length(MAT$x)-j)){
        if(i+tau <= length(MAT$x)){
          d <- sqrt((MAT$x[i+tau]-MAT$x[i])^2+(MAT$y[i+tau]-MAT$y[i])^2)
          matriu[i, 1] <- tau
          # Finally, in this 4th loop, we calculate the qth moment (q being between 0 and 8) for each d
          for(q in 0:8){
            matriu[i, q+2] <- d^(q)
            names(matriu)[q+2] <- paste("q", q, sep = "")
          }
        }
      }
      graella <- rbind(graella, matriu)
    }
    
    # We now calculate the exponents of the regression between mean.dist and tau
    tot.expon <- matrix(nrow = 1, ncol = 4)
    tot.expon <- as.data.frame(tot.expon)
    names(tot.expon) <-  c("ID", "exponents", "num", "conf.int")
    for(r in 2:length(names(graella))){
      mean.dist <- tapply(graella[, r], graella$tau, mean)
      mean.dist <- as.numeric(mean.dist)
      a <- lm(log(mean.dist)~log(unique(graella$tau)))
      exponent <- as.numeric(coef(a)[2])
      conf.int <- as.numeric(summary(a)$coefficients[4])
      tot.expon[r-1,1] <- nom
      tot.expon[r-1,2] <- exponent
      tot.expon[r-1,3] <- r-2
      tot.expon[r-1,4] <- conf.int
    }
  tot.expon  
}


###

# Function to calculate the slope of the q moments line for each individual urchin ----
indiv_exp <- function(listExp){
  noms <-unique(listExp$ID)
  tot <- matrix(ncol = 2, nrow = 0)
  tot <- as.data.frame(tot)
  names(tot) <- c("ID", "coef")
  for(i in 1:length(noms)){
    indiv <- subset(listExp, ID == noms[i])
    regre <- lm(indiv$exponents~indiv$num)
    coeficients <- as.numeric(coef(regre)[2])
    tot[i, 1] <- noms[i]
    tot[i, 2] <- coeficients
  }
  tot
}


###

# Standard error function ----
std.error <- function(x, na.rm = FALSE) {
  sd(x, na.rm = T)/sqrt(length(x))
}


###

# Functions to make final model validations (qqplot and plot to inspect residual heterogeneity)
mcheck <- function (obj, ... ) {  
  rs <- resid(obj)
  fv<-fitted(obj)
  par(mfrow=c(1,2))
  require(car)
  plot(fv,rs,xlab="Fitted values",ylab="Residuals")
  abline(h=0, lty=2)
  qqPlot(rs,xlab="Normal scores",ylab="Ordered residuals")
  par(mfrow=c(1,1))
  invisible(NULL)}

mcheck2 <- function (obj, ... ) {  
  rs <- resid(obj)
  rs2 <- resid(obj, type = "pearson")
  fv<-fitted(obj)
  par(mfrow=c(2,2))
  require(car)
  plot(fv,rs,xlab="Fitted values",ylab="Residuals")
  abline(h=0, lty=2)
  qqPlot(rs,xlab="Normal scores",ylab="Ordered residuals")
  plot(fv,rs2,xlab="Fitted values",ylab="Residuals Pearson")
  par(mfrow=c(1,1))
  invisible(NULL)}




#########
# New function with logbinning to better estimate linear slopes from log log plots
# Function to calculate long-range correlations - q moments (following methods by Seuront & Stanley, PNAS) ----
# granMAT <- urch.null.MAT

qmom_logbin <- function(granMAT){
  noms <- as.character(unique(granMAT$ID))
  llistaExponents <- matrix(nrow = 1, ncol = 4)
  llistaExponents <- as.data.frame(llistaExponents)
  names(llistaExponents) <-  c("ID", "exponents", "num", "conf.int")
  # This first loop selects a single sea urchin every time it loops.
  for(m in 1:length(noms)){
    graella <- NULL
    MAT <- subset(granMAT, ID == noms[m])
    # We now create a power vector with base 2, to have equally spaced taus in log log plots.
    l <- length(4:(length(MAT$x)-1))
    power_tau_list <- 2^(1:10)
    power_tau_OK <- power_tau_list[which(power_tau_list<=l)] 
    # In this second for, we select a value for tau
    for(j in 1:length(power_tau_OK)){
      tau <- power_tau_OK[j]
      matriu <- matrix(nrow = 1, ncol = 1)
      matriu <- as.data.frame(matriu)
      names(matriu) <-  c("tau")
      #  And in this 3rd loop, we calculate the distance between increments (see Seuront and Stanley PNAS) 
      for(i in 1:(length(MAT$x)-j)){
        if(i+tau <= length(MAT$x)){
          d <- sqrt((MAT$x[i+tau]-MAT$x[i])^2+(MAT$y[i+tau]-MAT$y[i])^2)
          matriu[i, 1] <- tau
          # Finally, in this 4th loop, we calculate the qth moment (q being between 0 and 8) for each d
          for(q in 0:8){
            matriu[i, q+2] <- d^(q)
            names(matriu)[q+2] <- paste("q", q, sep = "")
          }
        }
      }
      graella <- rbind(graella, matriu)
    }
    
    # We now calculate the exponents of the regression between mean.dist and tau
    tot.expon <- matrix(nrow = 1, ncol = 4)
    tot.expon <- as.data.frame(tot.expon)
    names(tot.expon) <-  c("ID", "exponents", "num", "conf.int")
    for(r in 2:length(names(graella))){
      mean.dist <- tapply(graella[, r], graella$tau, mean)
      mean.dist <- as.numeric(mean.dist)
      a <- lm(log(mean.dist)~log(unique(graella$tau)))
      exponent <- as.numeric(coef(a)[2])
      conf.int <- as.numeric(summary(a)$coefficients[4])
      tot.expon[r-1,1] <- noms[m]
      tot.expon[r-1,2] <- exponent
      tot.expon[r-1,3] <- r-2
      tot.expon[r-1,4] <- conf.int
    }
    llistaExponents <- rbind(llistaExponents, tot.expon)
  }
  
  llistaExponents <- llistaExponents[-1,] # We get rid of the first row, which is always NA.
  llistaExponents
}


loglogdata_logbin <- function(granMAT, nom){
  graella <- NULL
  MAT <- filter(granMAT, ID == nom)
  # We now create a power vector with base 2, to have equally spaced taus in log log plots.
  l <- length(4:(length(MAT$x)-1))
  power_tau_list <- 2^(1:10)
  power_tau_OK <- power_tau_list[which(power_tau_list<=l)] 
  # In this second for, we select a value for tau
  for(j in 1:length(power_tau_OK)){
    tau <- power_tau_OK[j]
    matriu <- matrix(nrow = 1, ncol = 1)
    matriu <- as.data.frame(matriu)
    names(matriu) <-  c("tau")
    #  And in this loop, we calculate the distance between increments (see Seuront and Stanley PNAS) 
    for(i in 1:(length(MAT$x)-j)){
      if(i+tau <= length(MAT$x)){
        d <- sqrt((MAT$x[i+tau]-MAT$x[i])^2+(MAT$y[i+tau]-MAT$y[i])^2)
        matriu[i, 1] <- tau
        # Finally, in this loop, we calculate the qth moment (q being between 0 and 8) for each d
        for(q in 0:8){
          matriu[i, q+2] <- d^(q)
          names(matriu)[q+2] <- paste("q", q, sep = "")
        }
      }
    }
    graella <- rbind(graella, matriu)
  }
  graella
}


qmom1_logbin <- function(granMAT, nom){
  llistaExponents <- matrix(nrow = 1, ncol = 4)
  llistaExponents <- as.data.frame(llistaExponents)
  names(llistaExponents) <-  c("ID", "exponents", "num", "conf.int")
  # This first loop selects a single sea urchin every time it loops.
  graella <- NULL
  MAT <- filter(granMAT, ID == nom)
  # We now create a power vector with base 2, to have equally spaced taus in log log plots.
  l <- length(4:(length(MAT$x)-1))
  power_tau_list <- 2^(1:10)
  power_tau_OK <- power_tau_list[which(power_tau_list<=l)] 
  # In this second for, we select a value for tau
  for(j in 1:length(power_tau_OK)){
    tau <- power_tau_OK[j]
    matriu <- matrix(nrow = 1, ncol = 1)
    matriu <- as.data.frame(matriu)
    names(matriu) <-  c("tau")
    #  And in this 3rd loop, we calculate the distance between increments (see Seuront and Stanley PNAS) 
    for(i in 1:(length(MAT$x)-j)){
      if(i+tau <= length(MAT$x)){
        d <- sqrt((MAT$x[i+tau]-MAT$x[i])^2+(MAT$y[i+tau]-MAT$y[i])^2)
        matriu[i, 1] <- tau
        # Finally, in this 4th loop, we calculate the qth moment (q being between 0 and 8) for each d
        for(q in 0:8){
          matriu[i, q+2] <- d^(q)
          names(matriu)[q+2] <- paste("q", q, sep = "")
        }
      }
    }
    graella <- rbind(graella, matriu)
  }
  
  # We now calculate the exponents of the regression between mean.dist and tau
  tot.expon <- matrix(nrow = 1, ncol = 4)
  tot.expon <- as.data.frame(tot.expon)
  names(tot.expon) <-  c("ID", "exponents", "num", "conf.int")
  for(r in 2:length(names(graella))){
    mean.dist <- tapply(graella[, r], graella$tau, mean)
    mean.dist <- as.numeric(mean.dist)
    a <- lm(log(mean.dist)~log(unique(graella$tau)))
    exponent <- as.numeric(coef(a)[2])
    conf.int <- as.numeric(summary(a)$coefficients[4])
    tot.expon[r-1,1] <- nom
    tot.expon[r-1,2] <- exponent
    tot.expon[r-1,3] <- r-2
    tot.expon[r-1,4] <- conf.int
  }
  tot.expon  
}

