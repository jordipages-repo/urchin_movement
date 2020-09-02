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

# Function to calculate the slope of the q moments line for each individual urchin, for the shadows experiment ----
# (given there were 3 trials for each urchin, we calculate the mean slope for these 3 trials, 
# to calculate an 'individual level' slope) 
indiv_exp_shadows <- function(listExp){
  listExp <- listExp.urch.shadows
  noms <-unique(listExp$indiv)
  tot <- matrix(ncol = 2, nrow = 0)
  tot <- as.data.frame(tot)
  names(tot) <- c("ID", "coef")
  for(i in 1:length(noms)){
    indiv <- subset(listExp, indiv == noms[i])
    exponents <- as.numeric(tapply(indiv$exponents, indiv$num, mean))
    regre <- lm(exponents~unique(indiv$num))
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
