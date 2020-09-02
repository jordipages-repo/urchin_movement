# # # # # # # # # # # # # # # #
# Initial centroid processing # 
#    to obtain trajectories   #
#                             #
#        September 2020       #
#         Jordi F Pagès       #
# # # # # # # # # # # # # # # # 

# This script is only needed once to produce 3 .RData files, that will then be analysed (long correlation analysis, q moments)
# using the script urchins_q_moments.R

# # # 
# Libraries ----
# # # 

library(adehabitatHR)
library(adehabitatLT)
library(tripack)


# # # 
# Grouping all trajectories for control urchins ----
# # # 

# We search each urchin by date (first for), and replicate (for nested in the previous loop), from the control urchins folder
# Once a correct folder is found, the file is read, and then it's bound to the previous trajectory by rows (rbind)
date <- 20120524
urchins <- NULL
for(i in 1:100){
  nom <- as.character(date+i)
  filepath <- paste("Data_urchin_centroids/controls/", nom, sep ="")
  try(for(j in 1:50){
    filepath.j <- paste(filepath, "_", j, ".txt", sep = "")
    matriu <- as.data.frame(read.table(file = filepath.j))
    matriu$ID <- rep(paste(nom, "_", j, sep = ""), length(matriu[,1]))
    matriu$Time <- paste(rep(nom, length(matriu[,1])), rep("10:00", length(matriu[,1])))
    matriu$Time <- as.POSIXct(matriu$Time, "%Y%m%d %H:%M", tz = "UTC")
    segons <- seq(from = 0, to = (length(matriu[,1])-1)*30, by = 30)
    matriu$Time <- matriu$Time + segons
    urchins <- rbind(urchins, matriu)
  })                       
}

names(urchins) <- c("x", "y", "ID", "Time")

# We now have the trajectories with POSIXct date, therefore we can compute 
# trajectories using typeII = TRUE in adehabitatLT as.ltraj() function
urch.null <- as.ltraj(xy = urchins[,1:2], date = urchins$Time, id = urchins$ID)
plot(urch.null, xlim = c(0,1936), ylim = c(0, 1296))

# We now save this data as .RData
# save(urch.null, file = "RData/urch.null.RData")


#####################################

# # # 
# Grouping all trajectories for urchins of the predator cue experiment ----
# # # 

# We search each urchin by date (first for), and replicate (for nested in the previous loop), from the control urchins folder
# Once a correct folder is found, the file is read, and then it's bound to the previous trajectory by rows (rbind)
date <- 20120610
urchins <- NULL
for(i in 1:100){
  nom <- as.character(date+i)
  filepath <- paste("Data_urchin_centroids/predators/", nom, sep ="")
  try(for(j in 1:50){
    filepath.j <- paste(filepath, "_", j, ".txt", sep = "")
    matriu <- as.data.frame(read.table(file = filepath.j))
    matriu$ID <- rep(paste(nom, "_", j, sep = ""), length(matriu[,1]))
    matriu$Time <- paste(rep(nom, length(matriu[,1])), rep("10:00", length(matriu[,1])))
    matriu$Time <- as.POSIXct(matriu$Time, "%Y%m%d %H:%M", tz = "UTC")
    segons <- seq(from = 0, to = (length(matriu[,1])-1)*30, by = 30)
    matriu$Time <- matriu$Time + segons
    urchins <- rbind(urchins, matriu)
  })                       
}

names(urchins) <- c("x", "y", "ID", "Time")

# We now have the trajectories with POSIXct date, therefore we can compute 
# trajectories using typeII = TRUE in adehabitatLT as.ltraj() function
urch.pred <- as.ltraj(xy = urchins[,1:2], date = urchins$Time, id = urchins$ID)
plot(urch.pred, xlim = c(0,1936), ylim = c(0, 1296)) # Els plota tots de cop a un sol full.

# We now save this data as .RData
save(urch.pred, file = "RData/urch.pred.RData")


#####################################

# # # 
# Grouping all trajectories for urchins of the shadows experiment ----
# # # 

# We search each urchin by date (first for), and replicate (for nested in the previous loop), from the control urchins folder
# Once a correct folder is found, the file is read, and then it's bound to the previous trajectory by rows (rbind)
date <- 20120620
urchins <- NULL
for(i in 1:100){
  nom <- as.character(date+i)
  filepath <- paste("Data_urchin_centroids/shadows/", nom, sep ="")
  try(for(j in 1:50){
    filepath.j <- paste(filepath, "_", j, ".txt", sep = "")
    matriu <- as.data.frame(read.table(file = filepath.j))
    matriu$ID <- rep(paste(nom, "_", j, sep = ""), length(matriu[,1]))
    matriu$Time <- paste(rep(nom, length(matriu[,1])), rep("10:00", length(matriu[,1])))
    matriu$Time <- as.POSIXct(matriu$Time, "%Y%m%d %H:%M", tz = "UTC")
    segons <- seq(from = 0, to = (length(matriu[,1])-1)*30, by = 30)
    matriu$Time <- matriu$Time + segons
    urchins <- rbind(urchins, matriu)
  })                       
}

names(urchins) <- c("x", "y", "ID", "Time")

# We now have the trajectories with POSIXct date, therefore we can compute 
# trajectories using typeII = TRUE in adehabitatLT as.ltraj() function
urch.shadows <- as.ltraj(xy = urchins[,1:2], date = urchins$Time, id = urchins$ID)
plot(urch.shadows, xlim = c(0,1936), ylim = c(0, 1296)) # Els plota tots de cop a un sol full.

# We now save this data as .RData
# save(urch.shadows, file = "RData/urch.shadows.RData")
