# # # # # # # # # # # # # # # # # # 
#     Analysing the circular      #
#  statistics for each treatment  #
#                                 #
#        Jordi F. Pagès           #
#        September 2020           #
# # # # # # # # # # # # # # # # # # 

library(adehabitatLT)
library(plotrix)
library(circular)
library(CircStats)
library(shape)


# # # 
# Circular stats for control sea urchins ----
# # # 
 
load("RData/urch.null.RData")

urch.angles <- matrix(ncol = 3, nrow = 1)
urch.angles <- as.data.frame(urch.angles)
names(urch.angles) <- c("ID", "alfa_init", "alfa_final")
for(i in 1:length(urch.null)){
  if(id(urch.null)[i] != "20120528_4" & id(urch.null)[i] != "20120528_5" & id(urch.null)[i] != "20120528_6" & id(urch.null)[i] != "20120607_5"){
    # we need the function atan2()
    # The arc-tangent of two arguments atan2(y, x) returns the angle between the x-axis and the vector from the origin to (x, y)
    # For the initial angle, we calculate the bearing between the first and 5th position
    deltax_init <- urch.null[[i]]$x[5] - urch.null[[i]]$x[1]
    deltay_init <- urch.null[[i]]$y[5] - urch.null[[i]]$y[1]
    alfa_init <- atan2(deltay_init, deltax_init)
    plot(urch.null[i], xlim = c(0, 1936), ylim = c(-200, 1500))
    draw.circle(x = 968, y= 648 ,radius = 968)
    Arrowhead(x0=968, y0=648, angle = deg(alfa_init))
    urch.angles[i,1] <- id(urch.null)[i]
    urch.angles[i,2] <- alfa_init
    
    # For the final angle, we calculate the initial and final x and ys
    # We then calculate the increment between xfinal-xinitial (and the same for ys) and after that we calculate the atan2(y, x)
    deltax_final <- urch.null[[i]]$x[length(urch.null[[i]]$x)] - urch.null[[i]]$x[1]
    deltay_final <- urch.null[[i]]$y[length(urch.null[[i]]$y)] - urch.null[[i]]$y[1]
    alfa_final <- atan2(deltay_final, deltax_final)
    plot(urch.null[i], xlim = c(0, 1936), ylim = c(-200, 1500))
    draw.circle(x = 968, y= 648 ,radius = 968)
    Arrowhead(x0=968, y0=648, angle = deg(alfa_final))
    urch.angles[i,3] <- alfa_final
  }
}

# We get rid of NA's
urch.angles <- urch.angles[-which(is.na(urch.angles$ID) == T),]

# We now have the initial and final angles. Now, we want to check if they distribute uniformly along the circumference, or not.
# That's why we'll now do a Rayleigh test, but we must first convert the data into class circular.
# Rayleigh test
#   H0: the population is uniformly distributed around the circle
#   HA: the population is not distributed uniformly around the circle
#   Assumption: the distribution has maximally one mode and the data is sampled from a von Mises distribution!

# With initial angles
angles.init.circular <- circular(urch.angles$alfa_init, type = "angles", units = "radians")
rayleigh.test(angles.init.circular)
# Rayleigh Test of Uniformity 
# General Unimodal Alternative 
# Test Statistic:  0.1642 
# P-value:  0.4612 
# Thus, for control urchins, the initial distribution of the angles can be considered uniform.
plot(angles.init.circular, pch = 1, main = "Urch.null - Initial angles")
points(mean.circular(angles.init.circular), col = "red")

# With the final angles
angles.final.circular <- circular(urch.angles$alfa_final, type = "angles", units = "radians")
rayleigh.test(angles.final.circular)
# Rayleigh Test of Uniformity 
# General Unimodal Alternative 
# Test Statistic:  0.207 
# P-value:  0.2909 
# Thus, for control sea urchins, the final angle distribution can also be considered uniform.
plot(angles.final.circular, pch = 1, main = "Urch.null - Final angles")
points(mean.circular(angles.final.circular), col = "red")

# Checking assumptions of the Rayleigh test: angles must follow a Von Mises distribution.
# For initial angles
watson.test(angles.init.circular, alpha = 0.05, dist="vonmises")
# Watson's Test for the von Mises Distribution 
# Test Statistic: 0.0137 
# Level 0.05 Critical Value: 0.066 
# Do Not Reject Null Hypothesis >>> OKK!!

# For final angles
watson.test(angles.final.circular, alpha = 0.05, dist="vonmises")
# Watson's Test for the von Mises Distribution 
# Test Statistic: 0.0325 
# Level 0.05 Critical Value: 0.066 
# Do Not Reject Null Hypothesis 
# Assumptions satisfied.


# # # 
# Circular stats for sea urchins with predator cues ----
# # # 

load("RData/urch.pred.RData")

urch.pred.angles <- matrix(ncol = 3, nrow = 1)
urch.pred.angles <- as.data.frame(urch.pred.angles)
names(urch.pred.angles) <- c("ID", "alfa_init", "alfa_final")
for(i in 1:length(urch.pred)){
  if(id(urch.pred)[i] != "20120613_12" & id(urch.pred)[i] != "20120614_8"){
    # we need the function atan2()
    # The arc-tangent of two arguments atan2(y, x) returns the angle between the x-axis and the vector from the origin to (x, y)
    # For the initial angle, we calculate the bearing between the first and 5th position
    deltax_init <- urch.pred[[i]]$x[5] - urch.pred[[i]]$x[1]
    deltay_init <- urch.pred[[i]]$y[5] - urch.pred[[i]]$y[1]
    alfa_init <- atan2(deltay_init, deltax_init)
    plot(urch.pred[i], xlim = c(0, 1936), ylim = c(-200, 1500))
    draw.circle(x = 968, y= 648 ,radius = 968)
    Arrowhead(x0=968, y0=648, angle = deg(alfa_init))
    urch.pred.angles[i,1] <- id(urch.pred)[i]
    urch.pred.angles[i,2] <- alfa_init
    
    # For the final angle, we calculate the initial and final x and ys
    # We then calculate the increment between xfinal-xinitial (and the same for ys) and after that we calculate the atan2(y, x)
    deltax_final <- urch.pred[[i]]$x[length(urch.pred[[i]]$x)] - urch.pred[[i]]$x[1]
    deltay_final <- urch.pred[[i]]$y[length(urch.pred[[i]]$y)] - urch.pred[[i]]$y[1]
    alfa_final <- atan2(deltay_final, deltax_final)
    plot(urch.pred[i], xlim = c(0, 1936), ylim = c(-200, 1500))
    draw.circle(x = 968, y= 648 ,radius = 968)
    Arrowhead(x0=968, y0=648, angle = deg(alfa_final))
    urch.pred.angles[i,3] <- alfa_final
  }
}

# We get rid of NA's
urch.pred.angles <- urch.pred.angles[-which(is.na(urch.pred.angles$ID) == T),]

# We now have the initial and final angles. Now, we want to check if they distribute uniformly along the circumference, or not.
# That's why we'll now do a Rayleigh test, but we must first convert the data into class circular.
# Rayleigh test
#   H0: the population is uniformly distributed around the circle
#   HA: the population is not distributed uniformly around the circle
#   Assumption: the distribution has maximally one mode and the data is sampled from a von Mises distribution!

# With initial angles.
angles.init.circular <- circular(urch.pred.angles$alfa_init, type = "angles", units = "radians")
rayleigh.test(angles.init.circular)
# Rayleigh Test of Uniformity 
# General Unimodal Alternative 
# Test Statistic:  0.2331 
# P-value:  0.3232 
# Thus, for sea urchins with predator cues, the initial angle distribution can also be considered uniform.
plot(angles.init.circular, pch = 1, main = "urch.pred - Initial angles")
points(mean.circular(angles.init.circular), col = "red")

# With final angles
angles.final.circular <- circular(urch.pred.angles$alfa_final, type = "angles", units = "radians")
rayleigh.test(angles.final.circular)
# Rayleigh Test of Uniformity 
# General Unimodal Alternative 
# Test Statistic:  0.1889 
# P-value:  0.4781 
# Thus, for sea urchins predator cues, the final angle distribution can also be considered uniform.
plot(angles.final.circular, pch = 1, main = "urch.pred - Final angles")
points(mean.circular(angles.final.circular), col = "red")

# Checking assumptions of the Rayleigh test: angles must follow a Von Mises distribution.
# For initial angles
watson.test(angles.init.circular, alpha = 0.05, dist="vonmises")
# Watson's Test for the von Mises Distribution 
# Test Statistic: 0.0172 
# Level 0.05 Critical Value: 0.066 
# Do Not Reject Null Hypothesis >>> OKK!!
# For final angles
watson.test(angles.final.circular, alpha = 0.05, dist="vonmises")
# Watson's Test for the von Mises Distribution 
# Test Statistic: 0.0474 
# Level 0.05 Critical Value: 0.066 
# Do Not Reject Null Hypothesis 
# Assumptions satisfied.



# # # #
# END #
# # # # 