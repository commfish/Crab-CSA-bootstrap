## AREA:Juneau
### SPECIES:Red crab
#### YEAR: 2015
#Date modified: 7-16-14 / 8-5-14 / 7-9-15


# CSA function here - boostrap function in crabboot file.
#functions are in "
getwd()
setwd("C:/Users/kjpalof/Documents/R/CSA/Crab-CSA-bootstrap/Juneau RKC")
#setwd("C:/Users/kjpalof/Documents/Red King_KJP/2014 RKC/Areas-individual 2014/Juneau/R CSA")
# working directory needs to be set to where the .csv input file is located

JNUred <- read.csv("Juneau 2015 RKC.csv")
#file name needs to be changed to reflect area and species

str(JNUred)

## For RED KING CRAB need to adjust function for weighting variable to 
##    be different in each year.
########STOP and make sure crabCSA function is loaded##########
## open "RKC_CSA_working crabCSA fnc_CI_parms_0.001.R" and load function at bottom

#### HERE:
#Name of file and variables will need to be changed, along with other input parms
# use input initial and M from Excel spreadsheet
#   if needed.
JNU_RKC_fit1 <- RcrabCSA1 (year = JNUred$Year, catch = JNUred$Catch..Number., 
                          preR = JNUred$PreR, 
                   recr = JNUred$Recruit, post = JNUred$PostR, csT= JNUred$Catch..Survey.Tau, 
                   sTs =JNUred$Survey.Tau, LegWt=JNUred$Legal.Weight, 
                   PreRWt=JNUred$Prerecruit.Weight, M = 0.32, 
                   w = JNUred$w, initial = c(1.47, 2.00, 0.90, 101.10, 79.02), 
                   uprn = 1000000, graph = TRUE)

JNU_RKC_fit1
JNU_RKC_fit1$estimates
write.csv(JNU_RKC_fit1$estimates, file = "JNU_RKC_fit1_estimates.csv")

write.csv(JNU_RKC_fit1$CI, file = "JNU_RKC_fit1_par&CI.csv")
write(JNU_RKC_fit1$SSQ, file = "JNU_SSQ.txt")
### save graphical output also 

#########STOP########
##open "bootstrap function crabboot.R /// OR go to below 
##      and follow instructions BEFORE
###    loading crabboot function
##############################

JNU_RKC_boot_fit <- crabbootJNU(dataset=JNUred, CSAoutput=JNU_RKC_fit1, B=1000)

JNU_RKC_boot_fit$quantCI
#  getting errors so intitial values aren't changing...how to fix this???


write.csv(JNU_RKC_boot_fit$est, file = "JNU_RKC_boot_estimates_v2.csv")
write.csv(JNU_RKC_boot_fit$quantCI, file = "JNU_RKC_boot_quantiles_v2.csv")

#load bootstrap results back in - when opening new session
boot.par <- read.csv("JNU_RKC_boot_estimates_v2.csv")
boot.par <- boot.par[ ,-1] # need to remove the first column which has the rep number in it

head(boot.par)

#SSQ quantiles
boot.par$SSQ
hist(boot.par$SSQ)
mean(boot.par$SSQ)
quantile(boot.par$SSQ, c(0.025, 0.975))

#write.csv(boot.yearQ, file = "JNU_RKC_boot_quantiles_trial1.csv")

#should end up with boot_fit file that has biomass (legal and mature), and
  #estimates of q, s, and SSQ for each bootstrap replication.  This file also
  #contains "quantCI" which are 2.5% and 97.5% confidence bounds for biomass (L and M)
  #in each year, assuming years are 1979-2014.

#################  GRAPHS #########################
# Attempts to graph bootstrap results
# Biomass estimates are in crabCSA output as $est
# Quantiles ci's are in bootstrap output$quantCI
# graph is now a function of the CSAoutput and the bootstrap output.
#  Also need to give this function a title - area and species, and a min y value

crabboot.graph(CSAout = JNU_RKC_fit1, bootout = JNU_RKC_boot_fit , 
               title = "JNU_RKC red crab", min= 0)
#  plots both legal and mature in 2 ways.
#crabboot.graph(CSAout = JNU_RKC_fit1, bootout = boot_fit , 
 #              title = "JNU_RKC red crab", min= 0)
crabboot.Legal.graph(CSAout = JNU_RKC_fit1, bootout = JNU_RKC_boot_fit , 
                     title = "JNU_RKC Legal crab", min= 0)
# plots just legal shaded
crabboot.LM.graph(CSAout = JNU_RKC_fit1, bootout = JNU_RKC_boot_fit , 
                     title = "JNU_RKC Legal & Mature crab", min= 0)
# plots both legal and mature - shaded

############LEGAL GRAPH#############
crabboot.Legal.graph <- function (CSAout=NULL, bootout=NULL, title="none", 
                                  min = 50000 ){
  par(mfrow = c(1, 1))
  plot(CSAout$est[,1], CSAout$est[,11], xlab = "Year", 
       ylab = "Legal Biomass", main = title, 
       type = "l", pch =20,
       ylim = c(min, max(bootout$quantCI[,2])) )
  polygon(c(CSAout$est[,1], rev(CSAout$est[,1])), 
          c(bootout$quantCI[,1],rev(bootout$quantCI[,2])), 
          col="red", border = "red", lty= "dashed", density=25)
  lines(CSAout$est[,1], CSAout$est[,11], lwd=2)
}

############## Legal and mature - larger #########################
crabboot.LM.graph <- function (CSAout=NULL, bootout=NULL, title="none", 
                            min = 50000 ){
  ### set up output to file
  #pdf("C:/Users/kjpalof/Documents/R/tanner crab/SP_TCS_CIplot.pdf")
  par(mfrow = c(1, 1))
  
### attempt at shading Both legal and mature biomass
  
  ##### legal biomass first
  plot(CSAout$est[,1], CSAout$est[,11], xlab = "Year", 
       ylab = "Biomass", main = title, 
       type = "l", pch =20,
       ylim = c(min, max(bootout$quantCI[,4])) )
  polygon(c(CSAout$est[,1], rev(CSAout$est[,1])), 
          c(bootout$quantCI[,1],rev(bootout$quantCI[,2])), 
          col="red", border = "red", lty= "dashed", density=25)
  lines(CSAout$est[,1], CSAout$est[,11], lwd=2)
  #Mature biomass
  polygon(c(CSAout$est[,1], rev(CSAout$est[,1])), 
          c(bootout$quantCI[,3],rev(bootout$quantCI[,4])), 
          col="blue", border = "blue", lty= "dashed", density=25,
          angle=110)
  lines(CSAout$est[,1], CSAout$est[,12], lwd=2)
  #dev.off()
}

############Graph with BOTH LEGAL AND MATURE###################
crabboot.graph <- function (CSAout=NULL, bootout=NULL, title="none", 
                            min = 50000 ){
  ### set up output to file
  #pdf("C:/Users/kjpalof/Documents/R/tanner crab/SP_TCS_CIplot.pdf")
  par(mfrow = c(2, 1))
  
  plot(CSAout$est[,1], CSAout$est[,11], xlab = "Year", 
       ylab = "Biomass", main = title, 
       type = "b", pch =20,
       ylim = c(min, max(bootout$quantCI[,4])))
  #lines for legal biomass
  lines(x=CSAout$est[,1], y =bootout$quantCI[,1], 
        col = "red", type = "l", lty="dashed")
  lines(x=CSAout$est[,1], y =bootout$quantCI[,2], 
        col = "red", type = "l", lty="dashed")
  
  lines(x=CSAout$est[,1], y =CSAout$est[,12], 
        col = "black", type = "b", pch= 1)
  lines(x=CSAout$est[,1], y =bootout$quantCI[,3], 
        col = "blue", type = "l", lty="dashed")
  lines(x=CSAout$est[,1], y =bootout$quantCI[,4], 
        col = "blue", type = "l", lty="dashed")
  
  ### attempt at shading Both legal and mature biomass
  
  ##### legal biomass first
  plot(CSAout$est[,1], CSAout$est[,11], xlab = "Year", 
       ylab = "Biomass", main = title, 
       type = "l", pch =20,
       ylim = c(min, max(bootout$quantCI[,4])) )
  polygon(c(CSAout$est[,1], rev(CSAout$est[,1])), 
          c(bootout$quantCI[,1],rev(bootout$quantCI[,2])), 
          col="red", border = "red", lty= "dashed", density=25)
  lines(CSAout$est[,1], CSAout$est[,11], lwd=2)
  #Mature biomass
  polygon(c(CSAout$est[,1], rev(CSAout$est[,1])), 
          c(bootout$quantCI[,3],rev(bootout$quantCI[,4])), 
          col="blue", border = "blue", lty= "dashed", density=25,
          angle=110)
  lines(CSAout$est[,1], CSAout$est[,12], lwd=2)
  #dev.off()
}



################BOOTSTRAP FUNTION ##########################
# meant for use after the crabCSA function because it uses inputs from this previous function.
#bootstrap function
# meant for use after the crabCSA function because it uses inputs from this previous function.

#confirm data is loaded
head(JNUred) #or whatever the name of the data set is

#add estimates of index of preR, R, and post to original data
#set up to run years 1997-2013, need to be automated or edited for other years.
# for a new AREA/SPECIES combo copy crabCSA command into this function.##########
####
#########
#steps:
#1. Years should be read automatically from input but confirm this below
#2. make sure that the original data file and the RcrabCSA output exist, these are input into 
##this function.
#3. copy "RcrabCSA1" code into this function (not the function just the call to the function from original run)
#4. make sure that in this function the PreR, R, and Post input refer to the bootstrap NOT the original data
#5. Make sure that graph= FALSE in crabCSA function

JNU_RKC_boot_fit <- crabbootJNU(dataset=JNUred, CSAoutput=JNU_RKC_fit1, B=2)

crabbootJNU <- function (dataset=NULL, CSAoutput=NULL, B=NULL){
  est<- CSAoutput$estimates
  dat1 <- cbind(dataset, est[,2:4])
  #calculate log residuals
  dat1$LresidPR <- log(dat1$PreR) - log(dat1$prest)
  dat1$LresidR <- log(dat1$Recruit) - log(dat1$rest)
  dat1$LresidN <- log(dat1$PostR) - log(dat1$nest)
  #calculate SD of residuals for each data input  
  sd_LresidPR <- sd(dat1$LresidPR, na.rm=TRUE)
  sd_LresidR <- sd(dat1$LresidR, na.rm = TRUE)
  sd_LresidN <- sd(dat1$LresidN, na.rm = TRUE)
  
  yrs <- length(dat1$Year)
  boot.par <- matrix(nrow=B, ncol= (2*yrs)+3) # could also be # yrs*2+3
  # want to keep legal and mature biomass for each year (17) for each run, 
  #######   and keep estimates of q, s, and SSQ just for the hell of it
  #dimnames(boot.par) <-list(NULL, c(1979:2014, 1979:2014,"q", "s", "SSQ"))
  dimnames(boot.par) <- list(NULL, c(min(dat1$Year):max(dat1$Year), 
                                     min(dat1$Year):max(dat1$Year),"q", "s", "SSQ"))
  
  for (i in 1:B){
    # resample and get new PR, R, and N index input
    PR.boot <- exp(log(dat1$prest) + rnorm(yrs, sd=sd_LresidPR)+
                     ((sd_LresidPR^2)/2))
    R.boot <- exp(log(dat1$rest) + rnorm(yrs, sd=sd_LresidR)+
                    ((sd_LresidR^2)/2))
    N.boot <- exp(log(dat1$nest) + rnorm(yrs, sd=sd_LresidN)+
                    ((sd_LresidN^2)/2))
    #starting values and input same as in original function.
    
    fit.boot <- RcrabCSA1B (year = JNUred$Year, catch = JNUred$Catch..Number., 
                          preR = PR.boot, 
                          recr = R.boot, post = N.boot, csT= JNUred$Catch..Survey.Tau, 
                          sTs =JNUred$Survey.Tau, LegWt=JNUred$Legal.Weight, 
                          PreRWt=JNUred$Prerecruit.Weight, M = 0.32, 
                          w = JNUred$w, initial = c(1.47, 2.00, 0.90, 101.10, 79.02), 
                          uprn = 1000000, graph = FALSE)
     
    #need to figure out how to get output from function into boot.par
    boot.par[i, ] <- c(fit.boot$estimates[,11],fit.boot$estimates[,12], 
                       fit.boot$parm, fit.boot$SSQ)
    write.csv(boot.par, file = "boot.par.csv")
    
  }
  ########## Quantile, percentile confidence intervals
  ## need a quantile for each year biomass 
  boot.yearQ <- matrix(nrow=yrs, ncol= 4)
  #boot.yearQ
  #dimnames(boot.yearQ) <-list((1979:2014), c("L_0.025", "L_.975",
  #                                                       "M_0.025", "M_.975"))
  dimnames(boot.yearQ) <-list(min(dat1$Year):max(dat1$Year), c("L_0.025", "L_.975",
                                                               "M_0.025", "M_.975"))
  
  for (i in 1:yrs) {
    boot.yearQ[i,] <- c((quantile(boot.par[,i],c(0.025,0.975))),
                        (quantile(boot.par[,i+yrs],c(0.025,0.975))))
  }
  #name.boot.yearQ <- boot.yearQ # gives the quantiles for each year for both legal and mature biomass.
  
  
  
  output <- list(est = boot.par, quantCI = boot.yearQ)
  return(output)
}

#####################
#################################
#############################################
##################################
###################
# graph for mature and legal

plot(JNU_RKC_fit1$est[,1], JNU_RKC_fit1$est[,11], xlab = "Year", 
     ylab = "Biomass", main = "JNU red crab mature and legal biomass", 
     type = "l", pch =20,
     ylim = c(0, max(JNU_RKC_boot_fit$quantCI[,4])) )
polygon(c(JNU_RKC_fit1$est[,1], rev(JNU_RKC_fit1$est[,1])), 
        c(JNU_RKC_boot_fit$quantCI[,1],rev(JNU_RKC_boot_fit$quantCI[,2])), 
        col="red", border = "red", lty= "dashed", density=25)
lines(JNU_RKC_fit1$est[,1], JNU_RKC_fit1$est[,11], lwd=2)
#Mature biomass
polygon(c(JNU_RKC_fit1$est[,1], rev(JNU_RKC_fit1$est[,1])), 
        c(JNU_RKC_boot_fit$quantCI[,3],rev(JNU_RKC_boot_fit$quantCI[,4])), 
        col="blue", border = "blue", lty= "dashed", density=25,
        angle=110)
lines(JNU_RKC_fit1$est[,1], JNU_RKC_fit1$est[,12], lwd=2)


#yrs

#for (i in 1:yrs) {
#  boot.yearQ[i,] <- c((quantile(JNU_RKC_boot_fit$est[,i],c(0.025,0.975))),
#                      (quantile(JNU_RKC_boot_fit$est[,i+yrs],c(0.025,0.975))))
#}


# function is the same for all.  Set up to use input data.  
# the only thing that might need to be changed are the lower limits, but they 
#   should be ok.


### to get SSQ and CI's for SSQ in boot.
