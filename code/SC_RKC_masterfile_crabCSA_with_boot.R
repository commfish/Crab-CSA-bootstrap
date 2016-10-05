## AREA: Seymour Canal
### SPECIES: Red crab
#### YEAR: 2016 
#Date modified: 7-16-14/ 8-1-14 / 8-21-14(boot results -graph or redo)
## 8-12-15 / 10-3-16

# # CSA is in 'RKC_RcrabCSA_fnc.R' and 
# bootstrap is here below 
###all data is stored in the data folder.  Functions are sourced out of the functions folder
##### workspace ------------------------
getwd()

##### Load data -------------------------------
SCred <- read.csv('./data/Seymour_2016_Rinput.csv')
#file name needs to be changed to reflect area and species

str(SCred)

##### Load functions --------------------
source("./functions/RKC_RcrabCSA_fnc.R")# sources the file with the model code
source("./functions/graph_fnc_CSA.R")# sources the file with the graphing function for the bootstrap
#   OR 
########STOP and make sure crabCSA function is loaded##########
## open "RKC_Rcrab CSA fnc.R" and load function 

##### Run model -----------------------------------
#Name of file and variables will need to be changed, along with other input parms if needed.
#       initial(PreR initial each year, R in the 1st year, Post in the first year, q scaled by 100, 
#          s scaled by 1,000,000)
# M = 0.30 for Tanner crab AND M = 0.32 for Red Crab
# w is a vector of weigthings found in the data file. 

SC_RKC_fit1 <- RcrabCSA1 (year = SCred$Year, catch = SCred$Catch..Number., preR = SCred$PreR, 
                         recr = SCred$Recruit, post = SCred$PostR, csT= SCred$Catch..Survey.Tau, 
                         sTs =SCred$Survey.Tau, LegWt=SCred$Legal.Weight, 
                         PreRWt=SCred$Prerecruit.Weight, M = 0.32, 
                         w = SCred$w, initial = c(0.75, 2.41, 1.51, 135.6966, 114.6239), 
                         uprn = 100000, graph = TRUE)
#5.00, 7.80, 3.60, 63.73, 85.02
#c(0.65, 2.29, 1.46, 133.02, 101.0)
# worked using these input c(0.65, 2.29, 1.46, 122.0019, 93.15), but larger SSQ, try other inputs
# uprn = 1000000
## !!!  This area is very sensitive to the input values for q and s.
SC_RKC_fit1
SC_RKC_fit1$estimates
SC_RKC_fit1$parms
SC_RKC_fit1$SSQ
# save model output
write.csv(SC_RKC_fit1$estimates, './output/SC_RKC_fit1_estimates.csv')
write.csv(SC_RKC_fit1$CI, './output/SC_RKC_fit1_par&CI.csv')
write(SC_RKC_fit1$SSQ, file = './output/SC_SSQ.txt')
### save graphical output also - DO THIS manually, I have NOT automated this step.

#########STOP  --------------------------
##### BOOTSTRAP ---------------------------------
##     go to below ### BOOTSTRAP 
##      and follow instructions BEFORE
###    loading crabboot function - LOAD the specific function in this file.
##############################
##### Run bootstrap -------------------------
source("./functions/RKC_RcrabCSA_fnc_for_boot.R")

# WARNING ! Do NOT run 1,000 reps now.  This will take a LONG time.  B=1000 is preferred but 
#             here B is smaller to test the boostrap function.
SC_RKC_boot_fit <- crabbootSC(dataset=SCred, CSAoutput=SC_RKC_fit1, B=2)

#### bootstrap save ----------------------------------
write.csv(SC_RKC_boot_fit$est, file = "SC_RKC_boot_estimates_b=1000.csv")
write.csv(SC_RKC_boot_fit$quantCI, file = "SC_RKC_boot_quantiles_B=1000.csv")



#################  GRAPHS #########################
# Sourced above OR from the 'graph_fnc_CSA.R' file   
#  Attempts to graph bootstrap results
# Biomass estimates are in crabCSA output as $est
# Quantiles ci's are in bootstrap output$quantCI
# graph is now a function of the CSAoutput and the bootstrap output.
#  Also need to give this function a title - area and species, and a min y value


## !!!! add code to save graphs here.
crabboot.graph(CSAout = SC_RKC_fit1, bootout = SC_RKC_boot_fit , 
               title = "SC_RKC red crab", min= 0)
crabboot.Legal.graph(CSAout = SC_RKC_fit1, bootout = SC_RKC_boot_fit , 
                     title = "SC_RKC Legal crab", min= 0)
crabboot.Mature.graph(CSAout = SC_RKC_fit1, bootout = SC_RKC_boot_fit , 
                     title = "SC_RKC Legal crab", min= 0)
crabboot.LM.graph(CSAout = SC_RKC_fit1, bootout = SC_RKC_boot_fit , 
                  title = "SC_RKC Legal and mature crab", min= 0)

################BOOTSTRAP FUNTION ----------------------------------------------
# meant for use after the crabCSA function because it uses inputs from this previous function.

#confirm data is loaded
#head(SCred) #or whatever the name of the data set is

#add estimates of index of preR, R, and post to original data
#set up to run years 1997-2013, need to be automated or edited for other years.
# for a new AREA/SPECIES combo copy crabCSA command into this function.
####
#########
#steps:
#1. Years should be read automatically from input but confirm this below
#2. make sure that the original data file and the RcrabCSA output exist, these are input into 
##   this function.
#3. copy call to "RcrabCSA1" code into this function (from above)
#(not the function just the call to the function from original run), make sure
# the call here is to "RcrabCSA1B" - slightly different for bootstrap.
#4. make sure that in this function the PreR, R, and Post input refer to the bootstrap NOT the original data
#5. Make sure that graph= FALSE in crabCSA function

### test here after loading.

#boot_fit <- crabboot(dataset=SCred, CSAoutput=SC_RKC_fit1, B=1)

##### BOOT LOAD THIS----------------------------
source("./functions/RKC_RcrabCSA_fnc_for_boot.R")
crabbootSC <- function (dataset=NULL, CSAoutput=NULL, B=NULL){
  est<- CSAoutput$estimates
  dat1 <- cbind(dataset, est[,2:4])
  #calculate log residuals
  dat1$LresidPR <- log(dat1$PreR) - log(dat1$prest)
  dat1$LresidR <- log(dat1$Recruit) - log(dat1$rest)
  dat1$LresidN <- log(dat1$PostR) - log(dat1$nest)
  #calculate SD of residuals for each data input	
  sd_LresidPR <- sd(dat1$LresidPR, na.rm = TRUE)
  sd_LresidR <- sd(dat1$LresidR, na.rm = TRUE)
  sd_LresidN <- sd(dat1$LresidN, na.rm = TRUE)
  
  yrs <- length(dat1$Year)
  boot.par <- matrix(nrow=B, ncol= (2*yrs)+3) # could also be # yrs*2+3
  # want to keep legal and mature biomass for each year (17) for each run, 
  #######   and keep estimates of q, s, and SSQ just for the hell of it
  
  #dimnames(boot.par) <- list(NULL, c(1997:2013, 1997:2013,"q", "s", "SSQ"))
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
    
    fit.boot <- RcrabCSA1B (year = SCred$Year, catch = SCred$Catch..Number., 
                         preR = PR.boot, recr = R.boot, 
                         post = N.boot, csT= SCred$Catch..Survey.Tau, 
                         sTs =SCred$Survey.Tau, LegWt=SCred$Legal.Weight, 
                         PreRWt=SCred$Prerecruit.Weight, M = 0.32, 
                         w = SCred$w, initial = c(0.75, 2.41, 1.51, 135.6966, 114.6239), 
                         uprn = 1000000, graph = FALSE)
    # old starting values 
    #need to figure out how to get output from function into boot.par
    boot.par[i, ] <- c(fit.boot$estimates[,11],fit.boot$estimates[,12], 
                       fit.boot$parm, fit.boot$SSQ)
    write.csv(boot.par, file = "boot.par.csv")
    
  }
  ########## Quantile, percentile confidence intervals
  ## need a quantile for each year biomass 
  boot.yearQ <- matrix(nrow=yrs, ncol= 4)
  #boot.yearQ
  #dimnames(boot.yearQ) <-list((1997:2013), c("L_0.025", "L_.975",
  #                                           "M_0.025", "M_.975"))
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

############### END/ STOP ---------------------------------------------------------------------------------------

######################################################
##########################################################
#######################################
##############################
### combined all 1,000 reps into one file and upload
##### loading back in boot.par file ---------------------------
boot.par <- read.csv("boot.par_1000.csv")
boot.par <- boot.par[ ,-1] # need to remove the first column which has the rep number in it

head(boot.par)
yrs <- length(SCred$Year)
#need to summarize into quantiles for graphs.
boot.yearQ <- matrix(nrow=yrs, ncol= 4)
#boot.yearQ
#dimnames(boot.yearQ) <-list((1997:2013), c("L_0.025", "L_.975",
#                                                       "M_0.025", "M_.975"))
dimnames(boot.yearQ) <-list(min(SCred$Year):max(SCred$Year), c("L_0.025", "L_.975",
                                                               "M_0.025", "M_.975"))
for (i in 1:yrs) {
  boot.yearQ[i,] <- c((quantile(boot.par[,i],c(0.025,0.975))),
                      (quantile(boot.par[,i+yrs],c(0.025,0.975))))
}

boot.yearQ

#SSQ quantiles
boot.par$SSQ
hist(boot.par$SSQ)
mean(boot.par$SSQ)
quantile(boot.par$SSQ, c(0.025, 0.975))
#should end up with boot_fit file that has biomass (legal and mature), and
#estimates of q, s, and SSQ for each bootstrap replication.  This file also
#contains "quantCI" which are 2.5% and 97.5% confidence bounds for biomass (L and M)
#in each year, assuming years are 1997-2016.

### graphs using new combined file and boot.yearQ

par(mfrow = c(1, 1))
plot(SC_RKC_fit1$est[,1], SC_RKC_fit1$est[,11], xlab = "Year", 
     ylab = "Legal Biomass", main = "SC Legal Biomass ", 
     type = "l", pch =20,
     ylim = c(0, max(boot.yearQ[,2])) )
polygon(c(SC_RKC_fit1$est[,1], rev(SC_RKC_fit1$est[,1])), 
        c(boot.yearQ[,1],rev(boot.yearQ[,2])), 
        col="red", border = "red", lty= "dashed", density=25)
lines(SC_RKC_fit1$est[,1], SC_RKC_fit1$est[,11], lwd=2)

### legal and mature 
plot(SC_RKC_fit1$est[,1], SC_RKC_fit1$est[,11], xlab = "Year", 
     ylab = "Biomass", main = "SC mature and legal biomass", 
     type = "l", pch =20,
     ylim = c(0, max(boot.yearQ[,4])) )
polygon(c(SC_RKC_fit1$est[,1], rev(SC_RKC_fit1$est[,1])), 
        c(boot.yearQ[,1],rev(boot.yearQ[,2])), 
        col="red", border = "red", lty= "dashed", density=25)
lines(SC_RKC_fit1$est[,1], SC_RKC_fit1$est[,11], lwd=2)
#Mature biomass
polygon(c(SC_RKC_fit1$est[,1], rev(SC_RKC_fit1$est[,1])), 
        c(boot.yearQ[,3],rev(boot.yearQ[,4])), 
        col="blue", border = "blue", lty= "dashed", density=25,
        angle=110)
lines(SC_RKC_fit1$est[,1], SC_RKC_fit1$est[,12], lwd=2)

### Just mature 
par(mfrow = c(1, 1))
plot(SC_RKC_fit1$est[,1], SC_RKC_fit1$est[,12], xlab = "Year", 
     ylab = "Mature Biomass", main = "SC Mature Biomass ", 
     type = "l", pch =20,
     ylim = c(0, max(boot.yearQ[,4])) )
polygon(c(SC_RKC_fit1$est[,1], rev(SC_RKC_fit1$est[,1])), 
        c(boot.yearQ[,3],rev(boot.yearQ[,4])), 
        col="blue", border = "blue", lty= "dashed", density=25)
lines(SC_RKC_fit1$est[,1], SC_RKC_fit1$est[,12], lwd=2)


## save the quantile file for the future
write.csv(boot.yearQ, file = "SC_RKC_boot_quantilesB=1000.csv")

write(SC_RKC_fit1$SSQ, file = "SC_SSQ.txt")
