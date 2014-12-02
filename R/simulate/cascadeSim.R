# ============================
# = Simulate Trophic Cascade =
# ============================

# =================
# = Steve's Notes =
# =================
# Treat-and-Halt using the foodweb model, rolling window statistics, and quickest detection
# Foodweb model for simulating transients, adapted from FS6_trans0.r
# This version has continuous reproduction and mortality for piscivores, not pulsed
# Simulation of the full food web for investigating the squeal
#  employed in the PLoS paper
# Noise is added to F, H and P
# SRC 12 Nov 2012


# ==================
# = Load Libraries =
# ==================
library(zoo)



# ============================
# = Phytoplankton Parameters =
# ============================
I0 <- 300  # Surface irradiance, microEinsteins m-1 s-1
# P-I curve parameters, Follows et al.
k_sat <- 0.012 # per microEinsteins m-1 s-1
k_inh <- 0.004 # per microEinsteins m-1 s-1 (nominal 0.004, range 0.001-0.007)
# Light extinction parameters, Carpenter et al. L&O 1998
DOC <- 5  # Assumed DOC conc, g m-3
eps0 <- 0.0213 + 0.0514*DOC  # Baseline, per m (nominal DOC par 0.0514)
epsP <- 0.0177   # Phytoplankton, per m2 (mg phosphorus)-1

# Derived parameter from Follows et al.
Fmax <- ((k_sat + k_inh)/k_sat)*exp(-(k_inh/k_sat)*log(k_inh/(k_sat+k_inh)))

# Information for depth integration
Zmix <- 4  # Mixed layer depth, m
nZ <- 10  # Steps for vertical integration
dZ <- Zmix/nZ
Zvec <- seq(0,Zmix,by=dZ)

rP <- 3  # Phytoplankton growth parameter per unit phosphorus load
Load <- 0.6  # Daily phosphorus load
mP <- 0.1  # Phytoplankton daily mortality 


# ==========================
# = Zooplankton Parameters =
# ==========================
Ho <- 4    # Refuge biomass    # 1 in OLD
DH <- 0.5  # Diffusion parameter  # 0.5 in OLD
cHF <- 0.1  # Consumption rate by planktivore
alf <- 0.3  # Conversion efficiency of consumed phytoplankton to zooplankton
cPH <- 0.25  # Consumption rate of phytoplankton by zooplankton


# ===================
# = Fish Parameters =
# ===================
qELO <- 1  # First Catchability x Effort
qEHI <- 4  # Second Catchability x Effort
fA <- 2  # Fecundity of adult piscivore (2 in OLD)
cJA <- 0.1  # Density dependent mortality rate of juveniles
cJF <- 0.5  # Consumption of juveniles by planktivores
cFA <- 0.3  # Consumption of planktivores by adult piscivores
vuln <- 80  # Vulnerability coefficient
hide <- 80  # Hiding coefficient
surv <- 0.6  # Overwinter survivorship of adults
Fo <- 200  # Refuge density of planktivores  # 100 in OLD
DF <- 0.09  # Diffusion parameter for planktivores
sigma <- 0.05  # SD of additive noise for planktivores (0.1 in May 07)
A2biom <- 0.2  # Convert A to kg / ha
J2biom <- 0.05  # Convert J to kg / ha
F2biom <- 1  # Convert F to kg / ha

# ======================
# = Initial Conditions =
# ======================
Ainit <- 20  # Critical A is about 106 based on Fish_Thresh2.R, 11 Jun 07
Finit <- 1
Hinit <- 5
Pinit <- 3



# # Compute rolling window autocorrelation time
# ACtime = function(x) {
#   zzz=acf(x,lag=1,plot=FALSE)
#   zz=zzz$acf[2]
#   ACt=-1/log(zz)
#   return(ACt)
# }
#
# # Compute rolling window autocorrelation
# AClag1 = function(x) {
#   zzz=acf(x,lag=1,plot=FALSE)
#   zz=zzz$acf[2]
#   #ACt=-1/log(zz)
#   return(zz)
# }


# ==================
# = Initial Values =
# ==================
nint <- 10  # Time steps per 'year' 
dt <- 1/nint
dtZ <- sqrt(dt)

nburn <- 1000
nstep <- nburn + 1000  # total time steps
tstep <- 1:nstep

qEvec <- c(rep(0.001,nburn),seq(0.001, 0.05, length.out=(nstep-nburn)))

noise.vec <- rnorm(3*nstep)
noise.mat <- matrix(noise.vec, nrow=nstep, ncol=3)

# Set up vectors to hold simulation results

# Food web:
At <- rep(0,nstep)
Ft <- At
Jt <- At
Ht <- At
Pt <- At
At[1] <- Ainit
Ft[1] <- Finit
Jt[1] <- fA*Ainit
Ht[1] <- Hinit
Pt[1] <- Pinit

# # Indicators:
# SDt <- rep(0,nstep)
# ARt <- SDt
# SRt <- rep(1,nstep)
#
# # Alarm statistics
# LAMvec <- rep(0,nstep)
# Rvec <- rep(0,nstep)
# Alarmvec <- rep(0,nstep)

# ===============
# = Run Burn-in =
# ===============
for(i in 2:nburn)  {
  qE <- qEvec[i]
  FWnext <- FWsim.step(qE,At[i-1],Ft[i-1],Jt[i-1],Ht[i-1],Pt[i-1],dt,dtZ,noise.mat[i,])
  At[i] <- FWnext[[1]]
  Ft[i] <- FWnext[[2]]
  Jt[i] <- FWnext[[3]]
  Ht[i] <- FWnext[[4]]
  Pt[i] <- FWnext[[5]]
}

# Start the decision-making loop

# Set alarm boundary
# A.adj <- 1000 # A.adj = exp(logA.adj)
# print(' ',quote=FALSE)
# print(c('squeal boundary for SR statistic',A.adj),quote=FALSE)
# LL.stat <- 2*log(A.adj)
# pNLL <- 1 - pchisq(LL.stat,df=1)
# print(c('Prob. for baseline model at squeal boundary',round(pNLL,6)),quote=FALSE)
#
# # Set window length for statistics
# winlen <- 60

# ========================
# = Simulate time series =
# ========================
# Gather true qE values
qEtrue <- qEvec
for(i in (nburn+1):(nstep) ) {
	
	# Update food web
	qE <- qEvec[i] #ifelse(Alarmvec[i-1]<0.5,qEvec[i],0.001)
	qEtrue[i] <- qE
	FWnext <- FWsim.step(qE,At[i-1],Ft[i-1],Jt[i-1],Ht[i-1],Pt[i-1],dt,dtZ,noise.mat[i,])
	At[i] <- FWnext[[1]]
	Ft[i] <- FWnext[[2]]
	Jt[i] <- FWnext[[3]]
	Ht[i] <- FWnext[[4]]
	Pt[i] <- FWnext[[5]]
	
	# Update indicators
	# Xvec <- Ft[(i-winlen):i]
	# SDt[i] <- sd(Xvec)
	# ARt[i] <- ACtime(Xvec) # lag 1 autocorr time
	#ARt[i] = AClag1(Xvec) # lag 1 autocorr
	
	# Update SR statistic and alarm
	
	# Compute current lamda
	# f.L <- dnorm(SDt[i], mean=0.5, sd=0.5, log=TRUE)
# 	g.L <- f.L#dnorm(SDt[i],mean=2,sd=2,log=TRUE)
# 	logLam <- (g.L-f.L)#/1.e+5 # rescale if it blows up
# 	LAM <- exp(logLam)
	
	# update R
	# LAMvec[i] <- LAM
	# Rtest <- (1+Rvec[i-1])*LAM
	# # Check for alarm
	# Alarmvec[i] <- ifelse(Rtest>A.adj,1,Alarmvec[i-1])
	# Rvec[i] <- ifelse(Rtest>A.adj,1,Rtest)
} # END of decision-making loop

# ===========
# = Figures =
# ===========
dev.new(width=3, height=6)
par(mfrow=c(3,1), mar=c(2,2,0.25, 0.25), mgp=c(1.15, 0.25, 0), tcl=-0.15, ps=10, cex=1)
plot(tstep,qEvec,type='l',lwd=2,col='brown',xlab='',ylab='qE')
plot(tstep,At,type='l',lwd=2,col='darkgreen',xlab='',ylab='Adults')
plot(tstep,Jt,type='l',lwd=2,col='cyan',xlab='time step',ylab='Juveniles')

dev.new(width=3, height=6)
par(mfrow=c(3,1), mar=c(2,2,0.25, 0.25), mgp=c(1.15, 0.25, 0), tcl=-0.15, ps=10, cex=1)
plot(tstep,Ft,type='l',lwd=2,col='red',xlab='',ylab='Planktivores')
plot(tstep,Ht,type='l',lwd=2,col='blue',xlab='',ylab='Herbivores')
plot(tstep,Pt,type='l',lwd=2,col='green',xlab='time step',ylab='Phytoplankton')

# Set indices to zoom in on the "decision" period
# decide0 <- nburn+1
# decide1 <- nstep
# dev.new()
# par(mfrow=c(2,1),cex.lab=1.6,cex.axis=1.6)
# plot(tstep[decide0:decide1],SDt[decide0:decide1],type='l',
#      lwd=2,col='red',xlab='time step',ylab='Rolling SD')
# grid()
# plot(tstep[decide0:decide1],ARt[decide0:decide1],type='l',
#      lwd=2,col='blue',xlab='time step',ylab='Rolling AC time')
# grid()

# dev.new()
# par(mfrow=c(3,1),cex.lab=1.6,cex.axis=1.6)
# plot(tstep[decide0:decide1],LAMvec[decide0:decide1],type='l',
#      lwd=2,col='magenta',xlab='time step',ylab='Lamda')
# plot(tstep[decide0:decide1],Rvec[decide0:decide1],type='l',
#      lwd=2,col='purple',xlab='time step',ylab='S-R statistic')
# plot(tstep[decide0:decide1],Alarmvec[decide0:decide1],
#      type='l',lwd=2,col='red',xlab='time step',ylab='Alarm Flag')

dev.new()
par(mfrow=c(1,1),cex.lab=1.6,cex.axis=1.6,oma=c(2,2.2,2,4))
plot(tstep[decide0:decide1],10*At[decide0:decide1],type='l',lwd=2,col='magenta',xlab='time step',
     ylab='Piscivore or Forage Fish Biomass',ylim=c(0,140))
points(tstep[decide0:decide1],Ft[decide0:decide1],type='l',lwd=2,col='blue')
legend(x=1050,y=150,legend=c('Piscivore','Forage Fish'),col=c('magenta','blue'),lwd=c(2,2,2),bty='n',
       cex=1.4,text.font=1)
# par(new=TRUE,mfg=c(1,1)) # add the new axis
# plot(tstep[decide0:decide1],SDt[decide0:decide1],type='l',
#      lwd=2,col='darkred',yaxt='n',xaxt='n',xlab=' ',ylab=' ')
# axis(4,pretty(range(SDt[decide0:decide1],na.rm=TRUE),5),col='darkred',col.axis='darkred')
# mtext('SD of Forage Fish',side=4,line=3,font=1,cex=1.6,col='darkred')

# Save data for key graphic
# save(tstep,At,Ft,SDt,file='Fish_TreatHalt_NoSqueal.Rdata')
