
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
qELO <- 0.001 #1  # First Catchability x Effort
qEHI <- 0.05 #4  # Second Catchability x Effort
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


# ==========================
# = Ecology Letters Params =
# ==========================
qELO <- 1 #0.001 #1  # First Catchability x Effort
qEHI <- 1.7169 #0.05 #4  # Second Catchability x Effort

Ho <- 1 #4    # Refuge biomass
DH <- 0.5  # Diffusion parameter
cHF <- 0.1  # Consumption rate by planktivore
alf <- 0.3  # Conversion efficiency of consumed phytoplankton to zooplankton
cPH <- 0.25  # Consumption rate of phytoplankton by zooplankton


fA <- 2  # Fecundity of adult piscivore (2 in OLD)
cJA <- 1E-3 #0.1  # Density dependent mortality rate of juveniles
cJF <- 0.5  # Consumption of juveniles by planktivores
cFA <- 0.3  # Consumption of planktivores by adult piscivores
vuln <- 1 #80  # Vulnerability coefficient (this is "v" in eco lett table/ equations)
hide <- 8 #80  # Hiding coefficient (this is "h" in eco lett table/ equations)
surv <- 0.5 #0.6  # Overwinter survivorship of adults
Fo <- 100  # Refuge density of planktivores  # 100 in OLD
DF <- 0.1 #0.09  # Diffusion parameter for planktivores
sigma <- 0.05  # SD of additive noise for planktivores (0.1 in May 07)
A2biom <- 0.2  # Convert A to kg / ha
J2biom <- 0.05  # Convert J to kg / ha
F2biom <- 1  # Convert F to kg / ha




# ======================
# = Initial Conditions =
# ======================
Ainit <- 300  # Critical A is about 106 based on Fish_Thresh2.R, 11 Jun 07
Finit <- 1
Hinit <- 5
Pinit <- 3
foodWeb.init <- c("At"=Ainit, "Ft"=Finit, "Jt"=Ainit*fA, "Ht"=Hinit, "Pt"=Pinit)


# ==================
# = Initial Values =
# ==================
nint <- 10  # Time steps per 'year' 
dt <- 1/nint
dtZ <- sqrt(dt)

nburn <- 1000
nstep <- nburn + 500  # total time steps
tstep <- 1:nstep

# qEvec <- c(rep(qELO, nburn), seq(qELO, qEHI, length.out=(nstep-nburn)))
# qEvec0 <- arima.sim(list(0.99,0,0), n=nstep-nburn, innov=rnorm(n=nstep-nburn, mean=0, sd=0.005))
qEvec0 <- c(
	seq(qELO,qEHI, length.out=(nstep-nburn)/3-1),
	seq(qEHI, qELO, length.out=(nstep-nburn)/3),
	seq(qELO, qEHI, length.out=(nstep-nburn)/3)
)

# qEvec0 <- seq(0,0, length.out=(nstep-nburn))
qEvec <- c(rep(qELO,nburn), qEvec0)


noise.vec <- rnorm(3*nstep)
noise.mat <- matrix(noise.vec, nrow=nstep, ncol=3)

# Set up vectors to hold simulation results

# Food web:
fWeb <- matrix(NA, nrow=nstep, ncol=5, dimnames=list(NULL, c("At", "Ft", "Jt", "Ht", "Pt")))
fWeb[1,] <- foodWeb.init


# ========================
# = Simulate Time Series =
# ========================
for(i in 2:nstep){	
	FWnext <- FWsim.step(
		qEvec[i], 
		fWeb[i-1, "At"],
		fWeb[i-1, "Ft"],
		fWeb[i-1, "Jt"],
		fWeb[i-1, "Ht"],
		fWeb[i-1, "Pt"],
		dt,
		dtZ,
		noise.mat[i,]
	)

	fWeb[i,] <- FWnext
}



# ===========
# = Figures =
# ===========
# Set indices to zoom in on the "decision" period
decide0 <- nburn+1
decide1 <- nstep

# dev.new()
HPccf <- ccf(fWeb[,"Ht"], fWeb[,"Pt"], plot=FALSE)
HPccf$lag[which.max(HPccf$acf)]

myCol <- function(n){
	colorRampPalette(c("#000099", "#00FEFF", "#45FE4F", "#FCFF00", "#FF9400", "#FF3100"))(n)
}
dev.new()
plot(as.data.frame(cbind(qEvec,fWeb))[decide0:decide1,], col=myCol(nstep-nburn), cex=0.75, pch=20)



dev.new(width=3, height=6)
par(mfrow=c(3,1), mar=c(2,2,0.25, 0.25), mgp=c(1.15, 0.25, 0), tcl=-0.15, ps=10, cex=1)
plot(tstep[decide0:decide1],qEvec[decide0:decide1],type='l',lwd=2,col='brown',xlab='',ylab='qE')
plot(tstep[decide0:decide1],fWeb[decide0:decide1,"At"],type='l',lwd=2,col='darkgreen',xlab='',ylab='Adults')
plot(tstep[decide0:decide1],fWeb[decide0:decide1,"Jt"],type='l',lwd=2,col='cyan',xlab='time step',ylab='Juveniles')


dev.new(width=3, height=6)
par(mfrow=c(3,1), mar=c(2,2,0.25, 0.25), mgp=c(1.15, 0.25, 0), tcl=-0.15, ps=10, cex=1)
plot(tstep[1:decide0],qEvec[1:decide0],type='l',lwd=2,col='brown',xlab='',ylab='qE')
plot(tstep[1:decide0],fWeb[1:decide0,"At"],type='l',lwd=2,col='darkgreen',xlab='',ylab='Adults')
plot(tstep[1:decide0],fWeb[1:decide0,"Jt"],type='l',lwd=2,col='cyan',xlab='time step',ylab='Juveniles')


dev.new(width=3, height=6)
par(mfrow=c(3,1), mar=c(2,2,0.25, 0.25), mgp=c(1.15, 0.25, 0), tcl=-0.15, ps=10, cex=1)
plot(tstep[decide0:decide1],fWeb[decide0:decide1,"Ft"],type='l',lwd=2,col='red',xlab='',ylab='Planktivores')
plot(tstep[decide0:decide1],fWeb[decide0:decide1,"Ht"],type='l',lwd=2,col='blue',xlab='',ylab='Herbivores')
plot(tstep[decide0:decide1],fWeb[decide0:decide1,"Pt"],type='l',lwd=2,col='green',xlab='time step',ylab='Phytoplankton')



dev.new()
par(mfrow=c(1,1),cex.lab=1.6,cex.axis=1.6,oma=c(2,2.2,2,4))
plot(tstep[decide0:decide1],10*At[decide0:decide1],type='l',lwd=2,col='magenta',xlab='time step',
     ylab='Piscivore or Forage Fish Biomass',ylim=c(0,140))
points(tstep[decide0:decide1],Ft[decide0:decide1],type='l',lwd=2,col='blue')
legend(x=1050,y=150,legend=c('Piscivore','Forage Fish'),col=c('magenta','blue'),lwd=c(2,2,2),bty='n',
       cex=1.4,text.font=1)
