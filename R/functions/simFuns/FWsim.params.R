
# ==============================
# = Trophic Cascade Parameters =
# ==============================	
paramSet <- c("EcoLett","QD")[1]
	
fw.choice <- paramSet


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

if(fw.choice=="QD"){

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
}

if(fw.choice=="EcoLett"){
	# ==========================
	# = Ecology Letters Params =
	# ==========================

	Ho <- 1 #4    # Refuge biomass
	DH <- 0.5  # Diffusion parameter for herbivore (zooplankton)
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
	sigma <- 0.1 #0.05  # SD of additive noise for planktivores (0.1 in May 07)
	A2biom <- 0.2  # Convert A to kg / ha
	J2biom <- 0.05  # Convert J to kg / ha
	F2biom <- 1  # Convert F to kg / ha
}



# ======================
# = Initial Conditions =
# ======================
nint <- 10  # Time steps per 'year' 
dt <- 1/nint
dtZ <- sqrt(dt)
nburn <- 1000 # with the use of FW.find.init(), don't need much burn in, but still appears necessary both as check and b/c stochasticity changes things (?)

