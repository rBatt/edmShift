# Simulation of one time step *************************************************
FWsim.step <- function(qE,A0,F0,J0,H0,P0,dt,dtZ,nvec) { # Start simulation function

	NoiseF <- nvec[1]
	NoiseH <- nvec[2]
	NoiseP <- nvec[3]

	# Fish dynamics
	#Arate <- (surv/nint)*J0 - qE*A0 - ((1-surv)/nint)*A0
	Arate <- (surv)*J0 - qE*A0 - ((1-surv))*A0
	Frate <- DF*(Fo-F0) - cFA*F0*A0
	Jpredloss <- (-cJA*J0*A0)-(cJF*vuln*J0*F0/(hide + vuln + cJF*F0) ) # Note this is negative
	
	#Jrate <- (fA/nint)*A0 + Jpredloss - (surv/nint)*J0 
	Jrate <- (fA)*A0 + Jpredloss - (surv)*J0
	A1 <- A0 + (Arate*dt)   # Update A numerically
	F1 <- F0 + (Frate*dt) + (sigma*NoiseF*dtZ)
	J1 <- J0 + (Jrate*dt)
	A1 <- max(A1,0.1)  # Force A1 greater than 0.1
	F1 <- max(F1,0.1)  # Force F greater than 0.1
	J1 <- max(J1,0.1)  # Force J greater than 0.1
	
	# Zooplankton dynamics
	Hrate <- DH*(Ho-H0) + alf*cPH*H0*P0 - cHF*H0*F0
	H1 <- H0 + (Hrate*dt) + (sigma*NoiseH*dtZ)
	H1 <- max(H1,0.1)  # Force H greater than 0.01
	
	# Phytoplankton dynamics
	Pbar <- P0   # Set P value for vertical integration
	gamvec <- GAMMA(Zvec,Pbar) # vertical sequence of light effect on growth
	gamI <- dZ*sum(gamvec)  # vertically integrated light effect on growth
	Prate <- (rP*Load*gamI*P0) - (mP*P0) - (cPH*H0*P0)  
	P1 <- P0 + (Prate*dt) + (sigma*NoiseP*dtZ)
	P1 <- max(P1,0.1)  # Force P greater than 0.1

	# Construct list for output
	SimList <- list(A1,F1,J1,H1,P1)
	return(SimList)

}  # End simulation function  ************************************