
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

# params.objs <- c("A2biom", "Ainit", "alf", "cFA", "cHF", "cJA", "cJF", "cPH", "DF", "DH", "DOC", "dt", "dtZ", "dZ", "eps0", "epsP", "F2biom", "fA", "Finit", "Fmax", "Fo", "foodWeb.init", "hide", "Hinit", "Ho", "I0", "J2biom", "k_inh", "k_sat", "Load", "mP", "nint", "nZ", "paramSet", "Pinit", "rP", "sigma", "surv", "vuln", "Zmix", "Zvec")
# for(i in 1:length(params.objs)){
# 	present.params <- exists(params.objs[i])
# }

# ===========
# = Options =
# ===========



FWsim.wrap <- function(qE=1.2, mthd=c("constant","linear"), steps=500, ...){
	mthd <- match.arg(mthd)
	nstep <- nburn + steps  # total time steps
	tstep <- 1:nstep
	
	if(length(qE)>1){
		if(mthd=="constant"){
			xout <- as.numeric(cut(1:steps, length(qE)))
		}else if(mthd=="linear"){
			xout <- seq(1, length(qE), length.out=steps)
		}
	}else{
		xout <- rep(1, steps)
	}
	# qELO <- 1 #0.001 #1  # First Catchability x Effort
	# qEHI <- 1.38 #1.7169 #0.05 #4  # Second Catchability x Effort
	qEvec0 <- approx(1:length(qE), qE, xout=xout, rule=2, method="linear")$y
	qEvec <- c(rep(qE[1],nburn), qEvec0)
	
	

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
	
	equi <- (nburn+1):(nburn+steps)
	return(cbind("qE"=qEvec[equi], fWeb[equi,]))

}
