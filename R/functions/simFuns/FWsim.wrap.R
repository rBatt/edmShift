
# ============================
# = Simulate Trophic Cascade =
# ============================
FWsim.wrap <- function(qE=1.2, mthd=c("constant","linear"), steps=500, ...){
	mthd <- match.arg(mthd)
	nstep <- nburn + steps  # total time steps
	tstep <- 1:nstep
	
	
	# ====================
	# = Create qE values =
	# ====================
	# First, have to figure out when qE changes – create an "x" vector w/ each unique value corresponding to a value of qE ("y")
	if(length(qE)>1){ # if more than 1 qE
		if(mthd=="constant"){ # if qE is to be held constant between each value of qE provided
			xout <- as.numeric(cut(1:steps, length(qE)))
		}else if(mthd=="linear"){ # if qE is to change linearly between each value of qE provided
			xout <- seq(1, length(qE), length.out=steps)
		}
	}else{
		xout <- rep(1, steps) # if only 1 value of qE (well, not greater than 1)
	}
	
	# Second, need to find values of qE for each time step given qE values to hit and change method
	qEvec0 <- approx(1:length(qE), qE, xout=xout, rule=2, method=mthd)$y # either constant or linear between time steps
	qEvec <- c(rep(qE[1],nburn), qEvec0) # vector of qE values for each time step
	
	
	# =========
	# = Noise =
	# =========
	noise.vec <- rnorm(3*nstep)
	noise.mat <- matrix(noise.vec, nrow=nstep, ncol=3)
	
	
	# ===========================
	# = Food Web Initial Values =
	# ===========================
	fWeb <- matrix(NA, nrow=nstep, ncol=5, dimnames=list(NULL, c("At", "Ft", "Jt", "Ht", "Pt")))
	foodWeb.init <- FW.find.init(qE=qE[1])
	names(foodWeb.init) <- c("At", "Ft", "Jt", "Ht", "Pt")
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
	
	equi <- (nburn+1):(nburn+steps) # values in the time series when the simulation is at equilibrium/ past burn-in
	return(cbind("qE"=qEvec[equi], fWeb[equi,])) # return
}
