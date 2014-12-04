
# Find initial values of state variables given a value of qE
FW.find.init <- function(qE){
	Ainit <- 200  # Critical A is about 106 based on Fish_Thresh2.R, 11 Jun 07
	Finit <- 1
	Hinit <- 5
	Pinit <- 3

	optim(c(Ainit, Finit, Ainit*fA, Hinit, Pinit), FWsim.rate, qE=qE, method="L-BFGS-B", lower=0, upper=Inf)$par
}
