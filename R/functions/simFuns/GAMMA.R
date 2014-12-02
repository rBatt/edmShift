# From SRC
# Phytoplankton Growth
	GAMMA <- function(z,Pbar) {
	Iz <- I0*exp(-z*(eps0+epsP*Pbar))
	rate <- (1/Fmax)*(1 - exp(-k_sat*Iz))*exp(-k_inh*Iz)
}