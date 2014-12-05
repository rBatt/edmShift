
# ============================
# = Simulate Trophic Cascade =
# ============================
set.seed(2)

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


# =================================
# = Load Parameters and Functions =
# =================================
simFuns.location <- "~/Documents/School&Work/pinskyPost/edmShift/R/functions/simFuns"
invisible(sapply(paste(simFuns.location, list.files(simFuns.location), sep="/"), source, .GlobalEnv))


# ===========================
# = Load Plotting Functions =
# ===========================
figFuns.location <- "~/Documents/School&Work/pinskyPost/edmShift/R/functions/figFuns"
invisible(sapply(paste(figFuns.location, list.files(figFuns.location), sep="/"), source, .GlobalEnv))


# ==============================
# = Experiment #1: Constant qE =
# ==============================
# Set up options and result array
exp1.steps <- 200 # number of steps for each qE in experiment 1
# exp1.qE <- c(1, 1.2, 1.4, 1.6, 1.7, 1.8, 1.9) # qE values for Exp #1; from Fig S2.1 Carpenter et al. 2008 Eco Lett
exp1.qE <- c(0, 0.7, 1.19, 1.3, 1.5, 1.7, 2.0) # qE values for Exp #1
fw.exp1 <- array(data=NA, dim=c(exp1.steps, 6, length(exp1.qE)), dimnames=list(NULL, c("qE","At","Ft","Jt","Ht","Pt"),NULL))

# Run Experiment 1
for(i in 1:length(exp1.qE)){
	fw.exp1[,,i] <- FWsim.wrap(qE=exp1.qE[i], step=exp1.steps, mthd="constant")
}


# ===============================================
# = Experiment #2: Waver far from tipping point =
# ===============================================
# Set up Exp 2
exp2.steps <- 200 # number of steps for each qE
exp2.qE <- c(0.7, 1.19, 0.7, 1.19)
fw.exp2 <- array(data=NA, dim=c(exp2.steps, 6, length(exp2.qE)), dimnames=list(NULL, c("qE","At","Ft","Jt","Ht","Pt"),NULL))


# Run Experiment 2
fw.exp2 <- FWsim.wrap(qE=exp2.qE, step=exp2.steps, mthd="linear")


# ===================================
# = Experiment #3: Gradual Increase =
# ===================================
# Set up Exp 3
exp3.steps <- 300 # number of steps for each qE
exp3.qE <- c(1.18, 1.72)
fw.exp3 <- array(data=NA, dim=c(exp3.steps, 6, 1), dimnames=list(NULL, c("qE","At","Ft","Jt","Ht","Pt"),NULL))

# Run Experiment 3
fw.exp3 <- FWsim.wrap(qE=exp3.qE, step=exp3.steps, mthd="linear")


# ===================================
# = Experiment 4: Waver all over qE =
# ===================================
# Set up Exp 4
exp4.steps <- 400
exp4.qE <- c(rep(c(0.9, 1.2, 1.5, 1.8), each=2)+rep(c(0.1, -0.1),4))
fw.exp4 <- array(data=NA, dim=c(exp4.steps, 6, 1), dimnames=list(NULL, c("qE","At","Ft","Jt","Ht","Pt"),NULL))

# Run Experiment 4
fw.exp4 <- FWsim.wrap(qE=exp4.qE, step=exp4.steps, mthd="linear")

save(
	exp1.steps, exp1.qE, fw.exp1,
	exp2.steps, exp2.qE, fw.exp2,
	exp3.steps, exp3.qE, fw.exp3,
	exp4.steps, exp4.qE, fw.exp4,
	file="/Users/Battrd/Documents/School&Work/pinskyPost/edmShift/results/FWsim/FWsim.RData"
	)