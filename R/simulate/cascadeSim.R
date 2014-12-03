
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

# ===================
# = Load Parameters =
# ===================
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

# ===========
# = Options =
# ===========
# nburn <- 1000
# nstep <- nburn + 2000  # total time steps

qELO <- 1 #0.001 #1  # First Catchability x Effort
qEHI <- 1.38 #1.7169 #0.05 #4  # Second Catchability x Effort


step.choice <- 500
fw <- FWsim.wrap(qE=c(qELO,qEHI), step=step.choice, mthd="linear")



# ===========
# = Figures =
# ===========
# TODO Needs to be moved to /edmShift/R/figures ... but no hurry, this is really brief anyway
dev.new()
plot(as.data.frame(fw), col=myCol(step.choice), cex=0.75, pch=20)
