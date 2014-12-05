

# =======================
# = Load Stat Functions =
# =======================
statFuns.location <- "~/Documents/School&Work/pinskyPost/edmShift/R/functions/statFuns"
invisible(sapply(paste(statFuns.location, list.files(statFuns.location), sep="/"), source, .GlobalEnv))


# ===========================
# = Load Plotting Functions =
# ===========================
figFuns.location <- "~/Documents/School&Work/pinskyPost/edmShift/R/functions/figFuns"
invisible(sapply(paste(figFuns.location, list.files(figFuns.location), sep="/"), source, .GlobalEnv))


# ================
# = Load Results =
# ================
load("/Users/Battrd/Documents/School&Work/pinskyPost/edmShift/results/FWsim/FWsim.RData")


# ==================
# = Load Libraries =
# ==================
library(FNN)


# ===========
# = Options =
# ===========
nLS.opt <- 10



# ================
# = Experiment 1 =
# ================

# Experiment 1.1
ccm.fw1.1 <- list()
ccm.fw1.1$HP <- ccm(data=fw.exp1[,,1], var1N="Ht", var2N="Pt", nLS=nLS.opt)


# Experiment 1.2
ccm.fw1.2 <- list()

# Adults 1.2
ccm.fw1.2$AF <- ccm(data=fw.exp1[,,2], var1N="At", var2N="Ft", nLS=nLS.opt)
ccm.fw1.2$AJ <- ccm(data=fw.exp1[,,2], var1N="At", var2N="Jt", nLS=nLS.opt)
ccm.fw1.2$AH <- ccm(data=fw.exp1[,,2], var1N="At", var2N="Ht", nLS=nLS.opt)
ccm.fw1.2$AP <- ccm(data=fw.exp1[,,2], var1N="At", var2N="Pt", nLS=nLS.opt)

# Fish 1.2
ccm.fw1.2$FJ <- ccm(data=fw.exp1[,,2], var1N="Ft", var2N="Jt", nLS=nLS.opt)
ccm.fw1.2$FH <- ccm(data=fw.exp1[,,2], var1N="Ft", var2N="Ht", nLS=nLS.opt)
ccm.fw1.2$FP <- ccm(data=fw.exp1[,,2], var1N="Ft", var2N="Pt", nLS=nLS.opt)

# Juveniles 1.2
ccm.fw1.2$JH <- ccm(data=fw.exp1[,,2], var1N="Jt", var2N="Ht", nLS=nLS.opt)
ccm.fw1.2$JP <- ccm(data=fw.exp1[,,2], var1N="Jt", var2N="Pt", nLS=nLS.opt)

# Herbivores 1.2
ccm.fw1.2$HP <- ccm(data=fw.exp1[,,2], var1N="Ht", var2N="Pt", nLS=nLS.opt)



# Experiment 1.3
ccm.fw1.3 <- list()

# Adults 1.3
ccm.fw1.3$AF <- ccm(data=fw.exp1[,,3], var1N="At", var2N="Ft", nLS=nLS.opt)
ccm.fw1.3$AJ <- ccm(data=fw.exp1[,,3], var1N="At", var2N="Jt", nLS=nLS.opt)
ccm.fw1.3$AH <- ccm(data=fw.exp1[,,3], var1N="At", var2N="Ht", nLS=nLS.opt)
ccm.fw1.3$AP <- ccm(data=fw.exp1[,,3], var1N="At", var2N="Pt", nLS=nLS.opt)

# Fish 1.3
ccm.fw1.3$FJ <- ccm(data=fw.exp1[,,3], var1N="Ft", var2N="Jt", nLS=nLS.opt)
ccm.fw1.3$FH <- ccm(data=fw.exp1[,,3], var1N="Ft", var2N="Ht", nLS=nLS.opt)
ccm.fw1.3$FP <- ccm(data=fw.exp1[,,3], var1N="Ft", var2N="Pt", nLS=nLS.opt)

# Juveniles 1.3
ccm.fw1.3$JH <- ccm(data=fw.exp1[,,3], var1N="Jt", var2N="Ht", nLS=nLS.opt)
ccm.fw1.3$JP <- ccm(data=fw.exp1[,,3], var1N="Jt", var2N="Pt", nLS=nLS.opt)

# Herbivores 1.3
ccm.fw1.3$HP <- ccm(data=fw.exp1[,,3], var1N="Ht", var2N="Pt", nLS=nLS.opt)





# Experiment 1.4
ccm.fw1.4 <- list()

# Fish 1.4
ccm.fw1.4$FJ <- ccm(data=fw.exp1[,,4], var1N="Ft", var2N="Jt", nLS=nLS.opt)
ccm.fw1.4$FH <- ccm(data=fw.exp1[,,4], var1N="Ft", var2N="Ht", nLS=nLS.opt)
ccm.fw1.4$FP <- ccm(data=fw.exp1[,,4], var1N="Ft", var2N="Pt", nLS=nLS.opt)

# Juveniles 1.4
ccm.fw1.4$JH <- ccm(data=fw.exp1[,,4], var1N="Jt", var2N="Ht", nLS=nLS.opt)
ccm.fw1.4$JP <- ccm(data=fw.exp1[,,4], var1N="Jt", var2N="Pt", nLS=nLS.opt)

# Herbivores 1.4
ccm.fw1.4$HP <- ccm(data=fw.exp1[,,4], var1N="Ht", var2N="Pt", nLS=nLS.opt)




# Experiment 1.5
ccm.fw1.5 <- list()

# Fish 1.5
ccm.fw1.5$FJ <- ccm(data=fw.exp1[,,5], var1N="Ft", var2N="Jt", nLS=nLS.opt)
ccm.fw1.5$FH <- ccm(data=fw.exp1[,,5], var1N="Ft", var2N="Ht", nLS=nLS.opt)
ccm.fw1.5$FP <- ccm(data=fw.exp1[,,5], var1N="Ft", var2N="Pt", nLS=nLS.opt)

# Juveniles 1.5
ccm.fw1.5$JH <- ccm(data=fw.exp1[,,5], var1N="Jt", var2N="Ht", nLS=nLS.opt)
ccm.fw1.5$JP <- ccm(data=fw.exp1[,,5], var1N="Jt", var2N="Pt", nLS=nLS.opt)

# Herbivores 1.5
ccm.fw1.5$HP <- ccm(data=fw.exp1[,,5], var1N="Ht", var2N="Pt", nLS=nLS.opt)






# Experiment 1.6
ccm.fw1.6 <- list()

# Fish 1.6
ccm.fw1.6$FJ <- ccm(data=fw.exp1[,,6], var1N="Ft", var2N="Jt", nLS=nLS.opt)
ccm.fw1.6$FH <- ccm(data=fw.exp1[,,6], var1N="Ft", var2N="Ht", nLS=nLS.opt)
ccm.fw1.6$FP <- ccm(data=fw.exp1[,,6], var1N="Ft", var2N="Pt", nLS=nLS.opt)

# Juveniles 1.6
ccm.fw1.6$JH <- ccm(data=fw.exp1[,,6], var1N="Jt", var2N="Ht", nLS=nLS.opt)
ccm.fw1.6$JP <- ccm(data=fw.exp1[,,6], var1N="Jt", var2N="Pt", nLS=nLS.opt)

# Herbivores 1.6
ccm.fw1.6$HP <- ccm(data=fw.exp1[,,6], var1N="Ht", var2N="Pt", nLS=nLS.opt)






# Experiment 1.7
ccm.fw1.7 <- list()

# Fish 1.7
ccm.fw1.7$FJ <- ccm(data=fw.exp1[,,7], var1N="Ft", var2N="Jt", nLS=nLS.opt)
ccm.fw1.7$FH <- ccm(data=fw.exp1[,,7], var1N="Ft", var2N="Ht", nLS=nLS.opt)
ccm.fw1.7$FP <- ccm(data=fw.exp1[,,7], var1N="Ft", var2N="Pt", nLS=nLS.opt)

# Juveniles 1.7
ccm.fw1.7$JH <- ccm(data=fw.exp1[,,7], var1N="Jt", var2N="Ht", nLS=nLS.opt)
ccm.fw1.7$JP <- ccm(data=fw.exp1[,,7], var1N="Jt", var2N="Pt", nLS=nLS.opt)

# Herbivores 1.7
ccm.fw1.7$HP <- ccm(data=fw.exp1[,,7], var1N="Ht", var2N="Pt", nLS=nLS.opt)




# ================
# = Experiment 2 =
# ================
# Experiment 2
ccm.fw2 <- list()

# Adults 2
ccm.fw2$AF <- ccm(data=fw.exp2, var1N="At", var2N="Ft", nLS=nLS.opt)
ccm.fw2$AJ <- ccm(data=fw.exp2, var1N="At", var2N="Jt", nLS=nLS.opt)
ccm.fw2$AH <- ccm(data=fw.exp2, var1N="At", var2N="Ht", nLS=nLS.opt)
ccm.fw2$AP <- ccm(data=fw.exp2, var1N="At", var2N="Pt", nLS=nLS.opt)

# Fish 2
ccm.fw2$FJ <- ccm(data=fw.exp2, var1N="Ft", var2N="Jt", nLS=nLS.opt)
ccm.fw2$FH <- ccm(data=fw.exp2, var1N="Ft", var2N="Ht", nLS=nLS.opt)
ccm.fw2$FP <- ccm(data=fw.exp2, var1N="Ft", var2N="Pt", nLS=nLS.opt)

# Juveniles 2
ccm.fw2$JH <- ccm(data=fw.exp2, var1N="Jt", var2N="Ht", nLS=nLS.opt)
ccm.fw2$JP <- ccm(data=fw.exp2, var1N="Jt", var2N="Pt", nLS=nLS.opt)

# Herbivores 2
ccm.fw2$HP <- ccm(data=fw.exp2, var1N="Ht", var2N="Pt", nLS=nLS.opt)




# ================
# = Experiment 3 =
# ================
# Experiment 3
ccm.fw3 <- list()

# Adults 3
ccm.fw3$AF <- ccm(data=fw.exp3, var1N="At", var2N="Ft", nLS=nLS.opt)
ccm.fw3$AJ <- ccm(data=fw.exp3, var1N="At", var2N="Jt", nLS=nLS.opt)
ccm.fw3$AH <- ccm(data=fw.exp3, var1N="At", var2N="Ht", nLS=nLS.opt)
ccm.fw3$AP <- ccm(data=fw.exp3, var1N="At", var2N="Pt", nLS=nLS.opt)

# Fish 3
ccm.fw3$FJ <- ccm(data=fw.exp3, var1N="Ft", var2N="Jt", nLS=nLS.opt)
ccm.fw3$FH <- ccm(data=fw.exp3, var1N="Ft", var2N="Ht", nLS=nLS.opt)
ccm.fw3$FP <- ccm(data=fw.exp3, var1N="Ft", var2N="Pt", nLS=nLS.opt)

# Juveniles 3
ccm.fw3$JH <- ccm(data=fw.exp3, var1N="Jt", var2N="Ht", nLS=nLS.opt)
ccm.fw3$JP <- ccm(data=fw.exp3, var1N="Jt", var2N="Pt", nLS=nLS.opt)

# Herbivores 3
ccm.fw3$HP <- ccm(data=fw.exp3, var1N="Ht", var2N="Pt", nLS=nLS.opt)





# ================
# = Experiment 4 =
# ================
# Experiment 4
ccm.fw4 <- list()

# Adults 4
ccm.fw4$AF <- ccm(data=fw.exp4, var1N="At", var2N="Ft", nLS=nLS.opt)
ccm.fw4$AJ <- ccm(data=fw.exp4, var1N="At", var2N="Jt", nLS=nLS.opt)
ccm.fw4$AH <- ccm(data=fw.exp4, var1N="At", var2N="Ht", nLS=nLS.opt)
ccm.fw4$AP <- ccm(data=fw.exp4, var1N="At", var2N="Pt", nLS=nLS.opt)

# Fish 4
ccm.fw4$FJ <- ccm(data=fw.exp4, var1N="Ft", var2N="Jt", nLS=nLS.opt)
ccm.fw4$FH <- ccm(data=fw.exp4, var1N="Ft", var2N="Ht", nLS=nLS.opt)
ccm.fw4$FP <- ccm(data=fw.exp4, var1N="Ft", var2N="Pt", nLS=nLS.opt)

# Juveniles 4
ccm.fw4$JH <- ccm(data=fw.exp4, var1N="Jt", var2N="Ht", nLS=nLS.opt)
ccm.fw4$JP <- ccm(data=fw.exp4, var1N="Jt", var2N="Pt", nLS=nLS.opt)

# Herbivores 4
ccm.fw4$HP <- ccm(data=fw.exp4, var1N="Ht", var2N="Pt", nLS=nLS.opt)



save(
	ccm.fw1.1, ccm.fw1.2, ccm.fw1.3, ccm.fw1.4, ccm.fw1.5, ccm.fw1.6, ccm.fw1.7, 
	ccm.fw2, ccm.fw3, ccm.fw4,
	file="/Users/Battrd/Documents/School&Work/pinskyPost/edmShift/results/FWsim/ccm.fw.RData"
)
