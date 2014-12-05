
# ================
# = Load Results =
# ================
load("/Users/Battrd/Documents/School&Work/pinskyPost/edmShift/results/FWsim/FWsim.RData")


# ============================
# = Sim Figure File Skeleton =
# ============================
simFile <- "/Users/Battrd/Documents/School&Work/pinskyPost/edmShift/figures/FWsim/"


# ========================
# = Set Plotting Options =
# ========================
figRes <- 200


# =======================
# = Plot Experiment 1.1 =
# =======================
png(paste0(simFile, "FWsim.ccm.expt1.qE", format(exp1.qE[1], nsmall=2), ".png"), width=3, height=3, units="in", res=figRes)
par(mar=c(1.75,1.75,0.2,0.2), mgp=c(0.85,0.25,0), tcl=-0.2, ps=10, family="Times", cex=1)
ccmPlot(ccm.fw1.1$HP)
dev.off()


# =======================
# = Plot Experiment 1.2 =
# =======================
# dev.new(width=7.5, height=7.5)
png(paste0(simFile, "FWsim.ccm.expt1.qE", format(exp1.qE[2], nsmall=2), ".png"), width=7.5, height=7.5, units="in", res=figRes)
par(mar=c(1,1.75,0.2,0.2), mgp=c(0.85,0.25,0), oma=c(1.75,0,2,2), tcl=-0.2, ps=11, family="Times", cex=1)
lay.dat <- c(1, 2, 3, 4, 11, 5, 6, 7, 0, 12, 8, 9, 0, 0, 13, 10)
lay.mat <- matrix(lay.dat, nrow=4)
layout(lay.mat)


# Adults 1.2
ccmPlot(ccm.fw1.2$AF)
mtext("At", side=3, line=0.5, cex=1.2, font=2)
ccmPlot(ccm.fw1.2$AJ)
ccmPlot(ccm.fw1.2$AH)
ccmPlot(ccm.fw1.2$AP)

# Fish 1.2
ccmPlot(ccm.fw1.2$FJ)
ccmPlot(ccm.fw1.2$FH)
ccmPlot(ccm.fw1.2$FP)

# Juveniles 
ccmPlot(ccm.fw1.2$JH)
ccmPlot(ccm.fw1.2$JP)

# Herbivores
ccmPlot(ccm.fw1.2$HP)
mtext("Pt", side=4, line=0.5, cex=1.2, font=2, las=1)


plot(1, type="n", xlab="", ylab="", xaxt="n", yaxt="n"); text(1, 1, "Ft", font=2, cex=2)
plot(1, type="n", xlab="", ylab="", xaxt="n", yaxt="n"); text(1, 1, "Jt", font=2, cex=2)
plot(1, type="n", xlab="", ylab="", xaxt="n", yaxt="n"); text(1, 1, "Ht", font=2, cex=2)

dev.off()



# =======================
# = Plot Experiment 1.3 =
# =======================
# dev.new(width=7.5, height=7.5)
png(paste0(simFile, "FWsim.ccm.expt1.qE", format(exp1.qE[3], nsmall=2), ".png"), width=7.5, height=7.5, units="in", res=figRes)
par(mar=c(1,1.75,0.2,0.2), mgp=c(0.85,0.25,0), oma=c(1.75,0,2,2), tcl=-0.2, ps=11, family="Times", cex=1)
lay.dat <- c(1, 2, 3, 4, 11, 5, 6, 7, 0, 12, 8, 9, 0, 0, 13, 10)
lay.mat <- matrix(lay.dat, nrow=4)
layout(lay.mat)


# Adults 1.3
ccmPlot(ccm.fw1.3$AF)
mtext("At", side=3, line=0.5, cex=1.3, font=2)
ccmPlot(ccm.fw1.3$AJ)
ccmPlot(ccm.fw1.3$AH)
ccmPlot(ccm.fw1.3$AP)

# Fish 1.3
ccmPlot(ccm.fw1.3$FJ)
ccmPlot(ccm.fw1.3$FH)
ccmPlot(ccm.fw1.3$FP)

# Juveniles 
ccmPlot(ccm.fw1.3$JH)
ccmPlot(ccm.fw1.3$JP)

# Herbivores
ccmPlot(ccm.fw1.3$HP)
mtext("Pt", side=4, line=0.5, cex=1.3, font=2, las=1)


plot(1, type="n", xlab="", ylab="", xaxt="n", yaxt="n"); text(1, 1, "Ft", font=2, cex=2)
plot(1, type="n", xlab="", ylab="", xaxt="n", yaxt="n"); text(1, 1, "Jt", font=2, cex=2)
plot(1, type="n", xlab="", ylab="", xaxt="n", yaxt="n"); text(1, 1, "Ht", font=2, cex=2)

dev.off()



# =======================
# = Plot Experiment 1.4 =
# =======================
# dev.new(width=7.5, height=7.5)
png(paste0(simFile, "FWsim.ccm.expt1.qE", format(exp1.qE[4], nsmall=2), ".png"), width=6, height=6, units="in", res=figRes)
par(mar=c(1,1.75,0.2,0.2), mgp=c(0.85,0.25,0), oma=c(1.75,0,2,2), tcl=-0.2, ps=11, family="Times", cex=1)
lay.dat <- c(1, 2, 3, 7, 4, 5, 0, 8, 6)
lay.mat <- matrix(lay.dat, nrow=3)
layout(lay.mat)


# Fish 1.4
ccmPlot(ccm.fw1.4$FJ)
mtext("Ft", side=3, line=0.5, cex=1.4, font=2)
ccmPlot(ccm.fw1.4$FH)
ccmPlot(ccm.fw1.4$FP)

# Juveniles 
ccmPlot(ccm.fw1.4$JH)
ccmPlot(ccm.fw1.4$JP)

# Herbivores
ccmPlot(ccm.fw1.4$HP)
mtext("Pt", side=4, line=0.5, cex=1.4, font=2, las=1)


plot(1, type="n", xlab="", ylab="", xaxt="n", yaxt="n"); text(1, 1, "Jt", font=2, cex=2)
plot(1, type="n", xlab="", ylab="", xaxt="n", yaxt="n"); text(1, 1, "Ht", font=2, cex=2)

dev.off()





# =======================
# = Plot Experiment 1.5 =
# =======================
# dev.new(width=7.5, height=7.5)
png(paste0(simFile, "FWsim.ccm.expt1.qE", format(exp1.qE[5], nsmall=2), ".png"), width=6, height=6, units="in", res=figRes)
par(mar=c(1,1.75,0.2,0.2), mgp=c(0.85,0.25,0), oma=c(1.75,0,2,2), tcl=-0.2, ps=11, family="Times", cex=1)
lay.dat <- c(1, 2, 3, 7, 4, 5, 0, 8, 6)
lay.mat <- matrix(lay.dat, nrow=3)
layout(lay.mat)


# Fish 1.5
ccmPlot(ccm.fw1.5$FJ)
mtext("Ft", side=3, line=0.5, cex=1.4, font=2)
ccmPlot(ccm.fw1.5$FH)
ccmPlot(ccm.fw1.5$FP)

# Juveniles 
ccmPlot(ccm.fw1.5$JH)
ccmPlot(ccm.fw1.5$JP)

# Herbivores
ccmPlot(ccm.fw1.5$HP)
mtext("Pt", side=4, line=0.5, cex=1.4, font=2, las=1)


plot(1, type="n", xlab="", ylab="", xaxt="n", yaxt="n"); text(1, 1, "Jt", font=2, cex=2)
plot(1, type="n", xlab="", ylab="", xaxt="n", yaxt="n"); text(1, 1, "Ht", font=2, cex=2)

dev.off()






# =======================
# = Plot Experiment 1.6 =
# =======================
# dev.new(width=7.5, height=7.5)
png(paste0(simFile, "FWsim.ccm.expt1.qE", format(exp1.qE[6], nsmall=2), ".png"), width=6, height=6, units="in", res=figRes)
par(mar=c(1,1.75,0.2,0.2), mgp=c(0.85,0.25,0), oma=c(1.75,0,2,2), tcl=-0.2, ps=11, family="Times", cex=1)
lay.dat <- c(1, 2, 3, 7, 4, 5, 0, 8, 6)
lay.mat <- matrix(lay.dat, nrow=3)
layout(lay.mat)


# Fish 1.6
ccmPlot(ccm.fw1.6$FJ)
mtext("Ft", side=3, line=0.5, cex=1.4, font=2)
ccmPlot(ccm.fw1.6$FH)
ccmPlot(ccm.fw1.6$FP)

# Juveniles 
ccmPlot(ccm.fw1.6$JH)
ccmPlot(ccm.fw1.6$JP)

# Herbivores
ccmPlot(ccm.fw1.6$HP)
mtext("Pt", side=4, line=0.5, cex=1.4, font=2, las=1)


plot(1, type="n", xlab="", ylab="", xaxt="n", yaxt="n"); text(1, 1, "Jt", font=2, cex=2)
plot(1, type="n", xlab="", ylab="", xaxt="n", yaxt="n"); text(1, 1, "Ht", font=2, cex=2)

dev.off()


# =======================
# = Plot Experiment 1.7 =
# =======================
# dev.new(width=7.5, height=7.5)
png(paste0(simFile, "FWsim.ccm.expt1.qE", format(exp1.qE[7], nsmall=2), ".png"), width=6, height=6, units="in", res=figRes)
par(mar=c(1,1.75,0.2,0.2), mgp=c(0.85,0.25,0), oma=c(1.75,0,2,2), tcl=-0.2, ps=11, family="Times", cex=1)
lay.dat <- c(1, 2, 3, 7, 4, 5, 0, 8, 6)
lay.mat <- matrix(lay.dat, nrow=3)
layout(lay.mat)


# Fish 1.7
ccmPlot(ccm.fw1.7$FJ)
mtext("Ft", side=3, line=0.5, cex=1.4, font=2)
ccmPlot(ccm.fw1.7$FH)
ccmPlot(ccm.fw1.7$FP)

# Juveniles 1.7
ccmPlot(ccm.fw1.7$JH)
ccmPlot(ccm.fw1.7$JP)

# Herbivores 1.7
ccmPlot(ccm.fw1.7$HP)
mtext("Pt", side=4, line=0.5, cex=1.4, font=2, las=1)


plot(1, type="n", xlab="", ylab="", xaxt="n", yaxt="n"); text(1, 1, "Jt", font=2, cex=2)
plot(1, type="n", xlab="", ylab="", xaxt="n", yaxt="n"); text(1, 1, "Ht", font=2, cex=2)

dev.off()








# =======================
# = Plot Experiment 2 =
# =======================
# dev.new(width=7.5, height=7.5)
png(paste0(simFile, "FWsim.ccm.expt2.png"), width=7.5, height=7.5, units="in", res=figRes)
par(mar=c(1,1.75,0.2,0.2), mgp=c(0.85,0.25,0), oma=c(1.75,0,2,2), tcl=-0.2, ps=11, family="Times", cex=1)
lay.dat <- c(1, 2, 3, 4, 11, 5, 6, 7, 0, 12, 8, 9, 0, 0, 13, 10)
lay.mat <- matrix(lay.dat, nrow=4)
layout(lay.mat)


# Adults 2
ccmPlot(ccm.fw2$AF)
mtext("At", side=3, line=0.5, cex=1.2, font=2)
ccmPlot(ccm.fw2$AJ)
ccmPlot(ccm.fw2$AH)
ccmPlot(ccm.fw2$AP)

# Fish 2
ccmPlot(ccm.fw2$FJ)
ccmPlot(ccm.fw2$FH)
ccmPlot(ccm.fw2$FP)

# Juveniles 2
ccmPlot(ccm.fw2$JH)
ccmPlot(ccm.fw2$JP)

# Herbivores 2
ccmPlot(ccm.fw2$HP)
mtext("Pt", side=4, line=0.5, cex=1.2, font=2, las=1)


plot(1, type="n", xlab="", ylab="", xaxt="n", yaxt="n"); text(1, 1, "Ft", font=2, cex=2)
plot(1, type="n", xlab="", ylab="", xaxt="n", yaxt="n"); text(1, 1, "Jt", font=2, cex=2)
plot(1, type="n", xlab="", ylab="", xaxt="n", yaxt="n"); text(1, 1, "Ht", font=2, cex=2)

dev.off()





# =======================
# = Plot Experiment 3 =
# =======================
# dev.new(width=7.5, height=7.5)
png(paste0(simFile, "FWsim.ccm.expt3.png"), width=7.5, height=7.5, units="in", res=figRes)
par(mar=c(1,1.75,0.2,0.2), mgp=c(0.85,0.25,0), oma=c(1.75,0,2,2), tcl=-0.2, ps=11, family="Times", cex=1)
lay.dat <- c(1, 2, 3, 4, 11, 5, 6, 7, 0, 12, 8, 9, 0, 0, 13, 10)
lay.mat <- matrix(lay.dat, nrow=4)
layout(lay.mat)


# Adults 3
ccmPlot(ccm.fw3$AF)
mtext("At", side=3, line=0.5, cex=1.2, font=2)
ccmPlot(ccm.fw3$AJ)
ccmPlot(ccm.fw3$AH)
ccmPlot(ccm.fw3$AP)

# Fish 3
ccmPlot(ccm.fw3$FJ)
ccmPlot(ccm.fw3$FH)
ccmPlot(ccm.fw3$FP)

# Juveniles 3
ccmPlot(ccm.fw3$JH)
ccmPlot(ccm.fw3$JP)

# Herbivores 3
ccmPlot(ccm.fw3$HP)
mtext("Pt", side=4, line=0.5, cex=1.2, font=2, las=1)


plot(1, type="n", xlab="", ylab="", xaxt="n", yaxt="n"); text(1, 1, "Ft", font=2, cex=2)
plot(1, type="n", xlab="", ylab="", xaxt="n", yaxt="n"); text(1, 1, "Jt", font=2, cex=2)
plot(1, type="n", xlab="", ylab="", xaxt="n", yaxt="n"); text(1, 1, "Ht", font=2, cex=2)

dev.off()






# =======================
# = Plot Experiment 4 =
# =======================
# dev.new(width=7.5, height=7.5)
png(paste0(simFile, "FWsim.ccm.expt4.png"), width=7.5, height=7.5, units="in", res=figRes)
par(mar=c(1,1.75,0.2,0.2), mgp=c(0.85,0.25,0), oma=c(1.75,0,2,2), tcl=-0.2, ps=11, family="Times", cex=1)
lay.dat <- c(1, 2, 3, 4, 11, 5, 6, 7, 0, 12, 8, 9, 0, 0, 13, 10)
lay.mat <- matrix(lay.dat, nrow=4)
layout(lay.mat)


# Adults 4
ccmPlot(ccm.fw4$AF)
mtext("At", side=3, line=0.5, cex=1.2, font=2)
ccmPlot(ccm.fw4$AJ)
ccmPlot(ccm.fw4$AH)
ccmPlot(ccm.fw4$AP)

# Fish 4
ccmPlot(ccm.fw4$FJ)
ccmPlot(ccm.fw4$FH)
ccmPlot(ccm.fw4$FP)

# Juveniles 4
ccmPlot(ccm.fw4$JH)
ccmPlot(ccm.fw4$JP)

# Herbivores 4
ccmPlot(ccm.fw4$HP)
mtext("Pt", side=4, line=0.5, cex=1.2, font=2, las=1)


plot(1, type="n", xlab="", ylab="", xaxt="n", yaxt="n"); text(1, 1, "Ft", font=2, cex=2)
plot(1, type="n", xlab="", ylab="", xaxt="n", yaxt="n"); text(1, 1, "Jt", font=2, cex=2)
plot(1, type="n", xlab="", ylab="", xaxt="n", yaxt="n"); text(1, 1, "Ht", font=2, cex=2)

dev.off()


