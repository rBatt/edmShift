
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



# ======================
# = Plot Experiment #1 =
# ======================
# Plot Experiment 1 Time Series
qe.labels <- paste("qE", exp1.qE, sep="=")
# FW.tsPlot(fw.array=fw.exp1, colLabs=qe.labels)
FW.tsPlot(fw.array=fw.exp1, colLabs=qe.labels, pngFile=paste0(simFile, "FWsim.ts.expt1.png"), figRes=figRes)

# Plot Experiment 1 State Space
for(i in 1:length(exp1.qE)){
	# dev.new(width=5, height=5)
	png(paste0(simFile, "FWsim.pairs.expt1.qE", format(exp1.qE[i], nsmall=2), ".png"), width=5, height=5, units="in", res=figRes)
	par(mar=c(1,1,0.1,0.1), oma=c(0.1,0.1,0.5,0.1), ps=10, cex=1, family="Times", mgp=c(0.75,0.15,0), tcl=-0.1)
	FW.pairPlot(fw.exp1[,,i])
	dev.off()
	# plot(as.data.frame(fw.exp1[,-1,i]), col=myCol(exp1.steps), cex=0.75, pch=20)
}



# ======================
# = Plot Experiment #2 =
# ======================
# Experiment 2
# Plot Experiment 2 Time Series
qe.labels.2 <- paste("qE between", min(exp2.qE), "&", max(exp2.qE))
# FW.tsPlot(fw.array=fw.exp2, colLabs=qe.labels.2)
FW.tsPlot(fw.array=fw.exp2, colLabs=qe.labels.2, pngFile=paste0(simFile, "FWsim.ts.expt2.png"))

# Plot Experiment 2 State Space
# dev.new(width=5, height=5)
png(paste0(simFile, "FWsim.pairs.expt2.png"), width=5, height=5, res=figRes, units="in")
par(mar=c(1,1,0.1,0.1), oma=c(0.1,0.1,0.5,0.1), ps=10, cex=1, family="Times", mgp=c(0.75,0.15,0), tcl=-0.1)
FW.pairPlot(fw.exp2)
dev.off()



# ======================
# = Plot Experiment #3 =
# ======================
# Experiment 3
# Plot Experiment 3 Time Series
qe.labels.3 <- paste("qE between", min(exp3.qE), "&", max(exp3.qE))
FW.tsPlot(fw.array=fw.exp3, colLabs=qe.labels.3, pngFile=paste0(simFile, "FWsim.ts.expt3.png")) 

# Plot Experiment 3 State Space
# dev.new(width=5, height=5)
png(paste0(simFile, "FWsim.pairs.expt3.png"), width=5, height=5, res=figRes, units="in")
par(mar=c(1,1,0.1,0.1), oma=c(0.1,0.1,0.5,0.1), ps=10, cex=1, family="Times", mgp=c(0.75,0.15,0), tcl=-0.1)
FW.pairPlot(fw.exp3)
dev.off()


# =====================
# = Plot Experiment 4 =
# =====================
# Plot Experiment 4 Time Series
qe.labels.4 <- paste("qE between", min(exp4.qE), "&", max(exp4.qE))
FW.tsPlot(fw.array=fw.exp4, colLabs=qe.labels.4, pngFile=paste0(simFile, "FWsim.ts.expt4.png"))

# Plot Experiment 4 State Space
# dev.new(width=5, height=5)
png(paste0(simFile, "FWsim.pairs.expt4.png"), width=5, height=5, res=figRes, units="in")
par(mar=c(1,1,0.1,0.1), oma=c(0.1,0.1,0.5,0.1), ps=10, cex=1, family="Times", mgp=c(0.75,0.15,0), tcl=-0.1)
FW.pairPlot(fw.exp4)
dev.off()
 



