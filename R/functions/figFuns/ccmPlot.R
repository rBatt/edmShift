

# produce plot (Figure 5B from Sugihara et al. 2012)
ccmPlot <- function(x){
	
	# Get correlations out of matrix
	v1xv2 <-x[,"v1xv2"]
	v2xv1 <- x[,"v2xv1"]
	
	# Get other values from x matrix attributes
	var1N <- attr(x, "var1N")
	var2N <- attr(x, "var2N")
	Ls <- attr(x, "Ls")
	slope_1x2 <- attr(x, "slope_1x2")
	slope_2x1 <- attr(x, "slope_2x1")
	slope_1x2_se <- attr(x, "slope_1x2_se")
	slope_2x1_se <- attr(x, "slope_2x1_se")
	
	# Set up expressions for legend
	l1x2 <- bquote(.(var1N)%<=%hat(.(var2N))~~(slope~'='~.(slope_1x2)%+-%.(slope_1x2_se)))
	l2x1 <- bquote(.(var2N)%<=%hat(.(var1N))~~(slope~'='~.(slope_2x1)%+-%.(slope_2x1_se)))
	
	# Y limits for plot
	ylim <- range(c(v1xv2,v2xv1)*c(1,1.4), na.rm=TRUE)
	
	# Colors for lines in plot
	c1 <- "slateblue1"
	c2 <- "forestgreen"

	# Begin plotting
	plot(Ls, v1xv2, type="l", col=c1, lwd=2, xlim=range(Ls), ylim=ylim, xlab="Library size", ylab=expression(rho))
	lines(Ls, v2xv1, col=c2, lwd=2)
	
	# Add plot legend
	e1 <- as.expression(l1x2)
	e2 <- as.expression(l2x1)
	legend("topleft", legend=c(e1, e2) , lty=1, lwd=2, col=c(c1, c2), y.intersp=0.85, cex=0.9)
	
}