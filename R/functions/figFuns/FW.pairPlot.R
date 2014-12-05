
FW.pairPlot <- function(df){
	panel.cor <- function(x, y, digits = 2, prefix = "", cex.cor, ...){
	    usr <- par("usr"); on.exit(par(usr))
	    par(usr = c(0, 1, 0, 1))
		if(sd(x)==0|sd(y)==0){
			r <- NA
		}else{
		    r <- abs(cor(x, y))
		}
	    txt <- format(c(r, 0.123456789), digits = digits)[1]
	    txt <- paste0(prefix, txt)
	    if(missing(cex.cor)) cex.cor <- 0.8/strwidth(txt)
		r <- approx(x=0:1, y=c(0.75,1.5), xout=r, method="linear")$y
	    text(0.5, 0.5, txt, cex = cex.cor * r)
	}
	panel.hist <- function(x, ...){
	    usr <- par("usr"); on.exit(par(usr))
	    par(usr = c(usr[1:2], 0, 1.5) )
	    h <- hist(x, plot = FALSE)
	    breaks <- h$breaks; nB <- length(breaks)
	    y <- h$counts; y <- y/max(y)
	    rect(breaks[-nB], 0, breaks[-1], y, col="gray")
	}
	p.m <- bquote(bar(qE)==.(format(mean(df[,1]), digits=2))) #paste("qE",mean(df[,1]),sep="=")
	pairs(df, upper.panel=panel.cor, diag.panel=panel.hist, col=myCol(nrow(df)), cex=0.75, pch=20, main=p.m, oma=c(0.6,0.6,2,0.6))
	mtext(p.m, side=3, line=-0.5, outer=TRUE, font=2)
	
}

