FW.tsPlot <- function(fw.array, colLabs, type=c("png","dev.new"), pngFile=NULL, figRes=150){
	
	type <- match.arg(type)
	
	if(type!="dev.new" & is.null(pngFile)){
		stop("type is 'png', but file name not specified in pngFile")
	}
	
	figW <- max(8.5*(length(colLabs)/7),3)
	if(type=="png"){
		png(filename=pngFile, width=figW, height=6, units="in", res=figRes)
	}else if(type=="dev.new"){
		dev.new(width=figW, height=6)
	}
	n.rows <- 5 + as.numeric(length(dim(fw.array))!=3)
	par(mfcol=c(n.rows, length(colLabs)), mar=c(1,1.35,0.15,0.15), oma=c(0.5,0.1,0.5,0.1), cex=1, ps=8, mgp=c(0.75,0.15,0), tcl=-0.125, family="Times")

	fw.labels <- c("Adult Pisc", "Planktivore", "Juv Pisc", "Herb Zoop", "Phyto")
	# fw1.lims <- apply(fw.array, 2, range)[,-1]

	for(i in 1:length(colLabs)){
		if(length(dim(fw.array))==3){
			tfw <- fw.array[,-1,i]
		}else{
			tfw <- fw.array
			fw.labels <- c("qE",fw.labels)
		}
		
	
		yo <- i==1
	
		for(j in 1:ncol(tfw)){
			if(yo){
				ylo <- fw.labels[j]
			}else{
				ylo <- ""
			}
		
			xo <- j==n.rows
			to <- j==1
		
			plot(tfw[,j], xaxt=c("n","s")[xo+1], ylab=ylo, xlab="", type="l")
			if(to){
				mtext(colLabs[i], side=3, line=0)
			}
			if(!xo){
				axis(1, labels=FALSE)
			}
		}

	
	}
	
	if(type!="dev.new"){
		dev.off()
	}
	
}