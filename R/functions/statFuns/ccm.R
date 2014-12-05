
ccm <- function(data, var1N, var2N, pred, E=3, nStart=20, libSize=NULL){
	
	var1 <- data[,var1N]
	var2 <- data[,var2N]


	ed <- (E*2)+1
	if(is.null(libSize)){
		pickL <- c(ed, diff(pred))
	}else{
		pickL <- libSize
	}

	Ls <- pickL[1]:pickL[2]
	var1_xmap_var2 <- data.frame()
	var2_xmap_var1 <- data.frame()


	for(lib_size in pickL[1]:pickL[2])
	{
		pO <- pred[1]:(pred[2]-lib_size)
		if(length(pO)>1){
			start_lib_20 <- sample(pO, min(nStart, length(pO)))
		}else{
			start_lib_20 <- pO
		}
	
		for(start_lib in (start_lib_20))
	  {
	     lib <- c(start_lib, start_lib+lib_size-1)
	     results <- cross_mapping(var1, var2, lib, pred, E, tau = 1, b = E+1)
	     var1_xmap_var2 <- rbind(var1_xmap_var2, data.frame(L = lib_size, rho = results$stats$rho))

	     results <- cross_mapping(var2, var1, lib, pred, E, tau = 1, b = E+1)
	     var2_xmap_var1 <- rbind(var2_xmap_var1, data.frame(L = lib_size, rho = results$stats$rho))     
	  }
	}


	# compute mean rhos at each L
	var1_xmap_var2$L <- as.factor(var1_xmap_var2$L)
	var2_xmap_var1$L <- as.factor(var2_xmap_var1$L)
	v1xv2 <- pmax(0,do.call(rbind, lapply(split(var1_xmap_var2, var1_xmap_var2$L), function(x){median(x$rho)})))
	v2xv1 <- pmax(0,do.call(rbind, lapply(split(var2_xmap_var1, var2_xmap_var1$L), function(x){median(x$rho)})))
	
	se.fun <- function(x){
		sd(x$rho)/sqrt(length(x$rho))
	}
	
	# compute upper and lower intervals around median (s.e.'s are the bounds)
	# for 1 crossmap 2
	v1xv2.se <- do.call(rbind, lapply(split(var1_xmap_var2, var1_xmap_var2$L), se.fun))
	v1xv2.up <- v1xv2 + v1xv2.se 
	v1xv2.low <- v1xv2 - v1xv2.se 
	
	# for 2 crossmap 1
	v2xv1.se <- do.call(rbind, lapply(split(var1_xmap_var2, var1_xmap_var2$L), se.fun))
	v2xv1.up <- v2xv1 + v2xv1.se 
	v2xv1.low <- v2xv1 - v2xv1.se 
	
	# compute the slopes correlation vs. library size
	# slopes for 1 crossmap 2
	slope_1x2_coeff <- summary(lm(v1xv2~log(Ls)))$coeff
	slope_1x2 <- round(slope_1x2_coeff[2],2)
	slope_1x2_se <- round(slope_1x2_coeff[4],2)
	
	# slopes for 2 crossmap 1
	slope_2x1_coeff <- summary(lm(v2xv1~log(Ls)))$coeff
	slope_2x1 <- round(slope_2x1_coeff[2],2)
	slope_2x1_se <- round(slope_2x1_coeff[4],2)

	
	# return the effect of 1 on 2, then 2 on 1 (effect of 1 on 2 is from 2xmap1)	
	# return(c(slope_2x1, slope_1x2))
	outMat <- matrix(
		c(
			v1xv2,
			v2xv1,
			v1xv2.up,
			v1xv2.low,
			v2xv1.up,
			v2xv1.low
		),
		ncol=6,
		dimnames=list(NULL, c("v1xv2","v2xv1","v1xv2.up","v1xv2.low","v2xv1.up","v2xv1.low"))
	)
	
	attr(outMat, "Ls") <- Ls
	attr(outMat, "var1N") <- var1N
	attr(outMat, "var2N") <- var2N
	attr(outMat, "slope_2x1") <- slope_2x1
	attr(outMat, "slope_1x2") <- slope_1x2
	
}
