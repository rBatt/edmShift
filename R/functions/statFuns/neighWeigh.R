# ==================================================================
# = Luke: I wrote this function, it's called by simplex_projection =
# ==================================================================
#Although I wrote this,it's basically just copied from their old code that was within a for() loop
neighWeigh <- function(dists, num_neighbors){
	min_d <- min(dists, na.rm=TRUE)
	if(min_d != 0){ # not perfect match
     	wei <- exp(-dists/min_d)
     	wei[wei < 0.000001] <- 0.000001
	}else{ # else, if it is a perfect match
     	wei <- rep.int(0.000001, times = num_neighbors)
     	wei[dists == 0] <- 1
	}
	return(wei/sum(wei, na.rm=TRUE))
}
