
# =================================================
# = Luke: simplex_projection is the slow function =
# =================================================
simplex_projection <- function(vectors, target, lib_indices, pred_indices, num_neighbors)
{
  # do simplex projection
  # vectors = reconstructed state-space (each row is a separate vector/state)
  # target = time series to be used as the target (should line up with vectors)
  # lib_indices = vector of T/F values (which states to include when searching for neighbors)
  # pred_indices = vector of T/F values (which states to predict from)
  # num_neighbors = number of neighbors to use for simplex projection
  
  # setup output
	preds <- rep.int(NaN, times = length(target))
	empty1 <- matrix(NA, nrow=sum(pred_indices), ncol=num_neighbors)
	empty2 <- empty1
	lp <- lib_indices[pred_indices]
	addBack <- min(which(lp))-1 # when getting the indices of the nearest neighbors, need to account for get.knnx() not seeing the full matrix, as it missing the indices removed by lp; i.e., the indices from get.knnx() are off my min(which(lp))-1 relative to the original (full length(target) x length(target)) matrix
  # make predictions
	#Should be possible to do this with dist()
	# ================================
	# = Slow step below? (distance2) =
	# ================================
	
	# =======
	# = OLD =
	# # =======
	# # distance2 <- as.matrix(dist(vectors[pred_indices,])) # each column contains the distances between all pairs of rows in vectors[lib_indices]
	# distance2 <- rdist(vectors[pred_indices,])
	# # distance2[upper.tri(distance2, diag=TRUE)] <- NA
	# diag(distance2) <- NA # the diagonals are always 0, because each row maps onto itself perfect (distance = 0), so replace w/ NA so it doesn't get ranked later
	# distance2[!lib_indices[pred_indices], ] <- NA # think of each column as the index for the prediction, and each row as a reference to the library. When finding the distances, we want to be able to select between all combinations of prediciton x library, which means have all the columns, but only some of the rows. Later, each column will have 4 non-zero values, representing the 4-closest time steps for the column that we want to predict; our search for the closest 4 is limited to within the library, i.e., a subset of the rows. (the library is a subset of prediction)
	# neighbors2 <- apply(distance2, 2, order)[1:num_neighbors,] # get the rank (order) for the distances, applied per column 
	# #(neighbors2 ultimately gives the 4 closest rows in vectors[lib_indices,] for each row in vectors[lib_indices,])
	# ===========
	# = END OLD =
	# ===========
	
	# =======
	# = NEW =
	# =======
	data2 <- vectors[lib_indices,]
	query2 <- vectors[pred_indices,]
	knn <- get.knnx(data2, query2, k=num_neighbors+1, algorithm="brute")
	
	# knn2 <- function(x, y)
	
# f0 <- function()	get.knnx(data2, query2, k=num_neighbors+1, algorithm="cover_tree")
# f1 <- function()	get.knnx(data2, query2, k=num_neighbors+1, algorithm="kd_tree")
# f2 <- function()	get.knnx(data2, query2, k=num_neighbors+1, algorithm="CR")
# f3 <- function()	get.knnx(data2, query2, k=num_neighbors+1, algorithm="brute")
# microbenchmark(f0(), f1(), f2(), f3())
	
	empty1[lp,] <- knn$nn.dist[lp, 2:(num_neighbors+1)]
	empty1[!lp,] <- knn$nn.dist[!lp, 1:num_neighbors]
	distance3 <- t(empty1)
	
	empty2[lp,] <- knn$nn.index[lp, 2:(num_neighbors+1)]
	empty2[!lp,] <- knn$nn.index[!lp, 1:num_neighbors] 
	neighbors2 <- t(empty2)
	# ===========
	# = END NEW =
	# ===========
	
	neighbors3 <- matrix(c(neighbors2+addBack, rep(1:ncol(neighbors2), each=num_neighbors)), ncol=2) # neighbors2 gives the indices, but not in a format convenient for subsetting (neighbors3 mimics the format of which(... arr.ind=TRUE))
	# distance3 <- matrix(distance2[neighbors3], nrow=num_neighbors) # given the indices, actually extract the distances (neighbors has the locations of the nearest neighbors, distances has the values of the nearest neighbors)
	weights2 <- apply(distance3, 2, neighWeigh, num_neighbors=num_neighbors)
	# total_weight2 <- colSums(weights2)
	
	weigh_mat <- matrix(rep.int(0, sum(pred_indices)^2), ncol=sum(pred_indices)) #function to contain the weights, which are computed from the distance matrix
	weigh_mat[neighbors3] <- weights2

	# ============================
	# = Slow step below? (preds) =
	# ============================
	preds[pred_indices] <- (target[pred_indices]%*%weigh_mat)[,]
	# weights2%*%target[lib_indices]
	
  # return output & stats
  return(list(preds = preds, stats = compute_stats(target[pred_indices], preds[pred_indices])))
}
