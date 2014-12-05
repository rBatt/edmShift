cross_mapping <- function(data, target0, lib, pred, E, tau = 1, b = E+1)
{
  # do cross mapping using simplex projection
  # data = time series
  # lib = matrix (n x 2) using n sequences of data for library
  # pred = matrix (n x 2) using n sequences of data to predict from
  # E = number of dimensions for the attractor reconstruction
  # tau = time lag for the lagged-vector construction
  # b = number of nearest neighbors to use for prediction

	# ===========================
	# = Ryan Replacement: Start =
	# ===========================
	#This entire function can be replaced with the follow lines, and run ~38% faster:
	# NOTE: introduces mismatch between lib_indices and pred_indices; subtracing lFix from lib_indices fixes this; then need to truncate first lFix of target
	# NOTE (cont'd): this can be fixed in later functions, or by passing a different "target" to simplex_projection()
	n <- NROW(data)
	lib <- matrix(lib, ncol = 2)
	pred <- matrix(pred, ncol = 2)
	lFix <- (E-1)*tau
	vectors <- embed(data, E)
	target <- target0[-(1:lFix)]
	n1 <- 1:n
	lib_indices <- ((n1)>=(lib[1,1]+lFix) & (n1)<=(lib[1,2]))[-(1:lFix)]
	pred_indices <- ((n1)>=(pred[1,1]+lFix) & (n1)<=(pred[1,2]))[-(1:lFix)]
	# =========================
	# = Ryan Replacement: End =
	# =========================
  
	# ==============
	# = Ryan Summary: =
	# ==============
	# This function simply creates 2 indices and 1 matrix. 
	# The indices are basically (1:n)%in%pred[1]:pred[2], but accounts for the lags.
	# The matrix contains the lagged versions of X.
  return(simplex_projection(vectors, target, lib_indices, pred_indices, b))
}