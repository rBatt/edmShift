
# ===================================================================================
# = Luke: I haven't edited compute_stats, but I don't think this is called too much =
# ===================================================================================
compute_stats <- function(obs, preds)
{
  # computes performance metrics for how well predictions match observations
  # obs = vector of observations
  # pred = vector of prediction
  
  N = sum(is.finite(obs) & is.finite(preds))
  rho = cor(obs, preds, use = "pairwise.complete.obs")
  mae = mean(abs(obs-preds), na.rm = TRUE)
  rmse = sqrt(mean((obs-preds)^2, na.rm = TRUE))
  return(data.frame(N = N, rho = rho, mae = mae, rmse = rmse))
}
