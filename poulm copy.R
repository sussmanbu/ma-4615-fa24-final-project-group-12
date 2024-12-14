poulm = function(Xk, Y, mtype = 'ls', ridged = 0){
  library(glmnet)
  
  # SAMPLE SIZE AND PREDICTOR COUNT
  n = length(Y)
  p = ncol(Xk)
  if (is.null(p)){
    p = 1
  }
  
  # DESIGN MATRIX
  X = cbind(1, Xk)
  
  # DESIGN MATRIX (STANDARDIZED PREDICTORS)
  Zk = matrix(0, nrow(Xk), p)
  for (j in 1:p){
    if (min(Xk[, j], na.rm = TRUE) == 0 && max(Xk[, j], na.rm = TRUE) == 1){
      Zk[,j] = Xk[,j]
    }
    else{
      mean_j = mean(Xk[, j], na.rm = TRUE)
      sd_j = sd(Xk[, j], na.rm = TRUE)
      Zk[,j] = ((Xk[,j] - mean_j) / sd_j)
    }
  }
  Z = cbind(1, Zk)
  
  # BETAHAT AND ZETAHAT
  if (mtype == 'ls'){
    betahat = solve(t(X)%*%X)%*%t(X)%*%Y
    zetahat = solve(t(Z)%*%Z)%*%t(Z)%*%Y
  }
  else if (mtype == 'ridge'){
    betahat = solve(t(X)%*%X + ridged^2*diag(p+1))%*%t(X)%*%Y
    zetahat = solve(t(Z)%*%Z + ridged^2*diag(p+1))%*%t(Z)%*%Y
  }
  else if (mtype == 'lasso'){
    lamideal = cv.glmnet(Xk, Y, alpha=1)$lambda.min
    model = glmnet(Xk, Y, alpha = 1, lambda = lamideal)
    betahat = coef(model)
    lamideal_z = cv.glmnet(Zk, Y, alpha=1)$lambda.min
    model_z = glmnet(Zk, Y, alpha = 1, lambda = lamideal_z)
    zetahat = coef(model_z)
  }
  
  # MODEL METRICS
  yhat = X%*%betahat
  residuals = Y - yhat
  SSE = sum(residuals^2)
  MSE = SSE / (n-p-1)
  RMSE = sqrt(SSE / n)
  sigma2 = MSE
  SST = var(Y) * (n-1)
  MST = SST / (n-1)
  SSM = SST - SSE
  MSM = SSM / p
  Fstat = MSM / MSE
  p_value = pf(Fstat, p, n-p-1, lower.tail = FALSE)
  r2 = 1 - (SSE / SST)
  adj_r2 = 1 - (MSE / MST)

  # COVARIANCE MATRIX
  sigma_bhat = sigma2 * solve(t(X)%*%X) 
  
  # STANDARD ERROR OF BETAHAT
  se_bhat = sqrt(diag(sigma_bhat))
  
  # STANDARDIZED BETAHAT
  betahat_s = betahat / se_bhat
  
  # HAT MATRIX
  H = X %*% solve(t(X)%*%X) %*% t(X)
  
  # LEVERAGES
  lev = diag(H)
  
  # STANDARDIZED RESIDUALS
  std_resid = (residuals - 0) / (sqrt(MSE) * sqrt(1 - lev))
  
  # CORRELATION MATRIX
  cor_matrix = cor(cbind(Xk, Y))
  
  # DETERMINANT OF DESIGN MATRIX
  design_det = sqrt(abs(det(t(X)%*%X)))
  
  # VARIANCE INFLATION FACTORS
  myvif <- c()
  if(p == 1){
    myvif <- append(myvif, 1)
  }
  else{
    for(k in 1:p){
      Xp <- X[,-c(k+1)]
      Yp <- X[, k+1]
      betahatp = solve(t(Xp)%*%Xp)%*%t(Xp)%*%Yp
      Yp_hat = Xp%*%betahatp
      resids = Yp - Yp_hat
      SSEp = sum(resids^2)
      SSTp = var(Yp) * (length(Yp)-1)
      r2p = 1 - (SSEp/SSTp)
      vk = 1/(1-r2p)
      myvif <- append(myvif, vk)
    }
  }
  
  # MULTI-COLLINEARITY INDICES
  mymci = sqrt(myvif)
  
  # ADDED-VARIABLE PLOT SLOPE COEFFICIENTS
  slopes = c()
  for(k in 1:p){
    Xk_c <- X[,-c(k+1)]
    Xk <- X[,c(k+1)]
    Xk_betahats = solve(t(Xk_c)%*%Xk_c)%*%t(Xk_c)%*%Xk
    Xk_hat = Xk_c%*%Xk_betahats
    Xk_eps = Xk - Xk_hat
    SSEk = sum(Xk_eps^2)
    MSEk = SSEk / (n - p - 2)
    Hk = Xk_c%*%solve(t(Xk_c)%*%Xk_c)%*%t(Xk_c)
    leveragesk = diag(Hk)
    std_resid_Xk = Xk_eps / (sqrt(MSEk) * sqrt(1 - leveragesk))
    
    Yk_betahats = solve(t(Xk_c)%*%Xk_c)%*%t(Xk_c)%*%Y
    Yk_hat = Xk_c%*%Yk_betahats
    Yk_eps = Y - Yk_hat
    SSE_Yk = sum(Yk_eps^2)
    MSE_Yk = SSE_Yk / (n - p - 2)
    std_resid_Yk = Yk_eps / (sqrt(MSE_Yk) * sqrt(1 - leveragesk))
    
    betahat_k = solve(t(std_resid_Xk)%*%std_resid_Xk)%*%t(std_resid_Xk)%*%std_resid_Yk
    slopes = c(slopes, betahat_k)
  }
  
  # PARTIAL CORRELATION COEFFICIENTS
  myvif <- c()
  if(p == 1){
    myvif <- append(myvif, 1)
  }
  else{
    for(k in 1:p){
      Xp <- X[,-c(k+1)]
      Yp <- X[, k+1]
      betahatp = solve(t(Xp)%*%Xp)%*%t(Xp)%*%Yp
      Yp_hat = Xp%*%betahatp
      resids = Yp - Yp_hat
      SSEp = sum(resids^2)
      SSTp = var(Yp) * (length(Yp)-1)
      r2p = 1 - (SSEp/SSTp)
      vk = 1/(1-r2p)
      myvif <- append(myvif, vk)
    }
  }
  
  results = list('n'              = n,
                 'num_predictors' = p,
                 'betahat'        = betahat,
                 'zetahat'        = zetahat,
                 'SE_betahat'     = se_bhat,
                 'std_betahat'    = betahat_s,
                 'yhat'           = yhat,
                 'SSE'            = SSE,
                 'MSE'            = MSE,
                 'RMSE'           = RMSE,
                 'SSM'            = SSM,
                 'SST'            = SST,
                 'Fstat'          = Fstat,
                 'p_value'        = p_value,
                 'R2'             = r2,
                 'R2_adj'         = adj_r2,
                 'H_matrix'       = H,
                 'leverages'      = lev,
                 'residuals'      = residuals,
                 'std_residuals'  = std_resid,
                 'R'              = cor_matrix,
                 'det_X'          = design_det,
                 'VIFs'           = myvif,
                 'MCIs'           = mymci,
                 'AD_slopes'      = slopes)
  
  return(results)
}
