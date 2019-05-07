poissreg <- function(formula,data=.GlobalEnv) {
  X = model.matrix(formula,data = data)
  p = ncol(X)
  Yvar = all.vars(update.formula(formula, .~1))
  Y = get(Yvar, data)
  loglik = function(beta){sum(t(Y) %*% X %*% beta) - sum(exp(X %*% beta))
  }
  negloglik = function(beta) - loglik(beta)
  sol = optim (par=rep(0,p), fn = negloglik)$par
  names(sol)=colnames(X)
  return(sol)
}

