set.seed(101)

slope1 <- -.3; intercept1 <- 1.5    # generating data from C_1
View(slope1)
xs1 <- sample(seq(-2,2,len=201), 40)
View(xs1)
ys1 <- intercept1 + slope1*xs1 + rnorm(length(xs1),0,.15) # add some noise

slope2 <- 1.2; intercept2 <- -.4    # generating data from C_2
xs2 <- sample(seq(-2,2,len=201), 40)
ys2 <- intercept2 + slope2*xs2 + rnorm(length(xs1),0,.15)

mydata <- rbind( cbind(xs1,ys1), cbind(xs2,ys2) ) 
View(mydata)
plot(mydata, pch=19, xlab="X", ylab="Y")

#init step
i1 <- s1 <- i2 <- s2 <- 0 # model parameters for slope and intersect
init_params <- function() {
  i1 <<- 2*runif(1)
  s1 <<- 2*runif(1)
  i2 <<- 2*runif(1)
  s2 <<- 2*runif(1)
  c(i1,s1,i2,s2)
}

params <- init_params()

#E-Step
# params is [s1,i1,s2,i2]
e.step <- function(mydata, params, sigma=0.5) {
  w1 <- rep(NA, nrow(mydata))
  w2 <- rep(NA, nrow(mydata))
  
  for (i in 1:nrow(mydata)) {
    r1 <- abs(params[1] + params[2] * mydata[i,1] - mydata[i,2]) # residual for model 1
    r2 <- abs(params[3] + params[4] * mydata[i,1] - mydata[i,2]) # residual for model 2
    
    exp1 <- exp(-r1^2/sigma^2)
    exp2 <- exp(-r2^2/sigma^2)
    
    w1[i] <- exp1 / (exp1+exp2)
    w2[i] <- exp2 / (exp1+exp2)
  }
  
  cbind(w1,w2)  
}

ws <- e.step(mydata, params)
head(ws)

# wls - weighted least squares
wls <- function(X,Y,W) {
  solve(t(X) %*% W %*% X) %*% t(X) %*% W %*% Y
}

m.step <- function(mydata, ws) {
  X <- cbind(rep(1, nrow(mydata)), mydata[,1])
  Y <- as.matrix(mydata[,2 ncol=1)
  p_1 <- wls(X,Y,diag(ws[,1]))
  p_2 <- wls(X,Y,diag(ws[,2]))
  
  c(p_1, p_2)
}

params <- m.step(mydata, ws)



#putting E and M step together
em.2lines <- function(mydata, tol=1e-2, max.step=1e3) {
  step <- 0
  
  s1 <- i1 <- s2 <- i2 <- 0 # model parameters for slope and intersect
  params <- init_params()
  
  repeat {
    ws         <- e.step(mydata, params)
    old.params <- params
    params     <- m.step(mydata, ws)
    
    if (norm(as.matrix(old.params-params), type="F") < tol) # convergence achieved
      break
    
    step <- step +1
    if (step > max.step)
      break
  } 
  
  list(params=params,    # the estimated parameters
       weights=ws,       # the weighs for each datapoint x^i
       class=apply(ws, 1, function(v) if (v[1]>v[2]) 1 else 2))  # the class for each datapoint
}

report <- em.2lines(mydata)
report$params

