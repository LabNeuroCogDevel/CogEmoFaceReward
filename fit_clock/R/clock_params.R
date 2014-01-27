setwd("~/CogEmoFaceReward/fit_behavior/r_port")
#Object-oriented R implementation of time clock algorithm
source("TC_ClockData_RefClasses.R")
source("clock_model.R")


#generic parameter class
#should expose:
#  - min_value:  minimum parameter value in optimization
#  - max_value:  maximum parameter value
#  - init_value: initial parameter value
#  - cur_value:  current parameter value
#
#  - getRTUpdate: compute contribution to RT prediction for this parameter

#general sanity checks and field assignment handled here so subordinate classes need not duplicate
initialize_par <- function(obj, min_value=NULL, max_value=NULL, init_value=NULL, cur_value=NULL, par_scale=NULL, by=NULL) {
  stopifnot(cur_value <= max_value)
  stopifnot(cur_value >= min_value)
  stopifnot(init_value <= max_value)
  stopifnot(init_value >= min_value)
  
  obj$min_value <- min_value
  obj$max_value <- max_value
  obj$init_value <- init_value
  obj$cur_value <- cur_value
  obj$par_scale <- par_scale
  #name each parameter value so that when flattened to theta vector, all elements can be identified
  names(obj$min_value) <- names(obj$max_value) <- names(obj$init_value) <- names(obj$cur_value) <- names(obj$par_scale) <- obj$name  
  obj$by <- by
  obj$w <- emptyenv() #workspace should never be unique to parameter, but is set upstream by clock_model
  
  return(obj)
}


##some notes on an s3 implementation
##  can use shared w to update multiple values without having to copy objects etc.
##  but functions can have few, if any, side effects for updating object fields... would need a separate assignment outside of the function.


###PARAMETER CONSTRUCTORS
#meanRT constructor
meanRT <- function(min_value=100, max_value=5000, init_value=1000, cur_value=init_value, par_scale=1e3, by=NULL) {
  
  obj <- structure(
      list(
          name = "K"
      ), class=c("p_meanRT", "param"))
  
  obj <- initialize_par(obj, min_value, max_value, init_value, cur_value, par_scale, by) #check and initialize fields
  
  ##by is a character vector specifying which fields of a clockRun object should be used for setting multiple named cur_value fields just prior to fit.
  ##so, $fit (and predict?) should check the unique values for all by fields, then do an expand.grid, then name cur_values things like K.run_condition:IEV, etc.
  
  return(invisible(obj))
}

#autocorrelation with previous reaction time
autocorrPrevRT <- function(min_value=0.0, max_value=1.0, init_value=0.3, cur_value=init_value, par_scale=1e-1, by=NULL) {  
  obj <- structure(
      list(name = "lambda"),
      class=c("p_autocorrPrevRT", "param")
  )
  
  if (min_value < 0.0) { stop("Autocorr prev RT (lambda) parameter cannot be negative") }
  if (max_value > 1.0) { stop("Autocorr prev RT (lambda) parameter cannot exceed 1.0") }
  
  obj <- initialize_par(obj, min_value, max_value, init_value, cur_value, par_scale, by) #check and initialize fields
  return(invisible(obj))
}

#go: speedup of RT for PPE
go <- function(min_value=0.01, max_value=5.0, init_value=0.2, cur_value=init_value, par_scale=1e-1) {
  obj <- structure(
      list(name = "alphaG"),
      class=c("p_go", "param")
  )
  
  if (min_value < 0.01) { stop("alphaG min_value must be at least 0.01") }
  if (max_value > 5.0) { stop("alphaG max_value must be less than 5.0") }
  
  obj <- initialize_par(obj, min_value, max_value, init_value, cur_value, par_scale) #check and initialize fields
  return(invisible(obj))
}

##NoGo: slow down of RT for NPE
noGo <- function(min_value=0.01, max_value=5.0, init_value=0.2, cur_value=init_value, par_scale=1e-1, by=NULL) {
  obj <- structure(
      list(name = "alphaN"),
      class=c("p_nogo", "param")
  )
  
  if (min_value < 0.01) { stop("alphaN min_value must be at least 0.01") }
  if (max_value > 5.0) { stop("alphaN max_value must be less than 5.0") }
  
  obj <- initialize_par(obj, min_value, max_value, init_value, cur_value, par_scale, by) #check and initialize fields
  return(invisible(obj))
}

##Go for the gold: adapt RT toward the best
goForGold <- function(min_value=0.0, max_value=100.0, init_value=0.1, cur_value=init_value, par_scale=1e-1, by=NULL, bestRT_t1=numeric(0)) {
  obj <- structure(
      list(
          name = "scale",
          bestRT_t1 = bestRT_t1 #initial value for best reaction time on trial 1
      ), class=c("p_gold", "param"))
  
  if (min_value < 0) { stop("Go for gold (nu) parameter cannot be negative") }
  
  obj <- initialize_par(obj, min_value, max_value, init_value, cur_value, par_scale, by) #check and initialize fields
  return(invisible(obj))
}

##rho: Adapt toward fast responses if these have been better thus far
meanSlowFast <- function(min_value=0, max_value=10000, init_value=300, cur_value=init_value, par_scale=1e2, by=NULL) {
  obj <- structure(
      list(name = "rho"), 
      class=c("p_meanSlowFast", "param")
  )
  
  if (min_value < 0) { stop("Slow versus fast mean parameter (rho) cannot be negative") }
  
  obj <- initialize_par(obj, min_value, max_value, init_value, cur_value, par_scale, by) #check and initialize fields
  return(invisible(obj))
}

#epsilon: adapt to fast/slow responses in proportion to uncertainty about other dist
exploreBeta <- function(min_value=0, max_value=100000, init_value=2000, cur_value=init_value, par_scale=1e3, by=NULL) {
  obj <- structure(
      list(name = "epsilonBeta"), 
      class=c("p_epsilonBeta", "param")
  )
  
  obj <- initialize_par(obj, min_value, max_value, init_value, cur_value, par_scale, by) #check and initialize fields
  return(invisible(obj))
}

#beta distribution worker class (handled as environment shared by epsilon and rho)
betaFastSlow <- function(
    w=NULL, #shared workspace
    alpha_slow=1.01, #alpha shape parameter for slow (above average) RTs 
    beta_slow=1.01, #beta shape parameter for slow RTs
    alpha_fast=1.01, 
    beta_fast=1.01, 
    decay=1.0, 
    lastUpdateTrial=1L, 
    local_RT_learning_rate=0.1) #TODO: Does the learning rate for RT need to be yoked to alphaV for the critic update, as in MF's code? 
{
  stopifnot(decay <= 1.0)
  if (is.null(w) || !is.environment(w)) { stop ("Shared workspace w must be passed at initialization into betaFastSlow") }
  
  fparams <- as.list(environment()) #copy initial function parameters to list object
  #new tack: use environment for beta dists
  
  obj <- new.env(parent=baseenv()) #create empty new environment with base as parent to allow for curly evaluation
  for(n in ls(fparams, all.names=TRUE)) assign(n, get(n, fparams), obj) #copy all objects to new environment
  
  eval(
      quote(
          {
            mean_fast=numeric(0)
            mean_slow=numeric(0)
            mode_fast=numeric(0)
            mode_slow=numeric(0)
            var_fast=numeric(0)
            var_slow=numeric(0)
            mean_fast_last=numeric(0)
            mean_slow_last=numeric(0)
            var_fast_last=numeric(0)
            var_slow_last=numeric(0)
            explore=numeric(0)
            explore_last=numeric(0)
            local_RT=numeric(0)
            local_RT_last=numeric(0)
          }),
      obj)
  
  #class(obj) <- "betaFastSlow" #keep as list for use of within in updateBetaDist
  
  obj$local_RT[1L] <- obj$local_RT_last[1L] <- w$avg_RT #set initial local average to the block mean RT
  
  return(invisible(obj))
}

###
#getRTUpdate functions
getRTUpdate <- function(obj, theta) { UseMethod("getRTUpdate") } #general dispatch

getRTUpdate.default <- function(obj, theta, condition=NULL) {
  return (NULL)
} #shouldn't have an empty rt update

getTheta <- function(obj, condition) {
  if (is.null(obj$by)) {
    obj$name[1L]
  } else {
    obj$name[which(obj$name == paste0(obj$base_name, "/",  paste(obj$by, condition[obj$by], sep=":", collapse="/")))]
  }
}

#K: mean RT
getRTUpdate.p_meanRT <- function(obj, theta, condition=NULL) {
  #theta[obj$name] #for baseline RT, the parameter itself is the speed in ms
  theta[obj$theta_lookup] #for baseline RT, the parameter itself is the speed in ms
}

#lambda: corr with prior RT
getRTUpdate.p_autocorrPrevRT <- function(obj, theta, condition=NULL) {
  #theta[obj$name]*obj$w$RT_last
  theta[obj$theta_lookup]*obj$w$RT_last
}

#nu: go for the gold 
getRTUpdate.p_gold <- function(obj, theta, condition=NULL) {
  #compute maximum reward and reward variability up to current trial
  eval(
      quote({ 
            rew_max <- max(Reward[1:lastTrial]) # max reward received in block thus far -- used for updating best RT
            rew_sd  <- if(lastTrial > 1) { stats::sd(Reward[1:lastTrial]) } else { 0 } # sd of rewards in block thus far (use a value of 0 if just one trial).
            # If PPE on prior trial and obtained reward falls within one SD of max, save as bestRT
            #N.B. This works magically well in the test case
            #was trying to see whether the PPE aspect here is necessary
            #if (Rew_last >= (rew_max - rew_sd)) {
            if (Rew_last > V_last && Rew_last >= (rew_max - rew_sd)) {
              bestRT[cur_trial] <- RT_last
            } else {
              bestRT[cur_trial] <- bestRT[lastTrial] #carry forward best so far
            }
          }),
      obj$w
  )
  
  theta[obj$theta_lookup]*with(obj$w, bestRT[cur_trial] - avg_RT) #rtContrib
}

#Go: speed up for PPE
getRTUpdate.p_go <- function(obj, theta, condition=NULL) {
  #a bit of a hack here to copy the alphaG learning rate into w for easier code below
  obj$w$cur_value <- theta[obj$theta_lookup]
  
  eval(
      quote({
            Go_last   <- Go[lastTrial]
            
            #carry forward Go term unless updated below by PPE
            Go_new <- Go_last
            
            #if obtained reward was better than expected (PPE), speed up (scaled by alphaG)
            if (Rew_last > V_last) {                  
              Go_new <- Go_last + cur_value*(Rew_last - V_last)
            }
            
            Go[cur_trial] <- Go_new              
          }),
      obj$w
  )
  #N.B. The call below drastically slows down optimization
  #rm(cur_value, envir=obj$w)
  
  -1.0*obj$w$Go_new #rtContrib
}

#NoGo: slow down for NPE
getRTUpdate.p_nogo <- function(obj, theta, condition=NULL) {
  #a bit of a hack here to copy the alphaN learning rate into w for easier code below
  obj$w$cur_value <- theta[obj$theta_lookup]
  eval(
      quote({
            NoGo_last   <- NoGo[lastTrial]
            
            #carry forward NoGo term unless updated below by NPE
            NoGo_new <- NoGo_last
            
            #if obtained reward was worse than expected (NPE), slow down (scaled by alphaN)
            if (Rew_last <= V_last) {                  
              NoGo_new <- NoGo_last + cur_value*(V_last - Rew_last)
            }
            
            NoGo[cur_trial] <- NoGo_new                
          }),
      obj$w
  )
  #N.B. The call below drastically slows down optimization.
  #rm(cur_value, envir=obj$w)
  
  +1.0*obj$w$NoGo_new #rtContrib
}

#beta distribution tracking
updateBetaDists=function(bfs) {
  #because explore and meandiff parameters may both be present in the model
  #need to check whether the beta distribution has already been updated on this trial
  #if so, do not update again  
  if (bfs$lastUpdateTrial == bfs$w$cur_trial) { return(invisible(NULL)) }
  
  bfs$lastUpdateTrial <- bfs$w$cur_trial
#  cat("cur_trial is: ", w$cur_trial, "\n")
#  cat("betaFastSlow_lastUpdateTrial is: ", w$betaFastSlow$lastUpdateTrial, "\n")
  
  #model tracks two distributions, one for fast responses (less than mean RT)
  #and one for slow responses (above mean RT)
  #here, we update the estimates of the corresponding beta distribution for slow or fast responses
  #cache means and variances of prior trial
  
  eval(
      quote({
            
            mean_fast_last <- mean_fast
            mean_slow_last <- mean_slow
            var_fast_last  <- var_fast
            var_slow_last  <- var_slow
            local_RT_last  <- local_RT
            
            if (w$RT_last > local_RT_last) { #last response was slower than local average
              if (w$Rew_last > w$V_last) { #ppe: increment alpha shape parameter for slow dist
                alpha_slow <- alpha_slow + 1
              } else {
                beta_slow <- beta_slow + 1
              }
            } else if (w$RT_last <= local_RT_last) { #last response was faster than average
              if(w$Rew_last > w$V_last) {
                alpha_fast <- alpha_fast + 1
              } else {
                beta_fast <- beta_fast + 1
              }
            }
            
            if (decay < 1.0) {
              alpha_slow     <- decay * alpha_slow # if decay < 1 then this decays counts, making beta dists less confident
              beta_slow      <- decay * beta_slow
              alpha_fast     <- decay * alpha_fast
              beta_fast      <- decay * beta_fast
            }
            
            # compute mode and variances of beta distribution
            var_fast    <- alpha_fast * beta_fast / ( (alpha_fast + beta_fast)^2 * (alpha_fast + beta_fast + 1) )
            var_slow    <- alpha_slow*beta_slow/( (alpha_slow + beta_slow)^2 * (alpha_slow + beta_slow + 1) )
            #mode_slow  <- (alpha_slow - 1) / (alpha_slow + beta_slow - 2) #not used at present, omit for optimization speed
            #mode_fast  <- (alpha_fast - 1) / (alpha_fast + beta_fast - 2) 
            mean_slow   <- alpha_slow / (alpha_slow + beta_slow)
            mean_fast   <- alpha_fast / (alpha_fast + beta_fast)
            
            local_RT <- local_RT_last + local_RT_learning_rate * (w$RT_last - local_RT_last) # update estimate of recent RTs by 10% (0.1) of deviation of this trial's RT from the local average      
          }),
      bfs)
  
}


getRTUpdate.p_meanSlowFast=function(obj, theta, condition=NULL) {
  updateBetaDists(obj$w$betaFastSlow) #update fast/slow beta dists
  theta[obj$theta_lookup] * with(obj$w$betaFastSlow, mean_slow - mean_fast) #rtContrib
}

getRTUpdate.p_epsilonBeta=function(obj, theta, condition=NULL) {
  #model tracks two distributions, one for fast responses (less than mean RT)
  #and one for slow responses (above mean RT)
  #here, we update the estimates of the corresponding beta distribution for slow or fast responses
  
  obj$w$explore_last <- obj$w$explore
  updateBetaDists(obj$w$betaFastSlow) #update fast/slow beta dists
  
  obj$w$cur_value <- theta[obj$theta_lookup]
  eval(
      quote({                
            if (RT_last > betaFastSlow$local_RT_last) {
              #explore parameter scales the difference in SDs between the fast and slow beta dists
              explore <- -1.0*cur_value * (sqrt(betaFastSlow$var_fast) - sqrt(betaFastSlow$var_slow))  # speed up if more uncertain about fast responses
            } else {
              explore <- +1.0*cur_value * (sqrt(betaFastSlow$var_slow) - sqrt(betaFastSlow$var_fast)) #slow down if more uncertain about slow responses
            }
            
            # reset if already explored in this direction last trial (see supplement of Frank et al 09)
            if ( (RT_last < RT_last2 && explore < 0) ||
                (RT_last > RT_last2 && explore > 0) ) {
              explore <- 0
            }
          }),
      obj$w
  )
  
  obj$w$explore #rtContrib  
}

##RESET WORKSPACE FUNCTIONS
##TODO: Does preallocating the pred_contrib vectors buy any time?
# does not seem to slow down optimization at all to pre-initialize, although doesn't seem needed during
# optimization per se, since pred_contrib is only really useful at optimized parameter values.

reset_workspace <- function(obj) { UseMethod("reset_workspace") }
reset_workspace.param <- function(obj) { lapply(obj$name, function(f) { obj$w$pred_contrib[[f]] <- rep(NA_real_, obj$w$ntrials) } ) }
#reset_workspace.param <- function(obj) { NULL }

reset_workspace.p_go <- function(obj) {
  obj$w$Go <- rep(NA_real_, obj$w$ntrials) #vector of Go term
  obj$w$Go[1L] <- 0.0 #may want to override this later...
  NextMethod()
}

reset_workspace.p_nogo <- function(obj) {
  obj$w$NoGo <- rep(NA_real_, obj$w$ntrials) #vector of NoGo term
  obj$w$NoGo[1L] <- 0.0 #may want to override this later...
  NextMethod()
}

reset_workspace.p_gold <- function(obj) {
  #After shared workspace (w) address is passed in, setup expectation on bestRT for first trial
  obj$w$bestRT  <- rep(NA_real_, obj$w$ntrials) #initialize empty bestRT vector
  if (identical(obj$bestRT_t1, numeric(0))) {
    #default to average RT
    #message("Using average reaction time across block as best RT prior.")
    obj$w$bestRT[1L] <- obj$w$avg_RT
  } else {
    #use the user-specified value for the bestRT on t=1
    obj$w$bestRT[1L] <- obj$bestRT_t1
  }
  NextMethod()
}

reset_workspace.p_meanSlowFast=function(obj) {
  if (!exists("betaFastSlow", envir=obj$w, inherits=FALSE)) { obj$w$betaFastSlow <- betaFastSlow(obj$w) }
  NextMethod()
}

reset_workspace.p_epsilonBeta=function(obj) {
  if (!exists("betaFastSlow", envir=obj$w, inherits=FALSE)) { obj$w$betaFastSlow <- betaFastSlow(obj$w) }
  NextMethod()
}
