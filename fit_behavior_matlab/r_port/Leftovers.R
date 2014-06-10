
###
#leftovers
#predictRun_norefclass=function(theta, runRTs, runRewards, prior_w) {
#  #1) reset run workspace: setup RTobs, initial V, etc.
#  #2) copy run workspace address into alg
#  #3) potentially set global avg RT
#  #4) reset workspace for each parameter
#  #5) loop over trials, setting up RT_last, Reward_last, etc.
#  #    5a) call getRTUpdate for each parameter
#  
#  #long story short, we need the shared workspace to be the only thing that's needed by all update functions...
#  
#},


##
#some basic ideas for how to overcome the overhead of refclasses
#basically, in the fit function, setup a list for
# 1) getRTUpdate functions (copy from each parameter)
# 2) the shared workspace
# 3) would mean not accessing fields or methods of the function,
#    so, would need to avoid assigning to fields

#need to have RTobs and RTpred in workspace
#also would need some sort of list structure for runs...
#so, copy RTobs into a list structure with one element per Run
#same for rewards
#then lapply over the observed data
#would it be better to have a $predictLocal method that keeps existing OO-based predict intact?
#then $predictLocal would do the copying of the RTupdate funcs and data into primitives.

#updateFuncs <- lapply(.self$params, function(p) { p$getRTUpdate } )

#predictNoLookup=function(theta=get_param_current_vector(), updateFields=FALSE) {
#  if (class(clockData) == "clockSubject") {
#    runRTs <- lapply(clockData$runs, "[[", "RTobs")
#    runRews <- lapply(clockData$runs, "[[", "Reward")
#    resetFuncs <- lapply(.self$params, function(p) { p$reset_workspace } )
#    updateFuncs <- lapply(.self$params, function(p) { p$getRTUpdate } )
#  } else if (class(clockData) == "clockRun") {
#    
#  }
#},



#updateBetaDists=function(w) {
#  #because explore and meandiff parameters may both be present in the model
#  #need to check whether the beta distribution has already been updated on this trial
#  #if so, do not update again
#  
#  if (w$betaFastSlow$lastUpdateTrial == w$cur_trial) { return(invisible(NULL)) }
#  
#  w$betaFastSlow$lastUpdateTrial <- w$cur_trial
##  cat("cur_trial is: ", w$cur_trial, "\n")
##  cat("betaFastSlow_lastUpdateTrial is: ", w$betaFastSlow$lastUpdateTrial, "\n")
#  #browser()
#  #eval(quote(betaFastSlow$lastUpdateTrial <- cur_trial), w$w) #update the trial count for beta dist tracking
#  
#  #model tracks two distributions, one for fast responses (less than mean RT)
#  #and one for slow responses (above mean RT)
#  #here, we update the estimates of the corresponding beta distribution for slow or fast responses
#  #cache means and variances of prior trial
#  
#  #by updating betaFastSlow within the shared environment, we get persistence without return value.
#  #conceptually ugly relative to refClass implementation... need to think about this.
#  
#  w$betaFastSlow$mean_fast_last <- w$betaFastSlow$mean_fast
#  w$betaFastSlow$mean_slow_last <- w$betaFastSlow$mean_slow
#  w$betaFastSlow$var_fast_last  <- w$betaFastSlow$var_fast
#  w$betaFastSlow$var_slow_last  <- w$betaFastSlow$var_slow
#  w$betaFastSlow$local_RT_last  <- w$betaFastSlow$local_RT
#  
#  if (w$RT_last > w$betaFastSlow$local_RT_last) { #last response was slower than local average
#    if (w$Rew_last > w$V_last) { #ppe: increment alpha shape parameter for slow dist
#      w$betaFastSlow$alpha_slow <- w$betaFastSlow$alpha_slow + 1
#    } else {
#      w$betaFastSlow$beta_slow <- w$betaFastSlow$beta_slow + 1
#    }
#  } else if (w$RT_last <= w$betaFastSlow$local_RT_last) { #last response was faster than average
#    if(w$Rew_last > w$V_last) {
#      w$betaFastSlow$alpha_fast <- w$betaFastSlow$alpha_fast + 1
#    } else {
#      w$betaFastSlow$beta_fast <- w$betaFastSlow$beta_fast + 1
#    }
#  }
#  
#  if (w$betaFastSlow$decay < 1.0) {
#    w$betaFastSlow$alpha_slow     <- w$betaFastSlow$decay * w$betaFastSlow$alpha_slow # if decay < 1 then this decays counts, making beta dists less confident
#    w$betaFastSlow$beta_slow      <- w$betaFastSlow$decay * w$betaFastSlow$beta_slow
#    w$betaFastSlow$alpha_fast     <- w$betaFastSlow$decay * w$betaFastSlow$alpha_fast
#    w$betaFastSlow$beta_fast      <- w$betaFastSlow$decay * w$betaFastSlow$beta_fast
#  }
#  
#  # compute mode and variances of beta distribution
#  w$betaFastSlow$var_fast    <- w$betaFastSlow$alpha_fast * w$betaFastSlow$beta_fast / ( (w$betaFastSlow$alpha_fast + w$betaFastSlow$beta_fast)^2 * (w$betaFastSlow$alpha_fast + w$betaFastSlow$beta_fast + 1) )
#  w$betaFastSlow$var_slow    <- w$betaFastSlow$alpha_slow*w$betaFastSlow$beta_slow/( (w$betaFastSlow$alpha_slow + w$betaFastSlow$beta_slow)^2 * (w$betaFastSlow$alpha_slow + w$betaFastSlow$beta_slow + 1) )
#  #mode_slow  <- (alpha_slow - 1) / (alpha_slow + beta_slow - 2) #not used at present, omit for optimization speed
#  #mode_fast  <- (alpha_fast - 1) / (alpha_fast + beta_fast - 2) 
#  w$betaFastSlow$mean_slow   <- w$betaFastSlow$alpha_slow / (w$betaFastSlow$alpha_slow + w$betaFastSlow$beta_slow)
#  w$betaFastSlow$mean_fast   <- w$betaFastSlow$alpha_fast / (w$betaFastSlow$alpha_fast + w$betaFastSlow$beta_fast)
#  
#  w$betaFastSlow$local_RT <- w$betaFastSlow$local_RT_last + w$betaFastSlow$local_RT_learning_rate * (w$RT_last - w$betaFastSlow$local_RT_last) # update estimate of recent RTs by 10% (0.1) of deviation of this trial's RT from the local average      
#  
#  
##  w$betaFastSlow <- within(w$betaFastSlow, {
##        mean_fast_last <- mean_fast
##        mean_slow_last <- mean_slow
##        var_fast_last  <- var_fast
##        var_slow_last  <- var_slow
##        local_RT_last  <- local_RT
##        
##        if (w$RT_last > local_RT_last) { #last response was slower than local average
##          if (w$Rew_last > w$V_last) { #ppe: increment alpha shape parameter for slow dist
##            alpha_slow <- alpha_slow + 1
##          } else {
##            beta_slow <- beta_slow + 1
##          }
##        } else if (w$RT_last <= local_RT_last) { #last response was faster than average
##          if(w$Rew_last > w$V_last) {
##            alpha_fast <- alpha_fast + 1
##          } else {
##            beta_fast <- beta_fast + 1
##          }
##        }
##        
##        if (decay < 1.0) {
##          alpha_slow     <- decay * alpha_slow # if decay < 1 then this decays counts, making beta dists less confident
##          beta_slow      <- decay * beta_slow
##          alpha_fast     <- decay * alpha_fast
##          beta_fast      <- decay * beta_fast
##        }
##        
##        # compute mode and variances of beta distribution
##        var_fast    <- alpha_fast * beta_fast / ( (alpha_fast + beta_fast)^2 * (alpha_fast + beta_fast + 1) )
##        var_slow    <- alpha_slow*beta_slow/( (alpha_slow + beta_slow)^2 * (alpha_slow + beta_slow + 1) )
##        #mode_slow  <- (alpha_slow - 1) / (alpha_slow + beta_slow - 2) #not used at present, omit for optimization speed
##        #mode_fast  <- (alpha_fast - 1) / (alpha_fast + beta_fast - 2) 
##        mean_slow   <- alpha_slow / (alpha_slow + beta_slow)
##        mean_fast   <- alpha_fast / (alpha_fast + beta_fast)
##        
##        local_RT <- local_RT_last + local_RT_learning_rate * (w$RT_last - local_RT_last) # update estimate of recent RTs by 10% (0.1) of deviation of this trial's RT from the local average      
##      })
#  
#}

#ugly version without eval
#  obj$w$NoGo_last   <- obj$w$NoGo[obj$w$lastTrial]
#  
#  #carry forward NoGo term unless updated below by NPE
#  obj$w$NoGo_new <- obj$w$NoGo_last
#  
#  #if obtained reward was worse than expected (NPE), slow down (scaled by alphaN)
#  if (obj$w$Rew_last <= obj$w$V_last) {                  
#    obj$w$NoGo_new <- obj$w$NoGo_last + theta[obj$name]*(obj$w$V_last - obj$w$Rew_last)
#  }
#  
#  obj$w$NoGo[obj$w$cur_trial] <- obj$w$NoGo_new                


#rho and epsilon parameters both depend on tracking two beta
#distributions for fast and slow responses.
#rho scales with the difference in mean expected payoff for fast versus slow responses
#epsilon scales with difference in uncertainty (SD) for fast versus slow responses
#abstract the beta tracking to a shared class
#and the means vs SDs lookup is extrapolated to the explore and meandiff classes
#betaFastSlow <- function(
#    w=NULL, #shared workspace
#    alpha_slow=1.01, #alpha shape parameter for slow (above average) RTs 
#    beta_slow=1.01, #beta shape parameter for slow RTs
#    alpha_fast=1.01, 
#    beta_fast=1.01, 
#    decay=1.0, 
#    lastUpdateTrial=1L, 
#    local_RT_learning_rate=0.1) #TODO: Does the learning rate for RT need to be yoked to alphaV for the critic update, as in MF's code? 
#{
#  stopifnot(decay <= 1.0)
#  if (is.null(w) || !is.environment(w)) { stop ("Shared workspace w must be passed at initialization into betaFastSlow") }
#  obj <- as.list(environment()) #copy initialization parameters to object
#  obj <- c(obj, list(
#          mean_fast=numeric(0),
#          mean_slow=numeric(0),
#          mode_fast=numeric(0),
#          mode_slow=numeric(0),
#          var_fast=numeric(0),
#          var_slow=numeric(0),
#          mean_fast_last=numeric(0),
#          mean_slow_last=numeric(0),
#          var_fast_last=numeric(0),
#          var_slow_last=numeric(0),
#          explore=numeric(0),
#          explore_last=numeric(0),
#          local_RT=numeric(0),
#          local_RT_last=numeric(0)
#      ))
#  #class(obj) <- "betaFastSlow" #keep as list for use of within in updateBetaDist
#  
#  obj$local_RT[1L] <- obj$local_RT_last[1L] <- w$avg_RT #set initial local average to the block mean RT
#  
#  return(invisible(obj))
#}


#if (updateFields) {
#  obj$w$pred_contrib[[obj$name]][obj$w$cur_trial] <- rtContrib 
#}