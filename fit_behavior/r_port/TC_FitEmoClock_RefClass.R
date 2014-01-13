setwd("~/CogEmoFaceReward/fit_behavior/r_port")
#Object-oriented R implementation of time clock algorithm
source("TC_ClockData_RefClasses.R")

#add a reset method here to set values back to initial (clear for re-using the same alg object with a new dataset)
#actually, couldn't we just note when the dataset is updated and zero out parameters?
alg <- setRefClass(
    Class="alg",
    fields=list(
        #updateEquation="expression",
        params="list",
        noiseWt="numeric",
        SSE="numeric",
        clockData="ANY", #allow this to be dataset, subject, or run,
        use_global_avg_RT="logical",
        global_avg_RT="numeric"
    ),
    methods=list(
        initialize=function(clockData=NULL, ...) {
          cat("Initializing alg\n")
          if (!is.null(clockData) && !class(clockData) %in% c("clockDataset", "clockSubject", "clockRun")) {
            stop("data for alg object must be one of the following types: clockDataset, clockSubject, or clockRun")
          }
          
          params <<- list() #initialize empty list of model parameters
          noiseWt <<- 0 #do not add noise to RT prediction
          use_global_avg_RT <<- TRUE #whether to use average RT across all blocks in fit (e.g., "go for gold" scales wrt avg_RT). 
          set_global_avg_RT()
          
          callSuper(...) #for classes inheriting from this, pass through unmatched iniitalization values
        },
        set_global_avg_RT=function() {
          if (use_global_avg_RT && !is.null(clockData) && class(clockData) == "clockSubject") {
            #compute average reaction time across all runs
            global_avg_RT <<- mean(do.call("c", lapply(clockData$runs, "[[", "RTobs")), na.rm=TRUE)
          }
        },
        lappend=function(target, append) {
          if (!is.list(target)) stop("target is not a list.")
          if (!is.list(append)) stop("append is not a list.")

          #force names of list to match corresponding parameter names
          names(append) <- sapply(append, function(p) { return(p$name) })
          
          for (elementName in names(append)) {
            if (!is.null(target[[elementName]])) warning("Element is already present in target list: ", elementName)
            target[[elementName]] <- append[[elementName]]
          }
          
          return(target)
        },
        add_params=function(...) {
          #accept a variable number of parameter objects
          p_objs <- as.list(match.call())[-1L] #first element of match.call is a class for refMethodDef for the alg object
          p_objs <- lapply(p_objs, eval) #force initialization of param objects (otherwise stays as a language expression)
          
          #pass along address of shared workspace to sub-objects
          #now diverting workspace issues until just before fit
          #allow alg object to be completely setup independent of data
          #p_objs <- lapply(p_objs, function(p) { p$w <- .self$w; p$reset_workspace(); return(p) })
          
          params <<- lappend(params, p_objs)
        },
        list_params=function() {
          cat("Current parameters in algorithm\n\n")
          df <- data.frame(
              p_name=names(params), 
              min_value=get_param_minimum_vector(),
              max_value=get_param_maximum_vector(),
              init_value=get_param_initial_vector(),
              cur_value=get_param_current_vector(),
              par_scale=get_param_par_scale_vector()
          )
          #cat(names(params), sep="\n")
          print(format(df,digits=3,scientific=FALSE))
        },
        reorder_params=function(neworder) {
          #reorders the parameter collection
          #potentially useful if there are dependencies among parameters or workspace values: params are evaluated in sequential order 
          left_out <- names(params)[! names(params) %in% neworder]
          if (length(left_out) > 0L) {
            warning("reorder_params call did not include position for: ", paste(left_out, collapse=", "), ". Defaulting to end")
            neworder <- c(neworder, left_out)
          }
          if (length(unmatched <- neworder[! neworder %in% names(params)]) > 0L) {
            stop("reorder_params unable to find parameters in alg: ", paste(unmatched, collapse=", "))
          }
          params <<- params[neworder]
        },
        
        #these functions get values, bounds, and scales for all parameters in the current model 
        get_param_current_vector=function() {
          sapply(params, function(p) { p$cur_value })
        },        
        get_param_minimum_vector=function() {
          sapply(params, function(p) { p$min_value })          					
        },        
        get_param_maximum_vector=function() {
          sapply(params, function(p) { p$max_value })
        },        
        get_param_initial_vector=function() {
          sapply(params, function(p) { p$init_value })
        },        
        get_param_par_scale_vector=function() {
          sapply(params, function(p) { p$par_scale })
        },
        
        fit=function(toFit=NULL, initialValues=get_param_initial_vector(),
            lower=get_param_minimum_vector(),
            upper=get_param_maximum_vector()) {
          
          require(foreach)
          #require(optimx)
          
          #can pass in new dataset at $fit call
          #if not passed in, use the current clockData field
          if (!is.null(toFit)) {
            if (!class(toFit) %in% c("clockDataset", "clockSubject", "clockRun")) { stop("data for alg object must be one of the following types: clockDataset, clockSubject, or clockRun")
            } else { 
              clockData <<- toFit
              set_global_avg_RT() #compute global RT
            }
          }
          
          #call predict using optimizer
          if (length(initialValues) == 1L) { method="Brent"
          } else { method="L-BFGS-B" }
          
#          if (length(initialValues) == 1L) { method="Brent"
#          } else { method="Rvmmin" } #bobyqa
          
          #lapply(params, function(p) { p$value_history <- numeric(0) }) #reset value history for all parameters prior to fit REFCLASS VERSION
          params <<- lapply(params, function(p) { p$value_history <- numeric(0); return(p) }) #reset value history for all parameters prior to fit S3 VERSION
          
          #this is a bit scary/problematic to the extent that RT prediction depend on proper scaling
          #initialValues <- initialValues / get_param_par_scale_vector() #scale prior to optimization
          
          #system.time(optResult <- optim(par=initialValues, fn=.self$predict, method=method,
          #        lower=lower, upper=upper,
          #        control=list(parscale=get_param_par_scale_vector()),
          #        updateFields=FALSE, trackHistory=TRUE))
          
          #track optimization
          Rprof("tc_prof.out")#, line.profiling=TRUE)
          
          #this is the most sensible, and corresponds to optim above (and is somewhat faster)
          elapsed_time <- system.time(optResult <- nlminb(start=initialValues, objective=.self$predict, 
                  lower=lower, upper=upper, scale=1/get_param_par_scale_vector(),
                  updateFields=FALSE, trackHistory=TRUE))
          
          Rprof(NULL)          
          
          prof <- summaryRprof("tc_prof.out")#, lines = "both")
          unlink("tc_prof.out")
                    
          #this is roughly what is suggested in the PORT documentation (1/max scaling). But it's slower than 1/magnitude above.
          #system.time(optResult <- nlminb(start=initialValues, objective=.self$predict, lower=lower, upper=upper, scale=1/upper, #scale=c(1, 100, 100, 100, 100),
          #        updateFields=FALSE, trackHistory=TRUE))
          
          #even though this is faster, I have no idea why one would use a scale of 100 for all...
          #system.time(optResult <- nlminb(start=initialValues, objective=.self$predict, lower=lower, upper=upper, scale=100L, #scale=c(1, 100, 100, 100, 100),
          #        updateFields=FALSE, trackHistory=TRUE))
          
          #system.time(optResult <- nlminb(start=initialValues, objective=.self$predict, lower=lower, upper=upper, scale=10L, #scale=c(1, 100, 100, 100, 100),
          #        updateFields=FALSE, trackHistory=TRUE))
          
          #system.time(optResult <- nlminb(start=initialValues, objective=.self$predict, lower=lower, upper=upper,
          #        updateFields=FALSE, trackHistory=TRUE))
          
          #scalecheck()
          #optimx does not appear to be suited to pass through scale to nlminb...
          #elapsed_time <- system.time(optResult <- optimx(par=initialValues, fn=.self$predict, method="nlminb",
          #        lower=lower, upper=upper, 
          #        control=list(
          #            scale=1/get_param_par_scale_vector(), #parameter scaling since values are quite different in magnitude 
          #            usenumDeriv=FALSE, 
          #            all.methods=FALSE
          #            ),
          #        updateFields=FALSE, trackHistory=TRUE))
          
          #time <- system.time(p <- optimx(par=initialValues, fn=.self$predict, lower=lower, upper=upper, method=method, control=list(parscale=c(1e3, 1e0, 1e-1, 1e-1))))
          #time <- system.time(p <- optimx(par=initialValues, fn=.self$predictNoLookup, lower=lower, upper=upper, method=method, updateFields=FALSE))
          
          if (optResult$convergence == 0) {
            #success
            #lapply(params, function(p) { p$cur_value <- optResult$par[p$name] }) #set current parameter values based on optimization REFCLASS
            #need to unname the parameter value to avoid getting a named vector, which lapply concatenates with list names (e.g., lambda.lambda)
            params <<- lapply(params, function(p) { p$cur_value <- unname(optResult$par[p$name]); return(p) }) #set current parameter values based on optimization S3

            #compute trialwise predictions with optimized parameters
            .self$predict(updateFields=TRUE, trackHistory=FALSE)
            
            #TODO: Would be nice if $fit returned a fit object that had parameter values, the constituent environments that were fit,
            # and key workspace variables as data.frames (e.g., predicted RTs, observed RTs, trial-by-trial param contributions, etc.
  
            #TODO: compute AIC here.
          } else {
            warning("Optimization failed.")
          }
          
          return(list(optResult, elapsed_time, prof))
          #optimize(.self$predict, interval=c(100, 4000))
          
        },
        
        predict=function(theta=get_param_current_vector(), updateFields=FALSE, trackHistory=FALSE) {
          #TODO: alg object shoud allow for some sort of symbolic specification of how
          #values from prior runs are carried forward.
          #this is an alg-level decision, not subject, run, parameter, etc.
          
          totalSSE <- 0
          
          #track value of parameters over the course of optimization
          if (trackHistory) {
            params <<- lapply(params, function(p) { p$value_history <- c(p$value_history, theta[p$name]); return(p) }) #set current parameter values based on optimization S3 version
            #lapply(params, function(p) { p$value_history <- c(p$value_history, theta[p$name]) }) #set current parameter values based on optimization REFCLASS VERSION
          }
          
          if (class(clockData)=="clockDataset") {
            #loop over subjects and runs (group fit)
            for (s in clockData$subjects) { #make this parallel
              for (r in s$runs) {
                
              }
            }
          } else if (class(clockData)=="clockSubject") {            
            for (r in 1:length(clockData$runs)) {
              if (r > 1L) {
                prior_w <- clockData$runs[[r - 1]]$w #pass along environment from previous run
              } else { prior_w <- NULL }
              
              #pass forward theta (likely from optim)
              totalSSE <- totalSSE + predictRun(theta=theta, clockRun=clockData$runs[[r]], prior_w=prior_w, updateFields=updateFields) #no cached update functions
            }
            
          } else if (class(clockData)=="clockRun") {
            #pass forward theta (likely from optim)
            totalSSE <- predictRun(theta=theta, clockRun=clockData)
          }
          
          return(totalSSE)
        },
        
        predictRun=function(clockRun=NULL, prior_w=NULL, theta=get_param_current_vector(), updateFields=FALSE) { #, updateFuncs=NULL) {
          if (is.null(clockRun) || !class(clockRun) == "clockRun") { stop("predictRun requires a clockRun object") }
          
          #setup basics of workspace -- shouldn't this be in alg conceptually?
          clockRun$reset_workspace(prior_w)
          
          #UPDATE: Try just having a local w here -- maybe code analysis will then not stink up the place with as.environment etc.
          w <- clockRun$w
          
          #if using global RT average, set environment here
          #N.B. Need to set here before calling reset in each param because this is where bestRT[1] is set...
          #would be better to make this more robust
          if (use_global_avg_RT) { w$avg_RT <- global_avg_RT }
          
          #lapply(.self$params, function(p) { p$w <- .self$w; p$reset_workspace() }) #setup workspace variables for each parameter REFCLASS VERSION
          params <<- lapply(.self$params, function(p) { p$w <- w; reset_workspace(p); return(p) }) #setup workspace variables for each parameter (NEED TO UPDATE PARAMS SINCE THEY ARE PASS BY VALUE)
          
          #optim using Brent, or optimize fail to pass names of parameters into predict.
          #names are critical for the params to lookup properly.
          if (is.null(names(theta))) {
            #warning("Unnamed theta vector passed to predict. Looking up from parameter list.")
            names(theta) <- names(params) #sapply(params, function(p) { return(p$name) })
          }
          
          #reset workspace before proceeding!
          
          w$RT_new  <- w$RTobs[1L] #use observed RT as predicted RT for t=1
          w$RT_last <- w$RTobs[1L]
          w$RT_last2 <- w$RTobs[1L]
          
          #TODO: Risk that we initialize some vals above (like initial V), but predict is called iteratively for optimization, so values will not be reset here if they were set in initialize
          
          for (t in 2:w$ntrials) {
            w$cur_trial <- t
            w$lastTrial <- t - 1
            
            eval(
                quote({
                      Rew_last <- Reward[lastTrial]
                      RT_last  <- RTobs[lastTrial]
                      if (cur_trial > 2) { RT_last2 <- RTobs[cur_trial - 2] }
                      
                      V_last <- V[lastTrial]
                      V_new = V_last + alphaV*(Rew_last - V_last) # update critic expected value
                      V[cur_trial] <- V_new
                    }), 
                w)
            
            #refclass version
            #w$RT_new <<- sum(sapply(params, function(p) { p$getRTUpdate(theta, updateFields=updateFields) } )) + noiseWt*(runif(1,-0.5,0.5)) #add or subtract noise according to noiseWt (0 for now)
            #w$RT_new <- sum(sapply(params, function(p) { getRTUpdate(p, theta, updateFields=updateFields) } )) + noiseWt*(runif(1,-0.5,0.5)) #add or subtract noise according to noiseWt (0 for now)
            
            #getting profile information that simplify2array is taking a few seconds. Try unlist(lapply(
            noiseContrib <- if (noiseWt > 0.0) { noiseWt*(runif(1,-0.5,0.5)) } else { 0.0 }
            w$RT_new <- sum(pred_contrib <- unlist(lapply(params, function(p) { getRTUpdate(p, theta, updateFields=updateFields) } ))) + noiseContrib
            
            if (updateFields) { #track trial-by-trial contribution of each param to RT prediction
              pnames <- names(params)
              for ( p in 1:length(pnames) ) {
                w$pred_contrib[[ pnames[p] ]][w$cur_trial] <- pred_contrib[p]
              }
            }
            
            w$RTpred[w$cur_trial] <- w$RT_new
            
            #TODO: incorporate this code from matlab. Affects beta dist local_RT and autocorrelation parameters 
            #if RT_last==0, RT_last = RT_last2; end; %% if last trial there
            #was no response, use trial before that for updating RT avg and autocorrelation effects (otherwise counted as 0)
            
            w$rpe[t-1] <- w$Rew_last - w$V_last
            
          }
          
          SSE <<- sum((w$RTobs - w$RTpred)^2) #sum of squared error: cost
          #cat("SSE: ", SSE, "\n")
          return(SSE)
        })

)


#generic parameter class
#should expose:
#  - min_value:  minimum parameter value in optimization
#  - max_value:  maximum parameter value
#  - init_value: initial parameter value
#  - cur_value:  current parameter value
#
#  - getRTUpdate: compute contribution to RT prediction for this parameter

#general sanity checks and field assignment handled here so subordinate classes need not duplicate
initialize_par <- function(obj, min_value=NULL, max_value=NULL, init_value=NULL, cur_value=NULL, par_scale=NULL) {
  stopifnot(cur_value <= max_value)
  stopifnot(cur_value >= min_value)
  stopifnot(init_value <= max_value)
  stopifnot(init_value >= min_value)
  
  obj$min_value <- min_value
  obj$max_value <- max_value
  obj$init_value <- init_value
  obj$cur_value <- cur_value
  obj$par_scale <- par_scale
  obj$w <- emptyenv() #workspace should never be unique to parameter, but is set upstream by alg
  
  return(obj)
}


##some notes on an s3 implementation
##  can use shared w to update multiple values without having to copy objects etc.
##  but functions can have few, if any, side effects for updating object fields... would need a separate assignment outside of the function.


###PARAMETER CONSTRUCTORS
#meanRT constructor
meanRT <- function(min_value=100, max_value=5000, init_value=1000, cur_value=init_value, par_scale=1e3) {
  
  obj <- structure(
      list(
          name = "K"
      ), class=c("p_meanRT", "param"))
  
  obj <- initialize_par(obj, min_value, max_value, init_value, cur_value, par_scale) #check and initialize fields
  return(invisible(obj))
}


#autocorrelation with previous reaction time
autocorrPrevRT <- function(min_value=0.0, max_value=1.0, init_value=0.3, cur_value=init_value, par_scale=1e-1) {  
  obj <- structure(
      list(name = "lambda"),
      class=c("p_autocorrPrevRT", "param")
  )
  
  if (min_value < 0.0) { stop("Autocorr prev RT (lambda) parameter cannot be negative") }
  if (max_value > 1.0) { stop("Autocorr prev RT (lambda) parameter cannot exceed 1.0") }
  
  obj <- initialize_par(obj, min_value, max_value, init_value, cur_value, par_scale) #check and initialize fields
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
noGo <- function(min_value=0.01, max_value=5.0, init_value=0.2, cur_value=init_value, par_scale=1e-1) {
  obj <- structure(
      list(name = "alphaN"),
      class=c("p_nogo", "param")
  )
  
  if (min_value < 0.01) { stop("alphaN min_value must be at least 0.01") }
  if (max_value > 5.0) { stop("alphaN max_value must be less than 5.0") }
  
  obj <- initialize_par(obj, min_value, max_value, init_value, cur_value, par_scale) #check and initialize fields
  return(invisible(obj))
}

##Go for the gold: adapt RT toward the best
goForGold <- function(min_value=0.0, max_value=100.0, init_value=0.1, cur_value=init_value, par_scale=1e-1, bestRT_t1=numeric(0)) {
  obj <- structure(
      list(
          name = "scale",
          bestRT_t1 = bestRT_t1 #initial value for best reaction time on trial 1
      ), class=c("p_gold", "param"))
  
  if (min_value < 0) { stop("Go for gold (nu) parameter cannot be negative") }
  
  obj <- initialize_par(obj, min_value, max_value, init_value, cur_value, par_scale) #check and initialize fields
  return(invisible(obj))
}

##rho: Adapt toward fast responses if these have been better thus far
meanSlowFast <- function(min_value=0, max_value=10000, init_value=300, cur_value=init_value, par_scale=1e2) {
  obj <- structure(
      list(name = "rho"), 
      class=c("p_meanSlowFast", "param")
  )
  
  if (min_value < 0) { stop("Slow versus fast mean parameter (rho) cannot be negative") }
  
  obj <- initialize_par(obj, min_value, max_value, init_value, cur_value, par_scale) #check and initialize fields
  return(invisible(obj))
}

#epsilon: adapt to fast/slow responses in proportion to uncertainty about other dist
exploreBeta <- function(min_value=0, max_value=100000, init_value=2000, cur_value=init_value, par_scale=1e3) {
  obj <- structure(
      list(name = "epsilonBeta"), 
      class=c("p_epsilonBeta", "param")
  )
  
  obj <- initialize_par(obj, min_value, max_value, init_value, cur_value, par_scale) #check and initialize fields
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
getRTUpdate <- function(obj, theta, updateFields=FALSE) { UseMethod("getRTUpdate") } #general dispatch

getRTUpdate.default <- function(obj, theta, updateFields=FALSE) {
  return (NULL) 
} #shouldn't have an empty rt update

#K: mean RT
getRTUpdate.p_meanRT <- function(obj, theta, updateFields=FALSE) {
  theta[obj$name] #for baseline RT, the parameter itself is the speed in ms
}

#lambda: corr with prior RT
getRTUpdate.p_autocorrPrevRT <- function(obj, theta, updateFields) {
  theta[obj$name]*obj$w$RT_last
}

#nu: go for the gold 
getRTUpdate.p_gold <- function(obj, theta, updateFields=FALSE) {
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
  
  theta[obj$name]*with(obj$w, bestRT[cur_trial] - avg_RT) #rtContrib
}

#Go: speed up for PPE
getRTUpdate.p_go <- function(obj, theta, updateFields) {
  #a bit of a hack here to copy the alphaG learning rate into w for easier code below
  obj$w$cur_value <- theta[obj$name]
  
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
getRTUpdate.p_nogo <- function(obj, theta, updateFields) {
  #a bit of a hack here to copy the alphaN learning rate into w for easier code below
  obj$w$cur_value <- theta[obj$name]
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
  #browser()
  #eval(quote(betaFastSlow$lastUpdateTrial <- cur_trial), w$w) #update the trial count for beta dist tracking
  
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


getRTUpdate.p_meanSlowFast=function(obj, theta, updateFields=FALSE) {
  updateBetaDists(obj$w$betaFastSlow) #update fast/slow beta dists
  theta[obj$name] * with(obj$w$betaFastSlow, mean_slow - mean_fast) #rtContrib
}


getRTUpdate.p_epsilonBeta=function(obj, theta, updateFields=FALSE) {
  #model tracks two distributions, one for fast responses (less than mean RT)
  #and one for slow responses (above mean RT)
  #here, we update the estimates of the corresponding beta distribution for slow or fast responses
  
  obj$w$explore_last <- obj$w$explore
  updateBetaDists(obj$w$betaFastSlow) #update fast/slow beta dists
  
  obj$w$cur_value <- theta[obj$name]
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

reset_workspace <- function(obj) { UseMethod("reset_workspace") }
#reset_workspace.param <- function(obj) { obj$w$pred_contrib[[obj$name]] <- rep(NA_real_, obj$w$ntrials) }

reset_workspace.p_go <- function(obj) {
  obj$w$Go <- rep(NA_real_, obj$w$ntrials) #vector of Go term
  obj$w$Go[1L] <- 0.0 #may want to override this later...
#  NextMethod()
}

reset_workspace.p_nogo <- function(obj) {
  obj$w$NoGo <- rep(NA_real_, obj$w$ntrials) #vector of NoGo term
  obj$w$NoGo[1L] <- 0.0 #may want to override this later...
  #NextMethod()
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
  #NextMethod()
}

reset_workspace.p_meanSlowFast=function(obj) {
  if (!exists("betaFastSlow", envir=obj$w, inherits=FALSE)) { obj$w$betaFastSlow <- betaFastSlow(obj$w) }
  #NextMethod()
}

reset_workspace.p_epsilonBeta=function(obj) {
  if (!exists("betaFastSlow", envir=obj$w, inherits=FALSE)) { obj$w$betaFastSlow <- betaFastSlow(obj$w) }
  #NextMethod()
}
