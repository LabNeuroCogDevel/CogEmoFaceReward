#OO implementation of clock algorithm

#' Define a predictive model to fit RT and reward data from clock task. 
#'
#' @method initialize Constructor function for object. Accepts an optional object of clockData
#'
#' @param clockData optional. Must be of class clockData, clockRun, clockSubject


#add a reset method here to set values back to initial (clear for re-using the same alg object with a new dataset)
#actually, couldn't we just note when the dataset is updated and zero out parameters?
clock_model <- setRefClass(
    Class="clock_model",
    fields=list(
        #updateEquation="expression",
        params="list",
        noiseWt="numeric",
        SSE="numeric",
        AIC="numeric",
        clockData="ANY", #allow this to be dataset, subject, or run,
        use_global_avg_RT="logical",
        global_avg_RT="numeric",
        all_by="character" #character vector that is the union of all run-level fields defining vary-by-run parameters
    ),
    methods=list(
        #' are you listening?
        #' 
        #' @param clockData test.
        #' @param ... all others.
        
        initialize=function(clockData=NULL, ...) {
          cat("Initializing clock_model\n")
          
          params <<- list() #initialize empty list of model parameters
          noiseWt <<- 0 #do not add noise to RT prediction
          use_global_avg_RT <<- TRUE #whether to use average RT across all blocks in fit (e.g., "go for gold" scales wrt avg_RT). 
          
          if (!is.null(clockData)) { set_data(clockData) }
          callSuper(...) #for classes inheriting from this, pass through unmatched iniitalization values
        },
        set_global_avg_RT=function() {
          if (use_global_avg_RT && !is.null(clockData)) {
            if (class(clockData) == "clockSubject") {
              #compute average reaction time across all runs
              global_avg_RT <<- mean(do.call("c", lapply(clockData$runs, "[[", "RTobs")), na.rm=TRUE)
            } else if (class(clockData) == "clockRun") {
              global_avg_RT <<- mean(clockData$RTobs, na.rm=TRUE)
            }
          } 
        },
        lappend=function(target, append) {
          if (!is.list(target)) stop("target is not a list.")
          if (!is.list(append)) stop("append is not a list.")
          
          #force names of list to match corresponding parameter names
          #this ignores any list names provided by user
          names(append) <- sapply(append, function(p) { return(p$name) })
          
          for (elementName in names(append)) {
            if (!is.null(target[[elementName]])) warning("Element is already present in target list: ", elementName)
            target[[elementName]] <- append[[elementName]]
          }
          
          return(target)
        },
        add_params=function(...) {
          #accept a variable number of parameter objects
          p_objs <- as.list(match.call())[-1L] #first element of match.call is a class for refMethodDef for the clock_model object
          p_objs <- lapply(p_objs, eval) #force initialization of param objects (otherwise stays as a language expression)
          
          #pass along address of shared workspace to sub-objects
          #now diverting workspace issues until just before fit
          #allow clock_model object to be completely setup independent of data
          #p_objs <- lapply(p_objs, function(p) { p$w <- .self$w; p$reset_workspace(); return(p) })
          
          params <<- lappend(params, p_objs)
          
          ##TODO: need to call 
        },
        
        set_data=function(cdata) {
          if (!class(cdata) %in% c("clockDataset", "clockSubject", "clockRun")) {
            stop("data for clock_model object must be one of the following types: clockDataset, clockSubject, or clockRun")
          }
          
          clockData <<- cdata
          set_global_avg_RT()
          
          setup_param_by()
        },
        
        setup_param_by=function() { #expand parameters to accommodate per-condition variation, depending on clockData
          if (inherits(clockData, "uninitializedField")) { return(invisible(NULL)) } #do nothing
          
          #determine by settings for all parameters
          if (!is.null( uniq_by <- unique(unlist(lapply(params, function(p) { p$by }))) )) { 
            all_by <<- uniq_by 
          } else {
            return(invisible(NULL)) #no use of by-condition parameters
          }
          
          #develop a lookup matrix of unique conditions combinations
          if (class(clockData) == "clockSubject") {
            conMat <- lapply(clockData$runs, function(r) {
                  #set by_lookup (union of all by fields) for each run object
                  #and return a list of conditions for each run to be expanded for each parameter below, as needed
                  r$by_lookup <- sapply(all_by, function(b) { r[[b]]} )
                })
            conMat_df <- data.frame(do.call(rbind, conMat))
            for (n in 1:ncol(conMat_df)) {
              conMat_df[,n] <- paste(names(conMat_df)[n], conMat_df[,n], sep=":")
            }
            params <<- lapply(params, function(p) {
                  p$base_name <- p$name[1L] #just basic parameter name without condition (used for getTheta lookup) 
                  if (!is.null(p$by)) {
                    #make sure that by is sorted alpha to correspond with getTheta
                    p$by <- sort(p$by)
                    #format parameter names into <param>/condition:value/condition:value/.../ for passing to optimizer (parse within predict)
                    #for now, leave off param name, just keep condition names
                    #when calling get_param_initial_vector, etc., lapply will include param name in vector
                    p$name <- paste(p$name, unique(apply(conMat_df[,p$by, drop=FALSE], 1, paste, collapse="/")), sep="/") #alpha sort is important so that getTheta can assume that order of theta condition string is alphabetical
                    #p$name <- unique(apply(conMat_df[,p$by, drop=FALSE], 1, paste, collapse="/"))
                    #I suppose using the first element as the value to replicate could backfire if an clock_model object is recycled across datasets.
                    p$init_value <- rep_len(p$init_value[1L], length(p$name))
                    p$cur_value <- rep_len(p$cur_value[1L], length(p$name))
                    p$max_value <- rep_len(p$max_value[1L], length(p$name))
                    p$min_value <- rep_len(p$min_value[1L], length(p$name))
                    p$par_scale <- rep_len(p$par_scale[1L], length(p$name))
                    names(p$init_value) <- names(p$cur_value) <- names(p$max_value) <- names(p$min_value) <- names(p$par_scale) <- p$name
                  }
                  p
                })
          }
          
        },
        
        list_params=function() {
          #cat("Current parameters in algorithm\n\n")
          df <- cbind(min_value=get_param_minimum_vector(),
              max_value=get_param_maximum_vector(),
              init_value=get_param_initial_vector(),
              cur_value=get_param_current_vector(),
              par_scale=get_param_par_scale_vector()
          )
          rownames(df) <- unlist(lapply(params, function(p) { p$name }))
          #cat(names(params), sep="\n")
          #print(format(df,digits=3,scientific=FALSE))
          return(df)
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
            stop("reorder_params unable to find parameters in clock_model: ", paste(unmatched, collapse=", "))
          }
          params <<- params[neworder]
        },
        
        #these functions get values, bounds, and scales for all parameters in the current model 
        get_param_current_vector=function() {
          unlist(unname(lapply(params, function(p) { p$cur_value }))) #need to unname lapply vector to avoid appending names(params) to each element
        },        
        get_param_minimum_vector=function() {
          unlist(unname(lapply(params, function(p) { p$min_value })))          					
        },        
        get_param_maximum_vector=function() {
          unlist(unname(lapply(params, function(p) { p$max_value })))
        },        
        get_param_initial_vector=function() {
          unlist(unname(lapply(params, function(p) { p$init_value })))
        },        
        get_param_par_scale_vector=function() {
          unlist(unname(lapply(params, function(p) { p$par_scale })))
        },
        
        fit=function(toFit=NULL, random_starts=NULL, profile=TRUE) {
          
          #can pass in new dataset at $fit call
          #if not passed in, use the current clockData field
          if (!is.null(toFit)) {
            set_data(toFit)
          }
          
          initialValues <- get_param_initial_vector()
          lower <- get_param_minimum_vector()
          upper <- get_param_maximum_vector()
          
          if (inherits(clockData, "uninitializedField")) { stop("clock dataset must be provided prior to $fit(). Set using $set_data()") }
          
          #require(optimx)
          
          #call predict using optimizer
          if (length(initialValues) == 1L) { method="Brent"
          } else { method="L-BFGS-B" }
          
          #lapply(params, function(p) { p$value_history <- numeric(0) }) #reset value history for all parameters prior to fit REFCLASS VERSION
          params <<- lapply(params, function(p) { p$value_history <- numeric(0); return(p) }) #reset value history for all parameters prior to fit S3 VERSION
          
          #initialValues <- initialValues / get_param_par_scale_vector() #scale prior to optimization
          
          #system.time(optResult <- optim(par=initialValues, fn=.self$predict, method=method,
          #        lower=lower, upper=upper,
          #        control=list(parscale=get_param_par_scale_vector()),
          #        updateFields=FALSE, trackHistory=TRUE))
          
          if (!is.null(random_starts) && is.numeric(random_starts)) {
            require(foreach)
            require(doMC)
            njobs <- min(parallel::detectCores, random_starts)
            registerDoMC(njobs)
            
          }
          
          #track optimization
          if (profile) { Rprof("tc_prof.out") }
          
          #this is the most sensible, and corresponds to optim above (and is somewhat faster)
          elapsed_time <- system.time(optResult <- nlminb(start=initialValues, objective=.self$predict, 
                  lower=lower, upper=upper, scale=1/get_param_par_scale_vector(),
                  updateFields=FALSE, trackHistory=TRUE))
          
          if (profile) {
            Rprof(NULL) #stop profiling
            
            prof <- summaryRprof("tc_prof.out")#, lines = "both")
            unlink("tc_prof.out") #remove profile file
          } else {
            prof <- list()
          }
          
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
            
            #set SSE for fit
            SSE <<- optResult$objective
            
            ##TODO: Would be nice if $fit returned a fit object that had parameter values, the constituent environments that were fit,
            # and key workspace variables as data.frames (e.g., predicted RTs, observed RTs, trial-by-trial param contributions, etc.
            
            ##only works for subject and run fits, not group
            if (class(clockData) == "clockSubject") {
              ntrials <- sum(unlist(lapply(clockData$runs, function(r) { r$w$ntrials } )))
              RTobs <- do.call(rbind, lapply(clockData$runs, function(r) { r$RTobs }))
              RTpred <- do.call(rbind, lapply(clockData$runs, function(r) { r$w$RTpred }))
              Reward <- do.call(rbind, lapply(clockData$runs, function(r) { r$Reward }))
            } else if (class(clockData) == "clockRun") {
              ntrials <- clockData$w$ntrials
              RTobs <- clockData$RTobs
              RTpred <- clockData$w$RTpred
              Reward <- clockData$Reward
            }
            
            nparams <- length(optResult$par) #number of free parameters
            
            AIC <<- ntrials*(log(2*pi*(SSE/ntrials))+1) + 2*nparams
            
            fitResult <- clockFit(RTobs=RTobs, RTpred=RTpred, Reward=Reward, total_SSE=SSE, AIC=AIC, 
                elapsed_time=unclass(elapsed_time), opt_data=optResult, profile_data=prof, theta=as.matrix(list_params()))
          } else {
            warning("Optimization failed.")
            fitResult <- NULL
          }
          
          return(fitResult)
          
        },
        
        predict=function(theta=get_param_current_vector(), updateFields=FALSE, trackHistory=FALSE) {
          ##TODO: clock_model object shoud allow for some sort of symbolic specification of how
          #values from prior runs are carried forward.
          #this is an clock_model-level decision, not subject, run, parameter, etc.
          
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
              run_SSE <- predictRun(theta=theta, clockRun=clockData$runs[[r]], prior_w=prior_w, updateFields=updateFields)
              clockData$runs[[r]]$w$run_SSE <- run_SSE
              totalSSE <- totalSSE + run_SSE
            }
            
          } else if (class(clockData)=="clockRun") {
            #pass forward theta (likely from optim)
            totalSSE <- predictRun(theta=theta, clockRun=clockData)
          }
          
          return(totalSSE)
        },
        
        predictRun=function(clockRun=NULL, prior_w=NULL, theta=get_param_current_vector(), updateFields=FALSE) { #, updateFuncs=NULL) {
          if (is.null(clockRun) || !class(clockRun) == "clockRun") { stop("predictRun requires a clockRun object") }
          
          #setup basics of workspace -- shouldn't this be in clock_model conceptually?
          clockRun$reset_workspace(prior_w)
          
          #UPDATE: Try just having a local w here -- maybe code analysis will then not stink up the place with as.environment etc.
          w <- clockRun$w
          
          #if using global RT average, set environment here
          #N.B. Need to set here before calling reset in each param because this is where bestRT[1] is set...
          #would be better to make this more robust
          if (use_global_avg_RT) { w$avg_RT <- global_avg_RT }
          
          #lapply(.self$params, function(p) { p$w <- .self$w; p$reset_workspace() }) #setup workspace variables for each parameter REFCLASS VERSION
          params <<- lapply(.self$params, function(p) { 
                p$w <- w
                reset_workspace(p)
                p$theta_lookup <- getTheta(p, clockRun$by_lookup) #define which element of theta is relevant for each parameter (based on $by and this run's condition)
                return(p)
              }) #setup workspace variables for each parameter (NEED TO UPDATE PARAMS SINCE THEY ARE PASS BY VALUE)
          
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
          
          ##TODO: Risk that we initialize some vals above (like initial V), but predict is called iteratively for optimization, so values will not be reset here if they were set in initialize
          
          #For speed, theta obj + condition lookup should occur here, outside of trial loop
          
          
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
            
            #need to pass along run condition to getRTUpdate functions
            #sapply(all_by, function(b) { clockRun[[b]]} )
            
            noiseContrib <- if (noiseWt > 0.0) { noiseWt*(runif(1,-0.5,0.5)) } else { 0.0 }
            w$RT_new <- sum(pred_contrib <- unlist(lapply(params, function(p) { getRTUpdate(p, theta) } ))) + noiseContrib
            
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
