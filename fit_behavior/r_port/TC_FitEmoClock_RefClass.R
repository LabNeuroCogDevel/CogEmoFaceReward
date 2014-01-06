
#Object-oriented R implementation of time clock algorithm


#dataset object is supposed to represent a group of data over which a single set of parameters (theta) are to be found
#so in the group case, get best-fitting parameters for all subjects and runs
#for a single subject, best fitting-parameters across subject's runs
#for a single run, best-fitting parameters for this run

#have predict in alg detect the class of the dataset to determine how to proceed

clockDataset <- setRefClass(
    Class="clockDataset",
    fields=list(
        subjects="list",
        dataset_name="character"
    ),
    methods=list(
        initialize=function(dataset_name=NULL, subjects=NULL) {
          if (!is.null(dataset_name)) { dataset_name <<- dataset_name }
          if (!is.null(subjects)) {
            if (!is.list(subjects) && class(subjects) == "clockSubject") {
              subjects <<- list(subjects) #single subject
            } else if (is.list(subjects) && all(sapply(subjects, class) == "clockSubject")) {
              subjects <<- subjects
            } else { stop ("subjects should be a list of clockSubject objects or a single clockSubject object")}
          }
        },
        add_subjects=function(...) {
          if (!is.null(subjects)) {
            existingSubjectIDs <- sapply(subjects, function(s) { s$subject_ID } )
          } else { existingSubjectNums <- c() }
          
          s_objs <- as.list(match.call())[-1L] #first element of match.call is a class for refMethodDef for the alg object
          
          #verify that all arguments are clockSubject objects
          if (!all(sapply(subjects, class) == "clockSubject")) { stop("All arguments to add_subjects must be clockSubject objects.") }
          
          newSubjectIDs <- sapply(s_objs, function(s) { s$subject_ID } )
          
          if (any(dupes <- newSubjectIDs %in% existingSubjectIDs)) {
            cat("Cannot add a subject with the same ID as an existing subject\n")
            stop("The following subjects conflict: ", paste(newSubjectIDs[dupes], collapse=", "))
          }
          
          subjects <<- c(subjects, s_objs) #concatenate
          #subjects <<- subjects[sort(sapply(subjects, function(s) { s$subject_ID }))] #sort in ascending order
          
        },
        delete_subjects=function(subjects_to_delete=NULL) {
          if (is.null(subjects_to_delete)) { return(invisible()) }
          if (!is.character(subjects_to_delete)) { stop("delete_subjects expects a character vector of subject IDs to delete") }
          
          subjects_to_keep <- which(! sapply(subjects, function(s) { s$subject_ID } ) %in% subjects_to_delete)
          unmatched <- which(! subjects_to_delete %in% subjects)
          
          subjects <<- subjects[subjects_to_keep]
          
          if (length(unmatched) > 0L) {
            warning("Unable to find the following subjects for deletion: ", paste0(unmatched, collapse=", "))
          }
        }

        
        )
)

#TODO: potentially automatically add runs when a csv_file is provided?
clockSubject <- setRefClass(
    Class="clockSubject",
    fields=list(
        subject_ID="character",
        runs="list",
        csv_file="character"
    ),
    methods=list(
        initialize=function(subject_ID=NULL, runs=NULL, ...) {
          if (is.null(subject_ID)) { stop("clockSubject requires ID at initialization.") }
          if (!is.null(runs)) {
            if (!is.list(runs) && class(runs) == "clockRun") {
              runs <<- list(runs) #single run
            } else if (is.list(runs) && all(sapply(runs, class) == "clockRun")) {
              runs <<- runs
            } else { stop ("runs should be a list of clockRun objects or a single clockRun object")}
          }
          callSuper(...)
        },
        import_runs_from_csv=function(fname=NULL) {          
          if (is.null(fname)) { fname <- csv_file } #use subject data file
          stopifnot(file.exists(fname))
          #for now, assuming a relatively fixed format for these CSV files
          
          sdata <- read.csv(fname, header=TRUE)
          d_split <- split(sdata, sdata$run)
          
          for (d in d_split) {
            
            .self$add_runs(            
                clockRun(
                    run_number=d$run[1L],
                    RTobs=d$rt, 
                    Reward=d$score,
                    global_trial_number=d$trial,
                    rew_function=as.character(d$rewFunc[1L]),
                    run_condition=as.character(d$emotion[1L]),
                    orig_data_frame=d))
          }
        
        },
        #add_runs=function(...) {
        add_runs=function(r_objs) {
          if (!is.null(runs)) {
            existingRunNums <- sapply(runs, function(r) { r$run_number } )
          } else { existingRunNums <- c() }

          #this is leading to all kinds of problems here -- objects are staying as language
          #then, when evaluated, they expect the (local) data from the initialization
          #core problem is that match.call seems to always get a language expression, whereas I need an object
          #makes me think that the add_params, although working, is actually double-instantiating all objects
          #once as parameters to the function
          #once at the eval step
  
          #r_objs <- as.list(match.call())[-1L] #first element of match.call is a class for refMethodDef for the alg object
          #r_objs <- lapply(r_objs, eval) #force initialization of run objects (otherwise stays as a language expression)
          
          #verify that all arguments are clockRuns objects
          
          if (!is.list(r_objs)) {
            if (class(r_objs) != "clockRun") { stop("add_runs expects a clockRun object or a list of such objects.") }
            r_objs <- list(r_objs)
          } else if (is.list(r_objs) && !all(sapply(r_objs, class) == "clockRun")) { 
            stop("add_runs expects a clockRun object or a list of such objects.") 
          }
          
          newRunNums <- sapply(r_objs, function(r) { r$run_number } )
          
          if (any(dupes <- newRunNums %in% existingRunNums)) {
            cat("Cannot add a run with the same number as an existing run\n")
            stop("The following runs conflict: ", paste(newRunNums[dupes], collapse=", "))
          }
          
          runs <<- c(runs, r_objs) #concatenate
          runs <<- runs[sort(sapply(runs, function(r) { r$run_number }))] #sort in ascending order
          
        },
        delete_runs=function(runs_to_delete=NULL) {
          if (is.null(runs_to_delete)) { return(invisible()) }
          if (!is.numeric(runs_to_delete)) { stop("delete_runs expects a numeric vector of run numbers to delete") }
          
          runs_to_keep <- which(! sapply(runs, function(r) { r$run_number} ) %in% runs_to_delete)
          unmatched <- which(! runs_to_delete %in% runs)
          
          runs <<- runs[runs_to_keep]
          
          if (length(unmatched) > 0L) {
            warning("Unable to find the following runs for deletion: ", paste0(unmatched, collapse=", "))
          }
        },
        plot_runs=function() {
          require(ggplot2)
          
        }
    )
)

clockRun <- setRefClass(
    Class="clockRun",
    fields=list(
        w="environment",
        SSE="numeric",
        run_number="numeric", #number of this run within a multi-run session
        RTobs="numeric", #vector of observed RTs
        Reward="numeric", #vector of obtained rewards
        global_trial_number="numeric", #vector of trial numbers in the overall experiment (1..runs x trials/run)
        rew_function="character",
        run_condition="character", #optional string specifying the conditions for this run (e.g., fear faces)
        orig_data_frame="data.frame" #optional data.frame from original experiment run containing full saved data (in case there are additional variables of interest)
        ),
        methods=list(
            initialize=function(run_number=NA_integer_, RTobs=NA_integer_, Reward=NA_integer_, global_trial_number=NA_integer_,
                rew_function=NULL, run_condition=NA_character_, ...) {
              
              if (is.na(run_number[1L])) { stop("At this point, clockRun must have a run number indicating temporal order.") }
              if (is.na(RTobs[1L])) { stop("construction of clockRun requires observed reaction times (RTobs)") }
              if (is.na(Reward[1L])) { stop("construction of clockRun requires observed rewards (Reward)") }
              
              run_number <<- run_number
              RTobs <<- RTobs
              Reward <<- Reward
              
              if (is.na(global_trial_number[1L])) { 
                warning("global_trial_number not provided. Defaulting to 1..t")
                global_trial_number <<- 1:length(RTobs)
              } else {
                global_trial_number <<- global_trial_number
              }
              
              if (length(unique(sapply(list(RTobs, Reward, global_trial_number), length))) > 1L) {
                stop("RTobs, Reward, and global_trial_number must have the same length")
              }
                
              if (is.null(rew_function)) { stop("Construction of clockRun requires rew_function") }
              rew_function <<- rew_function
              run_condition <<- run_condition
              
              reset_workspace() #initial setup of workspace
              
              callSuper(...) #assign unmatched args
              
            },
            reset_workspace=function(prior_w) {
              #initialize shared workspace
              #clear out old values if refitting
              w <<- new.env()#parent = emptyenv()) #make sure we do not accidentally inherit objects from .Globalenv as parent 
              
              w$ntrials <<- length(Reward)
              w$RTobs   <<- RTobs #initialize fields
              w$Reward  <<- Reward
              w$RTpred  <<- rep(NA_real_, w$ntrials) #initialize empty predicted RT
              w$RTpred[1L] <<- w$RTobs[1L] #cannot predict first trial behavior per se, so use observed RT so that t=1 doesn't contribute to SSE
              w$rpe     <<- rep(NA_real_, w$ntrials) #vector of reward prediction errors
              w$V       <<- rep(NA_real_, w$ntrials) #expected value vector
              if (!missing(prior_w) && !is.null(prior_w) && is.environment(prior_w)) {
                w$V[1L]   <<- prior_w$V[length(prior_w$V)] #carry forward expected value from last trial
                cat("using priorV: ", prior_w$V[length(prior_w$V)], "\n") #carry forward expected value from last trial
              } else {
                cat("using 0 V\n")
                w$V[1L]   <<- 0 #no assumption on prior expected value
              }
              
              
              w$avg_RT  <<- mean(RTobs, na.rm=TRUE)
              w$alphaV  <<- 0.1 # learning rate for critic (V).  Just set this to avoid degeneracy
 
            },
            plot_RTs=function() {
              require(ggplot2)
              stopifnot(exists(w$RTobs))
              if (exists(w$RTpred)) {
                rtDf <- data.frame(run_condition=run_condition, rew_function=rew_function,
                    rt=c(w$RTobs, w$RTpred), rt_type=rep(c("observed", "predicted"), each=length(w$RTobs))
                )
                rtPlot <- ggplot(rtDf, aes(x=trial, y=rt, color=rt_type))
              } else {
                message("Predicted RTs not found. Plotting only observed RTs.")
                rtDf <- data.frame(run_condition=run_condition, rew_function=rew_function, trial=1:length(w$RTobs),
                    rt=w$RTobs, rt_type=rep("observed", length(w$RTobs))
                )
                rtPlot <- ggplot(rtDf, aes(x=trial, y=rt))
              }
              
              rtPlot <- rtPlot + geom_line(size=1.1) + ggtitle(paste(rtDf$rew_function, rtDf$run_condition, sep=", ")) +
                  theme_bw(base_size=14)
              
              print(rtPlot)
              return(invisible(rtPlot))
              
            }
            )
    )

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
        w="environment" #a bit hacky (improve later?), but just copy the clockRun workspace to alg for working calculations
    ),
    methods=list(
        initialize=function(clockData=NULL, ...) {
          cat("Initializing alg\n")
          if (!is.null(clockData) && !class(clockData) %in% c("clockDataset", "clockSubject", "clockRun")) {
            stop("data for alg object must be one of the following types: clockDataset, clockSubject, or clockRun")
          }
          
          #should probably make this a member of this class, not in workspace
          
          
          params <<- list() #initialize empty list of model parameters
          noiseWt <<- 0 #do not add noise to RT prediction
          
          w <<- emptyenv() #shared workspace is abstracted to level of clockRun, copied here at fit-time for convenience
          
          callSuper(...) #for classes inheriting from this, pass through unmatched iniitalization values
        },
        lappend=function(target, append) {
          if (!is.list(target)) stop("target is not a list.")
          if (!is.list(append)) stop("append is not a list.")
          
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
              cur_value=get_param_current_vector()
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
        
        get_param_current_vector=function() {
          #corral a named vector of parameters for optim
          cur_p_vec <- sapply(params, function(p) {
                return(p$cur_value)
              })
          names(cur_p_vec) <- sapply(params, function(p) { return(p$name) }) #use internal param labels, not names provided by user
          return(cur_p_vec)
        },
        
        get_param_minimum_vector=function() {
          #corral a named vector of parameters for optim
          min_p_vec <- sapply(params, function(p) {
                return(p$min_value)
              })
          names(min_p_vec) <- sapply(params, function(p) { return(p$name) }) #use internal param labels, not names provided by user
          return(min_p_vec)					
        },
        
        get_param_maximum_vector=function() {
          #corral a named vector of parameters for optim
          max_p_vec <- sapply(params, function(p) {
                return(p$max_value)
              })
          names(max_p_vec) <- sapply(params, function(p) { return(p$name) }) #use internal param labels, not names provided by user
          return(max_p_vec)					
        },
        
        get_param_initial_vector=function() {
          #corral a named vector of parameters for optim
          init_p_vec <- sapply(params, function(p) {
                return(p$init_value)
              })
          names(init_p_vec) <- sapply(params, function(p) { return(p$name) }) #use internal param labels, not names provided by user
          return(init_p_vec)
        },
        
        fit=function(toFit=NULL, initialValues=get_param_initial_vector(),
            lower=get_param_minimum_vector(),
            upper=get_param_maximum_vector()) {
          
          
          require(optimx)
          
          #can pass in new dataset at $fit call
          #if not passed in, use the current clockData field
          if (!is.null(toFit)) {
            if (!class(toFit) %in% c("clockDataset", "clockSubject", "clockRun")) { stop("data for alg object must be one of the following types: clockDataset, clockSubject, or clockRun")
            } else { clockData <<- toFit }
          }
            
          #call predict using optimizer
          if (length(initialValues) == 1L) { method="Brent"
          } else { method="L-BFGS-B" }
          
#          if (length(initialValues) == 1L) { method="Brent"
#          } else { method="Rvmmin" } #bobyqa
          
          optResult <- optim(par=initialValues, fn=.self$predict, method=method,
              lower=lower, upper=upper, updateFields=TRUE)
  
          #p <- nlminb(start=initialValues, objective=.self$predict, lower=lower, upper=upper)
          #scalecheck()
          #browser()
  #        time <- system.time(p <- optimx(par=initialValues, fn=.self$predict, lower=lower, upper=upper, method=method, control=list(parscale=c(1e3, 1e0, 1e-1, 1e-1))))
          #time <- system.time(p <- optimx(par=initialValues, fn=.self$predictNoLookup, lower=lower, upper=upper, method=method, updateFields=FALSE))
          
          if (optResult$convergence == 0) {
            #success
            lapply(params, function(p) { p$cur_value <- optResult$par[p$name] }) #set current parameter values based on optimization
          } else {
            warning("Optimization failed.")
          }
  
          return(list(optResult, time))
          #optimize(.self$predict, interval=c(100, 4000))
  
        },
        
        predictNoLookup=function(theta=get_param_current_vector(), updateFields=FALSE) {
          if (class(clockData) == "clockSubject") {
            runRTs <- lapply(clockData$runs, "[[", "RTobs")
            runRews <- lapply(clockData$runs, "[[", "Reward")
            resetFuncs <- lapply(.self$params, function(p) { p$reset_workspace } )
            updateFuncs <- lapply(.self$params, function(p) { p$getRTUpdate } )
          } else if (class(clockData) == "clockRun") {
            
          }
        },
        predict=function(theta=get_param_current_vector(), updateFields=FALSE) {
          #TODO: alg object shoud allow for some sort of symbolic specification of how
          #values from prior runs are carried forward.
          #this is an alg-level decision, not subject, run, parameter, etc.
          require(foreach)
  
          totalSSE <- 0
          
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
              totalSSE <- totalSSE + predictRun(theta=theta, clockRun=clockData$runs[[r]], prior_w=prior_w, updateFields=updateFields) 
            }
            
          } else if (class(clockData)=="clockRun") {
            #pass forward theta (likely from optim)
            totalSSE <- predictRun(theta=theta, clockRun=clockData)
          }
          
          return(totalSSE)
        },
        
        ##TODO: Consider whether we want to have a "saveValues" T/F parameter here
        #during the function minimization, there's no need to save trial-by-trial estimates of the parameters
        #since the parameter values are not final yet. Could be faster not to save pred_contrib and trialwise_value, among others
        predictRun=function(clockRun=NULL, prior_w=NULL, theta=get_param_current_vector(), updateFields=FALSE) {
          if (is.null(clockRun) || !class(clockRun) == "clockRun") { stop("predictRun requires a clockRun object") }
          
          #setup basics of workspace -- shouldn't this be in alg conceptually?
          clockRun$reset_workspace(prior_w)
          
          w <<- clockRun$w #should now copy w to alg? and set to emptyenv() at the end?
          
          lapply(.self$params, function(p) { p$w <- .self$w; p$reset_workspace() }) #setup workspace variables for each parameter
          
          #optim using Brent, or optimize fail to pass names of parameters into predict.
          #names are critical for the params to lookup properly.
          if (is.null(names(theta))) {
            #warning("Unnamed theta vector passed to predict. Looking up from parameter list.")
            names(theta) <- sapply(params, function(p) { return(p$name) })
          }
          
          #reset workspace before proceeding!
          
          w$RT_new  <<- w$RTobs[1L] #use observed RT as predicted RT for t=1
          w$RT_last <<- w$RTobs[1L]
          w$RT_last2 <<- w$RTobs[1L]
          
          #TODO: Risk that we initialize some vals above (like initial V), but predict is called iteratively for optimization, so values will not be reset here if they were set in initialize
          
          for (t in 2:w$ntrials) {
            w$cur_trial <<- t
            w$lastTrial <<- t - 1
            
            evalq(
                {
                  Rew_last <- Reward[lastTrial]
                  RT_last  <- RTobs[lastTrial]
                  if (cur_trial > 2) { RT_last2 <- RTobs[cur_trial - 2] }
                  
                  V_last <- V[lastTrial];
                  V_new = V_last + alphaV*(Rew_last - V_last) # update critic expected value
                  V[cur_trial] <- V_new
                }, w)
            
            ##TODO: figure out how to setup list of RT update functions
            ##seems like this may need to be superordinate from here.
#            if (!updateFields) {
#              rtUpdateFuncs <- lapply(params, "[[", "getRTUpdate")
#              #then sum(lapply(rtUpdateFuncs)) roughly
#            }
            
            w$RT_new <<- sum(sapply(params, function(p) { p$getRTUpdate(theta, updateFields=updateFields) } )) + noiseWt*(runif(1,-0.5,0.5)) #add or subtract noise according to noiseWt (0 for now)
            w$RTpred[w$cur_trial] <<- w$RT_new        
            
            #TODO: incorporate this code from matlab. Affects beta dist local_RT and autocorrelation parameters 
            #if RT_last==0, RT_last = RT_last2; end; %% if last trial there
            #was no response, use trial before that for updating RT avg and autocorrelation effects (otherwise counted as 0)
            
            w$rpe[t-1] <<- w$Rew_last - w$V_last
            
          }
          
          SSE <<- sum((w$RTobs - w$RTpred)^2) #sum of squared error: cost
          cat("SSE: ", SSE, "\n")
          w <<- emptyenv() #alg should never have a persistent workspace
          return(SSE)
        })

)

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


##maybe have a version for fitting/minimization
#then a version that is used for the final predict at optimal theta
##

#generic parameter class
#should expose:
#  - min_value:  minimum parameter value in optimization
#  - max_value:  maximum parameter value
#  - init_value: initial parameter value
#  - cur_value:  current parameter value
#
#  - getRTUpdate: compute contribution to RT prediction for this parameter
param <- setRefClass(
    Class="param",
    fields=list(
        name="character",
        min_value="numeric",
        max_value="numeric",
        init_value="numeric",
        cur_value="numeric",
        par_scale="numeric",
        w="environment",
        pred_contrib="numeric", #trial-wise contribution to RT pred
        trialwise_value="numeric" #trial-wise estimate of the parameter
    ),
    methods=list(
        initialize=function(min_value=-Inf, max_value=Inf, init_value=0, cur_value=0, par_scale=1e0, ...) {
          w <<- emptyenv() #workspace should never be unique to parameter, but is set upstream by alg
          
          #general sanity checks and field assignmend handled here so subordinate classes need not duplicate
          stopifnot(cur_value <= max_value)
          stopifnot(cur_value >= min_value)
          stopifnot(init_value <= max_value)
          stopifnot(init_value >= min_value)
          
          min_value <<- min_value
          max_value <<- max_value
          init_value <<- init_value
          cur_value <<- cur_value
          par_scale <<- par_scale
          
          callSuper(...) #pass along any unmatched parameters for assignment          
        },
        reset_workspace=function(...) {
          #function that is called after object initialization
          #and after the shared workspace (w) address is passed to parameter.
          pred_contrib <<- rep(NA_real_, w$ntrials) #vector of this parameter's contribution to predicted RT
          trialwise_value <<- rep(NA_real_, w$ntrials) #vector of parameter value at each trial
        },
        #default to NULL RT (should be overloaded by sub-class)
        getRTUpdate=function() {
          return(NULL)
        }
    )

)

##K: mean reaction time
meanRT <- setRefClass(
    Class="p_K",
    contains="param",
    fields=list(),
    methods=list(
        initialize=function(min_value=100, max_value=5000, init_value=1000, cur_value=init_value, par_scale=1e3, ...) {
          name <<- "K"
          callSuper(min_value, max_value, init_value, cur_value, par_scale, ...) #call upstream constructor to initialize fields
        },
        getRTUpdate=function(theta, updateFields=FALSE) {
          rtContrib <- theta[name] #for baseline RT, the parameter itself is the speed in ms
          
          if (updateFields) {
            cur_value <<- rtContrib #update current value in param object
            trialwise_value[w$cur_trial] <<- rtContrib #save estimate of parameter at this trial
            pred_contrib[w$cur_trial] <<- rtContrib  
          }
          return(rtContrib)
        }
    )
)

##lambda: RT autocorrelated with t-1
autocorrPrevRT <- setRefClass(
    Class="p_autocorrPrevRT",
    contains="param",
    fields=list(),
    methods=list(
        initialize=function(min_value=0.0, max_value=1.0, init_value=0.3, cur_value=init_value, par_scale=1e-1, ...) {
          name <<- "lambda"
          
          if (min_value < 0.0) { stop("Autocorr prev RT (lambda) parameter cannot be negative") }
          if (max_value > 1.0) { stop("Autocorr prev RT (lambda) parameter cannot exceed 1.0") }
          
          callSuper(min_value, max_value, init_value, cur_value, par_scale, ...) #call upstream constructor to initialize fields
        },
        getRTUpdate=function(theta, updateFields=FALSE) {          
          rtContrib <- theta[name]*w$RT_last
          if (updateFields) {
            cur_value <<- theta[name] #update current value based on optimization
            trialwise_value[w$cur_trial] <<- cur_value #save estimate of parameter at this trial
            pred_contrib[w$cur_trial] <<- rtContrib  
          }
          return(rtContrib)
        }
    )
)


##Go: speed up of RT for PPE
go <- setRefClass(
    Class="p_go",
    contains="param",
    fields=list(), #Go="numeric"), #put Go vector into w
    methods=list(
        initialize=function(min_value=0.01, max_value=5.0, init_value=0.2, cur_value=init_value, par_scale=1e-1, ...) {
          if (min_value < 0.01) { stop("alphaG min_value must be at least 0.01") }
          if (max_value > 5.0) { stop("alphaG max_value must be less than 5.0") }
          name <<- "alphaG"
          callSuper(min_value, max_value, init_value, cur_value, par_scale, ...) #initialize upstream
        },
        reset_workspace=function(...) {
          w$Go <<- rep(NA_real_, w$ntrials) #vector of Go term
          w$Go[1L] <<- 0.0 #may want to override this later...
          callSuper(...) #setup pred_contrib upstream
        },
        getRTUpdate=function(theta, updateFields=FALSE) {
          #a bit of a hack here to copy the alphaG learning rate into w for easier code below
          w$cur_value <<- theta[name]
          evalq(
              {
                Go_last   <- Go[lastTrial]
                
                #carry forward Go term unless updated below by PPE
                Go_new <- Go_last
                
                #if obtained reward was better than expected (PPE), speed up (scaled by alphaG)
                if (Rew_last > V_last) {                  
                  Go_new <- Go_last + cur_value*(Rew_last - V_last)
                }
                
                Go[cur_trial] = Go_new                
              },
              w
          )
          rm(cur_value, envir=w)
          
          rtContrib <- -1.0*w$Go_new
          
          if (updateFields) {
            cur_value <<- theta[name] #update current value based on optimization
            pred_contrib[w$cur_trial] <<- rtContrib 
            trialwise_value[w$cur_trial] <<- cur_value #save estimate of parameter at this trial
          }
          return(rtContrib)
        }
    )
)

##NoGo: slow down of RT for NPE
noGo <- setRefClass(
    Class="p_nogo",
    contains="param",
    fields=list(),
    methods=list(
        initialize=function(min_value=0.01, max_value=5.0, init_value=0.2, cur_value=init_value, par_scale=1e-1, ...) {
          if (min_value < 0.01) { stop("alphaN min_value must be at least 0.01") }
          if (max_value > 5.0) { stop("alphaN max_value must be less than 5.0") }
          name <<- "alphaN"
          callSuper(min_value, max_value, init_value, cur_value, par_scale, ...) #initialize upstream
        },
        reset_workspace=function(...) {
          w$NoGo <<- rep(NA_real_, w$ntrials) #vector of NoGo term
          w$NoGo[1L] <<- 0.0 #may want to override this later...
          callSuper(...) #setup pred_contrib upstream
        },
        getRTUpdate=function(theta, updateFields=FALSE) {
          #a bit of a hack here to copy the alphaN learning rate into w for easier code below
          w$cur_value <<- theta[name]
          evalq(
              {
                NoGo_last   <- NoGo[lastTrial]
                
                #carry forward NoGo term unless updated below by NPE
                NoGo_new <- NoGo_last
                
                #if obtained reward was worse than expected (NPE), slow down (scaled by alphaN)
                if (Rew_last <= V_last) {                  
                  NoGo_new <- NoGo_last + cur_value*(V_last - Rew_last)
                }
                
                NoGo[cur_trial] = NoGo_new                
              },
              w
          )
          rm(cur_value, envir=w)
          
          rtContrib <- +1.0*w$NoGo_new
          
          if (updateFields) {
            cur_value <<- theta[name] #update current value based on optimization
            pred_contrib[w$cur_trial] <<- rtContrib 
            trialwise_value[w$cur_trial] <<- cur_value #save estimate of parameter at this trial 
          }
          return(rtContrib) 
        }
    )
)


##scale: go for the gold
goForGold <- setRefClass(
    Class="p_gold",
    contains="param",
    fields=list(bestRT_t1="numeric"),
    methods=list(
        initialize=function(min_value=0, max_value=5000, init_value=0.1, cur_value=init_value, par_scale=1e-1, ...) {
          #if user wants to override default behavior for expected bestRT on first trial, must pass in a bestRT_t1 value at initialize
          name <<- "scale"
          if (min_value < 0) { stop("Go for gold (nu) parameter cannot be negative") }
          callSuper(min_value, max_value, init_value, cur_value, par_scale, ...) #call upstream constructor to initialize fields
        },
        reset_workspace=function(...) {
          #After shared workspace (w) address is passed in, setup expectation on bestRT for first trial
          w$bestRT  <<- rep(NA_real_, w$ntrials) #initialize empty bestRT vector
          if (identical(bestRT_t1, numeric(0))) {
            #default to average RT
            #message("Using average reaction time across block as best RT prior.")
            w$bestRT[1L] <<- w$avg_RT
          } else {
            #use the user-specified value for the bestRT on t=1
            w$bestRT[1L] <<- bestRT_t1
          }
          callSuper(...) #setup pred_contrib using upstream method
        },
        getRTUpdate=function(theta, updateFields=FALSE) {
          
          #compute maximum reward and reward variability up to current trial
          #evaluate update within shared workspace
          evalq(
              { 
                rew_max <- max(Reward[1:lastTrial]) # max reward received in block thus far -- used for updating best RT
                rew_sd  <- ifelse(lastTrial > 1, sd(Reward[1:lastTrial]), 0) # sd of rewards in block thus far (use a value of 0 if just one trial)
                # If PPE on prior trial and obtained reward falls within one SD of max, save as bestRT
                #N.B. This works magically well in the test case
                #was trying to see whether the PPE aspect here is necessary
                #if (Rew_last >= (rew_max - rew_sd)) {
                if (Rew_last > V_last && Rew_last >= (rew_max - rew_sd)) {
                  bestRT[cur_trial] <- RT_last
                } else {
                  bestRT[cur_trial] <- bestRT[lastTrial] #carry forward best so far
                }
              },
              w
          )

          rtContrib <- theta[name]*with(w, bestRT[cur_trial] - avg_RT)

          if (updateFields) {
            cur_value <<- theta[name] #update current value based on optimization
            trialwise_value[w$cur_trial] <<- cur_value #save estimate of parameter at this trial
            pred_contrib[w$cur_trial] <<- rtContrib
          }
          
          return(rtContrib)          
        }
    
    )
)

###NEED TO USE RETURN VALUE FROM FIT FUNCTION TO SET CURRENT VALUES
###NEVER UPDATE CURRENT VALUES DURING FIT?

#rho and epsilon parameters both depend on tracking two beta
#distributions for fast and slow responses.
#rho scales with the difference in mean expected payoff for fast versus slow responses
#epsilon scales with difference in uncertainty (SD) for fast versus slow responses
#abstract the beta tracking to a shared class
#and the means vs SDs lookup is extrapolated to the explore and meandiff classes
betaFastSlow <- setRefClass(
    Class="betaFastSlow",
    fields=list(w="environment",
        alpha_slow="numeric", #alpha shape parameter for slow (above average) RTs
        beta_slow="numeric", #beta shape parameter for slow RTs
        alpha_fast="numeric",
        beta_fast="numeric",
        decay="numeric",
        mean_fast="numeric", #mean of fast beta dist
        mean_slow="numeric",
        mode_fast="numeric", #mode of fast beta dist
        mode_slow="numeric",
        var_fast="numeric",
        var_slow="numeric",
        mean_fast_last="numeric", #mean of fast beta dist on previous trial
        mean_slow_last="numeric",
        var_fast_last="numeric",
        var_slow_last="numeric",
        explore_last="numeric",
        explore="numeric",
        lastUpdateTrial="numeric",
        local_RT="numeric",
        local_RT_last="numeric",
        local_RT_learning_rate="numeric" #TODO: Does the learning rate for RT need to be yoked to alphaV for the critic update, as in MF's code?
    ),
    methods=list(
        initialize=function(w=NULL, alpha_slow=1.01, beta_slow=1.01, alpha_fast=1.01, beta_fast=1.01, decay=1.0, lastUpdateTrial=1L, local_RT_learning_rate=0.1, ...) {
          stopifnot(decay <= 1.0)
          if (is.null(w) || !is.environment(w)) { stop ("Shared workspace w must be passed at initialization into betaFastSlow") }
          w <<- w #setup shared workspace
          
          #initialize shape parameters for fast and slow RT beta distributions
          alpha_slow <<- alpha_slow
          beta_slow <<- beta_slow
          alpha_fast <<- alpha_fast 
          beta_fast  <<- beta_fast
          decay <<- decay
          lastUpdateTrial <<- lastUpdateTrial
          #local_RT <<- rep(NA_real_, w$ntrials) #model for fast/slow responses tracks recent RT average using alphaV learning rate
          #not using a vector for now
          local_RT[1L] <<- local_RT_last[1L] <<- w$avg_RT #set initial local average to the block mean RT
          local_RT_learning_rate <<- local_RT_learning_rate #rate at which estimate of recent RTs is updated
          
          callSuper(...)
        },
        reset_workspace=function(...) {
        },
        updateBetaDists=function() {
          #because explore and meandiff parameters may both be present in the model
          #need to check whether the beta distribution has already been updated on this trial
          #if so, do not update again
          if (lastUpdateTrial == w$cur_trial) { return(invisible(NULL)) }
          
          lastUpdateTrial <<- w$cur_trial #update the trial count for beta dist tracking
          
          #model tracks two distributions, one for fast responses (less than mean RT)
          #and one for slow responses (above mean RT)
          #here, we update the estimates of the corresponding beta distribution for slow or fast responses
          
          #cache means and variances of prior trial
          mean_fast_last <<- mean_fast
          mean_slow_last <<- mean_slow
          var_fast_last  <<- var_fast
          var_slow_last  <<- var_slow
          local_RT_last  <<- local_RT
          
          if (w$RT_last > local_RT_last) { #last response was slower than local average
            if (w$Rew_last > w$V_last) { #ppe: increment alpha shape parameter for slow dist
              alpha_slow <<- alpha_slow + 1
            } else {
              beta_slow <<- beta_slow + 1
            }
          } else if (w$RT_last <= local_RT_last) { #last response was faster than average
            if(w$Rew_last > w$V_last) {
              alpha_fast <<- alpha_fast + 1
            } else {
              beta_fast <<- beta_fast + 1
            }
          }
          
          if (decay < 1.0) {
            alpha_slow     <<- decay * alpha_slow # if decay < 1 then this decays counts, making beta dists less confident
            beta_slow      <<- decay * beta_slow
            alpha_fast     <<- decay * alpha_fast
            beta_fast      <<- decay * beta_fast
          }
          
          # compute mode and variances of beta distribution
          var_fast    <<- alpha_fast * beta_fast / ( (alpha_fast + beta_fast)^2 * (alpha_fast + beta_fast + 1) )
          var_slow    <<- alpha_slow*beta_slow/( (alpha_slow + beta_slow)^2 * (alpha_slow + beta_slow + 1) )
          mode_slow   <<- (alpha_slow - 1) / (alpha_slow + beta_slow - 2)
          mode_fast   <<- (alpha_fast - 1) / (alpha_fast + beta_fast - 2)
          mean_slow   <<- alpha_slow / (alpha_slow + beta_slow)
          mean_fast   <<- alpha_fast / (alpha_fast + beta_fast)
          
          local_RT <<- local_RT_last + local_RT_learning_rate * (w$RT_last - local_RT_last) # update estimate of recent RTs by 10% (0.1) of deviation of this trial's RT from the local average
          
        }
    )

)

##meandiff (rho)
meanSlowFast <- setRefClass(
    Class="p_meanSlowFast",
    contains="param",
    fields=list(),
    methods=list(
        initialize=function(min_value=0, max_value=10000, init_value=300, cur_value=init_value, ...) {
          name <<- "rho"
          if (min_value < 0) { stop("Slow versus fast mean parameter (rho) cannot be negative") }
          callSuper(min_value, max_value, init_value, cur_value, ...) #call upstream constructor to initialize fields
        },
        reset_workspace=function(...) {
          if (!exists("betaFastSlow", envir=w, inherits=FALSE)) { w$betaFastSlow <<- betaFastSlow(w) }
          
          callSuper(...)
        },
        getRTUpdate=function(theta) {          
          cur_value <<- theta[name] #update current value based on optimization
          w$betaFastSlow$updateBetaDists() #update fast/slow beta dists
          
          trialwise_value[w$cur_trial] <<- cur_value #save estimate of parameter at this trial
          pred_contrib[w$cur_trial] <<- cur_value * with(w$betaFastSlow, mean_slow - mean_fast) 
          return(pred_contrib[w$cur_trial])
        }
    )
)


#strategic explore parameter using beta distribution for counts of prediction errors
exploreBeta <- setRefClass(
    Class="p_epsilonBeta",
    contains="param",
    fields=list(),
    methods=list(
        initialize=function(min_value=0, max_value=100000, init_value=2000, cur_value=init_value, ...) {          
          name <<- "epsilonBeta"
          callSuper(min_value, max_value, init_value, cur_value, ...) #call upstream constructor to initialize fields
        },
        reset_workspace=function(...) {
          if (!exists("betaFastSlow", envir=w, inherits=FALSE)) { w$betaFastSlow <<- betaFastSlow(w) }
          
          callSuper(...)
        },
        getRTUpdate=function(theta) {
          #model tracks two distributions, one for fast responses (less than mean RT)
          #and one for slow responses (above mean RT)
          #here, we update the estimates of the corresponding beta distribution for slow or fast responses
          
          cur_value <<- theta[name] #update current value based on optimization
          
          w$explore_last <<- w$explore #cache prior explore product prior to beta update
          w$betaFastSlow$updateBetaDists() #update fast/slow beta dists
          
          w$cur_value <<- cur_value
          evalq(
              {                
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
              },
              w
          )
          rm(cur_value, envir=w)
         
          trialwise_value[w$cur_trial] <<- cur_value #save estimate of parameter at this trial
          pred_contrib[w$cur_trial] <<- w$explore 
          return(pred_contrib[w$cur_trial])
        }
    
    )
)

