# fit behavioral data for all participants who completed emo clock in scanner
# setup model-based fMRI GLM analysis for based on fitted data

library(fitclock)

behavDir <- "/Volumes/bea_res/Data/Tasks/EmoClockfMRI/Basic"
fmriDir <- "/Volumes/Serena/MMClock/MR_Raw"
behavFiles <- list.files(path=behavDir, pattern="*tcExport.csv", full.names=TRUE, recursive=TRUE)

setwd("/Users/michael/CogEmoFaceReward/analysis")
if (!file.exists("fmri_fits")) { dir.create("fmri_fits") }
setwd("fmri_fits")

#start with base Frank model
#force non-negative epsilon (no sticky choice)
posEps <- clock_model()
posEps$add_params(
    meanRT(max_value=4000),
    autocorrPrevRT(),
    goForGold(),
    go(),
    noGo(),
    meanSlowFast(),
    exploreBeta()
)

library(Rniftilib) #has nice function for just reading header

for (b in behavFiles) {
  subid <- sub("^.*fMRIEmoClock_(\\d+)_tc_tcExport.csv$", "\\1", b, perl=TRUE)
  if (file.exists(file.path(subid, paste0(subid, "_fitinfo.RData")))) { next }
  #identify corresponding fmri directory
  mrmatch <- grep(paste0(subid, "_\\d+"), list.files(fmriDir, full.names=TRUE), perl=TRUE, value=TRUE)
  if(length(mrmatch) != 1L) {
    warning("Unable to find fMRI directory for subid: ", subid)
    next
  }
  if (! file.exists(file.path(mrmatch, "MBclock_recon"))) {
    warning("Unable to find preprocessed data MBclock_recon for subid: ", subid)
    next
  }
  
  #identify fmri run lengths (4th dimension)
  mrfiles <- list.files(mrmatch, pattern="nfswuktmd_clock.*_5.nii.gz", full.names=TRUE, recursive=TRUE)
  
  if (length(mrfiles) == 0L) {
    warning("Unable to find any preprocessed MB files in dir: ", mrmatch)
    next
  }
  
  runlengths <- unname(sapply(mrfiles, function(x) { nifti.image.read(x, read_data=0)$dim[4L] }))
  
  dir.create(subid, showWarnings=FALSE)
  
  s <- clockdata_subject(subject_ID=subid, dataset=b)
  
  #set data for model fit
  posEps$set_data(s)
  
  incr_fit <- posEps$incremental_fit(njobs=7)
  
  png(file.path(subid, paste0(subid, "_incrfit.png")), width=9, height=6, units="in", res=300)
  print(incr_fit$AICplot)
  dev.off()

  f <- posEps$fit(random_starts=5)
  
  #design matrix matching Badre et al. 2012 Neuron
  d <- f$build_design_matrix(regressors=c("mean_uncertainty", "rel_uncertainty", "rpe_pos", "rpe_neg", "rt"), 
       event_onsets=c("clock_onset", "clock_onset", "feedback_onset", "feedback_onset", "feedback_onset"), 
      durations=c("rt", "rt", "feedback_duration", "feedback_duration", 0), baselineCoefOrder=2, writeTimingFiles="AFNI",
      runVolumes=runlengths, output_directory=file.path(subid, "run_timing"))
 
  save(f, d, s, incr_fit, file=file.path(subid, paste0(subid, "_fitinfo.RData")))
  
}