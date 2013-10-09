trialLength <- 5
times <- seq(300,700,by=50)
ntrialsProps <- seq(.3, .7, by=.05)
comb <- expand.grid(t=times, p=ntrialsProps)
comb$n <- ceiling((comb$t * comb$p)/trialLength)

library(plyr)
a_ply(comb, 1, function(r) {
  system(paste0("optseq2 --ntp ", r$t, " --tr 1.0 --psdwin 0 22 1.0 --ev evt1 5 ", r$n,
                " --nkeep 5 --o clock_t", r$t, "_n", r$n, "_p", r$p,
                " --nsearch 1000 --polyfit 2 --sum summary_t", r$t, "_n", r$n, "_p", r$p, ".log"))
})


files = list.files(pattern="summary_.*\\.log")

lens <- c()
effs <- c()
resmat <- c()
for (f in files) {
  s <- scan(f, what="character", sep="\n")
  #runLength <- as.numeric(sub("summary_(\\d+)\\.log", "\\1", f, perl=TRUE))
  runLength <- as.numeric(sub("summary_t(\\d+)_.*\\.log", "\\1", f, perl=TRUE))
  ntrials <- as.numeric(sub("summary_t\\d+_n(\\d+).*\\.log", "\\1", f, perl=TRUE))
  trialprop <- as.numeric(sub("summary_t\\d+_n\\d+_p([\\d\\.]+).*\\.log", "\\1", f, perl=TRUE))
  
  maxEff <- grep("Max Eff Encountered:", s, fixed=TRUE)
  maxEffNum <- as.numeric(sub("^\\s*Max Eff Encountered:\\s+([\\d.]+).*$", "\\1", s[maxEff[1L]], perl=TRUE))
  #lens <- c(lens, runLength)
  #effs <- c(effs, maxEffNum)
  resmat <- rbind(resmat,
                  c(t=runLength, n=ntrials, p=trialprop, eff=maxEffNum))
}

resmat <- as.data.frame(resmat)
resmat$t <- factor(resmat$t)
#resmat$n <- factor(resmat$n)
resmat$p <- factor(as.character(resmat$p*100))

library(ggplot2)
pdf("powerCurves_oneEvent.pdf", width=11, height=8)
ggplot(resmat, aes(x=n, y=eff, group=1)) + geom_point() + geom_line() + facet_wrap(~t, scales="free_x") + #, color=n
  geom_text(aes(x=n, y=eff+.2, group=1, label=p)) + ggtitle("Estimation efficiency for 5-second single event.\nLabels denote %run devoted to trials\nPaneled by total run length (seconds)") +
  xlab("Number of trials") + ylab("Efficiency")
dev.off()

plot(lens, effs, main="Efficiency at differing run lengths with 40 5-second trials", type="b")
