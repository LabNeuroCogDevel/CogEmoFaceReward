\name{clockRun-class}
\Rdversion{1.1}
\docType{class}
\alias{clockRun-class}
\alias{clockRun}

\title{Class \code{"clockRun"}}
\description{
%%  ~~ A concise (1-5 lines) description of what the class is. ~~
}
\section{Extends}{

All reference classes extend and inherit methods from \code{"\linkS4class{envRefClass}"}.

}
\references{
%%  ~~put references to the literature/web site here~~
}
\author{
%%  ~~who you are~~
}
\note{
%%  ~~further notes~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
%%  ~~objects to See Also as \code{\link{~~fun~~}}, ~~~
%%  ~~or \code{\linkS4class{CLASSNAME}} for links to other classes ~~~
}
\examples{
showClass("clockRun")
}
\keyword{classes}
\section{Fields}{
  \describe{
    \item{\code{w}:}{Object of class \code{environment} ~~ }
    \item{\code{SSE}:}{Object of class \code{numeric} ~~ }
    \item{\code{run_number}:}{Object of class \code{numeric} ~~ }
    \item{\code{RTobs}:}{Object of class \code{numeric} ~~ }
    \item{\code{Reward}:}{Object of class \code{numeric} ~~ }
    \item{\code{avg_RT}:}{Object of class \code{numeric} ~~ }
    \item{\code{global_trial_number}:}{Object of class \code{numeric} ~~ }
    \item{\code{rew_function}:}{Object of class \code{character} ~~ }
    \item{\code{run_condition}:}{Object of class \code{character} ~~ }
    \item{\code{by_lookup}:}{Object of class \code{character} ~~ }
    \item{\code{orig_data_frame}:}{Object of class \code{data.frame} ~~ }
  }
}
\section{Methods}{
  \describe{
    \item{\code{plot_RTs()}:}{ ~~ }
    \item{\code{reset_workspace(prior_w)}:}{ ~~ }
    \item{\code{initialize(run_number, RTobs, Reward, global_trial_number, rew_function, run_condition, ...)}:}{ ~~ }
  }
}
