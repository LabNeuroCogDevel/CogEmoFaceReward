\name{clock_model-class}
\Rdversion{1.1}
\docType{class}
\alias{clock_model-class}
\alias{clock_model}

\title{Class \code{"clock_model"}}
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
showClass("clock_model")
}
\keyword{classes}
\section{Fields}{
  \describe{
    \item{\code{params}:}{Object of class \code{list} ~~ }
    \item{\code{noiseWt}:}{Object of class \code{numeric} ~~ }
    \item{\code{SSE}:}{Object of class \code{numeric} ~~ }
    \item{\code{AIC}:}{Object of class \code{numeric} ~~ }
    \item{\code{clockData}:}{Object of class \code{ANY} ~~ }
    \item{\code{use_global_avg_RT}:}{Object of class \code{logical} ~~ }
    \item{\code{global_avg_RT}:}{Object of class \code{numeric} ~~ }
    \item{\code{all_by}:}{Object of class \code{character} ~~ }
  }
}
\section{Methods}{
  \describe{
    \item{\code{predictRun(clockRun, prior_w, theta, updateFields)}:}{ ~~ }
    \item{\code{predict(theta, updateFields, trackHistory)}:}{ ~~ }
    \item{\code{fit(toFit, random_starts, profile)}:}{ ~~ }
    \item{\code{get_param_par_scale_vector()}:}{ ~~ }
    \item{\code{get_param_initial_vector()}:}{ ~~ }
    \item{\code{get_param_maximum_vector()}:}{ ~~ }
    \item{\code{get_param_minimum_vector()}:}{ ~~ }
    \item{\code{get_param_current_vector()}:}{ ~~ }
    \item{\code{reorder_params(neworder)}:}{ ~~ }
    \item{\code{list_params()}:}{ ~~ }
    \item{\code{setup_param_by()}:}{ ~~ }
    \item{\code{set_data(cdata)}:}{ ~~ }
    \item{\code{add_params(...)}:}{ ~~ }
    \item{\code{lappend(target, append)}:}{ ~~ }
    \item{\code{set_global_avg_RT()}:}{ ~~ }
    \item{\code{initialize(clockData, ...)}:}{ ~~ }
  }
}
