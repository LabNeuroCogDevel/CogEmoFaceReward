\name{betaFastSlow}
\alias{betaFastSlow}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
betaFastSlow(w = NULL, alpha_slow = 1.01, beta_slow = 1.01, alpha_fast = 1.01, beta_fast = 1.01, decay = 1, lastUpdateTrial = 1L, local_RT_learning_rate = 0.1)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{w}{
%%     ~~Describe \code{w} here~~
}
  \item{alpha_slow}{
%%     ~~Describe \code{alpha_slow} here~~
}
  \item{beta_slow}{
%%     ~~Describe \code{beta_slow} here~~
}
  \item{alpha_fast}{
%%     ~~Describe \code{alpha_fast} here~~
}
  \item{beta_fast}{
%%     ~~Describe \code{beta_fast} here~~
}
  \item{decay}{
%%     ~~Describe \code{decay} here~~
}
  \item{lastUpdateTrial}{
%%     ~~Describe \code{lastUpdateTrial} here~~
}
  \item{local_RT_learning_rate}{
%%     ~~Describe \code{local_RT_learning_rate} here~~
}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
}
\references{
%% ~put references to the literature/web site here ~
}
\author{
%%  ~~who you are~~
}
\note{
%%  ~~further notes~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
##---- Should be DIRECTLY executable !! ----
##-- ==>  Define data, use random,
##--	or do  help(data=index)  for the standard data sets.

## The function is currently defined as
function (w = NULL, alpha_slow = 1.01, beta_slow = 1.01, alpha_fast = 1.01, 
    beta_fast = 1.01, decay = 1, lastUpdateTrial = 1L, local_RT_learning_rate = 0.1) 
{
    stopifnot(decay <= 1)
    if (is.null(w) || !is.environment(w)) {
        stop("Shared workspace w must be passed at initialization into betaFastSlow")
    }
    fparams <- as.list(environment())
    obj <- new.env(parent = baseenv())
    for (n in ls(fparams, all.names = TRUE)) assign(n, get(n, 
        fparams), obj)
    eval(quote({
        mean_fast = numeric(0)
        mean_slow = numeric(0)
        mode_fast = numeric(0)
        mode_slow = numeric(0)
        var_fast = numeric(0)
        var_slow = numeric(0)
        mean_fast_last = numeric(0)
        mean_slow_last = numeric(0)
        var_fast_last = numeric(0)
        var_slow_last = numeric(0)
        explore = numeric(0)
        explore_last = numeric(0)
        local_RT = numeric(0)
        local_RT_last = numeric(0)
    }), obj)
    obj$local_RT[1L] <- obj$local_RT_last[1L] <- w$avg_RT
    return(invisible(obj))
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
