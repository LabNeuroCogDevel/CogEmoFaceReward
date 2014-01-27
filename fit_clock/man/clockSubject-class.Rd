\name{clockSubject-class}
\Rdversion{1.1}
\docType{class}
\alias{clockSubject-class}
\alias{clockSubject}

\title{Class \code{"clockSubject"}}
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
showClass("clockSubject")
}
\keyword{classes}
\section{Fields}{
  \describe{
    \item{\code{subject_ID}:}{Object of class \code{character} ~~ }
    \item{\code{runs}:}{Object of class \code{list} ~~ }
    \item{\code{csv_file}:}{Object of class \code{character} ~~ }
  }
}
\section{Methods}{
  \describe{
    \item{\code{plot_runs()}:}{ ~~ }
    \item{\code{delete_runs(runs_to_delete)}:}{ ~~ }
    \item{\code{add_runs(r_objs)}:}{ ~~ }
    \item{\code{import_runs_from_csv(fname)}:}{ ~~ }
    \item{\code{initialize(subject_ID, runs, csv_file, ...)}:}{ ~~ }
  }
}
