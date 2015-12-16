\name{METcorplot}
\alias{METcorplot}

\title{
Correlation Plot of Module Eigengenes and Clinical Trait Variables
}

\description{
Plot the correlation heatmap of module eigengenes and clinical trait variables.
}

\usage{
METcorplot(MEs, trait, ...)
}

\arguments{
  \item{MEs}{Data frame of module eigengenes. Eigengenes in columns and samples in rows.}
  \item{trait}{Data frame of clinical trait variables. Clinical trait variables in columns and samples in rows.}
  \item{...}{Additional arguments used in \code{\link[WGCNA]{plotEigengeneNetworks}}.}
}

%\details{
%%  ~~ If necessary, more details than the description above ~~
%}

\value{
Heatmap plot.
}

%\references{
%
%}

\author{
Yun Zhang
}

%\note{
%%  ~~further notes~~
%}

%% ~Make other sections like Warning with \section{Warning }{....} ~

%\seealso{
%
%}

%\examples{
%
%}

% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ GTEx }
%\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
