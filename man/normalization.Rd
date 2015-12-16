\name{normalization}
\alias{normalization}

\title{
Normalization of Data Matrix
}

\description{
Make data matrix column-wise unit-length vectors.
}

\usage{
normalization(datExpr)
}

\arguments{
  \item{datExpr}{Data matrix or data frame.}
}

%\details{
%%  ~~ If necessary, more details than the description above ~~
%}

\value{
Normalized data matrix.
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

\examples{
data(lungData)
datExpr <- lungData
datExpr.normed <- normalization(datExpr)
}

% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ GTEx }
%\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
