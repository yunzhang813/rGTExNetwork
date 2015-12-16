\name{geneFilter}
\alias{geneFilter}

\title{
Filtering for Low Count and Low Variance Genes
}

\description{
Gene filtering based on the mean expression level and standard deviation for each gene across samples.
}

\usage{
geneFilter(dat, mean.thred, sd.thred)
}

\arguments{
  \item{dat}{Expression matrix with genes in columns and samples in rows.}
  \item{mean.thred}{Threshold for mean.}
  \item{sd.thred}{Threshold for standard deviation.}
}

%\details{
%%  ~~ If necessary, more details than the description above ~~
%}

\value{
Indices of genes that should be kept after the filtering.
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
dat <- t(lungData)
i.keep <- geneFilter(dat = dat, mean.thred = 6, sd.thred = 3)
newdat <- dat[i.keep,]
}

% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ GTEx }
%\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
