\name{METexprplot}
\alias{METexprplot}

\title{
Gene Expression Plot of Selected Gene Module with Its Eigengene and Correlated Trait Variable
}

\description{
Plot the expression data of genes in the selected gene module. The module eigengene and its correlated trait variable are also plotted in the same plot.
}

\usage{
METexprplot(MEs, trait, MEcolor, datExpr, moduleColors, ...)
}

\arguments{
  \item{MEs}{Data frame of module eigengenes. Eigengenes in columns and samples in rows.}
  \item{trait}{Data frame of phenotype trait variable(s). Phenotype trait variable(s) in column(s) and samples in rows.}
  \item{MEcolor}{Color of selected gene module. Should be an element in \code{moduleColors}.}
  \item{datExpr}{Gene expression data matrix or data frame of all genes. Genes in columns and samples in rows.}
  \item{moduleColors}{Vector of module colors of all genes in \code{datExpr}. Length should equal to number of columns in \code{datExpr}.}
  \item{...}{Additional arguments used in \code{\link[pheatmap]{pheatmap}}.}
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
