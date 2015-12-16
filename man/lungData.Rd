\name{lungData}
\alias{lungData}
\docType{data}

\title{
Preprocessed Data from Lung Tissue
}

\description{
The preprocessed gene expression data for lung tissue. 1. Do variance-stabilizing transformation. 2. Filter out low count and small variation genes, using the function in our package \code{geneFilter(original-vst-dat, mean.thred = 5, sd.thred = 2)}.
}

\usage{data(lungData)}

\format{
\describe{
\item{\code{lungData}}{The preprocessed and filtered data. It contains 175 genes (in columns) and 133 samples (in rows). 
}
}
}

%\seealso{
%
%}

\source{
The Genotype Tissue Expression (GTEx) project: \url{http://www.gtexportal.org/home/}
}

\examples{
data(lungData)

datExpr <- lungData
nGenes <- ncol(datExpr)
nSamples <- nrow(datExpr)
}

\keyword{datasets}


