\name{startAuc}
\alias{startAuc}

\title{AUC of a single attribute}

\description{
Compute AUC of every single attribute}

\usage{
startAuc(attribute, D)}

\arguments{
  \item{attribute}{	a matrix or data.frame containing attributes}
  \item{D}{ the decision vector }
}

\value{
\item{auc}{AUC of a single attribute}
\item{item}{attribute labels}
\item{summary}{a summary table}
}


\references{W.W. Koczkodaj,  T. Kakiashvili,  A. Szymanska, 
 J. Montero-Marin, R. Araya, J. Garcia-Campayo, K. Rutkowski,
 D. Strzalka,
 How to reduce the number of rating scale items without
 predictability loss? Scientometrics, 2017}
 
\author{Waldemar W. Koczkodaj, Alicja Wolny-Dominiak
}

\examples{
#creating the matrix of attributes and the decision vector
#must be as.numeric()
data(aSAH)
attach(aSAH)
is.numeric(aSAH)

attribute <-data.frame(as.numeric(gender), 
as.numeric(age), as.numeric(wfns), as.numeric(s100b), as.numeric(ndka))
colnames(attribute) <-c("a1", "a2", "a3", "a4", "a5")
decision <-as.numeric(outcome)

#compute AUC of all attributes
start <-startAuc(attribute, decision)
start$summary
}
