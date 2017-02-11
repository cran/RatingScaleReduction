\name{CheckAttr4Inclusion}

\alias{CheckAttr4Inclusion}

\title{Check the next attribute for possible inclusion into AUC}
\description{The next attribute is added to the running total.
The running total is used with the class (decision attribute)
to compute AUC. If the new AUC is larger that the former AUC,
the next attribute is added to the sequence of attributes.}


\usage{
CheckAttr4Inclusion(attribute, D, method=c("delong", "bootstrap",
"venkatraman", "sensitivity", "specificity"), 
alternative = c("two.sided", "less", "greater"))
}

\arguments{
  \item{attribute}{	a matrix or data.frame containing attributes}
  \item{D}{ the decision vector }
  \item{method}{the method to useas in the function roc.test\{pROC\}}
  \item{alternative}{the alternative hipothesis}
}


\value{
\item{test}{the result of the roc.test as in the function roc.test from the package pROC}}


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

#deLong test, two-side alternative hiphotesis
CheckAttr4Inclusion(attribute, decision, method=c("delong"), 
alternative=c("two.side"))

#bootstrap, two-side alternative hiphotesis
CheckAttr4Inclusion(attribute, decision, method=c("bootstrap"), 
alternative=c("two.side"))
}
