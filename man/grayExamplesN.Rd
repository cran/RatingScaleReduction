\name{grayExamplesN}
\alias{grayExamplesN}

\title{Examples belonging to both classes for the given example number}

\description{Identical examples on all attributes for the given example number}


\usage{
grayExamplesN(attribute, D, N)
}
\arguments{
  \item{attribute}{	a matrix or data.frame containing attributes}
  \item{D}{ the decision vector }
  \item{N}{an example number}
}

\value{
\item{examp}{a list of identical examples on all atributes}
\item{examp.nr}{a vector of example numbers }
}

\author{Waldemar W. Koczkodaj, Alicja Wolny-Dominiak
}

\examples{
#generate data

a=c(); attribute=c()
for (i in 1:3){
a <-sample(c(1,2,3), 100, replace=TRUE)
attribute <-cbind(attribute, a)
attribute=data.frame(attribute)
}
colnames(attribute)=c("a1", "a2", "a3")
names(attribute)

decision=sample(c(0,1), 100, replace=TRUE)

#check examples
grayExamplesN(attribute, decision, 13)
grayExamplesN(attribute, decision, 30)

}
