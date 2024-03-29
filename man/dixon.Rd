\encoding{latin1}
\name{dixon}
\alias{dixon}
\title{ Dixon (2002) Nearest-neighbor contingency table analysis }
\description{
  \code{dixon} is a wrapper to the functions of Dixon (2002) to test spatial segregation for several species by analyzing the 
  counts of the nearest neighbour contingency table for a marked point pattern.
}
\usage{
dixon(datos, nsim = 99, fortran =TRUE)
}
\arguments{
  \item{datos}{ \code{data.frame} with three columns: x-coordinate, y-coordinate and sp-name.
 See \code{\link{swamp}}. }
  \item{nsim}{ number of simulations for the randomization approximation of the p-values. }
   \item{fortran}{ should the fortran implementation be used?. }
}

\value{
  A list with the following components:
  \item{ON }{Observed nearest neighbor counts in table format. From row sp to column sp.  }
  \item{EN }{Expected nearest neighbor counts in table format.}
  \item{Z }{Z-score for testing whether the observed count equals the expected count.}
  \item{S }{Segregation measure.}
  \item{pZas }{P-values based on the asymptotic normal distribution of the Z statistic.}
  \item{pNr }{If nsim !=0, p-values of the observed counts based on the randomization distribution.}
  \item{C }{Overall test of random labelling.}
  \item{Ci }{Species-specific test of random labelling.}
  \item{pCas }{P-value of the overall test from the asymptotic chi-square distribution with the appropriate degrees of freedom.}
  \item{pCias }{P-values of the species-specific tests from the asymptotic chi-square distribution with the appropriate degrees of freedom.}
  \item{pCr }{If nsim !=0, p-value of the overall test from the randomization distribution.}
  \item{pCir }{If nsim !=0, p-values of the species-specific tests from the randomization distribution.}
  \item{tablaZ }{table with ON, EN, Z, S, pZas and pNr in pretty format, as in the table II of Dixon (2002).}
  \item{tablaC }{table with C, Ci, pCas,pCias, pCr and pCir  in pretty format, as in the table IV of Dixon (2002).}
}
\details{
  
  A measure of segregation describes the tendency of one species to be  associated with itself or with other species. Dixon (2002) proposed a measure of the 
  \strong{segregation of species} \emph{i} in a multiespecies  spatial pattern as:
  
  \deqn{S[i] = log{[(N[ii]/(N[i]-N[ii])] / [(N[i]-1)/(N-N[i])]}}
  
  where \eqn{N[i]} is the number of individuals of species \emph{i}, \eqn{N[ii]} is the frequency of species \emph{i} as neighbor of especies \emph{i} and \eqn{N} is the total
  number of locations.   Values of \eqn{S[i]} larger than 0 indicate that species \emph{i} is segregated; the larger the value of \eqn{S[i]}, the more extreme the segregation. 
  Values of \eqn{S[i]} less than 0 indicate that species \emph{i} is is found as neighbor of itself less than expected under random labelling. 
  Values of \eqn{S[i]} close to 0 are consistent with random labelling of the neighbors of species \emph{i}.
  
  Dixon (2002) also proposed a \strong{pairwise segregation index} for the off-diagonal elements of the contingency table:
  
  \deqn{S[ij] = log{[(N[ij]/(N[i]-N[ij])] / [(N[i])/(N-N[j])-1]}}
  
  \eqn{S[ij]} is larger than 0 when \eqn{N[ij]}, the frequency of neighbors of species \emph{j} around points of species \emph{i}, is larger than expected under random
  labelling and less than 0 when \eqn{N[ij]} is smaller than expected under random labelling.
    
  As a \strong{species/neighbor-specific test}, Dixon(2002) proposed the statistic
  \deqn{Z[ij] =(N[ij] -EN[ij])/sqrt(Var N[ij])}
  where \emph{j} may be the same as \emph{i} and \eqn{EN[ij]} is the expected count in the contingency table. It has an asymptotic normal distribution with mean 0 
  and variance 1;  its asymptotic p-value  can be obtained from the numerical evaluation of the cumulative normal distribution; when the sample size is small, a p-value on the observed counts in each cell  (\eqn{N[ij]}) may be obtained by simulation, i.e, by condicting a randomization test. 
     
  An \strong{overall test of random labelling} (i.e. a test that all counts in the \eqn{k} x \eqn{k} nearest-neighbor contingency table are equal to their expected counts) is based
  on the quadratic form
  \deqn{ C = (N-EN)' Sigma^- (N - EN)}
where \eqn{N} is the vector of all cell counts in the contingency table, \eqn{Sigma} is the variance-covariance matrix of those counts and \eqn{Sigma^-}
is a generalized inverse of \eqn{Sigma}. Under the null hypothesis of random labelling of points, \eqn{C}  has a asymptotic Chi-square distribution with \eqn{k(k-1)} 
degrees of freedom (if the sample sizes are small its distribution should be estimated using Monte-Carlo simulation). P-values are computed from the probability of observing
equal or larger values of \eqn{C}.
The overall statistic \eqn{C} can be partitioned into \eqn{k} \strong{species-specific test} statistics \eqn{C[i]}. Each \eqn{C[i]} test if the frequencies of the neighbors 
of species \emph{i} are similar to the expected frequencies if the points were randomly labelled. Because the \eqn{C[i]} are not independent Chi-square statistics, they do not
sum to the overall \eqn{C}.

}

 \section{Warning}{
The \eqn{S[i]} and \eqn{S[ij]} statistics asume that the spatial nearest-neighbor process is stationary, at least to second order, 
  i.e., have the same sign in every part of the entire plot. A biologically heterogeneous process will violate this asumption.
  }
\references{ 
Dixon, P.M. 2002. Nearest-neighbor contingency table analysis of spatial segregation for several species. \emph{Ecoscience}, \bold{9} (2): 142-151.

\enc{De la Cruz, M. 2008.  M�todos para analizar datos puntuales. In: \emph{Introducci�n al An�lisis
  Espacial de Datos en Ecolog�a y Ciencias Ambientales: M�todos y Aplicaciones} (eds. Maestre,
  F. T., Escudero, A. and Bonet, A.), pp 76-127. Asociaci�n Espa�ola de Ecolog�a Terrestre,
  Universidad Rey Juan Carlos y Caja de Ahorros del Mediterr�neo, Madrid. ISBN:
  978-84-9849-308-5.}{De la Cruz, M. 2008.  Metodos para analizar datos puntuales. In: \emph{Introduccion al Analisis Espacial de Datos en Ecologia y Ciencias Ambientales: Metodos y Aplicaciones} (eds. Maestre,
  F. T., Escudero, A. and Bonet, A.), pp 76-127. Asociacion Espanola de Ecologia Terrestre,
  Universidad Rey Juan Carlos y Caja de Ahorros del Mediterraneo, Madrid. ISBN:
  978-84-9849-308-5.} 
}
\author{ Philip M. Dixon (Iowa State University). Marcelino de la Cruz Rot wrote the wrapper code  and the fortran implementation for this package.}
\seealso{ \code{\link[ecespa]{K012}}  in the package \pkg{ecespa} for another segregation test, based in the differences of univariate and bivariate \eqn{K}-functions.}
\note{ This function is an improvement of function \code{\link[ecespa]{dixon2002}} of the package \pkg{ecespa}. It includes also a small typo correction of the original code. }

\examples{

data(swamp)

dixon(swamp,nsim=99)


}
\keyword{ math }
\keyword{ spatial }
