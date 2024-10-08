% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/eq5d.R
\name{eq5d_5l_index_vas}
\alias{eq5d_5l_index_vas}
\title{Calculate Swedish EQ5D-index based on VAS from EQ5D-5-level data}
\usage{
eq5d_5l_index_vas(mobility, selfcare, usual, pain, anxiety)
}
\arguments{
\item{mobility, selfcare, usual, pain, anxiety}{numeric
scale values for each dimension}
}
\value{
numeric vector with EQ5D-indices
}
\description{
Calculate Swedish EQ5D-index based on VAS from EQ5D-5-level data
}
\examples{
eq5d_5l_index_vas(3, 4, 5, 4, 3) # 30.5
eq5d_5l_index_vas(rep(3, 10), rep(4, 10), rep(5, 10), rep(4, 10), rep(3, 10))
}
\references{
[Burström, model 5 VAS](https://link.springer.com/article/10.1007/s40273-020-00905-7/tables/9)
}
\seealso{
Other eq5d: 
\code{\link{eq5d_3l_index_tto}()},
\code{\link{eq5d_3l_index_tto_old}()},
\code{\link{eq5d_5l_index_tto}()},
\code{\link{eq5d_index}()}
}
\concept{eq5d}
