% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/entry.R
\name{runCromwellDashboard}
\alias{runCromwellDashboard}
\title{Start a dashboard for a running cromwell server}
\usage{
runCromwellDashboard(url = "127.0.0.1:8000", version = "v33", ...)
}
\arguments{
\item{url}{Cromwell server in the format of address:port}

\item{version}{Cromwell server version}

\item{...}{additional parameters for shinyApp()}
}
\value{
An object that represents the app
}
\description{
Start a dashboard for a running cromwell server
}
\examples{
\dontrun{
   runCromwellDashboard("127.0.0.1:8000", "v33")
}
}
