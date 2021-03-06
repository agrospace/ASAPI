% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/api_tools.R
\name{post_rgb_plot}
\alias{post_rgb_plot}
\title{Plot RGB}
\usage{
post_rgb_plot(
  client,
  farm,
  date,
  email,
  height = 200,
  width = 200,
  api_key,
  url = "https://api.agrospace.cl"
)
}
\arguments{
\item{client}{Client name}

\item{farm}{email of user}

\item{date}{URL for dev purpose}

\item{email}{Email of custome user}

\item{api_key}{Api Key obtain from /auth}
}
\description{
This function allows you to check if the raster was downlaod before in order to avoid repeated downloading of data
}
\examples{
post_rgb_plot(client='clientexample', farm='farm1example', date = '2021-02-07', email="user.example@agrospace.cl", api_key=asapi_auth(email="user.example@agrospace.cl", password="contra1234")$api_key)

}
\keyword{and}
\keyword{in}
\keyword{keep}
\keyword{local}
\keyword{plot}
\keyword{raster}
\keyword{rgb}
