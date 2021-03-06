% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/api_connector.R
\name{asapi_farm_get}
\alias{asapi_farm_get}
\title{The Farms GET Function}
\usage{
asapi_farm_get(
  client,
  farm,
  email,
  api_key,
  dash_param = FALSE,
  url = "https://api.agrospace.cl"
)
}
\arguments{
\item{client}{Client name}

\item{farm}{Farm name to query}

\item{email}{email of user}

\item{api_key}{Api Key obtain from /auth}

\item{dash_param}{AgroSpace internal use parameter. Default value FALSE}

\item{url}{URL for dev purpose}
}
\description{
This function allows you to GET Farms information with the API and retrieve your API KEY
}
\examples{
asapi_farm_get(client="clientexample", email="user.example@agrospace.cl",
farm="farm1example", api_key=asapi_auth(email="user.example@agrospace.cl", password="contra1234")$api_key)

}
\keyword{Farms}
