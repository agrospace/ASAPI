% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/api_connector.R
\name{asapi_sensor_get}
\alias{asapi_sensor_get}
\title{The Sensor GET Function}
\usage{
asapi_sensor_get(
  client,
  email,
  farm,
  api_key,
  dash_param = FALSE,
  url = "https://api.agrospace.cl"
)
}
\arguments{
\item{client}{Client name}

\item{email}{email of user}

\item{farm}{farm name}

\item{api_key}{Api Key obtain from /auth}

\item{dash_param}{AgroSpace internal use parameter. Default value FALSE}

\item{url}{URL for dev purpose}
}
\description{
This function allows you to GET Sensor information of client
}
\examples{
asapi_sensor_get(client="clientexample", email="user.example@agrospace.cl",
farm="farm1example", api_key=asapi_auth(email="user.example@agrospace.cl", password="contra1234")$api_key)

}
\keyword{API_KEY,}
\keyword{api_key}
