% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/api_connector.R
\name{asapi_list}
\alias{asapi_list}
\title{The List GET Function}
\usage{
asapi_list(client, type, api_key, url = "https://api.agrospace.cl")
}
\arguments{
\item{client}{Client name}

\item{type}{list type: farm or user}

\item{api_key}{Api Key obtain from /auth}

\item{url}{URL for dev purpose}
}
\description{
This function allows you to GET list of farms or users of a client.
Agrospace members only, with apikey master.
}
