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

\item{height}{Heigth}

\item{width}{Width of image}

\item{api_key}{Api Key obtain from /auth}

\item{url}{URL for dev purpose}
}
\description{
This function allows you to check if the raster was downlaod before in order to avoid repeated downloading of data
}
