
<!-- README.md is generated from README.Rmd. Please edit that file -->
<!-- badges: start -->

[![tic](https://github.com/agrospace/ASAPI/workflows/tic/badge.svg?branch=master)](https://github.com/agrospace/ASAPI/actions)
<!-- badges: end -->

# ASAPI: **A**gro**S**pace **API** R Packages <img src="https://storage.googleapis.com/cdnagrospace2021/img/agro_space.png" align="right" width=155 height=120 alt="" />

-   More than 20 satellite scientific index to be use in your
    application.
-   Satellite Biophysics index
-   Ready to use!

**OpenAPI** Full documentation in
[api.agrospace.cl](api.agrospace.cl/doc)

## Installation

``` r
remotes::install_github('agrospace/ASAPI')
```

### Raster Layer

Satellite information base on pixel size of your farm.

![](https://storage.googleapis.com/cdnagrospace2021/img/raster.png)

### Vectorial Layer

![](https://storage.googleapis.com/cdnagrospace2021/img/shape.png)

The vectorial layer contains attributable geometries to by assigned as
paddocks names or similar information.

![](https://storage.googleapis.com/cdnagrospace2021/img/shape_paddocks.png)

![](https://storage.googleapis.com/cdnagrospace2021/img/NDVI_paddock.png)

### Time series

![](https://storage.googleapis.com/cdnagrospace2021/img/time-series.png)

### GIF and Dashboards

![](https://storage.googleapis.com/cdnagrospace2021/img/Index_GIF.gif)

## Code of conduct

Please note that this project is released with a [Contributor Code of
Conduct](https://api.agrospace.cl). By participating in this project you
agree to abide by its terms.

# AgroSpace R Packages to use api.agrospace.cl

## For work over new features please work in new branches

    # create new branch
    git checkout -b features
    git pull

    # push new branch to github
    git push origin features 

    # set tracking
    git branch --set-upstream-to=origin/features features

    #delete branch locally
    git branch -d features

    # delete branch remotely
    git push origin --delete features

Additional documentation

    # https://r-pkgs.org/description.html
    # https://kbroman.org/github_tutorial/
