cromwellDashboard
========

  [![Build Status](https://travis-ci.org/zhanxw/cromwellDashboard.svg?branch=master)](https://travis-ci.org/zhanxw/cromwellDashboard)
![](http://cranlogs.r-pkg.org/badges/cromwellDashboard)
[![CRAN_Status_Badge](http://www.r-pkg.org/badges/version/cromwellDashboard)](https://cran.r-project.org/package=cromwellDashboard)


A Dashboard to Visualize Scientific Workflows in Cromwell


## Quick introduction ##

1. Start cromwell server in shell

java -jar cromwell-33.1.jar server

2. Start cromwellDashboard in R

runCromwellDashboard("192.168.54.28:8123")

## Installation ##

*Install the latest stable version*

  install.packages("cromwellDashboard")

*Install the development version* ([devtools](https://github.com/hadley/devtools) package is required):

  devtools::install_github("zhanxw/cromwellDashboard")

