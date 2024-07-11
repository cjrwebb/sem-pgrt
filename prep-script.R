
#' Run the following code to prepare your computer for the 
#' short course. You only need to run the code once. It may
#' take several minutes (up to an hour or half an hour 
#' depending on your internet connection speed)

#' Please read the instructions carefully and email 
#' c.j.webb@sheffield.ac.uk if you have any problems, or
#' speak to the University of Sheffield's IT Services.


# Toolchain installation --------------------------------------------------

#' Toolchain dependencies: two packages used here have some
#' additional dependencies on igraph that you may need to 
#' install on your computer. You should only need to do this
#' step *once*. However, you can also skip this step if you are
#' happy to ignore the very minimal use of ggdag and tidySEM 
#' on the course (alternative tools are also provided).

#' For *Windows users*, you may need to install Rtools for
#' ggdag and tidySEM to work (due to their igraph
#' depdendency which requires gfortran). You may have to
#' run Rstudio as an administrator by right-clicking, or
#' confirm in a pop up window that you are happy for R/Rstudio
#' to make changes to your hard drive (to install the tools)
install.packages("installr")
installr::install.Rtools()

#' For *Mac users*, you may need to install and run macrtools
#' in order for ggdag and tidySEM to work (due to their igraph
#' depdendency which requires gfortran).
install.packages("remotes")
remotes::install_github("coatless-mac/macrtools")
macrtools::macos_rtools_install()

#' For *Ubuntu / Debian users*, you may need to run the following
#' command in the terminal for ggdag and tidySEM to work properly:
#' (without the hash tag), to install the igraph.so dependency.
#' You may be prompted to enter your password.
  # sudo apt-get update && sudo apt-get install -y build-essential libcurl4-gnutls-dev libxml2-dev libssl-dev libxml2-dev libglpk-dev libnode-dev libv8-dev



# Package installation ----------------------------------------------------

#' You only need to run this code once before the course
#' starts. I recommend doing this as soon as possible so
#' any errors can be troubleshooted. You will need to be
#' connected to the internet, and will need to allow R several
#' minutes to install all of the packages. Take not of any 
#' packages that fail to install.

# Install code for packages (run in console)
install.packages('tidyverse')
install.packages('here')
install.packages('lavaan')
install.packages('dagitty')
install.packages('lavaanPlot')
install.packages('faux')
install.packages('simsem')
install.packages('ggdag')
install.packages('semTools')
install.packages('sjPlot')
install.packages('broom')
install.packages('GGally')
install.packages('tidySEM')
install.packages('patchwork')
install.packages('igraph')



# Testing that packages load ----------------------------------------------

#' You should now try loading the packages one-by-one in R
#' and ensure that they are all successfully able to load (
#' i.e. there is no "ERROR:" coming up after R loads them).
#' Warnings can be safely ignored - if you only get a warning,
#' the package is still loaded and will work.
#' 
#' If the only packages that fail to load are ggdag and 
#' tidySEM, even after running the toolchain commands above,
#' you can safely ignore this. This means there will only be
#' a couple of points in the course where you cannot follow along,
#' but alternative tools are provided that can be adapted. It 
#' will not affect your learning.


# Test whether the following packages successfully load

# Packages used for data management, visualising,
# and simulation
library(here)
library(tidyverse)
library(faux)
library(simsem)
library(broom)

# Packages used for plotting, tables etc.
library(patchwork)
library(sjPlot)
library(dagitty)
library(ggdag)
library(GGally)

# Packages used for SEM
library(lavaan)
library(semTools)
library(lavaanPlot)
library(semTools)
library(tidySEM)


# Set working directory to script location
setwd(here::here())

# test lavaan is working (no errors)
example(lavaan)


