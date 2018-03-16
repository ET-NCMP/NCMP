###################################################################################
#                                                                                 #
#    The R-NCMPs package has been developed by the ET-NCMP.                       #
#    Support.R                                                                    #
#                                                                                 #
#    This script contains functions that are common to a number of scripts        #
#    For further details please refer to the User Manual.                         #
#                                                                                 #
#    Programmers:                                                                 #
#    John Kennedy, adapting software originally written by
#      Megan Hartwell, McMaster University, Canada                                #
#      Lucie Vincent, Environment and Climate Change Canada                       #
#      December 2016                                                              #
#      Modified by Simon Grainger, Bureau of Meteorology, Australia               #
#      February 2017 - Cleaned up, code is more "R-like"                          #
#    John Kennedy June 2017
#                                                                                 #
###################################################################################

# The use of the data.table package imposes requirements on the version of
# R and data.table - test for this and advise the user
# If package data.table is missing, packageVersion() will generate an error and stop
if (getRversion() < "3.0.0") {
  warning("R Version < 3.0.0 is not supported by this code - upgrade is recommended",call.=FALSE)
}
if (packageVersion("data.table") < "1.9.6") {
  warning("Package 'data.table' Version < 1.9.6 is not tested - upgrade is recommended",call.=FALSE)
}
suppressPackageStartupMessages(library(data.table))


#Function to calulcate distance from lat and long
distance <- function (lat1,long1,lat2,long2) {
  x1 <- lat1*pi/180
  x2 <- lat2*pi/180
  y1 <- long1*pi/180
  y2 <- long2*pi/180
  d1 <- sin(x1)*sin(x2)
  d2 <- cos(x1)*cos(x2)*cos(y1-y2)
  acos(pmin(pmax(d1+d2,-1.0),1.0))*6371.009 } # acos is arccos, 6371.009 (km) is WGS84 uniform sphere


#Functions to fit variogram
#These are standard (but not all) kriging variogram functions

Gaussian <- function (x,n,r,s) { (s-n)*(1-exp(-(x^2)/(r^2)))+n }

Exponential <- function (x,n,r,s) { (s-n)*(1-exp(-x/r))+n }

Spherical <- function (x,n,r,s) { ifelse(x<=r,(s-n)*(3*x/(2*r)-x^3/(2*r^3))+n,s) }
