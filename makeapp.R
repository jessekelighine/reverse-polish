#!/usr/bin/env Rscript

args <- commandArgs(trailingOnly=TRUE)

if ( length(args)!=1 ) stop("Specify 'shinylive' or 'shinyapps'.")

if ( args=="shinylive" ) {
  library(shiny)
  library(shinylive)
  shinylive::export("reverse-polish/", "docs")
  quit()
}

if ( args=="shinyapps" ) {
  library(rsconnect)
  rsconnect::deployApp("reverse-polish/")
  quit()
}

stop("Specify 'shinylive' or 'shinyapps'.")
