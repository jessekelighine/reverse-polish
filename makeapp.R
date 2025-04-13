#!/usr/bin/env Rscript

args <- commandArgs(trailingOnly = TRUE)

if (length(args) != 1) stop("Specify 'shinylive' or 'shinyapps'.")

if (args == "shinylive") {
  library(shiny)
  library(shinylive)
  shinylive::export(appdir = "reverse-polish/", destdir = "docs")
  quit()
}

if (args == "shinyapps") {
  library(rsconnect)
  # Get these information from `https://shinyapps.io`
  rsconnect::setAccountInfo(name = "", token = "", secret = "")
  rsconnect::deployApp("reverse-polish/")
  quit()
}

stop("Specify 'shinylive' or 'shinyapps'.")
