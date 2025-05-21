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
  rsconnect::setAccountInfo(
    name = 'jessekelighine',
    token = '50DBA2761FA248DB2F03C444F6451A41',
    secret = 'GUED7l42H6uNkROtGj2cZeKewe6JNZ3AkFL4tqbi'
  )
  rsconnect::setAccountInfo(name = "jessekelighine", token = "", secret = "")
  rsconnect::deployApp("reverse-polish/")
  quit()
}

stop("Specify 'shinylive' or 'shinyapps'.")
