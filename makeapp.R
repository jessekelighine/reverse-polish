#!/usr/bin/env Rscript

# library(shiny)
# library(shinylive)
# shinylive::export("reverse-polish/", "docs")
# httpuv::runStaticServer("docs")

library(rsconnect)
rsconnect::deployApp("reverse-polish/")
