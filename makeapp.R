#!/usr/bin/env Rscript

library(shiny)
library(shinylive)
shinylive::export("myapp", "docs")
# httpuv::runStaticServer("docs")
