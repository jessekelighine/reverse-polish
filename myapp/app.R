#!/usr/bin/env Rscript

###############################################################################
# -*- encoding: UTF-8 -*-                                                     #
# Author: Jesse C. Chen (website: jessekelighine.com)                         #
# Description: Reverse Polish Notation                                        #
#                                                                             #
# Last Modified: 2023-12-14                                                   #
###############################################################################
library(shiny)
###############################################################################

parse         <- list()
parse$split   <- function ( command ) strsplit(command,split=" +")[[1]]
parse$optr    <- c("+"=2,"-"=2,"*"=2,"/"=2,"^"=2,"v"=1,"~"=1,"|"=2,"!"=1,"="=1)
parse$opnd    <- "^[-]{0,1}([0-9]*\\.){0,1}[0-9]+$"
parse$is.optr <- function ( item ) item %in% names(parse$optr)
parse$is.opnd <- function ( item ) item |> grepl(pattern=parse$opnd, x=_)

parse$eval    <- function ( opnds, optr ) {
  if ( optr=="+" ) output <- as.numeric(opnds[1]) + as.numeric(opnds[2])
  if ( optr=="-" ) output <- as.numeric(opnds[1]) - as.numeric(opnds[2])
  if ( optr=="*" ) output <- as.numeric(opnds[1]) * as.numeric(opnds[2])
  if ( optr=="/" ) output <- as.numeric(opnds[1]) / as.numeric(opnds[2])
  if ( optr=="^" ) output <- as.numeric(opnds[1]) ^ as.numeric(opnds[2])
  if ( optr=="v" ) output <- sqrt(as.numeric(opnds))
  if ( optr=="~" ) output <- -as.numeric(opnds)
  if ( optr=="!" ) output <- factorial(as.numeric(opnds))
  if ( optr=="|" ) output <- c( as.numeric(opnds[2]), as.numeric(opnds[1]) )
  if ( optr=="=" ) output <- NULL
  as.character(output)
}

parse$infix   <- function ( opnds, optr ) {
  if ( optr=="+" ) output <- paste("(", opnds[1], "+", opnds[2], ")", sep=" ")
  if ( optr=="-" ) output <- paste("(", opnds[1], "-", opnds[2], ")", sep=" ")
  if ( optr=="*" ) output <- paste("(", opnds[1], "*", opnds[2], ")", sep=" ")
  if ( optr=="/" ) output <- paste("(", opnds[1], "/", opnds[2], ")", sep=" ")
  if ( optr=="^" ) output <- paste("(", opnds[1], "^", opnds[2], ")", sep=" ")
  if ( optr=="v" ) output <- paste("sqrt( ", opnds, " )", sep="")
  if ( optr=="~" ) output <- paste0("( -",opnds," )")
  if ( optr=="!" ) output <- paste0("( ",opnds,"! )")
  if ( optr=="|" ) output <- c( opnds[2], opnds[1] )
  if ( optr=="=" ) output <- NULL
  output
}

###############################################################################

proto <- function ( command, fun=parse$eval ) {
  stack <- c()
  for ( item in parse$split(command) ) {
    if ( parse$is.opnd(item) ) { stack <- c(stack, item); next }
    if ( parse$is.optr(item) ) {
      n.opnd  <- parse$optr[item]
      n.stack <- length(stack)
      if ( !( n.stack >= n.opnd ) ) {
        return(c("Not enough operands in stack for", paste0('"',item,'"'), " to operate."))
      }
      output <- fun(opnds=stack[(n.stack-n.opnd+1):n.stack],optr=item)
      stack  <- if ( length(stack)==n.opnd ) c() else stack[1:(n.stack-n.opnd)]
      stack  <- c(stack, output)
      next
    }
    return(c("Input", paste0('"',item,'"'), "is not a legal command."))
  }
  return(stack)
}

evaluate <- function ( command ) proto(command, fun=parse$eval)
to.infix <- function ( command ) proto(command, fun=parse$infix)

###############################################################################

style.sheet <- "width: auto;
max-width: 45em;
margin: 0 auto;
padding: 10px 20px;
font-family: monospace;
font-size: 10pt;
font-weight: 500;"

ui <- fluidPage(verticalLayout(div(style=style.sheet,
                                   h3(strong("RPN Calculator")),br(),
                                   textInput(inputId="command", label="Input Command"),
                                   strong("Output (Stack):"), textOutput("stack"),
                                   strong("Infix Notation:"), textOutput("infix"),
                                   HTML(readLines("operators.html")))))

server <- function ( input, output ) {
  output$infix <- renderText({ paste(to.infix(input$command),collapse=" ") |> ( \ (x) if (x=="") "\u200B" else x )() })
  output$stack <- renderText({ paste(evaluate(input$command),collapse=" ") |> ( \ (x) if (x=="") "\u200B" else x )() })
}

shinyApp(ui=ui, server=server)
