#!/usr/bin/env Rscript

###############################################################################
# -*- encoding: UTF-8 -*-                                                     #
# Author: Jesse C. Chen (website: jessekelighine.com)                         #
# Description: Reverse Polish Notation                                        #
#                                                                             #
# Last Modified: 2023-12-14                                                   #
###############################################################################
library(shiny)
library(methods)
###############################################################################

Stack <- setRefClass("Stack",
                     fields  = list(values="vector", length="numeric"),
                     methods = list(initialize = function () {
                                      length <<- 0
                                    },
                                    push = function ( item ) {
                                      values <<- c(values, item)
                                      length <<- length + length(item)
                                      values[length]
                                    },
                                    pop = function ( n=1 ) {
                                      if ( length == 0 ) return(NULL)
                                      if ( length < n )  return(NULL)
                                      item    <- values[(length-n+1):length]
                                      length <<- length - n
                                      values <<- if ( length==0 ) vector() else values[1:length]
                                      item
                                    },
                                    top = function() values[length]
                     )
)

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
  if ( optr=="=" ) return(NULL)
  as.character(output)
}

### Main Function #############################################################

# precedence: ! = v = ~ = ^ > * = / > + = -

to.infix <- function ( command ) {
  stack.out <- Stack()
  stack.opt <- Stack()
  for ( item in parse$split(command) ) {
    if ( parse$is.opnd(item) ) {
      stack.out$push(item)
      stack.opt$push("1")
      next
    }
    if ( parse$is.optr(item) ) {
      n.opnd <- parse$optr[item]
      if ( stack.out$length < n.opnd ) return(paste("Not enough operands for", paste0('"',item,'"')))
      if ( item=="|" ) {
        out <- stack.out$pop(2)
        opt <- stack.opt$pop(2)
         stack.out$push(out[2:1])
         stack.opt$push(opt[2:1])
        next
      }
      if ( item=="=" ) {
        stack.out$pop()
        stack.opt$pop()
        next
      }
      if ( item=="!" ) {
        opt <- stack.opt$pop()
        out <- stack.out$pop()
        out <- if ( opt %in% c("1","v") ) paste0(out,"!") else paste0("(",out,")!")
        stack.out$push(out)
        stack.opt$push("!")
        next
      }
      if ( item=="~" ) {
        opt <- stack.opt$pop()
        out <- stack.out$pop()
        out <- if ( opt %in% c("1","v") ) paste0("-",out) else paste0("-(",out,")")
        stack.out$push(out)
        stack.opt$push("~")
        next
      }
      if ( item=="v" ) {
        opt <- stack.opt$pop()
        out <- stack.out$pop()
        stack.out$push(paste0("sqrt(",out,")"))
        stack.opt$push("v")
        next
      }
      if ( item=="^" ) {
        opt <- stack.opt$pop(2)
        out <- stack.out$pop(2)
        if ( ! opt[1] %in% c("1","v") ) out[1] <- paste0("(",out[1],")")
        if ( ! opt[2] %in% c("1","v") ) out[2] <- paste0("(",out[2],")")
        stack.out$push(paste0(out[1],"^",out[2]))
        stack.opt$push("^")
        next
      }
      if ( item=="/" ) {
        opt <- stack.opt$pop(2)
        out <- stack.out$pop(2)
        if ( ! opt[1] %in% c("1","!","~","v") ) out[1] <- paste0("(",out[1],")")
        if ( ! opt[2] %in% c("1","!","v") ) out[2] <- paste0("(",out[2],")")
        stack.out$push(paste(out[1],"/",out[2]))
        stack.opt$push("/")
        next
      }
      if ( item=="*" ) {
        opt <- stack.opt$pop(2)
        out <- stack.out$pop(2)
        if ( opt[1] %in% c("+","-") ) out[1] <- paste0("(",out[1],")")
        if ( opt[2] %in% c("+","-") ) out[2] <- paste0("(",out[2],")")
        stack.out$push(paste(out[1],"*",out[2]))
        stack.opt$push("*")
        next
      }
      if ( item=="+" ) {
        opt <- stack.opt$pop(2)
        out <- stack.out$pop(2)
        if ( opt[2]==1 & out[2]<0 ) out[2] <- paste0("(",out[2],")")
        stack.out$push(paste(out[1],"+",out[2]))
        stack.opt$push("+")
        next
      }
      if ( item=="-" ) {
        opt <- stack.opt$pop(2)
        out <- stack.out$pop(2)
        if ( ! opt[2] %in% c("1","!","v","^","*") ) out[2] <- paste0("(",out[2],")")
        stack.out$push(paste(out[1],"-",out[2]))
        stack.opt$push("-")
        next
      }
    }
    return(paste("Input", paste0('"',item,'"'), "is not a legal command"))
  }
  return(stack.out$values)
}

evaluate <- function ( command ) {
 stack <- Stack()
  for ( item in parse$split(command) ) {
    if ( parse$is.opnd(item) ) { stack$push(item); next }
    if ( parse$is.optr(item) ) {
      n.opnd <- parse$optr[item]
      if ( stack$length < n.opnd ) return(c("Not enough operands for", paste0('"',item,'"')))
      output <- stack$pop(n.opnd)
      output <- parse$eval(opnds=output, optr=item)
      stack$push(output)
      next
    }
    return(c("Input", paste0('"',item,'"'), "is not a legal command"))
  }
  return(stack$values)
}

### Shiny #####################################################################

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
  output$infix <- renderText({ paste(to.infix(input$command),collapse=", ") |> ( \ (x) if (x=="") "\u200B" else x )() })
  output$stack <- renderText({ paste(evaluate(input$command),collapse=" ")  |> ( \ (x) if (x=="") "\u200B" else x )() })
}

shinyApp(ui=ui, server=server)
