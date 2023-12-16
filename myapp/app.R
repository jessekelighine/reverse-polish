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

### Stack Object ##############################################################

Stack <- setRefClass("Stack", fields=list(values="vector",length="numeric"))
Stack$methods(initialize = function () length <<- 0)
Stack$methods(push = function ( item ) {
                values <<- c(values, item)
                length <<- length + length(item)
                return(values[length]) })
Stack$methods(pop = function ( n=1 ) {
                if ( length == 0 | length < n ) return(NULL)
                item    <- values[(length-n+1):length]
                length <<- length - n
                values <<- if ( length==0 ) vector() else values[1:length]
                return(item) })

### Helper Functions ##########################################################

parse         <- list()
parse$split   <- function ( command ) strsplit(command,split=" +")[[1]]
parse$optr    <- c("+"=2,"-"=2,"*"=2,"/"=2,"^"=2,"sqrt"=1,"~"=1,"!"=1,"%"=1,"&"=2,"="=1,"abs"=1)
parse$opnd    <- "^([-]{0,1}([0-9]*\\.){0,1}[0-9]+|[a-zA-Z])$"
parse$is.optr <- function ( item ) item %in% names(parse$optr)
parse$is.opnd <- function ( item ) item |> grepl(pattern=parse$opnd, x=_)
parse$eval    <- function ( opnds, optr ) {
  if ( optr=="+"    ) output <- as.numeric(opnds[1]) + as.numeric(opnds[2])
  if ( optr=="-"    ) output <- as.numeric(opnds[1]) - as.numeric(opnds[2])
  if ( optr=="*"    ) output <- as.numeric(opnds[1]) * as.numeric(opnds[2])
  if ( optr=="/"    ) output <- as.numeric(opnds[1]) / as.numeric(opnds[2])
  if ( optr=="^"    ) output <- as.numeric(opnds[1]) ^ as.numeric(opnds[2])
  if ( optr=="sqrt" ) output <- sqrt(as.numeric(opnds))
  if ( optr=="abs"  ) output <- abs(as.numeric(opnds))
  if ( optr=="~"    ) output <- -as.numeric(opnds)
  if ( optr=="!"    ) output <- factorial(as.numeric(opnds))
  if ( optr=="%"    ) output <- 1 / as.numeric(opnds)
  if ( optr=="&"    ) output <- c( as.numeric(opnds[2]), as.numeric(opnds[1]) )
  if ( optr=="=" ) return(NULL)
  as.character(output)
}

### Main Function #############################################################

to.infix <- function ( command ) {
  stack.out <- Stack()
  stack.opt <- Stack()
  for ( item in parse$split(command) ) {
    if ( parse$is.opnd(item) ) {
      stack.out$push(item)
      if ( grepl("^[a-zA-Z]$",item) ) stack.opt$push("a") else stack.opt$push("1")
      next
    }
    if ( parse$is.optr(item) ) {
      n.opnd <- parse$optr[item]
      if ( stack.out$length < n.opnd ) return(paste("ERROR: Not enough operands for", paste0('"',item,'"')))
      out <- stack.out$pop(n.opnd)
      opt <- stack.opt$pop(n.opnd)
      if ( item=="=" ) next
      if ( item=="&" ) {
        out <- out[2:1]
        opt <- opt[2:1]
      } else if ( item=="%" ) {
        out <- paste("\\frac1{",out,"}")
        opt <- item
      } else if ( item=="!" ) {
        out <- if ( opt %in% c("1","a") ) paste0(out,"!") else paste0("\\left(",out,"\\right)!")
        opt <- item
      } else if ( item=="~" ) {
        out <- if ( opt %in% c("1","a","sqrt","^") ) paste0("-",out) else paste0("-\\left(",out,"\\right)")
        opt <- item
      } else if ( item=="abs" ) {
        out <- paste0("\\left|",out,"\\right|")
        opt <- item
      } else if ( item=="sqrt" ) {
        out <- paste0("\\sqrt{",out,"}")
        opt <- item
      } else if ( item=="^" ) {
        if ( ! opt[1] %in% c("1","a","sqrt") ) out[1] <- paste0("\\left(",out[1],"\\right)")
        out <- paste0(out[1],"^{",out[2],"}")
        opt <- item
      } else if ( item=="/" ) {
        out <- paste("\\frac{",out[1],"}{",out[2],"}")
        opt <- item
      } else if ( item=="*" ) {
        if ( opt[1] %in% c("+","-") ) out[1] <- paste0("\\left(",out[1],"\\right)")
        if ( opt[2] %in% c("+","-") ) out[2] <- paste0("\\left(",out[2],"\\right)")
        out <- paste(out[1],ifelse(opt[1]=="1" & opt[2]=="1","\\cdot",""),out[2])
        opt <- item
      } else if ( item=="+" ) {
        if ( opt[2]==1 & out[2]<0 ) out[2] <- paste0("\\left(",out[2],"\\right)")
        out <- paste(out[1],"+",out[2])
        opt <- item
      } else if ( item=="-" ) {
        if ( ! opt[2] %in% c("1","a","!","sqrt","^","*") ) out[2] <- paste0("\\left(",out[2],"\\right)")
        out <- paste(out[1],"-",out[2])
        opt <- item
      } else { return("ERROR: ???") }
      stack.out$push(out)
      stack.opt$push(opt)
    } else { return(paste("ERROR: Input", paste0('"',item,'"'), "is not a legal command")) }
  }
  return(stack.out$values)
}

evaluate <- function ( command ) {
 stack <- Stack()
  for ( item in parse$split(command) ) {
    if ( parse$is.opnd(item) ) { stack$push(item); next }
    if ( parse$is.optr(item) ) {
      n.opnd <- parse$optr[item]
      if ( stack$length < n.opnd ) return(paste("ERROR: Not enough operands for", paste0('"',item,'"')))
      output <- stack$pop(n.opnd)
      output <- parse$eval(opnds=output, optr=item)
      stack$push(output)
      next
    }
    return(paste("ERROR: Input", paste0('"',item,'"'), "is not a legal command"))
  }
  return(stack$values)
}

### Shiny #####################################################################

style.sheet <- "width: auto;
max-width: 50em;
margin: 0 auto;
padding: 10px 20px;
font-family: monospace;
font-size: 10pt;
font-weight: 500;"

ui <- fluidPage(verticalLayout(div(style=style.sheet,
                                   h3(strong("RPN Calculator")),br(),
                                   textInput(inputId="command", label="Input Command"),
                                   strong("Output (Stack):"), textOutput("stack"),
                                   strong("Infix Notation:"), uiOutput("infix"),
                                   HTML(readLines("operators.html")))))

server <- function ( input, output ) {
  output$stack <- renderText({ paste(evaluate(input$command),collapse=", ") |> ( \ (x) if (x=="") "\u200B" else x )() })
  output$infix <- renderUI({
      paste(to.infix(input$command),collapse=", ") |> 
      ( \ (y) if ( y == "" ) "\u200B" else if ( grepl("ERROR",y) ) y else paste0("$$",y,"$$") )() |> 
      withMathJax()
  })
}

shinyApp(ui=ui, server=server)
