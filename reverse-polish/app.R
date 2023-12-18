#!/usr/bin/env Rscript

###############################################################################
# -*- encoding: UTF-8 -*-                                                     #
# Author: Jesse C. Chen (website: jessekelighine.com)                         #
# Description: Reverse Polish Notation                                        #
#                                                                             #
# Last Modified: 2023-12-17                                                   #
###############################################################################
library(shiny)
###############################################################################

### Stack Object ##############################################################

Stack <- setRefClass("Stack",
                     fields  = list(values="vector",length="numeric"),
                     methods = list(initialize = function () length <<- 0,
                                    push = function ( item ) {
                                      values <<- c(values, item)
                                      length <<- length + length(item)
                                      values[length]
                                    },
                                    pop = function ( n=1 ) {
                                      if ( length == 0 | length < n ) return(NULL)
                                      item    <- values[(length-n+1):length]
                                      length <<- length - n
                                      values <<- if ( length==0 ) vector() else values[1:length]
                                      item }
                     )
)

### Operators & Parsing #######################################################

operator <- list()
parse    <- list()

parse$split   <- function ( command ) strsplit(command,split=" +")[[1]]
parse$is.neg  <- function ( str  ) substr(str,start=1,stop=1)=="-"
parse$is.optr <- function ( item ) item %in% names(operator)
parse$is.opnd <- function ( item ) item |> grepl(pattern="^[-]{0,1}(([0-9]*\\.){0,1}[0-9]+|[a-zA-Z])$", x=_)
parse$is.symb <- function ( item ) item |> grepl(pattern="^[-]{0,1}[a-zA-Z]$", x=_)

operator[["+"]]       <- list()
operator[["+"]]$n     <- 2
operator[["+"]]$eval  <- function ( opnds ) as.numeric(opnds[1]) + as.numeric(opnds[2])
operator[["+"]]$infix <- function ( opt, out ) {
  if ( parse$is.neg(out[2]) ) out[2] <- paste0("\\left(",out[2],"\\right)")
  list(out=paste(out[1],"+",out[2]), opt="+")
}

operator[["-"]]       <- list()
operator[["-"]]$n     <- 2
operator[["-"]]$eval  <- function ( opnds ) as.numeric(opnds[1]) - as.numeric(opnds[2])
operator[["-"]]$infix <- function ( opt, out ) {
  if ( parse$is.neg(out[2]) | opt[2] %in% c("+","-") ) out[2] <- paste0("\\left(",out[2],"\\right)")
  list(out=paste(out[1],"-",out[2]), opt="-")
}

operator[["*"]]       <- list()
operator[["*"]]$n     <- 2
operator[["*"]]$eval  <- function ( opnds ) as.numeric(opnds[1]) * as.numeric(opnds[2])
operator[["*"]]$infix <- function ( opt, out ) {
  temp <- parse$is.neg(out[2])
  if ( opt[1] %in% c("+","-") ) out[1] <- paste0("\\left(",out[1],"\\right)")
  if ( temp | opt[2] %in% c("+","-") ) out[2] <- paste0("\\left(",out[2],"\\right)")
  temp <- if ( opt[1]=="1" & opt[2]=="a" & !temp ) paste(out[1],out[2]) else paste(out[1],"\\cdot",out[2])
  list(out=temp,opt="*")
}

operator[["/"]]       <- list()
operator[["/"]]$n     <- 2
operator[["/"]]$eval  <- function ( opnds ) as.numeric(opnds[1]) / as.numeric(opnds[2])
operator[["/"]]$infix <- function ( opt, out ) list(out=paste("\\frac{",out[1],"}{",out[2],"}"), opt="/")

operator[["^"]]       <- list()
operator[["^"]]$n     <- 2
operator[["^"]]$eval  <- function ( opnds ) as.numeric(opnds[1]) ^ as.numeric(opnds[2])
operator[["^"]]$infix <- function ( opt, out ) {
  if ( parse$is.neg(out[1]) | ! parse$is.opnd(out[1]) ) out[1] <- paste0("\\left(",out[1],"\\right)")
  list(out=paste0(out[1],"^{",out[2],"}"), opt="^")
}

operator[["!"]]       <- list()
operator[["!"]]$n     <- 1
operator[["!"]]$eval  <- function ( opnds ) factorial(as.numeric(opnds))
operator[["!"]]$infix <- function ( opt, out ) {
  temp <- if ( parse$is.neg(out) | ! parse$is.opnd(opt) ) paste0("\\left(",out,"\\right)!") else paste0(out,"!")
  list(out=temp, opt="!")
}

operator[["sqrt"]]       <- list()
operator[["sqrt"]]$n     <- 1
operator[["sqrt"]]$eval  <- function ( opnds ) sqrt(as.numeric(opnds))
operator[["sqrt"]]$infix <- function ( opt, out ) list(out=paste0("\\sqrt{",out,"}"), opt="sqrt")

operator[["|"]]       <- list()
operator[["|"]]$n     <- 2
operator[["|"]]$eval  <- function ( opnds ) c(as.numeric(opnds[2]), as.numeric(opnds[1]))
operator[["|"]]$infix <- function ( opt, out ) list(out=out[2:1], opt=opt[2:1])

operator[["~"]]       <- list()
operator[["~"]]$n     <- 1
operator[["~"]]$eval  <- function ( opnds ) -as.numeric(opnds)
operator[["~"]]$infix <- function ( opt, out ) {
  temp.neg <- parse$is.neg(out)
  out <- if ( parse$is.neg(out) | opt %in% c("+","-") ) paste0("-\\left(",out,"\\right)") else paste0("-",out)
  list(out=out, opt="~")
}

### Main Functions ############################################################

main <- function ( command ) {
  command <- gsub(pattern="(^\\s+|\\s+$)",replacement="",x=command)
  stack.val <- Stack()
  stack.out <- Stack()
  stack.opt <- Stack()
  for ( item in parse$split(command) ) {
    if ( parse$is.opnd(item) ) {
      stack.val$push( item )
      stack.out$push( item )
      stack.opt$push( ifelse(parse$is.symb(item),"a","1") )
      next
    } else if ( parse$is.optr(item) ) {
      if ( stack.val$length < operator[[item]]$n ) {
        mess <- paste("ERROR: Not enough operands for", paste0('"',item,'"'))
        return(list(val=mess,inf=mess))
      }
      opd <- stack.val$pop(operator[[item]]$n)
      out <- stack.out$pop(operator[[item]]$n)
      opt <- stack.opt$pop(operator[[item]]$n)
      out.v <- operator[[item]]$eval(opd)
      out.i <- operator[[item]]$infix(opt,out)
      stack.val$push(out.v)
      stack.out$push(out.i$out)
      stack.opt$push(out.i$opt)
      next
    }
    mess <- paste("ERROR: Input", paste0('"',item,'"'), "is not a legal command")
    return(list(val=mess,inf=mess))
  }
  return(list(val=stack.val$values, inf=stack.out$values))
}

### Shiny #####################################################################

style.sheet <- "width: auto; max-width: 55em; margin: 0 auto; font-family: monospace;"

ui <- fluidPage(includeCSS("www/style.css"),
                verticalLayout(div(style=style.sheet, withMathJax(),
                                   h3(strong("RPN Calculator")),br(),
                                   textInput(inputId="command", label="Input Command"),
                                   uiOutput("result"), includeHTML("www/body.html")
                                   )))

server <- function ( input, output ) {
  output$result <- renderUI({
    res <- main(input$command) |> lapply( \ (x) paste(x,collapse=", ") )
    if ( res$val == "" ) res$val <- "\u200B"
    if ( res$inf == "" ) {
      res$inf <- "\u200B"
    } else if ( !grepl("ERROR",res$inf ) ) {
      res$inf <- paste0("\\[",res$inf,"\\]")
    }
    withMathJax(strong("Output (Stack):"),br(),res$val,br(),
                strong("Infix Notation:"),br(),res$inf)
  })
}

shinyApp(ui=ui, server=server)
