#!/usr/bin/env Rscript

###############################################################################
# -*- encoding: UTF-8 -*-                                                     #
# Author: Jesse C. Chen (website: jessekelighine.com)                         #
# Description: Reverse Polish Notation                                        #
#                                                                             #
# Last Modified: 2025-04-13                                                   #
###############################################################################
library(shiny)
library(fastmap)
###############################################################################

operator <- list()
parse <- list()

## Parsing ####################################################################

parse$split <- function(command) strsplit(command, split = " +")[[1]]

parse$is_negative <- function(str) substr(str, start = 1, stop = 1) == "-"

parse$is_operator <- function(item) item %in% names(operator)

parse$is_operand <- function(item) {
  grepl(
    pattern = "^[-]{0,1}(([0-9]*\\.){0,1}[0-9]+|[a-zA-Z])$",
    x = item
  )
}

parse$is_symbolic <- function(item) {
  grepl(pattern = "^[-]{0,1}[a-zA-Z]$", x = item)
}

## Operators ##################################################################

operator[["+"]] <- list()
operator[["+"]]$n <- 2
operator[["+"]]$eval <- function(operand) {
  as.numeric(operand[1]) + as.numeric(operand[2])
}
operator[["+"]]$infix <- function(operator, infix) {
  if (parse$is_negative(infix[2])) {
    infix[2] <- paste0("\\left(", infix[2], "\\right)")
  }
  list(infix = paste(infix[1], "+", infix[2]), operator = "+")
}

operator[["-"]] <- list()
operator[["-"]]$n <- 2
operator[["-"]]$eval <- function(operand) {
  as.numeric(operand[1]) - as.numeric(operand[2])
}
operator[["-"]]$infix <- function(operator, infix) {
  if (parse$is_negative(infix[2]) || operator[2] %in% c("+", "-")) {
    infix[2] <- paste0("\\left(", infix[2], "\\right)")
  }
  list(infix = paste(infix[1], "-", infix[2]), operator = "-")
}

operator[["*"]] <- list()
operator[["*"]]$n <- 2
operator[["*"]]$eval <- function(operand) {
  as.numeric(operand[1]) * as.numeric(operand[2])
}
operator[["*"]]$infix <- function(operator, infix) {
  if (operator[1] %in% c("+", "-")) {
    infix[1] <- paste0("\\left(", infix[1], "\\right)")
  }
  if (parse$is_negative(infix[2]) || operator[2] %in% c("+", "-")) {
    infix[2] <- paste0("\\left(", infix[2], "\\right)")
  }
  list(infix = paste(infix[1], "\\cdot", infix[2]), operator = "*")
}

operator[["/"]] <- list()
operator[["/"]]$n <- 2
operator[["/"]]$eval <- function(operand) {
  as.numeric(operand[1]) / as.numeric(operand[2])
}
operator[["/"]]$infix <- function(operator, infix) {
  list(
    infix = paste("\\frac{", infix[1], "}{", infix[2], "}"),
    operator = "/"
  )
}

operator[["^"]] <- list()
operator[["^"]]$n <- 2
operator[["^"]]$eval <- function(operand) {
  as.numeric(operand[1])^as.numeric(operand[2])
}
operator[["^"]]$infix <- function(operator, infix) {
  if (parse$is_negative(infix[1]) || !parse$is_operand(infix[1])) {
    infix[1] <- paste0("\\left(", infix[1], "\\right)")
  }
  infix_output <- paste0(infix[1], "^{", infix[2], "}")
  list(infix = infix_output, operator = "^")
}

operator[["!"]] <- list()
operator[["!"]]$n <- 1
operator[["!"]]$eval <- function(operand) factorial(as.numeric(operand))
operator[["!"]]$infix <- function(operator, infix) {
  infix_output <- if (parse$is_negative(infix) || !parse$is_operand(operator)) {
    paste0("\\left(", infix, "\\right)!")
  } else {
    paste0(infix, "!")
  }
  list(infix = infix_output, operator = "!")
}

operator[["sqrt"]] <- list()
operator[["sqrt"]]$n <- 1
operator[["sqrt"]]$eval <- function(operand) sqrt(as.numeric(operand))
operator[["sqrt"]]$infix <- function(operator, infix) {
  infix_output <- paste0("\\sqrt{", infix, "}")
  list(infix = infix_output, operator = "sqrt")
}

operator[["|"]] <- list()
operator[["|"]]$n <- 2
operator[["|"]]$eval <- function(operand) {
  c(as.numeric(operand[2]), as.numeric(operand[1]))
}
operator[["|"]]$infix <- function(operator, infix) {
  list(infix = infix[2:1], operator = operator[2:1])
}

operator[["~"]] <- list()
operator[["~"]]$n <- 1
operator[["~"]]$eval <- function(operand) -as.numeric(operand)
operator[["~"]]$infix <- function(operator, infix) {
  infix_output <- if (parse$is_negative(infix) || operator %in% c("+", "-")) {
    paste0("-\\left(", infix, "\\right)")
  } else {
    paste0("-", infix)
  }
  list(infix = infix_output, operator = "~")
}

## Main Functions #############################################################

main <- function(command) {
  command <- gsub(pattern = "(^\\s+|\\s+$)", replacement = "", x = command)
  stack <- list()
  stack$value <- faststack()
  stack$infix <- faststack()
  stack$operator <- faststack()
  for (item in parse$split(command)) {
    if (parse$is_operand(item)) {
      stack$value$push(item)
      stack$infix$push(item)
      stack$operator$push("atom")
      next
    }
    if (parse$is_operator(item)) {
      if (stack$value$size() < operator[[item]]$n) {
        error_message <- paste(
          "ERROR: Not enough operands for",
          paste0('"', item, '"')
        )
        return(list(value = error_message, infix = error_message))
      }
      operand <- stack$value$mpop(operator[[item]]$n) |> unlist() |> rev()
      infix <- stack$infix$mpop(operator[[item]]$n) |> unlist() |> rev()
      operators <- stack$operator$mpop(operator[[item]]$n) |> unlist() |> rev()
      output_value <- operator[[item]]$eval(operand)
      output_infix <- operator[[item]]$infix(operators, infix)
      stack$value$mpush(.list = as.list(output_value))
      stack$infix$mpush(.list = as.list(output_infix$infix))
      stack$operator$mpush(.list = as.list(output_infix$operator))
      next
    }
    error_message <- paste(
      "ERROR: Input",
      paste0('"', item, '"'),
      "is not a legal command"
    )
    return(list(value = error_message, infix = error_message))
  }
  return(list(
    value = stack$value$as_list() |> unlist(),
    infix = stack$infix$as_list() |> unlist()
  ))
}

## Shiny ######################################################################

style_sheet <- paste(
  "width: auto;",
  "max-width: 55em;",
  "margin: 0 auto;"
)

ui <- fluidPage(
  includeCSS("www/style.css"),
  verticalLayout(
    div(
      style = style_sheet,
      withMathJax(),
      h3(strong("Reverse Polish Notation Calculator")),
      shiny::HTML(
        paste(
          "<p>",
          "Created by Jesse C. Chen",
          paste0(
            "(",
            "<a href='https://jessekelighine.com'>jessekelighine.com</a>",
            ")"
          ),
          "</p>"
        )
      ),
      br(),
      textInput(inputId = "command", label = "RPN Input", width = "55em"),
      uiOutput("result"),
      includeHTML("www/body.html")
    )
  )
)

server <- function(input, output) {
  output$result <- renderUI({
    result <- main(input$command) |> lapply(\(x) paste(x, collapse = ", "))
    zero_width_space <- "\u200B"
    if (result$value == "") result$value <- zero_width_space
    if (result$infix == "") {
      result$infix <- zero_width_space
    } else if (!grepl("ERROR", result$infix)) {
      result$infix <- paste0("\\[", result$infix, "\\]")
    }
    withMathJax(
      strong("Output (Stack):"),
      br(),
      result$value,
      br(),
      strong("Infix Notation:"),
      br(),
      result$infix
    )
  })
}

shinyApp(ui = ui, server = server)
