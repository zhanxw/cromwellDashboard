rm(list = ls())
library(shiny)
library(shinydashboard)
library(stringr)
library(httr)
library(DT)

#' Disable proxy by removing environment variables
#'
#' @export
#'
disableProxy <- function() {
  Sys.unsetenv("http_proxy")
  Sys.unsetenv("https_proxy")
  Sys.unsetenv("no_proxy")
}

#' Start a dashboard for a running cromwell server
#'
#' @param url Cromwell server in the format of address:port
#' @param version Cromwell server version
#' @param ... additional parameters for shinyApp()
#'
#' @return An object that represents the app
#' @export
#'
#' @examples
#'\dontrun{
#'    runCromwellDashboard("127.0.0.1:8000", "v33")
#'}
runCromwellDashboard <- function(url = "127.0.0.1:8000", version = "v33", ...) {
  URL = url
  VERSION = version
  test <- NULL
  status <- NULL

  getEngineStats <- function() {
    res <- httr:: GET(stringr::str_interp("http://${URL}/engine/${VERSION}/stats"))
    httr::content(res)
  }

  getWorkflow <- function() {
    res <- httr:: GET(stringr::str_interp("http://${URL}/api/workflows/${VERSION}/query"))
    tmp <- httr::content(res)$results
    # do.call("rbind.fill", lapply(tmp, as.data.frame))
    res <- dplyr::bind_rows(tmp)
    res[res$status == "Running",, drop = FALSE]
    res
  }

  ui <- dashboardPage(
    dashboardHeader(title = "Cromwell dashboard",
                    tags$li(class = "dropdown", actionButton("refresh_button", "Refresh", icon = icon("refresh")))
    ),
    dashboardSidebar(
      sidebarMenu(
        menuItem("Timeline", tabName = "menu_timeline", icon = icon("dashboard", lib = "glyphicon")),
        menuItem("Status", tabName = "menu_status", icon = icon("stats", lib = "glyphicon")),
        menuItem("Workflows", tabName = "menu_workflow", icon = icon("th"))
      )
    ),
    dashboardBody(
      tabItems(
        tabItem(tabName = "menu_timeline",
                # Boxes need to be put in a row (or column)
                fluidRow(
                  selectInput("id.workflow.select",
                              label=h5("Choose a workflow"),
                              choices= {
                                tmp <- getWorkflow()
                                paste( subset(tmp, status=='Running')$name,
                                       subset(tmp, status=='Running')$id,
                                       sep = "/")})),
                fluidRow(
                  htmlOutput("id.frame")
                )
        ),
        tabItem(tabName = "menu_status",
                # Boxes need to be put in a row (or column)
                fluidRow(
                  valueBoxOutput("id.numWorkflow"),
                  valueBoxOutput("id.numJob")
                )
        ),
        tabItem(tabName = "menu_workflow",
                fluidRow(
                  DT::dataTableOutput("id.workflow.table")
                ),
                fluidRow(
                )
        )
      )
    )
  )

  server <- function(input, output) {
    engineStat <- reactiveValues(data = getEngineStats())
    workflows <- reactiveValues(data = getWorkflow())
    # print(engineStat)
    # print(workflows)

    observe({
      query <- sub(pattern = "^.*/", replacement = "", input$id.workflow.select)
      print(query)
      test <<- stringr::str_interp("http://${URL}/api/workflows/${VERSION}/${query}/timing")
    })

    output$id.numWorkflow <- renderValueBox(
      valueBox(
        engineStat$data$workflows, "Workflows", icon = icon("list"),
        color = "purple"
      )
    )
    output$id.numJob <- renderValueBox(
      valueBox(
        engineStat$data$jobs, "Jobs", icon = icon("list"),
        color = "blue"
      )
    )

    output$id.frame <- renderUI({
      input$id.workflow.select
      my_test <- tags$iframe(src=test, height=600, width=535)
      print(my_test)
      my_test
    })

    createShinyInput <- function(FUN, mask, id, ...) {
      inputs <- character(length(mask))
      for (i in seq_along(mask)) {
        if (mask[i]) {
          inputs[i] <- as.character(FUN(paste0(id, i), ...))
        } else {
          inputs[i] <- ""
        }
      }
      inputs
    }

    output$id.workflow.table <- DT::renderDataTable({
      tmp <- workflows$data
      mask <- tmp$status == "Running"
      tmp$Actions = createShinyInput(actionButton, mask, 'button_', label = "Stop", onclick = 'Shiny.onInputChange(\"select_button\",  this.id)' )

      tmp
      print(tmp[1,])
      tmp
    }, server = FALSE, escape = FALSE, selection = 'none')

    observeEvent(input$select_button, {
      selectedRow <- as.numeric(strsplit(input$select_button, "_")[[1]][2])
      id <- workflows$data$id[selectedRow]
      url <- stringr::str_interp("http://${URL}/api/workflows/${VERSION}/${id}/abort")
      tmp <- (httr::POST(url))
      #print(tmp)
      #print(httr::content(tmp))
      workflows$data <- getWorkflow()
      #print(workflows$data)
      # print(workflows[selectedRow,])
    })

    observeEvent(input$refresh_button, {
      workflows$data <- getWorkflow()
      print(workflows$data)
      # print(workflows[selectedRow,])
    })

  }

  shinyApp(ui, server, ...)

}

if (FALSE) {
  disableProxy()
  runCromwellDashboard("192.168.54.28:8123")
}
