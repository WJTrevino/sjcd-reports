# Load Dependencies ==========================================================
library(jsonlite)
library(magrittr)
library(readxl)
# trim when shipping
library(tidyverse)
# load shiny last so it is not masked
library(shiny)
library(shinydashboard)
library(shinydashboardPlus)

# Load Helpers ===============================================================
source("helpers.R")

# UI Definition ==============================================================
ui <- shiny::fluidPage(
  title = "EAC DEI Report",
  shiny::sidebarPanel(
    shiny::h3("EAC DEI Report"),
    # shiny::fileInput("file", "EAC Spreadsheet", accept = ".xlsx"),
    shiny::textInput("file","Mock file upload (File path)"),
    shiny::conditionalPanel(
        condition = "input.file",
        #shiny::conditionalPanel(
          #condition = "output.dataValid == '1'",
          shiny::h4("About This Exam"),
          shiny::div(
            "Name: ",
            shiny::textOutput("examName", inline = TRUE)
          ),
          shiny::div(
            "Students: ",
            shiny::textOutput("nStudents", inline = TRUE)
          ),
          shiny::div(
            "Questions: ",
            shiny::textOutput("nQuestions", inline = TRUE)
          ),
          shiny::div(
            "Questions: ",
            shiny::textOutput("nGoals", inline = TRUE)
          ),
          shiny::actionButton(
            "doAnalysis",
            "Run Analysis",
            icon = shiny::icon("calculator")
          )
        #)
    ),
    shiny::helpText(shiny::textOutput("status"))
  ),
  shiny::mainPanel(
    shiny::textOutput("file"),
    shiny::verbatimTextOutput("sqlData"),
    shiny::verbatimTextOutput("joinedData"),
    shiny::verbatimTextOutput("helper"),
    shiny::uiOutput("groups")
  )
)

# Server Definition ==========================================================
server <- function(input, output, session) {
  data <- shiny::reactive({
    # shiny::req(input$file$datapath)
    shiny::req(input$file)
    
    sheetNames = c(
      "Test Info",
      "Courses Included",
      "Summary Statistics",
      "Item Analysis",
      "Student Questions",
      "Goals Summary"
    )
    progress <- shiny::Progress$new(max = length(sheetNames))
    on.exit(progress$close())
    
    tryCatch({
      sheetNames %>%
        purrr::set_names() %>%
        purrr::map(function(x) {
          progress$inc(amount = 1, message = "Processing Sheets")
          readxl::read_xlsx(
            x,
            path = input$file,
            # path = input$file$datapath,
            na = c("--"),
            .name_repair = "minimal"
          )
        })
    },
    warning = function(e) {
      NA # Silently swallow error.
    },
    error = function(e) {
      NA # Silently swallow error.
    })
  }, label = "data")
  
  validateData <- shiny::reactive({
    shiny::validate(
      shiny::need(input$file, "Waiting for file.")
    )
    d <- data()
    m <- "Invalid Formatting"
    shiny::validate(
      shiny::need(d$'Summary Statistics'[[2,1]] == "Scorable Questions", m),
      shiny::need(d$'Summary Statistics'[[2,2]] > 0, m)
    )
    "Valid"
  }, label = "validateData")
  
  dataValid <- shiny::reactive({
    ifelse(validateData() == "Valid", TRUE, FALSE)
  }, label = "dataValid")
  output$dataValid <- shiny::renderText({
    ifelse(dataValid(), "1", "0")
  })
  
  sqlData <- shiny::reactive({
    jsonlite::read_json("../secrets/test.json", simplifyVector = TRUE) %>%
    tibble::tibble(.)
  }, label = "sqlData")
  output$sqlData <- shiny::renderPrint({ sqlData() })
  
  joinedData <- shiny::reactive({
    d <- shiny::req(data()$'Student Questions')
    s <- shiny::req(sqlData())
    dplyr::left_join(d, s, by = c("Student_id" = "sid")) %>%
      purrr::modify_if(is.character, as.factor) %>%
      purrr::modify_at("Student_id", as.character) %>%
      dplyr::mutate(
        race = forcats::fct_other(
          race, drop = "Unknown or Not Reported")) %>%
      dplyr::mutate(pell = forcats::fct_other(pell, drop = "Unkn"))
  }, label = "joinedData")
  output$joinedData <- shiny::renderPrint({ joinedData() })

  output$examName <- shiny::renderText({
    shiny::req(dataValid())
    d <- data()
    sub(" (**Webcam**) - Requires Respondus LockDown Browser",
        "",
        d$`Test Info`[1,1],
        fixed = TRUE)
  })
  
  nQuestions <- shiny::renderText({
    shiny::req(dataValid())
    d <- data()
    d$'Summary Statistics'[[2,2]]
  })
  output$nQuestions <- shiny::renderText({ nQuestions() })
  
  output$file <- shiny::renderText({
    input$file
  })
  
  output$helper <- shiny::renderPrint({
    .DoAnalysis(joinedData(), "Total", "race")
  })
  
  output$status <- shiny::renderPrint({ validateData() })
  
  output$groups <- shiny::renderUI({
    lapply(1:nQuestions(), function(x) {
      shiny::h4(x)
    })
  })
}

# Return App Object ==========================================================
shiny::shinyApp(ui = ui, server = server)