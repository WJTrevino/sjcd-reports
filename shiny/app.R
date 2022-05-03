# Load Dependencies ==========================================================
library(shiny)
library(jsonlite)
library(magrittr)
library(readxl)
# trim when shipping
library(tidyverse)

# Load Helpers ===============================================================
source("helpers.R")

# UI Definition ==============================================================
ui <- fluidPage(
  title = "EAC DEI Report",
  sidebarPanel(
    h3("EAC DEI Report"),
    # fileInput("file", "EAC Spreadsheet", accept = ".xlsx"),
    textInput("file","Mock file upload (File path)"),
    conditionalPanel(
        condition = "input.file",
        conditionalPanel(
          condition = "output.dataValid == '1'",
          hr(),
          h4("About This Exam"),
          div("Name: ", textOutput("examName", inline = TRUE)),
          div("Students: ", textOutput("nStudents", inline = TRUE)),
          div("Questions: ", textOutput("nQuestions", inline = TRUE)),
          div("Questions: ", textOutput("nGoals", inline = TRUE)),
          actionButton(
            "doAnalysis",
            "Run Analysis",
            icon = icon("calculator")
          )
        )
    )
  ),
  mainPanel(
    textOutput("file"),
    verbatimTextOutput("sqlData"),
    verbatimTextOutput("joinedData"),
    verbatimTextOutput("helper"),
    uiOutput("groups")
  )
)

# Server Definition ==========================================================
server <- function(input, output, session) {
  data <- reactive({
    # req(input$file$datapath)
    req(input$file)
    
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
        set_names() %>%
        map(function(x) {
          progress$inc(amount = 1, message = "Processing Sheets")
        # map(read_xlsx, path = input$file$datapath, .name_repair = "minimal")
          read_xlsx(
            x,
            path = input$file,
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
  })
  
  validateData <- reactive({
    validate(
      need(input$file, "Waiting for file.")
    )
    d <- data()
    m <- "Invalid Formatting"
    validate(
      need(d$'Summary Statistics'[[2,1]] == "Scorable Questions", m),
      need(d$'Summary Statistics'[[2,2]] > 0, m)
    )
    "Valid"
  })
  
  dataValid <- reactive({
    ifelse(validateData() == "Valid", TRUE, FALSE)
  })
  output$dataValid <- renderText({
    ifelse(dataValid(), "1", "0")
  })
  
  sqlData <- reactive({
    read_json("../secrets/test.json", simplifyVector = TRUE) %>% tibble
  })
  output$sqlData <- renderPrint({ sqlData() })
  
  joinedData <- reactive({
    d <- req(data()$'Student Questions')
    s <- req(sqlData())
    left_join(d, s, by = c("Student_id" = "sid")) %>%
      purrr::modify_if(is.character, as.factor) %>%
      purrr::modify_at("Student_id", as.character) %>%
      dplyr::mutate(
        race = forcats::fct_other(
          race, drop = "Unknown or Not Reported")) %>%
      dplyr::mutate(pell = forcats::fct_other(pell, drop = "Unkn"))
  })
  output$joinedData <- renderPrint({ joinedData() })

  output$examName <- renderText({
    req(dataValid())
    d <- data()
    sub(" (**Webcam**) - Requires Respondus LockDown Browser",
        "",
        # d$`Test Info`[1,1],
        fixed = TRUE)
  })
  
  output$nQuestions = renderText({
    req(dataValid())
    d <- data()
    d$'Summary Statistics'[[2,2]]
  })
  
  output$file <- renderText({
    input$file
  })
  
  output$helper <- renderPrint({
    .DoAnalysis(joinedData(), "Total", "race")
  })
  
  output$groups <- renderUI({
    lapply(1:10, function(x) {
      h4(x)
    })
  })
}

# Return App Object ==========================================================
shinyApp(ui = ui, server = server)