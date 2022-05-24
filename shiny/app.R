# Load Dependencies ==========================================================
library("jsonlite")
library("multcomp")
library("magrittr")
library("readxl")
# trim when shipping
library("tidyverse")
library("glue")
# load shiny last so it is not masked
library("shiny")
library("shinydashboard")
library("shinydashboardPlus")

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
        shiny::conditionalPanel(
          condition = "1 == 1",
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
            "Goals: ",
            shiny::textOutput("nGoals", inline = TRUE)
          ),
          shiny::actionButton(
            "doSummaryAnalysis",
            "Run Summary",
            icon = shiny::icon("calculator")
          ),
          shiny::br(),
          shiny::actionButton(
            "doQuestionAnalysis",
            "Run Question Analysis",
            icon = shiny::icon("calculator")
          )
      )
    ),
    shiny::helpText(shiny::textOutput("status"))
  ),
  shiny::mainPanel(
    shiny::textOutput("file"),
    shiny::verbatimTextOutput("joinedData"),
    shiny::verbatimTextOutput("helper"),
    shiny::uiOutput("detailOutput")
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
      "Student Goals",
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
    
    # List of formatting checks to verify EAC file.
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
    item_sheet <- shiny::req(data()$'Student Questions')
    goal_names <- shiny::req(data()$'Goals Summary'$Goals)
    goal_sheet <- shiny::req(data()$'Student Goals') %>%
      dplyr::select(dplyr::any_of(c("Student_id", goal_names)))
    sql_data <- shiny::req(sqlData())
    
    dplyr::left_join(item_sheet, sql_data, by = c("Student_id" = "sid")) %>%
      dplyr::left_join(., goal_sheet, by = "Student_id") %>%
      dplyr::filter(!is.na(Student_id)) %>%
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
    sub(" (**Webcam**) - Requires Respondus LockDown Browser",
        "",
        data()$'Test Info'[[1,1]],
        fixed = TRUE)
  })

  nStudents <- shiny::renderText({
    shiny::req(dataValid())
    data()$'Summary Statistics'[[1,2]]
  })
  output$nStudents <- shiny::renderText({ nStudents() })
    
  nQuestions <- shiny::renderText({
    shiny::req(dataValid())
    data()$'Summary Statistics'[[2,2]]
  })
  output$nQuestions <- shiny::renderText({ nQuestions() })
  
  nGoals <- shiny::renderText({
    shiny::req(dataValid())
    data()$'Summary Statistics'[[2,2]]
  })
  output$nGoals <- shiny::renderText({ nQuestions() })
  
  output$file <- shiny::renderText({
    input$file
  })
  
  output$helper <- shiny::renderPrint({
    joinedData() %>% names
    #.DoAnalysis(joinedData(), "Total", "race")
  })
  
  output$status <- shiny::renderPrint({ validateData() })
  
  output$detailOutput <- shiny::renderUI({
    shiny::req(joinedData())
    lapply(1:nQuestions(), function(x) {
      shiny::h4(x)
      #shiny::div(renderPrint({
      #  .DoAnalysis(joinedData(), as.character(x), "race")
      #}))
    })
  })
}

# Return App Object ==========================================================
shiny::shinyApp(ui = ui, server = server)