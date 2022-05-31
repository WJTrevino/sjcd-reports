# Load Dependencies ==========================================================
library("jsonlite")
library("multcomp")
library("emmeans")
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
          shiny::br(),
          shiny::actionButton(
            "doQuestionAnalysis",
            "Run Detail Analysis ",
            icon = shiny::icon("calculator")
          ),
          shiny::helpText("Detail analysis is more speculative and",
                          "will take longer to compute.")
      )
    ),
    shiny::helpText(shiny::textOutput("status"))
  ),
  shiny::mainPanel(
    DT::DTOutput("helper")
  )
)

# Server Definition ==========================================================
server <- function(input, output, session) {
  data <- shiny::reactive({
    return(.LoadData(shiny::req(input$file)))
   #return(.LoadData(shiny::req(input$file$datapath)))
  }, label = "data")
  
  .LoadData = function(path = NULL, shiny_app = TRUE) {
    stopifnot(is.character(path))
    
    sheetNames = c(
      "Test Info",
      "Courses Included",
      "Summary Statistics",
      "Item Analysis",
      "Student Questions",
      "Student Goals",
      "Goals Summary"
    )
    
    if(shiny_app){
      progress <- shiny::Progress$new(max = length(sheetNames))
      on.exit(progress$close())
    }
    
    tryCatch({
      data <- sheetNames %>%
        purrr::set_names() %>%
        purrr::map(function(x) {
          if (shiny_app) {
            progress$inc(amount = 1, message = "Processing Sheets")
          }
          readxl::read_xlsx(
            x,
            path = path,
            na = c("--"),
            .name_repair = "minimal"
          )
        })
    }, warning = function(e) {
      NA # Silently swallow warnings.
    }, error = function(e) {
      NA # Silently swallow errors.
    })
    
    return(data)
  }
  
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
    return("Valid")
  }, label = "validateData")
  
  dataValid <- shiny::reactive({
    ifelse(validateData() == "Valid", TRUE, FALSE)
  }, label = "dataValid")
  output$dataValid <- shiny::renderText({
    ifelse(dataValid(), "1", "0")
  })
  
  sqlData <- shiny::reactive({
    # dummy SQL fetch -- to be replaced
    jsonlite::read_json("../secrets/test.json", simplifyVector = TRUE) %>%
      tibble::tibble(.) %>%
      return(.)
  }, label = "sqlData")
  output$sqlData <- shiny::renderPrint({ sqlData() })
  
  .JoinData <- function(excel = NULL, sql_data = NULL) {
    stopifnot(
      is.list(excel),
      is.data.frame(sql_data)
    )
    
    item_sheet <- excel$'Student Questions'
    goal_names <- excel$'Goals Summary'$Goals
    goal_sheet <- excel$'Student Goals' %>%
      dplyr::select(dplyr::any_of(c("Student_id", goal_names)))
    
    join_data <- dplyr::left_join(
        item_sheet,
        sql_data,
        by = c("Student_id" = "sid")
      ) %>%
      dplyr::left_join(
        .,
        goal_sheet,
        by = "Student_id"
      ) %>%
      dplyr::filter(!is.na(Student_id)) %>%
      purrr::modify_if(is.character, as.factor) %>%
      purrr::modify_at("Student_id", as.character) %>%
      dplyr::mutate(
        race = forcats::fct_other(
          race,
          drop = "Unknown or Not Reported"
        )) %>%
      dplyr::mutate(pell = forcats::fct_other(pell, drop = "Unkn"))
    
    var_list <- c(
      "race",
      "gender",
      "FT",
      "FG",
      "pell"
    )
    
    for(x in var_list) {
      join_data %<>%
        dplyr::filter(!is.na(.data[[x]])) %>%
        dplyr::mutate("{x}" := forcats::fct_lump_min(.data[[x]], min = 3)) %>%
        dplyr::mutate("{x}" := forcats::fct_infreq(.data[[x]]))
      
      if ("Other" %in% levels(join_data[[x]])) {
        join_data %<>%
          dplyr::mutate("{x}" := forcats::fct_relevel(
            .data[[x]], "Other", after = Inf))
      }
    }
    
    return(join_data)
  }
  joinedData <- shiny::reactive({
    return(.JoinData(shiny::req(data()), shiny::req(sqlData())))
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
    return(data()$'Summary Statistics'[[1,2]])
  })
  output$nStudents <- shiny::renderText({ nStudents() })
    
  nQuestions <- shiny::renderText({
    shiny::req(dataValid())
    data()$'Summary Statistics'[[2,2]]
  })
  output$nQuestions <- shiny::renderText({ nQuestions() })
  
  nGoals <- shiny::renderText({
    shiny::req(dataValid())
    length(data()$'Goals Summary'$Goals)
  })
  output$nGoals <- shiny::renderText({ nGoals() })
  
  output$file <- shiny::renderText({
    input$file
  })
  
  output$helper <- DT::renderDataTable({
    shiny::req(dataValid())
    .DoAnalysis(joinedData(), "Total", "race") %$%
      tibble::rowid_to_column(effects, var = "#")
  },
  class = "compact stripe",
  rownames = FALSE
  )
  
  output$status <- shiny::renderPrint({ validateData() })
  
  output$detailOutput <- shiny::renderUI({
    shiny::req(joinedData())
    shiny::req(nQuestions())
    
    lapply(1:nQuestions(), function(x) {
      shiny::h4(x)
      shiny::div(renderPrint({
        .DoAnalysis(joinedData(), as.character(x), "race")
      }))
    })
  })
}

# Return App Object ==========================================================
shiny::shinyApp(ui = ui, server = server)