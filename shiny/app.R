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

# Module: Result Table (Multi-Comparison)=====================================
multiTableUI <- function(id) {
  ns <- shiny::NS(id)
  
  shiny::tagList(
    shiny::conditionalPanel(
      condition = "1 == 1",
      shiny::verbatimTextOutput(ns("selected")),
      DT::DTOutput(ns("table")),
      shiny::helpText(
        shiny::textOutput(
          ns("interpretation"),
          inline = TRUE
        )
      )
    )
  )
}
multiTableServer <- function(id, data = NULL, odds_ratios = FALSE) {
  shiny::moduleServer(
    id,
    function(input, output, session) {
      # notes
      l <- shiny::reactive({
        data %>%
          dplyr::filter(Group != "Other") %>%
          nrow
      })
      
      m <- shiny::observe({
        paste(input$table_cells_selected, collapse = ", ")
      })
      
      output$table <- DT::renderDT({
        data
        },
        class = "compact stripe",
        rownames = FALSE,
        options = list(
          ordering = FALSE,
          searching = FALSE,
          paging = FALSE,
          info = FALSE,
          # https://datatables.net/reference/option/infoCallback
          rowCallback = DT::JS(
            'function(row, data) {',
            '  for (x in data) {',
            '     console.log(data[x]);',
            '     if(data[x] == null) {',
            '       $("td:eq("+x+")", row).html("—");',
            '     }',
            '  }',
            '}'
          )
        ),
        # callback = DT::JS('console.info("Done rendering table.");'),
        # caption = ""
        selection = list(
          target = "cell",
          mode = "single",
          selectable = as.matrix(expand.grid(1:l(), 2:(l()+1)))
        ),
      )
      
      output$interpretation = shiny::renderText({
        s <- input$table_cells_selected
        if(length(s) == 0) return("Select a cell for interpretation.")
        r <- s[1,1]
        c <- s[1,2]
        if(r == c - 1) return("Cannot compare a group to itself.")
        if(!odds_ratios) {
          stringr::str_glue(
            "Group ({data[[r, 1]]}) scored",
            "{abs(round(data[[r, c+1]], digits = 2))}%",
            ifelse(data[[r, c+1]] > 0, "higher", "lower"),
            "than group ({data[[c-1, 1]]}).",
            .sep = " "
          )
        } else {
          stringr::str_glue(
            "Group ({data[[r, 1]]}) is",
            "{abs(round(data[[r, c+1]], digits = 1))} times as",
            "likely than group ({data[[c-1, 1]]}) to answer correctly.",
            .sep = " "
          )
        }
      })
    }
  )
}

# Module: Result Table (Single Comparisons) ==================================
singleTableUI <- function(id) {
  ns <- shiny::NS(id)
  shiny::tagList(
    shiny::div("Placeholder")
  )
}
singleTableServer <- function(id, data = NULL, odds_ratios = FALSE) {
  shiny::moduleServer(
    id,
    function(input, output, session) {
      # notes
    }
  )
}

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
    multiTableUI("1")
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
    .DoAnalysis(joinedData(), "Total", "race")$effects
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
  
  multiTableServer("1", data = .DoAnalysis(joinedData(), "4", "race")$effects, odds_ratios = TRUE)
  singleTableServer("2", data = .DoAnalysis(joinedData(), "4", "race")$effects, odds_ratios = TRUE)
}

# Return App Object ==========================================================
shiny::shinyApp(ui = ui, server = server)