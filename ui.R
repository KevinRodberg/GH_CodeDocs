##==============================================================================
# Graphic Interface
##==============================================================================
ui <- shiny::navbarPage("Code Docs (SFWMD-WSB)", id = "NAVBAR",
  tabPanel("Scripts",
    shiny::fluidPage(# Application title
      fluidRow(column(width = 2,
                      actionButton("slctv", "Details",
                                   icon("magnifying-glass"),
                                   style = paste0("color: #fff; ",
                                                  "background-color: #337ab7; ",
                                                  "border-color: #2e6da4")),
                      offset = 4)),
      fluidRow(column(12, DT::dataTableOutput("allCodeDocs")))
    )
  ),
  #End of tab 1
  #TAB 2========================================================================
  tabPanel("New Entry",
    fluidPage(
      titlePanel(h2("Field Entries", align = "left")),
      br(),
      #Entries PAGE-------------------------------------------------------------
      mainPanel(
        fluidRow(
          column(width = 4,
            selectInput(
              inputId = "APPType",
              label = "App Type:",
              choices = c("App", "Script", "Query", "Function", "New")
            )
          ),
          column(width = 4,
                 textInput("Author", "Author:",
                           placeholder = "First Last")),
          column(width = 4,
                 textInput("Script_name", "Script Name:"))
        ),
        #Ends Row 1
        fluidRow(
          column(width = 4,
                 textInput("Location", "Location:",
                           placeholder =
                             "\\\\ad.sfwmd.gov\\dfsroot\\Look_here")),
          column(width = 4,
                 textInput("Dependencies", "Dependencies:",
                           placeholder =
                             "\\\\ad.sfwmd.gov\\dfsroot\\Look_here.filetype")),
          column(width = 4,
                 textInput("Parent_file", "Parent File:",
                           placeholder =
                             "\\\\ad.sfwmd.gov\\dfsroot\\Look_here.filetype")),
        ),
        #Ends 2nd row
        fluidRow(column(width = 4,
                        selectInput("Database",
                                    div("Database:", style = "color: #96BC42"),
                                    choices = c(NA, "DBHYDRO", "Regulation"))),
          column(width = 4,
                 selectInput("Instance", div("Instance:",
                                             style = "color: #96BC42"),
                             choices = c(NA, "GENP", "WREP", "WRED"))),
          column(width = 4,
                 selectInput("Schema", div("Schema:",
                                           style = "color: #96BC42"),
                             choices = c(NA, "bturcott", "DMDBASE",
                                         "REG", "WILMA", "KRODBERG",
                                         "WSUP")))
        ),
        #Ends row 3
        fluidRow(
          column(width = 4,
            selectInput(inputId = "SQL_standard",
                        label = div("SQL Standard:", style = "color: #96BC42"),
                        choices = c(NA, "ANSI", "Old"))
          ),
          column(width = 4,
            selectInput(inputId = "Language",
                        label = "Language:",
                        choices = c("ArcPy", ".bat", "Fortran", "Python",
                                    "R", "SQL"))
          ),
          column(width = 4,
                 dateInput("DATECREATED", "Date Developed:",
                           format = "mm/dd/yyyy"))
        ),
        #Ends row 4
        fluidRow(
          column(width = 4,
                 textInput("Project", "Project name:")),
          column(width = 4,
                 textInput("Modified", "Modified by:",
                           placeholder = "First Last")),
          column(width = 4,
                 dateInput(inputId = "Date_modified",
                           label = "Date Modified:",
                           format = "mm/dd/yyyy"))
        ),
        #Ends row 5
        fluidRow(
          column(width = 4,
                 textAreaInput(inputId = "Keyword",
                               label = "Keywords:",
                               placeholder = "Keyword_1,KW_2,compound_word")),
          column(width = 4,
                 textInput("Documentation", "Documentation:",
                           placeholder =
                             "\\\\ad.sfwmd.gov\\dfsroot\\Look_here.txt")),
          column(width = 4,
                 textAreaInput("Description", "Description:",
                               placeholder =
                                 paste0("Detailed description of the script's",
                                        " functions and what the script was ",
                                        "commissioned to accomplish"),
                               width = "400px"))
        ),
        #Ends Fluid Row
        fluidRow(
          column(2, actionButton("ADD", "Add Entry",  icon("plus"),
                                 style = paste0("color: #fff; ",
                                                "background-color: #337ab7;",
                                                " border-color: #2e6da4")),
                 offset = 8)
        )
        #Ends fluid row
      )
      #Ends panel
    )
    #Ends page
  )
  #Ends Tab
  ##============================================================================
)
# Closes first UI argument
