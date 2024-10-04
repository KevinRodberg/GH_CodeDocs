##==============================================================================
# Author: Jose O. Grisales, Data Analyst SFWMD
# Purpose: Visualize & ease access to Groundwater Technical programs and scripts
# Date developed: 04/06/2023
# Last updated: 06/03/2024
# SQL Functions developed by Kevin A. Rodberg. Slight modifications were
# made for functionality with the application design.
#==============================================================================
library("lintr")
envVar<- Sys.getenv("HOSTNAME")

logFilePath <- file("./logs/code-docs-connect.log", open = "a")
tempDF <- as.data.frame(installed.packages())
for (i in rownames(tempDF)) {
  cat(file = logFilePath, format(Sys.time(), "%a %b %d %X %Y"))
  cat(file = logFilePath, 
      paste(" ", tempDF[i,c("Package", "Version","LibPath")],sep = ", "))
  cat(file = logFilePath, "\n")
}

#Options=======================================================================
base::options(shiny.maxRequestSize = 100 * 1024^2)

server <-
  function(input, output) {

    logFilePath <- file("./logs/code-docs-connect.log", open = "a")

    # Assign appropriate dbInstance depending upon DEV, TEST, or PROD envs
    HOSTNAME <- Sys.getenv("HOSTNAME")
    if (HOSTNAME=="" ) {
      dbInstance <- "WRED"
    } else {
      if (grep("d.sfwmd.gov", HOSTNAME) == 1) {
        dbInstance <- "WRED"
      } else if (grep("t.sfwmd.gov", HOSTNAME) == 1) {
        dbInstance <- "WRET"
      } else if (grep("p.sfwmd.gov", HOSTNAME) == 1) {
        dbInstance <- "WREP"        
      }
    }

    # Function to support Graceful exit from app
    stopQuietly <- function() {
      opt <- options(show.error.messages = FALSE)
      on.exit(options(opt))
      stop()
    }

    tryCatch({
      connex <- RODBC::odbcConnect(dbInstance, uid = "pub", pwd = "pub",
                                   believeNRows = FALSE)
      r <- TRUE
    },
    warning = function(warnMsg) {
      write(paste(format(Sys.time(), "%a %b %d %X %Y"),
                  toString(warnMsg)), logFilePath, append = TRUE)
      stopQuietly()
    },
    error = function(errMsg) {
      write(paste(format(Sys.time(), "%a %b %d %X %Y"),
                  toString(errMsg)), logFilePath, append = TRUE)
      stopQuietly()
    }
    )
    
    # Test db connection & reconnect if needed prior to other sql function calls
    testDB <- function() {
      tryCatch({
        RODBC::odbcGetInfo(connex)
      },
      error = function(e) {
        write(paste(format(Sys.time(), "%a %b %d %X %Y"),
                    toString("err Reconnect required")), logFilePath,
              append = TRUE)
        connex <<- RODBC::odbcConnect(dbInstance, uid = "pub", pwd = "pub",
                                      believeNRows = FALSE)
        RODBC::odbcGetInfo(connex)
      }
      )
    }
    #SQL Functions==============================================================
    #---------------------------------------------------------------------------
    #--    isDate:   Checks if argument has valid date format
    #---------------------------------------------------------------------------
    isDate <- function(x) {
      !is.na(base::as.Date(base::as.character(x),
                           tz = "UTC", format = "%Y-%m-%d"))
    }

    #---------------------------------------------------------------------------
    #--    defineQuery:    Defines SQL strings for Oracle query
    #---------------------------------------------------------------------------
    defineQuery <- function() {

      #---
      # Columns for Documentation and description omitted from query
      #        check for consistency with NA.rec in shinyServer
      #---
      #  Rows sorted in reverse RECID order so newly edited records stay on top
      filterCols <- base::paste0("select AppType,ScriptName,Author,Language,",
                                 "Database,DateCreated,DateModified,",
                                 "Dependencies,Instance,Keyword,Location,",
                                 "PARENTFILE,DOCUMENTATION,Project,Schema,",
                                 "SQL_Standard,DESCRIPTION,RecID ",
                                 "from wsup.codeDocs where deleted is null ",
                                 "order by RecID desc")
      return(filterCols)
    }

    # Query the data with defineQuery function
    qry <- defineQuery()

    #---------------------------------------------------------------------------
    #-- getSqlData: Run SQL query from str executed w/RODBC connection: connex
    #---------------------------------------------------------------------------
    getSqlData <- function(connex, qry) {
      testDB()
      dbResult <- RODBC::sqlQuery(connex, qry)

      return(dbResult)
    }

    dataRecId <- NA
    n <- 0
    delID <- NA
    recIdSelected <- NA
    sqlData <- getSqlData(connex, qry)

    #---
    #--    deleteSql:  create SQL statements to "DELETE" records in the database
    #--             by marking their status as DELETED
    #---
    deleteSql <- function(dbTable, df, pkeyName, keyId, connex) {
      sqlDel <- base::paste("UPDATE ", dbTable, " SET DELETED = 'Y' where ",
                            pkeyName, "=", keyId)
      cat(file = logFilePath,
          paste(format(Sys.time(), "%a %b %d %X %Y"), sqlDel),
          sep = "\n")
      tryCatch({
        testDB()
        RODBC::sqlQuery(connex, sqlDel)
        r <- TRUE
      },
      error = function(cond) {
        write(paste(format(Sys.time(), "%a %b %d %X %Y"),
                    toString(cond)), logFilePath, append = TRUE)

        r <- FALSE
        return(r)
      }
      )
      return(r)
    }

    #---
    #--    updateSQL:  create SQL statements to UPDATE records in the database
    #---
    updateSqlM <- function(dbTable, df, selectRow, pkeyName,
                           cellEdited, connex) {
      r <- FALSE
      testDB()
      RODBC::odbcSetAutoCommit(connex)

      if (base::is.numeric(cellEdited$value)) {
        sep <- ""
      } else {
        sep <- "'"
      }

      #  Process updates to date slightly different than numeric or character
      if (isDate(cellEdited$value)) {
        sqlUpdate <- base::paste0("UPDATE ", dbTable, " SET ",
                                  base::colnames(df)[cellEdited$row],
                                  "=TO_DATE(", sep,
                                  base::as.character(cellEdited$value),
                                  sep, ",'YYYY-MM-DD') where ", pkeyName, "=",
                                  selectRow)
      } else {
        sqlUpdate <- paste0("UPDATE ", dbTable, " SET ",
                            base::colnames(df)[cellEdited$row],
                            "=", sep, base::as.character(cellEdited$value), sep,
                            " where ", pkeyName, "=", selectRow)
      }

      tryCatch({
        cat(file = logFilePath,
            paste(format(Sys.time(), "%a %b %d %X %Y"), sqlUpdate),
            sep = "\n")
        RODBC::sqlQuery(connex, sqlUpdate)
        r <- TRUE
      },
      error = function(cond) {
        r <- FALSE
        write(paste(format(Sys.time(), "%a %b %d %X %Y"),
                    toString(cond)), logFilePath, append = TRUE)
        return(r)
      }
      )
      return(r)
    }
    
    #---
    #  Defines server logic to manipulate data table and database for CodeDocs
    # --    updateSQL:  create SQL statements to UPDATE records in the database
    # ---
    #(Modified to only process new entries)
    updateSqlNewEntry <- function(connex, newEntry) {
      r <- FALSE
      tryCatch({
        testDB()
        RODBC::odbcSetAutoCommit(connex, autoCommit = TRUE)
        cat(file = logFilePath,
            RODBC::sqlQuery(connex, newEntry, errors = TRUE), "\n")
        r <- TRUE
      },
      error = function(cond) {
        r <- FALSE
        return(r)
      }
      )
      return(r)
    }

    testDB()

    sqlData <- shiny::reactiveVal(getSqlData(connex, qry))
    recIdSelected <- shiny::reactiveVal(NULL)

    renderDtblNewEntry <- function(data, server = TRUE, ...) {
      DT::renderDT(data,
        selection = "single", server = server, ...,
        options = list(autoWidth = FALSE, pageLength = 10,
          initComplete = DT::JS("function(settings, json) {",
                                paste0("$(this.api().table().header()).",
                                       "css({'background-color': '#000', ",
                                       "'color': '#fff'});"),
                                "}") #Changes color of headings
        )
      )
    }

    #---
    #  Wrapper function setting a couple pass-thrus
    #      and default values to DT::renderDT()
    #---
    #Editable Version (Modified) ===============================================
    renderDtbl <- function(data, editable = "cell", server = TRUE, ...) {
      DT::renderDT(data,
        selection = "none", server = server,
        editable = list(target = editable, disable = list(columns = 0)), ...,
        options = list(autoWidth = FALSE, scrollX = TRUE,
          pageLength = nrow(data),
          dom = "t", ordering = FALSE,
          initComplete = DT::JS("function(settings, json) {",
                                paste0("$(this.api().table().header()).",
                                       "css({'background-color':",
                                       " '#000', 'color': '#fff'});"), "}")
          #Changes color of headings
        )
      )
    }

    # A queue of notification IDs
    delID <- character(0)

    # A counter
    n <- 0

    # Render the SQL data each time data is edited
    # filter option provides filter for each column
    output$allCodeDocs <- renderDtblNewEntry(sqlData(), filter = "bottom",
                                             rownames = FALSE,
                                             class =
                                               "white-space: nowrap hover")
    #--- observeEvent:  cell_edit
    #  When an edit is detected, update the database and refresh SQL data
    #  If data in RECID=0 is edited, a new RECID is assigned from Oracle seq
    #---
    shiny::observeEvent(input$selectCodeDocs_cell_edit, {

      r <- updateSqlM("WSUP.codeDocs", sqlData(),
                      recIdSelected(), "RECID",
                      input$selectCodeDocs_cell_edit, connex)
      if (r) {
        cat(file = logFilePath,
            paste(format(Sys.time(), "%a %b %d %X %Y"),
                  "Modify rec", recIdSelected()),
            sep = "\n")
      } else {
        cat(file = logFilePath,
            paste(format(Sys.time(), "%a %b %d %X %Y"),
                  "--DID NOT--  Modify rec", recIdSelected()),
            sep = "\n")
      }

      # Refresh sqlData & proxy1 with updated DB recs & add NA.rec to beginning
      sqlData(getSqlData(connex, qry))
      #  Avoid to use a reactive dataset in datatable. Each time it changes,
      #  the table is re-rendered. It is better to use a proxy.

      output$allCodeDocs <- renderDtblNewEntry(sqlData(), filter = "bottom",
                                               rownames = FALSE,
                                               class =
                                                 "white-space: nowrap hover")
    })

    #Observe Event for Entry Additions==========================================
    shiny::observeEvent(input$ADD, {
      newInsertEntry <- paste("DECLARE", "\n", "pkVal NUMBER(8);", "\n",
                              "BEGIN", "\b", " INSERT into WSUP.CODEDOCS ",
                              "(RECID, APPTYPE, AUTHOR, DATABASE, ",
                              "DATECREATED, DATEMODIFIED, DESCRIPTION, ",
                              "DOCUMENTATION, KEYWORD, LANGUAGE, ",
                              "LOCATION, MODIFIED, PROJECT,\"SCHEMA\", ",
                              "SCRIPTNAME,SQL_STANDARD)", " Values ",
                              "(WSUP.DOCS_SEQ.NEXTVAL,",
                              paste0("'", input$APPType, "',"),
                              paste0("'", input$Author, "',"),
                              paste0("'", input$Database, "',"),
                              paste0("TO_DATE('",
                                     format(as.Date(input$DATECREATED),
                                            "%m/%d/%Y"), "','MM/DD/YYYY'),"),
                              paste0("TO_DATE('",
                                     format(as.Date(input$Date_modified),
                                            "%m/%d/%Y"), "','MM/DD/YYYY'),"),
                              paste0("'", input$Description, "',"),
                              paste0("'", input$Documentation, "',"),
                              paste0("'", input$Keyword, "',"),
                              paste0("'", input$Language, "',"),
                              paste0("'", input$Location, "',"),
                              paste0("'", input$Modified, "',"),
                              paste0("'", input$Project, "',"),
                              paste0("'", input$Schema, "',"),
                              paste0("'", input$Script_name, "',"),
                              paste0("'", input$SQL_standard, "')"),
                              "returning RECID into pkVal; ",
                              "\n", "")

      newEntry <- paste0(newInsertEntry, "UPDATE ", "WSUP.codeDocs", " SET ",
                         "RECID", "=", "pkVal",
                         " where ",
                         "RECID", "=pkVal;", "\n", "commit;", "\n", "end;")

      r <- updateSqlNewEntry(connex, newEntry)
      cat(file = logFilePath,
          paste(format(Sys.time(), "%a %b %d %X %Y"),
                input$APPType, input$Script_name,
                "entry was recorded"),
          sep = "\n")
      if (r) {
        shiny::showNotification(paste("Your",
                                      input$APPType, input$Script_name,
                                      "entry was recorded", duration = NULL))
      } else {
        shiny::showNotification(paste("Your",
                                      input$APPType, input$Script_name,
                                      "entry was ---NOT--- recorded",
                                      duration = NULL))
      }

      # Refresh sqlData & proxy1 with updated DB recs & add NA.rec to beginning
      # Transfer the following to global.R & put assignment local to global var
      sqlData(getSqlData(connex, qry))
      #  Avoid to use a reactive dataset in datatable. Each time it changes,
      #  the table is re-rendered. It is better to use a proxy.

      output$allCodeDocs <- renderDtblNewEntry(sqlData(), filter = "bottom",
                                               rownames = FALSE,
                                               class =
                                                 "white-space: nowrap hover")
    })

    #Deletion Logic=============================================================
    #--- observeEvent: Del [Delete Select Row] button
    #  selected row is marked for deletion
    #---
    shiny::observeEvent(input$Del, {
      if (!is.null(input$allCodeDocs_rows_selected)) {
        #NOTE: codeDocs_rows_selected may return more than 1 row
        dataRecId <- sqlData()[input$allCodeDocs_rows_selected, ]$RECID
        cat(file = logFilePath, paste(format(Sys.time(), "%a %b %d %X %Y"),
                                      "Rows Selected for Deletion:",
                                      input$allCodeDocs_rows_selected,
                                      "RECID =", dataRecId),
            sep = "\n")
        shiny::showNotification(paste("Record",
                                      paste0(dataRecId, collapse = ","),
                                      "identified for deletion"),
                                duration = NULL)
        n <- n + 1
        delID <- dataRecId

        # r is True or False return value from SQL execution
        r <- deleteSql("WSUP.codeDocs", sqlData(), "RECID", delID, connex)
        cat(file = logFilePath,
            paste(format(Sys.time(), "%a %b %d %X %Y"),
                  "Record deleted=", r), sep = "\n")

        # Refresh sqlData & proxy1 w/ updated DB recs & add NA.rec to beginning
        sqlData(getSqlData(connex, qry))

        output$allCodeDocs <- renderDtblNewEntry(sqlData(), filter = "bottom",
                                                 rownames = FALSE,
                                                 class =
                                                   "white-space: nowrap hover")
      }
    })

    #View details of a selected row=============================================
    shiny::observeEvent(input$slctv, {
      if (!(is.null(input$allCodeDocs_rows_selected))) {
        selectionLongForm <- as.data.frame(
          cbind(names(sqlData()),
            t(sqlData()[input$allCodeDocs_rows_selected, ])
          )
        )
        names(selectionLongForm) <- c("Field", "Entry")

        output$selectCodeDocs <- renderDtbl(
          selectionLongForm[-18, ],
          rownames = FALSE,
          class = "white-space: wrap"
        )
        recIdSelected(selectionLongForm[18, 2])
      }
      col1 <- shiny::column(width = 6,
        shinyWidgets::actionBttn(inputId = "Del",
                                 label = "Delete Entry",
                                 size = "s",
                                 color = "danger",
                                 style = "stretch")
      )
      col2 <- shiny::column(width = 6, "Double-click Cell to Edit Data")
      col3 <- shiny::column(width = 12,
        if (is.null(input$allCodeDocs_rows_selected)) {
          paste0("No Selection Registered")
        } else {
          DT::DTOutput("selectCodeDocs")
        }
      )
      shiny::showModal(
        shiny::modalDialog(
          title = shiny::fluidRow(col1,col2, 
          ),
          shiny::fluidPage(
            shiny::fluidRow(col3, escape = FALSE)
          ),
          shiny::tags$head(shiny::tags$style(".modal-dialog{ width:1269px}")),
          shiny::tags$head(shiny::tags$style(".modal-body{ min-height:700px}")),
          easyClose = TRUE
        )
      )
    }
    )
  }
