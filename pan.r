rm(list=ls())

library(shiny)
library(shinythemes)
library(digest)
library(DT)
library(jsonlite)

read_text = function(pathname) return (paste(readLines(pathname), collapse="\n"))

interview_settings <- fromJSON(read_text('interview-settings.json'))

responses_directory <- file.path(interview_settings$responses_directory)
if(!dir.exists(responses_directory)) dir.create(responses_directory)

responses_directory_backup <- file.path(interview_settings$responses_directory_backup)
if(!dir.exists(responses_directory_backup)) dir.create(responses_directory_backup)

demog_interview_timestamp_raw <- Sys.time()
demog_interview_timestamp <- round(as.numeric(demog_interview_timestamp_raw))

story_interview_timestamp_raw <- Sys.time()
story_interview_timestamp <- as.numeric(story_interview_timestamp_raw)

travel_interview_timestamp_raw <- Sys.time()
travel_interview_timestamp <- as.numeric(story_interview_timestamp_raw)

relative_table <- data.frame(
    relative_name=character(), 
    relative_subj_relationship=character(), 
    relative_sex=character(), 
    relative_age=character(), 
    relative_dob=character(), 
    relative_mom=character(), 
    relative_dad=character(), 
    stringsAsFactors=FALSE)

story_table <- data.frame(story_timestamp=character(), 
    story_title=character(), story_body=character(), stringsAsFactors=FALSE)

trip_table <- data.frame(
    trip_id=character(), 
    subj_name=character(),
    subj_age=character(),
    subj_sex=character(),
    trip_entry_timestamp=character(),
    trip_location=character(),
    trip_times_visited=character(), 
    trip_durations=character(), 
    trip_hosts=character(),
    school_yesno=logical(), 
    school_duration=character(), 
    school_level=character(),
    school_tuition=character(),
    school_notes=character(),
    work_yesno=logical(), 
    work_duration=character(),
    work_whowith=character(), 
    work_details=character(),
    work_notes=character(),
    market_yesno=logical(), 
    market_sell=character(), 
    market_sell_who=character(),
    market_buy=character(), 
    market_buy_who=character(), 
    market_notes=character(), 
    other_yesno=logical(),
    other_notes=character(),
    stringsAsFactors=FALSE)

readResponses <- function(dir){
    files <- list.files(dir, pattern=".csv", full.names=TRUE)
    response_data <- list()
    if(length(files)>0){
        for(i in 1:length(files)) response_data[[i]] <- read.csv(files[i], stringsAsFactors=FALSE)
    }
    return(response_data)
}

resetTravelInterview <- function(session){
    updateTextInput(session, "travel_subj_name", value="")
    updateRadioButtons(session, "travel_subj_sex", selected="male")
    updateTextInput(session, "travel_subj_age", value="")
    resetTripFields(session)
    trip_table <- data.frame(
        trip_id=character(), 
        subj_name=character(),
        subj_age=character(),
        subj_sex=character(),
        trip_entry_timestamp=character(),
        trip_location=character(),
        trip_times_visited=character(), 
        trip_durations=character(), 
        trip_hosts=character(),
        school_yesno=logical(), 
        school_duration=character(), 
        school_level=character(),
        school_tuition=character(),
        school_notes=character(),
        work_yesno=logical(), 
        work_duration=character(),
        work_whowith=character(), 
        work_details=character(),
        work_notes=character(),
        market_yesno=logical(), 
        market_sell=character(), 
        market_sell_who=character(),
        market_buy=character(), 
        market_buy_who=character(), 
        market_notes=character(), 
        other_yesno=logical(),
        other_notes=character(),
        stringsAsFactors=FALSE)
}

resetStoryInterview <- function(session){
    updateTextInput(session, "story_subj_name", value="")
    updateRadioButtons(session, "story_subj_sex", selected="male")
    updateTextInput(session, "story_subj_age", value="")
    updateTextInput(session, "story_misc_notes", value="")
    updateTextInput(session, "story_title", value="")
    updateTextAreaInput(session, "story_body", value="")
    story_table <<- data.frame(story_timestamp=character(), story_title=character(), 
        story_body=character())
}

resetDemogInterview <- function(session){
    updateTextInput(session, "demog_subj_name", value="")
    updateRadioButtons(session, "demog_subj_sex", selected="male")
    updateTextInput(session, "demog_subj_age", value="")
    updateTextInput(session, "demog_subj_dob", value="")
    updateNumericInput(session, "demog_number_kids", value="")
    resetRelativeFields(session)
    relative_table <<- data.frame(
        relative_name=character(), 
        relative_subj_relationship=character(), 
        relative_sex=character(), 
        relative_age=character(), 
        relative_dob=character(), 
        relative_mom=character(), 
        relative_dad=character(), 
        stringsAsFactors=FALSE)
}

resetTripFields <- function(session){
    updateTextInput(session, "trip_id", value="0")
    updateTextInput(session, "trip_location", value="")
    updateTextInput(session, "trip_times_visited", value="")
    updateTextInput(session, "trip_durations", value="")
    updateTextInput(session, "trip_hosts", value="")
    updateTextInput(session, "school_yesno", value=FALSE)
    updateTextInput(session, "school_duration", value="")
    updateTextInput(session, "school_level", value="")
    updateTextInput(session, "school_tuition", value="")
    updateTextInput(session, "school_notes", value="")
    updateTextInput(session, "work_yesno", value=FALSE)
    updateTextInput(session, "work_duration", value="")
    updateTextInput(session, "work_whowith", value="")
    updateTextInput(session, "work_details", value="")
    updateTextInput(session, "work_notes", value="")
    updateTextInput(session, "market_yesno", value=FALSE)
    updateTextInput(session, "market_sell", value="")
    updateTextInput(session, "market_sell_who", value="")
    updateTextInput(session, "market_buy", value="")
    updateTextInput(session, "market_buy_who", value="")
    updateTextInput(session, "market_notes", value="")
    updateTextInput(session, "other_yesno", value=FALSE)
    updateTextInput(session, "other_notes", value="")
    updateActionButton(session, "add_trip", label = "Add Trip")
}

resetRelativeFields <- function(session){
    updateTextInput(session, "relative_id", value = "0")
    updateTextInput(session, "relative_name", value = "")
    updateTextInput(session, "relative_subj_relationship", value="")
    updateRadioButtons(session, "relative_sex", selected = "male")
    updateTextInput(session, "relative_dob", value="")
    updateTextInput(session, "relative_age", value = "")
    updateTextInput(session, "relative_mom", value = "")
    updateTextInput(session, "relative_dad", value = "")
    updateActionButton(session, "add_relative", label = "Add Relative")
    shinyjs::disable("delete_relative")
    shinyjs::disable("add_relative")
}

ui <- fluidPage(title="pan-interviews",
    shinyjs::useShinyjs(),
    theme=shinytheme(interview_settings$shiny_ui_theme),
    navbarPage(title="pan", id='main_navbar',
        tabPanel("travel",
            fluidRow(
                column(width=3,
                    wellPanel(
                        textInput(inputId="travel_subj_name", label="Name", value=""),
                        radioButtons(inputId="travel_subj_sex", label=NULL, choices=c("male", "female"), 
                            selected="male", inline=TRUE),
                        textInput(inputId="travel_subj_age", label="Age", value="")
                    )   
                ),
                column(width=3,
                    column(width=6,
                        h4("List of Places"),
                        "Fatima", br(),
                        "Arenales", br(),
                        "Chacal", br(),
                        "Pupuritumpsi", br(),
                        "Maraca", br(),
                        "Ivasichi", br(),
                        "Las Palmas", br(),
                        "Emeya", br(),
                        "Cuchisama"
                    ),
                    column(width=6,
                        br(),
                        "Agua Negro", br(),
                        "San Jose", br(),
                        "San Antonio", br(),
                        "Tacuaral", br(),
                        "Covendo", br(),
                        "Moseruna", br(),
                        "La Paz", br(),
                        "Rurrenabaque", br(),
                        "San Borja"
                    )
                ),
                column(width=6,
                    wellPanel(
                        tags$style(HTML('#trip_table {color:#848484}')),
                        DT::dataTableOutput('trip_table'),
                        actionButton(inputId="delete_trip", label="Remove")
                    )
                )
            ),
            fluidRow(
                wellPanel(style = "overflow: hidden;",
                    h4("Trip Details"),
                    column(width=3,
                        shinyjs::hidden(textInput(inputId="trip_id", label="Id", value="0")),
                        textInput(inputId="trip_location", label="Location", value=""),
                        textInput(inputId="trip_times_visited", label="How many times visited?", value=""),
                        textInput(inputId="trip_durations", label="How long did you stay each time?", value=""),
                        textInput(inputId="trip_hosts", label="Whom did you stay with?", value=""),
                        column(width=12, offset=7, actionButton(inputId="add_trip", label="Add Trip"))
                    ),
                    column(width=2,
                        checkboxInput(inputId="school_yesno", label="School", value=FALSE),
                        textInput(inputId="school_duration", label="For how long?", value=""),
                        textInput(inputId="school_level", label="What grade levels?", value=""),
                        textInput(inputId="school_tuition", label="Who paid for school?", value=""),
                        textAreaInput(inputId="school_notes", label="Notes", value="", resize="none")
                    ),
                    column(width=2,
                        checkboxInput(inputId="work_yesno", label="Work", value=FALSE),
                        textInput(inputId="work_duration", label="For how long?", value=""),
                        textInput(inputId="work_whowith", label="Who did you work with?", value=""),
                        textInput(inputId="work_details", label="What was the nature of the work?", value=""),
                        textAreaInput(inputId="work_notes", label="Notes", value="", resize="none")
                    ),                
                    column(width=2,
                        checkboxInput(inputId="market_yesno", label="Buy/Sell", value=FALSE),
                        textInput(inputId="market_sell", label="What did you sell?", value=""),
                        textInput(inputId="market_sell_who", label="To whom?", value=""),
                        textInput(inputId="market_buy", label="What did you buy?", value=""),
                        textInput(inputId="market_buy_who", label="From whom?", value=""),
                        textAreaInput(inputId="market_notes", label="Notes", value="", resize="none")
                    ),                
                    column(width=3,
                        checkboxInput(inputId="other_yesno", label="Other", value=FALSE),
                        textAreaInput(inputId="other_notes", label="Notes", value="", height=300, resize="none")
                    )
                )
            ),
            fluidRow(
                column(width=3, offset=9,
                    wellPanel(
                        actionButton(inputId="travel_submit_init", label="Submit Travel Interview"),
                        shinyjs::hidden(img(id="travel_checkmark", src="check.png")),
                        shinyjs::hidden(
                            div(id="travel_submit_prompt",
                                "are you sure you are ready to submit?",
                                br(),
                                actionButton(inputId="travel_submit_yes", label="yes"),
                                actionButton(inputId="travel_submit_no", label="no")
                            )
                        ),
                        shinyjs::hidden(
                            div(id="travel_submit_confirmation",
                                "data saved"
                            )
                        )
                    )
                )
            )
        ),
        tabPanel("narratives",
            fluidRow(
                column(width=3,
                    wellPanel(
                        textInput(inputId="story_subj_name", label="Name", value=""),
                        radioButtons(inputId="story_subj_sex", label=NULL, choices=c("male", "female"), 
                            selected="male", inline=TRUE),
                        textInput(inputId="story_subj_age", label="Age", value="")
                    )
                ),
                column(width=3,
                    wellPanel(
                        "Here you can put some notes about what topics to discuss, 
                        in case you forgot!", br(),
                        br(),
                        "Be sure to ask about", br(),
                        "Interactions with outsiders", br(),
                        "How you met your partner", br(),
                        "Recent deaths and why", br(),
                        "What you want for your children"
                    )
                ),
                column(width=6,
                    wellPanel(
                        textOutput("story_body_toprint")
                    )
                )
            ),
            fluidRow( # data input row
                column(width=3,
                    wellPanel(
                        textAreaInput(inputId="story_misc_notes", label="Interview Notes", value="", height=200, resize="none")
                    )
                ),
                column(width=6,
                    wellPanel(
                        textInput(inputId="story_title", label="Vignette Title", value=""),
                        textAreaInput(inputId="story_body", label="Vignette Body", value="", height=200, resize="none"),
                        actionButton(inputId="add_story", label="Add Vignette")
                    )
                ),
                column(width=3,
                    wellPanel(
                        tags$style(HTML('#story_index {color:#848484}')),
                        DT::dataTableOutput("story_index"),
                        actionButton(inputId='delete_story', label='Delete Vignette')
                    )
                )
            ),
            fluidRow(
                column(width=3, offset=9,
                    wellPanel(
                        actionButton(inputId="story_submit_init", label="Submit Narratives Interview"),
                        shinyjs::hidden(img(id="story_checkmark", src="check.png")),
                        shinyjs::hidden(div(id="story_submit_confirmation", "data saved")),
                        shinyjs::hidden(
                            div(id="story_submit_prompt",
                                "are you sure you are ready to submit?",
                                br(),
                                actionButton(inputId="story_submit_yes", label="yes"),
                                actionButton(inputId="story_submit_no", label="no")
                            )
                        )
                    )
                )
            )
        ),
        tabPanel("demog",
            fluidRow(
                column(width=3,
                    wellPanel(
                        textInput(inputId="demog_subj_name", label="Name", value=""),
                        radioButtons(inputId="demog_subj_sex", label=NULL, choices=c("male", "female"), 
                            selected="male", inline=TRUE),
                        textInput(inputId="demog_subj_age", label="Age", value=""),
                        textInput(inputId="demog_subj_dob",label="Date of Birth", value=""),
                        numericInput(inputId="demog_number_kids", label="Number of Kids", value="")
                    )
                ),
                column(width=3,
                    wellPanel(
                        shinyjs::hidden(textInput(inputId="relative_id", label="Id", value="0")),
                        textInput(inputId="relative_name", label="Relative Name"),
                        textInput(inputId="relative_subj_relationship", label="Relationship"),
                        radioButtons(inputId="relative_sex", label=NA, choices=c("male", "female"), 
                            selected="male", inline=TRUE),
                        textInput(inputId="relative_dob", label="Date of Birth", value=""),
                        textInput(inputId="relative_age", label="Age", value=""),
                        textInput(inputId="relative_mom", label="Mother"),
                        textInput(inputId="relative_dad", label="Father"),

                        ## control buttons
                        column(width=2, offset=6,
                            actionButton(inputId="add_relative", label="Add Relative")
                        ),
                        br(),
                        br()
                    )
                ),
                column(width=6,
                    wellPanel(
                        tags$style(HTML('#relative_table {color:#848484}')),
                        DT::dataTableOutput("relative_table"),
                        br(),
                        column(width=3, offset=9,
                            actionButton(inputId="delete_relative", label="Remove")
                        ),
                        br(),
                        br()
                    )
                )
            ),
            fluidRow(
                column(width=3, offset=9,
                    wellPanel(
                        actionButton(inputId="demog_submit_init", label="Submit Demog Interview"),
                        shinyjs::hidden(img(id="demog_checkmark", src="check.png")),
                        shinyjs::hidden(
                            div(id="demog_submit_prompt",
                                "are you sure you are ready to submit?",
                                br(),
                                actionButton(inputId="demog_submit_yes", label="yes"),
                                actionButton(inputId="demog_submit_no", label="no")
                            )
                        ),
                        shinyjs::hidden(
                            div(id="demog_submit_confirmation",
                                "data saved"
                            )
                        )
                    )
                )
            )
        ),
        tabPanel(""),
        tabPanel(""),
        tabPanel(""),
        tabPanel(""),
        tabPanel(""),
        tabPanel(""),
        tabPanel(""),
        tabPanel(""),
        tabPanel("summary",
            column(width=5, offset=3,
                wellPanel(
                    tags$style(HTML('#response_register {color:#848484}')),
                    h3("Interview Counts"),
                    verbatimTextOutput("counts"),
                    br(),
                    h3("List of Interviewees"),
                    DT::dataTableOutput("response_register")
                )
            )
        ),
        tabPanel("options",
            column(width=3,
                wellPanel(
                h4("Preferences"),
                div(
                  selectInput(inputId='shinytheme_selector', label='UI Themes', choices=c("default", 
                  shinythemes:::allThemes()), selected=interview_settings$shiny_ui_theme, selectize = FALSE),
                    tags$script(read_text('shinytheme-picker.css'))
                ),
                wellPanel(
                    selectInput(inputId="output_filetype", label="output file type", 
                        choices=c(".csv")),
                    textOutput("filetype_info")
                )
                )
            ),
            column(width=3,
                wellPanel(
                    h4("Interview Settings"),
                    textInput(inputId="interview_device", label="Device Name", value=interview_settings$device_name),
                    textInput(inputId="interviewer_name", label="Interviewer", value=interview_settings$interviewer_name),
                    textInput(inputId='interview_location', label='Current Location', value=interview_settings$location),
                    textInput(inputId='interview_lat', label='Latitude', value=interview_settings$latitude),
                    textInput(inputId='interview_lon', label='Longitude', value=interview_settings$longitude)
                )
            ),
            column(width=6,
                wellPanel(style = "overflow: hidden;",
                    fluidRow(
                        column(width=10, 
                            textInput(inputId="data_destination_one", label="hard drive destination folder", value=responses_directory, width=400)
                        ),
                        column(width=2, 
                            br(),
                            shinyjs::hidden(img(id="folder_one_checkmark", src="check.png"))
                        )
                    ),
                    fluidRow(
                        column(width=10, 
                            textInput(inputId="data_destination_two", label="SD card destination folder", value=responses_directory_backup, width=400)
                        ),
                        column(width=2, 
                            br(),
                            shinyjs::hidden(img(id="folder_two_checkmark", src="check.png"))
                        )
                    ),
                    fluidRow(
                        column(width=12,
                        "file paths can be changed in the `interview-settings.yml` file"
                        )
                    )
                )
            )
        )
    )
)



server <- function(input, output, session){

    observe({
        shinyjs::disable("data_destination_one")
        shinyjs::disable("data_destination_two")
    })

    observe({
        if(input$main_navbar=="summary"){
            db <<- readResponses(responses_directory)
        }
    })

    output$response_register <- DT::renderDataTable({
        input$main_navbar
        out <- NULL
        if(length(db)>0){
            interview_name_list <- character(0)
            subject_name_list <- character(0)
            subject_age_list <- character(0)
            for(i in 1:length(db)){ 
                interview_name_list[i] <- db[[i]]$interview_name[1]
                subject_name_list[i] <- db[[i]]$subj_name[1]
                subject_age_list[i] <- db[[i]]$subj_age[1]
            }
            name <- sort(unique(subject_name_list))
            age <- subject_age_list[match(name, subject_name_list)]
            has_demog <- as.numeric(name %in% subject_name_list[interview_name_list=='demog_main'])
            has_story <- as.numeric(name %in% subject_name_list[interview_name_list=='story_main'])
            has_travel <- as.numeric(name %in% subject_name_list[interview_name_list=='travel_main'])
            out <- data.frame(name,age,has_travel, has_story, has_demog)
        }
        return(out)
    })

    output$counts <- renderText({
        input$main_navbar
        out <- 0
        if(length(db)>0){
            name_list <- character()
            for(i in 1:length(db)) name_list[i] <- db[[i]]$interview_name[1]
            n_story_int <- sum(name_list=="story_main")
            n_travel_int <- sum(name_list=="travel_main")
            n_demog_int <- sum(name_list=="demog_main")
            out <- paste(
                paste("Demography:", n_demog_int), 
                paste("Travel:", n_travel_int), 
                paste("Story:", n_story_int),
                sep="\n"
            )
        }
        return(out)
    })

    observeEvent(input$data_destination_one, {
        if(dir.exists(input$data_destination_one)){
            shinyjs::show("folder_one_checkmark")
            responses_directory <<- input$data_destination_one
        } else {
            shinyjs::hide("folder_one_checkmark")
        }
        
    })
    
    observeEvent(input$data_destination_two, {
        if(dir.exists(input$data_destination_two)){
            shinyjs::show("folder_two_checkmark")
            responses_directory_backup <<- input$data_destination_two
        } else {
            shinyjs::hide("folder_two_checkmark")
        }
        
    })


    output$filetype_info <- renderText({
        if(input$output_filetype=='.json'){
            out <- "Javascript Object Notation is a common way to 
            store data in plaintext. Unlike csv files, it can readily store
            a table heirarchy of any complexity, and unlike RData files it
            stores everything in plaintext. Strongly recommended for interview data output."
        }
        if(input$output_filetype=='.csv'){
            out <- "Comma-seperated value files are a common plaintext table format. 
            They are great for storing data that fit into a simple rectangular
            design, but do not have a clear way to store 
            multi-table heirarchical schemas."
        }
        if(input$output_filetype=='.RData'){
            out <- "RData is the native binary format for R. 
            You can load them into R using the load() function, 
            which will add whatever they contain into your R session's namespace. 
            Thus, unlike csv files, they can contain lists, arrays, multiple tables 
            and even metadata notes. 
            However, RData files can only be opened with R, and cannot be properly
            placed under version control."
        }
        return(out)
    })

    ## <narratives server logic> ##
    shinyjs::onclick("story_subj_name", {
        story_interview_timestamp_raw <<- Sys.time()
        story_interview_timestamp <<- as.numeric(story_interview_timestamp_raw)
        shinyjs::hide("story_checkmark")
        shinyjs::hide("story_submit_confirmation")
    })
    observeEvent(input$add_story, {
        story_timestamp <- Sys.time()
        new_story <- data.frame(story_timestamp=story_timestamp, 
            story_title=input$story_title, story_body=input$story_body, 
            stringsAsFactors=FALSE)
        story_table <<- rbind(story_table, new_story)
        updateTextAreaInput(session, inputId="story_body", value="")
        updateTextInput(session, inputId="story_title", value="")
    })
    observe({
      if(is.null(input$story_index_rows_selected)){ 
        shinyjs::disable("delete_story")
      } else {
        shinyjs::enable("delete_story")
      } 
    })
    observeEvent(input$delete_story, {
        if (length(input$story_index_rows_selected) > 0) {
            drop_row <- as.numeric(input$story_index_rows_selected)
            story_table <<- story_table[-drop_row, ,drop=FALSE]
            resetTripFields(session=session)
        }
    })
    output$story_body_toprint <- renderText({
        input$story_index_rows_selected
        story_table[input$story_index_rows_selected,"story_body"]
    })
    output$story_index <- DT::renderDataTable({
        input$add_story
        input$delete_story
        input$story_submit_yes
        story_table[,c("story_title", "story_timestamp")]
        }, server = FALSE, selection = "single", rownames = FALSE,
        colnames=c("vignette title", "timestamp"),
        options = list(searching = FALSE, lengthChange = TRUE, 
          paging=FALSE, info=FALSE, sort=FALSE)
    )
    observeEvent(input$story_submit_init,{
        shinyjs::show("story_submit_prompt")
        shinyjs::disable("story_submit_init")
    })
    observeEvent(input$story_submit_no,{
        shinyjs::hide("story_submit_prompt")
        shinyjs::enable("story_submit_init")
    })
    observeEvent(input$story_submit_yes,{
        shinyjs::hide("story_submit_prompt")
        shinyjs::enable("story_submit_init")
        shinyjs::show("story_submit_confirmation")
        shinyjs::show("story_checkmark")
        story_interview_output <- data.frame(
            interview_name="story_main",
            interview_timestamp_legible=as.character(format(story_interview_timestamp_raw, "%Y%m%d-%H%M%OS")),
            interview_timestamp=story_interview_timestamp,
            interviewer_name=input$interviewer_name, 
            interview_device=input$interview_device,
            interview_location=input$interview_location,
            interview_lat=input$interview_lat,
            interview_lon=input$interview_lon,
            subj_name=input$story_subj_name, 
            subj_sex=input$story_subj_sex, 
            subj_age=input$story_subj_age, 
            story_misc_notes=input$story_misc_notes, stringsAsFactors=FALSE)
        story_interview_filename <- paste(
            story_interview_output$subj_name,
            story_interview_output$interview_name,
            format(story_interview_timestamp_raw, "%Y%m%d-%H%M%OS")
        )
        story_interview_filename <- paste0(story_interview_filename, ".csv")
        write.csv(story_interview_output, file.path(responses_directory, story_interview_filename), row.names=FALSE)
        write.csv(story_interview_output, file.path(responses_directory_backup, story_interview_filename), row.names=FALSE)
        if(nrow(story_table)>0){
            story_table$interview_name <- "story_list"
            story_table$interview_timestamp_legible <- story_interview_output$interview_timestamp_legible
            story_table$interview_timestamp <- story_interview_output$interview_timestamp
            story_table$subj_name <- story_interview_output$subj_name
            story_table$subj_age <- story_interview_output$subj_age
            story_table_filename <- paste(
                story_table$subj_name[1],
                story_table$interview_name[1],
                format(story_interview_timestamp_raw, "%Y%m%d-%H%M%OS")
            )
            story_table_filename <- paste0(story_table_filename, ".csv")
            write.csv(story_table, file.path(responses_directory, story_table_filename), row.names=FALSE)            
            write.csv(story_table, file.path(responses_directory_backup, story_table_filename), row.names=FALSE)

        }
        resetStoryInterview(session)
    })
    ## </narratives server logic> ##

    ## <travel server logic> ##
    observe({
        if(!input$school_yesno){ 
            shinyjs::hide("school_duration")
            shinyjs::hide("school_level")
            shinyjs::hide("school_tuition")
            shinyjs::hide("school_notes")
        } else {
            shinyjs::show("school_duration")
            shinyjs::show("school_level")
            shinyjs::show("school_tuition")
            shinyjs::show("school_notes")
        }
        if(!input$work_yesno){ 
            shinyjs::hide("work_duration")
            shinyjs::hide("work_whowith")
            shinyjs::hide("work_details")
            shinyjs::hide("work_notes")
        } else { 
            shinyjs::show("work_duration")
            shinyjs::show("work_whowith")
            shinyjs::show("work_details")
            shinyjs::show("work_notes")
        }
        if(!input$market_yesno){ 
            shinyjs::hide("market_sell")
            shinyjs::hide("market_sell_who")
            shinyjs::hide("market_buy")
            shinyjs::hide("market_buy_who")
            shinyjs::hide("market_notes")
        } else { 
            shinyjs::show("market_sell")
            shinyjs::show("market_sell_who")
            shinyjs::show("market_buy")
            shinyjs::show("market_buy_who")
            shinyjs::show("market_notes")
        }
        if(!input$other_yesno){
            shinyjs::hide("other_notes")
        } else {
            shinyjs::show("other_notes")
        }
    })
    observeEvent(input$trip_table_rows_selected, {
        if(!is.null(input$trip_table_rows_selected)){
            sel <- input$trip_table_rows_selected
            updateTextInput(session, "trip_id", value = input$trip_table_rows_selected)
            updateTextInput(session, "trip_location", value = trip_table$trip_location[sel])
            updateTextInput(session, "trip_times_visited", value = trip_table$trip_times_visited[sel])
            updateTextInput(session, "trip_durations", value = trip_table$trip_durations[sel])
            updateTextInput(session, "trip_hosts", value = trip_table$trip_hosts[sel])
            updateCheckboxInput(session, "school_yesno", value = trip_table$school_yesno[sel]) # wrong
            updateTextInput(session, "school_duration", value = trip_table$school_duration[sel])
            updateTextInput(session, "school_level", value = trip_table$school_level[sel])
            updateTextInput(session, "school_tuition", value = trip_table$school_tuition[sel])
            updateTextInput(session, "school_notes", value = trip_table$school_notes[sel])
            updateCheckboxInput(session, "work_yesno", value = trip_table$work_yesno[sel])  # wrong
            updateTextInput(session, "work_duration", value = trip_table$work_duration[sel])
            updateTextInput(session, "work_whowith", value = trip_table$work_whowith[sel])
            updateTextInput(session, "work_details", value = trip_table$work_details[sel])
            updateTextInput(session, "work_notes", value = trip_table$work_notes[sel])
            updateCheckboxInput(session, "market_yesno", value = trip_table$market_yesno[sel])  # wrong
            updateTextInput(session, "market_sell", value = trip_table$market_sell[sel])
            updateTextInput(session, "market_sell_who", value = trip_table$market_sell_who[sel])
            updateTextInput(session, "market_buy", value = trip_table$market_buy[sel])
            updateTextInput(session, "market_buy_who", value = trip_table$market_buy_who[sel])
            updateTextInput(session, "market_notes", value = trip_table$market_notes[sel])
            updateTextInput(session, "other_yesno", value = trip_table$other_yesno[sel])
            updateTextInput(session, "other_notes", value = trip_table$other_notes[sel])
            shinyjs::enable("delete_trip")
            updateActionButton(session, "add_trip", label = "Update Trip")
        }
    })    
    observe({
      if(is.null(input$trip_table_rows_selected)) shinyjs::disable("delete_trip") 
    })
    observeEvent(input$delete_trip, {
        if (length(input$trip_table_rows_selected) > 0) {
            drop_row <- as.numeric(input$trip_table_rows_selected)
            trip_table <<- trip_table[-drop_row, ,drop=FALSE]
            resetTripFields(session=session)
        }
    })
    # add trip button logic
    observeEvent(input$add_trip, {
        edit_row <- input$trip_table_rows_selected
        if(is.null(edit_row) | input$trip_id=="0") edit_row <- nrow(trip_table) + 1
        new_trip <- data.frame(
            trip_id=input$trip_id,
            subj_name=input$travel_subj_name,
            subj_age=input$travel_subj_age,
            subj_sex=input$travel_subj_sex,
            trip_entry_timestamp=as.numeric(Sys.time()),
            trip_location=input$trip_location,
            trip_times_visited=input$trip_times_visited,
            trip_durations=input$trip_durations,
            trip_hosts=input$trip_hosts,
            school_yesno=input$school_yesno,
            school_duration=input$school_duration,
            school_level=input$school_level,
            school_tuition=input$school_tuition,
            school_notes=input$school_notes,
            work_yesno=input$work_yesno,
            work_duration=input$work_duration,
            work_whowith=input$work_whowith,
            work_details=input$work_details,
            work_notes=input$work_notes,
            market_yesno=input$market_yesno,
            market_sell=input$market_sell,
            market_sell_who=input$market_sell_who,
            market_buy=input$market_buy,
            market_buy_who=input$market_buy_who,
            market_notes=input$market_notes,
            other_yesno=input$other_yesno,
            other_notes=input$other_notes,
            stringsAsFactors=FALSE)
        resetTripFields(session=session)
        trip_table[edit_row,] <<- new_trip
    })
    output$trip_table <- DT::renderDataTable({
        input$add_trip
        input$delete_trip
        input$travel_submit_yes
        out <- trip_table[,c("trip_location", "trip_durations", 
            "school_yesno", "work_yesno", "market_yesno")]
        out
        }, 
        server = FALSE, selection = "single", rownames = FALSE,
        colnames=c("place", "duration", "school", "work", "buysell"),
        options = list(searching = FALSE, lengthChange = TRUE, 
          paging=FALSE, info=FALSE, sort=FALSE)
    )
    # submit button logic
    observeEvent(input$travel_submit_init,{
        shinyjs::show("travel_submit_prompt")
        shinyjs::disable("travel_submit_init")
    })
    observeEvent(input$travel_submit_no,{
        shinyjs::hide("travel_submit_prompt")
        shinyjs::enable("travel_submit_init")
    })
    observeEvent(input$travel_submit_yes,{
        shinyjs::hide("travel_submit_prompt")
        shinyjs::enable("travel_submit_init")
        shinyjs::show("travel_submit_confirmation")
        shinyjs::show("travel_checkmark")
        travel_interview_output <- data.frame(
            interview_name="travel_main",
            interview_timestamp_legible=as.character(format(travel_interview_timestamp_raw, "%Y%m%d-%H%M%OS")),
            interview_timestamp=travel_interview_timestamp, 
            interviewer_name=input$interviewer_name, 
            interview_device=input$interview_device,
            interview_location=input$interview_location,
            interview_lat=input$interview_lat,
            interview_lon=input$interview_lon,
            subj_name=input$travel_subj_name,
            subj_age=input$travel_subj_age,
            subj_sex=input$travel_subj_sex,
            stringsAsFactors=FALSE)
        travel_interview_filename <- paste(
            travel_interview_output$subj_name,
            travel_interview_output$interview_name,
            format(travel_interview_timestamp_raw, "%Y%m%d-%H%M%OS")
        )
        travel_interview_filename <- paste0(travel_interview_filename, ".csv")
        write.csv(travel_interview_output, file.path(responses_directory, travel_interview_filename), row.names=FALSE)
        write.csv(travel_interview_output, file.path(responses_directory_backup, travel_interview_filename), row.names=FALSE)
        if(nrow(trip_table)>0){
            trip_table$interview_name <- "travel_list"
            trip_table$interview_timestamp_legible <- travel_interview_output$interview_timestamp_legible
            trip_table$interview_timestamp <- travel_interview_output$interview_timestamp
            trip_table$subj_name <- travel_interview_output$subj_name
            trip_table$subj_age <- travel_interview_output$subj_age
            trip_table_filename <- paste(
                trip_table$subj_name[1],
                trip_table$interview_name[1],
                format(travel_interview_timestamp_raw, "%Y%m%d-%H%M%OS")
            )
            trip_table_filename <- paste0(trip_table_filename, ".csv")
            write.csv(trip_table, file.path(responses_directory, trip_table_filename), row.names=FALSE)
            write.csv(trip_table, file.path(responses_directory_backup, trip_table_filename), row.names=FALSE)
        }
        resetTravelInterview(session)
    })
    ## </travel server logic> ##

    ## <reproductive-history server logic> ##
    shinyjs::onclick("demog_subj_name", {
        demog_interview_timestamp_raw <<- Sys.time()
        demog_interview_timestamp <<- as.numeric(story_interview_timestamp_raw)
        shinyjs::hide("demog_checkmark")
        shinyjs::hide("demog_submit_confirmation")
    })
    observeEvent(input$add_relative, {
        edit_row <- input$relative_table_rows_selected
        if(is.null(edit_row) | input$relative_id=="0") edit_row <- nrow(relative_table) + 1
        new_relative <- data.frame(
            relative_name=input$relative_name, 
            relative_subj_relationship=input$relative_subj_relationship, 
            relative_sex=input$relative_sex, 
            relative_age=input$relative_age, 
            relative_dob=input$relative_dob, 
            relative_mom=input$relative_mom, 
            relative_dad=input$relative_dad, 
            stringsAsFactors=FALSE)
        resetRelativeFields(session=session)
        relative_table[edit_row,] <<- new_relative
      }
    )
    observeEvent(input$delete_relative, {
      if (length(input$relative_table_rows_selected) > 0) {
        drop_row <- as.numeric(input$relative_table_rows_selected)
        relative_table <<- relative_table[-drop_row, ,drop=FALSE]
        resetRelativeFields(session=session)
      } 
    })
    observe({
      if(input$relative_name!="") shinyjs::enable("add_relative")
    })
    observe({
      if(is.null(input$relative_table_rows_selected)) shinyjs::disable("delete_relative") 
    })
    # clicking on a relative row logic
    observeEvent(input$relative_table_rows_selected, {
      if (!is.null(input$relative_table_rows_selected)) {
        sel <- input$relative_table_rows_selected
        updateTextInput(session, "relative_id", value = input$relative_table_rows_selected)
        updateTextInput(session, "relative_name", value = relative_table$relative_name[sel])
        updateTextInput(session, "relative_subj_relationship", value=relative_table$relative_subj_relationship[sel])
        updateRadioButtons(session, "relative_sex", selected = as.character(relative_table$relative_sex[sel]))
        updateTextInput(session, "relative_age", value = relative_table$relative_age[sel])
        updateTextInput(session, "relative_dob", value = relative_table$relative_dob[sel])
        updateTextInput(session, "relative_mom", value = relative_table$relative_mom[sel])
        updateTextInput(session, "relative_dad", value = relative_table$relative_dad[sel])
        updateActionButton(session, "add_relative", label = "Update Relative")
        shinyjs::enable("delete_relative")
      }
    })
    # submit button logic
    observeEvent(input$demog_submit_init,{
        shinyjs::show("demog_submit_prompt")
        shinyjs::disable("demog_submit_init")
    })
    observeEvent(input$demog_submit_no,{
        shinyjs::hide("demog_submit_prompt")
        shinyjs::enable("demog_submit_init")
    })
    observeEvent(input$demog_submit_yes,{
        shinyjs::hide("demog_submit_prompt")
        shinyjs::enable("demog_submit_init")
        shinyjs::show("demog_submit_confirmation")
        shinyjs::show("demog_checkmark")
        demog_interview_output <- data.frame(
            interview_name="demog_main",
            interview_timestamp_legible=as.character(format(demog_interview_timestamp_raw, "%Y%m%d-%H%M%OS")),
            interview_timestamp=demog_interview_timestamp, 
            interviewer_name=input$interviewer_name, 
            interview_device=input$interview_device,
            interview_location=input$interview_location,
            interview_lat=input$interview_lat,
            interview_lon=input$interview_lon,
            subj_name=input$demog_subj_name, 
            subj_sex=input$demog_subj_sex,
            subj_dob=input$demog_subj_dob,
            subj_age=input$demog_subj_age,
            subj_nkids=input$demog_number_kids,
            stringsAsFactors=FALSE)
        demog_interview_filename <- paste(
            demog_interview_output$subj_name,
            demog_interview_output$interview_name,
            format(demog_interview_timestamp_raw, "%Y%m%d-%H%M%OS")
        )
        demog_interview_filename <- paste0(demog_interview_filename, ".csv")
        write.csv(demog_interview_output, file.path(responses_directory, demog_interview_filename), row.names=FALSE)
        write.csv(demog_interview_output, file.path(responses_directory_backup, demog_interview_filename), row.names=FALSE)
        if(nrow(relative_table)>0){
            relative_table$interview_name <- "demog_relatives"
            relative_table$interview_timestamp_legible <- demog_interview_output$interview_timestamp_ledigble
            relative_table$interview_timestamp <- demog_interview_output$interview_timestamp_ledigble
            relative_table$subj_name <- demog_interview_output$subj_name
            relative_table$subj_age <- demog_interview_output$subj_age
            relative_table_filename <- paste(
                relative_table$subj_name[1],
                relative_table$interview_name[1],
                format(demog_interview_timestamp_raw, "%Y%m%d-%H%M%OS")
            )
            relative_table_filename <- paste0(relative_table_filename, ".csv")
            write.csv(relative_table, file.path(responses_directory, relative_table_filename), row.names=FALSE)
            write.csv(relative_table, file.path(responses_directory_backup, relative_table_filename), row.names=FALSE)

        }
        # also need to clear all fields and reset the timestamp
        demog_interview_timestamp_raw <<- Sys.time()
        demog_interview_timestamp <<- as.numeric(demog_interview_timestamp_raw)
        resetDemogInterview(session)
    })
    output$relative_table <- DT::renderDataTable({
        input$add_relative
        input$delete_relative
        input$demog_submit_yes
        relative_table[,c("relative_name", "relative_age", "relative_subj_relationship")]
        }, server = FALSE, selection = "single", rownames = FALSE,
        colnames=c("name", "age", "relationship"),
        options = list(searching = FALSE, lengthChange = TRUE, 
          paging=FALSE, info=FALSE, sort=FALSE)
    )
    ## </reproductive-history server logic> ##


}

shinyApp(ui=ui, server=server)






