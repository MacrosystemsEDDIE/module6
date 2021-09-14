# Load required libraries
suppressPackageStartupMessages(library(shinyjs, quietly = TRUE))
suppressPackageStartupMessages(library(shinyBS, quietly = TRUE))
suppressPackageStartupMessages(library(shinydashboard, quietly = TRUE))
suppressPackageStartupMessages(library(rintrojs, quietly = TRUE))
suppressPackageStartupMessages(library(slickR, quietly = TRUE))
suppressPackageStartupMessages(library(sortable, quietly = TRUE))
suppressPackageStartupMessages(library(ncdf4, quietly = TRUE))
suppressPackageStartupMessages(library(ggplot2, quietly = TRUE))
suppressPackageStartupMessages(library(stringr, quietly = TRUE))
# suppressPackageStartupMessages(library(ggforce, quietly = TRUE)) # Only for geom_ellipse (doesn't work in plotly!)

# Functions required
source("R/textAreaInput2.R")

# Help documentation
help_text <- read.csv("data/help_text.csv", row.names = 1)

# Load text input
module_text <- read.csv("data/module_text.csv", row.names = 1, header = FALSE)

# Read in assessment questions
quest <- read.csv("data/handout_questions.csv", row.names = 1)
answers <- quest
answers[, 1] <- NA

# colors for theme
obj_bg <- "#D4ECE1"
ques_bg <- "#B8E0CD"
nav_bg <- "#DDE4E1"
nav_butt <- "#31ED92"
nav_txt <- "#000000" # white = #fff; black = #000000
slider_col <- "#2CB572"

ui <- function(req) {

  tagList( # Added functionality for not losing your settings
    # shinythemes::themeSelector(), # user-defined theme
    # Java to prompt the students to click a button
    # Java script https://community.rstudio.com/t/keeping-track-of-idle-time-during-app-usage/1735
    tags$script("
              (function() {
  var timeoutWarningMsecs = 12 * 60 * 1000;
  var idleTimer;

  function onTimeout() {
    alert('Warning: Session is about to time out! Please click a button to prevent losing progress.');
  }

  function startIdleTimer() {
    if (idleTimer) clearTimeout(idleTimer);
    idleTimer = setTimeout(onTimeout, timeoutWarningMsecs);
  }

  $(document).on('shiny:message shiny:inputchanged', startIdleTimer);

})();"),
    tags$style(type = "text/css", "text-align: justify"),
    tags$head(tags$link(rel = "shortcut icon", href = "macroeddi_ico_green.ico")), # Add icon for web bookmarks
    fluidPage(
      column(1, offset = 11, align = "right",
             introBox(
               actionButton("help", label = "", icon = icon("question-circle")), data.step = 7, data.intro = help_text["help", 1]
             )
      )
    ),
    navbarPage(title = "Module 6: Ecological Forecast Uncertainty",
               position = "static-top", id = "maintab",

               # 1. Module Overview ----
               tabPanel(introBox("Overview",
                                 data.step = 2,
                                 data.intro = help_text["tab_nav1", 1]
               ),
               value = "mtab1",
               introjsUI(), # must include in UI
               introBox(
                 img(src = "project-eddie-banner-2020_green.png", height = 100,
                     width = 1544, top = 5),
                 data.step = 1,
                 data.intro = help_text["welcome", 1]
               ),

               tags$style(".btn-file {
             background-color:#98CAB2;
             border-color: #579277;
             }

             .progress-bar {
             background-color: #579277;
             }"),
               # Change progress bar color
               tags$style(paste0("
                                   .irs-bar,
.irs-bar-edge,
.irs-single,
.irs-grid-pol {
  background: ", slider_col, ";
  border-color: ", slider_col, ";
}")),
               includeCSS("www/slider_cols.css"),
               tags$style(HTML("
               .irs-bar {
                        border-color: transparent;
                        background-color: transparent;
                        }
                        #first {
                        border: 4px double red;
                        }
                        #13a_graz {
                        margin-bottom: 10px;
                        }
                        #bla_border {
                        border: 2px solid black;
                        }
                        #bla_border2 {
                        border: 1px solid black;
                        box-shadow: 0 4px 8px 0 rgba(0, 0, 0, 0.2), 0 6px 20px 0 rgba(0, 0, 0, 0.19);
                        }
                        #txt_j {
                        text-align: justify;
                        }
                        #txt_c {
                        text-align: center;
                        }
                        #txt_l {
                        text-align: left;
                        }
                        #ackn {
                        color: gray;
                        font-size: 12px
                        }
                        #pheno img {
                        transition:transform 0.25s ease;
                        max-width: 100%; width: 100%; height: auto
                        }
                        #nextBtn1:hover {
                        background-color: yellow;
                        }
                        #dl_btn {
                        width:290px
                        }
                        #pheno:hover img{
    -webkit-transform:scale(1.5);
    transform:scale(1.5);
}
                        #wh_link a {
                        color: #FFFFFF
                        }
                        #q6_tab {
                        'border':'1px solid #ddd'
                        }
                        .box.box-solid.box-primary>.box-header {

                }

                .box.box-solid.box-primary{

                background:#B8E0CD
                }
                .box.box-solid.box-success{

                background: #DDE4E1;
                }
                .box.box-solid.box-warning>.box-header {

                }

                .box.box-solid.box-warning{

                background:#FFBE85
                }
                        ")),
               introBox(
                 fluidRow(
                   column(6,
                          #* Module text ====
                          h2("Ecological Forecast Uncertainty"),
                          h3("Summary"),
                          p(id = "txt_j", module_text["intro_eco_forecast", ]),
                          p(id = "txt_j", module_text["this_module", ])
                   ),
                   column(5, offset = 1,
                          br(), br(), br(),
                          img(src = "mod5_viz_v2.png", height = "80%",
                              width = "80%", align = "left")
                          )
                   ), data.step = 8, data.intro = help_text["start", 1]
                 ),
               ),

               # 2. Presentation recap ----
               tabPanel(title = "Presentation", value = "mtab2",
                        img(src = "project-eddie-banner-2020_green.png", height = 100,
                            width = 1544, top = 5),
                        fluidRow(
                          hr(),
                          column(4,
                                 h3("Presentation"),
                                 p("The presentation accompanying this module covers the introduction to forecasting, the nutrient-phytoplankton model (NP) and the importance and relevance of ecological forecasts."),
                                 p("What is a forecast?"),
                                 tags$ul(
                                   tags$li(module_text["what_forecast", ])
                                 ),
                                 p("Why do we forecast?"),
                                 tags$ul(
                                   tags$li(module_text["why_forecast", ])
                                 ),
                                 p("How do we generate a forecast?"),tags$ul(
                                   tags$li(module_text["how_forecast", ])
                                 ),
                                 p("Click through the slides to recap some of the main points from the lecture.")
                          ),
                          column(8, offset = 0, align = "center",
                                 h3("Key Slides",
                                    align = "center"),
                                 h5("Click the arrows to navigate through the slides", align = "center"),
                                 wellPanel(
                                   # slickROutput("slides", width = "600px", height = "450px")
                                 )
                          )
                        )
               ),

               # 3. Introduction ----
               tabPanel(title = "Introduction", value = "mtab3",
                        # tags$style(type="text/css", "body {padding-top: 65px;}"),
                        img(src = "project-eddie-banner-2020_green.png", height = 100,
                            width = 1544, top = 5),
                        fluidRow(
                          column(5,
                                 h3("Workflow for this module"),
                                 tags$ol(
                                   tags$li(id = "txt_j", module_text["workflow1", ]),
                                   tags$li(id = "txt_j", module_text["workflow2", ]),
                                   tags$li(id = "txt_j", module_text["workflow3", ]),
                                   tags$li(id = "txt_j", module_text["workflow4", ])
                                   # tags$li(id = "txt_j", module_text["workflow5", ]),
                                   # tags$li(id = "txt_j", module_text["workflow6", ])
                                 )
                          ),
                          column(6, align = "center", offset = 1,
                                 br(), br(),
                                 img(src = "activity_outline.png", height = "80%", id = "bla_border",
                                     width = "80%", tags$style("border: solid 2px black;"))

                          )
                        ), hr(),
                        fluidRow(
                          column(7, offset = 1,
                                 h3("Student Activities"),
                                 p("Within Introduction, Exploration and Activities A, B and C tabs there are questions for students to complete as part of this module. These can be completed by writing your answers into the text boxes within the green boxes. If you do not complete the module in one continuous sitting you can download a file with your responses saved which you can then upload when you return. When you finish the module, you can generate a report which will embed your answers and saved plots into a Word (.docx) file which you can download and make further edits to before submitting to your instructor."),
                                 box(width = 12, status = "warning",
                                     solidHeader = TRUE,
                                     p(tags$b("WARNING:"), " The Shiny app will disconnect from the server if it is left idle for 15 minutes. If this happens you will lose all your inputs into the app. It is recommended to download the user input at the end of the class, but you can also download throughout the class."),
                                 ),
                                 p("Alternatively, you can download the questions as a Word (.docx) file  and record your answers there. If you opt for this option, you can hide the green question boxes by unchecking the box below."),
                                 checkboxInput("show_q1", "Show questions", value = TRUE),
                                 tags$style(type="text/css", "#stud_dl {background-color:#579277;color: white}"),
                                 conditionalPanel("output.handoutbuilt",
                                                  downloadButton(outputId = "stud_dl", label = "Download Student Handout"),
                                 )
                          ),
                        ), hr(),
                        #* Generate report buttons ====
                        fluidRow(
                          column(4,offset = 1,
                                 h3("Save your progress"),
                                 p(id = "txt_j", "If you run out of time to finish all the activities you can save your progress and return to it at a later date. Click the 'Download user input' button at the bottom of the page and a file 'module5_answers_ID_number.eddie' will download. Store this file in a safe place locally on your computer."),
                                 br(),
                                 h3("Resume your progress"),
                                 p(id = "txt_j", "To reload the app input you can upload the downloaded '.eddie' file below and it will populate your answers into the Shiny app."),
                                 fileInput("upload_answers", "Upload data", accept = c(".eddie", ".rds")), # B77C2C
                                 p(id = "txt_j", HTML(paste0(tags$b("Note:"), " You will need to navigate to tabs Objective 1, 2 and 3 in Activity A after uploading your file for the inputs to load there. You will also need to load the NOAA data in Objective 6."))),
                                 p(id = "txt_j", "Currently the plots do not save to the file.  If you generated plots during your last session, you will need to reload the data and reproduce the plots before generating your report.  Additionally, the answers for Q.10 will need to be re-submitted.")
                          ),
                          column(4, offset = 1,
                                 introBox(
                                   h3("Generate Report"),
                                   p("This will take the answers you have input into this app and generate a Microsoft Word document (.docx) document with your answers which you can download and make further edits before submitting. Return here when you have completed the module."),
                                   actionButton("generate", "Generate Report (.docx)", icon = icon("file"), width = "190px", class = "btn-primary"
                                                # id = "dl_btn", # This is the only button that shows up when the app is loaded
                                                # style = "color: #fff; background-color: #337ab7; border-color: #2e6da4"
                                   ), br(), br(),
                                   data.step = 6, data.intro = help_text["finish", 1]
                                 ),
                                 tags$style(type="text/css", "#download {background-color:#579277;color: white}"),
                                 conditionalPanel(condition = "output.reportbuilt", # This button appears after the report has been generated and is ready for download.
                                                  downloadButton("download", "Download Report", width = "60px", style = "width:190px;"
                                                                 # style = "color: #fff; background-color: #337ab7; border-color: #2e6da4"
                                                  )), br(),
                                 h5(tags$b("Questions still to be completed:")),
                                 # verbatimTextOutput("check_list"),
                                 wellPanel(
                                   htmlOutput("check_list")
                                 )

                          )
                        ),
                        fluidRow(
                          hr(),
                          column(10, align = "left",
                                 box(id = "box1", width = 10, status = "primary",
                                     solidHeader = TRUE,
                                     fluidRow(
                                       column(8, offset = 1,
                                              h3("Before you start..."),
                                              p("Input your name and Student ID and this will be added to your final report."),
                                              textInput("name", "Name:"),
                                              textInput("id_number", "ID number:"),
                                              introBox(
                                                h3(tags$b("Think about it!")),
                                                p("Note: The size of these text boxes can be adjusted by clicking and dragging the bottom right of the text box."),
                                                textAreaInput2(inputId = "q1", label = quest["q1", 1]),
                                                data.step = 5, data.intro = help_text["questions", 1]
                                              )
                                       )
                                     ),

                                 ),
                          )
                        ),
                        hr(),
                        fluidRow(
                          column(6,
                                 h3("Data sources"),
                                 p(HTML(paste0('This module will introduce key concepts within Ecological forecasting through exploration of ', a(href = "https://www.neonscience.org/", "NEON (National Ecological Observation Network) data", target = "_blank"), ", building a model and then generating a short-term ecological forecast.")))
                          ),
                          column(6, align = "center",
                                 a(
                                   href = "https://www.neonscience.org/",
                                   img(src = "NSF-NEON-logo.png", title = "NEON - NSF logo"), target = "_blank"
                                   )
                                 )
                          )
                        ),

               # 4. Site Selection ----
               tabPanel(title = "Site Selection", value = "mtab5",
                        tags$style(".nav-tabs {
  background-color: #DDE4E1;
  border-color: #FFF;

}

.nav-tabs-custom .nav-tabs li.active:hover a, .nav-tabs-custom .nav-tabs li.active a {
background-color: #4D6A5C;
border-color: #FFF;
}

.nav-tabs-custom .nav-tabs li.active {
    border-top-color: #FFF;
    background-color: #4D6A5C;
}"),
                        # tags$style(type="text/css", "body {padding-top: 65px;}"),
                        img(src = "project-eddie-banner-2020_green.png", height = 100,
                            width = 1544, top = 5),
                        fluidRow(
                          column(12,
                                 h3("Activity A: Visualize data from a selected NEON site"),
                                 h4("Explore Data & Understand Model"),
                                 p("Complete objectives 1-3 to...")
                          )
                        ),

                        #* Objective 1 - Select and view site ====
                        tabsetPanel(id = "tabseries1",
                                    tabPanel(title = "Objective 1 - Select and view a NEON site",

                                             value = "obj1", id = "wh_link",

                                             tags$style("outline: 5px dotted green;"),
                                             #* Objective 1 ====
                                             # introBox(
                                               fluidRow(
                                                 column(12,
                                                        wellPanel(style = paste0("background: ", obj_bg),
                                                                  h3("Objective 1 - Select a Site"),
                                                                  p(module_text["obj_01", ])
                                                        )
                                                 )
                                               ),
                                               # data.step = 4, data.intro = help_text["objectives", 1], data.position = "top"),
                                             #** NEON Map ====
                                             fluidRow(
                                               #** NEON Intro ----
                                               column(4,
                                                      h2("Site Description"),
                                                      p("Select a site in the table to highlight on the map"),
                                                      conditionalPanel("input.row_num > 25",
                                                                       selectizeInput("row_num", "Select row",
                                                                                      choices = 1:nrow(neon_sites_df),
                                                                                      options = list(
                                                                                        placeholder = 'Please select a row',
                                                                                        onInitialize = I('function() { this.setValue(""); }')),
                                                                       )
                                                      )
                                                      ,
                                                      DTOutput("table01"),
                                                      p(tags$b("Click 'View latest photo' to see the latest image from the webcam on site (this may take 10-30 seconds).")),
                                                      actionButton("view_webcam", label = "View latest photo", icon = icon("eye"))
                                               ),
                                               #** Site map ----
                                               column(4,
                                                      h2("Map of NEON sites"),
                                                      wellPanel(
                                                        leafletOutput("neonmap")
                                                      )
                                               )

                                               ,
                                               #** Site photo ----
                                               column(4,
                                                      h2("Phenocam"),
                                                      textOutput("prompt1"),
                                                      wellPanel(
                                                        imageOutput("pheno"),
                                                        p(id = "txt_j", module_text["phenocam", ])
                                                      )
                                               )
                                             ), br(),
                                             span(textOutput("site_name1"), style = "font-size: 22px;
                                        font-style: bold;"),
                                             fluidRow(
                                               wellPanel(
                                                 h4(tags$b("About Site")),
                                                 uiOutput("site_html"),
                                                 textOutput("prompt2"),
                                                 htmlOutput("site_link")
                                               ),
                                             ),
                                             fluidRow(
                                               column(10, align = "left",
                                                      box(id = "box3", width = 10, status = "primary",
                                                          solidHeader = TRUE,
                                                          fluidRow(
                                                            column(7, offset = 1,
                                                                   h3("Questions"),
                                                                   h4(quest["q5", 1]),
                                                                   p("If the information for your lake is not on the NEON website then you can input NA (Not Available) into the text box.")
                                                            )
                                                          ),
                                                          fluidRow(
                                                            column(4, offset = 1, align = "left", style = paste0("background: ", ques_bg),
                                                                   textInput(inputId = "q5a", label = quest["q5a", 1] , width = "90%"),
                                                                   textInput(inputId = "q5b", label = quest["q5b", 1], width = "90%"),
                                                                   textInput(inputId = "q5c", label = quest["q5c", 1], width = "90%")
                                                            ),
                                                            column(4, offset = 1, align = "left", style = paste0("background: ", ques_bg),
                                                                   textInput(inputId = "q5d", label = quest["q5d", 1] , width = "90%"),
                                                                   textInput(inputId = "q5e", label = quest["q5e", 1], width = "90%"),
                                                                   textInput(inputId = "q5f", label = quest["q5f", 1], width = "90%")
                                                            )
                                                          )
                                                      )
                                               )
                                             ),
                                             fluidRow(
                                               column(5, offset = 1,
                                                      h3("Next step"),
                                                      p("We will explore the data which has been measured at this site by NEON.."))
                                             )
                                    ),
                                    #* Objective 2 - Explore data
                                    tabPanel(title = "Objective 2 - Explore data",  value = "obj2",
                                             #* Objective 2 - Explore the Data ====
                                             fluidRow(
                                               column(12,
                                                      wellPanel(style = paste0("background: ", obj_bg),
                                                                h3("Objective 2 - Explore the Data"),
                                                                # p(id = "txt_j", module_text["obj_02", ]),
                                                                p("If there are some variables which you are not familiar with, visit the ", a(href = "https://data.neonscience.org/home", "NEON Data Portal", target = "_blank"), "and click 'Explore Data Products' to learn more about how the data are collected.")
                                                      )
                                               )
                                             ),
                                             fluidRow(
                                               column(8, offset = 2,
                                                      h3("Variable descriptions"),
                                                      DT::DTOutput("var_desc")
                                               )
                                             ),
                                             hr(),
                                             fluidRow(
                                               #** Data Table ----
                                               column(4,
                                                      h3("Data Table"),
                                                      p("This is a Shiny data table. It is interactive and allows you to navigate through the data table by searching or clicking through the different pages."),
                                                      DT::DTOutput("neon_datatable")
                                               ),
                                               #** Plot of data ----
                                               column(8,
                                                      h3("Data Plot"),
                                                      p("All plots in this Shiny app are generated using Plotly. This allows you to hover your mouse over the plot to get information from each of the plots. You can inspect the data closely by clicking and zooming into particular areas. There is a tool box at the top of the plot which has the selection function required for Q6."),
                                                      plotlyOutput("var_plot"),
                                                      useShinyjs(),  # Set up shinyjs
                                                      # splitLayout(cellWidths = c("75%", "25%"),
                                                                  selectizeInput("view_var", "Select variable",
                                                                                 choices = unique(neon_vars$Short_name),
                                                                                 options = list(
                                                                                   placeholder = 'Please select a variable',
                                                                                   onInitialize = I('function() { this.setValue(""); }')),
                                                                  ),
                                                                  actionButton("clear_sel1", "Clear Selection"),
                                                      # ),
                                                      wellPanel(
                                                        h4("Variable Description"),
                                                        textOutput("txt_out")
                                                      )
                                               )
                                             ), hr(),
                                             fluidRow(
                                               column(4,
                                                      h3("Calculate statistics"),
                                                      selectInput("stat_calc", label = "Select calculation:", choices = stats),
                                                      textOutput("out_stats")
                                               ),
                                               column(8,
                                                      box(id = "box4", width = 12, status = "primary",
                                                          solidHeader = TRUE,
                                                          fluidRow(
                                                            column(10, offset = 1,
                                                                   h3("Questions"),
                                                                   # h4(quest["q6", 1]),
                                                                   p("Make sure you select data that best represent an annual period (i.e. one or two complete years), be wary of including potential outliers in your selection."),
                                                                   p("To edit the table you must double click the cell and then type in your answer."),
                                                                   DTOutput("q6_tab"),
                                                                   bsTooltip("q6_tab", title = "Double click the cell to edit", placement = "top", trigger = "hover"),
                                                                   br()
                                                            )
                                                          )
                                                      )
                                                      # )
                                                      # )
                                               )
                                             ),
                                             fluidRow(
                                               column(5, offset = 1,
                                                      h3("Next step"),
                                                      p("We will plot each of the variables against chlorophyll-a to see if there are any relationships."))
                                             )
                                    ),

                                    #* Objective 3 - Understand the ecological model ====
                                    tabPanel(title = "Objective 3 - Understand model", value = "obj3",
                                             fluidRow(
                                               column(12,
                                                      wellPanel(style = paste0("background: ", obj_bg),
                                                                h3("Objective 3 - Understand the ecological model"),
                                                                p(module_text["obj_04", ])
                                                      )
                                               ),
                                             ),
                                             fluidRow(
                                               column(12, align = "center",
                                                      img(src = "02-build-model.png", height = "30%",
                                                          width = "30%")
                                               )
                                             ), br(), br(), hr(),
                                             #* Intro text ====
                                             fluidRow(
                                               # conditionalPanel(condition = "input.site_html > 1",
                                               #** NEON Intro ----
                                               column(4,
                                                      h3("What is a Model?"),
                                                      h4("Read through this section and scroll through the slides"),
                                                      p(id = "txt_j", module_text["model1", ]),
                                                      p(id = "txt_j", module_text["model2", ]),
                                                      p(id = "txt_j", module_text["model3", ]),
                                                      p(id = "txt_j", module_text["mod_desc", ]),
                                                      p(id = "txt_j", module_text["phyto_chla", ]),
                                                      p("Click through the images to see how we can go from a conceptual food web model to a mathematical representation of the interaction of Nitrogen (N) and Phytoplankton (P).", id = "txt_j")
                                               ),
                                               column(8,
                                                      br(), br(), br(),
                                                      h5("Click on the arrows to navigate through the slides", align = "center"),
                                                      wellPanel(
                                                        slickROutput("slck_model", width = "600px", height = "450px")
                                                      )
                                               )
                                             ),
                                             hr(),
                                             fluidRow(
                                               column(10, align = "left",
                                                      box(id = "box6", width = 12, status = "primary",
                                                          solidHeader = TRUE,
                                                          fluidRow(
                                                            column(10, offset = 1,
                                                                   h3("Questions"),
                                                                   h4(quest["q9", 1]),
                                                                   radioButtons("q9a", quest["q9a", 1], choices = mod_choices, inline = TRUE, selected = character(0)),
                                                                   radioButtons("q9b", quest["q9b", 1], choices = mod_choices, inline = TRUE, selected = character(0)),
                                                                   radioButtons("q9c", quest["q9c", 1], choices = mod_choices, inline = TRUE, selected = character(0)),
                                                                   br()
                                                            )
                                                          )
                                                      )
                                               )
                                             ),
                                             hr(),

                                             #** Sort state and process variables ====
                                             h2(tags$b("Exercise")),
                                             p(id = "txt_j", "When working with ecological models, the terms 'state variable' and 'parameter' are used. Using the model diagram above, can you identify which are state variables or parameters?"),
                                             p(id = "txt_j", module_text["state_var", 1]),
                                             p(id = "txt_j", module_text["parameter", 1]),

                                             fluidRow(
                                               column(12, align = "left",
                                                      box(id = "box7", width = 12, status = "primary",
                                                          solidHeader = TRUE,
                                                          fluidRow(
                                                            column(8, offset = 1,
                                                                   h3("Questions"),
                                                                   h4(quest["q10", 1]),
                                                                   bucket_list(
                                                                     header = "",
                                                                     group_name = "bucket_list_group",
                                                                     orientation = "horizontal",
                                                                     add_rank_list(
                                                                       text = tags$b("Drag from here"),
                                                                       labels = sample(c(state_vars, process_vars)),
                                                                       input_id = "rank_list_1"
                                                                     ),
                                                                     add_rank_list(
                                                                       text = tags$b("State variable"),
                                                                       labels = NULL,
                                                                       input_id = "rank_list_2"
                                                                     ),
                                                                     add_rank_list(
                                                                       text = tags$b("Parameter"),
                                                                       labels = NULL,
                                                                       input_id = "rank_list_3"
                                                                     )
                                                                   ),
                                                                   br(),
                                                                   h4(quest["q11", 1]),
                                                                   radioButtons("q11a", quest["q11a", 1], choices = mod_choices, inline = TRUE, selected = character(0)),
                                                                   radioButtons("q11b", quest["q11b", 1], choices = mod_choices, inline = TRUE, selected = character(0)),
                                                                   br()
                                                            ),
                                                            column(2,
                                                                   wellPanel(
                                                                     useShinyjs(),  # Set up shinyjs
                                                                     actionButton("ans_btn", "Check answers"),
                                                                     textOutput("state_ans"),
                                                                     textOutput("proc_ans")
                                                                   )
                                                            )
                                                          )
                                                      )
                                               )
                                             ),
                                             fluidRow(
                                               column(5, offset = 1,
                                                      h3("Next step"),
                                                      p("Now we will use this information about the model to..."))
                                               )
                                             )
                                    ),
                        ),

               # 5. Stats 101 ----
               tabPanel(title = "Stats 101", value = "mtab5",
                        img(src = "project-eddie-banner-2020_green.png", height = 100,
                            width = 1544, top = 5),
                        fluidRow(
                          column(12,
                                 wellPanel(style = paste0("background: ", obj_bg),
                                           h3("Objective 4 - Statistics 101"),
                                           p(module_text["obj_04", ])
                                 )
                          ),
                        ),
                        fluidRow(
                          hr(),
                          column(4,
                                 h3("Statistics 101"),
                                 p("We will quickly revisit some key statistical concepts"),
                                 p("What is a population?"),
                                 tags$ul(
                                   tags$li(module_text["what_forecast", ])
                                 ),
                                 p("What is a sample?"),
                                 tags$ul(
                                   tags$li(module_text["why_forecast", ])
                                 ),
                                 p("What is a statistical parameter"),tags$ul(
                                   tags$li(module_text["how_forecast", ])
                                 ),
                                 p("Click through the slides to recap some of the main points from the lecture.")
                          ),
                          column(8, offset = 0, align = "center",
                                 h3("Key Figures",
                                    align = "center"),
                                 h5("Click the arrows to navigate through the slides", align = "center"),
                                 wellPanel(
                                   # slickROutput("slides", width = "600px", height = "450px")
                                   )
                                 )
                          ),
                        hr(),
                        fluidRow(
                          column(6,
                                 h3("Linear Regression"),
                                 p("We will explore the relationship between air temperature and surface water temperature"),
                                 actionButton("plot_airt_swt", "Plot")
                                 ),
                          column(6,
                                 h4("Interactive Linear Regression plot..."),
                                 p("Plot the air temperature against the surface water temperature,"),
                                 wellPanel(
                                   plotOutput("airt_swt_plot")
                                 )
                                 )
                          ),
                        hr(),
                        fluidRow(
                          column(6,
                                 h3("Draw a line through the most points"),
                                 p("By adjusting the y-intercept (b) and the slope (m), draw 10 lines which represent the relationship between air temperature and surface water temperature."),
                                 numericInput("m", "Slope (m)", value = 1, min = -2, max = 2, step = 0.1),
                                 actionButton("draw_line", "Draw line"),
                                 numericInput("b", "Intercept (b)", value = 0, min = -15, max = 15, step = 0.1),
                                 actionButton("save_line", "Save line"),
                                 DTOutput("lr_DT", width = "40%")
                                 ),
                          column(6,
                                 wellPanel(
                                   plotlyOutput("airt_swt_plot_lines")
                                   )
                                 )
                        ),
                        hr(),
                        fluidRow(
                          column(6,
                                 h3("Generate distributions for intercept & slope"),
                                 p("Using the values from the lines you drew above, you will calculate a normal distribution for the parameters."),
                                 p("Calculate the mean and standard deviation of the parameters"),
                                 actionButton("calc_stats", "Calculate!"),
                                 DTOutput("lr_stats", width = "60%"),
                                 hr(),
                                 p("Generate plots of the normal distribution of the parameters (m and b) using the mean and standard deviation from the lines you created."),
                                 sliderInput("m_std", "Slope (m) - Std. Dev.", min = 0, max = 0.5, value = 0.25, step = 0.05),
                                 sliderInput("b_std", "Intercept (b) - Std. Dev.", min = 0, max = 1, value = 0.5, step = 0.05),
                                 actionButton("gen_lr_dist_plot", "Generate plot!")
                                 ),
                          column(6,
                                 plotOutput("lr_m_dist_plot"),
                                 hr(),
                                 plotOutput("lr_b_dist_plot")
                                 )
                          ),
                        hr(),
                        fluidRow(
                          column(6,
                                 h3("Create multiple lines"),
                                 p("Using the distributions you have created above, you are going to randomnly draw lines by sampling values for the slope (m) and the intercept (b) from the distributions you have defined."),
                                 radioButtons("n_samp", "No. of samples", choices = c(10, 20, 50, 75, 100), selected = character(0)),
                                 actionButton("gen_lin_mods", "Add lines"),
                                 conditionalPanel("input.gen_lin_mods >= 1",
                                                  checkboxInput("add_dist", "Distribution plot")),
                                 br(),
                                 DTOutput("mb_samps", width = "60%")
                                 ),
                          column(6,
                                 plotlyOutput("add_lin_mods")
                                 )
                          )
                        ),

               # 6. Probability 101 ----
               tabPanel(title = "Probability 101", value = "mtab6",
                        img(src = "project-eddie-banner-2020_green.png", height = 100,
                            width = 1544, top = 5),
                        fluidRow(
                          column(12,
                                 wellPanel(style = paste0("background: ", obj_bg),
                                           h3("Objective 5 - Probability 101"),
                                           p(module_text["obj_04", ])
                                 )
                          ),
                        ),
                        fluidRow(
                          hr(),
                          column(4,
                                 h3("Probability 101"),
                                 p("We will go over some key probability concepts"),
                                 p("What is probability?"),
                                 tags$ul(
                                   tags$li(module_text["what_forecast", ])
                                 ),
                                 p("What is a sample?"),
                                 tags$ul(
                                   tags$li(module_text["why_forecast", ])
                                 ),
                                 p("What is a statistical parameter"),tags$ul(
                                   tags$li(module_text["how_forecast", ])
                                 ),
                                 p("Click through the slides to recap some of the main points from the lecture.")
                          ),
                          column(8, offset = 0, align = "center",
                                 h3("Key Figures",
                                    align = "center"),
                                 h5("Click the arrows to navigate through the slides", align = "center"),
                                 wellPanel(
                                   # slickROutput("slides", width = "600px", height = "450px")
                                 )
                          )
                        ),
                        fluidRow(
                          column(6,
                                 h3("Linear Regression"),
                                 p("We will explore the relationship between air temperature and surface water temperature")
                          ),
                          column(6,
                                 h4("Interactive Linear Regression plot..."),
                                 wellPanel(

                                 )
                                 )
                          )
                        ),

               # 7. Activity A ----
               tabPanel(title = "Activity A", value = "mtab7",
                        img(src = "project-eddie-banner-2020_green.png", height = 100,
                            width = 1544, top = 5),
                        fluidRow(
                          column(12,
                                 wellPanel(style = paste0("background: ", obj_bg),
                                           h2("Activity A - Explore Uncertainty"),
                                           h3("Objective X - "),
                                           p(module_text["obj_04", ])
                                           )
                                 ),
                          ),
                        #* Initial Condition UC
                        fluidRow(
                          column(6,
                                 h4("Initial Condition Uncertainty"),
                                 p("Initial condition uncertainty is...")
                                 ),
                          column(6,
                                 h4("Some image for IC Uncertainty!")
                                 )
                          ),
                        fluidRow(
                          #** Weather Forecast ----
                          column(12, align = "center",
                                 h3("Weather Forecast"), hr()
                          ),
                        ),
                        fluidRow(
                          column(5,
                                 p(id = "txt_j", module_text["weather_forecast1", ]),
                                 p(id = "txt_j", HTML(paste0("Weather forecasts are produced using ",tags$b("ensemble modelling"), "."))),
                                 p(id = "txt_j", module_text["ens_mod1", ]),
                                 p(id = "txt_j", "Each simulation in an ensemble is called a _member_."),
                                 p(id = "txt_j", module_text["weather_forecast2", ])
                          ),
                          column(6,
                                 p("Some image of a weather forecast...")
                                 )
                          ),
                        fluidRow(
                          column(6,
                                 h4("Observational Error"),
                                 p("If you have 3 thermometers in this room, what are the chances they would all read the EXACT same measurement?"),
                                 p("Very unlikely! Why is that?"),
                                 p("Despite having high-tech equipment there is always going to be slight discrepancies between instruments."),
                                 p("Does this mean the instruments are wrong?"),
                                 p("No! But it means that even though the instruments are all measuring the same variable (air temperature), they will have slightly different readings as they are not in the exact same spot in space and time."),
                                 p("This is what is called _observational error_.")
                          ),
                          column(6,
                                 h4("Some image for observational error [3 thermometers showing different temps]")
                                 )
                          ),
                        fluidRow(
                          column(6,
                                 h4("NOAA Forecast data"),
                                 p("NOAA forecast data comes from GEFS etc."),
                                 actionButton("load_noaa_at", "Load forecast"),
                                 verbatimTextOutput("noaa_at_loaded"),
                                 checkboxInput("view_day0", "View observation"),
                                 conditionalPanel("input.view_day0",
                                                  checkboxInput("add_obs_uc", "Add observational uncertainty")
                                                  ),
                                 checkboxInput("view_ic", "View forecast initial conditions"),
                                 conditionalPanel("input.view_ic",
                                                  numericInput("noaa_n_mems", "Number of forecasts (0-30)", 1, 0, 30),
                                                  p("The initial conditions have been jittered to avoid points overlapping.")
                                                  ),
                                 checkboxInput("view_day7", "View forecast 7-days ahead"),
                                 conditionalPanel("input.view_day7",
                                                  radioButtons("add_to_plot", "Add to plot", choices = c("None", "Line", "Actual data", "Distribution"))
                                                  )
                                 ),
                          column(6,
                                 h4("Air temperature forecast"),
                                 plotlyOutput("noaa_at_plot")
                                 # plotOutput("noaa_at_plot")
                                 )
                          ),
                        # Initial Conditions - Primary prod model ----
                        fluidRow(
                          column(3,
                                 h4("How does initial condition uncertainty affect our forecasts of primary productivity?"),
                                 p("Adjust the level of uncertainty associated with the initial conditions and see how levels of high, medium and low affect
                                   forecasts of primary productivity."),
                                 p("We will use one weather forecast to drive the model but instead of using one value for the initial conditions we will use
                                   multiple to represent the uncertainty related to our measurement."),
                                 p("Can you think of occasions when initial condition uncertainty might be higher than others?"),
                                 numericInput("phy_ic_value", "Chlorophyll-a initial conditions", value = 4, min = 0.1, max = 10, step = 0.1),
                                 sliderInput("phy_ic_sd", "Chlorophyll-a standard deviation", min = 0.1, max = 1, value = 0.3, step = 0.05),
                                 numericInput("nut_ic_value", "Nurtients initial conditions", value = 1, min = 0.01, max = 5, step = 0.01),
                                 sliderInput("nut_ic_sd", "Nutrients standard deviation", min = 0.01, max = 1, value = 0.5, step = 0.05),
                                 p("Select how many samples you wish to use to run your forecast."),
                                 radioButtons("n_samp_ic", "No. of samples", choices = c(10, 20, 50, 75, 100), selected = character(0)),
                                 actionButton("gen_ic_dist", "Generate initial condition distributions")
                          ),
                          column(4,
                                 h3("Distributions of potential initial conditions"),
                                 plotOutput("ic_phy_dist_plot"),
                                 hr(),
                                 plotOutput("ic_nut_dist_plot")
                          ),
                          column(5,
                                 h4("IC UC"),
                                 p("Run forecast with IC UC"),
                                 actionButton("run_ic_fc", "Run forecast"),
                                 radioButtons("ic_fc_type", "Type of plot", choices = c("Line", "Distribution"), selected = character(0)),
                                 h4("IC FC Plot"),
                                 plotlyOutput("ic_fc_plot")
                                 )
                        ),
                        fluidRow(
                          column(6,
                          ),
                          column(6,
                          )
                        ),
                        hr(),
                        #* Model UC ----
                        fluidRow(
                          column(6,
                                 h4("Model Uncertainty"),
                                 p("Process uncertainty is uncertainty caused by an incomplete description of the process."),
                                 p("Below is a description of our model. But we know that rates of nutrient uptake (N_uptake) and phytoplankton death (Mortality) are variable and that our model has simplified these."),
                                 p(withMathJax("$$Phyto_{t+1} = Phyto_t + N_uptake - Mortality$$")),
                                 p("To account for the uncertainty these simplifications introduce to our model we can add in process noise (", tags$em("W"), ") to our equation:"),
                                 p(withMathJax("$$Phyto_{t+1} = Phyto_t + N_uptake - Mortality + W_t$$")),
                                 p("First we will explore the model sensitivity to the parameters. Adjust the parameters and investigate how the model responds."),
                                 radioButtons("proc_uc0", "Level of process uncertainty", choices = c("None", "Low", "Medium", "High"), selected = character(0)),
                                 actionButton("run_mod0", "Run model")
                          ),
                          column(6,
                                 plotlyOutput("run_mod0_plot")
                          )
                        ),
                        hr(),
                        fluidRow(
                          column(6,
                                 h4("Model Uncertainty"),
                                 p("Parameter uncertainty is related to how the parameter within the model are simplifications."),
                                 p("First we will explore the model sensitivity to the parameters. Adjust the parameters and investigate how the model responds."),
                                 sliderInput("mort_rate1", "Mortality rate", min = 0.01, max = 1, value = 0.6, step = 0.01),
                                 sliderInput("nut_uptake1", "Nutrient uptake", min = 0.01, max = 1, value = 0.3, step = 0.01),
                                 sliderInput("refTEMP1", "Reference temperature", min = 10, max = 30, value = 20, step = 1),
                                 actionButton("run_mod1", "Run model")
                          ),
                          column(6,
                                 h4("Some image for Parameter Uncertainty!"),
                                 plotlyOutput("run_mod1_plot"),
                                 DTOutput("run_mod1_pars")
                                 )
                          ),
                        hr(),
                        fluidRow(
                          column(3,
                                 h4("Add parameter uncertainty"),
                                 p("Now we will run a forecast with parameter uncertainty added...  "),
                                 numericInput("mort_rate2", "Mortality rate", min = 0.01, max = 1, value = 0.8, step = 0.01),
                                 checkboxInput("add_mort_uc", "Add uncertainty"),
                                 conditionalPanel("input.add_mort_uc",
                                                  sliderInput("mort_rate2_sd", "Standard deviation", min = 0.01, max = 0.4, value = 0.1, step = 0.01)
                                 ),
                                 numericInput("nut_uptake2", "Nutrient uptake", min = 0.01, max = 1, value = 0.2, step = 0.01),
                                 checkboxInput("add_nut_uc", "Add uncertainty"),
                                 conditionalPanel("input.add_nut_uc",
                                                  sliderInput("nut_uptake2_sd", "Standard deviation", min = 0.01, max = 0.4, value = 0.1, step = 0.01)
                                 )
                          ),
                          column(4,
                                 h4("Generate parameter distributions"),
                                 p("To add parameter uncertainty, we will need to 'Add uncertainty' in the form of standard deviation around the set parameter value."),
                                 radioButtons("n_samp_pars", "No. of samples", choices = c(10, 20, 50, 75, 100), selected = character(0)),
                                 actionButton("gen_param_dist", "Generate parameter distributions"),
                                 h4("Plot for Param UC"),
                                 plotOutput("mort_rate_dist_plot"),
                                 hr(),
                                 plotOutput("nut_uptake_dist_plot")
                                 ),
                          column(5,
                                 h3("Run Forecast - Parameter UC"),
                                 actionButton("run_pars_fc", "Run forecast"),
                                 radioButtons("pars_fc_type", "Type of plot", choices = c("Line", "Distribution"), selected = character(0)),
                                 plotlyOutput("pars_fc_plot")
                                 )
                          ),
                        hr(),
                        fluidRow(
                          column(6,
                                 h3("Driver Uncertainty"),
                                 p("Driver uncertainty is related to the uncertainty of the data which is used to drive the model e.g. weather forecast data."),
                                 p("Weather forecasts are generated using numerical weather prediction models. Weather forecasts are based on current weather observations, which are assimilated into the model’s framework and used to produce predictions for temperature, precipitation, and hundreds of other meteorological elements from the oceans to the top of the atmosphere."),
                                 p("Due to uncertainty related to the ", tags$b("initial conditions"), " of such models, they use an ensemble of initial conditions to start the model and generate a  multitude of potential future realizations. Each different realization within the ensemble is called a ", tags$b("member"), ". The weather forecast data we are using has 31 members in the ensemble.")
                          ),
                          column(6,
                                 h3("Driver UC Diagram"),
                                 )
                          ),
                        fluidRow(
                          column(4,
                                 h3("First we will run the model with just one member of the weather forecast."),
                                 p("Run the forecast multiple times and each time it will run the model with one randomly selected member."),
                                 p("A key part of a forecast is that it generates a ", tags$b("probabilistic"), " output. How will we generate a probabilistic output from a single model output?")                                 ),
                          column(8,
                                 h3("Plot of 1 Driver UC"),
                                 actionButton("run_driv_fc0", "Run forecast")
                                 )
                        ),
                        fluidRow(
                          column(5,
                                 h4("Driver data"),
                                 plotOutput("driv_fc_plot1"),
                                 actionButton("add_mem", "Add members")
                                 ),
                          column(5, offset = 1,
                                 h4("Primary Productivity Forecast"),
                                 plotOutput("driv_fc_plot0")
                                 )
                        )

                        ),

               # 8. Activity B ----
               tabPanel(title = "Activity B", value = "mtab8",
                        img(src = "project-eddie-banner-2020_green.png", height = 100,
                            width = 1544, top = 5),
                        fluidRow(
                          column(12,
                                 wellPanel(style = paste0("background: ", obj_bg),
                                           h2("Activity B - Explore Uncertainty"),
                                           h3("Objective X - "),
                                           p(module_text["obj_04", ])
                                           )
                                 ),
                          )
                        ),

               # 8. Activity C ----
               tabPanel(title = "Activity C", value = "mtab9",
                        img(src = "project-eddie-banner-2020_green.png", height = 100,
                            width = 1544, top = 5),
                        fluidRow(
                          column(12,
                                 wellPanel(style = paste0("background: ", obj_bg),
                                           h2("Activity C - Explore Uncertainty"),
                                           h3("Objective X - "),
                                           p(module_text["obj_04", ])
                                           )
                                 )
                          )
                        )
               )
    )
  }

shinyUI(ui)

# end
