# Load required libraries
suppressPackageStartupMessages(library(shiny, quietly = TRUE))
suppressPackageStartupMessages(library(shinydashboard, quietly = TRUE))
suppressPackageStartupMessages(library(rintrojs, quietly = TRUE))

# Functions required
source("textAreaInput2.R")

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
               tabPanel(introBox("Module Overview",
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
               )
               )
    )
  }

server <- function(input, output, session) {

}

shinyApp(ui, server)

# end
