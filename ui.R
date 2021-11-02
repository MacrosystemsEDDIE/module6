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

# Help documentation
help_text <- read.csv("data/help_text.csv", row.names = 1)

# Load text input
module_text <- read.csv("data/module_text.csv", row.names = 1, header = FALSE)

# Read in assessment questions
quest <- read.csv("data/student_questions.csv", row.names = 1)
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
    navbarPage(title = "Module 6: Understanding Uncertainty in Ecological Forecasts",
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
               withMathJax(), # NEEDS to be here for rendering eqn's in data.table

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
                          h2("Understanding Uncertainty in Ecological Forecasts"),
                          h3("Summary"),
                          p(id = "txt_j", module_text["eco_forecast", ]),
                          p(id = "txt_j", module_text["this_module", ]),
                          h3("Learning Outcomes"),
                          tags$line(),
                          tags$ul(
                            tags$li(id = "txt_j", module_text["LO1", ]),
                            tags$li(id = "txt_j", module_text["LO2", ]),
                            tags$li(id = "txt_j", module_text["LO3", ]),
                            tags$li(id = "txt_j", module_text["LO4", ]),
                            tags$li(id = "txt_j", module_text["LO5", ]),
                            tags$li(id = "txt_j", module_text["LO6", ])
                          )
                   ),
                   column(5, offset = 1,
                          br(), br(), br(),
                          h2("Module summary icon!"),
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
                                 p("The presentation accompanying this module covers the introduction to forecast uncertainty, sources of forecast uncertainty and the importance and relevance of quantifying uncertainty within ecological forecasts."),
                                 p("What is ecological forecast uncertainty?"),
                                 tags$ul(
                                   tags$li(module_text["uncertainty", ])
                                 ),
                                 p("Where does ecological forecast uncertainty come from?"),
                                 tags$ul(
                                   tags$li(module_text["uncert1", ])
                                 ),
                                 p("Why is uncertainty important to quantify for an ecological forecast?"),
                                 tags$ul(
                                   tags$li(module_text["why_important", ])
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
                                 p("Within the Introduction, Exploration, and Activities A, B and C tabs, there are questions for students to complete as part of this module. These can be completed by writing your answers into the text boxes within the green boxes. If you do not complete the module in one continuous sitting, you can download a file with your responses saved, which you can then upload when you return. When you finish the module, you can generate a report which will embed your answers and saved plots into a Word (.docx) file which you can download and make further edits to before submitting to your instructor."),
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
                                                textAreaInput2(inputId = "q2", label = quest["q2", 1]),
                                                # textAreaInput2(inputId = "q1", label = quest["q3", 1]),
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
                                 p(HTML(paste0('This module will introduce key concepts within Ecological forecasting through exploration of ', a(href = "https://www.neonscience.org/", "NEON (National Ecological Observation Network) data", target = "_blank"), ", building a model, and then generating a short-term ecological forecast.")))
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
                                 p("Complete objectives 1-3 to familiarize yourself with the data from your selected site and learn about the ecological model you will be using.")
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
                                                                   h4(quest["q4", 1]),
                                                                   p("If the information for your lake is not on the NEON website then you can input NA (Not Available) into the text box.")
                                                            )
                                                          ),
                                                          fluidRow(
                                                            column(4, offset = 1, align = "left", style = paste0("background: ", ques_bg),
                                                                   textInput(inputId = "q5a", label = quest["q4a", 1] , width = "90%"),
                                                                   textInput(inputId = "q5b", label = quest["q4b", 1], width = "90%"),
                                                                   textInput(inputId = "q5c", label = quest["q4c", 1], width = "90%")
                                                            ),
                                                            column(4, offset = 1, align = "left", style = paste0("background: ", ques_bg),
                                                                   textInput(inputId = "q5d", label = quest["q4d", 1] , width = "90%"),
                                                                   textInput(inputId = "q5e", label = quest["q4e", 1], width = "90%"),
                                                                   textInput(inputId = "q5f", label = quest["q4f", 1], width = "90%")
                                                            )
                                                          )
                                                      )
                                               )
                                             ),
                                             fluidRow(
                                               column(5, offset = 1,
                                                      h3("Next step"),
                                                      p("We will explore the data which has been measured at this site by NEON."))
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
                                                      selectizeInput("view_var", "Select variable",
                                                                     choices = unique(neon_vars$Short_name),
                                                                     options = list(
                                                                       placeholder = 'Please select a variable',
                                                                       onInitialize = I('function() { this.setValue(""); }')),
                                                      ),
                                                      plotlyOutput("var_plot"),
                                                      useShinyjs(),  # Set up shinyjs
                                                      # splitLayout(cellWidths = c("75%", "25%"),
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

               # 5. Activity A ----
               tabPanel(title = "Activity A", value = "mtab5",
                        img(src = "project-eddie-banner-2020_green.png", height = 100,
                            width = 1544, top = 5),
                        fluidRow(
                          column(12,
                                 wellPanel(style = paste0("background: ", obj_bg),
                                           h2("Activity A - Build A Model With Uncertainty"),
                                           p(module_text["obj_03", ])
                                 )
                          ),
                        ),
                        tabsetPanel(id = "tabseries2",
                                    #* Objective 3 - Build a water temperature model ====
                                    tabPanel(title = "Objective 3 - Build a water temperature model", value = "obj3",
                                             fluidRow(
                                               column(12,
                                                      wellPanel(style = paste0("background: ", obj_bg),
                                                                h3("Objective 3 - Build a water temperature model"),
                                                                p(id = "txt_j", module_text["obj_03", ])
                                                      ))
                                             ),
                                             fluidRow(
                                               column(5,
                                                      h3("Statistics & Probability"),
                                                      p("Before we begin generating an ecological forecast with uncertainty we will define some key basic statistical and probability concepts relevant to ecology.")
                                               )
                                             ),
                                             fluidRow(
                                               column(5, offset = 1,
                                                      p("What is an ecological model?"),
                                                      tags$ul(
                                                        tags$li(module_text["model1", ])
                                                      ),
                                                      p("What is a parameter"),
                                                      tags$ul(
                                                        tags$li(module_text["parameter", ])
                                                      ),
                                                      p("What is a parameter distribution?"),
                                                      tags$ul(
                                                        tags$li(module_text["distribution", ])
                                                      )
                                               ),
                                               column(5, offset = 1,
                                                      p("What is a linear relationship?"),
                                                      tags$ul(
                                                        tags$li(module_text["linear_relationship", ])
                                                      ),
                                                      p("In our example, water temperature is the dependent variable and air temperature is the independent variable."),
                                                      p("What is model error"),
                                                      tags$ul(
                                                        tags$li(module_text["mod_error", ])
                                                      ),
                                                      p("What is a parameter distribution?"),
                                                      tags$ul(
                                                        tags$li(module_text["distribution", ])
                                                      )
                                                      ),
                                               column(4, offset = 0, align = "center",
                                                      h3("Key Figures",
                                                         align = "center"),
                                                      h5("Click the arrows to navigate through the slides", align = "center"),
                                                      wellPanel(
                                                        # slickROutput("slides", width = "600px", height = "450px")
                                                      )
                                               )
                                             ),
                                             hr(),
                                             #* Linear regression ----
                                             fluidRow(
                                               column(6,
                                                      h3("Investigate variable relationships"),
                                                      p("We will explore the relationship between air temperature and surface water temperature for a lake site."),
                                                      p("First, we will look at a time series of the real air and water temperature data measured at the lake you chose in the “Site selection” tab."),
                                                      actionButton("plot_airt_swt", "Plot"),
                                                      radioButtons("q12", quest["q12", ], choices = c("Yes", "No"), selected = character(0), inline = TRUE),
                                                      conditionalPanel("input.q12 == 'Yes'",
                                                                       p(tags$b("Good job!")),
                                                                       p("When there is a linear relationship we can use ", tags$b("linear regression"), " to model the variable."),
                                                                       br(),
                                                                       h4("Linear Regression"),
                                                                       div("The formula for a linear regression is: $$y = m \\times x + b$$"),
                                                                       p("In our case we will be using ", tags$b("air temperature"), " (airtemp) to model ", tags$b("water temperature"), " (wtemp) using the following model:"),
                                                                       div("$$wtemp = m \\times airtemp + b$$"),
                                                                       p("where the ", tags$b("parameters"), "of the model are ", tags$em("m"), "(the slope) and ", tags$em("b"), " (the intercept)."),
                                                                       p(tags$b("R-squared"), module_text["r_squared", ])
                                                      ),
                                                      conditionalPanel("input.q12 == 'No'",
                                                                       p(tags$em("Are you sure?"))
                                                      )
                                               ),
                                               column(6,
                                                      h4("Time series of air temperature and water temperature"),
                                                      p("Click on “Plot” to graph the time series of the air temperature and the surface water temperature. Compare the seasonal cycles of both the dependent variable (air temperature) and independent variable (water temperature)."),
                                                      wellPanel(
                                                        plotOutput("airt_swt_plot")
                                                      )
                                               )
                                             ),
                                             hr(),
                                             fluidRow(
                                               column(3,
                                                      h3("Investigate how collecting more data affects your model"),
                                                      p("As we collect more and more data at our lake site, the parameters for our linear model will likely change, affecting the model performance (assessed by the R-squared value). Fit a linear model with six varying amounts of data coverage and add each model’s parameter set to the parameter table."),
                                                      p("Toggle the buttons below to select a frequency of data collection which you would use to build your linear regression model."),
                                                      # " Use the time series plot above to guide your selection of dates. If there are values that look like a sensor malfunction then you can omit them from the selection."),
                                                      radioButtons("samp_freq", "Data collection frequency:", choices = samp_freq),
                                                      # uiOutput("date_slider1"),
                                                      actionButton("plot_airt_swt2", "Plot")
                                                      # p("Use the date slider to choose how much data is used to go into your model.")
                                               ),
                                               column(4,
                                                      p("For the linear regression model, ", tags$em("m"), " and ", tags$em("b"), "are ", tags$b("parameters.")),
                                                      p("You can also select a row in the table and then click 'Save line' to overwrite an entry."),
                                                      DTOutput("lr_DT", width = "100%"),
                                                      br(),
                                                      # p("You do not need to get the percentages exactly right, but close enough will work fine."),
                                                      actionButton("add_lm", "Get model parameters"),
                                                      wellPanel(
                                                        uiOutput("lm_mod")
                                                      ),
                                                      br(), br(),
                                                      box(id = "box2", width = 12, status = "primary",
                                                          solidHeader = TRUE,
                                                          fluidRow(
                                                            column(10, offset = 1,
                                                                   # h3("Questions"),
                                                                   textAreaInput2(inputId = "q13", label = quest["q13", ], width = "90%"),
                                                                   textAreaInput2(inputId = "q14", label = quest["q14", ], width = "90%")
                                                            )
                                                          ),
                                                      )
                                                      # verbatimTextOutput("lm_out")
                                               ),
                                               column(5,
                                                      h3("Air temperature vs. water temperature"),
                                                      wellPanel(
                                                        plotlyOutput("airt_swt_plot_lines")#,
                                                      )
                                               )
                                             ),
                                             fluidRow(
                                               column(3,
                                                      h3("Compare model performance"),
                                                      box(id = "box2", width = 12, status = "primary",
                                                          solidHeader = TRUE,
                                                          fluidRow(
                                                            column(10, offset = 1,
                                                                   # h3("Questions"),
                                                                   textAreaInput2(inputId = "q15", label = quest["q15", ], width = "90%"),
                                                                   textAreaInput2(inputId = "q16", label = quest["q16", ], width = "90%")
                                                            )
                                                          ),
                                                      )
                                               ),
                                               column(9,
                                                      h3("Water temperature time series"),
                                                      p("When you add your models to the table, they will appear here as lines with colors corresponding to the plot above."),
                                                      p("Use the interactivity of the plots to zoom in at different times of the year to inspect closer."),
                                                      wellPanel(
                                                        plotlyOutput("lm_ts_plot")
                                                        )
                                                      )
                                               )
                                             ),
                                    #* Objective 4 - Explore Parameter ====
                                    tabPanel(title = "Objective 4 - Explore Parameters", value = "obj4",
                                             fluidRow(
                                               column(12,
                                                      wellPanel(style = paste0("background: ", obj_bg),
                                                                h3("Objective 4 - Explore Parameters"),
                                                                p(id = "txt_j", module_text["obj_04", ])
                                                                )
                                                      )
                                               ),
                                             #** Generate distributions for intercept & slope ----
                                             fluidRow(
                                               column(12,
                                                      h3("Generate distributions for intercept & slope")
                                               ),
                                               column(4,
                                                      p("Using the values from the model parameters you fit above, you will calculate a normal distribution for the parameters."),
                                                      p("Calculate the mean and standard deviation of the parameters"),
                                                      DTOutput("lr_DT2", width = "100%"),
                                                      br(),
                                                      p("You must select rows in the data table before you can calculate the statistics."),
                                                      # actionButton("calc_stats", "Calculate!"),
                                                      # div(DTOutput("lr_stats"), style = "font-size: 50%; width: 50%"),
                                                      p("Generate plots of the normal distribution of the parameters (m and b) using the mean and standard deviation from the lines you created."),
                                                      module_text["density_plots", ],
                                                      p("Density plots are a variation of histograms and they are better at determining the distribution shape."),
                                                      br(), br(),
                                                      box(id = "box2", width = 12, status = "primary",
                                                          solidHeader = TRUE,
                                                          fluidRow(
                                                            column(10, offset = 1,
                                                                   textAreaInput2(inputId = "q17", label = quest["q17", ], width = "90%"),
                                                            )
                                                          ),
                                                      ),
                                                      p("You will use the parameter sliders to answer Q 19-20.")
                                               ),
                                               column(4, align = "center",
                                                      plotOutput("lr_m_dist_plot"),
                                                      sliderInput("m_std", "Slope (m) - Std. Dev.", min = 0, max = 0.5, value = 0.25, step = 0.01),
                                                      actionButton("gen_lr_dist_plot", "Generate plot!")
                                               ),
                                               column(4, align = "center",
                                                      plotOutput("lr_b_dist_plot"),
                                                      sliderInput("b_std", "Intercept (b) - Std. Dev.", min = 0, max = 1, value = 0.5, step = 0.05)
                                               )
                                             ),
                                             hr(),
                                             #* Adding multiple lines ----
                                             fluidRow(
                                               column(3,
                                                      h3("Create multiple models"),
                                                      p("Using the distributions you have created above, you are going to randomly create models by sampling values for the slope (m) and the intercept (b) from the distributions you have defined above for each parameter."),
                                                      radioButtons("n_samp", "No. of samples", choices = c(10, 20, 50, 75, 100), selected = character(0)),
                                                      actionButton("gen_lin_mods", "Add models"),
                                                      p("Every time you click 'Add models' it will generate models randomly from the sample distributions."),
                                                      conditionalPanel("input.gen_lin_mods >= 1",
                                                                       p("Using the properties of a normal distribution, we can calculate the confidence intervals of these samples and add this to our plot."),
                                                                       radioButtons("plot_type1", "Plot type", c("Line", "Distribution"),
                                                                                    inline = TRUE)
                                                      )
                                                      # checkboxInput("add_dist", "Distribution plot"))
                                               ),
                                               column(3,
                                                      DTOutput("lr_stats", width = "100%"),
                                                      DTOutput("mb_samps", width = "50%")
                                               ),
                                               column(6,
                                                      plotlyOutput("add_lin_mods")
                                               )
                                             ),
                                             fluidRow(
                                               column(12,
                                                      box(id = "box2", width = 12, status = "primary",
                                                          solidHeader = TRUE,
                                                          fluidRow(
                                                            column(12,
                                                                   h4("Questions")
                                                            ),
                                                            column(4,
                                                                   textAreaInput2(inputId = "q18", label = quest["q18", ], width = "90%")
                                                            ),
                                                            column(4,
                                                                   textAreaInput2(inputId = "q19", label = quest["q19", ], width = "90%")
                                                            ),
                                                            column(4,
                                                                   textAreaInput2(inputId = "q20", label = quest["q20", ], width = "90%")
                                                                   )
                                                            )
                                                          )
                                                      )
                                               )
                                             ),
                                    #* Objective 5 - Assess Model ====
                                    tabPanel(title = "Objective 5 - Assess Model", value = "obj5",
                                             fluidRow(
                                               column(12,
                                                      wellPanel(style = paste0("background: ", obj_bg),
                                                                h3("Objective 5 - Assess Model"),
                                                                p(id = "txt_j", module_text["obj_05", ])
                                                                )
                                                      )
                                               ),
                                             #** Calculation model error - deterministic ----
                                             fluidRow(
                                               column(6,
                                                      h3("Calculating model error"),
                                                      p("If you look at our linear model, we can see that the points do not fall exactly on our model’s predicted line. We can compare our model results to actual observations to determine how “good” the model is. Select points on the graph and the table below will display how far off the model was from the observations by calculating model error (difference between modelled water temperature and observed temperature)."),
                                                      h4("Selected points"),
                                                      p("Select a point on the plot by clicking on it or select multiple points by clicking and dragging the selection pane."),
                                                      DTOutput("click_dt", width = "90%"),
                                                      br(),
                                                      actionButton("calc_err", "Calculate"),
                                                      wellPanel(
                                                        textOutput("mean_err")
                                                      )
                                               ),
                                               column(6,
                                                      plotOutput("mod_err_plot",
                                                                 click = "mod_err_plot_click",
                                                                 brush = brushOpts(
                                                                   id = "mod_err_plot_brush"
                                                                 )),
                                               )
                                             ),
                                             fluidRow(
                                               column(12,
                                                      box(id = "box2", width = 12, status = "primary",
                                                          solidHeader = TRUE,
                                                          fluidRow(
                                                            column(12,
                                                                   h4("Questions"),
                                                                   p("Calculate the model error (difference between modelled water temperature and observed water temperature) at varying intervals (0-10, 10-20, 20-30, 30-40 degC).")
                                                            ),
                                                            column(6,
                                                                   textAreaInput2(inputId = "q21", label = quest["q21", ], width = "90%")
                                                            ),
                                                            column(6,
                                                                   textAreaInput2(inputId = "q22", label = quest["q22", ], width = "90%")
                                                            )
                                                          ),
                                                          #* Calculation model error - probabilistic ----
                                                          fluidRow(
                                                            column(6,
                                                                   h3("Calculating model error with uncertainty"),
                                                                   p("In Objective X, we noticed that there were multiple models that could potentially fit our data and this allowed us to create a plot with confidence intervals. Now we will assess how many of these observations fall within our confidence intervals, giving us a measure of how 'good' our model is."),
                                                                   p("Use the 'Lasso select' tool to highlight points outside of the confidence interval and input the number below."),
                                                                   textOutput("sel_points"),
                                                                   textOutput("total_points"),
                                                                   numericInput("points_above", "Number of points above the confidence interval", 0, min = 0, max = 1000, step = 1),
                                                                   numericInput("points_below", "Number of points below the confidence interval", 0, min = 0, max = 1000, step = 1),
                                                                   textOutput("pct_inside"),
                                                                   actionButton("calc_pct", "Calculate percentage points inside the confidence intervals."),
                                                                   p("Does the percentage of points inside your confidence intervals match your intervals?"),
                                                                   p("If not, why do you think that is?"),
                                                                   p("If you were to go back and repeat this exercise, what would you do differently?")
                                                            ),
                                                            column(6,
                                                                   plotlyOutput("mod_err_uc_plot"),
                                                                   actionButton("clear_sel1", "Clear selection")
                                                                   )
                                                            )
                                                          )
                                                      )
                                             )
                                    ),
                                    #* Objective 6 - Improve Model for Forecasting ====
                                    tabPanel(title = "Objective 6 - Improve Model for Forecasting", value = "obj6",
                                             fluidRow(
                                               column(12,
                                                      wellPanel(style = paste0("background: ", obj_bg),
                                                                h3("Objective 6 - Improve Model for Forecasting"),
                                                                p(id = "txt_j", module_text["obj_06", ])
                                                      )
                                               )
                                             ),
                                             fluidRow(
                                               column(12,
                                                      h3("Create a Forecast model"),
                                               ),
                                               column(3,
                                                      p("The model we have built uses the current air temperature to predict the current water temperature. But, if we want to make a forecast of future water temperature, we would be unable to use this model unless we used forecasted (future) air temperature."),
                                                      p("The simplest forecast model that we can create is to predict that tomorrow's water temperature will be the same as today’s water temperature, with t = tomorrow and t-1 = today:"),
                                                      wellPanel(
                                                        div("$$wtemp_{t} = wtemp_{t-1}$$")
                                                      ),
                                                      p("Let's plot this model versus observations. Adjust the date slider below to choose which period to plot this model for. The R-squared value will be calculated for the plotted data."),
                                                      uiOutput("date_persist")
                                               ),
                                               column(9,
                                                      wellPanel(
                                                        plotlyOutput("persist_plot"),
                                                        verbatimTextOutput("persist_r2")
                                                      )
                                               )
                                             ),
                                             hr(),
                                             fluidRow(
                                               column(12,
                                                      h3("Build a ", tags$em("Better"), " model"),
                                               ),
                                               column(3,
                                                      p("The model we have built depends on the current air temperature. But, if we want to make a forecast of water temperature, we would be unable to use this model unless we used forecasted air temperature."),
                                               ),
                                               column(3,
                                                      p("We are going to build a model that uses historical data, which will allow us to use historical data to forecast water temperature."),
                                                      p("We will use historical air temperature and water temperature. You will have the option of either using them with days ahead (e.g. days ahead of 1 would be using data from 1 day ago to predict today's value) or a rolling mean (e.g. a mean of 3 would be using a the average over the previous 3 days).")
                                               ),
                                               column(6,
                                                      h3("PLACEHOLDER")
                                               )
                                             ),
                                             fluidRow(
                                               column(3,
                                                      p("Build a multiple regression model below and test adding different predictors and see how well your model works at forecasting water temperature."),
                                                      wellPanel(
                                                        div("$$y = \\beta _{1}x_{1} + \\beta _{2}x_{2} + ... + b$$")
                                                      ),
                                                      p("where \\(\\beta_{n}\\) represents the parameters in the model, similarly to the slope in a linear regression model."),
                                                      selectInput("mult_lin_reg_vars", "Select predictors", choices = lin_reg_vars$Name, multiple = TRUE),
                                                      numericInput("lag_t", "Days ahead", value = 1, min = 1, max = 7, step = 1),
                                                      numericInput("mean_t", "Mean (days)", value = 1, min = 1, max = 7, step = 1),
                                               ),
                                               column(3,
                                                      h4("Fit multiple linear regression model"),
                                                      p("Use a multiple linear regression model to estimate the parameters (\\(\\beta_{n}\\)) in your model below."),
                                                      wellPanel(
                                                        uiOutput("mult_lin_reg_eqn")
                                                      ),
                                                      # verbatimTextOutput("mlr_invis"),
                                                      br(),
                                                      actionButton("fit_mlr", "Fit model"),
                                                      uiOutput("date_train"),
                                                      uiOutput("date_test")
                                               ),
                                               column(6,
                                                      plotlyOutput("mlr_ts_plot"),
                                                      wellPanel(
                                                        uiOutput("mlr_mod")
                                                      )
                                               )
                                             ),
                                             fluidRow(
                                               column(6,
                                                      box(id = "box2", width = 12, status = "primary",
                                                          solidHeader = TRUE,
                                                          fluidRow(
                                                            column(12,
                                                                   h4("Questions")
                                                            )
                                                          ),
                                                          fluidRow(
                                                            column(4,
                                                                   textAreaInput2(inputId = "q23", label = quest["q23", ], width = "90%")
                                                            ),
                                                            column(4,
                                                                   textAreaInput2(inputId = "q24", label = quest["q24", ], width = "90%")
                                                            )
                                                          ),
                                                          fluidRow(
                                                            column(4,
                                                                   textAreaInput2(inputId = "q25", label = quest["q25", ], width = "90%")
                                                            ),
                                                            column(4,
                                                                   textAreaInput2(inputId = "q26", label = quest["q26", ], width = "90%")
                                                            ),
                                                            column(4,
                                                                   textAreaInput2(inputId = "q27", label = quest["q27", ], width = "90%")
                                                            )
                                                          ),
                                                          fluidRow(
                                                            column(4,
                                                                   textAreaInput2(inputId = "q28", label = quest["q28", ], width = "90%")
                                                            ),
                                                            column(4,
                                                                   textAreaInput2(inputId = "q29", label = quest["q29", ], width = "90%")
                                                            )
                                                          )
                                                      )
                                               ),
                                               column(6,
                                                      DTOutput("mlr_dt")
                                                      )
                                               )
                                             )
                                    )
                        ),
               # 6. Activity B ----
               tabPanel(title = "Activity B", value = "mtab7",
                        img(src = "project-eddie-banner-2020_green.png", height = 100,
                            width = 1544, top = 5),
                        fluidRow(
                          column(12,
                                 wellPanel(style = paste0("background: ", obj_bg),
                                           h2("Activity B - Explore Forecast Uncertainty"),
                                           p(module_text["obj_04", ])
                                 )
                          ),
                        ),
                        tabsetPanel(id = "tabseries3",
                                    #* Objective 7 - Model Uncertainty ====
                                    tabPanel(title = "Objective 7 - Model Uncertainty", value = "obj7",
                                             fluidRow(
                                               column(12,
                                                      wellPanel(style = paste0("background: ", obj_bg),
                                                                h3("Objective 7 - Model Uncertainty"),
                                                                p(id = "txt_j", module_text["obj_06", ])
                                                                )
                                                      )
                                             ),
                                             #* Model Uncertainty ----
                                             #** Process Uncertainty ----
                                             fluidRow(
                                               column(6,
                                                      h3("Model Uncertainty"),
                                                      p("We will now generate forecasts of water temperature."),
                                                      p("We will use the models that you developed in the last Objective to explore model uncertainty,"),
                                                      p("We will focus on three types of uncertainty associated with model uncertainty as we generate water temperature forecasts:"),
                                                      tags$ol(
                                                        tags$li("Model selection"),
                                                        tags$li("Process"),
                                                        tags$li("Parameter")
                                                      )
                                               ),
                                               column(6,
                                                      h2("IMAGE OF MODEL UNCERTAINTY!")
                                               )
                                             ),
                                             fluidRow(
                                               column(6,
                                                      h3("Model Selection Uncertainty"),
                                                      p(module_text["mod_selec_uc", ]),
                                                      p("To account for model selection uncertainty, we will use three different models, which you developed in the last objective, to generate 7-day ahead forecasts of water temperature at your site."),
                                                      p("Select a model from the table below and then click 'Run Forecast' to add a forecast to the plot"),
                                                      div("$$ wtemp_{t+1} = ?? $$"),
                                                      p("Depending on your model selection, the necessary required driving variables are shown."),
                                                      actionButton("load_mods", "Load models"),
                                                      DTOutput("mod_selec_tab")
                                               ),
                                               column(6,
                                                      wellPanel(
                                                        plotlyOutput("wtemp_fc1")
                                                      ),
                                                      wellPanel(
                                                        actionButton("load_driv1", "Load driver data"),
                                                        actionButton("run_wtemp_fc1", "Run forecast"),
                                                        uiOutput("sel_mod"),
                                                        textOutput("txt_fc_out")
                                                      )
                                               )
                                             ),
                                             hr(),
                                             #** Model Process Uncertainty ----
                                             fluidRow(
                                               column(6,
                                                      h3("Model Process Uncertainty"),
                                                      p(module_text["proc_uc", ]),
                                                      p("Below is a description of our \"simple\" model. But we know that rates of nutrient uptake (N_uptake) and phytoplankton death (Mortality) are variable and that our model has simplified these."),
                                                      p(withMathJax("$$ wtemp_{t+1} = wtemp_{t} $$")),
                                                      p("To account for the uncertainty these simplifications introduce to our model we can add in process noise (", tags$em("W"), ") to our model:"),
                                                      p(withMathJax("$$ wtemp_{t+1} = wtemp_{t} + W_t $$")),
                                                      p("Water temperature tomorrow is equal to water temperature today ", tags$b("plus"), " some noise ", tags$em("(W)"), "."),
                                                      p(withMathJax("$$W_t = N(0, Std. Dev)$$")),
                                                      p("where process noise is equal to a random number with a mean of zero and some standard deviation."), br(),
                                                      p("First we will explore how the model responds to differing levels of process uncertainty. Run the model with each of the differing levels multiple time and observe how the forecast outcome changes."),
                                                      radioButtons("proc_uc1", "Level of process uncertainty", choices = c("None", "Low", "Medium", "High"), selected = character(0)),
                                                      conditionalPanel("input.proc_uc1 != ''",
                                                                       p("To account for uncertainty in the noise, we can run the model multiple times with random noise added to each model run. More noise is associated with high process uncertainty, and vice versa. Using multiple model runs is called an ", tags$b("ensemble."), " Each individual run is referred to as an ensemble ", tags$b("member."), "Forecasters typically run tens to hundreds of ensemble members to build uncertainty in their forecasts."),
                                                                       p("Using the slider below, adjust the number of members to see how process uncertainty changes with forecast horizon."),
                                                                       sliderInput("n_mem1", "No. of members", min = 1, max = 100, value = 5, step = 5)
                                                      )
                                               ),
                                               column(6,
                                                      plotlyOutput("wtemp_fc2"),
                                                      actionButton("run_wtemp_fc2", "Run forecast"),
                                                      conditionalPanel("input.run_wtemp_fc2 > 0",
                                                                       p("Using the properties of a normal distribution, we can calculate the confidence intervals of these samples and use this to visualize uncertainty in our forecast."),
                                                                       radioButtons("plot_type2", "Plot type", c("Line", "Distribution"),
                                                                                    inline = TRUE),
                                                                       p("Click on items in the legend to show/hide them from the plot.")
                                                                       )
                                                      )
                                               ),
                                             #** Model Parameter Uncertainty ----
                                             fluidRow(
                                               column(6,
                                                      h3("Model Parameter Uncertainty"),
                                                      p(module_text["param_uncert", ]),
                                               ),
                                               column(6,
                                                      h2("Some image of parameter uncertainty")
                                               ),
                                             ),
                                             fluidRow(
                                               column(6,
                                                      h3("Forecasting with linear regression model"),
                                                      p("We just received a weather forecast data from NOAA. It is a 7-day forecast of air temperature at our site. Click the button below to view it."),
                                                      actionButton("view_at_fc", "View forecast"),
                                                      p("With this air temperature forecast we can use the linear regression model we built in Objective X, to forecast water temperature:"),
                                                      wellPanel(
                                                        uiOutput("lr_mod_eqn")
                                                      )
                                               ),
                                               column(6,
                                                      plotlyOutput("airt1_fc_plot"),
                                                      actionButton("run_wtemp_fc3a", "Run forecast")
                                               ),
                                             ),
                                             fluidRow(
                                               column(3,
                                                      h3("Forecasting with Parameter Uncertainty"),
                                                      p("But we know from Objective X, that when we used different sets of data to build our model that we got slightly different values for our model parameters ", tags$em("m"), " and ", tags$em("b"), ". To account for this ", tags$b("uncertainty"), " we built distributions for parameter that we can sample from."),
                                                      p("We will revisit those samples and generate an ", tags$b("ensemble forecast"), " using multiple different potential parameters."),
                                                      sliderInput("n_mem3b", "No. of members", min = 1, max = 100, value = 5, step = 5)
                                               ),
                                               column(3,
                                                      plotOutput("param_fcast3b"),
                                                      p("Select the number of parameter to be used in the forecast ensemble.")
                                               ),
                                               column(6,
                                                      plotlyOutput("wtemp_fc3b"),
                                                      actionButton("run_wtemp_fc3b", "Run forecast"),
                                                      conditionalPanel("input.run_wtemp_fc3b > 0",
                                                                       p("Using the properties of a normal distribution, we can calculate the confidence intervals of these samples and use this to visualize uncertainty in our forecast."),
                                                                       radioButtons("plot_type3b", "Plot type", c("Line", "Distribution"),
                                                                                    inline = TRUE)
                                                                       )
                                                      )
                                               )
                                             ),
                                    #* Objective 8 - Initial Conditions Uncertainty ====
                                    tabPanel(title = "Objective 8 - Initial Conditions Uncertainty", value = "obj08",
                                             fluidRow(
                                               column(12,
                                                      wellPanel(style = paste0("background: ", obj_bg),
                                                                h3("Objective 8 - Initial Conditions Uncertainty"),
                                                                p(id = "txt_j", module_text["obj_08", ])
                                                                )
                                                      )
                                               ),
                                             fluidRow(
                                               column(3,
                                                      h3("Initial Condition Uncertainty"),
                                                      p(module_text["init_uncert", ]),
                                                      p("Even though we have measurements of water temperature from our lake, we know that water temperature varies throughout the day so this measurement might not capture exactly the temperature in our lake at this time. To account for this we can generate a distribution around this value and then run our model with slightly different initial conditions to account for this uncertainty."),
                                                      p("Use the slider below to adjust the standard deviation and then generate a normal distribution around the observation"),
                                                      sliderInput("ic_uc", "Standard deviation", min = 0.01, max = 0.5, value = 0.1, step = 0.05),
                                                      actionButton("gen_ic", "Generate distribution")
                                               ),
                                               column(3,
                                                      plotOutput("ic_uc_plot"),
                                                      sliderInput("n_mem4", "No. of members", min = 1, max = 100, value = 5, step = 5)
                                               ),
                                               column(6,
                                                      wellPanel(
                                                        plotlyOutput("wtemp_fc4"),
                                                      ),
                                                      actionButton("run_wtemp_fc4", "Run forecast"),
                                                      conditionalPanel("input.run_wtemp_fc4 > 0",
                                                                       p("Using the properties of a normal distribution, we can calculate the confidence intervals of these samples and use this to visualize uncertainty in our forecast."),
                                                                       radioButtons("plot_type4", "Plot type", c("Line", "Distribution"),
                                                                                    inline = TRUE)
                                                                       )
                                                      )
                                               )
                                             ),
                                    #* Objective 9 - Driver Uncertainty ====
                                    tabPanel(title = "Objective 9 - Driver Uncertainty", value = "obj09",
                                             fluidRow(
                                               column(12,
                                                      wellPanel(style = paste0("background: ", obj_bg),
                                                                h3("Objective 10 - Driver Uncertainty"),
                                                                p(id = "txt_j", module_text["obj_09", ])
                                                      )
                                               )
                                             ),
                                             fluidRow(
                                               column(6,
                                                      h3("Driver Uncertainty"),
                                                      p(module_text["driver_uncert", ]),
                                                      p("Load the NOAA ensemble forecast for air temperature below."),
                                                      actionButton("load_noaa_at", "Load forecast"),
                                                      verbatimTextOutput("noaa_at_loaded"),
                                                      p("You can adjust the number of ensemble members plotted and these are what you will use to drive your model."),
                                                      numericInput("noaa_n_mems", "Number of forecasts (0-30)", 3, 1, 30),
                                               ),
                                               column(6,
                                                      wellPanel(
                                                        plotlyOutput("airt_fc5"),
                                                        actionButton("run_wtemp_fc5", "Run forecast"),
                                                        radioButtons("plot_type5", "Plot type", c("Line", "Distribution"),
                                                                     inline = TRUE),
                                                        plotlyOutput("wtemp_fc5")
                                                        )
                                                      )
                                               )
                                             )
                                    )
                        ),

               # 6. Activity C ----
               tabPanel(title = "Activity C", value = "mtab7",
                        img(src = "project-eddie-banner-2020_green.png", height = 100,
                            width = 1544, top = 5),
                        fluidRow(
                          column(12,
                                 wellPanel(style = paste0("background: ", obj_bg),
                                           h2("Activity C - Managing/Reducing/Accounting Uncertainty"),
                                           p(module_text["obj_04", ])
                                           )
                                 ),
                          ),
                        #* Initial Condition UC
                        fluidRow(
                          column(6,
                                 h4("Initial Condition Uncertainty"),
                                 p(module_text["init_uncert",])
                                 ),
                          column(6,
                                 h4("Some image for IC Uncertainty!")
                                 )
                          ),
                        hr(),
                        fluidRow(
                          #** Weather Forecast ----
                          column(12, align = "center",
                                 h3("Weather Forecast")
                          ),
                        ),
                        fluidRow(
                          column(5,
                                 p(id = "txt_j", module_text["weather_forecast1", ]),
                                 p(id = "txt_j", HTML(paste0("Weather forecasts are produced using ",tags$b("ensemble modelling"), "."))),
                                 p(id = "txt_j", module_text["ens_mod1", ]),
                                 p(id = "txt_j", "Each simulation in an ensemble is called a ", tags$b("member.")),
                                 p(id = "txt_j", module_text["weather_forecast2", ])
                          ),
                          column(6, align = "center",
                                 img(src = "weather_fc.png", width = "90%", id = "bla_border",
                                     align = "center")
                                 )
                          ),
                        hr(),
                        fluidRow(
                          column(6,
                                 h4("Observational Uncertainty"),
                                 p("If you have 3 thermometers in this room, what are the chances they would all read the EXACT same measurement?"),
                                 radioButtons("obs_err1", "", choices = c("Unlikely", "50/50", "Very likely"), selected = character(0), inline = TRUE),
                                 conditionalPanel("input.obs_err1 == '50/50'",
                                                  p("Really?")
                                 ),
                                 conditionalPanel("input.obs_err1 == 'Very likely'",
                                                  p(tags$em("Are you sure?"))
                                 ),
                                 conditionalPanel("input.obs_err1 == 'Unlikely'",
                                                  p("Very unlikely! Why is that?"),
                                                  p("Despite having high-tech equipment there is always going to be slight discrepancies between instruments."),
                                                  p("Does this mean the instruments are wrong?"),
                                                  radioButtons("obs_err2", "", choices = c("No", "Yes"), selected = character(0), inline = TRUE),
                                                  conditionalPanel("input.obs_err2 == 'Yes'",
                                                                   p("Well you would hope not considering how much money you spent on them!"),
                                                  ),
                                                  conditionalPanel("input.obs_err2 == 'No'",
                                                                                    p("Exactly! But it means that even though the instruments are all measuring the same variable (air temperature), they will have slightly different readings as they are not in the exact same spot in space and time."),
                                                                                    p("This is what is called ", tags$b("observational uncertainty"), ".")
                                                                   )
                                                  )
                                 ),
                          column(6,
                                 h4("Some image for observational uncertainty [3 thermometers showing different temps]")
                                 )
                          ),
                        fluidRow(
                          column(6,
                                 h4("NOAA Forecast data"),
                                 p(id = "txt_j", "Here we will load in data from a ", a(href = "https://www.ncdc.noaa.gov/data-access/model-data/model-datasets/global-ensemble-forecast-system-gefs", "NOAA GEFS", target = "_blank"), " forecast for the NEON site you chose in Activity A."),
                                 img(src = "noaa_logo.jpg", height = "20%",
                                     width = "20%", align = "right"),
                                 # actionButton("load_noaa_at", "Load forecast"),
                                 # verbatimTextOutput("noaa_at_loaded"),
                                 checkboxInput("view_day0", "View observation"),
                                 conditionalPanel("input.view_day0",
                                                  checkboxInput("add_obs_uc", "Add observational uncertainty")
                                                  ),
                                 checkboxInput("view_ic", "View forecast initial conditions"),
                                 conditionalPanel("input.view_ic",
                                                  # numericInput("noaa_n_mems", "Number of forecasts (0-30)", 1, 0, 30),
                                                  p(tags$b("Note:"), "The initial conditions have been jittered to avoid points overlapping. All forecasts start at the same time.")
                                                  ),
                                 checkboxInput("view_day7", "View forecast 7-days ahead"),
                                 conditionalPanel("input.view_day7",
                                                  radioButtons("add_to_plot", "Add to plot", choices = c("None", "Line", "Forecast members", "Forecast distribution")),
                                                  radioButtons("noaa_timestep", "Timestep of forecasts", choices = c("Hourly", "Daily mean"), inline = TRUE)
                                                  )
                                 ),
                          column(6,
                                 h4("Air temperature forecast"),
                                 plotlyOutput("noaa_at_plot")
                                 )
                          ),
                        br(),
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
                                 h4("Initial Conditions Uncertainty"),
                                 p("Run forecast with initial conditions uncertainty"),
                                 actionButton("run_ic_fc", "Run forecast"),
                                 radioButtons("ic_fc_type", "Type of plot", choices = c("Line", "Distribution"), selected = character(0)),
                                 h4("Forecast generated with initial condition uncertainty"),
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
                                 p("To account for the uncertainty these simplifications introduce to our model we can add in process noise (", tags$em("W"), ") to our model:"),
                                 p(withMathJax("$$Phyto_{t+1} = Phyto_t + N_uptake - Mortality + W_t$$")),
                                 p("Phytoplankton concentration tomorrow is equalt to phytoplankton concentration today ", tags$b("plus"), " nutrient uptake and ", tags$b("minus"), " mortality."),
                                 p(withMathJax("$$W_t = N(0, Std. Dev)$$")),
                                 p("where process noise is equal a random number with a mean of zero and some standard deviation."), br(),
                                 p("First we will explore how the model responds to differing levels of process uncertainty. Run the models with each of the differing levels multiple time and observe how the forecast outcome changes."),
                                 radioButtons("proc_uc0", "Level of process uncertainty", choices = c("None", "Low", "Medium", "High"), selected = character(0)),
                                 actionButton("run_mod0", "Run model")
                          ),
                          column(6,
                                 plotOutput("proc_uc_plot"),
                                 hr(),
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
                          ),
                        fluidRow(
                          column(5,
                                 h4("Probabilistic forecast"),
                                 p("We have created a forecast with multiple different realizations of the future. How do we calculate a ", tags$em("probabilistic"), " forecast from these different realizations?"),
                                 p("We can calculate descriptive statistics about the distribution of the forecasts (e.g. mean and standard deviation) and then use that to calculate different levels of confidence."),
                                 p("Calculate the daily mean and standard deviation from the forecasts above."),
                                 p("Use these calculations to convert")
                          ),
                          column(5, offset = 1,
                                 h4("Primary Productivity Forecast"),
                                 )
                          )
                        )
               )
    )
  }

shinyUI(ui)

# end
