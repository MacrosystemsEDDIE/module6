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
    tags$html(lang = "en"), # Add language attribute
    tags$head(tags$link(rel = "shortcut icon", href = "macroeddi_ico_green.ico")), # Add icon for web bookmarks
    tags$head(includeHTML(("google-analytics.html"))),
    fluidPage(
      column(10,
             br(),
             p(tags$b("Teaching materials associated with this module can be found at ",
                      tags$a(href="http://module6.macrosystemseddie.org", 
                             "http://module6.macrosystemseddie.org.", target="_blank")))
      )
    ),
    navbarPage(title = tags$b("Module 6: Understanding Uncertainty in Ecological Forecasts"),
               position = "static-top", id = "maintab",
               tags$header(
                 fluidRow(
                   column(11,
                          bookmarkButton(id = "bookmarkBtn", label = "Bookmark my progress"),
                          br(), 
                          p(tags$em("At any time, use this button to obtain a link that saves your progress."))
                   ),
                   column(1, align = "right",
                          introBox(
                            actionButton("help", label = "Help", icon = icon("question-circle")), data.step = 7, data.intro = help_text["help", 1]
                          )
                   )
                 )
                ),
               # 1. Module Overview ----
               tabPanel(introBox(tags$b("Overview"),
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
                          img(src = "Schematic_Draft_v3.png", height = "80%",
                              width = "80%", align = "left")
                          )
                   ), data.step = 8, data.intro = help_text["start", 1]
                 )
               ),

               # 2. Presentation recap ----
               tabPanel(title = tags$b("Presentation"), value = "mtab2",
                        img(src = "project-eddie-banner-2020_green.png", height = 100,
                            width = 1544, top = 5),
                        fluidRow(
                          hr(),
                          column(4,
                                 h3("Presentation"),
                                 p(id = "txt_j", "The presentation accompanying this module covers the introduction to forecast uncertainty, sources of forecast uncertainty and the importance and relevance of quantifying uncertainty within ecological forecasts."),
                                 p(id = "txt_j", tags$b("What is ecological forecast uncertainty?")),
                                 tags$ul(
                                   tags$li(module_text["uncertainty", ])
                                 ),
                                 p(tags$b("Where does ecological forecast uncertainty come from?")),
                                 tags$ul(
                                   tags$li("Uncertainty comes from natural variability in the environment and imperfect representation of an ecological system in a model. When generating a forecast, uncertainty can come from the ",tags$b("structure")," of the model used, the ",tags$b("initial conditions")," of the model, the ",tags$b("parameters")," of the model, and the ",tags$b("data")," used to drive the model, among other sources.")
                                 ),
                                 p(id = "txt_j", tags$b("Why is uncertainty important to quantify for an ecological forecast?")),
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
                                   slickROutput("slides", width = "600px", height = "450px")
                                 )
                          )
                        )
               ),

               # 3. Introduction ----
               tabPanel(title = tags$b("Introduction"), value = "mtab3",
                        # tags$style(type="text/css", "body {padding-top: 65px;}"),
                        img(src = "project-eddie-banner-2020_green.png", height = 100,
                            width = 1544, top = 5),
                        fluidRow(
                          column(10, 
                                 h3("Workflow for this module"),
                                 tags$ol(
                                   tags$li(id = "txt_j", module_text["workflow1", ]),
                                   tags$li(id = "txt_j", module_text["workflow2", ]),
                                   tags$li(id = "txt_j", module_text["workflow3", ]),
                                   tags$li(id = "txt_j", module_text["workflow4", ])
                                 )
                          )
                        ), hr(),
                        fluidRow(
                          column(6, 
                                 h3("Student Handout"),
                                 p("Within the Introduction and Activities A, B and C tabs there are questions for students to complete as part of this module. These can be completed by writing your answers into the final report template, which can be downloaded as a Word document (.docx) below."),
                                 tags$style(type="text/css", "#stud_dl {background-color:#579277;color: white}"),
                                 conditionalPanel("output.handoutbuilt",
                                                  downloadButton(outputId = "stud_dl", label = "Download Final Report Template")
                                 )
                          ),
                          column(6,
                                 h3("Saving your progress"),
                                 p(style="text-align: justify;", "As you go, fill out answers to questions in the final report Word document. Some of the plots you generate in the Shiny app will be needed for the final report. When prompted, be sure to download these plots so you can copy-paste them into the final report."),
                                 p(style="text-align: justify;", "If you run out of time to finish all the activities you can save your progress and return to it at a later date. Click the 'Bookmark my progress' button at the top of the page and you will obtain a link, which you should save by copy-pasting it at the top of your final report. When you are ready to resume work, paste the link into your web browser, and it will load a Shiny app session that contains your progress."),
                                 br()
                          )
                        ),
                        hr(),
                        fluidRow(
                          hr(),
                          column(10, align = "left",
                                 box(id = "box1", width = 10, status = "primary",
                                     solidHeader = TRUE,
                                     fluidRow(
                                       column(8, offset = 1,
                                              h3("Before you start..."),
                                              p(id = "txt_j", "Download your final report (Word document) and input your name and Student ID. Then, answer the following questions in the final report."),
                                              introBox(
                                                h3(tags$b("Think about it!")),
                                                p(tags$b(quest["q1", 1])),
                                                p(tags$b(quest["q2", 1])),
                                                data.step = 5, data.intro = help_text["questions", 1]
                                              )
                                       )
                                     )

                                 )
                          )
                        ),
                        hr(),
                        fluidRow(
                          column(6,
                                 h3("Data sources"),
                                 p(id = "txt_j", HTML(paste0('This module will introduce key concepts within ecological forecasting through exploration of ', a(href = "https://www.neonscience.org/", "NEON (National Ecological Observatory Network) data", target = "_blank"), ", building a model, and then generating a short-term ecological forecast.")))
                          ),
                          column(6, align = "center",
                                 a(
                                   href = "https://www.neonscience.org/",
                                   img(src = "NSF-NEON-logo.png", title = "NEON - NSF logo"), target = "_blank"
                                   )
                                 )
                          )
                        ),


               # 5. Activity A ----
               tabPanel(title = tags$b("Activity A"), value = "mtab4",
                        img(src = "project-eddie-banner-2020_green.png", height = 100,
                            width = 1544, top = 5),
                        fluidRow(
                          column(12,
                                 wellPanel(style = paste0("background: ", obj_bg),
                                           h2("Activity A - Build A Model With Uncertainty"),
                                           p(module_text["act_A", ])
                                 )
                          )
                        ),
                        tabsetPanel(id = "tabseries1",
                                    #* Objective 1 - Select and view site ====
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
                                                      p(id = "txt_j", "Select a site in the table to highlight it on the map."),
                                                      conditionalPanel("input.row_num > 25",
                                                                       selectizeInput("row_num", "Select row",
                                                                                      choices = 1:nrow(neon_sites_df),
                                                                                      options = list(
                                                                                        placeholder = 'Please select a row',
                                                                                        onInitialize = I('function() { this.setValue(""); }'))
                                                                       )
                                                      ),
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
                                                      # textOutput("prompt1"),
                                                      wellPanel(
                                                        imageOutput("pheno"),
                                                        p(id = "txt_j", module_text["phenocam", ])
                                                      )
                                               )
                                             ), br(),
                                             #      span(textOutput("site_name1"), style = "font-size: 22px;
                                             # font-style: bold;"),
                                             fluidRow(
                                               wellPanel(
                                                 h4(tags$b("About Site")),
                                                 uiOutput("site_html"),
                                                 textOutput("prompt2"),
                                                 htmlOutput("site_link")
                                               )
                                             ),
                                             fluidRow(
                                               column(10, align = "left",
                                                      box(id = "box3", width = 10, status = "primary",
                                                          solidHeader = TRUE,
                                                          fluidRow(
                                                            column(7, offset = 1,
                                                                   h3("Questions"),
                                                                   h4(quest["q3", 1]),
                                                                   p(id = "txt_j", "If the information for your lake is not on the NEON website then you can input NA (Not Available) into the text box.")
                                                            )
                                                          ),
                                                          fluidRow(
                                                            column(4, offset = 1, align = "left", style = paste0("background: ", ques_bg),
                                                                   p(tags$b(quest["q3a", 1] , width = "90%")),
                                                                   p(tags$b(quest["q3b", 1], width = "90%")),
                                                                   p(tags$b(quest["q3c", 1], width = "90%"))
                                                            ),
                                                            column(4, offset = 1, align = "left", style = paste0("background: ", ques_bg),
                                                                   p(tags$b(quest["q3d", 1] , width = "90%")),
                                                                   p(tags$b(quest["q3e", 1], width = "90%")),
                                                                   p(tags$b(quest["q3f", 1], width = "90%"))
                                                            )
                                                          )
                                                      )
                                               )
                                             ),
                                             hr(),
                                             fluidRow(
                                               column(5, offset = 1,
                                                      h3("Next step"),
                                                      p("We will explore the water temperature data which has been measured at this site by NEON."))
                                             )
                                    ),
                                   
                                    #* Objective 2 - Explore water temperature ====
                                    tabPanel(title = "Objective 2 - Explore water temperature", value = "obj3",
                                             fluidRow(
                                               column(12,
                                                      wellPanel(style = paste0("background: ", obj_bg),
                                                                h3("Objective 2 - Explore water temperature"),
                                                                p(id = "txt_j", module_text["obj_02", ])
                                                      ))
                                             ),
                                             fluidRow(
                                               column(4,
                                                      h3("Water Temperature"),
                                                      p(id = "txt_j", tags$b("Water temperature")," exerts a major influence on biological activity and growth, has an effect on water chemistry, can influence water quantity measurements, and governs the kinds of organisms that live in water bodies."),
                                                      p(id = "txt_j", "Water temperature can have important effects on water quality, as changes in water temperature can directly or indirectly affect water quality variables such as dissolved oxygen, nutrient and heavy metal concentrations, and algae concentrations."),
                                                      p(id = "txt_j", "Freshwater ecosystems are currently experiencing a multitude of stressors such as land use change and climate change, which can affect water temperature."),
                                                      p(id = "txt_j", "Being able to predict how water temperature may change in the short-term (up to 7-days into the future) can provide natural resource managers with critical information to take pro-active actions to prevent degradation of water quality.")
                                                      ),
                                               column(8,
                                                     img(src = "lake_image2.jpg", height = "100%", id = "bla_border",
                                                         width = "100%", tags$style("border: solid 2px black;")),
                                                     br(),
                                                     br(),
                                                     box(id = "box4", width = 12, status = "primary",
                                                         solidHeader = TRUE,
                                                         fluidRow(
                                                           column(10, offset = 1,
                                                                  h3("Question"),
                                                                  p(tags$b(quest["q4", 1], width = "90%"))
                                                           )
                                                         )
                                                     )
                                                     )
                                             ),
                                             hr(),
                                             #* Time series plot ----
                                             fluidRow(
                                               column(5,
                                                      h3("Explore water temperature"),
                                                      p(id = "txt_j", "Click 'Plot water temperature' to view a time series of the real water temperature data measured at the lake you chose."),
                                                      fluidRow(
                                                        column(10, offset = 1,
                                                               actionButton("plot_airt_swt", "Plot water temperature")
                                                               )
                                                      ),
                                                      br(),
                                                      box(id = "box12", width = 12, status = "primary",
                                                          solidHeader = TRUE,
                                                          fluidRow(
                                                            column(10, offset = 1,
                                                                   h4("Questions"),
                                                                   p(tags$b(quest["q5", 1])),
                                                                   p(tags$b(quest["q6", 1])),
                                                                   selectInput("stat_calc", label = "Select calculation:", choices = stats0),
                                                                   wellPanel(
                                                                     textOutput("out_stats")
                                                                   )
                                                                   )
                                                          ),
                                                            fluidRow(
                                                              column(10, offset = 1,
                                                                     DTOutput("q6_tab")
                                                                     )
                                                            ),
                                                                   br(),
                                                      ),
                                                      hr(),
                                                      fluidRow(
                                                        br(),
                                                      ),
                                               ),
                                               column(7,
                                                      h4("Time series of water temperature"),
                                                      wellPanel(
                                                        plotlyOutput("airt_swt_plot")
                                                      )
                                               )
                                             ),
                                             hr(),
                                             fluidRow(
                                               column(5,
                                                      h3("Investigate variable relationships"),
                                                      p("Now, we will add air temperature data to the plot. Click 'Plot air temperature' to view the air temperature data at your site."),
                                                      fluidRow(
                                                        column(10, offset = 1,
                                                               actionButton("plot_airt_swt2", "Plot air temperature")
                                                        )
                                                      ),
                                                      br(),
                                                      box(id = "box12", width = 12, status = "primary",
                                                          solidHeader = TRUE,
                                                          fluidRow(
                                                            column(10, offset = 1,
                                                                   h4("Questions"),
                                                                   p(tags$b(quest["q7", 1])),
                                                                   selectInput("stat_calc1", label = "Select calculation:", choices = stats0),
                                                                   wellPanel(
                                                                     textOutput("out_stats1")
                                                                   )
                                                            )
                                                          ),
                                                          fluidRow(
                                                            column(10, offset = 1,
                                                                   DTOutput("q8_tab"),
                                                                   br(),
                                                                   p(tags$b(quest["q8", 1]))
                                                            )
                                                          ),
                                                          br(),
                                                      )
                                                      ),
                                               column(7,
                                                      h4("Time series of water temperature and air temperature"),
                                                      wellPanel(
                                                        plotlyOutput("airt_swt_plot1")
                                                      )
                                               )
                                             ),
                                             hr(),
                                             fluidRow(
                                               column(5, offset = 1,
                                                      h3("Next step"),
                                                      p(id = "txt_j", "We will build models that will allow us to predict water temperature."))
                                               )
                                             ),
                                    
                                    #* Objective 3 - Build models ====
                                    tabPanel(title = "Objective 3 - Build models", value = "obj3",
                                             fluidRow(
                                               column(12,
                                                      wellPanel(style = paste0("background: ", obj_bg),
                                                                h3("Objective 3 - Build models"),
                                                                p(id = "txt_j", module_text["obj_03", ])
                                                      )
                                               )
                                             ),
                                             #** Introduce models ----
                                             fluidRow(
                                               column(4,
                                                      h3("Before we build models..."),
                                                      p(tags$em("Use the slides and text below to understand the forecast models we will be using.")),
                                                      p(tags$b("What is an ecological model?")),
                                                      tags$ul(
                                                        tags$li(module_text["model1", ])
                                                      ),
                                                      p(tags$b("What is a model parameter?")),
                                                      tags$ul(
                                                        tags$li(module_text["parameter", ])
                                                      )
                                               ),
                                               column(8,
                                                      h5("Click the arrows to navigate through the slides", align = "center"),
                                                      wellPanel(
                                                        slickROutput("model_slides", width = "600px", height = "450px")
                                                      )
                                               )
                                             ),
                                             hr(),
                                             fluidRow(
                                               column(12,
                                                      h3("Understanding the forecast models we will use today"))
                                             ),
                                             fluidRow(
                                               column(4,
                                                      p(tags$b("What is a persistence model?")),
                                                      tags$ul(
                                                        tags$li("A persistence model predicts that tomorrow will be the same as today. Persistence models are often used as a type of 'null model', or baseline against which to compare other potential forecast models. Persistence models do not have any parameters.")
                                                      )
                                                      ),
                                               column(4,
                                                      p(tags$b("What is a linear regression model?")),
                                                      tags$ul(
                                                        tags$li(p("A linear regression model uses the values of one variable (the ",tags$b("independent variable; x"),") to predict another variable (the ",tags$b("dependent variable; y"),"), using two parameters."),
                                                                div("The formula for a linear regression is: $$y = m \\times x + b$$"),
                                                                p("where the ", tags$b("parameters"), "of the model are ", tags$em("m"), "(the slope) and ", tags$em("b"), " (the intercept).")
                                                        )
                                                      )
                                                      ),
                                               column(4,
                                                      p(tags$b("What is a multiple linear regression model?")),
                                                      tags$ul(
                                                        tags$li(p(tags$b("Multiple linear regression models")," are an extension of linear regression models. Multiple linear regression models have more than one independent variable. In a multiple linear regression model, each independent variable is associated with a ",tags$b("coefficient parameter,")," often represented using (\\(\\beta_{n}\\)). When this notation is used, the intercept parameter is represented as (\\(\\beta_{0}\\))."),
                                                                div("For example: $$y = \\beta _{0} + \\beta _{1}x_{1} + \\beta _{2}x_{2} + ... + \\beta _{n}x_{n}$$")
                                                        )
                                                      )
                                                      )
                                             ),
                                             hr(),
                                             fluidRow(
                                               column(10, align = "left",
                                                      box(id = "box10", width = 12, status = "primary",
                                                          solidHeader = TRUE,
                                                          fluidRow(
                                                            column(10, offset = 1,
                                                                   h3("Questions"),
                                                                   p(tags$b(quest["q9", 1])),
                                                                   p(tags$b(quest["q10", 1]))
                                                                   )
                                                          )
                                                      )
                                               )
                                             ),
                                             hr(),
                                             fluidRow(
                                               column(6,
                                                      h3("Create persistence model"),
                                                      p(id = "txt_j", "The simplest model that we can create is to predict that today's water temperature will be the same as yesterday’s water temperature. This is called a ", tags$b("persistence model.")),
                                                      wellPanel(
                                                        h4("Persistence model (Pers):"),
                                                        div("$$wtemp_{t} = wtemp_{t-1}$$"),
                                                        p("where t = today and t = yesterday.")
                                                      ),
                                                      p(id = "txt_j", "Let's plot this model versus observations."),
                                                      br(),
                                                      actionButton("plot_persist", "Plot"),
                                                      br(), br(),
                                                      box(id = "box2", width = 12, status = "primary",
                                                          solidHeader = TRUE,
                                                          fluidRow(
                                                            column(10, offset = 1,
                                                                   h3("Questions"),
                                                                   p(tags$b(quest["q11", 1])),
                                                                   p(tags$b(quest["q12", 1]))
                                                            )
                                                          )
                                                      )
                                               ),
                                               column(6,
                                                      wellPanel(
                                                        plotlyOutput("persist_plot")
                                                      )
                                               )
                                             ),
                                             hr(),
                                             fluidRow(
                                               column(12,
                                                      h3("Create linear regression models"),
                                                      p(id = "txt_j", "We will create two",tags$b(" linear regression models:")),
                                                      tags$ul(
                                                        tags$li("A model that uses ",tags$b("yesterday's water temperature")," to predict ",tags$b("today's water temperature.")),
                                                        tags$li("A model that uses ",tags$b("today's air temperature")," to predict ",tags$b("today's water temperature."))
                                                      ),                                
                                                      p("First, we will generate scatterplots and assess the relationships between the ",tags$b("independent variables")," (yesterday's water temperature, today's air temperature) and the ",tags$b("dependent variable")," (today's water temperature).")
                                               )
                                               ),
                                             hr(),
                                             fluidRow(
                                               column(12,
                                                      h4("Scatterplots of dependent vs. independent variables"),
                                                      p("A linear relationship is a statistical term used to describe a straight-line relationship between two variables. Linear relationships can either be positive or negative."),
                                                      tags$ul(
                                                        tags$li(tags$b("Positive linear relationship:")," as the independent variable increases or decreases, so does the dependent variable"),
                                                        tags$li(tags$b("Negative linear relationship:")," as the indepdendent variable increases or decreases, the dependent variable does the opposite")
                                                      )
                                                      )
                                             ),
                                             fluidRow(
                                               column(6,
                                                      h4("Today's water temperature vs. yesterday's water temperature"),
                                                      actionButton("plot_airt_swt3", "Plot data"),
                                                      br(), br(),
                                                      wellPanel(
                                                        plotlyOutput("swt_swt_plot_lines")
                                                      )
                                               ),
                                               column(6,
                                                      h4("Today's water temperature vs. today's air temperature"),
                                                      actionButton("plot_airt_swt4", "Plot data"),
                                                      br(), br(),
                                                      wellPanel(
                                                        plotlyOutput("airt_swt_plot_lines"),
                                                      )
                                                      )
                                             ),
                                             hr(),
                                             fluidRow(
                                               column(12, align = "left",
                                                      box(id = "box10", width = 12, status = "primary",
                                                          solidHeader = TRUE,
                                                          fluidRow(
                                                            column(6, offset = 1,
                                                                   h3("Questions"),
                                                                   p(tags$b(quest["q13", 1])),
                                                                   radioButtons("lin_rel1", "", c("Positive", "Negative"),
                                                                                inline = TRUE,
                                                                                selected = character(0)),
                                                                   conditionalPanel("input.lin_rel1 == 'Positive'",
                                                                                    p("That's right!")
                                                                                    ),
                                                                   conditionalPanel("input.lin_rel1 == 'Negative'",
                                                                                    p("Whoops! Try again.")
                                                                   ),
                                                                   p(tags$b(quest["q14", 1])),
                                                                   radioButtons("lin_rel2", "", c("Positive", "Negative"),
                                                                                inline = TRUE,
                                                                                selected = character(0)),
                                                                   conditionalPanel("input.lin_rel2 == 'Positive'",
                                                                                    p("That's right!")
                                                                   ),
                                                                   conditionalPanel("input.lin_rel2 == 'Negative'",
                                                                                    p("Whoops! Try again.")
                                                                   ),
                                                                   p(tags$b(quest["q15", 1]))
                                                            ),
                                                            column(4,
                                                                   br(),br(),
                                                                   h4("Example plot of a perfect linear relationship"),
                                                                   img(src = "example_linear_relationship.png", height = "120%", id = "bla_border",
                                                                       width = "120%", tags$style("border: solid 2px black;")),
                                                                   br(),br()
                                                                   )
                                                          )
                                                      )
                                               )
                                             ),
                                             hr(),
                                             fluidRow(
                                               column(12,
                                                      h4("Fit linear regression models"),
                                                      p("Click the 'Fit model' buttons below to use ",tags$b("one year of lake data")," to fit your model parameters. You will also plot timeseries of model predictions and observations of water temperature."),
                                                      p(tags$em("Note that once you fit the model, you will be able see the best-fit line for each model by scrolling back up to the scatterplots.")),
                                                      tags$ul(
                                                        tags$li("For the linear regression model, ", tags$b("m"), " and ", tags$b("b"), "are ", tags$b("parameters.")),
                                                        tags$li(tags$b("N"), "represents the number of data points used to estimate the linear regression.")
                                                      )
                                               )
                                             ),
                                    fluidRow(
                                      column(6,
                                             br(),br(),
                                             actionButton("add_lm", "Get model parameters"),
                                             DTOutput("lr_DT", width = "100%"),
                                             br(), br(),
                                             wellPanel(
                                               uiOutput("lm_mod")
                                             ),
                                             p(tags$b("Tip:"), " You can toggle what is shown on the plot by clicking on the options in the legend."),
                                             wellPanel(
                                               plotlyOutput("wt_reg_ts_plot")
                                             )
                                             ),
                                      column(6,
                                             br(),br(),
                                             actionButton("add_lm1", "Get model parameters"),
                                             DTOutput("lr_DT1", width = "100%"),
                                             br(),br(),
                                             wellPanel(
                                               uiOutput("lm_mod1")
                                             ),
                                             p(tags$b("Tip:"), " You can toggle what is shown on the plot by clicking on the options in the legend."),
                                             wellPanel(
                                               plotlyOutput("at_reg_ts_plot")
                                             )
                                             )
                                    ),
                                             hr(),
                                    fluidRow(
                                      column(10, align = "left",
                                             box(id = "box10", width = 12, status = "primary",
                                                 solidHeader = TRUE,
                                                 fluidRow(
                                                   column(10, offset = 1,
                                                          h3("Question"),
                                                          p(tags$b(quest["q16", 1]))
                                                          )
                                                 )
                                             )
                                      )
                                    ),
                                    hr(),
                                    fluidRow(
                                      column(6,
                                             h3("Create multiple linear regression model"),
                                             p(id = "txt_j", "Finally, we will fit a ", tags$b("multiple linear regression model.")," This model will use two independent variables, yesterday's water temperature and today's air temperature, to predict today's water temperature."),
                                             wellPanel(
                                               h4("Multiple linear regression model:"),
                                               div("$$wtemp_{t} = \\beta _{0} + \\beta _{1}wtemp_{t-1} + \\beta _{2}atemp_{t}$$"),
                                               p("where t = today and t-1 = yesterday.")
                                             ),
                                             p(id = "txt_j", "Let's plot this model versus observations."),
                                             br(),
                                             actionButton("plot_mlr", "Plot"),
                                             br(), br(),
                                             box(id = "box2", width = 12, status = "primary",
                                                 solidHeader = TRUE,
                                                 fluidRow(
                                                   column(10, offset = 1,
                                                          h3("Questions"),
                                                          p(tags$b(quest["q17", 1])),
                                                          radioButtons("mlr_params", "", c("wtemp", "atemp"),
                                                                       selected = character(0)),
                                                          conditionalPanel("input.mlr_params == 'wtemp'",
                                                                           p("That's right!")
                                                          ),
                                                          conditionalPanel("input.mlr_params == 'atemp'",
                                                                           p("Whoops! Try again.")
                                                          )                                                  
                                                          )
                                                 )
                                             )
                                      ),
                                      column(6,
                                             wellPanel(
                                               plotlyOutput("mlr_ts_plot1")
                                             ),
                                             DTOutput("mlr_DT", width = "100%"),
                                             br(),
                                             wellPanel(
                                               uiOutput("mlr_mod1")
                                             )
                                      )
                                    ),
                                    hr(),
                                    fluidRow(
                                      column(6,
                                             h3("Nice work! You've fit four models!"),
                                             box(id = "box2", width = 12, status = "primary",
                                                 solidHeader = TRUE,
                                                 fluidRow(
                                                   column(10, offset = 1,
                                                          h3("Question"),
                                                          p(tags$b(quest["q18", 1])),
                                                   )
                                                 )
                                             )
                                             ),
                                      column(6,
                                             wellPanel(
                                               plotlyOutput("all_mods_plot")
                                             )
                                             )
                                    ),
                                    hr(),
                                             fluidRow(
                                               column(5, offset = 1,
                                                      h3("Next step"),
                                                      p("We will use the models we have built to generate forecasts."))
                                             )
                                    ),
                                    #* Activity B - Overview ====
                                    tabPanel(title = "Objective 4 - Generate forecasts", value = "obj6",
                                             fluidRow(
                                               column(12,
                                                      wellPanel(style = paste0("background: ", obj_bg),
                                                                h3("Objective 4 - Generate forecasts"),
                                                                p(id = "txt_j", module_text["obj_04", ])
                                                      )
                                               )
                                             ),
                                             fluidRow(
                                               column(6,
                                                      h3("Deterministic forecasts"),
                                                      p(id = "txt_j", "We will now use the models you have fit to generate ", tags$b("deterministic")," forecasts of water temperature. A deterministic forecast is one that does not account for any uncertainty associated with predictions."),
                                                      h4("Shifting from model fitting to forecasting"),
                                                      p(id = "txt_j", "So far, we have fit our water temperature models using yesterday's water temperature and today's air temperature to predict today's water temperature. For example:"),
                                                      div("$$wtemp_{t} = \\beta_0 + \\beta_1*wtemp_{t-1} + \\beta_2*atemp_{t}$$"),
                                                      p(id = "txt_j", "Now, to forecast, we will need to make a subtle but important change in the way we write our models, to show that instead of predicting today's water temperature, we are now forecasting tomorrow's water temperature:"),
                                                      div("$$wtemp_{t+1} = \\beta_0 + \\beta_1*wtemp_{t} + \\beta_2*atemp_{t+1}$$"),
                                                      p(tags$b("Note the change in the subscripts of the wtemp and atemp variables from t-1 to t and t to t+1!!")),
                                                      p("This means, for models that require air temperature as an input, we will need some way of estimating tomorrow's air temperature in order to forecast tomorrow's water temperature.")
                                               ),
                                               column(6, align = "center",
                                                      img(src = "model_UC_draft_v2.png", height = "60%", id = "bla_border",
                                                          width = "60%", tags$style("border: solid 2px black;"))
                                               )
                                             ),
                                             hr(),
                                             #** View Weather Forecast Data ----
                                             fluidRow(
                                               column(6,
                                                      h3("Weather forecast data"),
                                                      p(id = "txt_j", "We just received a weather forecast data from NOAA. It is a 7-day forecast of air temperature at our site. We will need this data for our forecasts as some of our models depend on forecasted air temperature. Click the button below to view it."),
                                                      actionButton("view_at_fc", "View forecast"),
                                                      p(id = "txt_j", "With this air temperature forecast we can use the models that we built in Objective 3 that require air temperature, to forecast water temperature.")
                                               ),
                                               column(6,
                                                      h4("Air temperature forecast"),
                                                      wellPanel(
                                                        plotlyOutput("airt1_fc_plot")
                                                      )
                                               )
                                             ),
                                             fluidRow(
                                               #** Deterministic Forecast ----
                                               column(6,
                                                      h3("Deterministic Forecasts"),
                                                      p(id = "txt_j", "Now we will generate ", tags$b("deterministic"), " forecasts with each of our models. We will use the use the forecasted driver data (air temperature) for the models that use it as a driver. We will generate forecasts from today (Sep 25th), which is represented in the plots as the vertical dashed line, for seven days into the future (Oct 2nd)."),
                                                      p(id = "txt_j", "Select a model from the table below and then run the forecast."),
                                                      p(id = "txt_j", "Note: If there are '$' in the table below, click on one of the rows and this will re-render the table."),
                                                      DTOutput("mod_selec_tab1a"),
                                                      br(),
                                                      actionButton("run_wtemp_fc1a", "Run forecast"),
                                                      br(), br(),
                                                      box(id = "box2", width = 12, status = "primary",
                                                          solidHeader = TRUE,
                                                          fluidRow(
                                                            column(10, offset = 1,
                                                                   h3("Questions"),
                                                                   p(tags$b(quest["q19", 1]))
                                                            )
                                                          )
                                                      )
                                               ),
                                               column(6,
                                                      h4("Water temperature forecast"),
                                                      wellPanel(
                                                        plotlyOutput("wtemp_fc1a")
                                                      ),
                                                      wellPanel(
                                                        uiOutput("sel_mod1a"),
                                                        textOutput("txt_fc_out1a")
                                                      )
                                               )
                                             ),
                                             hr(),
                                             fluidRow(
                                               column(6,
                                                      h4("What is wrong with deterministic forecasts?"),
                                                      p(id = "txt_j", "Using a ", tags$b("deterministic"), " forecast (e.g. a forecast which is one single line, with no uncertainty) is guaranteed to be wrong, because it ignores the uncertainty that is inherently associated with the future."),
                                                      p(id = "txt_j", "There are many things which contribute to uncertainty when generating a forecast, and a forecast should represent the range of potential outcomes and the ", tags$b("likelihood"), " of such outcomes occurring."),
                                                      p(id = "txt_j", "Therefore, we need to generate a ", tags$b("probabilistic"), " forecast which represents both the range of outcomes and also the likelihood of each.")
                                               ),
                                               column(6,
                                                      box(id = "box2", width = 12, status = "primary",
                                                          solidHeader = TRUE,
                                                          fluidRow(
                                                            column(10, offset = 1,
                                                                   h3("Questions"),
                                                                   p(tags$b(quest["q20", 1]))
                                                            )
                                                          )
                                                      )
                                               )
                                             )
                                    ),
                                    )
                        ),
               # 6. Activity B ----
               tabPanel(title = tags$b("Activity B"), value = "mtab6",
                        img(src = "project-eddie-banner-2020_green.png", height = 100,
                            width = 1544, top = 5),
                        fluidRow(
                          column(12,
                                 wellPanel(style = paste0("background: ", obj_bg),
                                           h2("Activity B - Explore Forecast Uncertainty"),
                                           p(module_text["act_B", ])
                                 )
                          )
                        ),
                        tabsetPanel(id = "tabseries3",
                                    #* Objective 6 - Process Uncertainty ====
                                    tabPanel(title = "Objective 6 - Process Uncertainty", value = "obj7",
                                             fluidRow(
                                               column(12,
                                                      wellPanel(style = paste0("background: ", obj_bg),
                                                                h3("Objective 6 - Process Uncertainty"),
                                                                p(id = "txt_j", module_text["obj_06", ])
                                                                )
                                                      )
                                             ),
                                             #** Process Uncertainty ----
                                             fluidRow(
                                               column(12,
                                                      h3("Process Uncertainty")
                                                      )
                                             ),
                                             fluidRow(
                                               column(4,
                                                      p(id = "txt_j", tags$b("Process uncertainty")," is uncertainty caused by our inability to model all processes as observed in the real world."),
                                                      p(id = "txt_j", "Our 'simple' water temperature models use today's water temperature and tomorrow's forecasted air temperature to forecast tomorrow's water temperature. For example:"),
                                                      div("$$wtemp_{t+1} =  \\beta_0 + \\beta_1*wtemp_{t} + \\beta_2*atemp_{t+1}$$"),
                                                      p(id = "txt_j", "But we know that water temperature can be affected by other processes as well (such as rain, inflow streams to a lake, or water column mixing) and that our model has simplified or ignored these. To account for the uncertainty these simplifications introduce, we can add in ",tags$b("process noise (W)")," at each time step. In this model, water temperature tomorrow is equal to water temperature today plus air temperature tomorrow plus some noise ",tags$b("(W):")),
                                                      div("$$wtemp_{t+1} = \\beta_0 + \\beta_1*wtemp_{t} + \\beta_2*atemp_{t+1} + W_t$$"),
                                                      p("Scroll through the slides to the right to learn how",tags$b(" W")," is calculated and accounted for in a forecast.")
                                               ),
                                               column(8,
                                                      h5("Click the arrows to navigate through the slides", align = "center"),
                                                      wellPanel(
                                                        slickROutput("proc_uc_slides", width = "640px", height = "360px")
                                                        )
                                                      )
                                               ),
                                             hr(),
                                             fluidRow(
                                               column(6,
                                                      h4("Generate Process Uncertainty Distributions"),
                                                      p("We will generate a process uncertainty distribution for each of our models and sample these distributions to create an ensemble forecast."),
                                                      p(id = "txt_j", "Select a model from the table below and then calculate the process uncertainty distribution and run the forecast."),
                                                      p(id = "txt_j", "Note: If there are '$' in the table below, click on one of the rows and this will re-render the table."),
                                                      #DTOutput("mod_selec_tab2"),
                                                      actionButton("gen_proc_dist", "Generate process uncertainty distribution"),
                                                      actionButton("run_wtemp_fc2", "Run forecast")
                                                      ),
                                               column(6,
                                                      wellPanel(
                                                        plotlyOutput("proc_dist")
                                                      )
                                                      )
                                             ),
                                             fluidRow(
                                               column(6,
                                                      h4("Forecast with Process Uncertainty"),
                                                      p(id = "txt_j", "First we will explore how the different models respond to the addition of process uncertainty. Run each of the models with many ensemble members and observe how the forecast outcome changes."),
                                                      p(id = "txt_j", "Select a model from the table below and then load the driver data and run the forecast."),
                                                      p(id = "txt_j", "Note: If there are '$' in the table below, click on one of the rows and this will re-render the table."),
                                                      DTOutput("mod_selec_tab2"),
                                                      br(),
                                                      # actionButton("load_driv2", "Load driver data"),
                                                      actionButton("run_wtemp_fc2", "Run forecast"),
                                                      br(),
                                                      p(id = "txt_j", "To account for uncertainty in the noise, we can run the model multiple times with random noise added to each model run. More noise is associated with high process uncertainty, and vice versa. Using multiple model runs is called an ", tags$b("ensemble."), " Each individual run is referred to as an ensemble ", tags$b("member."), "Forecasters typically run tens to hundreds of ensemble members to build uncertainty in their forecasts."),
                                                      p(id = "txt_j", "Using the slider below, adjust the number of members to see how process uncertainty changes with time into the future (e.g. forecast horizon)."),
                                                      sliderInput("n_mem2", "No. of members", min = 5, max = 100, value = 50, step = 5),
                                                      box(id = "box2", width = 12, status = "primary",
                                                          solidHeader = TRUE,
                                                          fluidRow(
                                                            column(10, offset = 1,
                                                                   h3("Questions"),
                                                                   textAreaInput2(inputId = qid[23], label = quest[qid[23], ], width = "90%")
                                                                   )
                                                            )
                                                          )
                                               ),
                                               column(6,
                                                      wellPanel(
                                                        plotlyOutput("wtemp_fc2")
                                                      ),
                                                      wellPanel(
                                                        uiOutput("sel_mod2"),
                                                        textOutput("txt_fc_out2")
                                                      ),
                                                      conditionalPanel("input.run_wtemp_fc2 > 0",
                                                                       p(id = "txt_j", "Using the properties of a normal distribution, we can calculate the confidence intervals of these samples and use this to visualize uncertainty in our forecast."),
                                                                       radioButtons("plot_type2", "Plot type", c("Line", "Distribution"),
                                                                                    inline = TRUE),
                                                                       p(id = "txt_j", "Click on items in the legend to show/hide them from the plot.")
                                                      )
                                               )
                                             ),
                                             hr()
                                    ),
                                    #* Objective 7 - Parameter Uncertainty ====
                                    tabPanel(title = "Objective 7 - Parameter Uncertainty", value = "obj8",
                                             fluidRow(
                                               column(12,
                                                      wellPanel(style = paste0("background: ", obj_bg),
                                                                h3("Objective 7 - Parameter Uncertainty"),
                                                                p(id = "txt_j", module_text["obj_07", ])
                                                      )
                                               )
                                             ),
                                             #** Parameter Uncertainty ----
                                             fluidRow(
                                               column(4,
                                                      h3("Parameter Uncertainty"),
                                                      p(id = "txt_j", tags$b("Parameter uncertainty")," refers to the uncertainty in the model parameter values, which can be due to uncertainties in the data or the calibration process used."),
                                                      p(id = "txt_j", "With traditional modelling efforts, people generally find one set of the 'best fit' parameters and use them to predict with their model."),
                                                      p(id = "txt_j", "This method does not account for the uncertainty around the estimation of these parameters."),
                                                      p(id = "txt_j", "There is often the possibility that different parameter sets can yield similar metrics of model performance, e.g., similar RMSE values."),
                                                      p(id = "txt_j", "Using ", tags$b("parameter distributions"), " allows for a better representation of the potential predicted outcomes, leading to better quantification of the uncertainty.")
                                               ),
                                               column(8,
                                                      h5("Click the arrows to navigate through the slides", align = "center"),
                                                      wellPanel(
                                                        slickROutput("param_uc_slides", width = "640px", height = "360px")
                                                      )
                                               )
                                             ),
                                             hr(),
                                             fluidRow(
                                               column(4,
                                                      h4("Uncertainty in Parameters"),
                                                      p(id = "txt_j", "We know, from Objective 4, that when we used different amounts of data (e.g., monthly vs. daily data) to build our model, we got slightly different estimations of our model parameters ", tags$em("m"), " and ", tags$em("b.")),
                                                      p(id = "txt_j", "To account for this ", tags$b("parameter uncertainty,"), " we will build distributions for each parameter in our forecasting models, from which we will draw samples for forecasting."),
                                                      wellPanel(
                                                        h4("A Note on Parameters:"),
                                                        uiOutput("param_notation"),
                                                        div("$$wtemp_{t+1} = m*wtemp_{t} + b$$"),
                                                        div("$$wtemp_{t+1} = \\beta_1*wtemp_{t} + \\beta_2$$")
                                                      )
                                                      ),
                                               column(8,
                                                      plotOutput("param_fcast3b")
                                                      )
                                               ),
                                             fluidRow(
                                               column(6,
                                                      h4("Generate Parameter Distributions"),
                                                      p(id = "txt_j", "We will generate parameter distributions for each of our models and sample these distributions to create an ", tags$b("ensemble forecast"), " using multiple different parameter sets."),
                                                      p(id = "txt_j", "Select a model from the table below and then generate parameter distributions and run the forecast."),
                                                      p(id = "txt_j", "Note: If there are '$' in the table below, click on one of the rows and this will re-render the table."),
                                                      DTOutput("mod_selec_tab3"),
                                                      br(),
                                                      actionButton("gen_params3b", "Generate parameters"),
                                                      actionButton("run_wtemp_fc3b", "Run forecast"),
                                                      p(id = "txt_j", "We will use 100 parameter sets in the forecast ensemble. These will be sampled from the distributions generated above.")
                                               ),
                                               column(6,
                                                      h4("Parameter Distributions"),
                                                      wellPanel(
                                                        plotOutput("param_dist3b")
                                                      )
                                               )
                                             ),
                                             fluidRow(
                                               column(6,
                                                      h4("Forecast with Parameter Uncertainty"),
                                                      box(id = "box2", width = 12, status = "primary",
                                                          solidHeader = TRUE,
                                                          fluidRow(
                                                            column(10, offset = 1,
                                                                   h3("Questions"),
                                                                   textAreaInput2(inputId = qid[24], label = quest[qid[24], ], width = "90%"),
                                                                   textAreaInput2(inputId = qid[25], label = quest[qid[25], ], width = "90%")
                                                                   )
                                                            )
                                                          )
                                                      ),
                                               column(6,
                                                      h4("Water Temperature Forecasts"),
                                                      wellPanel(
                                                        plotlyOutput("wtemp_fc3b")
                                                      ),
                                                      wellPanel(
                                                        uiOutput("sel_mod3b"),
                                                        textOutput("txt_fc_out3b")
                                                      ),
                                                      conditionalPanel("input.run_wtemp_fc3b > 0",
                                                                       p("Using the properties of a normal distribution, we can calculate the confidence intervals of these samples and use this to visualize uncertainty in our forecast."),
                                                                       radioButtons("plot_type3b", "Plot type", c("Line", "Distribution"),
                                                                                    inline = TRUE)
                                                                       )
                                                      )
                                               ),
                                             hr(),
                                             fluidRow(
                                               column(5, offset = 1,
                                                      h3("Next step"),
                                                      p(id = "txt_j", "We will explore what initial conditions uncertainty is and how it can affect our forecasts of water temperature.")
                                                      )
                                               )
                                             ),
                                    #* Objective 8 - Initial Conditions Uncertainty ====
                                    tabPanel(title = "Objective 8 - Initial Conditions Uncertainty", value = "obj9",
                                             fluidRow(
                                               column(12,
                                                      wellPanel(style = paste0("background: ", obj_bg),
                                                                h3("Objective 8 - Initial Conditions Uncertainty"),
                                                                p(id = "txt_j", module_text["obj_08", ])
                                                                )
                                                      )
                                               ),
                                             fluidRow(
                                               column(4,
                                                      h3("Initial Condition Uncertainty"),
                                                      p(id = "txt_j", tags$b("Initial conditions uncertainty")," refers to uncertainty arising because the current conditions in an ecosystem are not precisely known or because the calculations cannot be performed with the precise initial conditions."),
                                                      p(id = "txt_j", "Even though we have measurements of water temperature from our lake, we know that water temperature varies throughout the day so this measurement might not capture exactly the temperature in our lake at this time. Additionally, there may be observation error in our temperature measurements.")
                                               ),
                                               column(8,
                                                      h5("Click the arrows to navigate through the slides", align = "center"),
                                                      wellPanel(
                                                        slickROutput("ic_uc_slides", width = "640px", height = "360px")
                                                        )
                                                      )
                                               ),
                                             hr(),
                                             fluidRow(
                                               column(4,
                                                      h4("Generate Initial Condition Distribution"),
                                                      p("To account for initial condition uncertainty we can generate a distribution around this value and then run our model with slightly different initial conditions."),
                                                      p(id = "txt_j", "Use the slider below to adjust the standard deviation and then generate a normal distribution around the initial condition."),
                                                      sliderInput("ic_uc", "Standard deviation", min = 0.05, max = 0.5, value = 0.1, step = 0.05),
                                                      actionButton("gen_ic", "Generate distribution")
                                                      ),
                                               column(4,
                                                      h4("Recent Observations"),
                                                      wellPanel(
                                                        plotlyOutput("ic_obs_plot")
                                                        )
                                                      ),
                                               column(4,
                                                      h4("Distribution of Initial Conditions"),
                                                      wellPanel(
                                                        plotOutput("ic_uc_plot")
                                                        )
                                                      )
                                             ),
                                             fluidRow(
                                               column(6,
                                                      h4("Forecast with Initial Conditions Uncertainty"),
                                                      p(id = "txt_j", "Now we will generate forecasts with different initial conditions for each of our models."),
                                                      p(id = "txt_j", "Select a model from the table below and then run the forecast."),
                                                      p(id = "txt_j", "Note: If there are '$' in the table below, click on one of the rows and this will re-render the table."),
                                                      DTOutput("mod_selec_tab4"),
                                                      br(),
                                                      # actionButton("load_driv4", "Load driver data"),
                                                      actionButton("run_wtemp_fc4", "Run forecast"),
                                                      br(),
                                                      p(id = "txt_j", "We will use 100 different initial condtions in the forecast ensemble. These will be sampled from the distribution generated above."),
                                                      br(),
                                                      box(id = "box2", width = 12, status = "primary",
                                                          solidHeader = TRUE,
                                                          fluidRow(
                                                            column(10, offset = 1,
                                                                   h3("Questions"),
                                                                   textAreaInput2(inputId = qid[26], label = quest[qid[26], ], width = "90%"),
                                                                   textAreaInput2(inputId = qid[27], label = quest[qid[27], ], width = "90%"),
                                                                   textAreaInput2(inputId = qid[28], label = quest[qid[28], ], width = "90%")
                                                                   )
                                                            )
                                                          )
                                                      ),
                                               column(6,
                                                      wellPanel(
                                                        plotlyOutput("wtemp_fc4")
                                                      ),
                                                      wellPanel(
                                                        uiOutput("sel_mod4"),
                                                        textOutput("txt_fc_out4"),
                                                        radioButtons("plot_type4", "Plot type", c("Line", "Distribution"),
                                                                     inline = TRUE)
                                                        )
                                                      )
                                               ),
                                             hr(),
                                             fluidRow(
                                               column(5, offset = 1,
                                                      h3("Next step"),
                                                      p("We will examine how driver uncertainty can affect our forecast of water temperature.")
                                                      )
                                               )
                                             ),
                                    #* Objective 9 - Driver Uncertainty ====
                                    tabPanel(title = "Objective 9 - Driver Uncertainty", value = "obj10",
                                             fluidRow(
                                               column(12,
                                                      wellPanel(style = paste0("background: ", obj_bg),
                                                                h3("Objective 9 - Driver Uncertainty"),
                                                                p(id = "txt_j", module_text["obj_09", ])
                                                      )
                                               )
                                             ),
                                             fluidRow(
                                               column(4,
                                                      h3("Driver Uncertainty"),
                                                      p(id = "txt_j", tags$b("Driver uncertainty")," comes from inaccuracies in the forecasted variables used to drive the model."),
                                                      p(id = "txt_j", "The driver variable for our model is air temperature. To generate a forecast of future water temperature, we need to use forecasted air temperature to drive the model."),
                                                      p(id = "txt_j", "Luckily for us, the National Oceanic and Atmospheric Administration (NOAA) generate ensemble forecasts of air temperature.")
                                                      ),
                                               column(8,
                                                      h5("Click the arrows to navigate through the slides", align = "center"),
                                                      wellPanel(
                                                        slickROutput("driver_uc_slides", width = "640px", height = "360px")
                                                        )
                                                      )
                                               ),
                                             fluidRow(
                                               column(6,
                                                      h4("NOAA Forecast data"),
                                                      p(id = "txt_j", "Here we will load in an air temperature forecast which has been generate from the ", a(href = "https://www.ncdc.noaa.gov/data-access/model-data/model-datasets/global-ensemble-forecast-system-gefs", "NOAA Global Ensemble Forecast System", target = "_blank"), " for the NEON site you chose in Activity A."),
                                                      img(src = "noaa_logo.jpg", height = "20%",
                                                          width = "20%", align = "right"),
                                                      p(id = "txt_j", "Load the NOAA ensemble forecast for air temperature below."),
                                                      actionButton("load_noaa_at", "Load forecast"),
                                                      verbatimTextOutput("noaa_at_loaded"),
                                                      p(id = "txt_j", "You can adjust the number of ensemble members plotted below. These are what you will use to drive your model."),
                                                      numericInput("noaa_n_mems", "Number of members (0-30)", 30, 1, 30)
                                                      ),
                                               column(6,
                                                      wellPanel(
                                                        plotlyOutput("airt_fc5")
                                                      )
                                                      )
                                               ),
                                             
                                             fluidRow(
                                               column(6,
                                                      h4("Forecast with Driver Data Uncertainty"),
                                                      p(id = "txt_j", "Now we will generate forecasts using the NOAA air temperature ensemble forecast to drive each of our models."),
                                                      p(id = "txt_j", "Select a model from the table below and then run the forecast."),
                                                      p(id = "txt_j", "Note: If there are '$' in the table below, click on one of the rows and this will re-render the table."),
                                                      DTOutput("mod_selec_tab5"),
                                                      br(),
                                                      actionButton("run_wtemp_fc5", "Run forecast"),
                                                      br(),
                                                      wellPanel(
                                                        uiOutput("sel_mod5"),
                                                        textOutput("txt_fc_out5"),
                                                        radioButtons("plot_type5", "Plot type", c("Line", "Distribution"),
                                                                     inline = TRUE)
                                                      )
                                               ),
                                               column(6,
                                                      wellPanel(
                                                        plotlyOutput("wtemp_fc5")
                                                      )
                                               )
                                             ),

                                             fluidRow(
                                               column(10, offset = 1,
                                                      box(id = "box2", width = 12, status = "primary",
                                                          solidHeader = TRUE,
                                                          fluidRow(
                                                            column(10, offset = 1,
                                                                   h3("Questions"),
                                                                   textAreaInput2(inputId = qid[29], label = quest[qid[29], ], width = "90%"),
                                                                   textAreaInput2(inputId = qid[30], label = quest[qid[30], ], width = "90%")
                                                                   )
                                                            )
                                                          )
                                                      )
                                               ),
                                             hr(),
                                             fluidRow(
                                               column(5, offset = 1,
                                                      h3("Next step"),
                                                      p("We will summarize the key sources of uncertainty in our forecasts and discuss them as a group.")
                                                      )
                                               )
                                             ),
                                    #* Activity B - Summary ====
                                    tabPanel(title = "Summary", value = "obj11",
                                             fluidRow(
                                               column(12,
                                                      wellPanel(style = paste0("background: ", obj_bg),
                                                                h3("Summary"),
                                                                p(id = "txt_j", module_text["act_B_summ", ]),
                                                                p(id = "txt_j", "Remember, the Shiny app will disconnect if you leave it idle for 10 minutes, so make sure to download your '.eddie' file at the bottom of the page to save your progress.")
                                                                )
                                               )
                                             ),
                                               fluidRow(
                                                 column(5, 
                                                      h4("Discussion Questions"),
                                                      p(id = "txt_j", tags$em("Use the figures below to answer the questions.")),
                                                      tags$line()
                                                 )
                                               ),
                                             fluidRow(
                                               column(4,
                                                      tags$ul(
                                                        textAreaInput2(inputId = qid[31], label = quest[qid[31], ], width = "90%")
                                                      )
                                               ),
                                               column(4,
                                                      tags$ul(
                                                        textAreaInput2(inputId = qid[32], label = quest[qid[32], ], width = "90%")
                                                      )
                                               ),
                                               column(4,
                                                      tags$ul(
                                                        textAreaInput2(inputId = qid[33], label = quest[qid[33], ], width = "90%")
                                                        )
                                                      )
                                               ),
                                             fluidRow(
                                               column(6,
                                                      h3("Process Uncertainty"),
                                                      wellPanel(
                                                        plotOutput("proc_uc_summ")
                                                        )
                                                      ),
                                               column(6,
                                                      h3("Parameter Uncertainty"),
                                                      wellPanel(
                                                        plotOutput("param_uc_summ")
                                                        )
                                                      ),
                                               column(6,
                                                      h3("Initial Conditions Uncertainty"),
                                                      wellPanel(
                                                        plotOutput("ic_uc_summ")
                                                        )
                                                      ),
                                               column(6,
                                                      h3("Driver Uncertainty"),
                                                      wellPanel(
                                                        plotOutput("driver_uc_summ")
                                                        )
                                                      )
                                             ),
                                             fluidRow(
                                               column(12,
                                                      h2("Completed Activity B!"),
                                                      p(id = "txt_j", "This is the end of Activity B. If you have been inputting your answers into the app, it is recommended to return to the 'Introduction' tab and generate the final report before completing Activity C. Otherwise you could lose your progress.")
                                                      )
                                               )
                                             )
                                    )
                        ),
               # 6. Activity C ----
               tabPanel(title = tags$b("Activity C"), value = "mtab7",
                        img(src = "project-eddie-banner-2020_green.png", height = 100,
                            width = 1544, top = 5),
                        fluidRow(
                          column(12,
                                 wellPanel(style = paste0("background: ", obj_bg),
                                           h2("Activity C - Managing Uncertainty"),
                                           p(id = "txt_j", module_text["act_C", ])
                                           )
                                 )
                          ),
                        tabsetPanel(id = "tabseries4",
                                    #* Objective 10 - Quantify Uncertainty ====
                                    tabPanel(title = "Objective 10 - Quantify Uncertainty", value = "obj12",
                                             fluidRow(
                                               column(12,
                                                      wellPanel(style = paste0("background: ", obj_bg),
                                                                h3("Objective 10 - Quantify Uncertainty"),
                                                                p(id = "txt_j", module_text["obj_10", ])
                                                      )
                                               )
                                             ),
                                             fluidRow(
                                               column(4,
                                                      h3("Quantifying Uncertainty"),
                                                      p(id = "txt_j", tags$b("Uncertainty quantification")," is the science of characterizing and reducing uncertainty in both computational and real world applications. It tries to determine how likely certain outcomes are if some aspects of the system are not exactly known."),
                                                      p(id = "txt_j", "So far we have explored where uncertainty comes from. Now we will quantify the uncertainty at each forecast horizon."),
                                                      p(id = "txt_j", "We will generate forecasts with two of the models including all of the sources of uncertainty. In practice, when you generate an ecological forecast, you will want to include and account for all sources of uncertainty."),
                                                      br(),
                                                      h3("Think, Pair, Share!"),
                                                      p(id = "txt_j", module_text["tps1", 1]),
                                                      selectizeInput("mod_selec_tot_fc", "Select two models for the next exercise:",
                                                                     choices = mod_names,
                                                                     options = list(
                                                                       maxItems = 2,
                                                                       placeholder = 'Please select a pair of models',
                                                                       onInitialize = I('function() { this.setValue(""); }'))
                                                      )
                                               ),
                                               column(6, align = "center", offset = 2,
                                                      img(src = "tot_uc2.png", height = "60%",
                                                          width = "60%", align = "center")
                                               )
                                             ),
                                             hr(),
                                             #** Model A - Run Forecast with total uncertainty ----
                                             fluidRow(
                                               column(4,
                                                      h3("Run Forecast with Total Uncertainty: "),
                                                      h4("Model 1"),
                                                      textOutput("modA_txt"),
                                                      uiOutput("modA_eqn"),
                                                      conditionalPanel("input.fc_uncertA == 'Total'",
                                                                       p(id = "txt_j", "Total uncertainty includes all four sources of uncertainty (Process, Parameter, Initial Conditions and Driver).")
                                                      ),
                                                      sliderInput("tot_fc_mem", "Forecast members", min = 10, max = 1000, value = 100, step = 10),
                                                      actionButton("run_tot_fcA", "Run forecast"),
                                                      radioButtons("plot_type_totA", "Plot type", c("Line", "Distribution"),
                                                                   inline = TRUE),
                                                      p(id = "txt_j", "For each forecast, you will need to quantify the different sources of uncertainty in the panel below.")
                                               ),
                                               column(8,
                                                      h4("Water Temperature Forecast with Total Uncertainty: Model 1"),
                                                      wellPanel(
                                                        plotlyOutput("tot_fc_uncertA")
                                                        # checkboxInput("add_obs1", "Add observations")
                                                      )
                                               )
                                             ),
                                             hr(),
                                             #Model A: Quantify uncertainty
                                             fluidRow(
                                               column(4,
                                                      h3("Quantify Forecast Uncertainty: "),
                                                      h4("Model 1"),
                                                      p(id = "txt_j", "For our forecasts, uncertainty is represented in the spread or the ", tags$em("variation"), " of the forecast ensemble members. From this variation we can calculate the ", tags$em("standard deviation"), " across our ensemble members and use this as a quantification of our uncertainty."),
                                                      actionButton("quant_ucA", "Quantify uncertainty"),
                                                      radioButtons(qid[34], quest[qid[34], ], choices = uc_sources[1:4], selected = character(0))
                                               ),
                                               column(8,
                                                      h4("Contribution of Uncertainty Sources: Model 1"),
                                                      wellPanel(
                                                        plotlyOutput("fc_quantA")
                                                      )
                                               )
                                             ),
                                             hr(),
                                             #** Model B - UC partitioning ----
                                             fluidRow(
                                               column(4,
                                                      h3("Run Forecast with Total Uncertainty: "),
                                                      h4("Model 2"),
                                                      textOutput("modB_txt"),
                                                      uiOutput("modB_eqn"),
                                                      actionButton("run_tot_fcB", "Run forecast"),
                                                      radioButtons("plot_type_totB", "Plot type", c("Line", "Distribution"), selected = "Line",
                                                                   inline = TRUE),
                                                      p("For each forecast, you will need to quantify the different sources of uncertainty in the panel below.")
                                               ),
                                               column(8,
                                                      h4("Water Temperature Forecast with Total Uncertainty: Model 2"),
                                                      wellPanel(
                                                        plotlyOutput("tot_fc_uncertB")
                                                      )
                                               )
                                             ),
                                             #** Quantify Uncertainty - Part B ----
                                             hr(),
                                             fluidRow(
                                               column(4,
                                                      h3("Quantify Forecast Uncertainty: "),
                                                      h4("Model 2"),
                                                      p(id = "txt_j", "Quantify uncertainty for the other model you have selected, compare the two results, and answer the questions below."),
                                                      actionButton("quant_ucB", "Quantify uncertainty"),
                                                      radioButtons(qid[35], quest[qid[35], ], choices = uc_sources[1:4], selected = character(0))
                                               ),
                                               column(8,
                                                      h4("Contribution of Uncertainty Sources: Model 2"),
                                                      wellPanel(
                                                        plotlyOutput("fc_quantB")
                                                        )
                                                      )
                                               ),
                                             hr(),

                                             fluidRow(
                                               column(6,
                                                      h3("Which source of uncertainty is contributing the most?"),
                                                      p(id = "txt_j", "This is the key question for forecasters. If we can identify which uncertainty is contributing the most then we can take steps to manage this uncertainty and reduce it in our forecasts."),
                                                      # p("Here is a figure from a paper which partitioned out the different contributors of uncertainty to a forecast of water temperature."),
                                                      br()
                                                      # h4("Q. How do you think you would be able to partition out the uncertainty?")
                                               ),
                                               column(4, offset = 1,
                                                      box(id = "box2", width = 12, status = "primary",
                                                          solidHeader = TRUE,
                                                          fluidRow(
                                                            column(10, offset = 1,
                                                                   h3("Questions"),
                                                                   textAreaInput2(inputId = qid[36], label = quest[qid[36], ], width = "90%")
                                                            )
                                                          )
                                                      )
                                               )
                                             ),
                                             hr(),
                                             fluidRow(
                                               column(12,
                                                      h3("Manage Uncertainty")
                                               ),
                                               column(5, offset = 1,
                                                      radioButtons("uc_manage", "Select a source of uncertainty below and learn of ways to reduce it:",
                                                                   choices = uc_sources[1:4], selected = character(0), inline = TRUE),
                                                      conditionalPanel("input.uc_manage == 'Process'",
                                                                       h4("Process Uncertainty"),
                                                                       tags$ul(
                                                                         tags$li(id = "txt_j", "Build a better model"),
                                                                         tags$li(id = "txt_j", "Collect more data")
                                                                       )
                                                      ),
                                                      conditionalPanel("input.uc_manage == 'Parameter'",
                                                                       h4("Parameter Uncertainty"),
                                                                       tags$ul(
                                                                         tags$li(id = "txt_j", "Collect more data"),
                                                                         tags$li(id = "txt_j", "Identify which variables you need to measure")
                                                                       )
                                                      ),
                                                      conditionalPanel("input.uc_manage == 'Driver'",
                                                                       h4("Driver Uncertainty"),
                                                                       tags$ul(
                                                                         tags$li(id = "txt_j", "Use better forecasted data"),
                                                                         tags$li(id = "txt_j", "Increase number of driver ensemble members")
                                                                       )
                                                      ),
                                                      conditionalPanel("input.uc_manage == 'Initial Conditions'",
                                                                       h4("Initial Conditions Uncertainty"),
                                                                       tags$ul(
                                                                         tags$li(id = "txt_j", "Collect data at time of forecast (in real-time)"),
                                                                         tags$li(id = "txt_j", "Collect data more frequently")
                                                                         )
                                                                       )
                                                      )
                                               )
                                             ),
                                    #* Objective 11 - Management Scenario ====
                                    tabPanel(title = "Objective 11 - Management Scenario", value = "obj13",
                                             fluidRow(
                                               column(12,
                                                      wellPanel(style = paste0("background: ", obj_bg),
                                                                h3("Objective 11 - Management Scenario"),
                                                                p(id = "txt_j", module_text["obj_11", ])
                                                                )
                                                      ),
                                               column(4,
                                                      h3("Management Scenario"),
                                                      p(id = "txt_j", module_text["mgmt_scen1", ]),
                                                      p(id = "txt_j", module_text["mgmt_scen2", ]),
                                                      p(id = "txt_j", module_text["mgmt_scen3", ]),
                                                      p(id = "txt_j", module_text["mgmt_scen4", ]),
                                                      p(id = "txt_j", "Some fish species, such as Chinook salmon ", tags$em("(Oncorhynchus tshawytscha),"), "have eggs which have higher rates of survival at colder temperatures."),
                                                      p(id = "txt_j", module_text["mgmt_scen5", ]),
                                                      p("")
                                                      ),
                                               column(8, align = "center",
                                                      img(src = "salmon_underwater_dam.jpg", height = "60%",
                                                          width = "60%", align = "center"),
                                                      br(),
                                                      a("Image source", href = "http://www.hatchmag.com/sites/default/files/styles/extra-large/public/field/image/pinksalmon-elwha.jpg", target = "_blank")
                                                      # h2("Image of Water Resource manager & SALMON")
                                                     )
                                               ),
                                             hr(),
                                             #** Decision #1 ----
                                             fluidRow(
                                               column(4,
                                                      h3("Decision #1"),
                                                      p(id = "txt_j", "Use the forecast of surface and bottom temperature (across) to make a decision."),
                                                      p(id = "txt_j", "This forecast was generated only including parameter uncertainty."),
                                                      p(id = "txt_j", "The horizontal dashed line indicates the threshold above which Chinook salmon survival decreases."),
                                                      radioButtons("dec_scen1", "Which level should be used to release water from the dam?", choices = dam_lev,
                                                                   selected = character(0)),
                                                      actionButton("scen1_dec", "Decide"),
                                                      conditionalPanel("input.scen1_dec > 0",
                                                                       wellPanel(
                                                                         p("Submitted! Now scroll down to make the next decision related to water extraction.")
                                                                         )
                                                                       )
                                                      ),
                                               column(8,
                                                      h4("Forecast of water temperature at the surface and bottom of the reservoir"),
                                                      wellPanel(
                                                        plotOutput("scen1_plot")
                                                        )
                                                      )
                                               ),
                                             hr(),
                                             #** Decision #2 ----
                                             fluidRow(
                                               column(4,
                                                      h3("Decision #2"),
                                                      p(id = "txt_j", "Use the forecast of surface and bottom temperature (across) to make a decision."),
                                                      p(id = "txt_j", "This forecast was generated including process, parameter, initial conditions and driver uncertainty."),
                                                      p(id = "txt_j", "The horizontal dashed line indicates the threshold above which Chinook salmon survival decreases."),
                                                      radioButtons("dec_scen2", "Which level should be used to release water from the dam?", choices = dam_lev,
                                                                   selected = character(0)),
                                                      actionButton("scen2_dec", "Decide"),
                                                      conditionalPanel("input.scen2_dec > 0",
                                                                       wellPanel(
                                                                         p("Submitted! Now scroll down and answer the questions below.")
                                                                         )
                                                                       )
                                                      ),
                                               column(8,
                                                      h4("Forecast of water temperature at the surface and bottom of the reservoir"),
                                                      wellPanel(
                                                        plotOutput("scen2_plot")
                                                        )
                                                      )
                                               ),
                                             hr(),
                                             # Think, Pair, Share
                                             fluidRow(
                                               column(12,
                                                      h3("Think, Pair, Share!")

                                               ),
                                               column(4,
                                                      p(id = "txt_j", "With your partner, compare your decisions and discuss how the uncertainty visualization affected your decision."),
                                                      p(id = "txt_j", "Answer the questions across."),
                                                      img(src = "TPS_icon.png", height = "50%",
                                                          width = "50%", align = "center", id = "bla_border")
                                                      ),
                                               column(6, offset = 2,

                                                      box(id = "box2", width = 12, status = "primary",
                                                          solidHeader = TRUE,
                                                          fluidRow(
                                                            column(10, offset = 1,
                                                                   h3("Questions"),
                                                                   textAreaInput2(inputId = qid[37], label = quest[qid[37], ], width = "90%"),
                                                                   textAreaInput2(inputId = qid[38], label = quest[qid[38], ], width = "90%")
                                                                   )
                                                            )
                                                          )
                                                      )
                                               )
                                             ),
                                    #* Activity C - Summary ====
                                    tabPanel(title = "Summary", value = "obj14",
                                             fluidRow(
                                               column(12,
                                                      wellPanel(style = paste0("background: ", obj_bg),
                                                                h3("Summary")
                                                      )
                                               )
                                             ),
                                             fluidRow(
                                               column(4,
                                                      h3("Completed Activity C!"),
                                                      p(id = "txt_j", "This is the end of Activity C. Now you can generate your final report which will input all your answers and figures into a Microsoft Word document which you can download and submit to your instructor.")
                                                      ),
                                               column(4,
                                                      h3("Generate Report"),
                                                      p(id = "txt_j", "This will take the answers you have input into this app and generate a Microsoft Word document (.docx) with your answers which you can download and edit before submitting."),
                                                      actionButton("generate2", "Generate Report (.docx)", icon = icon("file"), width = "190px", class = "btn-primary"
                                                                   # id = "dl_btn", # This is the only button that shows up when the app is loaded
                                                                   # style = "color: #fff; background-color: #337ab7; border-color: #2e6da4"
                                                      ), br(), br(),
                                                      tags$style(type="text/css", "#download2 {background-color:#579277;color: white}"),
                                                      conditionalPanel(condition = "output.reportbuilt2", # This button appears after the report has been generated and is ready for download.
                                                                       downloadButton("download2", "Download Report", width = "60px", style = "width:190px;"
                                                                                      )
                                                                       )

                                                      ),
                                               column(4,
                                                      h3(tags$b("Questions to be completed:")),
                                                      wellPanel(
                                                        htmlOutput("check_list2")
                                                        )
                                                      )
                                               )
                                             )
                                    )
                        )
               ),
    # Tab navigation buttons ----
    br(), hr(),
    introBox(
      # h4("Use the buttons below to navigate through the tabs", align = "center"),
      box(width = 12, status = "success",
          solidHeader = TRUE,
          fluidRow(

            column(5, align = "center",
                   br(),
                   hover_action_button(
                     inputId = "prevBtn1",
                     label = "< Previous",
                     button_animation = "glow",
                     style = paste0("color: ", nav_txt, "; background-color: ", nav_butt, "; border-color: #00664B; padding:15px; font-size:22px;")
                   ),
                   bsTooltip("prevBtn1", title = "Navigate to previous tab", placement = "left", trigger = "hover"),
                   br(), br()

            ),
            column(2, align = "center",
                   br(),
                   br(), br()
            ),
            column(5, align = "center",
                   br(),
                   use_hover(popback = TRUE),
                   hover_action_button(
                     inputId = "nextBtn1",
                     label = "Next >",
                     button_animation = "glow",
                     style = paste0("color: ", nav_txt, "; background-color: ", nav_butt, "; border-color: #00664B; padding:15px; font-size:22px;")
                   ),
                   bsTooltip("nextBtn1", title = "Navigate to next tab", placement = "right", trigger = "hover"),
                   br(), br()
                   # )
            )
          )
      ), data.step = 3, data.intro = help_text["tab_nav2", 1], data.position = "right"
    ),
    hr(),
    fluidRow(
      column(8, offset = 1,
             br(),
             p(module_text["acknowledgement", ], id = "ackn"),
             p(app_update_txt, id = "ackn")
             )
      )
    )
  }

shinyUI(ui)

# end
