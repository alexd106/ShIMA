
  # Call all the dependencies
  source("support.R")
  
  if(interactive()){
    ui <- fluidPage(shinyjs::useShinyjs(), tags$style(type = "text/css", ".navbar {background-color: #004d5a; font-size:120%; }"),
    h1(tags$hr(style="background-color: grey; height: 2px; border:none"), 
      img(src="ShIMA.png", height="50px", align = "left"),
      "Shiny Interface for Metabolite Analysis", align = "center", style = "color:white; font-weight: 900; background-color:#008B8C; font-family:; font-size:200%;",
      tags$hr(style="background-color: grey; height: 2px; border:none")),
    navbarPage(inverse=TRUE,"",
               
      tabPanel("Home", 
               column(12,
                    column(3,
                             img(src="heatmap.png", height="360px", align = "left") 
                      ),
                      column(6,
                             includeMarkdown("ShortDesc.Rmd")
                             #img(src="ShortDesc.png", height="350px", align = "left")
                      ),
                      column(3,
                             img(src="RLA_acrossGrp.png", height="350px", align = "right") 
                      )
                      
               ) 
               
               
               ),
      
      tabPanel("Pipeline",
        fluidPage(
               tags$style(".nav-tabs {background-color: white;}
               .nav-tabs-custom .nav-tabs li.active:hover a, .nav-tabs-custom .nav-tabs li.active a {
               background-color: transparent;
               border-color: transparent;
               }
               .nav-tabs-custom .nav-tabs li.active {
               border-top-color: #CCC;
               }"),
      
        tabsetPanel(id = "inTabset",
          tabPanel(title = tags$b("Autopipeline"), value = "panel1", 
            fixedRow(
              sidebarPanel(style="background-color: lightblue",
                                  ###< By Guanlin       
                                  h5(helpText("Select sample orgin below")),
                                  selectInput(inputId = "sample origin", label = "sample origin", choices = c("human","mouse","others"), selected = "human", multiple = FALSE),
                                  selectInput(inputId = "sample type", label = "sample type", 
                                              choices = c("None","serum","urine","milk","liver","brain","brown adipose tissue","white adipose tissue","others"), 
                                              selected = "None", multiple = FALSE),
                                  tags$hr(),
                                  textInput("text", "text (please specify others):", "input text"),
                                  br()
                                  #submitButton("Submit") 
                                  ###By Guanlin >      
                     
              ), 
              column(8,
                  bsCollapse(id = "collapse", open = "Upload Data",
                  bsCollapsePanel("Upload Data", style = "info",
                
                column(4,
                  shinyDirButton('directory1_1', 'Select directory', 'Please select a folder'),
                  tags$br(),
                  tags$br(),
                  verbatimTextOutput('directorypath1_1')
                  ),
                column(4,
                  shinyDirButton('directory1_2', 'Output directory', 'Please select a folder'),
                  tags$br(),
                  tags$br(),
                  verbatimTextOutput('directorypath1_2'),
                  tags$br(),
                  tags$br()
                  ),
                column(4,
                  tags$br(),
                  tags$br(),
                  tags$br(),
                  
                  actionButton("go1", "GO", style="float:right"),
                  tags$br(),
                  radioButtons('formatAutopipelineAnalysis', 'Document format', c('PDF', 'HTML', 'Word'), inline = TRUE),
                  downloadButton("reportAutopipelineAnalysis", "Generate report")
                )
                  
                )
              ) 
            ),
            
            fixedRow(
              column(12, style="background-color: lightgrey;"
                     
                     
                     
                     
              )
            )
            
          )
          ),
          tabPanel(title = tags$b("Preprocessing"), value = "panel2", 
            fixedRow(
              sidebarPanel(style="background-color: lightblue",
                           ###< By Guanlin       
                           h5(helpText("Select sample orgin below")),
                           selectInput(inputId = "sample origin", label = "sample origin", choices = c("human","mouse","others"), selected = "human", multiple = FALSE),
                           selectInput(inputId = "sample type", label = "sample type", 
                                       choices = c("None","serum","urine","milk","liver","brain","brown adipose tissue","white adipose tissue","others"), 
                                       selected = "None", multiple = FALSE),
                           tags$hr(),
                           textInput("text", "text (please specify others):", "input text")
                           
                           #submitButton("Submit") 
                           ###By Guanlin >      
                           
              ), 
              column(8, 
                bsCollapse(id = "collapse", open = "Upload Data",
                  bsCollapsePanel("Upload Data", style = "info",
                    column(4,
                      shinyDirButton('directory2_1', 'Select directory', 'Please select a folder'),
                        tags$br(),
                        tags$br(),
                        verbatimTextOutput('directorypath2_1')
                        ),
                        column(4,
                          shinyDirButton('directory2_2', 'Output directory', 'Please select a folder'),
                          tags$br(),
                          tags$br(),
                          verbatimTextOutput('directorypath2_2'),
                          tags$br(),
                          tags$br()
                          ),
                        column(4,
                          tags$br(),
                          tags$br(),
                          tags$br(),
                          tags$br()
                          
                        )
                        )
                      )
                   ),
                column(12, 
                  bsCollapse(id = "collapse1", 
                  bsCollapsePanel("Parameters", style = "info",
                    fixedRow(
                      column(12,
                        fixedRow(
                          column(12, 
                            column(4, tags$b(tags$br(),"PPM"),
                              column(12,
                                numericInput("ppm", "", 5, min = 1)
                                )
                             ),
                             "\t",
                            column(4, tags$b("Signal to noise ratio cutoff"),
                              column(12,
                                numericInput("snthresh", "", 5, min = 1)
                                )
                             ),
                             "\t",
                            column(4, tags$b("Number of slaves/core"),
                              column(12,
                                numericInput("nSlaves", "", 5, min = 1)
                                )
                             )
                         
                     )
                  )
                  ),
            
                    fixedRow(
                      column(12,
                        column(9, tags$b(tags$br(),"Peak width range"),
                          column(12,
                            sliderInput("peakwidth", "", min = 1, max = 1000, value = c(10, 100))
                            )
                          ),
                        "\t",
                        column(3, tags$b("Minimum difference in m/z"),
                          column(12,
                            numericInput("mzdiff", "", 0.01)
                            )
                          )
                        )
                      ),
             
                    fixedRow(
                      column(12,
                        column(9, tags$b(tags$br(),"Prefilter mass traces"),
                          column(12,
                            sliderInput("prefilter", "", min = 1, max = 10000, value = c(3, 1000))
                            )
                          ),
                        "\t",
                        column(3, tags$b("Integration method"),
                          column(12,
                            numericInput("integrate", "", 1, min = 1, max =2)
                            )
                          )
                        
                      )
                    ),
                  
                  
                    fixedRow(
                      column(12,
                        column(4, tags$b("Method"),
                          column(12,
                            selectInput("var", "",
                              c("obiwarp" = "obiwarp",
                                               "normal" = "normal")
                               )
                             )
                          ),
                        "\t",
                        column(4, tags$b("profStep"),
                          column(12,
                            numericInput("profStep", "", 0.01)
                            )
                          ),
                        "\t",
                        column(4, tags$b("center"),
                          column(12,
                            numericInput("center", "", 3, min = 1)
                            )
                          )
                        )
                      )
                    )
                  )
                )
              )
              ),

          tags$br(),
          actionButton("go2", "GO", style="float:right"),
          tags$br(),
          tags$br(), 
          radioButtons('formatProcessAnalysis', 'Document format', c('PDF', 'HTML', 'Word'), inline = TRUE),
          downloadButton("reportProcessAnalysis", "Generate report")
          #verbatimTextOutput("ppm")         
          ),
          tabPanel(title = tags$b("Annotation"), value = "panel3", 
            fixedRow(
              sidebarPanel(style="background-color: lightblue",
                           ###< By Guanlin       
                           h5(helpText("Select sample orgin below")),
                           selectInput(inputId = "sample origin", label = "sample origin", choices = c("human","mouse","others"), selected = "human", multiple = FALSE),
                           selectInput(inputId = "sample type", label = "sample type", 
                                       choices = c("None","serum","urine","milk","liver","brain","brown adipose tissue","white adipose tissue","others"), 
                                       selected = "None", multiple = FALSE),
                           tags$hr(),
                           textInput("text", "text (please specify others):", "input text")
                           #submitButton("Submit") 
                           ###By Guanlin >      
                           
              ), 
              column(8,  
                bsCollapse(id = "collapse", open = "Upload Data",
                  bsCollapsePanel("Upload Data", style = "info",
                column(4, tags$br(), 
                       checkboxInput('header', 'Header', TRUE),
                       radioButtons('sep', 'Separator', c(Comma=',',  Tab='\t'), ',')
                       
                    ),
                column(7, 
                  fileInput('file1', '', 
                  accept=c('text/csv', 'text/comma-separated-values,text/plain', '.csv')),
                    
                 
                  shinyDirButton('directory3_1', 'Output directory', 'Please select a folder'),
                  tags$br(),    
                  verbatimTextOutput('directorypath3_1')
                ) 
                    )
                )
                )
              ),
              bsCollapse(id = "collapse2", 
                bsCollapsePanel("Parameters", style = "info",                         
                  fixedRow(
                    column(12, 
                      column(3, tags$b("max.mz.diff"),
                        column(12,
                          numericInput("max.mz.diff", "", 10, min = 1)
                          )
                        ),
                      "\t",
                      column(3, tags$b("max.rt.diff"),
                        column(12,
                          numericInput("max.rt.diff", "", 10, min = 1)
                          )
                        ),
                      "\t",
                      column(3, tags$b("corthresh"),
                        column(12,
                          numericInput("corthresh", "", 0.7)
                          )
                        ),
                      "\t",
                      column(3, tags$b("max_isp"),
                             column(12,
                                    numericInput("max_isp", "", 5, min = 1)
                             )
                      )
                      )
                    ),
      
                    tags$br(),
                    fixedRow(
                      column(12,
                        #h4("Peak Picking",style="-webkit-transform: skew(10deg);",tags$hr(style="border-color: purple;")),
                        column(3, tags$b("mass_defect_window"),
                          column(12,
                            numericInput("mass_defect_window", "", 0.01)
                             )
                           ),
                         "\t",
                        column(3, tags$b("num_sets"),
                          column(12,
                            numericInput("num_sets", "", 300, min=1)
                            )
                          ),
                         "\t",
                         column(3, tags$b("num_nodes"),
                           column(12,
                             numericInput("num_nodes", "", 5, min=2)
                             )
                          ),
                         column(3, tags$b("Ionization mode"),
                           column(12,
                             selectInput("var4", "",
                               c("pos" = "pos",
                                 "neg" = "neg")
                                 )
                               )
                          )
                      ),

                      tags$br(),
                      fixedRow(
                        column(12,
                          #h4("Peak Picking",style="-webkit-transform: skew(10deg);",tags$hr(style="border-color: purple;")),
                          column(3, tags$b("Database"),
                            column(12,
                              selectInput("var1", "",
                                c("HMDB" = "HMDB",
                                  "KEGG" = "KEGG",
                                  "TMDB" = "TMDB",
                                  "LipidMaps" = "LipidMaps")
                                )
                              )
                            ),
                          "\t",
                          column(5, tags$b("Status"),
                            column(12,
                              selectInput("var2", "",
                                c("Detected and Quantified" = "Detected and Quantified",
                                  "Detected and Not Quantified" = "Detected and Not Quantified",
                                  "Expected and Not Quantified" = "Expected and Not Quantified"
                                )
                              )
                            )
                          ),
                          "\t",
                          column(4, tags$b("Adducts"),
                            column(12,
                              selectInput("var3", "", selected = "M+H", multiple = TRUE, selectize=FALSE,
                                c(
                                  "M+H" = "M+H",
                                  "M+2H" = "M+2H",
                                  "M+H+NH4" = "M+H+NH4",
                                  "M+ACN+2H" = "M+ACN+2H",
                                  "M+2ACN+2H" = "M+2ACN+2H",
                                  "M+NH4" = "M+NH4",
                                  "M+Na" = "M+Na",
                                  "M+ACN+H" = "M+ACN+H",
                                  "M+ACN+Na" = "M+ACN+Na",
                                  "M+2ACN+H" = "M+2ACN+H",
                                  "2M+H" = "2M+H",
                                  "2M+Na" = "2M+Na",
                                  "2M+ACN+H" = "2M+ACN+H",
                                  "M+2Na-H" = "M+2Na-H",
                                  "M+H-H2O" = "M+H-H2O",
                                  "M+H-2H2O" = "M+H-2H2O",
                                  "M-H" = "M-H",
                                  "M-H2O-H" = "M-H2O-H",
                                  "M+Na-2H" = "M+Na-2H",
                                  "M+Cl" = "M+Cl",
                                  "M+FA-H" = "M+FA-H"
                                )
                                  )
                               )
                           )
                        )
                      
                    )
                 )
              )
            ),
                           
                           
       tags$br(),
       actionButton("go3", "GO", style="float:right"),
       tags$br(),
       tags$br(), 
       radioButtons('formatAnnotAnalysis', 'Document format', c('PDF', 'HTML', 'Word'), inline = TRUE),
       downloadButton("reportAnnotAnalysis", "Generate report")
       ),
       tabPanel(title = tags$b("StatisticalAnalysis"), value = "panel4", 
         bsCollapse(id = "collapse3", 
            bsCollapsePanel("Differential Analysis", style = "info",
              fixedRow(
                column(12, style="background-color: lightgrey;",
                   column(6, h4(tags$u("Upload data")), style="background-color: lightgrey;", 
                      fileInput('file2', '', accept=c('text/csv', 'text/comma-separated-values,text/plain', '.csv')),
                      checkboxInput('header', 'Header', TRUE),
                      radioButtons('sep1', 'Separator', c(Comma=',',  Tab='\t'), ','),
                      shinyDirButton('directory4_1', 'Output directory', 'Please select a folder'),
                      verbatimTextOutput('directorypath4_1')
                               
                                
                         ),
                   column(6, style="background-color: lightgrey;",h4(tags$u("Choose groups for comparision")),
                          #textInput("ref", "Mention the reference group name:", "Type here"),
                          column(3,
                          radioButtons('comp', '', c(Reference='refGrp',  Groups='grpComb'), 'refGrp')
                          ),
                          column(9,
                                 selectizeInput("grp4comp", label="", choices="", multiple=TRUE)
                          )
                         ),
                   
                   
                   column(6, style="background-color: lightgrey;",h4(tags$u("Cutt offs for statistical parameters")),
                     h6(tags$u("F Statistics")),
                            column(3,
                                   radioButtons('Fparam', '', c(P='Fpvalue',  Adj.P='Fadjpvalue'), 'Fpvalue')
                            ),
                            column(6,
                                   numericInput("FparamValue", "Value < =", 1, step = 0.1)
                            )
                           
                       
                     ),
                   column(6, style="background-color: lightgrey;", h6(tags$u("All Statistics")),
                   column(6,
                       numericInput("Allpvalue", "P Value < =", 1, step = 0.1)
                       ),
                   column(6,
                          textInput("Allfcvalue", "log(Fold Change) < =", "")
                          #numericInput("Allfcvalue", "log(Fold Change) < =", maxCoef)
                     
                   )
                )                
                  
                ),
                column(12, style="background-color: lightgrey;",
                       column(6
                       ),
                       column(6,tags$hr(style="border-top: 1px solid #8c8b8b;"), 
                       column(6, 
                              radioButtons('formatDiffAnalysis', '', c('PDF', 'HTML', 'Word'), inline = TRUE),
                              downloadButton("reportDiffAnalysis", "Generate report")
                       ),
                     column(6,
                            tags$br(),tags$br(),
                            actionButton("go4", "GO", style="float:right")
                      
                     )
                       )
                      )
                   )
                ),
            
                
            bsCollapsePanel("Network Analysis", style = "info",                         
              fixedRow(
                column(12, h4(tags$u("Upload data")),style="background-color: lightgrey;",
                       column(6, style="background-color: lightgrey;", 
                              fileInput('file3', '', accept=c('text/csv', 'text/comma-separated-values,text/plain', '.csv')),
                              checkboxInput('header', 'Header', TRUE),
                              radioButtons('sep2', 'Separator', c(Comma=',',  Tab='\t'), ','),
                              shinyDirButton('directory4_2', 'Output directory', 'Please select a folder'),
                              verbatimTextOutput('directorypath4_2')        
                              ),
                
                column(6, style="background-color: lightgrey;",h4("Analysis options"),
                       #textInput("grp", "Choose Group name:", "NULL"),
                       selectizeInput("grp", label="Choose Group name:", choices="ALL", multiple=TRUE),
                       
                       checkboxGroupInput("var6", "Select output options:",
                                          c("GRAPHML" = "graphML",
                                            "STATS" = "stats"
                                            ))
                        ),
                column(12, style="background-color: lightgrey;",
                       actionButton("go5", "GO", style="float:right"),
                       tags$br(), 
                       radioButtons('formatNetwrkAnalysis', 'Document format', c('PDF', 'HTML', 'Word'), inline = TRUE),
                       downloadButton("reportNetwrkAnalysis", "Generate report")
                       
                      ) 
                
                   )
                
                )
              
              )
            
            
            
           )
         ),
       
       tabPanel(title = tags$b("Visualization"), value = "panel5", 
         bsCollapse(id = "collapse4", open = "Visualization",
           bsCollapsePanel("Visualization", style = "info", 
             fluidPage(
               navbarPage("",
                                     
                 tabPanel(title = tags$b("Heatmap"), value = "panel4.1", 
                   fixedRow(
                     column(12, style="background-color: lightgrey;",
                       column(6, h4(tags$u("Upload data")), style="background-color: lightgrey;", 
                                 fileInput('file4_1', '', accept=c('text/csv', 'text/comma-separated-values,text/plain', '.csv')),
                                 checkboxInput('header', 'Header', TRUE),
                                 radioButtons('sep3_1', 'Separator', c(Comma=',',  Tab='\t'), ',')
                                 
                             ),
                        column(6, tags$br(),tags$br(),
                               
                               actionButton("go6_1", "GO", style="float:right")
                                  
                               )
                            )
                     
                           ),
                 
                   
                   
                     column(12, tags$hr(),
                            
                            downloadButton("downloadHeatmap", "Download"),
                            #plotOutput("plotHeatmap")
                            d3heatmapOutput("plotHeatmapInteract")    
                     
                   
                        )),
                 tabPanel(title = tags$b("PCA Plot"), value = "panel4.2",
                          fixedRow(
                            column(12, style="background-color: lightgrey;",
                                   column(6, h4(tags$u("Upload data")), style="background-color: lightgrey;", 
                                          fileInput('file4_2', '', accept=c('text/csv', 'text/comma-separated-values,text/plain', '.csv')),
                                          checkboxInput('header', 'Header', TRUE),
                                          radioButtons('sep3_2', 'Separator', c(Comma=',',  Tab='\t'), ',')
                                          
                                         ),
                                   column(6, tags$br(),tags$br(),
                                          
                                          actionButton("go6_2", "GO", style="float:right")
                                          
                                         )
                                  )
                                  ),
                          
                          column(12, tags$hr(),
                                 downloadButton("downloadPCA", "Download"),
                                 plotOutput("plotPCA")
                           
                          ) 
                         ),
                 tabPanel(title = tags$b("RLA Plot"), value = "panel4.3",
                          fixedRow(
                            column(12, style="background-color: lightgrey;",
                                   column(6, h4(tags$u("Upload data")), style="background-color: lightgrey;", 
                                          fileInput('file4_3', '', accept=c('text/csv', 'text/comma-separated-values,text/plain', '.csv')),
                                          checkboxInput('header', 'Header', TRUE),
                                          radioButtons('sep3_3', 'Separator', c(Comma=',',  Tab='\t'), ',')
                                          
                                   ),
                                   column(6, tags$br(),tags$br(),
                                          
                                          actionButton("go6_3", "GO", style="float:right")
                                          
                                   )
                            )
                          ),
                          
                          column(12, tags$hr(),
                                 downloadButton("downloadRLA", "Download"),
                                 plotOutput("plotRLA")
                                 
                          )
                                                                  
                         ),
                 tabPanel(title = tags$b("Level Plot"), value = "panel4.4",
                          fixedRow(
                            column(12, style="background-color: lightgrey;",
                                   column(6, h4(tags$u("Upload data")), style="background-color: lightgrey;", 
                                          fileInput('file4_4', '', accept=c('text/csv', 'text/comma-separated-values,text/plain', '.csv')),
                                          checkboxInput('header', 'Header', TRUE),
                                          radioButtons('sep3_4', 'Separator', c(Comma=',',  Tab='\t'), ',')
                                          
                                   ),
                                   column(6, tags$br(),tags$br(),
                                          
                                          actionButton("go6_4", "GO", style="float:right")
                                          
                                   )
                            )
                          ),
                          
                          column(12, tags$hr(),
                                 downloadButton("downloadLevel", "Download"),
                                 plotOutput("plotLevel")
                                 
                          )
                                                                  
                         )
                                                         
                   )
               )  
            )      
            )
       )
       )
       
    )
    
    #fixedRow(
    #  column(12, style="background-color: lightgrey;",
    #         fluidPage(DT::dataTableOutput('tbl'))         
    #  ),
    # ) 
      
    
    
    
    
    
      ),
    
    tabPanel("Documentation", includeMarkdown("Documentation.Rmd")),
    tabPanel("Tutorial", includeMarkdown("Tutorial.Rmd")),
    tabPanel("Contact"),
    
  column(12,
         tags$hr(style=" border: 0; 
  height: 7px; 
  background-image: -webkit-linear-gradient(left, #b4c8ce, #0e6c87, #b4c8ce);
            border-bottom: 2px solid black;"
         ),
         column(3,
                img(src="ONR.png", height="50px", align = "left") 
         ),
         column(6,
                h5("The development of this software was funded by the US Office of Naval Research under grant #N000141512377 to the University of Aberdeen")    
         ),
         column(3,
                img(src="UoA.png", height="40px", align = "right") 
         )
         
  ) 
    )
   )
  
  
 
  
  server <- function(input, output, session) {
    #volumes <- c('R Installation'=R.home())
    #volumes <- c("UserFolder"="/home/s03js6/JeeTzzz")
    volumes <- getVolumes()
    
    shinyjs::disable("reportAutopipelineAnalysis")
    shinyjs::disable("reportProcessAnalysis")
    shinyjs::disable("reportAnnotAnalysis")
    shinyjs::disable("reportDiffAnalysis")
    shinyjs::disable("reportNetwrkAnalysis")
    shinyjs::disable("downloadHeatmap")
    shinyjs::disable("downloadPCA")
    shinyjs::disable("downloadRLA")
    shinyjs::disable("downloadLevel")
    
    
    shinyDirChoose(input, 'directory1_1', roots=volumes, session=session, restrictions=system.file(package='base'))
    output$directorypath1_1 <- renderText({if(is.null(input$directory1_1)){"None Selected"}else{parseDirPath(volumes, input$directory1_1)}})
    shinyDirChoose(input, 'directory1_2', roots=volumes, session=session, restrictions=system.file(package='base'))
    output$directorypath1_2 <- renderText({if(is.null(input$directory1_2)){"None Selected"}else{parseDirPath(volumes, input$directory1_2)}})
    shinyDirChoose(input, 'directory2_1', roots=volumes, session=session, restrictions=system.file(package='base'))
    output$directorypath2_1 <- renderText({if(is.null(input$directory2_1)){"None Selected"}else{parseDirPath(volumes, input$directory2_1)}})
    shinyDirChoose(input, 'directory2_2', roots=volumes, session=session, restrictions=system.file(package='base'))
    output$directorypath2_2 <- renderText({if(is.null(input$directory2_2)){"None Selected"}else{parseDirPath(volumes, input$directory2_2)}})
    shinyDirChoose(input, 'directory3_1', roots=volumes, session=session, restrictions=system.file(package='base'))
    output$directorypath3_1 <- renderText({if(is.null(input$directory3_1)){"None Selected"}else{parseDirPath(volumes, input$directory3_1)}})
    shinyDirChoose(input, 'directory3_2', roots=volumes, session=session, restrictions=system.file(package='base'))
    output$directorypath3_2 <- renderText({if(is.null(input$directory3_2)){"None Selected"}else{parseDirPath(volumes, input$directory3_2)}})
    shinyDirChoose(input, 'directory4_1', roots=volumes, session=session, restrictions=system.file(package='base'))
    output$directorypath4_1 <- renderText({if(is.null(input$directory4_1)){"None Selected"}else{parseDirPath(volumes, input$directory4_1)}})
    shinyDirChoose(input, 'directory4_2', roots=volumes, session=session, restrictions=system.file(package='base'))
    output$directorypath4_2 <- renderText({if(is.null(input$directory4_2)){"None Selected"}else{parseDirPath(volumes, input$directory4_2)}})
   
    source("pipeline.R")
    
    observeEvent(input$go, {
      updateTabsetPanel(session, "inTabset",
                        selected = paste0("panel1", input$controller)
      )
      
      

      })
    
    
    observeEvent(input$go1, {
      cwd<-getwd()
      #validate(
      #  need(input$directory1_1 == "", 'select input directory!'),
      #  need(input$directory1_2 == "", 'select output directory!')
      #)
      
      inputDir<-print({parseDirPath(volumes, input$directory1_1)})
      outputDir<-print({parseDirPath(volumes, input$directory1_2)})
      
      runAutopipeline(inputDir,outputDir)
      
      
      setwd(cwd)
      shinyjs::enable("reportAutopipelineAnalysis")
      output$reportAutopipelineAnalysis <- downloadHandler(
        # For PDF output, change this to "report.pdf"
        filename = function() {
          paste('reportAutopipelineAnalysis', sep = '.', switch(
            input$formatAutopipelineAnalysis, PDF = 'pdf', HTML = 'html', Word = 'docx'
          ))
        },
        
        content = function(file) {
          
          src <- normalizePath('reportAutopipelineAnalysis.Rmd')
          owd <- setwd(tempdir())
          on.exit(setwd(owd))
          file.copy(src, 'reportAutopipelineAnalysis.Rmd', overwrite = TRUE)
          out <- render('reportAutopipelineAnalysis.Rmd', switch(
            input$formatAutopipelineAnalysis,
            PDF = pdf_document(), HTML = html_document(), Word = word_document()
          ))
          file.rename(out, file)
        }
      )
      
      print("Job Done...")
      
      
    })
    
      
    observeEvent(input$go2, {
      cwd<-getwd()

     
      inputDir<-print({parseDirPath(volumes, input$directory2_1)})
      outputDir<-print({parseDirPath(volumes, input$directory2_2)})
      
      ppm=input$ppm
      peakwidth=c(input$peakwidth[1],input$peakwidth[2])
      snthresh=input$snthresh
      prefilter=c(input$prefilter[1], input$prefilter[2])
      integrate=input$integrate
      mzdiff=input$mzdiff
      nSlaves=input$nSlaves
      retcorMethod=input$var
      profStep=input$profStep
      center=input$center
      runPreprocess(inputDir,outputDir,ppm,peakwidth,snthresh,prefilter,integrate,mzdiff,nSlaves,retcorMethod,profStep,center)
      
      setwd(cwd)
      shinyjs::enable("reportProcessAnalysis")
      output$reportProcessAnalysis <- downloadHandler(
        # For PDF output, change this to "report.pdf"
        filename = function() {
          paste('reportProcessAnalysis', sep = '.', switch(
            input$formatProcessAnalysis, PDF = 'pdf', HTML = 'html', Word = 'docx'
          ))
        },
        
        content = function(file) {
          
          src <- normalizePath('reportProcessAnalysis.Rmd')
          owd <- setwd(tempdir())
          on.exit(setwd(owd))
          file.copy(src, 'reportProcessAnalysis.Rmd', overwrite = TRUE)
          out <- render('reportProcessAnalysis.Rmd', switch(
            input$formatProcessAnalysis,
            PDF = pdf_document(), HTML = html_document(), Word = word_document()
          ))
          file.rename(out, file)
        }
      )
      
      print("Job Done...")
      
    })
    
    
    
    observeEvent(input$go3, {
      
      
      
      inFile1 <- input$file1
      if (is.null(inFile1))
        return(NULL)
      dataA<-read.table(inFile1$datapath, header=input$header, sep=input$sep)
      filenameMet <- inFile1$name
      max.mz.diff=input$max.mz.diff
      max.rt.diff=input$max.rt.diff
      
      num_nodes=input$num_nodes
      queryadductlist=input$var3
      mode=input$var4
      outDir=print({parseDirPath(volumes, input$directory3_1)})
      db_name=input$var1
      num_sets=input$num_sets
      corthresh=input$corthresh
      #biofluid.location=NA
      status=input$var2
      max_isp=input$max_isp
      mass_defect_window=input$mass_defect_window
      
      
      
      runAnnotation(dataA,outDir,max.mz.diff,max.rt.diff,num_nodes,queryadductlist,mode,db_name,num_sets,corthresh,status,max_isp,mass_defect_window)
      
      cwd<-getwd()
      setwd(cwd)
      shinyjs::enable("reportAnnotAnalysis")
      output$reportAnnotAnalysis <- downloadHandler(
        # For PDF output, change this to "report.pdf"
        filename = function() {
          paste('reportAnnotAnalysis', sep = '.', switch(
            input$formatAnnotAnalysis, PDF = 'pdf', HTML = 'html', Word = 'docx'
          ))
        },
        
        content = function(file) {
          
          src <- normalizePath('reportAnnotAnalysis.Rmd')
          owd <- setwd(tempdir())
          on.exit(setwd(owd))
          file.copy(src, 'reportAnnotAnalysis.Rmd', overwrite = TRUE)
          out <- render('reportAnnotAnalysis.Rmd', switch(
            input$formatAnnotAnalysis,
            PDF = pdf_document(), HTML = html_document(), Word = word_document()
          ))
          file.rename(out, file)
        }
      )
      
      print("Job Done...")
     
      })
    
    observe({
      shinyFileChoose(input, "file2", roots = volumes, session = session)
      
      inFile2 <- input$file2
    if (is.null(inFile2))
      return(NULL)
    met<-read.table(inFile2$datapath, header=input$header, sep=input$sep1, row.names=1)
    filenameMet <- inFile2$name
    pathMet <- renderText({parseFilePaths(volumes, input$file2)})
     
    file2name <- print(gsub(".txt|.csv", "",inFile2$name))
    tmpGrp = met$Group
    
    grpUniq<-unique(tmpGrp)
    grpSel <- c()
    for (i in 1:length(grpUniq)) {
      grpSel <- c(grpSel, paste0(grpUniq[i], "-VS-",grpUniq[-which(grpUniq %in% grpUniq[i])]))
      
    }
    
    refGrp <- "refGrp"  %in% input$comp
    grpComb <- "grpComb"  %in% input$comp
    if (refGrp){
      updateSelectizeInput(session,'grp4comp', choices=tmpGrp, selected=tmpGrp)
    } else if (grpComb){
      updateSelectizeInput(session,'grp4comp', choices=grpSel, selected=grpSel[1:2])
    }
      

    save(met,file2name,filenameMet,pathMet, file="met.RData")
    })
    
    #source("runDiffAnalysis.R")
      observeEvent(input$go4, {
      
      load("met.RData")
     
      grp4comp=print(input$grp4comp)
      
      Fpvalue <- "Fpvalue"  %in% input$Fparam
      Fadjpvalue <- "Fadjpvalue"  %in% input$Fparam
      if (Fpvalue){
        FparamName<-"P.Value"
      } else if (Fadjpvalue){
        FparamName<-"adj.P.Val"
      }
      FparamValue<-input$FparamValue
      
      AllPvalue<-input$Allpvalue
      if(input$Allfcvalue == ""){
        AllFCvalue <- "NULL"
      } else if(is.na(as.numeric(input$Allfcvalue))){
        AllFCvalue <- "NULL"
      } else{
        AllFCvalue<-as.numeric(input$Allfcvalue)
      }
      print(AllFCvalue)
      outputDir<-print({parseDirPath(volumes, input$directory4_1)})
      cwd<-getwd()
      setwd(outputDir)
      dir.create("runDiffAnalysisRes")
      setwd("runDiffAnalysisRes")
      runDiffAnalysis(met, grp4comp, FparamName, FparamValue, AllPvalue, AllFCvalue, file2name)
      
      setwd(cwd)
      shinyjs::enable("reportDiffAnalysis")
      output$reportDiffAnalysis <- downloadHandler(
        # For PDF output, change this to "report.pdf"
        filename = function() {
          paste('reportDiffAnalysis', sep = '.', switch(
            input$formatDiffAnalysis, PDF = 'pdf', HTML = 'html', Word = 'docx'
          ))
        },
        
        content = function(file) {
         
          src <- normalizePath('reportDiffAnalysis.Rmd')
          owd <- setwd(tempdir())
          on.exit(setwd(owd))
          file.copy(src, 'reportDiffAnalysis.Rmd', overwrite = TRUE)
          out <- render('reportDiffAnalysis.Rmd', switch(
            input$formatDiffAnalysis,
            PDF = pdf_document(), HTML = html_document(), Word = word_document()
          ))
          file.rename(out, file)
        }
      )
      
      print("Job Done...")
        
      })
    
      
        #source("runCMI.R")
        
        #validate(
        #  need((!is.null(input$file3)), message = "Please select a file")
        #)
        observe({
        inFile3 <- input$file3
        if (is.null(inFile3))
          return(NULL)
        metMZ<-read.table(inFile3$datapath, header=input$header, sep=input$sep2, row.names=1)
        filenameMet <- inFile3$name
        file3name <- print(gsub(".txt|.csv", "",inFile3$name))
        tmpgroups = metMZ$Group
        updateSelectizeInput(session,'grp', choices=tmpgroups, selected=tmpgroups)
        save(metMZ,file3name,filenameMet, file="met.RData")
        })
        
        observeEvent(input$go5, {
        load("met.RData")
        Grp=print(input$grp)
        if(Grp=="ALL"){
          GrpMetMZ <- metMZ
        } else{
          GrpMetMZ <- rbind()
          for(i in 1:length(Grp)){
            
          GrpMetMZ <- rbind(GrpMetMZ,subset(metMZ, Group == Grp[i]))
          }
        }
        graphML <- "graphML"  %in% input$var6
        stats <- "stats"  %in% input$var6
        if (is.null(input$var6) ){
          graphML <- "NULL"
          stats <- "NULL"
        } else if (graphML & stats){
          graphML <- "graphML"
          stats <- "stats"
        } else if (graphML){
          graphML <- "graphML"
        } else if (stats){
          stats <- "stats"
        } 
        outputDir<-print({parseDirPath(volumes, input$directory4_2)})
        cwd<-getwd()
        setwd(outputDir)
        dir.create("runCMIanalysisRes")
        setwd("runCMIanalysisRes")
        runCMI(GrpMetMZ, irlba = FALSE, graphML = graphML, stats = stats, prefix = file3name)
        setwd(outputDir)
        
        setwd(cwd)
        shinyjs::enable("reportNetwrkAnalysis")
        output$reportNetwrkAnalysis <- downloadHandler(
          # For PDF output, change this to "report.pdf"
          filename = function() {
            paste('reportNetwrkAnalysis', sep = '.', switch(
              input$formatNetwrkAnalysis, PDF = 'pdf', HTML = 'html', Word = 'docx'
            ))
          },
          
          content = function(file) {
            
            src <- normalizePath('reportNetwrkAnalysis.Rmd')
            owd <- setwd(tempdir())
            on.exit(setwd(owd))
            file.copy(src, 'reportNetwrkAnalysis.Rmd', overwrite = TRUE)
            out <- render('reportNetwrkAnalysis.Rmd', switch(
              input$formatNetwrkAnalysis,
              PDF = pdf_document(), HTML = html_document(), Word = word_document()
            ))
            file.rename(out, file)
          }
        )
        
        print("Job Done...")
        
        })
        
        
        #source("runViz.R")
        observeEvent(input$go6_1, {
          
          
          inFile4_1 <- input$file4_1
          if (is.null(inFile4_1))
            return(NULL)
          met.log<-read.table(inFile4_1$datapath, header=input$header, sep=input$sep3_1, row.names = 1)
          file4_1name <- print(gsub(".txt|.csv", "",inFile4_1$name))
          
          met.3 <- as.matrix(met.log)
          
          output$plotHeatmapInteract <- renderD3heatmap({
          withProgress(message = 'Plotting heatmap:', value = 0, {
          n <- NROW(met.log)
          for (i in 1:n) {
            incProgress(1/n, detail = "Please wait...")
            #incProgress(1/n, detail = paste("Percentage completed:", (i/n)*100, "%"))
          }
          d3heatmap(met.log, scale = "column")
          })
          })
          
          shinyjs::enable("downloadHeatmap")
          
          output$downloadHeatmap <- downloadHandler(
            filename <- "heatmap.png",
            content <- function(file) {
              png(file)
              
              heatmap(met.3)
              dev.off()
            }
          )
         
          print("Job Done...")
          
          
        })
        
        
        observeEvent(input$go6_2, {
          
          
          inFile4_2 <- input$file4_2
          if (is.null(inFile4_2))
            return(NULL)
          met.log<-read.table(inFile4_2$datapath, header=input$header, sep=input$sep3_2)
          file4_2name <- print(gsub(".txt|.csv", "",inFile4_2$name))
          
          Group <- met.log$Group
          meta<-as.matrix(met.log[,-1])
          
          # replace all non-finite values with 0
          meta[!is.finite(meta)] <- 0
          
          plsda.dol<-plsda(meta, Group, ncomp = 2, logratio = "none")
          
          
          output$plotPCA <- renderPlot({
            withProgress(message = 'Plotting PCA plot:', value = 0, {
              n <- NROW(met.log)
              for (i in 1:n) {
                incProgress(1/n, detail = "Please wait...")
                #incProgress(1/n, detail = paste("Percentage completed:", (i/n)*100, "%"))
              }
              plotIndiv(plsda.dol, ellipse=TRUE, legend=TRUE, title="Individual Group's score", legend.position="bottom")
              
            })
          })
          
          shinyjs::enable("downloadPCA")
          
          output$downloadPCA <- downloadHandler(
            filename <- "PCA.png",
            content <- function(file) {
              png(file)
              
              plotIndiv(plsda.dol, ellipse=TRUE, legend=TRUE, title="Individual Group's score", legend.position="bottom")
              
              dev.off()
            }
          )
          
          print("Job Done...")
          
        })
        
        
        observeEvent(input$go6_3, {
          
          
          inFile4_3 <- input$file4_3
          if (is.null(inFile4_3))
            return(NULL)
          met.log<-read.table(inFile4_3$datapath, header=input$header, sep=input$sep3_3)
          file4_3name <- print(gsub(".txt|.csv", "",inFile4_3$name))
          
          
          
          
          output$plotRLA <- renderPlot({
            withProgress(message = 'Plotting RLA plot:', value = 0, {
              n <- NROW(met.log)
              for (i in 1:n) {
                incProgress(1/n, detail = "Please wait...")
                #incProgress(1/n, detail = paste("Percentage completed:", (i/n)*100, "%"))
              }
              par(mfrow=c(1,2))
              RlaPlots(met.log, main = "RLA Plot Across Groups", type = "ag", ylim = c(5, -5))  #across groups
              RlaPlots(met.log, main = "RLA Plot Within Groups", type = "wg", ylim = c(5, -5))  #within groups
            })
          })
          
          shinyjs::enable("downloadRLA")
          
          output$downloadRLA <- downloadHandler(
            filename <- "RLA.png",
            content <- function(file) {
              png(file)
              par(mfrow=c(1,2))
              RlaPlots(met.log, main = "RLA Plot Across Groups", type = "ag", ylim = c(5, -5))  #across groups
              RlaPlots(met.log, main = "RLA Plot Within Groups", type = "wg", ylim = c(5, -5))  #within groups
              dev.off()
            }
          )
          
          print("Job Done...")
          
        })
        
        observeEvent(input$go6_4, {
          
          
          inFile4_4 <- input$file4_4
          if (is.null(inFile4_4))
            return(NULL)
          met.log<-read.table(inFile4_4$datapath, header=input$header, sep=input$sep3_4)
          file4_4name <- print(gsub(".txt|.csv", "",inFile4_4$name))
          
          
          met.3 <- as.matrix(met.log[, -1])
          trans.met.log <- t(met.log[, -1])
          colnames(trans.met.log) <- met.log[, 1]
          cormat <- cor(trans.met.log, use = "complete.obs")
          
          
          output$plotLevel <- renderPlot({
            withProgress(message = 'Plotting Level plot:', value = 0, {
              n <- NROW(met.log)
              for (i in 1:n) {
                incProgress(1/n, detail = "Please wait...")
                #incProgress(1/n, detail = paste("Percentage completed:", (i/n)*100, "%"))
              }
              levelplot(cormat, main = "Correlation between samples", scales = list(x = list(rot = 90)))              
            })
          })
          
          shinyjs::enable("downloadLevel")
          
          output$downloadLevel <- downloadHandler(
            filename <- "LevelPlot.png",
            content <- function(file) {
              png(file)
              
              levelplot(cormat, main = "Correlation between samples", scales = list(x = list(rot = 90)))              
              dev.off()
            }
          )
          
          print("Job Done...")
          
        })
    
     }
  
    shinyApp(ui, server)
    
  }
  
