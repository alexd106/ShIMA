
  # Call all the dependencies
  source("support.R")
  
  if(interactive()){
    ui <- fluidPage(tags$style(type = "text/css", ".navbar {background-color: #004d5a; font-size:120%; }"),
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
                  tags$br(),
                  actionButton("go1", "GO", style="float:right")
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
          tags$br()
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
                                c("M+H" = "M+H",
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
       tags$br()
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
                          
                          radioButtons('comp', '', c(Reference='refGrp',  Groups='grpComb'), ','),
                          selectizeInput("grp4comp", label="", choices="", multiple=TRUE)
                          
                         )
                ),
                column(12, style="background-color: lightgrey;",
                     actionButton("go4", "GO", style="float:right"),
                     tags$br() 
                     
                    
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
                       tags$br()       
                      ) 
                
                   )
                
                )
              
              ),
            
            bsCollapsePanel("Visualization", style = "info",
                            fixedRow(
                              column(12, style="background-color: lightgrey;",
                                     column(6, h4(tags$u("Upload data")), style="background-color: lightgrey;", 
                                            fileInput('file4', '', accept=c('text/csv', 'text/comma-separated-values,text/plain', '.csv')),
                                            checkboxInput('header', 'Header', TRUE),
                                            radioButtons('sep3', 'Separator', c(Comma=',',  Tab='\t'), ','),
                                            shinyDirButton('directory4_3', 'Output directory', 'Please select a folder'),
                                            verbatimTextOutput('directorypath4_3')
                                            
                                            
                                     ),
                                     column(6, style="background-color: lightgrey;",h4(tags$u("Chose plots")),
                                           checkboxGroupInput("var7", "",
                                                               c("Heatmap" = "heatmap",
                                                                 "PCA" = "pca",
                                                                 "Level" = "lvl",
                                                                 "RLA" = "rla"))
                                            
                                            
                                     )
                              ),
                              column(12, style="background-color: lightgrey;",
                                     actionButton("go6", "GO", style="float:right"),
                                     tags$br() 
                                     
                                     
                              )
                            )
            )
           )
         )
       )
      
      
    ),
    
    fixedRow(
      column(12, style="background-color: lightgrey;",
             fluidPage(DT::dataTableOutput('tbl'))         
      )
    )
    
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
    volumes <- c("UserFolder"="/home/s03js6/JeeTzzz")
    #volumes <- getVolumes()
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
    shinyDirChoose(input, 'directory4_3', roots=volumes, session=session, restrictions=system.file(package='base'))
    output$directorypath4_3 <- renderText({if(is.null(input$directory4_3)){"None Selected"}else{parseDirPath(volumes, input$directory4_3)}})
    
    source("pipeline.R")
    
    observeEvent(input$go, {
      updateTabsetPanel(session, "inTabset",
                        selected = paste0("panel1", input$controller)
      )
      
      

      })
    
    
    observeEvent(input$go1, {
      
      #validate(
      #  need(input$directory1_1 == "", 'select input directory!'),
      #  need(input$directory1_2 == "", 'select output directory!')
      #)
      
      inputDir<-print({parseDirPath(volumes, input$directory1_1)})
      outputDir<-print({parseDirPath(volumes, input$directory1_2)})
      
      runAutopipeline(inputDir,outputDir)
      print("Job Done...")
      
      
    })
    
      
    observeEvent(input$go2, {
#      output$ppm <- renderText({ input$peakwidth[1] })
      
#      setwd("print({parseDirPath(volumes, input$directory2_1)})")
     
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
      runXCMS(inputDir,outputDir,ppm,peakwidth,snthresh,prefilter,integrate,mzdiff,nSlaves,retcorMethod,profStep,center)
      print("Job Done...")
      
    })
    
    
    
    observeEvent(input$go3, {
      #output$txt <- renderText({ 
        
       # withProgress(message = 'Running...Wait...', value = 0.1, {
        #  Sys.sleep(0.25)
        #})
        
        
        #print("Job Done")
      #})
      
      
      inFile1 <- input$file1
      if (is.null(inFile1))
        return(NULL)
      dataA<-read.table(inFile1$datapath, header=input$header, sep=input$sep)
      
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
      print("Job Done...")
      
      #system.time(annotres<-multilevelannotation(dataA=dataA,,,,,,,,, ,,allsteps=TRUE,,NOPS_check=TRUE,customIDs=customIDs,missing.value=NA,deepsplit=2,networktype="unsigned",minclustsize=10,module.merge.dissimilarity=0.2,filter.by=c("M+H"),,origin=NA,,boostIDs=NA,,HMDBselect="union",,pathwaycheckmode="pm",mass_defect_mode="pos"))
      #setwd("print({parseDirPath(volumes, input$directory3_1)})")
      #d1$mz<-round(d1$mz, digits=4)
      #inFile1$time<-round(d1$time, digits=1)
      #d2<-read.csv("Stage5.csv",header=T)
      #d<-merge(d1, d2, by=c("mz","time"))
      
      })
    
    observe({
      inFile2 <- input$file2
    if (is.null(inFile2))
      return(NULL)
    met<-read.table(inFile2$datapath, header=input$header, sep=input$sep1)
    file2name <- print(gsub(".txt|.csv", "",inFile2$name))
    tmpGrp = met$Group
    
    grpUniq<-unique(tmpGrp)
    grpSel <- c()
    for (i in 1:length(grpUniq)) {
      grpSel <- c(grpSel, paste0(grpUniq[i], "/",grpUniq[-which(grpUniq %in% grpUniq[i])]))
      
    }
    
    refGrp <- "refGrp"  %in% input$comp
    grpComb <- "grpComb"  %in% input$comp
    if (refGrp){
      updateSelectizeInput(session,'grp4comp', choices=tmpGrp, selected=tmpGrp)
    } else if (grpComb){
      updateSelectizeInput(session,'grp4comp', choices=grpSel, selected=grpSel[1:2])
    }
      
      
      
  
    save(met,file2name, file="met.RData")
    })
    
    #source("runDiffAnalysis.R")
      observeEvent(input$go4, {
      
      load("met.RData")
      
      grp4comp=print(input$grp4comp)
      
      outputDir<-print({parseDirPath(volumes, input$directory4_1)})
      #cwd<-getwd()
      setwd(outputDir)
      dir.create("runDiffAnalysisRes")
      setwd("runDiffAnalysisRes")
      runDiffAnalysis(met, grp4comp, file2name)
      setwd(outputDir)
      #setwd(cwd)
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
        metMZ<-read.table(inFile3$datapath, header=input$header, sep=input$sep2)
        tmpgroups = metMZ$Group
        updateSelectizeInput(session,'grp', choices=tmpgroups, selected=tmpgroups)
        save(metMZ, file="metMZ.RData")
        })
        
        observeEvent(input$go5, {
        load("metMZ.RData")
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
        #cwd<-getwd()
        setwd(outputDir)
        dir.create("runCMIanalysisRes")
        setwd("runCMIanalysisRes")
        runCMI(GrpMetMZ, irlba = FALSE, graphML = graphML, stats = stats, prefix = "CMI_stats")
        setwd(outputDir)
        #setwd(cwd5)
        print("Job Done...")
        
        })
        
        
        #source("runViz.R")
        observeEvent(input$go6, {
          
          
          inFile4 <- input$file4
          if (is.null(inFile4))
            return(NULL)
          met.log<-read.table(inFile4$datapath, header=input$header, sep=input$sep3)
          file4name <- print(gsub(".txt|.csv", "",inFile4$name))
          
          heatmap <- "heatmap" %in% input$var7
          lvl <- "lvl"  %in% input$var7
          rla <- "rla"  %in% input$var7
          pca <- "pca"  %in% input$var7
          if (is.null(input$var7) ){
            plots <- "NULL"
          } else if (heatmap & pca & lvl & rla){
            plots <- "heatmap pca lvl rla"
          } else if (heatmap & pca & lvl){
            plots <- "heatmap pca lvl"
          } else if (heatmap & pca & rla){
            plots <- "heatmap pca rla"
          } else if (heatmap & lvl & rla){
            plots <- "heatmap lvl rla"
          } else if (pca & lvl & rla){
            plots <- "pca lvl rla"
          } else if (heatmap & pca){
            plots <- "heatmap pca"
          } else if (heatmap & lvl){
            plots <- "heatmap lvl"
          } else if (heatmap & rla){
            plots <- "heatmap rla"
          } else if (pca & lvl){
            plots <- "pca lvl"
          } else if (pca & rla){
            plots <- "pca rla"
          } else if (lvl & rla){
            plots <- "lvl rla"
          } else if (heatmap){
            plots <- "heatmap"
          } else if (pca){
            plots <- "pca"
          } else if (lvl){
            plots <- "lvl"
          } else if (rla){
            plots <- "rla"
          } 
          
          outputDir<-print({parseDirPath(volumes, input$directory4_3)})
          #cwd<-getwd()
          setwd(outputDir)
          dir.create("runVizRes")
          setwd("runVizRes")
          runViz(met.log, plots, file4name)
          setwd(outputDir)
          #setwd(cwd)
          print("Job Done...")
          
        })
        
    
     }
  
    shinyApp(ui, server)
    
  }
  
