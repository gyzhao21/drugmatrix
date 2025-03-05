library(shiny)
library(shinydashboard)
library(shinyWidgets)
library(dplyr)
library(plotly)
library(enrichR)

# Define UI for application that draws a histogram
shinyUI(fluidPage(
  
  # use a gradient in background
  setBackgroundColor("ghostwhite"),
  
  setBackgroundImage(
    src = "stockbg.jpg"
  ),
   
  # box(#title = "DrugMatrix XApicalXOmics",
  #   status = "primary",
  #   solidHeader = T,
  #   collapsible = F,
  #   width = 12,
  #   fluidRow(
  #     #  column(width = 100%, textOutput( "DrugMatrix" ), align = "left"),
  #     column(width = 12, align = "right",
  #            img(src="favicon.png", height = 148, width= 1920)))),
  
    titlePanel(windowTitle = "DrugMatrix ApicalXOmics",
               title =
                 div(
                   box(
                     status = "primary",
                     solidHeader = T,
                     collapsible = F,
                     width = 12,
                     p(),
                     p(),
                     fluidRow(
                       column(width = 12, align = "left",
                              img(src="favicon.png", height = 148, width= 1920)))),
                   fluidRow(
                     box(width = "auto", height = "2px", background= #1A5276, title = "DrugMatrix ApicalXOmics", status = "primary", solidHeader = TRUE
                     )
                   ),
                 
                   p(),
                   
                   " DrugMatrix ApicalXOmics"
                 )
               ),

  
  tags$style(HTML("
.box.box-solid.box-primary>.box-header {
  color:#1A5276;
  background:#1A5276}

  .box.box-solid.box-primary{
    border-bottom-color:#1A5276;
      border-left-color:#1A5276;
      border-right-color:#1A5276;
      border-top-color:#1A5276;
      background-color: #020031;  # banner expansion color is r:2 g:0 b:49.  i.e.  #020031
      max-width:auto;
      margin:5px 5px  5px -15px;
  }
")),
  
  fluidRow(
    box(width = "auto", height = "5px", background= #1A5276, title = "DrugMatrix ApicalXOmics", status = "primary", solidHeader = TRUE
    )
  ),


  
  tags$style(type="text/css",
                                    ".shiny-output-error { visibility: hidden; }",
                                    ".shiny-output-error:before { visibility: hidden; }",
                                    
                                    ## The following line is for tab menu fonts
                                    "li a {
                                    font-family: Lucida, Arial;
                                    font-size: 15px;
                                    font-weight: bold;
                                    border: 1px solid #1A5276;
                                    border-radius: 50px 20px;
                                    }"

                         ),
    
    tags$style(HTML('
         .tabs-above >.nav > li[class=active] > a {
           font-family:"Lucida Sans", sans-serif;
           font-size: 20px;
           font-style: bold;
           background-color: #000;
           color:#FFF
        }')),

  tabsetPanel(type = "pills",
              tabPanel("Project Description",
                       p(),
                       p(),
                       h3("DrugMatrix Database"),
                       div("The DrugMatrix database is an integrated toxicogenomic that contains the short-term rat toxicity study results from approximately 
                           600 pharmaceuticals and environmental chemicals performed. The ", 
                           a(href="https://cebs.niehs.nih.gov/cebs/paper/15670", "complete DrugMatrix database ", target="_blank"),
                           "(plain text link: https://cebs.niehs.nih.gov/cebs/paper/15670) is available ",
                           a(href="https://cebs.niehs.nih.gov/cebs/", " on NIEHS CEBS site", target="_blank"),
                           ".", style = "color:black"), 
                    
                       img(src = "drugmatrix.jpg", height = 269, width = 475),
                       
                       p(),
                       div("Briefly:", style = "color:black"),
                       
                       span ("~", em("700", style = "color:brown"), "Short-term toxicity studies (0.25 to 5 days) in male SD rats"), br(),
                       span ("~", em("637", style = "color:brown"), "compounds studied at multiple doses, time points and tissues"), br(),
                       span ("~", em("4,000", style = "color:brown"), "dose-time-tissue combinations (biological triplicates)"), br(),
                       span ("~", em("13,000", style = "color:brown"), "Codelink RU1 Microarrays"), br(),
                       span ("~", em("5,000", style = "color:brown"), "Affymetrix RG230-2 Arrays"), br(),
                       span ("~", em("127,000", style = "color:brown"), "histopathology measurements"), br(),
                       span ("~", em("100,000", style = "color:brown"), "hematology and chemistry measurements"), br(),
                       span ("~", em("130", style = "color:brown"), "different in vitro assays"), br(),
                       span ("~", em("900", style = "color:brown"), "chemicals with detailed literature curation"), br(),
                       
                       hr(),
                       
                       h3("Search Strategy"),
                       
                       div("A combination of endpoints were accessed in each study including target organ histopathology, clinical chemistry and target organ toxicogenomics. 
                             This design allows for derivation of relationships between the different endpoint (e.g. identification of transcriptional biomarkers of pathology).", style = "color:black"),
                       p(),
                       div("To this end we have created a Shiny web application on top of the DrugMatrix database that allows users to", style = "color:black"),
                       tags$ul(
                         tags$li("query a gene and identify its relationship to all diagnosed pathologies, clinical pathologies, and experimental animal organ weight changes due to the chemial compound 
                               treatment"), 
                         tags$li("query a specific pathology, clinical pathology, organ weight change to identify the most strongly associated genes."),
                         style = "color:black"),
                       p(),
                       div("Users can refine the search by selecting the criteria of duration of exposure, organ/tissue source of gene expression, gene probe, histopathology, ", em("etc.,"), "on a microarray 
                         platform (Codelink and/or Affymetrix) and then click the",  strong(span("SUBMIT", style="font-family: Courier New, Courier, Lucida Sans Typewriter")), "button.  Results can be visualized graphically and are downloadable in multiple formats.", style = "color:black"),
                       
                       hr(),
                       
                       h3("Quick Guide to Perform a Search"),
                       div("In addition to the current ", strong("Project Description "), "page, we have six other tabs:", style = "color:black"), 
                       tags$ul(
                         tags$li(strong("Genes to Pathology"), style="color:black"),
                         tags$ul(
                           tags$li("If you are interested in knowing which histopathology is most significantly affected in a specific tissue organ, you choose a tissue and an array chip (Coldlink RU1 or Affymetrix RG230) with an exposure time (5 days)."),
                           tags$li("Within about a minute or so, you will be able to see the data retrieved from the DrugMatrix database. The significance is measured by looking at the 3 columns in the Summary table displayed on the main panel - gene expression level average Log10 Ratio DIFF, T-value, and P-value (see explanation below)."),
                           tags$li("Once you see the top row displayed in the Summary table, you may click on the pathology name, e.g. TUBULE, REGENERATION. You will see all the experiments associated with the choice in detail shown in the table under the Summary table. In the meantime, a box plot with overlaying scatter plot shows at the bottom of the main panel."),
                           tags$li("To generate a report, you may choose PDF, csv, or Excel file format as shown in the upper corner of the table of your interest.")
                         ),
                         tags$li(strong("Pathology to Genes"), style="color:black"),
                         tags$ul(
                           tags$li("If you are interested in knowing which gene is most significantly involved or associated in a specific tissue organ, you choose a tissue and an array chip with an exposure time (5 days). Note: Since there are ~ 34K gene probes needed to scan through, the query takes much longer up to ~4 minutes."),
                           tags$li("Similarly, the significance is measured by looking at the 3 columns in the Summary table displayed on the main panel, i.e. Log10 Ratio DIFF, T-value, and P-value."),
                           tags$li("Once you see the top row displayed in the Summary table, you may click on the gene probe name, e.g. Havcr1. You will see all the experiments associated with the choice in detail shown in the table under the Summary table. In the meantime, a list of associated pathological images, if there are any, with various severities (normal, minimal, mild, moderate, and marked) show on top of the main panel. A box plot with overlaying scatter plot shows on the bottom of the page. You may download the tabular data report the same way as described above.")
                         ),
                         tags$li(strong("Genes to Clinical Pathology"), style="color:black"),
                         tags$ul(
                           tags$li("You select a gene of interest to see which clinical assay is strongly expressed in an organ/tissue selected.  Assay effect was determined by comparing the averaged assay value to the range of normal.  Namely, if the average value falls in between the range, it is considered to be normal.  Hence, higher than the upper bound of the range, it is increase.  Lower than the lower bound, it is decrease.")
                         ),
                         tags$li(strong("Clinical Pathology to Genes"), style="color:black"),
                         tags$ul(
                           tags$li("You select a clinical assay and an assay effect, i.e. (increase or decrease ) as described above to see which gene expression is more significantly associated.")
                         ),
                         tags$li(strong("Genes to Organ Weight Change"), style="color:black"),
                         tags$ul(
                           tags$li("You select a gene of interest with the direction of organ/tissue weight change to see which expression source, e.g. LIVER, is highly associated with organ weight change.")
                         ),  
                         tags$li(strong("Organ Weight Change to Genes"), style="color:black"),
                         tags$ul(
                           tags$li("You choose a direction of organ/tissue weight change along with the gene expression tissue source, to find out which gene is mosted involved in the association with the weight change. It is noted that the cut-off criteria about organ/tissue weight change is decided by 10% organ weight increase or decrease relative to the mean of whole body weight.")
                         ),
                       ),
                       
                       div("Analysis applied with gene association:", style="color:black"),
                       tags$ul(
                         tags$li(strong("DIFF") ," - the Log10 ratio difference between severity score at zeros (control group) and the greater than zeros. Click the column header to sort."),
                         tags$li(strong("T-value"), " - value generated from paired observations between treatment group of average Log10 Ratio and non-treatment group of average Log10 Ratio."),
                         tags$li(strong("P-value"), " - calculated with Mann-Whitney method for the treatment group severity scores greater than zeros, range of normal, and level of gene expression.  
                                  Values \u2265 0.05 are highlighted in", strong("pink.")), 
                         style="list-style: none; color:black; font-family: Courier New;"
                       ),
                       p(),
                       div("In the interface, boxplots, together with dotted plots are used to visualize the distribution quickly to examine for outliers.  You can compare the multiple groups on the same variable or multiple variables on the same scale.  
                           To illustrate this, here is an example. If you look at the histopathology TUBULE REGENERATION in Genes to Pathology tab, the boxplots are created to compare the chemical compound treatments with zero severity scores versus the 
                           the treatments with severity score \u2265 0s.  Each boxplot represents the expression levels (i.e. log10 ratio) across the severities.  A dotted plot overlapped on the boxplot shows that each dot represents 
                           an individual experimental treatment at a concentration within a specific time exposure (e.g. 5 days). ", style = "color:black"),
                       
                       p(),
                       
                       img(src = "boxplot-illustration.jpg", height = "419"),
                       hr(),
                       
                       h3("Exemplary Search Output"),
                       div("Take an example output from ", strong ("Pathology to Genes"), " tab. It shows the gene experssion levels denoted by average log10 ratios versus various severity scores when choosing Abcc3 gene probe in Liver.  
                             Above the Summary table, you see a list of pathological images assocaited with the liver.", style = "color:black"), 
                       p(),
                       img(src="mild-liver-pathology.png", height = "334", width = "729"),
                       p(),
                       div("Click on the histopathology name on the row with severity, you get the pathological image shown as above. ", style = "color:black"), 
                       p(),
                       img(src = "samplepic.png", height = "419", width = "909"),
                       p(),
                       p(),
                       div("You may download a tabular output data in the format of some choices such as PDF, csv, or excel,", em("etc."), style = "color:black"), 
                       p(),
                       img(src = "tabulardata.png", height = "300", width = "687"),
                       
                       hr(),
                       
              ),
    
    tabPanel("Genes to Pathology",

                         
                         # The following serves to suppress the error messages to display using tags$style

                         tags$style(type="text/css",
                                    ".shiny-output-error { visibility: hidden; }",
                                    ".shiny-output-error:before { visibility: hidden; }",
                                    
                                    ## The following line is for tab menu fonts
                                    "li a {
                                    font-family: Lucida, Arial;
                                    font-size: 15px;
                                    font-weight: bold;
                                    border: 1px solid #1A5276;
                                    border-radius: 50px 20px;
                                    }"

                         ),
             tags$style(HTML('
         .tabs-above >.nav > li[class=active] > a {
           font-family: Arial, "Lucida Sans", sans-serif;
           font-size: 20px;
           font-style: bold;
           background-color: #000;
           color:#FFF
        }')),

                        tags$head(
                           tags$style(".shiny-notification { position: fixed; top: 30% ;left: 30%; color: #922B21; size: 90%}",
                                      "p {color:red;}",
                                      "body {color:#1A5276;}",
                                      "h2 {color:#1A5276;}",
                                      "label, body, input, select, button {font-family: 'Arial';}",
                                      "#sidebar {background-color: #ededed;}"
                           )
                         ),
             
                         tags$head(tags$style(HTML('#sidebar { background-color: #ededed; } 

                                                   body, label, input, button, select { font-family: "Arial";}')


                                              )#,
                                            # (
                                            #   ".shiny-notification { position: fixed; top: 30% ;left: 30%; color: #922B21; size: 90%}"
                                            # )
                                   ),


                        
                         
                         sidebarLayout(
                                         
                           sidebarPanel(id = "sidebar", #width=3, 
                                        div("After selecting the criteria, click the", strong(span("SUBMIT", style="font-family: Courier New, Courier, Lucida Sans Typewriter")), "button below.", style = "color:brown"),
                                        p(),

                                        radioButtons(inputId="in1", label = "Expression Platform:", 
                                                     c("Codelink RU1" = "RU1",
                                                       "Affymetrix RG230" = "RG230-2",
                                                       "BioSpyder S1500+" = "S1500+",
                                                       "Sciome Genie" = "GENIE"
                                                       
                                                     ),
                                                     selected = "RU1"),
                                        
                                        
                                       # radioButtons(inputId="in1", label = "Expression Platform:", choices = unique(dat$chip), selected = 'RU1', inline = TRUE),
                                        selectInput(inputId = "in2", label = "Choose a Tissue (Gene Expression and Pathology Source)", choices = NULL, multiple = FALSE),
                                        selectInput(inputId = "in3", label = "Duration of Exposure (Day)", choices = NULL, multiple = TRUE),
                                        selectInput(inputId = "in5", label = "Choose a Gene", selected ='Havcr1 | hepatitis A virus cellular receptor 1 (AF035963_PROBE1,1387965_at,286934)', choices = NULL, multiple = FALSE),
                                        #br(),
                                        radioButtons("selectSensivity", label = "Comparison (Tissue High Specificity versus High Sensitivity)", choices = c('Specificity', 'Sensitivity')),
                                       selectInput("selectPlot", label = "Histopathology Severity Grouping:", width="500px",
                                                   choices = c('All Severities','0 or >0 Severities')),

                                        # show the action button
                                       actionButton('bu1', 'Submit',  icon =icon("play-circle"), style = "color:#000000; background-color:  #afd4f5; margin-left: 60%", class = "btn-primary btn-md"),
                                        # conditionalPanel(
                                        #   condition = "input.in5.length > 0",
                                        #   actionButton(inputId = "bu1", label = "Submit", icon =icon("play-circle"),  style = "color:#000000; background-color:  #afd4f5; margin-left: 60%", class = "btn-primary btn-md"),
                                        # ),
                                        # 
                                        width = 3 
                                        
                             ),

                             

                             # Show a plot of the generated distribution

                             mainPanel(
                                 br(),
                                 DT::dataTableOutput("genePathImagetable"),
                                 br(),
                                 fluidRow(
                                   column(4, uiOutput("img2"))  ## The size of the images
                                 ),
                                 br(),
                                 textOutput('countNum'),
                                 br(),
                                 DT::dataTableOutput("summaryTable"),
                                 br(),
                                 textOutput('summarySelect'),
                                 br(),
                                 DT::dataTableOutput("pathologyTable"),
                                 hr(),
                                 plotOutput("boxPlot"),
                                 downloadButton("boxplot_download", "Image Download")
                                 
                             ))
                         ), # "end of the Genes to Pathology tab
    
    
    tabPanel("Pathology to Genes", 
             # The following serves to suppress the error messages to display using tags$style
             tags$style(type="text/css",
                        ".shiny-output-error { visibility: hidden; }",
                        ".shiny-output-error:before { visibility: hidden; }"
             ),
             
             tags$head(
               tags$style(".shiny-notification { position: fixed; top: 30% ;left: 30%; color: #922B21; size: 90%}",
                          "p {color:red;}",
                          "body {color:#1A5276;}",
                          "h2 {color:#1A5276;}"
               )
             ),
             
             tags$head(tags$style(
               HTML('
                                    #sidebar {
                                       background-color: #ededed;
                                            }

                                       body, label, input, button, select { 
                                      font-family: "Arial";
                                   }')
             )),
             
             sidebarLayout(
               sidebarPanel(id = 'sidebar', #width=3,
                            div("After selecting the criteria, click the", strong(span("SUBMIT", style="font-family: Courier New, Courier, Lucida Sans Typewriter")), "button below.", style = "color:brown"),
                            p(),
                            
                            radioButtons(inputId="tab2in1", label = "Expression Platform:", 
                                         c("Codelink RU1" = "RU1",
                                           "Affymetrix RG230" = "RG230-2",
                                           "BioSpyder S1500+" = "S1500+",
                                           "Sciome Genie" = "GENIE"
                                           
                                         ),
                                         selected = "RG230-2"),
                            
                            #radioButtons(inputId="tab2in1", label = "Expression Platform:", choices = unique(dat$chip), selected = 'RU1', inline = TRUE),
                            
                            selectInput(inputId = "tab2in2", label = "Choose a Tissue (Gene Expression and Pathology Source)", choices = NULL, multiple = FALSE),
                            selectInput(inputId = "tab2in3", label = "Duration of Exposure (Day)", choices = NULL, multiple = TRUE),
                            selectInput(inputId = "tab2in4", label = "Histopathology", choices = NULL, multiple = FALSE),
                            radioButtons("selectSpec", label = "Comparison (Tissue High Specificity versus High Sensitivity)", choices = c('Specificity', 'Sensitivity')),
                            #selectInput(inputId = "tab2in5", label = "Choose a Gene", selected = 'Havcr1 | hepatitis A virus cellular receptor 1 (AF035963_PROBE1,1387965_at,286934)', choices = NULL, multiple = FALSE),
                            br(),
                            selectInput("selectPlotPath", label = "Histopathology Severity Grouping:", width="500px",
                                        choices = c('All Severities','0 or >0 Severities')),
                            
                            # show the action button
                            
                            actionButton('bu2', 'Submit',  icon =icon("play-circle"), style = "color:#000000; background-color:  #afd4f5; margin-left: 60%", class = "btn-primary btn-md"),
                            # conditionalPanel(
                            #   condition = "input.tabin4.length > 0",
                            #   actionButton(inputId = "bu2", label = "Submit", icon =icon("play-circle"),  style = "color:#000000; background-color:  #afd4f5; margin-left: 60%", class = "btn-primary btn-md"),
                            # ),
                            
                            width = 3
                            
               ),
               
               # Show a plot of the generated distribution
               mainPanel(
                 ################### output the selected pathology image data if there is ###################
                 br(),
                 dataTableOutput("imagetable"),
                 fluidRow(
                   column(4, uiOutput("path2geneimg"))  ## The size of the images
                 ),
                 br(),
                 textOutput('pcountNum'),
                 br(),
                 textOutput('displayEntrezid'),
                 br(),
                 DT::dataTableOutput("geneSummaryTable"),
                 br(),
                 textOutput('geneSummarySelect'),
                 br(),
                 DT::dataTableOutput("geneTable"),
                 hr(),
                 plotOutput("boxPlotPath"),
                 # textInput('filename', "Filename"),
                 # checkboxInput('pathologyPlot', "Check to save")
                 downloadButton("pathlogyPlot_download", "Image Download")
               ))# end of sidebarLayout
    ), # "end of the Pathology to Genes" tab


        
    tabPanel("Genes to Clinical Pathology",
             # The following serves to suppress the error messages to display using tags$style
             
             tags$style(type="text/css",
                        ".shiny-output-error { visibility: hidden; }",
                        ".shiny-output-error:before { visibility: hidden; }"
                        
             ),
             

             tags$head(
               tags$style(".shiny-notification { position: fixed; top: 30% ;left: 30%; color: #922B21; size: 90%}",
                          "p {color:red;}",
                          "body {color:#1A5276;}",
                          "h2 {color:#1A5276;}"
               )
             ),
             
             tags$head(tags$style(
               HTML('
                                    #sidebar {
                                       background-color: #ededed;
                                            }

                                       body, label, input, button, select { 
                                      font-family: "Arial";
                                   }')
             )),
             
             
             sidebarLayout(
               sidebarPanel(id = 'sidebar', width=3,
                            
                            div("After selecting the criteria, click the", strong(span("SUBMIT", style="font-family: Courier New, Courier, Lucida Sans Typewriter")), "button below.", style = "color:brown"),
                            p(),
                            
                            radioButtons(inputId="cpin1", label = "Expression Platform:", 
                                         c("Codelink RU1" = "RU1",
                                           "Affymetrix RG230" = "RG230-2",
                                           "BioSpyder S1500+" = "S1500+",
                                           "Sciome Genie" = "GENIE"
                                           
                                         ),
                                         selected = "RG230-2"),

                            # radioButtons(inputId = "cpin1", label = "Expression Platform:", choices = unique(cpdat$chip_name), selected = 'RG230-2', inline = TRUE),
                            selectInput(inputId = "cpin2", label = "Choose an Organ/Tissue Expression Source:", choices = unique(cpdat$tissue), multiple = FALSE, selected='LIVER'),
                            selectInput(inputId = "cpin3", label = "Choose a Clinical Type", choices = NULL, multiple = FALSE),
                            selectInput(inputId = "cpin4", label = "Choose Assay Effect:", choices = NULL, multiple = FALSE),
                            #radioButtons(inputId = "cpin4", label = "Choose Assay Effect:", selected = 'increase', choices = unique(cpdat$category), inline = TRUE),
                            selectInput(inputId = "cpin5", label = "Choose a Gene", choices = NULL, multiple = FALSE, selected = 'Abcc3 | ATP binding cassette subfamily C member 3 (140668,ABCC3_7941,AB010467_PROBE1,1369698_at,AB010467_s_at)'),
                            selectInput(inputId = "cpin6", label = "Duration of Exposure (Day)", choices = NULL, multiple = TRUE),
                            
                            actionButton('bu3', 'Submit', icon =icon("play-circle"), style = "color:#000000; background-color:  #afd4f5; margin-left: 60%", class = "btn-primary btn-md"),
               ),
               
               
               
               # Show a plot of the generated distribution
               
               mainPanel(
                 br(),
                 textOutput('cpgCountNum'),
                 br(),
                 DT::dataTableOutput("genetoCPSummaryTable"),
                 br(),
                 textOutput('assaySummarySelect'),
                 br(),
                 DT::dataTableOutput("clin_pathologyTable"),
                 hr(),
                 plotOutput("boxPlotClinGenetoPath"),
                 downloadButton("cg2pathlogyPlot_download", "Image Download")
               ))
    ), # "end of the Genes to Clinical Pathology" tab
    
    
    tabPanel("Clinical Pathology to Genes", 
             # The following serves to suppress the error messages to display using tags$style
             
             tags$style(type="text/css",
                        ".shiny-output-error { visibility: hidden; }",
                        ".shiny-output-error:before { visibility: hidden; }"
                        
             ),
             
             tags$head(
               tags$style(".shiny-notification { position: fixed; top: 30% ;left: 30%; color: #922B21; size: 90%}",
                          "p {color:red;}",
                          "body {color:#1A5276;}",
                          "h2 {color:#1A5276;}"
               )
             ),

             tags$head(tags$style(
               HTML('
                                    #sidebar {
                                       background-color: #ededed;
                                            }

                                       body, label, input, button, select { 
                                      font-family: "Arial";
                                   }')
             )),
             
             
             sidebarLayout(
               sidebarPanel(id = 'sidebar', width=3,
                            
                            div("After selecting the criteria, click the", strong(span("SUBMIT", style="font-family: Courier New, Courier, Lucida Sans Typewriter")), "button below.", style = "color:brown"),
                            p(),
                            
                            radioButtons(inputId="pcin1", label = "Expression Platform:", 
                                         c("Codelink RU1" = "RU1",
                                           "Affymetrix RG230" = "RG230-2",
                                           "BioSpyder S1500+" = "S1500+",
                                           "Sciome Genie" = "GENIE"
                                           
                                         ),
                                         selected = "RG230-2"),
                            
                            # radioButtons(inputId = "pcin1", label = "Expression Platform:", choices = unique(cpdat$chip_name), selected = 'RG230-2', inline = TRUE),
                            selectInput(inputId = "pcin2", label = "Choose an Organ/Tissue Expression Source:", choices = unique(cpdat$tissue), multiple = FALSE, selected='LIVER'),
                            selectInput(inputId = "pcin3", label = "Choose a Clinical Type", choices = NULL, multiple = FALSE),
                            selectInput(inputId = "pcin7", label = "Clinical Assay:", choices = NULL, multiple = FALSE),
                            selectInput(inputId = "pcin4", label = "Choose Assay Effect:", choices = NULL, multiple = FALSE),
                            selectInput(inputId = "pcin6", label = "Duration of Exposure (Day)", choices = NULL, multiple = TRUE),
                            #pickerInput("pcin6","Time", choices=NULL, options = list(`actions-box` = TRUE),multiple = T),
                            
                            actionButton('bu4', 'Submit',  icon =icon("play-circle"), style = "color:#000000; background-color:  #afd4f5; margin-left: 60%", class = "btn-primary btn-md"),
               ),
               
               
               
               # Show a plot of the generated distribution
               
               mainPanel(
                 #br(),
                 textOutput('c2gCountNum'),
                 br(),                                 
                 DT::dataTableOutput("pathology2geneSummaryTable"),
                 br(),
                 textOutput('clinGeneSummarySelect'),
                 br(),
                 hr(),
                 DT::dataTableOutput("c2gdetailTable"),
                 hr(),
                 plotOutput("boxPlotClinPathtoGene"),
                 downloadButton("cp2genePlot_download", "Image Download"),
                 br()
               ))          
    ), # End of the Clinical Pathlogy to Genes tab
    
    tabPanel("Genes to Organ Weight Change", 
             # The following serves to suppress the error messages to display using tags$style
             
             tags$style(type="text/css",
                        ".shiny-output-error { visibility: hidden; }",
                        ".shiny-output-error:before { visibility: hidden; }"
                        
             ),
             
             tags$head(
               tags$style(".shiny-notification { position: fixed; top: 30% ;left: 30%; color: #922B21; size: 90%}",
                          "p {color:red;}",
                          "body {color:#1A5276;}",
                          "h2 {color:#1A5276;}"
               )
             ),
             
             tags$head(tags$style(
               HTML('
                                    #sidebar {
                                       background-color: #ededed;
                                            }

                                       body, label, input, button, select { 
                                      font-family: "Arial";
                                   }')
             )),
             
             
             sidebarLayout(
               sidebarPanel(id = 'sidebar', width=3,
                            
                            div("After selecting the criteria, click the", strong(span("SUBMIT", style="font-family: Courier New, Courier, Lucida Sans Typewriter")), "button below.", style = "color:brown"),
                            p(),
                            
                            radioButtons(inputId="gwin1", label = "Expression Platform:", 
                                         c("Codelink RU1" = "RU1",
                                           "Affymetrix RG230" = "RG230-2",
                                           "BioSpyder S1500+" = "S1500+",
                                           "Sciome Genie" = "GENIE"
                                           
                                         ),
                                         selected = "RU1"),
                            
                            # radioButtons(inputId ="gwin1", label = "Expression Platform:", choices = unique(gwdat$chip_name), selected = 'RU1', inline = TRUE),
                            selectInput(inputId = "gwin5", label = "Choose a Gene", choices = NULL, multiple = FALSE, selected = 'Havcr1 | hepatitis A virus cellular receptor 1 (AF035963_PROBE1,1387965_at,286934)'),
                            selectInput(inputId = "gwin2", label = "Choose an Organ Weight:", choices = NULL, multiple = FALSE, selected='LIVER'),
                            selectInput(inputId = "gwin3", label = "Choose Direction of Change", choices = NULL, multiple = FALSE),
                            selectInput(inputId = "gwin4", label = "Duration of Exposure (Day)", choices = NULL, multiple = TRUE),
                            selectInput(inputId = "gwin6", label = "Choose a Gene Expression Source", choices = NULL, multiple = FALSE),
                            
                           actionButton('bu5', 'Submit', icon =icon("play-circle"), style = "color:#000000; background-color:  #afd4f5; margin-left: 60%", class = "btn-primary btn-md"),
                            
               ),
               
               
               
               # Show a plot of the generated distribution
               
               mainPanel(
                 br(),
                 # textOutput('g2wCountNum'),
                 br(),                                 
                 DT::dataTableOutput("g2weightSummaryTable"),
                 br(),
                 hr(),
                 # textOutput('g2wDetailCountNum'),
                 br(),
                 # textOutput('g2wGeneSummarySelect'),
                 # br(),
                 # textOutput('g2wTissueChosen'),
                 # br(),
                 DT::dataTableOutput("g2wdetailTable"),
                 hr(),
                 plotOutput("boxPlotGene2Weight"),
                 downloadButton("gene2weightPlot_download", "Image Download"),
                 br()
               ))          
    ),  # End of Genes to Organ Weight
    
    
    
    
    tabPanel("Organ Weight Change to Genes", 
             # The following serves to suppress the error messages to display using tags$style
             
             tags$style(type="text/css",
                        ".shiny-output-error { visibility: hidden; }",
                        ".shiny-output-error:before { visibility: hidden; }"
                        
             ),
             
             tags$head(
               tags$style(".shiny-notification { position: fixed; top: 30% ;left: 30%; color: #922B21; size: 90%}",
                          "p {color:red;}",
                          "body {color:#1A5276;}",
                          "h2 {color:#1A5276;}"
               )
             ),
             
             tags$head(tags$style(
               HTML('
                                    #sidebar {
                                       background-color: #ededed;
                                            }

                                       body, label, input, button, select { 
                                      font-family: "Arial";
                                   }')
             )),
             
             sidebarLayout(
               sidebarPanel(id = 'sidebar', width=3,
                            div("After selecting the criteria, click the", strong(span("SUBMIT", style="font-family: Courier New, Courier, Lucida Sans Typewriter")), "button below.", style = "color:brown"),
                            p(),
                            
                            radioButtons(inputId="wgin1", label = "Expression Platform:", 
                                         c("Codelink RU1" = "RU1",
                                           "Affymetrix RG230" = "RG230-2",
                                           "BioSpyder S1500+" = "S1500+",
                                           "Sciome Genie" = "GENIE"
                                           
                                         ),
                                         selected = "RU1"),
                            
                            # radioButtons(inputId ="wgin1", label = "Expression Platform:", choices = unique(gwdat$chip_name), selected = 'RU1', inline = TRUE),
                            selectInput(inputId = "wgin2", label = "Choose an Organ Weight:", choices = NULL, multiple = FALSE, selected='LIVER'),
                            selectInput(inputId = "wgin3", label = "Choose Direction of Change", choices = NULL, multiple = FALSE),
                            selectInput(inputId = "wgin4", label = "Duration of Exposure (Day)", choices = NULL, multiple = TRUE),
                            selectInput(inputId = "wgin6", label = "Choose a Gene Expression Source", choices = NULL, multiple = FALSE),
                            
                            actionButton('bu6', 'Submit', icon =icon("play-circle"), style = "color:#000000; background-color: #afd4f5; margin-left: 60%", class = "btn-primary btn-md"),
                            
               ),
               
               
               
               # Show a plot of the generated distribution
               
               mainPanel(
                 br(),
                 textOutput('w2gCountNum'),
                 br(),                                 
                 DT::dataTableOutput("w2geneSummaryTable"),
                 br(),
                 hr(),
                 textOutput('w2gDetailcount'),
                 br(),
                 textOutput('w2gGeneSummarySelect'),
                 br(),
                 DT::dataTableOutput("w2gdetailTable"),
                 hr(),
                 plotOutput("boxPlotWeight2Gene"),
                 downloadButton("w2genePlot_download", "Image Download"),
                 br()
               ))          
    ), # End of Organ Weight to Genes

    tabPanel("Toxicological Profile",
             # The following serves to suppress the error messages to display using tags$style

             tags$style(type="text/css",
                        ".shiny-output-error { visibility: hidden; }",
                        ".shiny-output-error:before { visibility: hidden; }",
                        
                        ".block {
      border-color: #adadad;
      border-style: none;
      background-color: #ededed;
      text-align: left;
      margin-top: 10px;
      margin-bottom: 50px;
      margin-left: 350 px;
      margin-right: 300px;
      min-height: 320px;
      #width: 970px;
    }"

             ),
             
             tags$style(HTML('
                             .box.box-solid.box-primary>.box-header {
                                     color:#fff;
                                     background:#666666
                            }
                             .box.box-solid.box-primary{
                              border-bottom-color:#666666;
                              border-left-color:#666666;
                              border-right-color:#666666;
                              border-top-color:#666666;
                            },
                        ')),
             
             tags$head(
               tags$style(".shiny-notification { position: fixed; top: 30% ;left: 30%; color: #922B21; size: 90%}",
                          "p {color:red;}",
                          "body {color:#1A5276;}",
                          "h2 {color:#1A5276;}"
               )
             ),

             tags$head(tags$style(
               HTML('
                                    #sidebar {
                                       background-color: #ededed;
                                            }

                                       body, label, input, button, select {
                                      font-family: "Arial";
                                   }
                
                    ') # end of HTML content
             )),

             h3("Complete Downloadable Dataset"),
             
             p(),
             
             div("For complete DrugMatrix summary apical endpoints treatment group level data, click on the ", 
                 a(href="summaryApicalEndpoints.zip", "Complete Toxicological Profile (compressed zip file, ~10 MB)", download=NA, target="_blank"), 
             
                  " link.", style = "color:black"),
             p(),
             
             h3("Tabular Apical Endpoints Treatment Group Level Data"),
             p(),
             
              fluidRow(
                div(
                  class = "block",
            #      "Block 1",
                
   
               column(4,
                      selectInput("chemical",
                                  "chemical:",
                                  c("All",
                                    unique(as.character(dfprofile$chemical))))
               ),
               column(4,
                      selectInput("casrn",
                                  "casrn:",
                                  c("All",
                                    unique(as.character(dfprofile$casrn))))
               ),
               column(4,
                      selectInput("dtxsid",
                                  "dtxsid:",
                                  c("All",
                                    unique(as.character(dfprofile$dtxsid))))
               ),
               column(4,
                      selectInput("duration",
                                  "duration",
                                  c("0.25d", "1d", "3d", "4d", "5d", "6d", "7d", "14d", "28d", "91d"),
                                  selected = "5d")
               ),
               column(4,
                      selectInput("dose",
                                  "dose:",
                                  c("All",
                                    unique(as.character(dfprofile$dose))))
               ),
               column(4,
                      selectInput("assayname",
                                  "assayname:",
                                  c("All",
                                    unique(as.character(dfprofile$assayname))))
               ),
               column(4,
                      selectInput("tissue",
                                  "tissue:",
                                  c("LIVER", "KIDNEY", "INTESTINE", "THIGH MUSCLE", "BLOOD", "SPLEEN", "HEART", "LUNG", "NA"),
                                  selected = "LIVER")
               ),
               column(4,
                      selectInput("type",
                                  "type:",
                                  c("HISTOPATHOLOGY", "BLOOD_CHEM", "HEMATOLOGY", "ORGAN_WEIGHT"),
                                  selected = "HISTOPATHOLOGY")
               ),
               column(4,
                      selectInput("vehicle",
                                  "vehicle:",
                                  c("All",
                                    unique(as.character(dfprofile$vehicle))))
               ),
               column(4,
                      selectInput("route",
                                  "route:",
                                  c("All",
                                    unique(as.character(dfprofile$route))))
               ),
               column(4,
                      selectInput("unit",
                                  "unit:",
                                  c("All",
                                    unique(as.character(dfprofile$unit))))
               ),
               column(4,
                      selectInput("call",
                                  "call:",
                                  c("All",
                                    unique(as.character(dfprofile$call))))
               )
              ) # end of the block
             ),
             DT::dataTableOutput("table")
    ),  # End of Toxicology Profile Tab
    
    tabPanel("Individual Chemical Expression and Enrichment",
             
             
             # The following serves to suppress the error messages to display using tags$style
             
             tags$style(type="text/css",
                        ".shiny-output-error { visibility: hidden; }",
                        ".shiny-output-error:before { visibility: hidden; }"
                        
             ),
             
             tags$head(
               tags$style(".shiny-notification { position: fixed; top: 30% ;left: 30%; color: #922B21; size: 90%}",
                          "p {color:red;}",
                          "body {color:#1A5276;}",
                          "h2 {color:#1A5276;}"
               )
             ),
             
             tags$head(tags$style(
               HTML('
                                    #sidebar {
                                       background-color: #ededed;
                                            }

                                       body, label, input, button, select { 
                                      font-family: "Arial";
                                   }')
             )),
             
             sidebarLayout(
               
               sidebarPanel(id = 'sidebar', width=3,
                            div("After selecting the criteria, click the", strong(span("SUBMIT", style="font-family: Courier New, Courier, Lucida Sans Typewriter")), "button below.", style = "color:brown"),
                            p(),
                            
                            div(
                              form = "id",
                              
                              radioButtons(inputId="cechip", label = "Expression Platform:", 
                                           c("Codelink RU1" = "RU1",
                                             "Affymetrix RG230" = "RG230-2",
                                             "BioSpyder S1500+" = "S1500+",
                                             "Sciome Genie" = "GENIE"
                                             
                                           ),
                                           selected = "RU1"),
                              
                              # radioButtons(inputId = "cechip", label = "Expression Platform:", choices = unique(chem_enrich_dat$chip_name), selected = 'RG230-2', inline = TRUE),
                              selectInput(inputId = "cetissue", label = "Choose an Organ/Tissue Expression Source:", choices = NULL, multiple = FALSE),
                              selectInput(inputId = "cetime", label = "Duration of Exposure (Day):", choices = NULL, multiple = TRUE),
                              selectInput(inputId = "cecompound", label = "Choose a Chemical:", choices = NULL, multiple = FALSE, selected = 'FENOFIBRATE'),
                              selectInput(inputId = "cedose", label = "Dose (mg/kg):", choices = NULL, multiple = FALSE),

                              awesomeRadio("cecategory", "Choose a Gene Expression Direction:",
                                           c("Up-regulated (Log10 ratio \U2265 0.3, p \u2264 0.05) " = 'a.log_ratio >=0.3',
                                             "Down-regulated (Log10 ratio \u2264 - 0.3, p \u2264 0.05) " = 'a.log_ratio <=-0.3',
                                             "Both the above" = 'NOT (a.log_ratio BETWEEN -0.3 AND 0.3)' ),
                                           selected = 'a.log_ratio >=0.3',
                                           checkbox = TRUE
                              ),

                              
                              # checkboxGroupInput("cecategory", "Choose a Gene Expression Direction:",
                              #                    c("Up-regulated (Log10 ratio \U2265 0.3, p \u2264 0.05)" = 'a.log_ratio >=0.3',
                              #                      "Down-regulated (Log10 ratio \u2264 - 0.3, p \u2264 0.05)" = 'a.log_ratio <=-0.3'
                              #                      ),
                              #                    selected = 'a.log_ratio >=0.3'),
                              
                              checkboxGroupInput("enrichrDBS", "Choose an EnrichR Database:",
                                                 choices = dbs,
                                                 selected = "KEGG_2021_Human"),
                              
                              
                              actionButton('bu7', 'Submit', icon =icon("play-circle"), style = "color:#000000; background-color:  #afd4f5; margin-left: 60%", class = "btn-primary btn-md"),
                              
                            ) # end of div
                            
               ),
               
               mainPanel(
                 br(),
                 DT::dataTableOutput("ceTable"),
                 br(),
                 hr(),
                 DT::dataTableOutput("EnrichrTable"),
                 br(),
                 hr(),
                 plotOutput("enrichBoxPlot"),
                 # downloadButton("boxplot_download", "Image Download")
                 p(),
                 br()
               ) # end of main panel
               
             )# end of sidebar layout
    ),  # End of Individual Chemical Expression and Enrichment

    tabPanel("Scientific Citations",
             # The following serves to suppress the error messages to display using tags$style

             tags$style(type="text/css",
                        ".shiny-output-error { visibility: hidden; }",
                        ".shiny-output-error:before { visibility: hidden; }"

             ),
             
             tags$head(
               tags$style(".shiny-notification { position: fixed; top: 30% ;left: 30%; color: #922B21; size: 90%}",
                          "p {color:red;}",
                          "body {color:#1A5276;}",
                          "h2 {color:#1A5276;}"
               )
             ),

             tags$head(tags$style(
               HTML('
                                    #sidebar {
                                       background-color: #ededed;
                                            }

                                       body, label, input, button, select {
                                      font-family: "Arial";
                                   }')
             )),

             p(),
             
             DT::dataTableOutput("citationTable"),
             
             p(),
             br(),
    ) # End of Scientific Citations Tab
    
    
    
    
    

  ) # end of the tabset panel Note: This contains the multiple tabs - Genes to Pathology tab, Pathology to Genes tab 
 )
)
