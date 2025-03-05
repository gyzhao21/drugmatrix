

shinyServer(function(input, output, session) {
  
  timeinput <- function(a) { 
    # b=paste0('in(', paste0(a, collapse=","),")")
    b=paste0('(', paste0(a, collapse=","),")")
    return(b)
  }
  
  websiteLive <- getOption("enrichR.live")
  
  datQuery <- paste0("SELECT * FROM bsb_chip_histopath_duration;")
  dat <- dbGetQuery(pobj, datQuery)
  
  gQuery <- paste0("Select DISTINCT concat(symbol, ' | ', gene_name) gene_name from all_gene_fc;") 
  df_pgQuery <- dbGetQuery(pobj, gQuery)
  gene <- df_pgQuery$gene_name
  
  ## chemEnrichQuery <- paste0("SELECT * FROM bsb_chemical_enricher_menu;")
  chemEnrichQuery <- paste0("SELECT compound_name, time, dose, chip_name, 
                                     CASE
                                        WHEN tissue =5 THEN 'HEART'::text
                                        WHEN tissue =8  THEN 'LIVER'::text
                                        WHEN tissue =7  THEN 'KIDNEY'::text
                                        WHEN tissue =11  THEN 'SPLEEN'::text
                                        WHEN tissue =1  THEN 'BONE MARROW'::text
                                        WHEN tissue =16  THEN 'THIGH MUSCLE'::text
                                        WHEN tissue =3  THEN 'HEPATOCYTE'::text
                                        WHEN tissue =6  THEN 'INTESTINE'::text
                                        WHEN tissue =2 THEN 'BRAIN'::text
                                        ELSE NULL::text
                                     END AS tissue_name
                             FROM bsb_chemical_enricher_menu_vu; ")
  chem_enrich_dat <- dbGetQuery(pobj, chemEnrichQuery)
  
  ###################################  cpdatQuery:   For Genes to Clinical Pathology #####################################################################

  tissuelist2 <- c("1", "2", "5", "6", "8", "7", "11", "16") # This is for Clinical Pathology to Genes tab
  
  ########################################################  gwdatQuery: For Genes to Organ Weight ########################################################
  gwdatQuery <- paste0("SELECT chip_name, organ, status, duration, signature FROM bsb_gene_weight_change_list;")
  gwdat <- dbGetQuery(pobj, gwdatQuery)
  
  
  ################################################# Global Functions to swap tissue number and tissue name ###############################################
  
  timeinput <- function(a) { 
    # b=paste0('in(', paste0(a, collapse=","),")")
    b=paste0('(', paste0(a, collapse=","),")")
    return(b)
  }
  
  ########### The following tissue_num2name() is to convert tissue numbers to tissue names 
  tissue_num2name <- function(num) {
    name = c()
    for (i in 1:length(num)) {
      if (num[i] == 8) {
        name[i] = 'LIVER'
      } else if (num[i]== 7) {
        name[i] = 'KIDNEY'
      } else if (num[i] == 6) {
        name[i] = 'INTESTINE'
      } else if (num[i] == 5) {
        name[i] = 'HEART'
      } else if (num[i] == 1) {
        name[i] = 'BONE MARROW'
      } else if (num[i] == 2) {
        name[i] = 'BRAIN'
      } else if (num[i] == 11) {
        name[i] = 'SPLEEN'
      } else {
        name[i] = 'THIGH MUSCLE'
      }
    }
    return (name)
  } 
  
  ########### The following tissue_name2num() is to convert tissue names to tissue numbers
  tissue_name2num <- function(name) {
    num = c()
    # num = list()
    for(i in 1:length(name)) {
      if (name[i] == 'LIVER') {
        num[i] = 8
      } else if (name[i] == 'KIDNEY') {
        num[i] = 7
      } else if (name[i] == 'INTESTINE') {
        num[i] = 6
      } else if (name[i] == 'HEART') {
        num[i] = 5
      } else if (name[i] == 'BONE MARROW') {
        num[i] = 1
      } else if (name[i] == 'BRAIN') {
        num[i] = 2
      } else if (name[i] == 'SPLEEN') {
        num[i] = 11
      } else {
        num[i] = 16  # 'THIGH MUSCLE' is 16
      }
    }
    return(num)
  }
  
  
  ########################################################################################################################################################
  
  assaylistQuery <-paste0("SELECT assay_name, tablename, stat_tablename, type from bsb_clin_pathology_list;")  
  df_assaylistQuery <-dbGetQuery(pobj, assaylistQuery)
  
  assaylist<- df_assaylistQuery$assay_name       ### Retrieve assay_name from table bsb_clin_pathology_list and store as a vector
  tablelist<- df_assaylistQuery$stat_tablename   ### Retrieve stat_tablename from table bsb_clin_pathology_list and store as a vector
  detailtablelist <- df_assaylistQuery$tablename ### Retrieve tablename with detail information including experiment bsb_clin_pathology_list and store as a vector
  typelist <- c("BLOOD_CHEM", "HEMATOLOGY")
  
  # doselistQuery <- paste0("SELECT distinct dose FROM all_transcript_vu_fc order by 1;")
  # df_doselist <-dbGetQuery(pool, doselistQuery)
  # doselist <- df_doselist$dose
  
  #################################################  End of Defining Variables for Clinical Pathology to Genes ###################################################
  
  
  dfharmony <- read_excel("harmonizome.xlsx")
  
  # the following hlink() tends todata:get the dfharmony dataframe imported from Excel as input
  # x inside the hlink() is the input from a pathology associated entrezid.  It could be the gene probe you selected.
  
  hlink <- function (dfharmony, x) {
    # initialize harlink as a vector to catch the web link addresses
    harlink <- vector (mode = "character", length = length(x))
    
    for (n in 1:length(x)) {
      dfharmony %>% filter(entrezid %in% x[n]) -> harmonizelink
      #cat("\n******** harmonizelink\n")
      #print(harmonizelink)
      mylink <- harmonizelink$harmanizome
      #cat("\n******** mylink\n")
      #print(mylink)
      hplink <- vector (mode = "character", length = length(mylink))
      
      if(length(mylink)<1){
        hplink[0] <- c( paste0 (paste0('<a href=', paste0(paste0('"', mylink[0]), '"')),  paste0(', target="_blank">', paste0(" No Harmonize", "</a>"))                    ))
      }else if (length(mylink)==1) {
        hplink[1] <- c( paste0 (paste0('<a href=', paste0(paste0('"', mylink[1]), '"')),  paste0(', target="_blank">', paste0(" Harmonize", "</a>"))                    )  )
      }else{
        for (i in 1:length(mylink)) {
          hplink[i] <- c( paste0 (paste0('<a href=', paste0(paste0('"', mylink[i]), '"')),  paste0(', target="_blank">', paste0(paste0(" Harmonize", i), "</a>"))                    )  )
           }
      }
      hl <- paste(hplink, collapse=', ' )
      harlink[n] <- hl
    }
    #cat("\n******** harlink\n")
    harlink
  }
  
  # hlink2() is similar to the hlink() except that it only returns the Human gene symbols
  hlink2 <- function (dfharmony, x) {
    # initialize harlink as a vector to catch the web link addresses
    harlink <- vector (mode = "character", length = length(x))
    
    for (n in 1:length(x)) {
      dfharmony %>% filter(entrezid %in% x[n]) -> harmonizelink
      #cat("\n******** harmonizelink\n")
      #print(harmonizelink)
      mylink <- harmonizelink$harmanizome
      #cat("\n******** mylink\n")
      #print(mylink)
      hplink <- vector (mode = "character", length = length(mylink))
      
      if(length(mylink)<1){
        hplink[0] <- substring(mylink[0], instr (mylink[0], 'gene')+5)
      }else if (length(mylink)==1) {
        hplink[1] <- substring(mylink[1], instr (mylink[1], 'gene')+5)
      }else{
        for (i in 1:length(mylink)) {
          hplink[i] <- substring(mylink[i], instr (mylink[i], 'gene')+5)
        }
      }
      hl <- paste(hplink, collapse=', ' )
      harlink[n] <- hl
    }
    #cat("\n******** harlink\n")
    harlink
  }
  
  
  ###########################################################################################################################
  ################################### The Code for Genes to Pathology Tab ###################################################
  ###########################################################################################################################
  cpdatQuery <- paste0("SELECT assay_name, category, time, tissue, tissue_num, chip_name, type FROM bsb_clinical_assay_list;")
  cpdat <- dbGetQuery(pobj, cpdatQuery)
  
  
  gwdatQuery <- paste0("SELECT chip_name, organ, status, duration, signature FROM bsb_gene_weight_change_list;")
  gwdat <- dbGetQuery(pobj, gwdatQuery)
  
# choose chip name from in1
f_in1 <- reactive({
  filter(dat, chip %in% input$in1)
}) # filter out the "chip" information.  the in1 is for chip choices.  f_in1 is the data frame when you decide to choose a chip.

observeEvent(f_in1(), {
             freezeReactiveValue(input, "in2")
  print("######### Print f_in1()")
  print(f_in1())
             choices<-unique(f_in1()$tissue_name)
             updateSelectInput(inputId = "in2", choices = choices)
             })  # when you have the f_in1 database with a chip.  You filter in a tissue_name.

# choose tissue name from in2
f_in1_in2 <- reactive({
  req(input$in2)
  filter(f_in1(), tissue_name %in% input$in2)
})



observeEvent(f_in1_in2(), {
  freezeReactiveValue(input, "in3")
  print("######### Print f_in1_in2()")
  print(f_in1_in2())
  choices <- unique(f_in1_in2()$time)
  updateSelectInput(inputId = "in3", choices = choices, selected = 5)
})

# choose time from in3
f_in1_in2_in3 <-reactive({
  req(input$in3)
  filter(f_in1_in2(), time %in% input$in3)
  #choices <- unique(f_in1_in2()$histopathology_name)
})

observeEvent(f_in1_in2_in3(), {
    print("######### Print f_in1_in2_in3()")
    print(f_in1_in2_in3())
})

# choose pathology from in4() 
observeEvent(f_in1_in2_in3(), {
  freezeReactiveValue(input, "in4")
  print("######### Print f_in1_in2_in3()")
  print(f_in1_in2_in3())
  choices <- unique(f_in1_in2_in3()$histopathology_name)
  updateSelectInput(inputId = "in4", choices = choices, selected = choices)
})

observeEvent(f_in1_in2(), {
  freezeReactiveValue(input, "in5")
  choices <- unique(gene)
  #updateSelectInput(inputId = "in5", choices = choices, selected = choices)
  updateSelectizeInput(inputId = "in5", choices = gene, server = TRUE, selected = 'Havcr1 | hepatitis A virus cellular receptor 1 (286934,1387965_at,AF035963_PROBE1)') #this line populates dropdown
})

f_in5 <- reactive({
  req(input$in5)
  unique(filter(df_pgQuery, gene_name %in% input$in5))
})

observeEvent(f_in5(), {
  print("######### Print f_in5()")
  print(f_in5())
})

############ The following is the eventReactive to take the input values from the menu choices (Genes to Pathology)

pathology_table = eventReactive(
  input$bu1,
  {
     input$in1 # chip
     input$in2 # tissue
     input$in3 # time  
     input$in5 # gene
     input$selectSensivity # add tissue specificity and sensitivity of having severity in Control groups
    #},
    #{
    withProgress(message = 'Retrieving affected genes from drugmatrix database in progress',
                 detail = 'This may take up to 1 minute ...', value = 0.2, { ##Start progress at 0
              
                   if (input$selectSensivity == 'Specificity') {
                   
                   pQuery <- paste0("SELECT DISTINCT his.histopathology_name histopathology_name, his.tissue_name tissue_name, liver.chip_name chip_name, liver.compound_name compound, liver.experiment_name, 
                                            liver.dose dose, liver.time timelength, his.average_severity_total severity_total, liver.log_ratio log_ratio, liver.entrezid, liver.annotation
                                     FROM   all_transcript_vu_fc liver left join bsb_histopathology_exp_fc his
                                     ON     his.experiment=liver.experiment 
                                     AND    his.tissue = liver.tissue
                                     WHERE  his.tissue_name = '", input$in2, "'
                                     AND    liver.time IN ", timeinput(input$in3), "
                                     AND    liver.chip_name IN ('", input$in1, "')
                                     AND    liver.annotation = substr('", input$in5, "', position ('|' IN '", input$in5, "')+2)   
                                    ;")
                   } else {
                     
                  pQuery <- paste0("SELECT DISTINCT his.histopathology_name histopathology_name, his.tissue_name tissue_name, liver.chip_name chip_name, liver.compound_name compound, liver.experiment_name, 
                                            liver.dose dose, liver.time timelength, his.average_severity_total severity_total, liver.log_ratio log_ratio, liver.entrezid, liver.annotation
                                    FROM   all_transcript_vu_fc liver inner join bsb_histopathology_exp_fc his
                                    ON     his.experiment=liver.experiment 
                                    AND    his.tissue = liver.tissue 
                                    INNER JOIN bsb_summary_apical_endpoint b
                                    ON     his.experiment = b. experiment AND b.tissue_name = his.tissue_name 
                                    AND    his.signature = b.signature AND his.histopathology_name = b.assay_name
                                    WHERE  liver.chip_name = b.chip_name AND liver.time = b.duration
                                    AND    his.tissue_name = '", input$in2, "' AND b.signature = '", input$in2, "'
                                    AND    liver.time IN ", timeinput(input$in3), "
                                    AND    liver.chip_name IN ('", input$in1, "')
                                    AND    liver.annotation = substr('", input$in5, "', position ('|' IN '", input$in5, "')+2)
                                    AND    b.call = 'above normal'
                                    AND   cast(b.measurement as numeric) > 0
                                    AND   his.average_severity_total > 0
                                          UNION
                                    SELECT DISTINCT his.histopathology_name histopathology_name, his.tissue_name tissue_name, liver.chip_name chip_name, liver.compound_name compound, liver.experiment_name, 
                                           liver.dose dose, liver.time timelength, his.average_severity_total severity_total, liver.log_ratio log_ratio, liver.entrezid, liver.annotation
                                     FROM   all_transcript_vu_fc liver inner join bsb_histopathology_exp_fc his
                                            ON     his.experiment=liver.experiment 
                                     AND    his.tissue = liver.tissue 
                                     INNER JOIN bsb_summary_apical_endpoint b
                                     ON     his.experiment = b. experiment AND b.tissue_name = his.tissue_name 
                                     AND    his.signature = b.signature AND his.histopathology_name = b.assay_name
                                     WHERE  liver.chip_name = b.chip_name AND liver.time = b.duration
                                     AND    his.tissue_name = '", input$in2, "' AND b.signature = '", input$in2, "'
                                     AND    liver.time IN ", timeinput(input$in3), "
                                     AND    liver.chip_name IN ('", input$in1, "')
                                     AND    liver.annotation = substr('", input$in5, "', position ('|' IN '", input$in5, "')+2)
                                     AND    b.call <> 'above normal'
                                     AND    liver.experiment_name NOT IN  (SELECT experiment_name FROM  
							                                                                                        (SELECT experiment_name, assay_name, compound, measurement, call
								                                                                                       FROM bsb_summary_apical_endpoint
								                                                                                       WHERE chip_name IN ('", input$in1, "')
								                                                                                       AND   signature = '", input$in2, "'
								                                                                                       AND   tissue_name = '", input$in2, "'
							                                                                                         ) mno
								                                                            WHERE  mno.call = 'above normal'
							                                                              )   
                                     AND   cast(b.measurement as numeric) = 0
                                     AND   his.average_severity_total = 0
                                    ;")         
                   }
                   
                   print(pQuery)
                   
                   df_pgPQuery <- dbGetQuery(pobj, pQuery)
                   
                   print("######################## The following is the queryoutput.################")
                   print(df_pgPQuery)
                   
                   print("################# The number of rows of df_pgPQuery.######################")
                   print(nrow(df_pgPQuery))
                   
                   
                   
                   tissue_name_display <- df_pgPQuery$tissue_name
                   
                   mytissue_name <- function() {  
                     return(tissue_name_display[1])   
                   } # Ouptut the tissue name in order to pass into the Summary Table
                   
                   rowSelected <- function() {
                     return(nrow(df_pgPQuery))
                   }
                   
                   df_pgPQuery <- filter(df_pgPQuery, !is.na(severity_total))
                   df_pgPQuery <- filter(df_pgPQuery, !is.na(histopathology_name))  # adding this line does not help to get rid of null value of histopathology_name
                   severity_total <- df_pgPQuery$severity_total
                   
                   lpathology <- df_pgPQuery$histopathology_name
                   log_ratio <- df_pgPQuery$log_ratio
                   chip_name <- df_pgPQuery$chip_name
                   compound <- df_pgPQuery$compound
                   experiment_name <- df_pgPQuery$experiment_name
                   timelength <- df_pgPQuery$timelength
                   dose <- df_pgPQuery$dose
                   #  gene_name <- df_pgPQuery$gene_name
                   gene_name <- df_pgPQuery$annotation
                   
                   print("###########Removing the NULL data and print the df_pgQuery Again! ##############")
                   print("################################################################################")
                   print(df_pgPQuery)
                   
                   
                   incProgress(0.5, message = "Data Retrieved!  Now Summarizing data ...",
                               detail = "This may take up to 30 seconds to 2 minutes")
                   if(nrow(df_pgPQuery) > 0){
                     tempDF <- df_pgPQuery %>%
                       filter(!is.na(severity_total)) %>%
                       mutate(average_severity_total_cat = ifelse(severity_total > 0, "greater", "zero"))
                     
                     mydataframe <-tempDF %>%
                       group_by(histopathology_name, average_severity_total_cat) %>% 
                       spread(average_severity_total_cat, log_ratio) %>%
                       summarise(count = sum(!is.na(greater)),
                                 count0 = sum(!is.na(zero)),
                                 mean = mean(greater, na.rm=T),
                                 mean0 = mean(zero, na.rm=T),
                                 diff = mean - mean0,
                                 sd = sd(greater, na.rm = T),
                                 sd0 = sd(zero, na.rm = T),
                                 tval = ifelse(count >= 3 & count0 >= 3, wilcox.test(greater, zero, var.equal = F)$statistic, NA),
                                 pval = ifelse(count >= 3 & count0 >= 3, wilcox.test(greater, zero, var.equal = F)$p.val, NA)) %>% filter(!is.na(pval))
                     
                     # print("############### First time to display mydataframe############")
                     #  print(mydataframe)
                     
                     d1 = data.frame("Pathology" = lpathology, "Compound" = compound,   "Experiment Name" =experiment_name, "Chip Type" = chip_name, "Dose mg per kg" = dose, "Time per day" = timelength, "Average Severity" = severity_total, "Log10 Ratio" = log_ratio)
                     names(d1) <- c("Pathology" , "Compound", "Experiment Name", "Chip Type", "Dose (mg/kg)", "Time (day)", "Average Severity", "Log10 Ratio")
                     
                     # Using tidyr package to re-arrange the columns and rows of summary data mydataframe. Use spread(), gather(), and unite()
                     # keep the first 2 columns and group counts, log10_ratio, std, into type column
                     
                     incProgress(0.4, message = "Formatting output in progress",
                                 detail = "This step just take 3 seconds ...")
                     
                     
                     names(mydataframe) <- c("Histopathology Name", "Counts(>0)", "Counts(0)", "Log10 Ratio(>0)", "Log10 Ratio(0)", "Log10 Ratio DIFF", "Std(>0)", "Std(0)", "Tval", "Pval")
                     tempdf <- data.frame(mydataframe$"Histopathology Name",
                                          mydataframe$"Counts(>0)", mydataframe$"Counts(0)",
                                          round(mydataframe$"Log10 Ratio(>0)", 3), round(mydataframe$"Log10 Ratio(0)", 3),
                                          round(mydataframe$"Log10 Ratio DIFF", 3),
                                          round(mydataframe$"Std(>0)", 3), round(mydataframe$"Std(0)", 3),
                                          round(mydataframe$"Tval", 3),
                                          round(mydataframe$"Pval", 8))
                     print("############ This is tempdf ###############")
                     print(tempdf)
                     names(tempdf) <- c("Histopathology Name", "Counts(>0)", "Counts(0)", "Log10 Ratio(>0)", "Log10 Ratio(0)", "Log10 Ratio DIFF", "Std(>0)", "Std(0)", "Tval", "Pval")
                     
                     tempdf$pvalcolor1 <- ifelse(tempdf$`Pval` < 0.05, 2, 1) # add a color code to check if P value > 0.05, make it 2, otherwise, make it 1
                     
                     names(tempdf) <- c("Histopathology Name", "Counts(>0)", "Counts(0)", "Log10 Ratio(>0)", "Log10 Ratio(0)", "Log10 Ratio DIFF", "STD(>0)", "STD(0)", "T-value","P-value", "pvalcolor1")
                     
                     mydataframe <- tempdf
                     print("##################### This is my final mydataframe #####################")
                     print(mydataframe)
                     
                   }
                   else
                   {
                     d1 = NULL
                     mydataframe = NULL
                   }
                 })  # Progress bar message ended
    return(list(mydata = d1, mysummary= mydataframe, outputmytissue_name = mytissue_name(), rowCount=rowSelected()))
  }
) # important function to get query and manipulate the pathology table, gene option


output$summaryTable = DT::renderDataTable(
  DT::datatable(
    pathology_table()$mysummary,
    #selection = list(target = 'cell'),
    selection = list(mode='single', target = 'cell',
                     selectable = rbind(cbind(1:nrow(pathology_table()$mysummary), rep(11, nrow(pathology_table()$mysummary))),
                                        cbind(1:nrow(pathology_table()$mysummary), rep(1, nrow(pathology_table()$mysummary))))
    ), # selection list, ONLY column 1 and column 11 are selectable.  But then column 11 will be hidden
    
    rownames = TRUE,
    escape = 3,
    
    caption = htmltools::tags$caption(style = 'caption-side: top; text-align: center; color:brown; font-size:150% ;','Table: Summary of Histopathology Counts and Log10 Ratio.'),
    
    # caption = htmltools::tags$caption(style = 'caption-side: top; text-align: center; color:blue; font-size:100% ;','To get the affect details and the boxplots, choose one or more pathologies of your interest.'),
    #extensions = 'Buttons',
    
    options=list(
      pageLength = 5, info = FALSE,
      selector = 'tr>td:nth-child(1)',  # only column 1 can be selected
      lengthMenu = list(c(5, 10, 25, 500, -1), c("5", "10", "25", "500", "All")),
      columnDefs = list(list(className = 'dt-center', targets = c(2,3,4,5,6,7,8,9,10)), list(targets=c(11), visible=FALSE)),
      order = list(10, 'asc'),
      # dom = 'Bfrtip',
      dom = 'Bliftsp',
      searchHighlight = TRUE,  # When you typy in the Search Box, it will highlight.
      buttons=c('copy', 'csv', 'excel', 'pdf', 'print')
    ),
    
    container = htmltools::withTags(table(class = 'display',
                                          thead(
                                            tr(
                                              th(rowspan=2, ' '),
                                              th(rowspan = 2, 'Histopathology Name'),
                                              th(colspan = 9, paste0('Gene Expression and Pathology Source: ', pathology_table()$outputmytissue_name), class = "dt-center"),
                                              tr(
                                                lapply(rep(
                                                  c('# Treatments w/ Pathology', '# Treatments w/o Pathology', 'Avg. Log10 Ratio Treatments w/ Pathology', 'Avg. Log10 Ratio Treatments w/o Pathology',
                                                    'Log10 Ratio DIFF', 'STD Treatments w/ Pathology', 'STD Treatments w/o Pathology', 'T-value','P-value'), 1
                                                ), th)
                                              ))))),
    extensions = c("Select"),
    #selection = 'none'
    
  ) # datatable
  %>% formatStyle('Histopathology Name', backgroundColor = '#d4efdf')  # Light Green color
  %>% formatStyle(columns = "P-value", valueColumns = "pvalcolor1", backgroundColor = styleEqual(levels = c(1, 2), values = c('white', 'pink'))  )
)  # Summary table output, gene option


countSelection <- function() {
  counting <- pathology_table()$rowCount
  if (counting > 0){
    message = c('There are', counting, 'rows of data retrieved from DrugMatrix')
  }else{
    #message = c('No matching data found in DrugMatrix')
    message = c('Click the Cell that contains Histopathology Name Information.  You clicked: ')
    # message = c('No matching data found in DrugMatrix')
  }
  message
}   # count for gene option

output$countNum = renderText({
  countSelection()
}) # output the count number, gene option


sumSelected <- function() {
  cs <- input$summaryTable_cells_selected
  ndf <- pathology_table()$mysummary[cs]
  ndf
} # gene option

output$summarySelect = renderText({
  sumSelected()
}) # output summary table selected cell, gene option


output$pathologyTable = DT::renderDataTable(
  pathology_table()$mydata %>% filter(Pathology%in%sumSelected()), 
  # columnDefs = list(list(className = 'dt-center', targets = 5)),
  extensions = 'Buttons',
  options = list(
    pageLength = 5, 
    info = FALSE,
    lengthMenu = list(c(5,  -1), c("First 5 rows", "All")),
    dom = 'Bliftsp',
    # dom = 'Bfrtip',
    buttons = c('copy', 'csv', 'excel', 'pdf', 'print')
  ),   
  rownames = TRUE,
  caption = htmltools::tags$caption(style = 'caption-side: top; text-align: center; color:brown; font-size:150% ;','Table: Affected Histopathology Detail.')#,
) # gene option

dataPlot <- function() 
{
  validate (
    need(nchar(input$summaryTable_cells_selected) > 0, "Choose one or more pathologies from the summary table above")
  )

  # cSelectPathology = paste0(sprintf("'%s'", sumSelected()), collapse = ", ")
  cSelectPathology = c(sumSelected())
  plotData <- filter(pathology_table()$mydata, `Pathology` %in% cSelectPathology)
  plotData$`Average Severity` = factor(plotData$`Average Severity`)
  
    ###### DEBUG
  print("DEBUG: The following data is in the graph.")
  print(plotData)
  
  plotData$`Average Severity`[is.na(plotData$`Average Severity`)]
  #plotData$`Average Severity`[is.na(plotData$`Average Severity`)] <- 'NULL'

  if (input$selectPlot == "0 or >0 Severities") {
    plotData$`Average Severity` = as.character(plotData$`Average Severity`)
    plotData$`Average Severity`[plotData$`Average Severity` >0] = 'greater than 0'
    plotData$`Average Severity` = factor(plotData$`Average Severity`)
  }

  
  if (nrow(plotData) > 0) {
    # draw the boxplot
    plt = ggplot(plotData, aes(x=`Average Severity`, y=`Log10 Ratio`, fill= `Average Severity`)) + xlab("Severity Score") + ylab("Log10 Ratio") + geom_boxplot() + geom_point() + theme_bw() + labs(fill = "Severity Score") + ggtitle("Figure: Box Plot of Severity versus Log10 Ratio") + theme(plot.title = element_text(lineheight=.60, hjust = 0.5, color = "brown", size = 20)) 
    plt + facet_wrap(~ `Pathology`, ncol = 2) + theme(axis.text.x = element_text(face="bold", size=12), axis.text.y = element_text(face="bold", size = 12), 
                                                              axis.title.x = element_text(color="#993333", size=14, face="bold"),
                                                              axis.title.y = element_text(color="#993333", size=14, face="bold")                                                                      
    ) 
  }
  else {
    plot(c(0, 1), c(0, 1), ann = F, bty = 'n', type = 'n', xaxt = 'n', yaxt = 'n')
    text(x = 0.5, y = 0.5, "No Data Found", 
         cex = 1.6, col = "black")
  }
} # algorithm of dataPlot function, gene option

output$boxPlot <- renderPlot(
  {
    dataPlot()
    
  }) # Ouput the plot, gene option

output$boxplot_download <- downloadHandler(
  filename = function()
  {
    paste(input$"geneprobeplot", paste0('geneprobe_', Sys.time(), '.png'), sep='')
  },
  content= function(file)
  {
    # ggsave(file, plot=dataPlot(), width=10, height=10.4)
    ggsave(file, plot=dataPlot(), width=8, height=10.4)
  }
) # Output the graph - Download button, gene option 


################### For Gene Option, in order to get the pathology image, Try to use dataPlot()$cSelectPathology as input. ####################

test <- function() {
  ccSelectPathology = paste0(sprintf("'%s'", sumSelected()), collapse = ", ")
  
  print("############### This is gene that I selected ")
  
  print(ccSelectPathology)
  
  gene_path_imageQuery <-paste0("SELECT  histopathology_name, severity_name, image_name from bsb_histopathology_image_fc WHERE histopathology_name in (", ccSelectPathology, ");")
  
  print("############## This is the gene_path_imageQuery #################")
  print(gene_path_imageQuery)
  
  df_gene_path_imageQuery <-dbGetQuery(pobj, gene_path_imageQuery) 
  
  print("############## This is the data frame of df_gene_path_imageQuery ##############")
  print(df_gene_path_imageQuery)
  
  gene_img_image_name <- df_gene_path_imageQuery$image_name
  gene_img_severity_name <- df_gene_path_imageQuery$severity_name
  gene_img_histopathology_name <- df_gene_path_imageQuery$histopathology_name
  
  gene_img_image_name <- paste0(gene_img_image_name, '.JPG_large.jpg')
  
  gene_path_dat <- data.frame(
    gene_img_histopathology_name,
    gene_img_severity_name,
    gene_img_image_name
  )
  
  names(gene_path_dat) <- c("Histopathology Name", "Severity", "Image Name")
  return(gene_path_dat)
}

output$genePathImagetable = DT::renderDataTable({
  DT::datatable(test(), filter = list(position = "top", clear = FALSE),
                selection = list(target = 'row'),
                options = list(
                  columnDefs=list(list(visible=FALSE, targets = 3)),  # this hids the image name
                  autowidth = TRUE,
                  pageLength = -1,  # show all the rows.  If only 3 row, then type 3
                  #lengthMenu = c(2, 4)
                  lengthMenu = list(c(3,  -1), c("First 3 rows", "All"))
                ))
})


gene_path_df <- function() {
  gpDF <- test()
  gpd <- gpDF[input$genePathImagetable_rows_selected, ]
  
  imgfr <- lapply(gpd$`Image Name`, function(file){  # Image Name used to be gene_img_image_name
    tags$div(
      tags$img(src=file, width="400", height="310"),
      tags$script(src="titlescript.js")
    )
  })
  imgList <- tagList(imgfr)
  return(imgList)
}

output$img2 = renderUI({
  gene_path_df()
})



###########################################################################################################################
################################### The Code for Pathology to Genes Tab ###################################################
###########################################################################################################################


# choose chip name from in1
f_tab2in1 <- reactive({
  filter(dat, chip %in% input$tab2in1)
}) # filter out the "chip" information.  the in1 is for chip choices.  f_in1 is the data frame when you decide to choose a chip.

observeEvent(f_tab2in1(), {
  freezeReactiveValue(input, "tab2in2")
  print("######### Print f_tab2in1()")
  print(f_tab2in1())
  choices<-unique(f_tab2in1()$tissue_name)
  updateSelectInput(inputId = "tab2in2", choices = choices)
})  # when you have the f_in1 database with a chip.  You filter in a tissue_name.

# choose tissue name from in2
f_tab2in1_in2 <- reactive({
  req(input$tab2in2)
  filter(f_tab2in1(), tissue_name %in% input$tab2in2)
})

observeEvent(f_tab2in1_in2(), {
  freezeReactiveValue(input, "tab2in3")
  print("######### Print f_tab2in1_in2()")
  print(f_tab2in1_in2())
  choices <- unique(f_tab2in1_in2()$time)
  updateSelectInput(inputId = "tab2in3", choices = choices, selected = 5)
})

# choose time from in3
f_tab2in1_in2_in3 <-reactive({
  req(input$tab2in3)
  filter(f_tab2in1_in2(), time %in% input$tab2in3)
  #choices <- unique(f_tab2in1_in2()$histopathology_name)
})

# observeEvent(f_tab2in1_in2_in3(), {
#   print("######### Print f_tab2in1_in2_in3()")
#   print(f_tab2in1_in2_in3())
# })

# choose pathology from in4() 
observeEvent(f_tab2in1_in2_in3(), {
  freezeReactiveValue(input, "tab2in4")
  print("######### Print f_tab2in1_in2_in3()")
  print(f_tab2in1_in2_in3())
  choices <- unique(f_tab2in1_in2_in3()$histopathology_name)
  updateSelectInput(inputId = "tab2in4", choices = choices, selected = choices)
})

############ The following is the eventReactive to take the input values from the menu choices (Genes to Pathology)

gene_table = eventReactive(
  input$bu2,
  {
    # input$selectPathology # in ui section
    # input$ptissue # in ui section 
    # input$pchip  # in ui section
    # input$ptime
    input$tab2in1 # chip
    input$tab2in2 # tissue
    input$tab2in3 # time  
    input$tab2in4 # pathology
    input$selectSpec # add tissue specificity and sensitivity of having severity in Control groups
    # },  # in ui section 
    # {
    withProgress(message = 'Retrieving affected genes from drugmatrix database in progress',
                 detail = 'This may take 1 ~ 2 minutes.', value = 0, { ##Start progress at 0 
                   
                 if (input$selectSpec == 'Specificity') {
                   
                   pathologyQuery <- paste0("SELECT  DISTINCT concat(liver.symbol, ' | ', liver.annotation) ggene_name, his.tissue_name gtissue_name, liver.compound_name compound_name, liver.experiment_name, avg(liver.log_ratio) glog_ratio, 
                                                liver.dose gdose, liver.time gtime, avg(his.average_severity_total) gseverity_total, liver.chip_name gchip_name, liver.entrezid
                                        FROM        all_transcript_vu_fc liver 
                                        inner JOIN   bsb_histopathology_exp_fc his
                                        ON		      his.experiment=liver.experiment
                                        AND         liver.tissue = his.tissue
                                        WHERE       his.tissue_name = '", input$tab2in2, "'
                                        AND         liver.chip_name IN ('", input$tab2in1, "')
                                        AND         his.histopathology_name = '", input$tab2in4, "'
                                        AND         liver.time IN ", timeinput(input$tab2in3), " 
                                        AND liver.symbol is NOT NULL AND liver.annotation is not NULL 
                                        GROUP BY   concat(liver.symbol, ' | ', liver.annotation), his.tissue_name, his.tissue_name, liver.compound_name, liver.experiment_name,liver.dose, liver.time, liver.chip_name, liver.entrezid;")
                 } else {
                   
                                       temptableQuery <- paste0("CREATE TEMP TABLE temp1 (SELECT assay_name, experiment_name, measurement, call
                                                                  WHERE tissue_name = '", input$tab2in2, "' AND signature = '", input$tab2in2, "'
                                                                  AND   call = 'above normal';
                                                               );")
                   
                   pathologyQuery <- paste0("
                                    SELECT   DISTINCT concat(liver.symbol, ' | ', liver.annotation) ggene_name, his.tissue_name gtissue_name, liver.compound_name compound_name, liver.experiment_name, 
                                             liver.log_ratio glog_ratio, b.assay_name, liver.dose gdose, liver.time gtime, his.average_severity_total gseverity_total, liver.chip_name gchip_name, liver.entrezid
                                    FROM   bsb_histopathology_exp_fc his 
                                    INNER JOIN bsb_summary_apical_endpoint b
                                       ON     his.experiment = b.experiment AND b.tissue_name = his.tissue_name AND his.signature = b.signature AND his.histopathology_name = b.assay_name
                                    INNER JOIN all_transcript_vu_fc liver
                                       ON     liver.experiment = his.experiment AND  liver.chip_name = b.chip_name AND liver.time = b.duration AND liver.tissue = his.tissue
                                    WHERE his.tissue_name = '", input$tab2in2, "'
                                    AND   his.signature = '", input$tab2in2, "'
                                    AND   b.chip_name = '", input$tab2in1, "'
                                    AND   b.call <> 'above normal'
                                    AND   liver.experiment_name NOT IN  (SELECT experiment_name FROM  
							                                                                      (SELECT experiment_name, assay_name, compound, measurement, call
								                                                                     FROM bsb_summary_apical_endpoint
								                                                                     WHERE chip_name IN ('", input$tab2in1, "')
								                                                                     AND   signature = '", input$tab2in2, "'
								                                                                     AND   tissue_name = '", input$tab2in2, "'
							                                                                       ) mno
								                                                                WHERE  mno.call = 'above normal'
							                                                                  )
                                    AND   b.duration IN ", timeinput(input$tab2in3), "
                                    AND   his.histopathology_name = '", input$tab2in4, "'
                                    AND   liver.symbol is NOT NULL 
                                    AND   his.average_severity_total = 0
                                    AND his.average_severity_total IS NOT NULL
                              UNION
                                    SELECT   DISTINCT concat(liver.symbol, ' | ', liver.annotation) ggene_name, his.tissue_name gtissue_name, liver.compound_name compound_name, liver.experiment_name, 
                                             liver.log_ratio glog_ratio, b.assay_name, liver.dose gdose, liver.time gtime, his.average_severity_total gseverity_total, liver.chip_name gchip_name, liver.entrezid
                                    FROM   bsb_histopathology_exp_fc his 
                                    INNER JOIN bsb_summary_apical_endpoint b
                                       ON     his.experiment = b.experiment AND b.tissue_name = his.tissue_name AND his.signature = b.signature AND his.histopathology_name = b.assay_name
                                    INNER JOIN all_transcript_vu_fc liver
                                       ON     liver.experiment = his.experiment AND  liver.chip_name = b.chip_name AND liver.time = b.duration AND liver.tissue = his.tissue
                                    WHERE his.tissue_name = '", input$tab2in2, "'
                                    AND   b.signature = '", input$tab2in2, "'
                                    AND   b.chip_name = '", input$tab2in1, "'
                                    AND   b.call = 'above normal'
                                    AND   cast(b.measurement as numeric) > 0
                                    AND   his.average_severity_total > 0
                                    AND   b.duration IN ", timeinput(input$tab2in3), "
                                    AND   his.histopathology_name = '", input$tab2in4, "'
                                    AND   liver.symbol is NOT NULL
                                    AND his.average_severity_total IS NOT NULL
                                        ;")
                 }
                   
                   
                   
                   print("This the df_pathology Query#########################")
                   print(pathologyQuery)
                   df_pathologyQuery <- dbGetQuery(pobj, pathologyQuery)
                   print("this is df_pathology after I pooled it########################")
                   print(df_pathologyQuery)
                   print("############ The number of rows the query is: ")
                   #print(nrow(df_pathologyQuery))
                   
                   prowSelected <- function() {return(nrow(df_pathologyQuery))}
                   print(prowSelected())
                   
                   gtissue_name <- df_pathologyQuery$gtissue_name
                   
                   mygtissue_name <- function() {
                     return(gtissue_name[1])
                   }
                   print("My tissue selected")
                   print(mygtissue_name())
                   
                   print("###########I ONLY NEED THE ABOVE####################")
                   
                   
                   
                   df_pathologyQuery <- filter(df_pathologyQuery, !is.na(gseverity_total))
                   ggene_name <- df_pathologyQuery$ggene_name
                   compound_name <- df_pathologyQuery$compound_name
                   experiment_name <- df_pathologyQuery$experiment_name
                   gchip_name <- df_pathologyQuery$gchip_name
                   gdose <- df_pathologyQuery$gdose
                   gtime <- df_pathologyQuery$gtime
                   gseverity_total <- df_pathologyQuery$gseverity_total
                   glog_ratio <- df_pathologyQuery$glog_ratio
                   entrezid <- df_pathologyQuery$entrezid
                   gtissue_name <- df_pathologyQuery$gtissue_name
                   
                   #increment progress to 50% when dbGetQuery finishes
                   incProgress(0.5, message = "Data Retrieved!  Now Summarizing data ...",
                               detail = "This may take ~ 4 minutes")
                   
                   if(nrow(df_pathologyQuery) > 0){
                     ptempDF <- df_pathologyQuery %>% 
                       # distinct (ggene_name, gtissue_name, compound_name, experiment_name, glog_ratio, gdose, gtime, gseverity_total, gchip_name) %>%
                       filter(!is.na(gseverity_total)) %>%
                       mutate(gaverage_severity_total_cat = ifelse(gseverity_total > 0, "greater", "zero")) 
                     
                     ptempDF %>% mutate(index = 1:nrow(.)) %>%
                       group_by(ggene_name, gaverage_severity_total_cat, entrezid) %>% 
                       spread(gaverage_severity_total_cat, glog_ratio) %>% 
                       mutate(greater = as.numeric(greater),
                              zero = as.numeric(zero)) %>%
                       summarise(count = sum(!is.na(greater)),
                                 count0 = sum(!is.na(zero)),
                                 mean = mean(greater, na.rm=T),
                                 mean0 = mean(zero, na.rm=T),
                                 diff = mean - mean0,
                                 sd = sd(greater, na.rm = T),
                                 sd0 = sd(zero, na.rm = T),
                                 tval = ifelse(count >= 3 & count0 >= 3, wilcox.test(greater, zero, var.equal = F)$statistic, NA),
                                 pval = ifelse(count >= 3 & count0 >= 3, wilcox.test(greater, zero, var.equal = F)$p.val, NA)) %>% filter(!is.na(pval)) -> mypathologydataframe
                     
                     # mypathologydataframe$pgurl <- c(paste0('https://www.ncbi.nlm.nih.gov/gene/', mypathologydataframe$entrezid))
                     # paste0('<a href="pgurl", target="_blank">', 'Entrez', '</a>')
                     
                    # mypathologydataframe$pgurl <- c(paste0('<a href="', paste0('https://www.ncbi.nlm.nih.gov/gene/', mypathologydataframe$entrezid), '" , target="_blank">Entrez</a>'))
                    mypathologydataframe$pgurl <- c( paste0(paste0('<a href="', paste0('https://www.ncbi.nlm.nih.gov/gene/', mypathologydataframe$entrezid), '" , target="_blank">Entrez</a>'), hlink(dfharmony, mypathologydataframe$entrezid)))
                    
                     print("########### This is the second output of mypathologydataframe")
                     print(mypathologydataframe)  # Finally, I added pgurl into the summary dataframe
                     # View(mypathologydataframe)
                     
                     print("########### This prints only 2 columns of mypathologydataframe")
                     
                     new_df <- mypathologydataframe[,c("pgurl","entrezid")]
                     print(new_df)
                     # View(new_df)
                     
                     
                     d2 = data.frame("Probe" = ggene_name, "Compound" = compound_name, "Experiment Name" = experiment_name,  "Chip Type" = gchip_name, "Dose mg per kg" = gdose, "Time per day" = gtime, "Average Severity" = gseverity_total, "Log10 Ratio" = glog_ratio)
                     
                     
                     #increment progress to 90% when summarise finishes
                     incProgress(0.4, message = "Formatting output in progress",
                                 detail = "This step just take 3 seconds ...")
                     
                     names(mypathologydataframe) <- c("Gene Probe Name", "Entrez ID", "Counts(>0)", "Counts(0)", "Log10 Ratio(>0)", "Log10 Ratio(0)", "Log10 Ratio DIFF", "Std(>0)", "Std(0)", "Tval", "Pval", "pgurl")
                     
                     pathtempdf <- data.frame(mypathologydataframe$"Gene Probe Name",
                                              mypathologydataframe$"pgurl",
                                              mypathologydataframe$"Counts(>0)", mypathologydataframe$"Counts(0)",
                                              round(mypathologydataframe$"Log10 Ratio(>0)", 3), round(mypathologydataframe$"Log10 Ratio(0)", 3),
                                              round(mypathologydataframe$"Log10 Ratio DIFF", 3),
                                              round(mypathologydataframe$"Std(>0)", 3), round(mypathologydataframe$"Std(0)", 3),
                                              round(mypathologydataframe$"Tval", 3),
                                              round(mypathologydataframe$"Pval", 8))
                     
                     names(pathtempdf) <- c("Gene Probe Name", "pgurl", "Counts(>0)", "Counts(0)", "Log10 Ratio(>0)", "Log10 Ratio(0)", "Log10 Ratio DIFF", "STD(>0)", "STD(0)", "T-value", "P-value")
                     
                     pathtempdf$pvalcolor2 <-ifelse(pathtempdf$`P-value` < 0.05, 2, 1) # add a color code to check if P value > 0.05, make it 2, otherwise, make it 1
                     
                     # View(pathtempdf)
                     
                     mypathologydataframe <- pathtempdf  # now the dataframe has 12 columns.  The 12th column won't show in the Summary table
                     
                   }
                   else
                   {
                     d2 = NULL
                     mypathologydataframe = NULL
                   }

                   ########################################################################################################################
                   ####################### The following code is for generating the pathology images ######################################
                   
                   
                   imageQuery <-paste0("SELECT  histopathology_name, severity_name, image_name from bsb_histopathology_image_fc
                                                                         WHERE histopathology_name = '", input$tab2in4, "';")
                   ########## WHERE histopathology_name = 'HEPATOCYTE, CENTRILOBULAR, LIPID ACCUMULATION, MACROVESICULAR';")
                   df_imageQuery <-dbGetQuery(pobj, imageQuery)
                   print(df_imageQuery)
                   img_image_name <- df_imageQuery$image_name
                   img_severity_name <- df_imageQuery$severity_name
                   img_histopathology_name <- df_imageQuery$histopathology_name
                   print(img_histopathology_name)

                   # the following images are from database
                   print (img_image_name)


                   img_image_name <- paste0(img_image_name, '.JPG_large.jpg')
                   imagedat <- data.frame(
                     img_histopathology_name,
                     img_severity_name,
                     img_image_name
                   )

                   names(imagedat) <- c('Histopathology Name', 'Severity', 'Image Name')
                   
                 }) # Progress bar message ended
    
    return(list(myplotdata = d2, mygenesummary = mypathologydataframe, outputgtissue = mygtissue_name(), mygraph = df_pathologyQuery, prowCount=prowSelected(), imgdat = imagedat))
  }
)   # important function to get query and maniplate the gene table table, pathology option


output$geneSummaryTable = DT::renderDataTable(
  DT::datatable(
    # gene_table()$mygenesummary$pgurl <- paste0('<a href="pgurl", target="_blank">', 'Entrez', '</a>'),
    {gene_table()$mygenesummary},
    selection = list(mode='single', target = 'cell',
                     selectable = rbind(cbind(1:nrow(gene_table()$mygenesummary), rep(12, nrow(gene_table()$mygenesummary))),
                                        cbind(1:nrow(gene_table()$mygenesummary), rep(1, nrow(gene_table()$mygenesummary))))
    ), # selection list
    
    rownames = TRUE,
    escape = FALSE,
    
    #selection = list(target = 'cell'),
    
    caption = htmltools::tags$caption(style = 'caption-side: top; text-align: center; color:brown; font-size:150% ;','Table: Summary of Histopathology Counts and Log10 Ratio.'),
    
    # caption = htmltools::tags$caption(style = 'caption-side: top; text-align: center; color:blue; font-size:100% ;','To get the affect details and the boxplots, choose one or more gene probes of your interest.'),
    
    extensions = 'Buttons', 
    options=list(
      pageLength = 5, info = FALSE,
      lengthMenu = list(c(5, 10, 25, 100, 1000, -1), c("5", "10", "25", "100", "1000", "All")),
      columnDefs = list(list(className = 'dt-center', targets = c(2, 3,4,5,6,7,8,9,10, 11)), list(className = 'dt-center', targets = c(12),  visible = FALSE) ),
      order = list(7, 'desc'),
      # dom = 'Bfrtip',
      dom = 'Bliftsp',
      searchHighlight = TRUE,  # When you typy in the Search Box, it will highlight.
      buttons=c('copy', 'csv', 'excel', 'pdf', 'print')
    ),
    container = htmltools::withTags(table(class = 'display',
                                          thead(
                                            tr(
                                              th(rowspan=2, ' '),
                                              th(rowspan = 2, 'Gene Probe Name'),
                                              th(rowspan = 2, 'Gene Link'),
                                              th(colspan = 9, paste0('Gene Expression and Pathology Source: ', gene_table()$outputgtissue), class = "dt-center"),
                                              tr(
                                                lapply(rep(
                                                  c('# Treatments w/ Pathology', '# Treatments w/o Pathology', 'Avg. Log10 Ratio Treatments w/ Pathology', 'Avg. Log10 Ratio Treatments w/o Pathology',
                                                    'Log10 Ratio DIFF', 'STD Treatments w/ Pathology', 'STD Treatments w/o Pathology', 'T-value','P-value'), 1
                                                ), th)
                                              ))))),
    
  )%>% formatStyle('Gene Probe Name', backgroundColor = '#d4efdf')  # Light Green color
  #%>% formatStyle('Gene Information', valueColumns="<a href='pgurl'>EntrezID</a>")
  %>% formatStyle(columns = "P-value", valueColumns = "pvalcolor2", backgroundColor = styleEqual(levels = c(1, 2), values = c('white', 'pink'))  )
) # Summary table output, pathology option


######### This imagetable is created for the Pathology to Genes.  You choose a histopathology.  It should display the corresponding images if existing #########
output$imagetable = DT::renderDT({
  datatable(gene_table()$imgdat, filter = list(position = "top", clear = FALSE),    ########## imgdat retrieve here #############
            selection = list(target = 'row'),
            options = list(
              columnDefs=list(list(visible=FALSE, targets = 3)),  # this hids the image name
              autowidth = TRUE,
              pageLength = -1,  # show all the rows.  If only 3 row, then type 3
              lengthMenu = list(c(2,  -1), c("First 2 rows", "All"))
            ))
})

imgdf <- function() {
  gene_table()$imgdat[input$imagetable_rows_selected, ]
}

# Front image output
output$path2geneimg = renderUI({
  imgfr <- lapply(imgdf()$`Image Name`, function(file){  # `Image Name` used to be img_image_name in gene_table()'s imgdat
    tags$div(
      tags$img(src=file, width="400", height="310"),
      tags$script(src="titlescript.js")
    )
  })
  do.call(tagList, imgfr)
})


sumGeneSelected <- function() {
  csg <- input$geneSummaryTable_cells_selected
  ngdf <- gene_table()$mygenesummary[csg]
  ngdf
} # pathology option

output$geneSummarySelect = renderText({
  sumGeneSelected() 
}  ) # output summary table selected cell, pathology option

output$geneTable = DT::renderDataTable(
  gene_table()$myplotdata %>% filter(Probe%in%sumGeneSelected()),
  extensions = 'Buttons',
  options = list(
    pageLength = 5,
    info = FALSE,
    lengthMenu = list(c(5,  -1), c("First 5 rows", "All")),
    dom = 'Bliftsp',
    buttons = c('copy', 'csv', 'excel', 'pdf', 'print')
  ),
  rownames = TRUE,
  caption = htmltools::tags$caption(style = 'caption-side: top; text-align: center; color:brown; font-size:150% ;','Table: Affected Gene Detail.')#,
) # pathology option

dataPlotPath <- function() 
{
  validate (
    need(nchar(input$geneSummaryTable_cells_selected) > 0, "Choose one or more gene probes from the summary table above")
  )
  
  # cSelectGene = paste0(sprintf("'%s'", sumGeneSelected()), collapse = ", ")
  cSelectGene = c(sumGeneSelected())
  
  plotDataPath <- filter(gene_table()$mygraph, ggene_name %in% cSelectGene)
  plotDataPath$gseverity_total = factor(plotDataPath$gseverity_total)
  
  ###### DEBUG
  print("DEBUG: The following data is in the graph.")
  print(plotDataPath)
  
  # transform data based on input$selectPlotPath in "ui.R"
  if (input$selectPlotPath == "0 or >0 Severities") {
    plotDataPath$gseverity_total = as.character(plotDataPath$gseverity_total)
    plotDataPath$gseverity_total[plotDataPath$gseverity_total >0] <- 'greater than 0'
    plotDataPath$gseverity_total = factor(plotDataPath$gseverity_total)
  }
  
  if (nrow(plotDataPath) > 0) {
    pltpath = ggplot(plotDataPath, aes(x=gseverity_total, y=glog_ratio, fill= gseverity_total)) + xlab("Severity Score") + ylab("Log10 Ratio") + geom_boxplot() + geom_point() + theme_bw() + labs(fill = "Severity Score") + ggtitle("Figure: Box Plot of Severity versus Log10 Ratio") + theme(plot.title = element_text(lineheight=.60, hjust = 0.5, color = "brown", size = 20))
    
    pltpath + facet_wrap(~ ggene_name, ncol = 2) + theme(axis.text.x = element_text(face="bold", size=12), axis.text.y = element_text(face="bold", size = 12), 
                                                         axis.title.x = element_text(color="#993333", size=14, face="bold"),
                                                         axis.title.y = element_text(color="#993333", size=14, face="bold")                                                                      
    ) 
  }
  else {
    plot(c(0, 1), c(0, 1), ann = F, bty = 'n', type = 'n', xaxt = 'n', yaxt = 'n')
    text(x = 0.5, y = 0.5, "No Data Found", 
         cex = 1.6, col = "black")
  }
} # algorithm of dataPlotPath function, pathology option

output$boxPlotPath <- renderPlot(
  {
    dataPlotPath()
  }
) # Ouput the plot, pathology option 

output$pathlogyPlot_download <- downloadHandler(
  filename = function()
  {
    # paste(input$"pathologyPlot", 'pathology.png', sep='')
    paste(input$"pathologyPlot", paste0('pathology_', Sys.time(), '.png'), sep='')
  },
  content= function(file)
  {
    ggsave(file, plot=dataPlotPath(), width=10, height=10.4)
  }
) # Output the graph - Download button, pathology option

###########################################################################################################################
############################### The Code for Genes to Clinical Pathology Tab ##############################################
###########################################################################################################################

# choose chip_name from cpin1
f_cpin1 <- reactive({
  filter(cpdat, chip_name %in% input$cpin1)
}) # filter out the "chip_name" information.  the cpin1 is for chip choices.  f_in1 is the data frame when you decide to choose a chip.

observeEvent(f_cpin1(), {
  freezeReactiveValue(input, "cpin2")
  print("######### Print f_cpin1()")
  print(f_cpin1())
  #choices<-unique(f_cpin1()$tissue_num)
  choices<-unique(f_cpin1()$tissue)
  updateSelectInput(inputId = "cpin2", choices = choices)
})  # when you have the f_in1 database with a chip_name.  You filter in a tissue_num, correspondingly to various tissues.

# choose tissue from cpin2
f_cpin1_in2 <- reactive({
  req(input$cpin2)
  filter(f_cpin1(), tissue %in% input$cpin2)
  #filter(f_cpin1(), tissue_num %in% input$cpin2)
})

observeEvent(f_cpin1_in2(), {
  freezeReactiveValue(input, "cpin3")
  print("######### Print f_cpin1_in2()")
  print(f_cpin1_in2())
  choices <- unique(f_cpin1_in2()$type)
  updateSelectInput(inputId = "cpin3", choices = choices, selected = 'BLOOD_CHEM')
})

# choose type (Blood Chemistry or Hematology) from in3
f_cpin1_in2_in3 <-reactive({
  req(input$cpin3)
  filter(f_cpin1_in2(), type %in% input$cpin3)
})

observeEvent(f_cpin1_in2_in3(), {
  freezeReactiveValue(input, "cpin4")
  print("######### Print f_cpin1_in2_in3()")
  print(f_cpin1_in2_in3())
  choices <- unique(f_cpin1_in2_in3()$category)
  updateSelectInput(inputId = "cpin4", choices = choices, selected = 'increase')
})

# choose a clinical assay from in7
# observeEvent(f_cpin1_in2_in3(), {
#   freezeReactiveValue(input, "cpin7")
#   print("######### Print f_cpin1_in2_in3()")
#   print(f_cpin1_in2_in3())
#   choices <- unique(f_cpin1_in2_in3()$assay_name)
#   updateSelectInput(inputId = "cpin7", choices = choices, selected = 'CHOLESTEROL')
# })


# choose time from in6
observeEvent(f_cpin1_in2_in3(), {
  freezeReactiveValue(input, "cpin6")
  print("######### Print f_cpin1_in2_in3()")
  print(f_cpin1_in2_in3())
  choices <- unique(f_cpin1_in2_in3()$time)
  updateSelectInput(inputId = "cpin6", choices = choices, selected = 5)
})

observeEvent(f_cpin1_in2(), {
  freezeReactiveValue(input, "cpin5")
  choices <- unique(gene)
  updateSelectizeInput(inputId = "cpin5", choices = gene, server = TRUE, selected = 'Abcc3 | ATP binding cassette subfamily C member 3 (140668,ABCC3_7941,AB010467_PROBE1,1369698_at,AB010467_s_at)') #this line populates dropdown
})

f_cpin5 <- reactive({
  req(input$cpin5)
  unique(filter(df_pgQuery, gene_name %in% input$cpin5))
})

observeEvent(f_cpin5(), {
  print("######### Print f_cpin5()")
  print(f_cpin5())
})


clin_pathology_table = eventReactive(
  input$bu3,
  {
    input$cpin1       # original cpchip 
    input$cpin2       # original cptissue     This is a character based.  For example, 7 is KIDNEY, 8 is LIVER. 6 is INTESTINE.  5 is HEART.  1 is BONE MARROW.  11 is SPLEEN.  16 is THIGH MUSCLE. 2 is BRAIN
    input$cpin3       # original cptype
    input$cpin4       # original cpcategories
    input$cpin5       # orignial selectProbe
    input$cpin6       # original cptime
    # },
    # {
    
    
    withProgress(message = 'Retrieving affected genes from drugmatrix database in progress',
                 detail = 'This may take up to 1 minute ...', value = 0.2, { ##Start progress at 0

                   cpQuery <- paste0("SELECT  a.symbol, a.annotation, b.assay_name cpassay_name, b.experiment_name cpexperiment_name,
                                                  a.tissue cptissue, b.type cptype, b.chip_name cpchip, b.time cptime, a.dose cpdose, b.avg_val cpavg_val,
                                                  a.log_ratio cplog_ratio, b.normal_range, b.categories cpcategories
                                         FROM     all_transcript_vu_fc a RIGHT  JOIN  bsb_blood_report_vu_fc b
                                         ON       a.experiment = b.experiment AND a.time = b.time AND a.chip_name = b.chip_name AND a.dose=b.dose AND a.tissue = b.tissue
                                         WHERE    b.chip_name  IN ('", input$cpin1, "') AND      b.type = '", (input$cpin3), "' AND      b.time IN ", timeinput(input$cpin6), "
                                         AND      a.tissue IN (", tissue_name2num(input$cpin2), ")  AND      b.categories IN ('", (input$cpin4), "', 'normal')
                                         AND      a.annotation = substr('", input$cpin5, "', position ('|' IN '", input$cpin5, "')+2)
                                         ;")
                   
                   myd3inputcategory <- input$cpin4
                   
                   print("############ The following is my query for gene to clinical pathology")
                   print(cpQuery)

                   df_cpgPQuery <- dbGetQuery(pobj, cpQuery)

                   print("######################## The following is the queryoutput.################")
                   print(df_cpgPQuery)
                   print("########### The number of rows of the query is: ")
                   print(nrow(df_cpgPQuery))

                   cpgrowSelected <- function() {
                     return(nrow(df_cpgPQuery))
                   }  # number of rows returned when an assay name is chosen

                   ###########################################################################################
                   ########### Since there are 3 categories - normal, decrease, increase #####################
                   ########### Split them into two: normal vs increase; normal vs decrease -> rbind ##########
                   ###########################################################################################


                   ########### Declare the variables to be used in sub-dataframes and dataframe ##############
                   cpassay_name <- df_cpgPQuery$cpassay_name
                   cplog_ratio <- df_cpgPQuery$cplog_ratio
                   cpchip <- df_cpgPQuery$cpchip
                   cpexperiment_name <- df_cpgPQuery$cpexperiment_name
                   cptime <- df_cpgPQuery$cptime
                   cpdose <- df_cpgPQuery$cpdose
                   cpgene_name <- df_cpgPQuery$annotation
                   cpcategories <- df_cpgPQuery$cpcategories
                   cpavg_val <- round(df_cpgPQuery$cpavg_val, 3)
                   cpnormal_range <- df_cpgPQuery$normal_range
                   cptissue <- df_cpgPQuery$cptissue
                   cptissue <- as.integer(cptissue)  # Convert floating point cptissue into integer


                   mytissue <- function() {
                     x =''
                     if (cptissue[1] == 8) {
                       x = 'LIVER'
                     } else if (cptissue[1] == 7) {
                       x = 'KIDNEY'
                     } else if (cptissue[1] == 6) {
                       x = 'INTESTINE'
                     } else if (cptissue[1] == 5) {
                       x = 'HEART'
                     } else if (cptissue[1] == 1) {
                       x = 'BONE MARROW'
                     } else if (cptissue[1] == 2) {
                       x = 'BRAIN'
                     } else if (cptissue[1] == 11) {
                       x = 'SPLEEN'
                     } else {
                       x = 'THIGH MUSCLE'
                     }
                     return (x)
                   }


                   incProgress(0.5, message = "Data Retrieved!  Now Summarizing data ...",
                               detail = "This may take up to 30 seconds to 2 minutes")

                   if(nrow(df_cpgPQuery) > 0){

                     cpcategories<- as.factor(df_cpgPQuery$cpcategories)
                     
                     if(input$cpin4=='increase') {
                       df_cpgPQuery %>%
                         filter(cpcategories != "decrease") %>% #keep only increase category
                         group_by(cpassay_name)%>% #group by assay
                         filter(sum(cpcategories == "increase") > 1) %>% #filter assays with <=1 increase
                         spread(cpcategories, cplog_ratio) %>% #move log_ratio to two columns: increase and normal
                         summarise(count = sum(!is.na(increase)),
                                   count0 = sum(!is.na(normal)),
                                   mean = mean(increase, na.rm = T),
                                   mean0 = mean(normal, na.rm = T),
                                   diff = mean - mean0,
                                   sd = sd(increase, na.rm = T),
                                   sd0 = sd(normal, na.rm = T),
                                   tval = wilcox.test(increase, normal, var.equal = F)$statistic, #use x,y input for t.test instead of formula
                                   pval = wilcox.test(increase, normal, var.equal = F)$p.val) %>% filter(!is.na(pval)) %>%
                         mutate(cpcategory = "increase") ->
                         output_table
                     }
                     else{
                       df_cpgPQuery %>%
                         filter(cpcategories != "increase") %>% #keep only increase category
                         group_by(cpassay_name)%>% #group by assay
                         filter(sum(cpcategories == "decrease") > 1) %>% #filter assays with <=1 decrease
                         spread(cpcategories, cplog_ratio) %>% #move log_ratio to two columns: decrease and normal
                         summarise(count = sum(!is.na(decrease)),
                                   count0 = sum(!is.na(normal)),
                                   mean = mean(decrease, na.rm = T),
                                   mean0 = mean(normal, na.rm = T),
                                   diff = mean - mean0,
                                   sd = sd(decrease, na.rm = T),
                                   sd0 = sd(normal, na.rm = T),
                                   tval = wilcox.test(decrease, normal, var.equal = F)$statistic, #use x,y input for t.test instead of formula
                                   pval = wilcox.test(decrease, normal, var.equal = F)$p.val) %>% filter(!is.na(pval)) %>%
                         mutate(cpcategory = "decrease") ->
                         output_table
                     }

                     print(output_table)

                     cpcategory <- output_table$cpcategory

                     mycpcategory <- function(){
                       return(cpcategory[1])   # Only return the first value of category vector
                     }


                     print("########### here is the category increase or decrease I want to show")
                     print(mycpcategory())
                     #print(cpcategory[1])
                     print("######## This is it! #########")

                     cpgsummary <- data.frame(output_table$cpassay_name, output_table$cpcategory, output_table$count, output_table$count0, round(output_table$mean, 4), round(output_table$mean0, 4), round(output_table$diff, 4),
                                              round(output_table$sd, 4), round(output_table$sd0, 4), round(output_table$tval, 3), round(output_table$pval, 8))
                     names(cpgsummary) <- c("Clinical Assay", "Assay Effect", "Count(*)", "Count(0)", "Avg. Log10(*)", "Avg. Log10(0)", "Log10 DIFF", "SD(*)", "SD(0)", "Tval(*)", "Pval(*)" )

                     cpgsummary$pvalcolor3 <- ifelse(cpgsummary$`Pval(*)` < 0.05, 2, 1) # add a color code to check if P value > 0.05, make it 2, otherwise, make it 1

                     d3 = data.frame("Clinical Assay" = cpassay_name, "Categories"= cpcategories, "Expression Source" = mytissue(), "Experiment" = cpexperiment_name, "Chip Type" = cpchip, "Dose mg per kg" = cpdose, "Time per day" = cptime, "Avg Value"= cpavg_val, "Range of Normal"=cpnormal_range,"Log10 Ratio" = cplog_ratio)


                     names(d3) <- c("Clinical Assay" , "Assay Effect", "Expression Source", "Experiment",   "Chip Type", "Dose (mg/kg)", "Time (day)", "Avg Value", "Range of Normal","Log10 Ratio")

                     incProgress(0.4, message = "Formatting output in progress",
                                 detail = "This step just take 3 seconds ...")

                   }

                   else
                   {
                     d3 = NULL
                     cpgsummary = NULL
                   }
                 })  # Progress bar message ended
    return(list(mycpdata=d3, mygenetocpdataframe=cpgsummary, cpgtissue=mytissue(), myclinGeneToPathGraph=df_cpgPQuery, outputcpgcategory=myd3inputcategory, cpgrowCount=cpgrowSelected()))
  }
) # important function to get query and manipulate the gene to clinical pathology option

cpgcountSelection <- function() {
  counting <- clin_pathology_table()$cpgrowCount
  if (counting > 0){
    message = c('There are', counting, 'rows of data retrieved from DrugMatrix')
  }else{
    # message = c('No matching data found in DrugMatrix')
    message = c('No matching data found in DrugMatrix')
  }
  message
} # count for gene to clinical pathology option

output$cpgCountNum = renderText({
  cpgcountSelection()
}) # output the count number, gene to clinical pathology option



output$genetoCPSummaryTable = DT::renderDataTable(
  DT::datatable(
    clin_pathology_table()$mygenetocpdataframe,
    selection = list(mode='single', target = 'cell',
                     selectable = rbind(cbind(1:nrow(clin_pathology_table()$mygenetocpdataframe), rep(12, nrow(clin_pathology_table()$mygenetocpdataframe))),
                                        cbind(1:nrow(clin_pathology_table()$mygenetocpdataframe), rep(1, nrow(clin_pathology_table()$mygenetocpdataframe))))
    ), # selection list
    
    rownames = TRUE,
    escape = 3,

    #selection = list(target = 'cell'),

    caption = htmltools::tags$caption(style = 'caption-side: top; text-align: center; color:brown; font-size:150% ;','Table: Summary of Clinical Pathology Gene Expression.'),

    extensions = 'Buttons',
    options=list(
      pageLength = 8, info = FALSE,
      lengthMenu = list(c(8, 16, 30, 1000, -1), c("8", "16", "30", "1000", "All")),
      columnDefs = list(list(className = 'dt-center', targets=c(2,3,4,5,6,7,8,9,10,11), visible=TRUE), list(className = 'dt-center', targets = c(12),  visible = FALSE)),
      order = list(7, 'desc'),
      # dom = 'Bfrtip',
      searchHighlight = TRUE,  # When you typy in the Search Box, it will highlight.
      dom = 'Bliftsp',
      buttons=c('copy', 'csv', 'excel', 'pdf', 'print')
    ),

    container = htmltools::withTags(table(class = 'display',
                                          thead(
                                            tr(
                                              th(rowspan=2, ' '),
                                              th(rowspan = 2, 'Clinical Assay'),
                                              th(rowspan = 2, 'Assay Effect', class = "dt-center"),
                                              th(colspan = 9, paste0('Gene Expression Source: ', clin_pathology_table()$cpgtissue), class = "dt-center"),
                                              tr(
                                                lapply(rep(
                                                  c('# Treatments \u2260 Normal Range', '# Treatments = Normal Range', 'Avg. Log10 Ratio Treatments \u2260 Normal Range', 'Avg. Log10 Ratio Treatments= Normal Range',
                                                    'Log10 Ratio DIFF', 'STD Treatments \u2260 Normal Range', 'STD Treatments = Normal Range', 'T-value', 'P-value'), 1
                                                ), th)
                                              ))))),    # \u2260 is the unicode for ??? sign or symbol

  ) %>% formatStyle('Clinical Assay', backgroundColor = '#d4efdf')  # Light Green color
  %>% formatStyle(columns = "Pval(*)", valueColumns = "pvalcolor3", backgroundColor = styleEqual(levels = c(1, 2), values = c('white', 'pink'))  )

) # Summary table output, gene to clinical pathology option


output$assaySummarySelect = renderText({
  sumClinAssaySelected()
}  ) # output summary table selected cell, gene to clinical pathology option

sumClinAssaySelected <- function() {
  csgp <- input$genetoCPSummaryTable_cells_selected
  ngdf <- clin_pathology_table()$mygenetocpdataframe[csgp]
  ngdf
} # gene to clinical pathology option


output$clin_pathologyTable = DT::renderDataTable(
  clin_pathology_table()$mycpdata%>%filter(`Clinical Assay`%in%sumClinAssaySelected()),
  extensions = 'Buttons',
  options = list(
    pageLength = 10,
    info = FALSE,
    lengthMenu = list(c(10,  -1), c("First 10 rows", "All")),
    dom = 'Bliftsp',
    buttons = c('copy', 'csv', 'excel', 'pdf', 'print')
  ),
  rownames = TRUE,
  caption = htmltools::tags$caption(style = 'caption-side: top; text-align: center; color:brown; font-size:150% ;','Table: Affected Clinical Pathology Detail.')#,
) # gene to clinical pathology option

dataPlotClinGeneToPath <- function()
{
  validate (
    need(nchar(input$genetoCPSummaryTable_cells_selected) > 0, "Choose one or more pathology assays from the summary table above")
  )

  clinGeneToPathSelectGene = c(sumClinAssaySelected())

  plotDataClinGeneToPath <- filter(clin_pathology_table()$myclinGeneToPathGraph, cpassay_name %in% clinGeneToPathSelectGene)
  plotDataClinGeneToPath$cpcategories = factor(plotDataClinGeneToPath$cpcategories)

  ###### DEBUG
  print("DEBUG: The following data is in the graph.")
  print(plotDataClinGeneToPath)

  # transform data based on input$selectPlotPath in "ui.R"
  # if (input$selectPlotPath == "0 or >0 Severities") {
   # plotDataClinGeneToPath$cpcategories = as.character(plotDataClinGeneToPath$cpcategories)
   #    plotDataClinGeneToPath$cpcategories[plotDataClinGeneToPath$cpcategories == 'increase'] <- 'increase'
   # plotDataClinGeneToPath$cpcategories = factor(plotDataClinGeneToPath$cpcategories)
  # }
   
   plotDataClinGeneToPath$cpcategories <- factor(plotDataClinGeneToPath$cpcategories, levels=c("normal", "decrease", "increase"))

  if (nrow(plotDataClinGeneToPath) > 0) {
    clinGenePathpltpath = ggplot(plotDataClinGeneToPath, aes(x=cpcategories, y=cplog_ratio, fill= cpcategories)) + xlab("Clinical Pathology Effect") + ylab("Log10 Ratio") + geom_boxplot() + geom_point() + theme_bw() + labs(fill = "Pathology Effect") + ggtitle("Figure: Box Plot of Clinical Pathology Effect versus Log10 Ratio") + theme(plot.title = element_text(lineheight=.60, hjust = 0.5, color = "brown", size = 20))

    clinGenePathpltpath + facet_wrap(~ cpassay_name, ncol = 2) + theme(axis.text.x = element_text(face="bold", size=12), axis.text.y = element_text(face="bold", size = 12),
                                                                       axis.title.x = element_text(color="#993333", size=14, face="bold"),
                                                                       axis.title.y = element_text(color="#993333", size=14, face="bold")
    )
  }
  else {
    plot(c(0, 1), c(0, 1), ann = F, bty = 'n', type = 'n', xaxt = 'n', yaxt = 'n')
    text(x = 0.5, y = 0.5, "No Data Found",
         cex = 1.6, col = "black")
  }
} # algorithm of dataPlotClinGeneToPath function,  gene to clinical pathology option


output$boxPlotClinGenetoPath <- renderPlot(
  {
    dataPlotClinGeneToPath()
  }
) # Ouput the plot, clinical gene to pathology option



output$cg2pathlogyPlot_download <- downloadHandler(
  filename = function()
  {
    # paste(input$"Clinical_gene_to_pathologyPlot", 'clinical_gene_to_pathology.png', sep='')
    paste(input$"Clinical_gene_to_pathologyPlot", paste0('clin_gene2pathology_', Sys.time(), '.png'), sep='')
  },
  content= function(file)
  {
    ggsave(file, plot=dataPlotClinGeneToPath(), width=10, height=10.4)
  }
) # Output the graph - Download button, clinical gene to pathology option



###########################################################################################################################
############################### The Code for Clinical Pathology to Genes Tab ##############################################
###########################################################################################################################

# choose chip_name from pcin1
f_pcin1 <- reactive({
  filter(cpdat, chip_name %in% input$pcin1)
}) # filter out the "chip_name" information.  the pcin1 is for chip choices.  f_in1 is the data frame when you decide to choose a chip.

observeEvent(f_pcin1(), {
  freezeReactiveValue(input, "pcin2")
  print("######### Print f_pcin1()")
  print(f_pcin1())
  choices<-unique(f_pcin1()$tissue)
  updateSelectInput(inputId = "pcin2", choices = choices, selected = 'LIVER')
})  # when you have the f_pcin1 database with a chip_name.  You filter in a tissue, correspondingly to various tissues.

# choose tissue from pcin2
f_pcin1_in2 <- reactive({
  req(input$pcin2)
  filter(f_pcin1(), tissue %in% input$pcin2)
})

observeEvent(f_pcin1_in2(), {
  freezeReactiveValue(input, "pcin3")
  print("######### Print f_pcin1_in2()")
  print(f_pcin1_in2())
  choices <- unique(f_pcin1_in2()$type)
  updateSelectInput(inputId = "pcin3", choices = choices, selected = 'BLOOD_CHEM')
})

# choose type (Blood Chemistry or Hematology) from in3
f_pcin1_in2_in3 <-reactive({
  req(input$pcin3)
  filter(f_pcin1_in2(), type %in% input$pcin3)
})

observeEvent(f_pcin1_in2_in3(), {
  freezeReactiveValue(input, "pcin4")
  print("######### Print f_cpin1_in2_in3()")
  print(f_pcin1_in2_in3())
  choices <- unique(f_pcin1_in2_in3()$category)
  updateSelectInput(inputId = "pcin4", choices = choices, selected = 'decrease')
})

# choose a clinical assay from pcin7
observeEvent(f_pcin1_in2_in3(), {
  freezeReactiveValue(input, "pcin7")
  print("######### Print f_pcin1_in2_in3()")
  print(f_pcin1_in2_in3())
  choices <- unique(f_pcin1_in2_in3()$assay_name)
  updateSelectInput(inputId = "pcin7", choices = choices)
})


# choose time from pcin6
observeEvent(f_pcin1_in2_in3(), {
  freezeReactiveValue(input, "pcin6")
  print("######### Print f_pcin1_in2_in3()")
  print(f_pcin1_in2_in3())
  choices <- unique(f_pcin1_in2_in3()$time)
  updateSelectInput(inputId = "pcin6", choices = choices, selected = 5)
})


clin_pathology2gene_table = eventReactive(
  input$bu4,
  {
    input$pcin1  # original cpchip
    input$pcin2  # original cptissue
    input$pcin3  # input$c2gtype
    input$pcin4  # cpcategories # assay effect
    input$pcin6  # original cptime
    input$pcin7  # originial selectAssay
    # },
    # {
    
    withProgress(message = 'Retrieving affected genes from drugmatrix database in progress',
                 detail = 'This may take up to 1 minute ...', value = 0.2, { ##Start progress at 0
                   
      cp2gQuery <- paste0("SELECT   distinct concat(a.symbol, ' | ', a.annotation) ggene_name, b.assay_name cpassay_name, b.experiment_name cpexperiment_name,
                                    a.tissue cptissue, b.type cptype, b.chip_name cpchip, b.time cptime, a.dose cpdose, b.avg_val cpavg_val,
                                    a.log_ratio cplog_ratio, b.normal_range, b.categories cpcategories
                           FROM     all_transcript_vu_fc a RIGHT  JOIN  bsb_blood_report_vu_fc b
                           ON       a.experiment = b.experiment AND a.time = b.time AND a.chip_name = b.chip_name AND a.dose=b.dose AND a.tissue = b.tissue    
                           WHERE    b.chip_name  IN ('", input$pcin1, "')     AND      b.type = '", (input$pcin3), "'
                           AND      b.categories IN ('", (input$pcin4), "', 'normal')    AND      a.tissue IN (", tissue_name2num(input$pcin2), ")
                           AND      b.assay_name IN ('", input$pcin7, "')  AND  b.time IN ", timeinput(input$pcin6), " AND a.annotation IS NOT NULL;")
                   

                   print("######## The following is cp2gQuery")
                   print(cp2gQuery)
                   
                   df_cp2gQuery <- dbGetQuery(pobj, cp2gQuery)
                   print(df_cp2gQuery)
                   
                   print("########## BLOOD_CHEM or HEMATOLOGY?   ############")
                   #print(df_cp2gQuery$cptype)
                   
                   c2gassay_name <- function() {
                     myc2gassay_name = df_cp2gQuery$cpassay_name
                     return(myc2gassay_name[1])
                   }
                   print("########## The assay_name is: ")
                   print(c2gassay_name())
                   
                   print("########### The number of rows of the query is: ")
                   print(nrow(df_cp2gQuery))
                   
                   c2growSelected <- function() {
                     return(nrow(df_cp2gQuery))
                   }
                   
                   cpggene_name <- df_cp2gQuery$ggene_name
                   cptype <- df_cp2gQuery$cptype
                   cpcategories<- as.factor(df_cp2gQuery$cpcategories)
                   cpavg_val <- round(df_cp2gQuery$cpavg_val, 3)
                   cpnormal_range <- df_cp2gQuery$normal_range
                   cpdose <- df_cp2gQuery$cpdose
                   cpassay_name <- df_cp2gQuery$cpassay_name
                   cpexperiment_name <- df_cp2gQuery$cpexperiment_name
                   cpchip <- df_cp2gQuery$cpchip
                   cptime <- df_cp2gQuery$cptime
                   cplog_ratio <- df_cp2gQuery$cplog_ratio
                   cptissue <- df_cp2gQuery$cptissue
                   #cptissue <- as.integer(cptissue)  # Convert floating point cptissue into integer
                   
                   myc2gtissue <- function() {
                     x =''
                     
                     if (cptissue[1] == 8) {
                       x = 'LIVER'
                     } else if (cptissue[1] == 7) {
                       x = 'KIDNEY'
                     } else if (cptissue[1] == 6) {
                       x = 'INTESTINE'
                     } else if (cptissue[1] == 5) {
                       x = 'HEART'
                     } else if (cptissue[1] == 1) {
                       x = 'BONE MARROW'
                     } else if (cptissue[1] == 2) {
                       x = 'BRAIN'
                     } else if (cptissue[1] == 11) {
                       x = 'SPLEEN'
                     } else {
                       x = 'THIGH MUSCLE'
                     }
                       return (x)
                  }  # This function serves for converting tissue id into tissue name.
                   
                   print("*############ These are the Tissue name and ID I have chosen")
                   print(myc2gtissue())
                   print(input$pcin2)  # print the tissue name
                   
                   cpcategories<- as.factor(df_cp2gQuery$cpcategories)
                   
                   inputcategory <- function() {
                        mycat <- input$pcin4
                        return(mycat)
                     }
                   
                   incProgress(0.5, message = "Data Retrieved!  Now Summarizing data ...",
                               detail = "This may take up to 30 seconds to 2 minutes")
                   if(nrow(df_cp2gQuery) > 0)
                   {
                     #cpcategories<- as.factor(df_cp2gQuery$cpcategories)
                     
                     if(inputcategory() =='increase') {
                   
                       df_cp2gQuery %>%
                         mutate(index = 1:nrow(.)) %>%
                         filter(cpcategories != 'decrease') %>% #keep only increase category
                         group_by(ggene_name)%>% #group by gene
                         filter(sum(cpcategories == 'increase') > 1) %>% #filter assays with <=1 increase
                         spread(cpcategories, cplog_ratio) %>% #move log_ratio to two columns: increase and normal
                         dplyr::summarise(count = sum(!is.na(increase)),
                                   count0 = sum(!is.na(normal)),
                                   mean = mean(increase, na.rm = T),
                                   mean0 = mean(normal, na.rm = T),
                                   diff = mean - mean0,
                                   sd = sd(increase, na.rm = T),
                                   sd0 = sd(normal, na.rm = T),
                                   tval = wilcox.test(increase, normal, var.equal = F)$statistic, #use x,y input for t.test instead of formula
                                   pval = wilcox.test(increase, normal, var.equal = F)$p.val) %>% filter(!is.na(pval)) %>% mutate(cpcategory = "increase")  -> output_table
                     }
                     
                     else {
                       
                       df_cp2gQuery %>%
                         mutate(index = 1:nrow(.)) %>%
                         filter(cpcategories != "increase") %>% #keep only decrease category
                         group_by(ggene_name)%>% #group by gene
                         filter(sum(cpcategories == "decrease") > 1) %>% #filter assays with <=1 decrease
                         spread(cpcategories, cplog_ratio) %>% #move log_ratio to two columns: increase and normal
                         dplyr::summarise(count = sum(is.na(decrease)),
                                   count0 = sum(!is.na(normal)),
                                   mean = mean(decrease, na.rm = T),
                                   mean0 = mean(normal, na.rm = T),
                                   diff = mean - mean0,
                                   sd = sd(decrease, na.rm = T),
                                   sd0 = sd(normal, na.rm = T),
                                   tval = wilcox.test(decrease, normal, var.equal = F)$statistic, #use x,y input for t.test instead of formula
                                   pval = wilcox.test(decrease, normal, var.equal = F)$p.val) %>% filter(!is.na(pval)) %>% mutate(cpcategory = "decrease")  -> output_table  
                     }
                     
                     print("This is the output_table")
                     print(output_table)
                     print("Done")
                     
                     cpcategory <- output_table$cpcategory
                     output_table$assay_name <- c2gassay_name()
                     
                     print("THis is the output table after adding the assay_name")
                     print(output_table)
                     
                     mycpcategory <- function(){
                       return(cpcategory[1])   # Only return the first value of category vector
                       #return(input$cpcategories)
                       #return('increase')
                     }
                     
                     print("########### here is the category increase or decrease")
                     print(mycpcategory())
                     #print(cpcategory[1])
                     print("######## The following is the Summary Table #########")
                     
                     c2gsummary <- data.frame(output_table$ggene_name, output_table$assay_name, output_table$cpcategory, output_table$count0, output_table$count, round(output_table$mean0, 4), round(output_table$mean, 4), round(output_table$diff, 4),
                                              round(output_table$sd0, 4), round(output_table$sd, 4), round(output_table$tval, 3), round(output_table$pval, 8))
                     names(c2gsummary) <- c("Gene Probe", "Clinical Assay", "Assay Effect", "Count(0)", "Count(*)", "Avg. Log10(0)", "Avg. Log10(*)", "Log10 DIFF", "SD(0)", "SD(*)", "Tval(*)", "Pval(*)" )
                     
                     
                     print("This is c2gsummary")
                     print(c2gsummary)
                     
                     c2gsummary$pvalcolor4<- ifelse(c2gsummary$`Pval(*)` < 0.05, 2, 1) # add a color code to check if P value > 0.05, make it 2, otherwise, make it 1 
                     
                     
                     

                     print("################# print c2gsummary after adding pvalcolor4 ###############")
                     print(c2gsummary)
                     
                     print("################# print the detail of the d4 #############################")
                     d4 = data.frame("Gene"=cpggene_name, "Clinical Assay" = cpassay_name, "Assay Effect"= cpcategories, "Expression Source" = myc2gtissue(), "Experiment" = cpexperiment_name, "Chip Type" = cpchip, "Dose mg per kg" = cpdose, "Time per day" = cptime, "Avg Value"= cpavg_val, "Range of Normal"= cpnormal_range,"Log10 Ratio" = cplog_ratio)
                     
                     names(d4) <-c ("Gene", "Clinical Assay", "Assay Effect", "Expression Source", "Experiment", "Chip Type", "Dose mg per kg", "Time per day", "Avg Value", "Range of Normal", "Log10 Ratio")
                     print(d4)
                     
                    incProgress(0.4, message = "Formatting output in progress",
                                 detail = "This step just take 3 seconds ...")
                     
                   }
                   else
                   {
                     d4 = NULL
                     c2gsummary = NULL
                   }
                 })  # Progress bar message ended
    return(list(mycp2gdata = d4, mysummarydata4 = c2gsummary, rowCount=c2growSelected(),  
                sqlcategory=substring(input$c2gcomposite, regexpr("[|]", input$c2gcomposite)+2), passc2gtissue=myc2gtissue(), sqltime=input$cptime, sqlchip=input$cpchip, sqlassay_name=c2gassay_name()))
  }
) # clinical pathology to gene function


output$pathology2geneSummaryTable = DT::renderDataTable(
  DT::datatable(
    clin_pathology2gene_table()$mysummarydata4,
    selection = list(mode='single', target = 'cell',
                     selectable = rbind(cbind(1:nrow(clin_pathology2gene_table()$mysummarydata4), rep(13, nrow(clin_pathology2gene_table()$mysummarydata4))),
                                        cbind(1:nrow(clin_pathology2gene_table()$mysummarydata4), rep(1, nrow(clin_pathology2gene_table()$mysummarydata4))))
    ), # selection list
    rownames = TRUE,
    escape = 3,
    
    #selection = list(target = 'cell'),
    
    caption = htmltools::tags$caption(style = 'caption-side: top; text-align: center; color:brown; font-size:150% ;','Table: Summary of Clinical Pathology Gene Expression.'),
    
    extensions = 'Buttons',
    options=list(
      pageLength = 5, info = FALSE,
      lengthMenu = list(c(5, 20, 50, 100, -1), c("5", "20", "50", "100", "All")),
      columnDefs = list(list(className = 'dt-center', targets=c(2,3,4,5,6,7,8,9,10,11, 12), visible=TRUE), list(className = 'dt-center', targets = c(13),  visible = FALSE)),
      order = list(8, 'desc'),
      # dom = 'Bfrtip',
      searchHighlight = TRUE,  # When you typy in the Search Box, it will highlight.
      dom = 'Bliftsp',
      buttons=c('copy', 'csv', 'excel', 'pdf', 'print')
    ),
    
    container = htmltools::withTags(table(class = 'display',
                                          thead(
                                            tr(
                                              th(rowspan=2, ' '),
                                              th(rowspan = 2, 'Gene'),
                                              th(rowspan = 2, 'Clinical Assay'),
                                              th(rowspan = 2, 'Assay Effect', class = "dt-center"),
                                              th(colspan = 9, paste0('Gene Expression Source: ', clin_pathology2gene_table()$passc2gtissue), class = "dt-center"),
                                              tr(
                                                lapply(rep(
                                                  c('# Treatments \u2260 Normal Range', '# Treatments = Normal Range', 'Avg. Log10 Ratio Treatments \u2260 Normal Range', 'Avg. Log10 Ratio Treatments= Normal Range',
                                                    'Log10 Ratio DIFF', 'STD Treatments \u2260 Normal Range', 'STD Treatments = Normal Range', 'T-value', 'P-value'), 1
                                                ), th)
                                              ))))),    # \u2260 is the unicode for ??? sign or symbol
    
  ) %>% formatStyle('Gene Probe', backgroundColor = '#d4efdf')  # Light Green color
  %>% formatStyle(columns = "Pval(*)", valueColumns = "pvalcolor4", backgroundColor = styleEqual(levels = c(1, 2), values = c('white', 'pink'))  )
  
) # Summary table output, clinical pathology to gene option

c2gcountSelection <- function() {
  counting <- clin_pathology2gene_table()$rowCount
  if (counting > 0){
    message = c('There are', counting, 'rows of data retrieved from DrugMatrix')
  }else{
    # message = c('No matching data found in DrugMatrix')
    message = c('No matching data found in DrugMatrix')
  }
  message
} # count for clinical pathology to gene option

output$c2gCountNum = renderText({
  c2gcountSelection()
}) # output the count number, clinical pathology to gene option


sumClinGeneSelected <- function() {
  c2gp <- input$pathology2geneSummaryTable_cells_selected
  ngdf4 <- clin_pathology2gene_table()$mysummarydata4[c2gp]
  ngdf4
} # clinical pathology to gene option

output$clinGeneSummarySelect = renderText({
  sumClinGeneSelected() 
}  ) # output summary table selected cell, clinical pathology to gene option


output$c2gdetailTable = DT::renderDataTable(
  clin_pathology2gene_table()$mycp2gdata%>%filter(Gene%in%sumClinGeneSelected()),
  extensions = 'Buttons',
  options = list(
    pageLength = 10,
    info = FALSE,
    lengthMenu = list(c(10,  -1), c("First 10 rows", "All")),
    dom = 'Bliftsp',
    buttons = c('copy', 'csv', 'excel', 'pdf', 'print')
  ),
  rownames = TRUE,
  caption = htmltools::tags$caption(style = 'caption-side: top; text-align: center; color:brown; font-size:150% ;','Table: Affected Clinical Pathology Detail.')#,
) # Clinical pathology to gene option


dataPlotClinPathToGene <- function() 
{
  validate (
    need(nchar(input$pathology2geneSummaryTable_cells_selected) > 0, "Choose one or more gene probes from the summary table above")
  )
  
  c2gSelectGene = c(sumClinGeneSelected())
  
  print("Print the selected Gene Name")
  print(c2gSelectGene)
  
  plotDataClinPathToGene <- filter(clin_pathology2gene_table()$mycp2gdata, `Gene` %in% c2gSelectGene)
  
  ###### DEBUG
  print("DEBUG: The following data is in the graph.")
  print(plotDataClinPathToGene)
  
  plotDataClinPathToGene$`Assay Effect` <- factor(plotDataClinPathToGene$`Assay Effect`, levels=c("normal", "decrease", "increase"))
  
  if (nrow(plotDataClinPathToGene) > 0) {
    clinPath2genepltpath = ggplot(plotDataClinPathToGene, aes(x=`Assay Effect`, y=`Log10 Ratio`, fill= `Assay Effect`)) + xlab("Clinical Pathology Effect") + ylab("Log10 Ratio") + geom_boxplot() + geom_point() + theme_bw() + labs(fill = "Pathology Effect") + ggtitle("Figure: Box Plot of Clinical Pathology Effects versus Log10 Ratio") + theme(plot.title = element_text(lineheight=.60, hjust = 0.5, color = "brown", size = 20))
    
    clinPath2genepltpath + facet_wrap(~ `Gene`, ncol = 2) + theme(axis.text.x = element_text(face="bold", size=12), axis.text.y = element_text(face="bold", size = 12), 
                                                                  axis.title.x = element_text(color="#993333", size=14, face="bold"),
                                                                  axis.title.y = element_text(color="#993333", size=14, face="bold")                                                                      
    ) 
  }
  else {
    plot(c(0, 1), c(0, 1), ann = F, bty = 'n', type = 'n', xaxt = 'n', yaxt = 'n')
    text(x = 0.5, y = 0.5, "No Data Found", 
         cex = 1.6, col = "black")
  }
} # algorithm of dataPlotClinPathToGene function,  clinical pathology to gene option

output$boxPlotClinPathtoGene <- renderPlot(
  {  
    dataPlotClinPathToGene()
  }
) # Ouput the plot, clinical pathology to gene option 

output$cp2genePlot_download <- downloadHandler(
  filename = function()
  {
    # paste(input$"Clinical_pathology_to_genePlot", 'clinical_pathology_to_gene.png', sep='')
    paste(input$"Clinical_pathology_to_genePlot", paste0('clin_pathology2gene_', Sys.time(), '.png'), sep='')
  },
  content= function(file)
  {
    ggsave(file, plot=dataPlotClinPathToGene(), width=10, height=10.4)
  }
) # Output the graph - Download button, clinical pathology to gene option



###########################################################################################################################
###################################### The Code for Gene to Organ Weight Tab ##############################################
###########################################################################################################################

# choose chip_name from gwin1
f_gwin1 <- reactive({
  filter(gwdat, chip_name %in% input$gwin1)
}) # filter out the "chip_name" information.  the gwin1 is for chip choices.  f_gwin1 is the data frame when you decide to choose a chip.

observeEvent(f_gwin1(), {
  freezeReactiveValue(input, "gwin2")
  print("######### Print f_gwin1()")
  print(f_gwin1())
  choices<-unique(f_gwin1()$organ)
  updateSelectInput(inputId = "gwin2", choices = choices)
})  # when you have the f_gwin1 database with a chip.  You filter in a organ name.

# choose organ name from gwin2
f_gwin1_in2 <- reactive({
  req(input$gwin2)
  filter(f_gwin1(), organ %in% input$gwin2)
})

observeEvent(f_gwin1_in2(), {
  freezeReactiveValue(input, "gwin3")
  print("######### Print f_gwin1_in2()")
  print(f_gwin1_in2())
  choices <- unique(f_gwin1_in2()$status)
  updateSelectInput(inputId = "gwin3", choices = choices, selected = 'increase')
})

# choose status from gwin3
f_gwin1_in2_in3 <-reactive({
  req(input$gwin3)
  filter(f_gwin1_in2(), status %in% input$gwin3)
})

# choose duration from gwin4() 
observeEvent(f_gwin1_in2_in3(), {
  freezeReactiveValue(input, "gwin4")
  print("######### Print f_gwin1_in2_in3()")
  print(f_gwin1_in2_in3())
  choices <- unique(f_gwin1_in2_in3()$duration)
  updateSelectInput(inputId = "gwin4", choices = choices, selected = 5)
})

observeEvent(f_gwin1_in2_in3(), {
  freezeReactiveValue(input, "gwin6")
  print("######### Print f_gwin1_in2_in3()")
  print(f_gwin1_in2_in3())
  choices <- unique(f_gwin1_in2_in3()$signature)
  updateSelectInput(inputId = "gwin6", choices = choices)
})

# choose gene from gwin5
observeEvent(f_gwin1_in2(), {
  freezeReactiveValue(input, "gwin5")
  choices <- unique(gene)
  updateSelectizeInput(inputId = "gwin5", choices = gene, server = TRUE, selected = 'Havcr1 | hepatitis A virus cellular receptor 1 (286934,1387965_at,AF035963_PROBE1)') #this line populates dropdown
})

f_gwin5 <- reactive({
  req(input$gwin5)
  unique(filter(df_pgQuery, gene_name %in% input$gwin5))
})

observeEvent(f_gwin5(), {
  print("######### Print f_gwin5()")
  print(f_gwin5())
})

gene2weight_table = eventReactive(
  input$bu5, 
  {
    {
      input$gwin1               # original g2wchip
      input$gwin2               # original g2wcomposite "organ part"
      input$gwin3               # original g2wcomposite "status part" # this is to choose both the Organ and its weight change direction.
      input$gwin4               # original g2wtime     # there are only 2 options, 3 days and 5 days
      input$gwin5               # original g2wSelectGene
      input$gwin6               # original g2wsignature # this is to choose the expression source in the detail table
      # }
      # {
      withProgress( # Progress bar starts
        message = 'Retrieving affected genes from drugmatrix database in progress',
        detail = 'This may take up to 1 minute ...', value = 0.2, { ##Start progress at 0
          
          
          # For g2wQuery, all the organ/tissue weight change summary are calculated.  However, only the one chosen is displayed using the factor feature.
          # Comparing to g2wdetailQuery, this query does not have organ and signature.
          
          g2wQuery <- paste0("SELECT experiment_name, compound, gene_name, log_ratio, duration, chip_name, dose, dose_unit, organ_rel_wt, control_mean_wt, ratio, status, organ
                                          FROM bsb_organ_body_weight_d5_complete 
                                          WHERE   duration IN ", timeinput(input$gwin4), "
                                          AND   status IN  ('", input$gwin3, "', 'no change')
                                          AND   chip_name IN ('", input$gwin1, "')
                                          AND   gene_name = '", input$gwin5, "'
                                          and           signature IN ('", input$gwin6, "')    ;")
          
          # For g2wdetailQuery, Added the Organ chosen as well as the organ for the gene expression source.  By default, it is 'None'.
          
          g2wdetailQuery = paste0("SELECT organ, experiment_name, compound, gene_name, log_ratio, duration, chip_name, dose, dose_unit, organ_rel_wt, control_mean_wt, ratio, status, signature
                                          FROM          bsb_organ_body_weight_d5_complete
                                          WHERE         chip_name IN ('", input$gwin1, "')
                                          and           status IN ('", input$gwin3, "', 'no change')
                                          and           duration  in ", timeinput(input$gwin4), "
                                          and           gene_name = '", input$gwin5, "'
                                          and           organ IN ('", input$gwin2, "')
                                          and           signature IN ('", input$gwin6, "')   ;" )
          
          
          df_g2wQuery <- dbGetQuery(pobj, g2wQuery)
          print("######## The following is g2wQuery")
          df_g2wQuery
          
          # change 'no change' value into 'nochange' as one word in order to easy manipulate in R
          df_g2wQuery["status"][df_g2wQuery["status"] == "no change"] <- "nochange"  
          
          df_g2wQuery
          
          gene_name <- df_g2wQuery$gene_name
          status <- as.factor(df_g2wQuery$status)

          
          print("######## The following is g2wdetailQuery")
          print(g2wdetailQuery)
          
          df_g2wdetailQuery <- dbGetQuery(pobj, g2wdetailQuery)
          print("############## This is the g2wdetailQuery Output ####################")
          head(df_g2wdetailQuery)
          
          
          if(nrow(df_g2wdetailQuery)>0)
          {
            
            g2wtempdetail <- data.frame(
              df_g2wdetailQuery$organ,
              df_g2wdetailQuery$status,
              df_g2wdetailQuery$signature,
              df_g2wdetailQuery$gene_name,
              gsub("-2", "", df_g2wdetailQuery$chip_name),
              df_g2wdetailQuery$experiment_name,
              df_g2wdetailQuery$compound,
              df_g2wdetailQuery$log_ratio,
              df_g2wdetailQuery$duration,
              df_g2wdetailQuery$dose,
              df_g2wdetailQuery$dose_unit,
              df_g2wdetailQuery$organ_rel_wt,
              df_g2wdetailQuery$control_mean_wt,
              df_g2wdetailQuery$ratio
            )
            
            g2wdetailTotal <- g2wtempdetail
            
            names(g2wdetailTotal) <- c("Organ",  "Organ Effect", "Expression Source" , "Gene",  "Chip", "Experiment", "Compound",  "Log_ratio",  "Duration", "Dose", "Unit",  "Organ Rel Weight", "Control Mean Weight", "Ratio")
            
          }
          else
          {
            g2wdetailTotal = NULL
          }
          
          
          print("########### The number of rows of the summary query is: ")
          nrow(df_g2wQuery)
          
          # print("########### The number of rows of the detail query is: ")
          # head(nrow(df_g2wdetailQuery))
          
          
          g2wrowSelected <- function() {
            return(nrow(df_g2wQuery))
          }
          
          # g2wdetailrowSelected <- function() {
          #   return(nrow(df_g2wdetailQuery))
          # }
          
          g2wsignatureInput <- function() {
            return(df_g2wdetailQuery$signature[1])
          }
          
          # compound <- df_g2wQuery$compound
          organ <- df_g2wQuery$organ
          # organ_ctl <- df_g2wQuery_ctl$organ
          status <- df_g2wdetailQuery$status
          
          incProgress(0.5, message = "Data Retrieved!  Now Summarizing data ...",
                      detail = "This may take up to 30 seconds to 1 minute")
          if(nrow(df_g2wQuery) > 0)
          {
            
            if (input$gwin3=='increase') { 
              df_g2wQuery %>%
                filter(status != "decrease") %>% 
                group_by(organ)%>% 
                filter(sum(status == "increase") > 1) %>% #filter status with <=1 increase
                spread(status, log_ratio) %>% #move log_ratio to two columns: increase and nochange
                summarise(
                  count = sum(!is.na(increase)),
                  count0 = sum(!is.na(nochange)),
                  mean = mean(increase, na.rm = T),
                  mean0 = mean(nochange, na.rm = T),
                  diff = mean - mean0,
                  sd = sd(increase, na.rm = T),
                  sd0 = sd(nochange, na.rm = T),
                  tval = wilcox.test(increase, nochange, var.equal = F)$statistic, #use x,y input for t.test instead of formula
                  pval = wilcox.test(increase, nochange, var.equal = F)$p.val) %>% filter(!is.na(pval)) %>%  
                mutate(status = "increase") ->
                outputd5
            } else {
              df_g2wQuery %>%
                filter(status != "increase") %>% 
                group_by(organ)%>% 
                filter(sum(status == "decrease") > 1) %>% #filter status with <=1 increase
                spread(status, log_ratio) %>% #move log_ratio to two columns: increase and nochange
                summarise(
                  count = sum(!is.na(decrease)),
                  count0 = sum(!is.na(nochange)),
                  mean = mean(decrease, na.rm = T),
                  mean0 = mean(nochange, na.rm = T),
                  diff = mean - mean0,
                  sd = sd(decrease, na.rm = T),
                  sd0 = sd(nochange, na.rm = T),
                  tval = wilcox.test(decrease, nochange, var.equal = F)$statistic, #use x,y input for t.test instead of formula
                  pval = wilcox.test(decrease, nochange, var.equal = F)$p.val) %>% filter(!is.na(pval)) %>%  
                mutate(status = "decrease") ->
                outputd5
            }
            
            
            outputd5$status = input$gwin3
            outputd5$gene = input$gwin5
            
            print("####### This is outputd5. I want to see what the output6 looks like ######################")
            print(outputd5)
            
            tempoutputd5 <- data.frame(outputd5[,1], outputd5[,12], outputd5[,2], outputd5[,3], round(outputd5[,4], 3), round(outputd5[,5], 3), round(outputd5[,6], 3), 
                                       round(outputd5[,7], 3), round(outputd5[,8], 3), round(outputd5[,9], 3),  round(outputd5[,10], 8), outputd5[,11])
            
            d5 <- tempoutputd5
            
            conditional_data_frame <- d5
            print("########### The following is the conditional_data_frame")
            print(conditional_data_frame)
            
            names(d5) <- c("Organ",  "Organ Effect", "Gene Probe", "Count(*)", "Count(0)", "Avg Log10(*)", "Avg Log10(0)", "Log10 Ratio DIFF", "SD(*)", "SD(0)", "T-value", "P-value")
            
            print("############# The is the d5 ##################")
            print(d5)
            
            ####### Finalizing d5 by adding status column which then called Organ Effect
            
            tempd5 <- data.frame(d5[, 1], d5[, 12], d5[, 2], d5[, 3], d5[, 4], d5[, 5], d5[, 6] , d5[, 7], d5[, 8], d5[, 9], d5[, 10], d5[, 11])  
            d5 <- tempd5
            
            
            names(d5) <- c("Organ", "Organ Effect", "Gene Probe", "Count(*)", "Count(0)", "Avg Log(*)", "Avg Log10(0)", "Log10 Ratio DIFF", "SD(*)", "SD(0)", "T-value", "P-value")
            d5$pvalcolor5 <- ifelse(d5$`P-value` < 0.05, 2, 1) # add a color code to check if P value > 0.05, make it 2, otherwise, make it 1
            
            print ("This is the new d5")
            print(d5)
            
            conditional_data_frame <- d5
            
            if (str_trim(input$gwin2)== 'HEART') {
              d5 = conditional_data_frame[1, ]   ######### This print the first row of conditional data frame.
            } else if (str_trim(input$gwin2)== 'KIDNEY') {
              d5 = conditional_data_frame[2, ]   ######### This print the first row of conditional data frame.
            } else if (str_trim(input$gwin2)== 'LIVER') {
              d5 = conditional_data_frame[3, ]   ######### This print the first row of conditional data frame.
            } else if (str_trim(input$gwin2)== 'LUNG'){
              d5 = conditional_data_frame[4, ]   ######### This print the first row of conditional data frame.
            } else if (str_trim(input$gwin2)== 'SPLEEN') {
              d5 = conditional_data_frame[5, ]   ######### This print the first row of conditional data frame.
            } else {
              d5 = conditional_data_frame[6, ]   ######### This print the first row of conditional data frame.
            }
            
            
            
            incProgress(0.4, message = "Formatting output in progress",
                        detail = "This step just take 3 seconds ...")
            
          }
          else
          {
            d5 = NULL
          }
        })  # Progress bar message ended
      return(list(myg2wdata = d5, g2wrowCount=g2wrowSelected(), myg2wdataDetail = g2wdetailTotal, g2wsignatureChosen=g2wsignatureInput() ))
    }
  }
) # Gene to Organ Weight function

g2wcountSelection <- function() {
  counting <- gene2weight_table()$g2wrowCount
  if (counting >0 ) {
    message = c('There are', counting, 'rows of data retrieved from DrugMatrix')
  }else{
    # message = c('No matching data found in DrugMatrix')
    message = c('No matching data found in DrugMatrix')
  }
  message
} # count for Gene to Organ Weight option

output$g2wCountNum = renderText({
  g2wcountSelection()
}) # output the count number, Gene to Organ Weight option

g2wdataframefunction <- function() {
  return (gene2weight_table()$myg2wdata)
}

output$g2weightSummaryTable = DT::renderDataTable(
  datatable(
    g2wdataframefunction(), # %>% formatStyle('Gene Probe', backgroundColor = 'lightblue'),
    rownames = TRUE,
    escape=3,
    caption = htmltools::tags$caption(style = 'caption-side: top; text-align: center; color:brown; font-size:150% ;','Table: Summary of Genes to Organ Weight Change.'),
    #selection = list(target = 'cell'),
    extensions = 'Buttons',
    options = list(
      pageLength = 5, info = FALSE,
      lengthMenu = list(c(5, 20, 50, 100, -1), c("5", "20", "50", "100", "All")),
      columnDefs = list(list(className = 'dt-center', targets = c(4:12)), list(targets=c(13), visible=FALSE)),
      order = list(8, 'desc'),
      dom = 'Bliftsp',
      searchHighlight = TRUE,  # When you typy in the Search Box, it will highlight.
      buttons=c('copy', 'csv', 'excel', 'pdf', 'print')  
    ),      
    container = htmltools::withTags(table(class = 'display',
                                          thead(
                                            tr(
                                              th(rowspan=2, ' '),
                                              th(rowspan = 2, 'Organ'),
                                              th(rowspan = 2, 'Organ Effect'),
                                              th(rowspan = 2, 'Gene Probe', class = "dt-center"),
                                              th(colspan = 9, paste0('Gene Expression Source: ', gene2weight_table()$g2wsignatureChosen), class = "dt-center"),
                                              tr(
                                                lapply(rep(
                                                  c('# Treatments \u2260 Normal Range', '# Treatments Equal = Range', 'Avg. Log10 Ratio \u2260 Normal Range', 'Avg. Log10 Ratio = Normal Range',
                                                    'Log10 Ratio DIFF', 'STD Treatments \u2260 Normal Range', 'STD Treatments = Normal Range', 'T-value', 'P-value'), 1
                                                ), th)
                                              ))))),
    
  ) %>% formatStyle('Gene Probe', backgroundColor = '#d4efdf')  # Light Green color
  %>% formatStyle(columns = "P-value", valueColumns = "pvalcolor5", backgroundColor = styleEqual(levels = c(1, 2), values = c('white', 'pink'))  )
) # Summary table output, Genes to Organ Weight Changes option


######################################### Output Genes to Organ Weight Change Detail information ##################################################
output$g2wdetailTable = DT::renderDataTable(
  DT::datatable(
    gene2weight_table()$myg2wdataDetail,
    
    rownames = TRUE,
    escape = 3,
    
    selection = list(target = 'cell'),
    
    caption = htmltools::tags$caption(style = 'caption-side: top; text-align: center; color:brown; font-size:150% ;','Table: Detail Information of Genes of Interest to Organ Weight Changes.'),
    
    # caption = htmltools::tags$caption(style = 'caption-side: top; text-align: center; color:blue; font-size:100% ;','To get the affect details and the boxplots, choose one or more gene probes of your interest.'),
    
    extensions = 'Buttons',
    options=list(
      pageLength = 10, info = FALSE,
      lengthMenu = list(c(10, 25, 100, 1000, -1), c("10", "25", "100", "1000", "All")),
      columnDefs = list(list(className = 'dt-center', targets = c(8:14))),
      order = list(8, 'desc'),
      # dom = 'Bfrtip',
      dom = 'Bliftsp',
      searchHighlight = TRUE,  # When you typy in the Search Box, it will highlight.
      buttons=c('copy', 'csv', 'excel', 'pdf', 'print')
    )
  )) # detail table output, gene to organ weight option


dataPlotGeneToWeight <- function() 
{
  plotDataGene2Weight <- gene2weight_table()$myg2wdataDetail
  
  print("################This is the plotDataGene2Weight###############")
  print(plotDataGene2Weight)
  
  drawOrganEffect <-plotDataGene2Weight$`Organ Effect`
  drawOrganLogratio <-plotDataGene2Weight$`Log_ratio`
  
  ###### DEBUG
  print("DEBUG: The following data is in the graph.")
  print(plotDataGene2Weight)
  
  plotDataGene2Weight$`Organ Effect` <- factor(plotDataGene2Weight$`Organ Effect`, levels=c("no change", "decrease", "increase"))
  
  if (nrow(plotDataGene2Weight) > 0) {
    gene2weightpltpath = ggplot(plotDataGene2Weight, aes(x=`Organ Effect`, y=drawOrganLogratio, fill= `Organ Effect`)) + xlab("Organ Weight Change Effect") + ylab("Log10 Ratio") + geom_boxplot() + geom_point() + theme_bw() + labs(fill = "Organ Effect") + ggtitle("Figure: Box Plot of Organ Weight Change versus Log10 Ratio") + theme(plot.title = element_text(lineheight=.60, hjust = 0.5, color = "brown", size = 20))
    
    gene2weightpltpath + facet_wrap(~ `Gene`, ncol = 2) + theme(axis.text.x = element_text(face="bold", size=12), axis.text.y = element_text(face="bold", size = 12), 
                                                                axis.title.x = element_text(color="#993333", size=14, face="bold"),
                                                                axis.title.y = element_text(color="#993333", size=14, face="bold")                                                                      
    ) 
  }
  else {
    plot(c(0, 1), c(0, 1), ann = F, bty = 'n', type = 'n', xaxt = 'n', yaxt = 'n')
    text(x = 0.5, y = 0.5, "No Data Found", 
         cex = 1.6, col = "black")
  }
} # algorithm of dataPlotGeneToWeight function,  weight change to gene option

output$boxPlotGene2Weight <- renderPlot(
  {  
    dataPlotGeneToWeight()
  }
) # Ouput the plot, Gene to weight option 

output$gene2weightPlot_download <- downloadHandler(
  filename = function()
  {
    # paste(input$"Gene_to_weightPlot", 'gene_to_organ_weight.png', sep='')
    paste(input$"Gene_to_weightPlot", paste0('gene2organ_weight_', Sys.time(), '.png'), sep='')
  },
  content= function(file)
  {
    ggsave(file, plot=dataPlotGeneToWeight(), width=10, height=10.4)
  }
) # Output the graph - Download button, organ gene change to weight option


###########################################################################################################################
############################### The Code for Organ Weight Change to Genes Tab #############################################
###########################################################################################################################


# choose chip_name from gwin1
f_wgin1 <- reactive({
  filter(gwdat, chip_name %in% input$wgin1)
}) # filter out the "chip_name" information.  the gwin1 is for chip choices.  f_gwin1 is the data frame when you decide to choose a chip.

observeEvent(f_wgin1(), {
  freezeReactiveValue(input, "wgin2")
  print("######### Print f_wgin1()")
  print(f_wgin1())
  choices<-unique(f_wgin1()$organ)
  updateSelectInput(inputId = "wgin2", choices = choices)
})  # when you have the f_gwin1 database with a chip.  You filter in a organ name.

# choose organ name from wgin2
f_wgin1_in2 <- reactive({
  req(input$wgin2)
  filter(f_wgin1(), organ %in% input$wgin2)
})

observeEvent(f_wgin1_in2(), {
  freezeReactiveValue(input, "wgin3")
  print("######### Print f_wgin1_in2()")
  print(f_wgin1_in2())
  choices <- unique(f_wgin1_in2()$status)
  updateSelectInput(inputId = "wgin3", choices = choices, selected = 'increase')
})

# choose status from wgin3
f_wgin1_in2_in3 <-reactive({
  req(input$wgin3)
  filter(f_wgin1_in2(), status %in% input$wgin3)
})

# choose duration from wgin4() 
observeEvent(f_wgin1_in2_in3(), {
  freezeReactiveValue(input, "wgin4")
  print("######### Print f_wgin1_in2_in3()")
  print(f_wgin1_in2_in3())
  choices <- unique(f_wgin1_in2_in3()$duration)
  updateSelectInput(inputId = "wgin4", choices = choices, selected = 5)
})

observeEvent(f_wgin1_in2_in3(), {
  freezeReactiveValue(input, "wgin6")
  print("######### Print f_wgin1_in2_in3()")
  print(f_wgin1_in2_in3())
  choices <- unique(f_wgin1_in2_in3()$signature)
  updateSelectInput(inputId = "wgin6", choices = choices)
})

# choose gene from gwin5
# observeEvent(f_gwin1_in2(), {
#   freezeReactiveValue(input, "gwin5")
#   choices <- unique(gene)
#   updateSelectizeInput(inputId = "gwin5", choices = gene, server = TRUE, selected = 'Havcr1 | hepatitis A virus cellular receptor 1 (286934,1387965_at,AF035963_PROBE1)') #this line populates dropdown
# })
# 
# f_gwin5 <- reactive({
#   req(input$gwin5)
#   unique(filter(df_pgQuery, gene_name %in% input$gwin5))
# })

# observeEvent(f_gwin5(), {
#   print("######### Print f_gwin5()")
#   print(f_gwin5())
# })


###################### The following is for Organ Weight to Genes Data Manipulation #########################################################

weight2gene_table = eventReactive(
  input$bu6,
  {
    input$wgin1                # original w2gchip
    input$wgin2                # original w2gcomposite organ part # this is to choose both the Organ and its weight change direction.
    input$wgin3                # original w2gcomposite status part # this is to choose both the Organ and its weight change direction.
    input$wgin4                # original w2gtime there are only 2 options, 3 days and 5 days
    input$wgin6                # original w2gsignature # this is to choose the expression source in the detail table
    # },
    # {
    withProgress(message = 'Retrieving associated genes from drugmatrix database in progress',
                 detail = 'This may take up to 2 - 3 minutes ...', value = 0.2, { ##Start progress at 0
                   
                   w2gQuery <- paste0("SELECT distinct experiment_name, compound, gene_name, avg(log_ratio) log_ratio, duration, chip_name, dose, dose_unit, organ_rel_wt, control_mean_wt, ratio, status, organ
                                        FROM bsb_organ_body_weight_d5_complete 
                                        WHERE   organ IN ('", input$wgin2, "')
                                        AND   duration IN ", timeinput(input$wgin4), "
                                        AND   status IN ('", input$wgin3, "', 'no change')
                                        AND   chip_name IN ('", input$wgin1, "')
                                        AND   signature IN ('", input$wgin6, "')
                                        AND  gene_name not LIKE '_|_'
                                        GROUP BY experiment_name, compound, gene_name, duration, chip_name, dose, dose_unit, organ_rel_wt, control_mean_wt, ratio, status, organ
                                        ;")
                   
                   print("######## The following is w2gQuery")
                   print(w2gQuery)
                   
                   df_w2gQuery <- dbGetQuery(pobj, w2gQuery)
                   head(df_w2gQuery)
                   
                   df_w2gdetail <- df_w2gQuery
                   
                   gene_name <- df_w2gQuery$gene_name ## suspicious step
                   organ<- df_w2gQuery$organ          ## suspicious step
                   
                   print("########### The number of rows of the query is: ")
                   head(nrow(df_w2gQuery))
                   
                   status <- input$wgin3
                   
                   myw2gstatus <- function() {
                     return(status[1])
                   }
                   
                   # status <- as.factor(df_g2wQuery$status)
                   
                   df_w2gQuery["status"][df_w2gQuery["status"] == "no change"] <- "nochange"  # change 'no change' value into 'nochange' as one word
                   
                   
                   incProgress(0.5, message = "Data Retrieved!  Now Summarizing data ...",
                               detail = "This may take up to 30 seconds to 1 minute")
                   if(nrow(df_w2gQuery) > 0)
                   {
                     if (input$wgin3=='increase') { 
                       df_w2gQuery %>%
                         filter(status != "decrease") %>% 
                         group_by(gene_name)%>% 
                         filter(sum(status == "increase") > 1) %>% #filter status with <=1 increase
                         spread(status, log_ratio) %>% #move log_ratio to two columns: increase and nochange
                         summarise(
                           count = sum(!is.na(increase)),
                           count0 = sum(!is.na(nochange)),
                           mean = mean(increase, na.rm = T),
                           mean0 = mean(nochange, na.rm = T),
                           diff = mean - mean0,
                           sd = sd(increase, na.rm = T),
                           sd0 = sd(nochange, na.rm = T),
                           tval = wilcox.test(increase, nochange, var.equal = F)$statistic, #use x,y input for t.test instead of formula
                           pval = wilcox.test(increase, nochange, var.equal = F)$p.val) %>% filter(!is.na(pval)) %>%  
                         mutate(status = "increase") ->
                         w2gsummary
                     } else {
                       df_w2gQuery %>%
                         filter(status != "increase") %>% 
                         group_by(gene_name)%>% 
                         filter(sum(status == "decrease") > 1) %>% #filter status with <=1 decrease
                         spread(status, log_ratio) %>% #move log_ratio to two columns: decrease and nochange
                         summarise(
                           count = sum(!is.na(decrease)),
                           count0 = sum(!is.na(nochange)),
                           mean = mean(decrease, na.rm = T),
                           mean0 = mean(nochange, na.rm = T),
                           diff = mean - mean0,
                           sd = sd(decrease, na.rm = T),
                           sd0 = sd(nochange, na.rm = T),
                           tval = wilcox.test(decrease, nochange, var.equal = F)$statistic, #use x,y input for t.test instead of formula
                           pval = wilcox.test(decrease, nochange, var.equal = F)$p.val) %>% filter(!is.na(pval)) %>%  
                         mutate(status = "decrease") ->
                         w2gsummary  
                     }
                     
                     print("####  Let's get the w2gsummary here #############")
                     print(w2gsummary)
                     
                     w2gsummary$organ <- input$wgin2  # To get the organ

                     w2growSelected <- function() {
                       return(nrow(df_w2gQuery))
                     }
                     
                     w2gsignatureInput <- function() {
                       return(input$wgin6)
                     }
                     
                     d6 <- w2gsummary
                     
                     print("################# This is the FIRST d6 ##################")
                     print(d6)
                     
                     tempd6 <- data.frame(d6$organ, d6$status, d6$gene_name, d6$count, d6$count0, round(d6$mean, 3), round(d6$mean0, 3), round(d6$diff, 3), round(d6$sd, 3), round(d6$sd0, 3), round(d6$tval, 3), round(d6$pval, 8))
                     
                     d6 <- tempd6 
                     
                     incProgress(0.4, message = "Formatting output in progress",
                                 detail = "This step just take 3 seconds ...")
                     
                     print("#################### This is the Second Named d6 in right order #######################")
                     
                     
                     names(d6) <- c("Organ", "Organ Effect", "Gene Probe", "Count(*)", "Count(0)", "Avg Log10(*)", "Avg Log10(0)", "Log10 Ratio DIFF", "SD(*)", "SD(0)", "Tval(*)", "Pval(*)")
                     
                     d6$pvalcolor6 <- ifelse(d6$`Pval(*)` < 0.05, 2, 1) # add a color code to check if P value > 0.05, make it 2, otherwise, make it 1
                     
                     
                     print("################# This is the last d6 ##################")
                     
                     print(d6)
                   }
                   else
                   {
                     d6 = NULL
                   }
                 })  # Progress bar message ended
    return(list(myw2gdata = d6, w2growCount=w2growSelected(), w2gsqlchip=input$wgin1, w2gsqltime=timeinput(input$wgin4), w2gsqltissue=str_trim(input$wgin2), w2gsqlcategory=input$wgin3, 
                outputw2gstatus = myw2gstatus(),   #w2gdetail= df_w2gdetail
                w2gsqlsignature = input$wgin6, w2gsignatureChosen=w2gsignatureInput()))
  }
  
) # Organ Weight to gene function


############################## Tab 6 - Copy ################################

w2gchipcolumn <- function() {
  mychip<- weight2gene_table()$w2gsqlchip
  mychip
}

output$ChipChosen = renderText({
  w2gchipcolumn()
}) # output the type chosen, organ weight to gene option

w2gtimecolumn <- function() {
  mytime<- weight2gene_table()$w2gsqltime
  mytime
}

output$TimeChosen = renderText({
  w2gtimecolumn()
}) # output the type chosen, organ weight to gene option

w2gtissuecolumn <- function() {
  mytissue<- weight2gene_table()$w2gsqltissue
  mytissue
}

w2gsignaturecolumn <- function() {
  mysignature <- weight2gene_table()$w2gsqlsignature
  mysignature
}

output$TypeChosen = renderText({
  w2gtissuecolumn()
}) # output the type chosen, organ weight to gene option


w2gcategorycolumn <- function() {
  mycategory<- weight2gene_table()$w2gsqlcategory
  mycategory
}

output$CategoryChosen = renderText({
  w2gcategorycolumn()
}) # output the category chosen, organ weight to gene option


w2gcountSelection <- function() {
  counting <- weight2gene_table()$w2growCount
  if (counting >0 ) {
    message = c('There are', counting, 'rows of data retrieved from DrugMatrix')
  }else{
    message = c('No matching data found in DrugMatrix')
  }
  message
} # count for Organ Weight to gene option

output$w2gCountNum = renderText({
  w2gcountSelection()
}) # output the count number, Organ Weight to gene option


w2gcountDetail <- function() {
  w2gcountingDetail <-w2gDetail()$w2gdetailrowCount
  if (w2gcountingDetail > 0) {
    message = c('There are', w2gcountingDetail, 'rows of experiment information in detail')
  }else{
    # message = c('No matching data found')
    message = c('Click the Cell that contains Gene Probe Information.  You clicked: ')
  }
} # count for detail information and the informative message in Organ Weight to gene


output$w2gDetailcount = renderText({
  w2gcountDetail()   
}) # output the number of detail experiment when a specific gene probe is selected in Organ Weight to gene


output$w2geneSummaryTable = DT::renderDataTable(
  DT::datatable(
    
    weight2gene_table()$myw2gdata, # %>% formatStyle('Gene Probe', backgroundColor = 'lightblue'),
    rownames = TRUE,
    escape = 3,
    
    selection = list(mode='single', target = 'cell',
                     selectable = rbind(cbind(3:nrow(weight2gene_table()$myw2gdata), rep(13, nrow(weight2gene_table()$myw2gdata))),
                                        cbind(3:nrow(weight2gene_table()$myw2gdata), rep(3, nrow(weight2gene_table()$myw2gdata))))
    ), # selection list
    
    # selection = list(mode = 'single', target = 'cell'),
    caption = htmltools::tags$caption(style = 'caption-side: top; text-align: center; color:brown; font-size:150% ;','Table: Summary of Organ Weight Change to Gene Expression.'),
    
    extensions = 'Buttons', 
    options=list(
      pageLength = 5, info = FALSE,
      lengthMenu = list(c(5, 20, 50, 100, -1), c("5", "20", "50", "100", "All")),
      columnDefs = list(list(className = 'dt-center', targets = c(4:12)), list(targets= c(13), visible=FALSE)),
      order = list(8, 'desc'),
      # dom = 'Bfrtip',
      dom = 'Bliftsp',
      searchHighlight = TRUE,  # When you typy in the Search Box, it will highlight.
      buttons=c('copy', 'csv', 'excel', 'pdf', 'print')
    ),
    
    container = htmltools::withTags(table(class = 'display',
                                          thead(
                                            tr(
                                              th(rowspan=2, ' '),
                                              th(rowspan = 2, 'Organ'),
                                              th(rowspan = 2, 'Organ Effect'),
                                              th(rowspan = 2, 'Gene Probe', class = "dt-center"),
                                              th(colspan = 9, paste0('Gene Expression Source: ', weight2gene_table()$w2gsignatureChosen), class = "dt-center"),
                                              tr(
                                                lapply(rep(
                                                  c('# Treatments \u2260 Normal Range', '# Treatments = Normal Range', 'Avg. Log10 Ratio \u2260 Normal Range', 'Avg. Log10 Ratio = Normal Range',
                                                    'Log10 Ratio DIFF', 'STD Treatments \u2260 Normal Range', 'STD Treatments = Normal Range', 'T-value', 'P-value'), 1
                                                ), th)
                                              ))))),
  ) %>% formatStyle('Gene Probe', backgroundColor = '#d4efdf')  # Light Green color
  %>% formatStyle(columns = "Pval(*)", valueColumns = "pvalcolor6", backgroundColor = styleEqual(levels = c(1, 2), values = c('white', 'pink'))  )
) # Summary table output, Organ Weight to gene option

sumw2gGeneSelected <- function() {
  w2gp <- input$w2geneSummaryTable_cells_selected
  ngdf5 <- weight2gene_table()$myw2gdata[w2gp]
  ngdf5
} # organ weight to gene option

output$w2gGeneSummarySelect = renderText({
  sumw2gGeneSelected() 
}  ) # output summary table selected cell, organ weight change to gene option

######################################### Output Organ Weight Change to Genes Detail information ##################################################
output$w2gdetailTable = DT::renderDataTable(
  DT::datatable(
    w2gDetail()$myw2gdataDetail,
    rownames = TRUE,
    escape = 3,
    
    selection = list(target = 'cell'),
    
    caption = htmltools::tags$caption(style = 'caption-side: top; text-align: center; color:brown; font-size:150% ;','Table: Detail Information of Organ Weight to the Gene(s) of Interest.'),
    
    # caption = htmltools::tags$caption(style = 'caption-side: top; text-align: center; color:blue; font-size:100% ;','To get the affect details and the boxplots, choose one or more gene probes of your interest.'),
    
    extensions = 'Buttons',
    options=list(
      pageLength = 10, info = FALSE,
      lengthMenu = list(c(10, 25, 100, 1000, -1), c("10", "25", "100", "1000", "All")),
      columnDefs = list(list(className = 'dt-center', targets = c(8:14))),
      order = list(8, 'desc'),
      # dom = 'Bfrtip',
      dom = 'Bliftsp',
      searchHighlight = TRUE,  # When you typy in the Search Box, it will highlight.
      buttons=c('copy', 'csv', 'excel', 'pdf', 'print')
    )
  )) # detail table output, organ weight to gene option


######################################### Output Organ Weight Change to Genes Detail information ##################################################


dataPlotWeightToGene <- function() 
{
  validate (
    need(nchar(input$w2geneSummaryTable_cells_selected) > 0, "Choose one or more gene probes from the summary table above")
  )
  
  w2gSelectGene = c(sumw2gGeneSelected())
  
  print("Print the selected Gene Name")
  print(w2gSelectGene)
  
  plotDataWeight2Gene <- filter(w2gDetail()$myw2gdataDetail, `Gene` %in% w2gSelectGene)
  
  ###### DEBUG
  print("DEBUG: The following data is in the graph.")
  print(plotDataWeight2Gene)
  
  plotDataWeight2Gene$`Organ Effect` <- factor(plotDataWeight2Gene$`Organ Effect`, levels=c("no change", "decrease", "increase"))
  
  if (nrow(plotDataWeight2Gene) > 0) {
    weight2genepltpath = ggplot(plotDataWeight2Gene, aes(x=`Organ Effect`, y=`Log_ratio`, fill= `Organ Effect`)) + xlab("Organ Weight Change Effect") + ylab("Log10 Ratio") + geom_boxplot() + geom_point() + theme_bw() + labs(fill = "Organ Effect") + ggtitle("Figure: Box Plot of Organ Weight Change versus Log10 Ratio") + theme(plot.title = element_text(lineheight=.60, hjust = 0.5, color = "brown", size = 20))
    
    weight2genepltpath + facet_wrap(~ `Gene`, ncol = 2) + theme(axis.text.x = element_text(face="bold", size=12), axis.text.y = element_text(face="bold", size = 12), 
                                                                axis.title.x = element_text(color="#993333", size=14, face="bold"),
                                                                axis.title.y = element_text(color="#993333", size=14, face="bold")                                                                      
    ) 
  }
  else {
    plot(c(0, 1), c(0, 1), ann = F, bty = 'n', type = 'n', xaxt = 'n', yaxt = 'n')
    text(x = 0.5, y = 0.5, "No Data Found", 
         cex = 1.6, col = "black")
  }
} # algorithm of dataPlotWeightToGene function,  weight change to gene option


output$boxPlotWeight2Gene <- renderPlot(
  {  
    dataPlotWeightToGene()
  }
) # Ouput the plot, Weight to gene option 


output$w2genePlot_download <- downloadHandler(
  filename = function()
  {
    paste(input$"Weight_to_genePlot", 'organ_weight_to_gene.png', sep='')
    paste(input$"Weight_to_genePlot", paste0('organ_weight2gene_', Sys.time(), '.png'), sep='')
  },
  content= function(file)
  {
    ggsave(file, plot=dataPlotWeightToGene(), width=10, height=10.4)
  }
) # Output the graph - Download button, organ weight change to gene option

#################################################################################  


########################################### Output of Organ Weight to Genes Table Summary/Stat #############################################
w2gDetail <- function()
{
  validate (
    need(nchar(input$w2geneSummaryTable_cells_selected) > 0, "Choose one or more genes from the Statistical Anaylysis of Clinical Pathology to Gene Table above")
  )
  
  withProgress(message = 'Retrieving the detail information of the genes from drugmatrix database',
               detail = 'This may take up to 30 seconds ...', value = 0.2, { ##Start progress at 0
                 
                 
                 
                 w2gSelectGene = paste0(sprintf("'%s'", gsub("'", "`", (sumw2gGeneSelected()))), collapse = ", ")   ########### Need to work on
                 
                 w2gdetailQuery = paste0("SELECT organ, status, signature, experiment_name, compound, gene_name, log_ratio, duration, chip_name, dose, dose_unit, organ_rel_wt, control_mean_wt, ratio
                         FROM          bsb_organ_body_weight_d5_complete
                         WHERE         chip_name IN ('", w2gchipcolumn(), "')
                         AND           organ = '", w2gtissuecolumn(), "'
                         and           status in ('", w2gcategorycolumn(), "', 'no change')
                         and           duration  in ", w2gtimecolumn(), "
                         and           gene_name in (", gsub("`", "''", w2gSelectGene), ")
                         and           signature IN ('", w2gsignaturecolumn(), "')
                        ;" )
                 
                 print(w2gdetailQuery)
                 df_w2gdetailQuery <- dbGetQuery(pobj, w2gdetailQuery)
                 
                 print(df_w2gdetailQuery)
                 
                 w2gdetailrowSelected <- function() {
                   return(nrow(df_w2gdetailQuery))
                 }
                 print('################# This is the selected detail rows')
                 print(w2gdetailrowSelected())
                 
                 if(nrow(df_w2gdetailQuery)>0)
                 {
                   
                   w2gtempdetail <- data.frame(df_w2gdetailQuery$organ, df_w2gdetailQuery$status, df_w2gdetailQuery$signature, df_w2gdetailQuery$gene_name,
                                               gsub("-2", "", df_w2gdetailQuery$chip_name),
                                               df_w2gdetailQuery$experiment_name,
                                               df_w2gdetailQuery$compound,
                                               df_w2gdetailQuery$log_ratio,
                                               df_w2gdetailQuery$duration,
                                               df_w2gdetailQuery$dose,
                                               df_w2gdetailQuery$dose_unit,
                                               df_w2gdetailQuery$organ_rel_wt,
                                               df_w2gdetailQuery$control_mean_wt,
                                               df_w2gdetailQuery$ratio
                   )
                   
                   w2gdetailTotal <- w2gtempdetail
                   
                   names(w2gdetailTotal) <- c( "Organ", "Organ Effect", "Expression Source", "Gene",  "Chip", "Experiment", "Compound",  "Log_ratio",  "Duration", "Dose", "Unit",  "Organ Rel Weight", "Control Mean Weight", "Ratio")
                   
                 }
                 else
                 {
                   w2gdetailTotal = NULL
                 }
                 
               })  ######## Progress bar for the second query to get detail table information.
  
  return(list(myw2gdataDetail = w2gdetailTotal, w2gdetailrowCount=w2gdetailrowSelected()))
  
} 
######################################### End of Output Organ Weight to Genes Stat/Summary #################################################


################################### The following is the Toxicological Profile Information Tab #############################################
############################################################################################################################################

dfprofile$durl <- c( paste0('<a href="', dfprofile$dtxlink, '" , target="_blank">', paste0(dfprofile$dtxsid, "</a>")))   # add a column durl to dfprofile dataframe

dfprofile['dtxlink'] <- dfprofile['durl']

output$table = DT::renderDataTable(
 DT::datatable({dfprofile
                if (input$chemical != "All") {
                  dfprofile <- dfprofile[dfprofile$chemical == input$chemical,]
                }
                if (input$casrn != "All") {
                  dfprofile <- dfprofile[dfprofile$casrn == input$casrn,]
                }
                if (input$dtxsid != "All") {
                  dfprofile <- dfprofile[dfprofile$dtxsid == input$dtxsid,]
                }
                if (input$duration != "All") {
                  dfprofile <- dfprofile[dfprofile$duration == input$duration,]
                }
                if (input$dose != "All") {
                  dfprofile <- dfprofile[dfprofile$dose == input$dose,]
                }
                # if (input$dose_unit != "All") {
                #   dfprofile <- dfprofile[dfprofile$dose_unit == input$dose_unit,]
                # }
                if (input$assayname != "All") {
                  dfprofile <- dfprofile[dfprofile$assayname == input$assayname,]
                }
                if (input$tissue != "All") {
                  dfprofile <- dfprofile[dfprofile$tissue == input$tissue,]
                }
                # if (input$measurement != "All") {
                #   dfprofile <- dfprofile[dfprofile$measurement == input$measurement,]
                # }
                if (input$type != "All") {
                  dfprofile <- dfprofile[dfprofile$type == input$type,]
                }
   
                if (input$vehicle != "All") {
                  dfprofile <- dfprofile[dfprofile$vehicle == input$vehicle,]
                }
                if (input$route != "All") {
                  dfprofile <- dfprofile[dfprofile$route == input$route,]
                }
                # if (input$unit != "All") {
                #   dfprofile <- dfprofile[dfprofile$units == input$units,]
                # }
                # if (input$lower_bound_of_normal != "All") {
                #   dfprofile <- dfprofile[dfprofile$lower_bound_of_normal == input$lower_bound_of_normal,]
                # }
                # if (input$upper_bound_of_normal != "All") {
                #   profiledata <- dfprofile[dfprofile$upper_bound_of_normal == input$upper_bound_of_normal,]
                # }
                if (input$call != "All") {
                  dfprofile <- dfprofile[dfprofile$call == input$call,]
                }
                
                dfprofile
  }
   ,
     rownames = FALSE,
     escape = 3,   # This is critical because if you do not set this escape to FALSE or maybe 3?  The link will not show up.
  
     caption = htmltools::tags$caption(style = 'caption-side: top; text-align: center; color:brown; font-size:150% ;','Table: Toxicological Profile of Summary Apical Endpoints Treatment by Group Level.'),

     extensions = 'Buttons',
     options=list(
       pageLength = 10, info = FALSE,
       #lengthMenu = list(c(25, 100, 1000, -1), c("25", "100", "1000", "All")),
       lengthMenu = list(c(10, 25, 500, -1), c("10", "25", "500", "All")),
       columnDefs = list(list(className = 'dt-center', targets = c(4, 5, 6, 7, 11, 14, 15, 17)), list(targets= c(2,17), visible=FALSE)),
       # columnDefs = list(list(className = 'dt-center', targets = c(4:12)), list(targets= c(13), visible=FALSE)),
       order = list(0, 'asc'),
       # dom = 'Bfrtip',
       dom = 'Bliftsp',
       searchHighlight = TRUE,  # When you type in the Search Box, it will highlight.
       buttons=c('copy', 'csv', 'excel', 'pdf', 'print')
     ),
  
  container = htmltools::withTags(table(class = 'display',
                                        thead(
                                          tr(
                                            th(rowspan = 2, 'chemical'),
                                            th(rowspan = 2, 'casrn'),
                                            th(rowspan = 2, 'dtxsid'),
                                            th(rowspan = 2, 'dtxlink'),
                                            #th(rowspan = 2, 'durl'),
                                            th(colspan = 14, 'Experimental Setting and Output', class = "dt-center"),
                                            tr(
                                              lapply(rep(
                                                c('duration', 'dose',
                                                  'dose_unit', 'type', 'vehicle', 'route', 'assayname', 'tissue', 'measurement', 'unit', 'lower_bound_of_normal', 'upper_bound_of_normal', 'call'), 1
                                              ), th)
                                            )))))
                                            
  )
 ) # end of renderTable



############################################################################################################################################
########################################### The following is the Scientific Citation Tab ###################################################
############################################################################################################################################
# if (nchar(df_citation$pubmed ==32)) {
#   df_citation$url <- df_citation$topic
# } else {
#   df_citation$url <- c( paste0('<a href="', df_citation$pubmed, '" , target="_blank">', paste0(df_citation$topic, "</a>")))   # add a column durl to df_citation dataframe
# }

df_citation$url <- c( paste0('<a href="', df_citation$pubmed, '" , target="_blank">', paste0(df_citation$topic, "</a>")))   # add a column durl to df_citation dataframe
df_citation['topic'] <- df_citation['url']
# View(df_citation)


output$citationTable = DT::renderDataTable(
  DT::datatable(df_citation
    ,
  rownames = FALSE,
  escape = 3,   # This is critical because if you do not set this escape to FALSE or maybe 3?  The link will not show up.
  
  caption = htmltools::tags$caption(style = 'caption-side: top; text-align: center; color:brown; font-size:150% ;','Table: DrugMatrix Citations.'),
  
  extensions = 'Buttons',
  options=list(
    pageLength = 15, info = FALSE,
    lengthMenu = list(c(15, 50, 300, -1), c("15", "50", "300", "All")),
    #columnDefs = list(list(className = 'dt-center', targets = c(3, 7)), list(targets= c(6, 8), visible=FALSE)),
    order = list(2, 'asc'),
    # dom = 'Bfrtip',
    dom = 'Bliftsp',
    searchHighlight = TRUE,  # When you type in the Search Box, it will highlight.
    buttons=c('copy', 'csv', 'excel', 'pdf', 'print')
  ),
  
  )
) # end of citationTable



########################### The following is the Individual Chemical Expression and Enrichment Tab #########################################
############################################################################################################################################

# updateSelectizeInput(session, 'selectCompound', choices = compoundlist, server = TRUE, selected = 'FENOFIBRATE') #this line populates dropdown

###########################################################################################################################
################################### The Code for Chemical and Enrichr Tab ###################################################
###########################################################################################################################


# choose chip name from cechip
f_cechip <- reactive({
  filter(chem_enrich_dat, chip_name %in% input$cechip)
}) # filter out the "chip_name" information.  the cechip is for chip choices.  f_cechip is the data frame when you decide to choose a chip.

# Get the tissue name list based on the filtered dataframe containing the chip_name you chosen, e.g. RG230-2
observeEvent(f_cechip(), {
  freezeReactiveValue(input, "cetissue")
  print("######### Print f_cechip()")
  print(f_cechip())
  choices<-unique(f_cechip()$tissue_name)
  updateSelectInput(inputId = "cetissue", choices = choices)
})  # when you have the f_cechip database with a chip_name.  You filter in a tissue_name.

# choose tissue name from cetissue
f_cechip_cetissue <- reactive({
  req(input$cetissue)
  filter(f_cechip(), tissue_name %in% input$cetissue)
})

# Get a compound name list based on the filtered dataframe containing the chip_name AND the tissue chosen
observeEvent(f_cechip_cetissue(), {
  freezeReactiveValue(input, "cecompound")
  print("######### Print f_cechip_cetissue()")
  print(f_cechip_cetissue())
  choices <- unique(f_cechip_cetissue()$compound_name)
  updateSelectInput(inputId = "cecompound", choices = choices, selected = 'FENOFIBRATE')
})

# choose compound_name from cecompound
f_cechip_cetissue_cecompound <-reactive({
  req(input$cecompound)
  filter(f_cechip_cetissue(), compound_name %in% input$cecompound)
})


# Get a time list based on the filtered dataframe containing the chip_name, tisse_name, and the compound_name
observeEvent(f_cechip_cetissue_cecompound(), {
  freezeReactiveValue(input, "cetime")
  print("######### Print f_cechip_cetissue_cecompound()")
  print(f_cechip_cetissue_cecompound())
  choices <- unique(f_cechip_cetissue_cecompound()$time)
  updateSelectInput(inputId = "cetime", choices = choices, selected = 1)
})

# choose time from cetime
f_cechip_cetissue_cecompound_time <-reactive({
  req(input$cetime)
  #if (is.na(input$cetime)) { input$cetime <- 3 }
  #get(input$cetime, "package:datasets", inherits = FALSE)
  filter(f_cechip_cetissue_cecompound(), time %in% input$cetime)
})


# Get a dose list based on the filtered dataframe containing the chip_name, tissue_name, compound_name, and time
observeEvent(f_cechip_cetissue_cecompound_time(), {
  freezeReactiveValue(input, "cedose")
  print("######### Print f_cechip_cetissue_cecompound_time()")
  print(f_cechip_cetissue_cecompound_time())
  choices <- unique(f_cechip_cetissue_cecompound_time()$dose)
  updateSelectInput(inputId = "cedose", choices = choices)
})

# choose compound_name from cecompound
f_cechip_cetissue_cecompound_time_dose <-reactive({
  req(input$cedose)
  filter(f_cechip_cetissue_cecompound_time(), dose %in% input$cedose)
})


chemexpression_table = eventReactive(
  input$bu7,
  {
    #input$selectCompound
    input$cetissue
    input$cechip
    input$cetime
    input$cedose
    input$cecompound
    input$enrichrDBS
    input$cecategory
    # },
    # {
    withProgress(message = 'Retrieving affected genes from drugmatrix database in progress',
                 detail = 'This may take up to 1 minute ...', value = 0.2, 
                 { ##Start progress at 0
                   req(input$cetime)
                   
                   ceQuery <- paste0("SELECT a.compound_name, a.symbol, a.annotation, a.log_ratio, round(a.score, 5), a.experiment_name, b.tissue_name,  a.time, a.dose, a.chip_name, a.entrezid
                                      FROM all_transcript_vu_fc a INNER JOIN bsb_tissue_lookup b
                                      ON    b.tissue = a.tissue
                                      WHERE a.compound_name = '", input$cecompound, "'
                                      AND   b.tissue_name = '", input$cetissue, "'
                                      AND   a.chip_name IN ('", input$cechip, "')
                                      AND   a.time IN ", timeinput(input$cetime), "
                                      AND   a.score <= 0.05
                                      AND   a.dose = ", input$cedose, "
                                      AND   " , input$cecategory,"
                                      AND   a.symbol is NOT NULL
                                    UNION 
                                      SELECT a.compound_name, a.symbol, a.annotation, a.log_ratio, round(a.score, 5), a.experiment_name, b.tissue_name,  a.time, a.dose, a.chip_name, a.entrezid
                                      FROM all_transcript_vu_fc a INNER JOIN bsb_tissue_lookup b
                                      ON    b.tissue = a.tissue
                                      WHERE a.compound_name = '", input$cecompound, "'
                                      AND   b.tissue_name = '", input$cetissue, "'
                                      AND   a.chip_name IN ('", input$cechip, "')
                                      AND   a.time IN ", timeinput(input$cetime), "
                                      AND   a.score IS NULL
                                      AND   a.dose = ", input$cedose, "
                                      AND   " , input$cecategory,"
                                      AND   a.symbol is NOT NULL;")

                   print(ceQuery)
                   
                   df_ceQuery <- dbGetQuery(pobj, ceQuery)
                   
                   print("######################## The following is the ceQuery queryoutput.################")
                   print(df_ceQuery)
                   
                   print("################# The number of rows of df_pgPQuery.######################")
                   print(nrow(df_ceQuery))
                   
                   d7 <- df_ceQuery
                   
    incProgress(0.5, message = "Data Retrieved!  Now Summarizing data ...", 
                 detail = "This may take up to 30 seconds to 1 minute")
                   if(nrow(df_ceQuery) > 0){
                     
                     names(d7) <- c("compound" , "symbol", "annotation", "log_ratio", "pvalue", "experiment_name", "tissue", "time", "dose", "chip", "entrezid" )
                     
                     print("################# This is the list of rat genes from the queries")
                     print(paste(d7$symbol, d7$entrezid))
                     
                     d7$hu_gene <- hlink2(dfharmony, d7$entrezid)
                     
                     incProgress(0.4, message = "Formatting output in progress",
                                 detail = "This step take 3 seconds ...")
                     
                     tempd7 <- data.frame(d7$compound, d7$hu_gene, d7$symbol, d7$annotation, d7$log_ratio, d7$pvalue, d7$experiment_name, d7$tissue, d7$time, d7$dose, d7$chip)
                     tempd7 -> d7
                     names(d7) <-c("compound" , "hu_gene", "symbol", "annotation", "log_ratio", "pvalue", "experiment_name", "tissue", "time", "dose", "chip")
                     
                     if (input$cechip=='GENIE' || input$cechip == 'S1500+') {
                       d7$"pvalue" <- 0.049
                     }
                       
                     
                     print("##################### This is my final gene expression data frame #####################")
                     print(d7)
                     
                     myhugene <- na.omit(d7$hu_gene)
                     
                     harmonyhuman <- unique(myhugene) # remove NA values and make genes unique
                     
                     print("############### This is the list of corresponding human genes")
                     print(harmonyhuman)
                     
                   }
                   else
                   {
                     d7 = NULL
                   }
                 }
    )  # Progress bar message ended
    return(list(mycedata = d7, human_gene = harmonyhuman, enrichrInput = input$enrichrDBS))
  }
)  # End of chemical expression tab


output$ceTable = DT::renderDataTable(
  DT::datatable(
    chemexpression_table()$mycedata,
        selection = list(mode='single', target = 'column'
    ), # selection list, ONLY column 2 will be selected
    
    rownames = TRUE,
    escape = FALSE,
    
    caption = htmltools::tags$caption(style = 'caption-side: top; text-align: center; color:brown; font-size:150% ;','Table: Gene Expression Induced by Individual Chemical Compound'),
    
    extensions = 'Buttons',
    
    options=list(
      pageLength = 5, info = FALSE,
      lengthMenu = list(c(5, 20, 50, 100, -1), c("5", "20", "50", "100", "All")),
      columnDefs = list(list(className = 'dt-center', targets=c(1,2,3, 5,6,7,8,9,10))),
      order = list(5, 'desc'),
      # dom = 'Bfrtip',
      searchHighlight = TRUE,  # When you type in the Search Box, it will highlight.
      dom = 'Bliftsp',
      buttons=c('copy', 'csv', 'excel', 'pdf', 'print')
    ),

  )%>% formatStyle('hu_gene', backgroundColor = '#d4efdf')  # Light Green color
  # datatable
)

myEnrichrTable <- function() {

  withProgress(message = 'Retrieving pathway information from remote Enrichr databases in progress',
               detail = 'This may take up to 1 minute ...', value = 0.2, 
   { ##Start progress at 0
                 
    if (websiteLive) {
      print("#########This is the currently available dbs")
      #print("###############But let us print the original dbs available first!!!!!!!!!!")
      #  print(dbs)
        
      print("Now here are the 10 dbs Scott and I discussed.")
      # Assign the enrichr databases picked by Scott and I.
      dbs <- c ("BioPlanet_2019",  "ENCODE_and_ChEA_Consensus_TFs_from_ChIP-X", "GO_Biological_Process_2021", "GO_Cellular_Component_2021", "GO_Molecular_Function_2021", "KEGG_2021_Human",
                "MSigDB_Hallmark_2020", "Reactome_2022", "TG_GATES_2020", "WikiPathway_2021_Human")
      
      newdbs <- chemexpression_table()$enrichrInput  # Selected from 10 predefined databases shown above.  This is the multiple selection.
      
      print(dbs)
      print(newdbs)  # print out the newly selected databases
      
      enriched <- enrichr(chemexpression_table()$human_gene, newdbs)   # this dbs contains a lot of databases.  "KEGG_2021_Human is just one of them
    }
  
  incProgress(0.5, message = "Databases connected!  Now displaying data and graphics ...",
                 detail = "This may take up to 30 seconds to 1 minute")
  
    if (websiteLive)
    #  df_enriched <- enriched[["WikiPathway_2021_Human"]]    ########## I need to work on this line 
      # print("######################## print out the selected enrichr databases picked from the menu ########################## ")
      # print(chemexpression_table()$enrichrInput)
      # df_enriched <- enriched[[chemexpression_table()$enrichrInput]]
    {
      enriched_df <- data.frame(
        "Term"=character(),
        "Overlap"=character(),
        "P value"=double(),
        "Adjusted P value"=double(),
        "Old P value"=double(),
        "Old Adjusted P value"=double(),
        "Odds Ratio" = double(),
        "Combined Score"=double(),
        "Genes"=character(), 
        "DB Name"=character(),
        stringsAsFactors=FALSE) # initialize the dataframe to NA value, ready for the following FOR LOOP
      
      dbcolumn_df <- data.frame(character(), stringsAsFactors=FALSE)  # initialize the dataframe to hold the selected Enrichr databases
         
         for (i in 1:length(newdbs)){  # this newdbs is from the enrichr menu input.  It is a multiple selection
           print("########## The following is the looped db names") 
           print(newdbs[i])  # database name from EnrichR site
            enriched[[newdbs[i]]] # Dataframe of pathway under a certain EnrichR database but no database name included
            
            mytempframe <- enriched[[newdbs[i]]]  # this is the Enrichr dataframe from EnrichR.
            mytempframe <- cbind(mytempframe, newdbs[i])   # Add the database name into the current LOOP of dataframe
            
            enriched_df <- rbind(enriched_df, mytempframe)
         }  
          #  enriched_df <- rbind(enriched_df, enriched[[newdbs[i]]])
            
         
    }
  
          print("#################### This is the enriched_df output. #################")
          print(enriched_df)
          names(enriched_df) <-c("Term", "Overlap",  "P.value", "Adjusted.P.value", "Old.P.value", "Old.Adjusted.P.value", "Odds.Ratio", "Combined.Score", "Genes", "DB Name")
          # The above names are the default column names from EnrichR databases.  I may use some but not all.  That's why we see fewer columns in the output display.
          
          temp_enriched_df <- data.frame(enriched_df$"Term", enriched_df$"DB Name", enriched_df$"Overlap", round(enriched_df$"P.value", 5), round(enriched_df$"Odds.Ratio", 5), round(enriched_df$"Combined.Score", 3), enriched_df$"Genes")
          
          enriched_df <- temp_enriched_df
          # names(enriched_df) <-c("Term", "DB Name", "Overlap",  "P.value", "Adjusted.P.value", "Old.P.value", "Old.Adjusted.P.value", "Odds.Ratio", "Combined.Score", "Genes")
          names(enriched_df) <-c("Term", "DB Name", "Overlap",  "P.value", "Odds.Ratio", "Combined.Score", "Genes")
          
  
   } # end of the progress bar
  )  # Progress bar message ended
  
  return(list(myenrichdata = enriched_df, selectedDB = dbs, selectedNewDB = newdbs ))
}
output$EnrichrTable = DT::renderDataTable(
  myEnrichrTable()$myenrichdata,
  
  rownames = TRUE,
  escape = FALSE,
  
  caption = htmltools::tags$caption(style = 'caption-side: top; text-align: center; color:brown; font-size:150% ;','Table: Gene Related Pathways Analyzed from EnrichR Databases'),
  
  options=list(
    pageLength = 5, info = FALSE,
    lengthMenu = list(c(5, 20, 50, 100, -1), c("5", "20", "50", "100", "All")),
    columnDefs = list(list(className = 'dt-center', targets=c(3, 4, 5, 6))),
    order = list(4, 'asc'),
    # dom = 'Bfrtip',
    searchHighlight = TRUE,  # When you type in the Search Box, it will highlight.
    dom = 'Bliftsp',
    buttons=c('copy', 'csv', 'excel', 'pdf', 'print')
  )
  #,
  
  # options = list(lengthChange = FALSE)
)

enrichDataPlot <- function() 
{
  if (websiteLive)
     # enriched <- enrichr(chemexpression_table()$human_gene, dbs)
     # enriched <- enrichr(chemexpression_table()$human_gene, myEnrichrTable()$selectedDB)
       enriched <- enrichr(chemexpression_table()$human_gene, myEnrichrTable()$selectedNewDB)
       myEnrichrTable()$myenrichdata
    # plotEnrich(enriched[[1]], showTerms = 20, numChar = 40, y = "Count", orderBy = "P.value")
     plotEnrich(myEnrichrTable()$myenrichdata, showTerms = 20, numChar = 40, y = "Count", orderBy = "P value")
} # algorithm of enrichDataPlot function, chemical enrichr option

output$enrichBoxPlot <- renderPlot(
  {
    enrichDataPlot()
  }) # Ouput the enrichDataPlot, chemical enrichr option


} # function (input, output, session)

) # ShinyServer
