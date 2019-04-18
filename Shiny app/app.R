#REMEMBER TO TAKE OUT SET WD!!!!

#Jojo Emerson
#Gates Task 1
#R Shiny App for calculating DALYs
#Oct 31, 2018
#Version 28#
#updated to shiny apps.io? no (DALYcalculation)
#What has been updated this version:
  #ANNOTATED FOR CLARITY

##################################################################
#                             Jojo Emerson                       #
#                            DALY Calculator                     #
#                   Main directory: Shiny app file               #
#                     Code changes: October 31, 2018             #
#                    GitHub Upload: April 18, 2019               #
#                             Version 1.1.28                     #
##################################################################


############              REQUIRED FILES              ############

#Data directory: 
  # - IndexedDisabilityWeights.xlsx: Disability weights from GBD study 2010
  # - LEtable.xlsx: life expectancy table from GBD study 2016
#www directory:
  # - BMGF.png: BMGF logo for footer
  # - cevr.png: CEVR logo for footer
  # - TMC.png: Tufts medical center logo for footer
#Main directory: 
  # - app.R: R shiny app (current file)
  # - google-analytics.js: google analytics (removed for GitHub)

############ASSUMPTIONS############
#Inputs rounded to 0 decimal places#
#Constant value: 0.16243 

############PREAMBLE############
#load libraries#
library(shiny)
library(readxl)
library(shinyjs)
library(shinythemes)
library(readxl)

#pull in information from external spreadsheets
  #utility values
  gbd_utilities <- read_excel("data/IndexedDisabilityWeights.xlsx",1, col_types = c("text", "text", "text","text","text", "numeric", "numeric", "numeric"))
  utilities_list<-c(gbd_utilities$Mean_disabilityweight)
  names(utilities_list)<-(c(gbd_utilities$Sequela))
  #life expectancy
  lifeexpectancy<-read_excel("data/LEtable.xlsx",1)
  LE_list<-c(lifeexpectancy$Ex)
  names(LE_list)<-c(lifeexpectancy$age)

############UI############ 
ui <- navbarPage(
  #link to google analytics javascript file  (removed for GitHub)
  #tags$head(includeScript("google-analytics.js")),
  
  theme = shinytheme("flatly"),
  
  title = "DALY Calculator", 
  id = "tab",
  selected = "Calculate DALYs for an individual",
  
  
  ########TAB 1########
  #DALYs for an individual#
  tabPanel(
    #title for tab 1
    "Calculate DALYs for an individual",
    
    #CSS styling
    tags$style(type = 'text/css', '.navbar {font-family: Arial;}'),
    style = "font-family: 'Arial';height: 1000px;",
    
    #setup shinyjs
    useShinyjs(),
    
    
    sidebarLayout(
      sidebarPanel(
        #TAB 1 panel styling
        style = "border: 2px solid silver; border-radius: 10px;",
        #title
        htmlOutput(outputId = "col1title"),
          
             
        #required inputs
          #typeable disease selection
          selectizeInput("sequela", "Disease:", choices= c('', gbd_utilities$Sequela), multiple = FALSE, options = list(placeholder = 'Begin typing disease...')),
          #age of disability numeric input
          numericInput("a_disability", "Age of onset of disease (years):", value = "0", min = 0, max = 150, step=1),
          #age of death numeric input
          numericInput("a_death", "Age of premature death due to disease (years):", value = "0", min = 0, max = 150, step=1),
               
        #optional inputs
          #discount rate
            #checkbox
            checkboxInput("rcheck", "Discount rate?", value = TRUE),
            #rate
            conditionalPanel(
              condition = "input.rcheck == '1'",
              numericInput("r", "Discount rate:", value = 0.03, max = 1, min = 0, step = 0.01)),
              
          #age-weighting
            #checkbox
            checkboxInput("kcheck", "Include age weighting?", value = FALSE),
            #age weighting parameter
            conditionalPanel(
              condition = "input.kcheck == '1'",
              numericInput("K", "Age weighting parameter:", value = 0.04, max = 1, min = 0, step = 0.01)),
          
        
        #action button
          actionButton("go", "Calculate!"),
          width = 4
        
      ),#close sidebar panel
        
      mainPanel(
        #main panel styling
        style = "background-color: #ffffff;",
        
        fluidRow(
          column(
            #column styling
            style = "height:100%;",
          
            #outputs
              #title
              htmlOutput(outputId = "col2title"),
              #utility value popup
              htmlOutput(outputId = "disabilityweight"),
              #years lived with disease
              htmlOutput(outputId = "yearslived"),
              #LE at age of disability popup
              htmlOutput(outputId = "LEatdisability"),
              #row 3 title
              htmlOutput('row3title'),
              
            #error messages                    
              #error message for no disease input
              htmlOutput(outputId = "errormsg_disease"),
              #error message for too-large discount rate
              htmlOutput(outputId = 'errormsg_discount'),
              #error message for age-weighting parameter
              htmlOutput(outputId = 'errormsg_ageweight'),
              #error message for age of onset > age of death
              htmlOutput(outputId = 'errormsg_ages'),
              #error message for negative age inputs
              htmlOutput(outputId = 'errormsg_negages'),
                            
            #output table
            tableOutput(outputId = "table"), 
            width = 12
          )#close column
        )#close row
      )#close main panel
    ), #close sidebar layout
    
    #footer
      fluidRow(
        #styling for footer
        style = "
             position:fixed;
             font-family: 'Arial';
             font-size: 11px;
             bottom:0;
             width:100%;
             height:70px;
             color: black;
             padding: 5px;
             background-color: #ECF1F2;
             border: 2px solid silver;
             border-radius: 5px;
             z-index: -1000;",
             
        tagList(
          column(align = "left",width = 6,
            "Created by:", tags$br(),
            tags$img(src = "cevr.png", width = "60px", height = "30px"),
            tags$img(src = "TMC.png",width = "100px", height = "25px"),
            tags$br(),"Contact:", tags$a("jemerson@tuftsmedicalcenter.org", href="mailto:jemerson@tuftsmedicalcenter.org")
          ), #close column 1
          
          column(align = "right", width = 6, 
            "Funding provided by:",tags$br(),
            tags$img(src = "BMGF.png",width = "146px", height = "35px")
          ) #close column 2
        )
      )#close footer
), #close tab 1

  ########TAB 2########
  #Calculate DALYs for a population#
  tabPanel(
    #title and styling
    "Calculate DALYs for a population",
    style = "font-family: 'Arial';height: 1000px;",
    
    #setup shinyjs
    useShinyjs(),
         
    sidebarLayout(
      sidebarPanel(
        #styling
        style = "border: 2px solid silver; border-radius: 10px;",
        
        #title for tab 2
        htmlOutput(outputId = "tab2inputstitle"),
         
        #required inputs
          #disease
          selectizeInput("tab2sequela", "Disease:", choices= c('', gbd_utilities$Sequela), multiple = FALSE, options = list(placeholder = 'Begin typing disease...')),
          #age of onset
          numericInput("tab2ageonset", "Average age of onset:",value = 0, max = 7500000000, min = 1, step = 1),
          #age of death
          numericInput("tab2agedeath", "Average age of death:",value = 0, max = 7500000000, min = 1, step = 1),
          #number of cases
          numericInput("tab2cases", "Incident cases (cases/year):", value = 0, max = 7500000000, min = 1, step = 1),
          #number of deaths
          numericInput("tab2deaths", "Incident deaths (deaths/year):", value = 0, max = 7500000000, min = 1, step = 1),
             
        #optional inputs
          #discount rate
            #checkbox
            checkboxInput("rcheck2", "Discount rate?", value = TRUE),
            #discount rate
            conditionalPanel(
              condition = "input.rcheck2 == '1'",
              numericInput("r2", "Discount rate:", value = 0.03, max = 1, min = 0, step = 0.01)
            ),
             
          #age-weighting
            #checkbox
            checkboxInput("kcheck2", "Include age weighting?", value = FALSE),
            #age weighting parameter
            conditionalPanel(
              condition = "input.kcheck2 == '1'",
              numericInput("K2", "Age weighting parameter:", value = 0.04, max = 1, min = 0, step = 0.01)
            ),
             
          #action button
          actionButton("tab2go", "Calculate!")
        
        ),#close sidebar
      
      mainPanel(
        #styling and titles
        style = "height: 100%; background-color: #ffffff;",
        htmlOutput(outputId = "tab2outputstitle"), 
             
        #disability weight popup
        htmlOutput(outputId = "tab2disabilityweight"), 
        #average years lived in disease popup
        htmlOutput(outputId = "tab2yearslived"),  
        #LE at death popup
        htmlOutput(outputId = "tab2LEatdisability"),
             
        #table title
        htmlOutput(outputId = "tab2tabletitle"),
             
        #error messages
        htmlOutput(outputId = 'errormsg_disease2'),
        htmlOutput(outputId = 'errormsg_discount2'),
        htmlOutput(outputId = 'errormsg_ageweight2'),
        htmlOutput(outputId = 'errormsg_age2'),
        htmlOutput(outputId = 'errormsg_deathscases'),
        htmlOutput(outputId = 'errormsg_cases2'),
        htmlOutput(outputId = 'errormsg_negages2'),
          
        #output table
        htmlOutput(outputId = "tab2table")
        
      )#close main panel
    ),#close sidebar layout
         
    #footer
      fluidRow(
        #styling
        style = "
           position:fixed;
           font-family: 'Arial';
           font-size: 11px;
           bottom:0;
           width:100%;
           height:70px;
           color: black;
           padding: 5px;
           background-color: #ECF1F2;
           border: 2px solid silver;
           border-radius: 5px;
           z-index: -1000;",
        
        tagList(
          column(align = "left",width = 6,
            "Created by:", tags$br(),
            tags$img(src ="cevr.png", width = "60px", height = "30px"),
            tags$img(src = "TMC.png",width = "100px", height = "25px"),
            tags$br(),"Contact:", tags$a("jemerson@tuftsmedicalcenter.org", href="mailto:jemerson@tuftsmedicalcenter.org")
          ), #close column
          
          column(align = "right", width = 6, 
            "Funding provided by:",tags$br(),
            tags$img(src = "BMGF.png",width = "146px", height = "35px")
          ) #close column
        )
      )#close footer
  ),#close tab 2


  ########TAB 3########
  #References#

  tabPanel(
    #title and styling
    "References",
    tags$style(type = 'text/css', '.navbar {font-family: Arial;}'),
      style = "font-family: 'Arial';height: 1000px;",
        
    sidebarLayout(
      sidebarPanel(
        #styling and title
        style = "border: 2px solid silver; border-radius: 10px;",
        htmlOutput(outputId = "title_reference"),
             
        #footer info
        strong("Citation:"), tags$br(),
        "Emerson, J. and Kim, D.D. (2018). DALY calculator. Center for the Evaluation of Value and Risk in Health, Tufts Medical Center, Boston, MA.",
        tags$a("Methodology report", href = 'http://healtheconomics.tuftsmedicalcenter.org/ghcearegistry/2018_05_24_final_DALY_calculator_methodology_report.pdf', target = 'iframe'), tags$br(),
        tags$br(),
        "Created by:", tags$br(),
        tags$img(src = "cevr.png", width = "100px", height = "50px"),tags$br(), 
        tags$img(src = "TMC.png",width = "120px", height = "30px"),tags$br(), 
        tags$a("Center for the Evaluation of Value and Risk in Health", href = "http://healtheconomics.tuftsmedicalcenter.org/ghcearegistry/", target = "iframe"),tags$br(),
        "Contact:", tags$a("jemerson@tuftsmedicalcenter.org", href="mailto:jemerson@tuftsmedicalcenter.org"),tags$br(),
        tags$hr(),
        "Funding provided by:",tags$br(),
        tags$img(src = "BMGF.png",width = "185px", height = "40px"),tags$br(),
        #NOTE: gates target is not iframe because it does not work within it
        tags$a("Bill and Melinda Gates Foundation", href = "https://www.gatesfoundation.org/", target = "_blank"),tags$br(),
        tags$hr(),
             
        #definitions
        htmlOutput(outputId = "title_glossary"),
        htmlOutput(outputId = "info_dalys"),
        htmlOutput(outputId = "info_YLLs"),
        htmlOutput(outputId = "info_YLDs"),
        htmlOutput(outputId = "info_weights"),
             
        #sources
        htmlOutput(outputId = "title_sources"),
        htmlOutput(outputId = "source_equation"),
        htmlOutput(outputId = "source_weights"),
        htmlOutput(outputId = "source_LE"),
             
        width = 4
      ),
        
      #IFRAME   
      mainPanel(
        fluidRow(
          htmlOutput(outputId = "iframe")
        )
      )#close iframe
    )
  ),#close tab 

  ########TAB 4########
  #Code for R and GitHub#

  tabPanel("Code",
    #styling
    tags$style(type = 'text/css', '.navbar {font-family: Arial;}'),
    style = "font-family: 'Arial';height: 1000px;",
    
    sidebarLayout(
      sidebarPanel(
        #styling and title
        style = "border: 2px solid silver; border-radius: 10px;",
        htmlOutput(outputId = 'code_title'), tags$br(),
        
        #links to code and github
        "Download our R package to incorporate these functions in your models:",tags$br(),
        tags$a("TuftsCEVR/DALYCalculator", href = "https://github.com/TuftsCEVR/DALYcalculator", target = "_blank"),
        tags$br(),
        code("library('devtools')"), tags$br(),
        code("install_github('TuftsCEVR/DALYCalculator')")
      ),#close sidebar
      
      mainPanel(
        #function code
        fluidRow(
          verbatimTextOutput("individual_func")
        ),
             
        fluidRow(
          verbatimTextOutput('population_func')
        )
      )#close main panel
    ),#close sidebar layout
    
    #footer
    fluidRow(
      #style
      style = "
           position:fixed;
           font-family: 'Arial';
           font-size: 11px;
           bottom:0;
           width:100%;
           height:70px;
           color: black;
           padding: 5px;
           background-color: #ECF1F2;
           border: 2px solid silver;
           border-radius: 5px;
           z-index: -1000;",
      
      #info and logos
      tagList(
        column(align = "left",width = 6,
          "Created by:", tags$br(),
          tags$img(src ="cevr.png", width = "60px", height = "30px"),
          tags$img(src = "TMC.png",width = "100px", height = "25px"),
          tags$br(),"Contact:", tags$a("jemerson@tuftsmedicalcenter.org", href="mailto:jemerson@tuftsmedicalcenter.org")
        ), 
        
        column(align = "right", width = 6, 
          "Funding provided by:",tags$br(),
          tags$img(src = "BMGF.png",width = "146px", height = "35px")
        )
      )
    )#close the row
  )#close tab 4
)#close UI



############SERVER############ 
server <- function(input, output){
  
  #setup shinyjs
  useShinyjs()
  
  #make tables reactive
  outcomes <- reactiveValues(df = NULL, tab2df = NULL)
  
  #pull in information from external spreadsheets
  library(readxl)
  
  #utility values
  gbd_utilities <- read_excel("data/IndexedDisabilityWeights.xlsx",1, col_types = c("text", "text", "text","text","text", "numeric", "numeric", "numeric"))
  utilities_list<-c(gbd_utilities$Mean_disabilityweight)
  names(utilities_list)<-(c(gbd_utilities$Sequela))
  
  #life expectancy
  lifeexpectancy<-read_excel("data/LEtable.xlsx",1)
  LE_list<-c(lifeexpectancy$Ex)
  names(LE_list)<-c(lifeexpectancy$age)
  
  #DALY function definition - individual
  f_DALY<-function(K=0, C = 0.16243, r, beta=0, a_death, a_disability, YLD_L, disease){
    
    #pull in utility weight for disease and life expectancy at age of death
    D<-utilities_list[disease]
    named_a_death<-toString(a_death)
    YLL_L<-LE_list[named_a_death]
    
    #only r = 0
    if(r==0 & K!=0) {
      YLL<<-((K*C*exp(-beta*a_death))/(beta^2))*(exp(-beta*YLL_L)*((-beta)*(YLL_L+a_death)-1)-(-beta*a_death-1))+((1-K)*YLL_L)
      YLL_discounted<-YLL
      
      YLD<<-D*(((K*C*exp(-beta*a_disability))/(beta^2))*(exp(-beta*YLD_L)*(-beta*(YLD_L+a_disability)-1)-(-beta*a_disability-1))+((1-K)*YLD_L))
      
      DALY_total<<-YLL_discounted+YLD
    } 
    
    #both r and k = 0
    else if(r==0 & K==0) {
      YLL<<- ((1-K)/0.00000001)*(1-exp(-0.00000001*YLL_L))
      
      YLD<<- D*(((1-K)/0.00000001)*(1-exp(-0.00000001*YLD_L)))
      
      s<-a_death-a_disability
      YLL_discounted<-YLL*exp(-(0.00000001*s))
      
      DALY_total<<-YLL_discounted+YLD
    }
    
    #only k = 0
    else if(r!=0 & K==0) {
      YLL<<-((1-K)/r)*(1-exp(-r*YLL_L))
      
      YLD<<- D*(((1-K)/r)*(1-exp(-r*YLD_L)))
      
      s<-a_death-a_disability
      YLL_discounted<-YLL*exp(-(r*s))
      
      DALY_total<<-YLL_discounted+YLD
    }
    
    #neither r nor k = 0
    else if (r!=0 & K!=0){
      YLL<<-((K*C*exp(r*a_death))/((r+beta)^2))*(exp(-(r+beta)*(YLL_L+a_death))*(-(r+beta)*(YLL_L+a_death)-1)-exp(-(r+beta)*a_death)*(-(r+beta)*a_death-1))+((1-K)/r)*(1-exp(-r*YLL_L))
      
      YLD<<-D*((K*C*exp(r*a_disability))/((r+beta)^2))*(exp(-(r+beta)*(YLD_L+a_disability))*(-(r+beta)*(YLD_L+a_disability)-1)-exp(-(r+beta)*a_disability)*(-(r+beta)*a_disability-1))+((1-K)/r)*(1-exp(-r*YLD_L))
      
      s<-a_death-a_disability
      YLL_discounted<-YLL*exp(-(r*s))
      
      DALY_total<<-YLL_discounted+YLD
    }
    
    #return vector of years of life lost, years lived in disease, and total DALYs
    Amount<-c(YLL_discounted, YLD, DALY_total)
    return(Amount)
  }
  
  #DALY function definition - population
  f_DALYpop<-function(K=0, C = 0.16243, r, beta=0, a_death, a_disability, YLD_L, disease, incident_deaths, incident_cases){
    
    #pull in utility weight for disease and life expectancy at age of death
    D<-utilities_list[disease]
    named_a_death<-toString(a_death)
    YLL_L<-LE_list[named_a_death]
    
    #only r = 0
    if(r==0 & K!=0) {
      YLL<-((K*C*exp(-beta*a_death))/(beta^2))*(exp(-beta*YLL_L)*((-beta)*(YLL_L+a_death)-1)-(-beta*a_death-1))+((1-K)*YLL_L)
      popYLL_discounted<<-YLL*incident_deaths
      
      YLD<-D*(((K*C*exp(-beta*a_disability))/(beta^2))*(exp(-beta*YLD_L)*(-beta*(YLD_L+a_disability)-1)-(-beta*a_disability-1))+((1-K)*YLD_L))
      popYLD<<-YLD*incident_cases
      
      popDALY_total<<-popYLL_discounted+popYLD
    } 
    
    #both r and k = 0
    else if(r==0 & K==0) {
      YLL<- ((1-K)/0.00000001)*(1-exp(-0.00000001*YLL_L))
      s<-a_death-a_disability
      YLL_discounted<-YLL*exp(-(0.00000001*s))
      popYLL_discounted<<-YLL_discounted*incident_deaths
      
      YLD<<- D*(((1-K)/0.00000001)*(1-exp(-0.00000001*YLD_L)))
      popYLD<<-YLD*incident_cases
      
      popDALY_total<<-popYLL_discounted+popYLD
    }
    
    #only k = 0
    else if(r!=0 & K==0) {
      YLL<<-((1-K)/r)*(1-exp(-r*YLL_L))
      s<-a_death-a_disability
      YLL_discounted<-YLL*exp(-(r*s))
      popYLL_discounted<<-YLL_discounted*incident_deaths
      
      YLD<<- D*(((1-K)/r)*(1-exp(-r*YLD_L)))
      popYLD<<-YLD*incident_cases
      
      popDALY_total<<-popYLL_discounted+popYLD
    }
    
    #neither r nor k = 0
    else if (r!=0 & K!=0){
      YLL<-((K*C*exp(r*a_death))/((r+beta)^2))*(exp(-(r+beta)*(YLL_L+a_death))*(-(r+beta)*(YLL_L+a_death)-1)-exp(-(r+beta)*a_death)*(-(r+beta)*a_death-1))+((1-K)/r)*(1-exp(-r*YLL_L))
      s<-a_death-a_disability
      YLL_discounted<-YLL*exp(-(r*s))
      popYLL_discounted<<-YLL_discounted*incident_deaths
      
      YLD<-D*((K*C*exp(r*a_disability))/((r+beta)^2))*(exp(-(r+beta)*(YLD_L+a_disability))*(-(r+beta)*(YLD_L+a_disability)-1)-exp(-(r+beta)*a_disability)*(-(r+beta)*a_disability-1))+((1-K)/r)*(1-exp(-r*YLD_L))
      popYLD<<-YLD*incident_cases
      
      popDALY_total<<-popYLL_discounted+popYLD
    }
    
    #return vector of years of life lost, years lived in disease, and total DALYs
    Amount<-c(popYLL_discounted, popYLD, popDALY_total)
    return(Amount)
  }
  
  
  ########TAB 1########
  
    #column titles - tab 1
    output$col1title<-renderText({paste('<center>','<font size="5">', '<b>','Inputs:')})
    output$col2title<-renderText({paste('<br>','<center>','<font size="5">', '<b>','Outputs:')})
    output$row3title<-renderText({paste("<br>","<br>",'<b>', '<font size="3">','Years of Life Lost (YLLs),<br>
                                        Years Lived in Disability (YLDs),<br>
                                        and total Disability Adjusted Life Years (DALYs):')})
    
    #disability weight pop up - tab 1
    output$disabilityweight<-renderText({
      paste("<br>","<br>", '<font size="3">', "Disability weight = ", utilities_list[input$sequela])
    })
    
    #Years lived with disease pop up - tab 1
    output$yearslived<-renderText({
      paste("<br>", "<br>", '<font size="3">', "Years lived with disease = ", round(input$a_death-input$a_disability))
    })
    
    #LE at LEatdisability popup - tab 1 - this input has to be a_death+1 to make it correctly line up with the appropriate LE, would work if it were in quotes but we can't make it a string unless it's reactive
    output$LEatdisability<-renderText({
      paste("<br>", "<br>", '<font size="3">', "Life expectancy at age of premature death = ", LE_list[input$a_death+1])
    })
    
    #react to action button - tab 1
    observeEvent(input$go, {
      #require numeric inputs to be numeric
      validate(
        need(is.numeric(input$a_disability), "Please input a number"), 
        need(is.numeric(input$a_death), "Please input a number"),
        need(is.numeric(input$r), "Please input a number"),
        need(is.numeric(input$K), "Please input a number")
      )
      
    #run DALY function and assign it to outcomes dataframe, depending in inputs selected
    #rounds inputs to the nearest whole number
    if(input$rcheck == FALSE & input$kcheck == FALSE) {
      outcomes$df <- data.frame(c("Contribution of YLLs", "Contribution of YLDs", "Total DALYs"), f_DALY(r = 0, K = 0, a_death=round(input$a_death, digits = 0), a_disability = round(input$a_disability, digits = 0), YLD_L = round(input$a_death-input$a_disability, digits = 0), disease = input$sequela))
      colnames(outcomes$df)<-c("","")
    }
    
    else if(input$rcheck == TRUE & input$kcheck == FALSE){
      outcomes$df <- data.frame(c("Contribution of YLLs", "Contribution of YLDs", "Total DALYs"), f_DALY(r = input$r, K = 0, a_death=round(input$a_death, digits = 0), a_disability = round(input$a_disability, digits = 0), YLD_L = round(input$a_death-input$a_disability, digits = 0), disease = input$sequela))
      colnames(outcomes$df)<-c("","")
    }
    
    else if(input$rcheck == TRUE & input$kcheck == TRUE){
      outcomes$df <- data.frame(c("Contribution of YLLs", "Contribution of YLDs", "Total DALYs"), f_DALY(r = input$r, K = 1, beta = input$K, a_death=round(input$a_death, digits = 0), a_disability = round(input$a_disability, digits = 0), YLD_L = round(input$a_death-input$a_disability, digits =0), disease = input$sequela))
      colnames(outcomes$df)<-c("","")
    }
    
    else if(input$rcheck == FALSE & input$kcheck == TRUE){
      outcomes$df <- data.frame(c("Contribution of YLLs", "Contribution of YLDs", "Total DALYs"), f_DALY(r = 0, K = 1, beta = input$K, a_death=round(input$a_death, digits = 0), a_disability = round(input$a_disability, digits = 0), YLD_L = round(input$a_death-input$a_disability, digits =0), disease = input$sequela))
      colnames(outcomes$df)<-c("","")
    }
      
    #ERROR MESSAGES#
    #output error message if disease is not entered
    if (input$sequela == '' ){
      output$errormsg_disease<-renderText({paste("<font color=\"#FF0000\"><b>","Error: Please input disease name")})
      outcomes$df <- data.frame(c("Contribution of YLLs", "Contribution of YLDs", "Total DALYs"), c('NA', 'NA', 'NA'))
      colnames(outcomes$df)<-c("","")
      shinyjs::show('errormsg_disease')
    }
    
    else if(input$sequela != ''){
      shinyjs::hide('errormsg_disease')
    }
    
    #output error message if discount rate is >100
    if (input$r > 1 | input$r < 0){
      output$errormsg_discount<-renderText({paste("<font color=\"#FF0000\"><b>","Error: Discount rate must be between 0 and 1")})
      outcomes$df <- data.frame(c("Contribution of YLLs", "Contribution of YLDs", "Total DALYs"), c('NA', 'NA', 'NA'))
      colnames(outcomes$df)<-c("","")
      shinyjs::show('errormsg_discount')
    }
      
    else if(input$r <= 1| input$r >= 0){
      shinyjs::hide('errormsg_discount')
    }
    
    #output error message if age-weighting parameter is >100
    if (input$K > 1 | input$K < 0){
      output$errormsg_ageweight<-renderText({paste("<font color=\"#FF0000\"><b>","Error: Age weighting parameter must be between 0 and 1")})
      outcomes$df <- data.frame(c("Contribution of YLLs", "Contribution of YLDs", "Total DALYs"), c('NA', 'NA', 'NA'))
      colnames(outcomes$df)<-c("","")
      shinyjs::show('errormsg_ageweight')
    }
    
    else if(input$K <= 1| input$K >= 0){
      shinyjs::hide('errormsg_ageweight')
    }
    
    #output error message if age of onset exceeds age of death
    if (input$a_death < input$a_disability ){
      output$errormsg_ages<-renderText({paste("<font color=\"#FF0000\"><b>","Error: Age of onset cannot exceed age of death")})
      outcomes$df <- data.frame(c("Contribution of YLLs", "Contribution of YLDs", "Total DALYs"), c('NA', 'NA', 'NA'))
      colnames(outcomes$df)<-c("","")
      shinyjs::show('errormsg_ages')
    }
    
    else if(input$a_death > input$a_disability){
      shinyjs::hide('errormsg_ages')
    }
    
    #output error message if age input is negative
    if (input$a_death < 0 |  input$a_disability < 0){
      output$errormsg_negages<-renderText({paste("<font color=\"#FF0000\"><b>","Error: Age input cannot be negative")})
      outcomes$df <- data.frame(c("Contribution of YLLs", "Contribution of YLDs", "Total DALYs"), c('NA', 'NA', 'NA'))
      colnames(outcomes$df)<-c("","")
      shinyjs::show('errormsg_negages')
      shinyjs::hide('LEatdisability')
    }
    
    else if(input$a_death >= 0 |  input$a_disability >= 0){
      shinyjs::hide('errormsg_negages')
      shinyjs::show('LEatdisability')
    }
      
    #render table
    output$table<-renderTable({outcomes$df}, spacing = 'm', hover = TRUE)
    
  })   #close tab 1
  
  
  ########TAB 2########
  
    #TITLES
    output$tab2inputstitle<-renderText({paste('<center>','<font size="5">', '<b>','Inputs:')})
    #outputs title
    output$tab2outputstitle<-renderText({paste('<br>','<center>','<font size="5">', '<b>','Outputs:')})
    #output table title
    output$tab2tabletitle<-renderText({paste("<br>","<br>",'<b>', '<font size="3">','Population Years of Life Lost (YLLs), <br> Years Lived in Disability (YLDs), <br> and total Disability Adjusted Life Years (DALYs):')})
    
    #popups
    #disability weight pop up - tab 2
    output$tab2disabilityweight<-renderText({
      paste("<br>","<br>", '<font size="3">', "Disability weight = ", utilities_list[input$tab2sequela])
    })
    #Years lived with disease pop up - tab 2
    output$tab2yearslived<-renderText({
      paste("<br>", "<br>", '<font size="3">', "Average years lived with disease = ", round(input$tab2agedeath-input$tab2ageonset))
    })
    #LE at LEatdisability popup - tab 2 - this input has to be a_death+1 to make it correctly line up with the appropriate LE, would work if it were in quotes but we can't make it a string unless it's reactive
    output$tab2LEatdisability<-renderText({
      paste("<br>", "<br>", '<font size="3">', "Life expectancy at age of premature death = ", LE_list[input$tab2agedeath+1])
    })
    
    #react to calculate action button
    observeEvent(input$tab2go, {
      
      #make sure requirements are met, require numeric inputs to be numeric
      validate(
        need(is.numeric(input$tab2ageonset), "Please input a number"),
        need(is.numeric(input$tab2agedeath), "Please input a number"),
        need(is.numeric(input$tab2cases), "Please input a number"),
        need(is.numeric(input$tab2deaths), "Please input a number"),
        need(is.numeric(input$r2), "Please input a number"),
        need(is.numeric(input$K2), "Please input a number")
      )
      
      #make sure there are numbers in option inputs if checked
      if(input$rcheck2 == TRUE){req(input$r2)}
      if(input$kcheck2 == TRUE){req(input$K2)}
      
      #run DALY function and assign it to tab2df dataframe, depending in inputs selected 
      #rounds inputs to the nearest whole number
      if(input$rcheck2 == FALSE & input$kcheck2 == FALSE) {
        outcomes$tab2df <- data.frame(c("Contribution of YLLs", "Contribution of YLDs", "Total Population DALYs"), f_DALYpop(r = 0, K = 0, a_death=round(input$tab2agedeath, digits = 0), a_disability = round(input$tab2ageonset, digits = 0), YLD_L = round(input$tab2agedeath-input$tab2ageonset, digits = 0), disease = input$tab2sequela, incident_deaths=round(input$tab2deaths, digits = 0), incident_cases=round(input$tab2cases, digits = 0)))
        colnames(outcomes$tab2df)<-c("","")
      }
      else if(input$rcheck2 == TRUE & input$kcheck2 == FALSE){
        outcomes$tab2df <- data.frame(c("Contribution of YLLs", "Contribution of YLDs", "Total Population DALYs"), f_DALYpop(r = input$r2, K = 0, a_death=round(input$tab2agedeath, digits = 0), a_disability = round(input$tab2ageonset, digits = 0), YLD_L = round(input$tab2agedeath-input$tab2ageonset, digits = 0), disease = input$tab2sequela, incident_deaths=round(input$tab2deaths, digits = 0), incident_cases=round(input$tab2cases, digits = 0)))
        colnames(outcomes$tab2df)<-c("","")
      }
      else if(input$rcheck2 == TRUE & input$kcheck2 == TRUE){
        outcomes$tab2df <- data.frame(c("Contribution of YLLs", "Contribution of YLDs", "Total Population DALYs"), f_DALYpop(r = input$r2, K = 1, beta = input$K2, a_death=round(input$tab2agedeath, digits = 0), a_disability = round(input$tab2ageonset, digits = 0), YLD_L = round(input$tab2agedeath-input$tab2ageonset, digits = 0), disease = input$tab2sequela, incident_deaths=round(input$tab2deaths, digits = 0), incident_cases=round(input$tab2cases, digits = 0)))
        colnames(outcomes$tab2df)<-c("","")
      }
      else if(input$rcheck == FALSE & input$kcheck == TRUE){
        outcomes$tab2df <- data.frame(c("Contribution of YLLs", "Contribution of YLDs", "Total Population DALYs"), f_DALYpop(r = 0, K = 1, beta = input$K2, a_death=round(input$tab2agedeath, digits = 0), a_disability = round(input$tab2ageonset, digits = 0), YLD_L = round(input$tab2agedeath-input$tab2ageonset, digits = 0), disease = input$tab2sequela, incident_deaths=round(input$tab2deaths, digits = 0), incident_cases=round(input$tab2cases, digits = 0)))
        colnames(outcomes$tab2df)<-c("","")
      }
      
      #output table
      output$tab2table<-renderTable({outcomes$tab2df}, spacing = 'm', hover = TRUE)
      
      #ERROR MESSAGES#
      
      #for if the discount rate is over 100%
      if (input$r2 > 1 | input$r2 < 0){
        output$errormsg_discount2<-renderText({paste("<font color=\"#FF0000\"><b>","Error: Discount rate must be between 0 and 1")})
        shinyjs::show('errormsg_discount2')
        outcomes$tab2df <- data.frame(c("Contribution of YLLs", "Contribution of YLDs", "Total DALYs"), c('NA', 'NA', 'NA'))
        colnames(outcomes$tab2df)<-c("","")
      }
      else if(input$r2 <= 1| input$r2 >= 0){
        shinyjs::hide('errormsg_discount2')
      }
      
      #output error message if age-weighting parameter is >100
      if (input$K2 > 1 | input$K2 < 0){
        output$errormsg_ageweight2<-renderText({paste("<font color=\"#FF0000\"><b>","Error: Age weighting parameter must be between 0 and 1")})
        shinyjs::show('errormsg_ageweight2')
        outcomes$tab2df <- data.frame(c("Contribution of YLLs", "Contribution of YLDs", "Total DALYs"), c('NA', 'NA', 'NA'))
        colnames(outcomes$tab2df)<-c("","")
      }
      else if(input$K2 <= 1| input$K2 >= 0){
        shinyjs::hide('errormsg_ageweight2')
      }
      
      #output error message if no disease selected
      if (input$tab2sequela == '' ){
        output$errormsg_disease2<-renderText({paste("<font color=\"#FF0000\"><b>","Error: Please input disease name")})
        shinyjs::show('errormsg_disease2')
        outcomes$tab2df <- data.frame(c("Contribution of YLLs", "Contribution of YLDs", "Total DALYs"), c('NA', 'NA', 'NA'))
        colnames(outcomes$tab2df)<-c("","")
      }
      else if(input$tab2sequela != ''){
        shinyjs::hide('errormsg_disease2')
      }
      
      #output error message of # of deaths > # of cases
      if (input$tab2deaths>input$tab2cases ){
        output$errormsg_deathscases<-renderText({paste("<font color=\"#FF0000\"><b>","Error: # of death cannot exceed # of cases")})
        shinyjs::show('errormsg_deathscases')
        outcomes$tab2df <- data.frame(c("Contribution of YLLs", "Contribution of YLDs", "Total DALYs"), c('NA', 'NA', 'NA'))
        colnames(outcomes$tab2df)<-c("","")
      }
      else if(input$tab2deaths<=input$tab2cases){
        shinyjs::hide('errormsg_deathscases')
      }
      
      #ouptut error message of age of death exceed age of onset
      if (input$tab2ageonset>input$tab2agedeath ){
        output$errormsg_age2<-renderText({paste("<font color=\"#FF0000\"><b>","Error: Age of onset cannot exceed age of death")})
        shinyjs::show('errormsg_age2')
        outcomes$tab2df <- data.frame(c("Contribution of YLLs", "Contribution of YLDs", "Total DALYs"), c('NA', 'NA', 'NA'))
        colnames(outcomes$tab2df)<-c("","")
      }
      else if(input$tab2ageonset<=input$tab2agedeath ){
        shinyjs::hide('errormsg_age2')
      }
      
      #ouptut error message for when # of cases is 0 or fewer
      if (input$tab2cases<=0){
        output$errormsg_cases2<-renderText({paste("<font color=\"#FF0000\"><b>","Error: # of cases must be greater than 0")})
        shinyjs::show('errormsg_cases2')
        outcomes$tab2df <- data.frame(c("Contribution of YLLs", "Contribution of YLDs", "Total DALYs"), c('NA', 'NA', 'NA'))
        colnames(outcomes$tab2df)<-c("","")
      }
      else if(input$tab2cases>0){
        shinyjs::hide('errormsg_cases2')
      }
      
      #output error message if age input is negative
      if (input$tab2ageonset < 0 |  input$tab2agedeath < 0|  input$tab2cases < 0|  input$tab2deaths < 0){
        output$errormsg_negages2<-renderText({paste("<font color=\"#FF0000\"><b>","Error: Age input cannot be negative")})
        outcomes$df <- data.frame(c("Contribution of YLLs", "Contribution of YLDs", "Total DALYs"), c('NA', 'NA', 'NA'))
        colnames(outcomes$df)<-c("","")
        shinyjs::show('errormsg_negages2')
        shinyjs::hide('LEatdisability')
      }
      else if(input$a_death >= 0 |  input$a_disability >= 0){
        shinyjs::hide('errormsg_negages2')
        shinyjs::show('LEatdisability')
      }
    
  }) #close tab 2
  
  #######TAB 3#########
    #TITLES
    output$title_reference<-renderText({paste('<right>','<font size="5">', '<b>','References:')})
    output$title_glossary<-renderText({paste('<font size="4">','Glossary:')})
    output$title_sources<-renderText({paste('<font size="4">','Sources:')})
    
    #iframe
    output$iframe <- renderUI({
      tags$iframe(name ="iframe", width = "100%", height = "900")
    })
    
    #glossary
    #DALY
    daly_url<-a("DALYs", href = 'https://www.nimh.nih.gov/health/statistics/global/index.shtml', target="iframe")
    output$info_dalys <- renderUI({
      tagList(daly_url)
    })
    
    #YLL
    YLL_url<-a("Years of Life Lost", href = 'https://data.oecd.org/healthstat/potential-years-of-life-lost.htm',target="iframe")
    output$info_YLLs <- renderUI({
      tagList(YLL_url)
    })
    
    #YLD
    YLD_url<-a("Years Lived with Disability", href = 'https://www.nimh.nih.gov/health/statistics/disability/what-are-ylds.shtml ',target="iframe")
    output$info_YLDs <- renderUI({
      tagList(YLD_url)
    })
    
    #Utility weights
    weightinfo_url<-a("Disability Weights, Discounting, Age Weighting", href = 'http://www.who.int/healthinfo/global_burden_disease/daly_disability_weight/en/',target="iframe")
    output$info_weights <- renderUI({
      tagList(weightinfo_url)
    })
    
    #SOURCES
    
    #equations
    equation_url<-a("DALY Equations", href = 'https://academic.oup.com/heapol/article/16/3/326/601899',target="iframe")
    output$source_equation <- renderUI({
      tagList(equation_url)
    })
    
    #weights
    weight_url<-a("Disability Weights", href = 'https://www.sciencedirect.com/science/article/pii/S0140673612616808',target="iframe")
    output$source_weights <- renderUI({
      tagList(weight_url)
    })
    
    #life expectancies
    LE_url<-a("Life Expectancy", href = 'https://ora.ox.ac.uk/objects/uuid:36fb8d1d-12ae-4015-9aeb-1c862b0986ee',target="iframe")
    output$source_LE <- renderUI({
      tagList(LE_url)
  
  })#close tab 3
  
  #######TAB 4#########
    #TITLES
    output$code_title<-renderText({paste('<right>','<font size="5">', '<b>','Want to use our code?')})
    
    #CODE for individual function
    output$individual_func<-renderText({'
      #                                 CALCULATE DALYS FOR AN INDIVIDUAL                            #
      #                                                                                              #
      # Calculates disability-adjusted life years for an individual                                  # 
      # Returns a vector of Years of Life Lost (discounted), Years Lived in Disease, and total DALYs #
      #                                                                                              #
      # PARAMETERS:                                                                                  #
      #   K = Age weighting modulation factor (1=use age weighting, 0=no age weighting)              #
      #   C = contant (default = 0.16243)                                                            #
      #   r = discount rate (between 0-1)                                                            #
      #   beta = parameter of age weighting function (between 0-1)                                   #
      #   a_death = Age of premature death due to disease (in years)                                 #
      #   a_disability = Age of disease onset (in years)                                             #
      #   YLL_L = Life expectancy at age of death (in years)                                         #
      #   D = Disability weight (between 0-1)                                                        #
      
      f_DALY<-function(K, C = 0.16243, r, beta, a_death, a_disability, YLL_L, D){
      
      #calculate YLD_L
      YLD_L <- a_death - a_disability
      
      #only r = 0
      if(r==0 & K!=0) {
      YLL<<-((K*C*exp(-beta*a_death))/(beta^2))*(exp(-beta*YLL_L)*((-beta)*(YLL_L+a_death)-1)-(-beta*a_death-1))+((1-K)*YLL_L)
      YLL_discounted<-YLL
      
      YLD<<-D*(((K*C*exp(-beta*a_disability))/(beta^2))*(exp(-beta*YLD_L)*(-beta*(YLD_L+a_disability)-1)-(-beta*a_disability-1))+((1-K)*YLD_L))
      
      DALY_total<<-YLL_discounted+YLD
      } 
      
      #both r= 0 and k = 0
      else if(r==0 & K==0) {
      YLL<<- ((1-K)/0.00000001)*(1-exp(-0.00000001*YLL_L))
      
      YLD<<- D*(((1-K)/0.00000001)*(1-exp(-0.00000001*YLD_L)))
      
      s<-a_death-a_disability
      YLL_discounted<-YLL*exp(-(0.00000001*s))
      
      DALY_total<<-YLL_discounted+YLD
      }
      
      #only k = 0
      else if(r!=0 & K==0) {
      YLL<<-((1-K)/r)*(1-exp(-r*YLL_L))
      
      YLD<<- D*(((1-K)/r)*(1-exp(-r*YLD_L)))
      
      s<-a_death-a_disability
      YLL_discounted<-YLL*exp(-(r*s))
      
      DALY_total<<-YLL_discounted+YLD
      }
      
      #neither r = 0 nor k = 0
      else if (r!=0 & K!=0){
      YLL<<-((K*C*exp(r*a_death))/((r+beta)^2))*(exp(-(r+beta)*(YLL_L+a_death))*(-(r+beta)*(YLL_L+a_death)-1)-exp(-(r+beta)*a_death)*(-(r+beta)*a_death-1))+((1-K)/r)*(1-exp(-r*YLL_L))
      
      YLD<<-D*((K*C*exp(r*a_disability))/((r+beta)^2))*(exp(-(r+beta)*(YLD_L+a_disability))*(-(r+beta)*(YLD_L+a_disability)-1)-exp(-(r+beta)*a_disability)*(-(r+beta)*a_disability-1))+((1-K)/r)*(1-exp(-r*YLD_L))
      
      s<-a_death-a_disability
      YLL_discounted<-YLL*exp(-(r*s))
      
      DALY_total<<-YLL_discounted+YLD
      }
      
      #return vector of years of life lost, years lived in disease, and total DALYs
      Amount<-c(YLL_discounted, YLD, DALY_total)
      return(Amount)
      }'
    })
    
    #CODE for population function
    output$population_func<-renderText({'
      #                                   CALCULATE DALYS FOR A POPULATION                           #
      #                                                                                              #
      # Calculates disability-adjusted life years for a population                                   # 
      # Returns a vector of Years of Life Life (discounted), Years Lived in Disease, and total DALYs #
      #                                                                                              #
      # PARAMETERS:                                                                                  #
      #   K = Age weighting modulation factor (1=use age weighting, 0=no age weighting)              #
      #   C = contant (default = 0.16243)                                                            #
      #   r = discount rate (between 0-1)                                                            #
      #   beta = parameter of age weighting function (between 0-1)                                   #
      #   a_death = Average age of premature death due to disease for population (in years)          #
      #   a_disability = Average age of disease onset for population (in years)                      #
      #   YLL_L = Life expectancy at age of death (in years)                                         #
      #   D = Disability weight (between 0-1)                                                        #
      #   incident_deaths = average number of deaths for the population                              #
      #   incident_cases = average number of cases in the population                                 #
      
      f_DALYpop<-function(K, C = 0.16243, r, beta, a_death, a_disability, YLL_L, D, incident_deaths, incident_cases){
      #calculate YLD_L
      YLD_L <- a_death - a_disability
      
      #only r = 0
      if(r==0 & K!=0) {
      YLL<-((K*C*exp(-beta*a_death))/(beta^2))*(exp(-beta*YLL_L)*((-beta)*(YLL_L+a_death)-1)-(-beta*a_death-1))+((1-K)*YLL_L)
      popYLL_discounted<<-YLL*incident_deaths
      
      YLD<-D*(((K*C*exp(-beta*a_disability))/(beta^2))*(exp(-beta*YLD_L)*(-beta*(YLD_L+a_disability)-1)-(-beta*a_disability-1))+((1-K)*YLD_L))
      popYLD<<-YLD*incident_cases
      
      popDALY_total<<-popYLL_discounted+popYLD
      } 
      
      #both r = 0 and k = 0
      else if(r==0 & K==0) {
      YLL<- ((1-K)/0.00000001)*(1-exp(-0.00000001*YLL_L))
      s<-a_death-a_disability
      YLL_discounted<-YLL*exp(-(0.00000001*s))
      popYLL_discounted<<-YLL_discounted*incident_deaths
      
      YLD<<- D*(((1-K)/0.00000001)*(1-exp(-0.00000001*YLD_L)))
      popYLD<<-YLD*incident_cases
      
      popDALY_total<<-popYLL_discounted+popYLD
      }
      
      #only k = 0
      else if(r!=0 & K==0) {
      YLL<<-((1-K)/r)*(1-exp(-r*YLL_L))
      s<-a_death-a_disability
      YLL_discounted<-YLL*exp(-(r*s))
      popYLL_discounted<<-YLL_discounted*incident_deaths
      
      YLD<<- D*(((1-K)/r)*(1-exp(-r*YLD_L)))
      popYLD<<-YLD*incident_cases
      
      popDALY_total<<-popYLL_discounted+popYLD
      }
      
      #neither r = 0 nor k = 0
      else if (r!=0 & K!=0){
      YLL<-((K*C*exp(r*a_death))/((r+beta)^2))*(exp(-(r+beta)*(YLL_L+a_death))*(-(r+beta)*(YLL_L+a_death)-1)-exp(-(r+beta)*a_death)*(-(r+beta)*a_death-1))+((1-K)/r)*(1-exp(-r*YLL_L))
      s<-a_death-a_disability
      YLL_discounted<-YLL*exp(-(r*s))
      popYLL_discounted<<-YLL_discounted*incident_deaths
      
      YLD<-D*((K*C*exp(r*a_disability))/((r+beta)^2))*(exp(-(r+beta)*(YLD_L+a_disability))*(-(r+beta)*(YLD_L+a_disability)-1)-exp(-(r+beta)*a_disability)*(-(r+beta)*a_disability-1))+((1-K)/r)*(1-exp(-r*YLD_L))
      popYLD<<-YLD*incident_cases
      
      popDALY_total<<-popYLL_discounted+popYLD
      }
      
      #return vector of years of life lost, years lived in disease, and total DALYs
      Amount<-c(popYLL_discounted, popYLD, popDALY_total)
      return(Amount)
      }
      '})
    
  }#close tab 4

############SHINY APP############ 
shinyApp(ui = ui, server = server)