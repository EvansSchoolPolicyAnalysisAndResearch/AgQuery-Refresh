#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    https://shiny.posit.co/
#

library(shiny)
library(shinyTree)
library(DT)
library(readxl)
library(bslib)
library(dplyr)
library(stringr)
library(shinyjs)
#library(shinyWidgets)
#library(duckdb)
#library(duckplyr)

#indicators <- read_excel("Data/EPAR_UW_335_AgDev_Indicator_Estimates.xlsx", 
#                         sheet = "Estimates by Instrument")
#DuckDB will make this faster but is currently experiencing compilation issues on shinyapps.io
#con <- dbConnect(duckdb(), dbdir="Data/database.duckdb", read_only=T)
#duckdb_register(con, "indicators", indicators)
indicators <- read.csv("Data/AgDev_Indicator_Estimates.csv")
countree <- indicators %>% select(Geography, Year) %>% distinct()
indiclist <- indicators %>% select(indicatorcategory, indicatorname) %>% distinct()
genders <- indicators %>% select(genderdisaggregation) %>% distinct()
genders$level <- str_extract(genders$genderdisaggregation, "(households)|(livestock managers)|(plot managers)|(laborers)") %>% str_to_title
genders <- genders[order(genders$level),]
farm_sizes <- indicators %>% select(hhfarmsizedisaggregation) %>% distinct() %>% unlist(use.names=F)
farm_sizes <- farm_sizes[order(farm_sizes)]
table_nicenames <- c("Geography", 
                     "Survey", 
                     "Instrument",
                     "Year",
                     "Indicator Category", 
                     "Indicator Name", 
                     "Units", 
                     "Commodity", 
                     "Gender", 
                     "Farm Size", 
                     "Total Population",
                     "Sample Population",
                     "Currency Conversion",
                     "Level of Observation",
                     "Weight",
                     "Short Name",
                     "Mean",
                     "SE",
                     "SD", 
                     "p25",
                     "p50",
                     "p75",
                     "min",
                     "max",
                     "N",
                     "N > 30")

filterTable <- function(tab, countries, indics, gender, farmsize){
  
  if(length(countries) > 0) {
    counsout <- treeToDf(countries)[,1:2]
    names(counsout) <- c("Geography","Year")
    tab <- inner_join(tab, counsout, by=c("Geography","Year"))
  }
  if(length(indics) > 0){
    indicsout <- treeToDf(indics)[,1:2]
    names(indicsout) <- c("indicatorcategory", "indicatorname")
    tab <- inner_join(tab, indicsout, by=c("indicatorcategory","indicatorname"))
  }
  if(length(gender > 0)){
    gendout <- treeToDf(gender)[,2]
    tab <- tab %>% filter(genderdisaggregation %in% gendout)
  }
  if(length(farmsize > 0)){
    tab <- tab %>% filter(hhfarmsizedisaggregation %in% farmsize)
  }
  tab <- tab %>% mutate(mean=signif(mean, 4), 
                        semean_strata = signif(semean_strata, 4),
                        sd = signif(sd, 4),
                        p25 = signif(p25, 4),
                        p50 = signif(p50, 4),
                        p75 = signif(p75, 4),
                        min = signif(min, 4),
                        max = signif(max, 4))
  
  names(tab) <- table_nicenames
    
  return(tab)
}

# Define UI for application that draws a histogram
ui <- page_fixed(
  includeCSS("www/main.css"),
    tags$head(
      tags$style("@import url('https://fonts.googleapis.com/css?family=Encode+Sans:900|Open+Sans');"),
    #tags$link(rel = "stylesheet", type = "text/css", href = "main.css")#,
    #tags$h1(id='banner')
  ),
    # Application title
    #headerPanel(HTML("<h1 id='banner'>AgQuery from EPAR</h1>"), windowTitle="AgQuery"),
      tags$header(
        tags$h1(
      id='banner', 'AgQuery from EPAR'
    ),
      tags$nav(
        tags$ul(
          #li( #Doesn't do anything
          #  a(class='home', href="/", width='180px', 'AgQuery Home')
          #),
          tags$li(
            a(class='dl', href='https://github.com/EvansSchoolPolicyAnalysisAndResearch/LSMS-Data-Dissemination/raw/main/EPAR_UW_335_AgDev_Indicator_Estimates.xlsx', 'Get Estimates')
          ),
          tags$li(
            a(class='about', href='https://evans.uw.edu/policy-impact/epar/agricultural-development-data-curation', 'About the Data')
          )
      ))),
    # Sidebar with a slider input for number of bins 
  useShinyjs(),
  layout_columns( 
    card(card_header("Select Country and Survey Year(s)"),
         shinyTree("countree", checkbox=T, search=F, multiple=T, themeDots=F, whole_node=T, themeIcons=F, theme='proton'),
         column(4, actionButton("selectCtry", "Clear Filter"))
         #treeInput("countree", "", choices=create_tree(countree, levels=c("Geography","Year")), closeDepth=0)
         ),
    
    card(card_header("Select Indicator(s)"),
         shinyTree("indics", checkbox=T, search=T, multiple=T, themeDots=F, whole_node=T, themeIcons=F, theme='proton'),
         column(4, actionButton("selectIndics", "Clear Filter"))
         #treeInput("indics", "", choices=create_tree(indiclist, levels=c("indicatorcategory", "indicatorname")),closeDepth=0)
         ),
    card(card_header(HTML("Select Gender Disaggregation <i>(Optional)</i>")),
         shinyTree("genders", checkbox=T, search=F, multiple=T, themeDots=F, whole_node=T, themeIcons=F, theme='proton'),
         column(4, actionButton("selectGender", "Clear Filter"))
         ),
    card(card_header(HTML("Select Farm Size Disaggregation <i>(Optional)</i>")),
         checkboxGroupInput("farmsizes",label="", choices=farm_sizes),
         column(4, actionButton("selectFarm", "Clear Filter"))
         ),
    
    col_widths=c(-1, 5, 5, -1, -1, 5, 5, -1)
    ),
  HTML("<br><hr><h3>Results</h3>"),
  
  tags$div(DTOutput("dataTab", width='110%'), style="font-size:80%; margin:0 0 0 -70px;"),
    tags$footer(
      tags$div(id='citediv',
        tags$p(class='credits', 'University of Washington, Evans Policy Analysis and Research Group (EPAR) (2024) Living Standards Measurement Study - Integrated Surveys on Agriculture: Processed Datasets for Ethiopia ESS, Malawi IHS, Nigeria GHS, Tanzania NPS, and Uganda NPS from 2009-2022.'
             ),
        tags$p(class='credits', 'This application was originally published in 2019 and last updated on October 24th, 2024')),
    tags$div(id='credits',
             tags$p(id='footimg', 'EPAR @ University of Washington')),
      tags$div(id='about',
             tags$p(HTML("&copy; Copyright 2019-2024 Evans Policy Analysis and Research Group")),
             tags$p('Established in 2008, the Evans School Policy Analysis and Research Group (EPAR) uses an innovative student-faculty team model to provide rigorous, applied research and analysis to international development stakeholders. Our research focuses on agriculture, development policy, financial services, poverty reduction, gender, and measurement and evaluation.'),
             tags$p('To learn more about EPAR, please visit our ', tags$a(href='https://epar.evans.uw.edu', 'homepage'), ".")
             )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output, session) {

    output$countree <- renderTree(dfToTree(countree, c("Geography","Year")))
    output$indics <- renderTree(dfToTree(indiclist, c("indicatorcategory", "indicatorname")))
    output$genders <- renderTree(dfToTree(genders, c("level", "genderdisaggregation")))
    observeEvent(input$selectCtry, {runjs(HTML('$("#countree").jstree("deselect_all");'))})
    observeEvent(input$selectIndics, {runjs(HTML('$("#indics").jstree("deselect_all");'))})
    observeEvent(input$selectGender, {runjs(HTML('$("#genders").jstree("deselect_all");'))})
    observeEvent(input$selectFarm, {runjs(HTML('$("#farmsizes").jstree("deselect_all");'))})
    
    outtable <- reactive({filterTable(indicators, get_selected(input$countree, format="slices"), get_selected(input$indics, format="slices"), get_selected(input$genders, format="slices"), input$farmsizes)})
    output$dataTab <- renderDT(datatable(req(outtable()), extensions='Buttons', 
                                         options=list(autoWidth=T, 
                                                      columnDefs=list(list(width='150px', targets=6)),
                                                      dom='Bfrtip', 
                                                      scrollX=T,
                                                      buttons=list(list(extend='colvis', text='Show/Hide Columns', columns=(1:26)))) 
                                         )
                               )
#onStop(function() dbDisconnect(con))
    }


# Run the application 
shinyApp(ui = ui, server = server)
