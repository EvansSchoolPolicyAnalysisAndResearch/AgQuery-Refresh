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
library(shinyWidgets)
#library(duckdb)
#library(dbplyr)
#library(duckplyr)

indicators <- read_excel("Data/AgDev_Indicator_Estimates.xlsx")
#DuckDB will make this faster but is currently experiencing compilation issues on shinyapps.io
#indicators <- data.frame()
#con <- dbConnect(duckdb(), dbdir="Data/database.duckdb", read_only=T)
#indicators <- tbl(con, "indicators")
#duckdb_register(con, "indicators", indicators, overwrite=T)
#indicators <- read.csv("Data/AgDev_Indicator_Estimates.csv")
#row.names(indicators) <- NULL

countree <- indicators %>% select(Geography, Year) %>% distinct()
indiclist <- indicators %>% select(indicatorcategory, indicatorname) %>% distinct()
genders <- indicators %>% select(genderdisaggregation) %>% distinct()
genders$level <- str_extract(genders$genderdisaggregation, "(households)|(livestock managers)|(plot managers)|(laborers)") %>% str_to_title
genders <- genders[order(genders$level),]
#Kludgy solution
farm_sizes <- data.frame(farm_size_html = factor(seq(1:9),
                                                 labels=c("0 ha", "0&lt;ha&lt;=1", "0&lt;ha&lt;=2", "0&lt;ha&lt;=4", "1&lt;ha&lt;=2", "2&lt;ha&lt;=4", "&gt;4 ha", "All", "N/A"), ordered=T),
                         farm_size_raw = c("0 ha", "0<ha<=1", "0<ha<=2", "0<ha<=4", "1<ha<=2", "2<ha<=4", ">4 ha", "All", "N/A"))
#farm_sizes <- indicators %>% select(hhfarmsizedisaggregation) %>% distinct() %>% unlist(use.names=F)
#farm_sizes <- farm_sizes[order(farm_sizes)]
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
  #if(length(farmsize > 0)){
  #  tab <- tab %>% filter(hhfarmsizedisaggregation %in% farmsize)
  #}
  if(length(farmsize > 0)){
    farmout <- data.frame(farm_size_html=treeToDf(farmsize)[,1])
    farmout <- inner_join(farmout, farm_sizes, by="farm_size_html") |> select(-farm_size_html)
    tab <- inner_join(tab, farmout, by=join_by(hhfarmsizedisaggregation == farm_size_raw))
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
      tags$style("
      @import url('https://fonts.googleapis.com/css?family=Encode+Sans:900|Open+Sans');
                 .btn.btn-default.action-button {--bs-btn-line-height: 0.9; font-size:0.7em};
                 "),
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
          tags$li(
            a(class='dl', href='https://github.com/EvansSchoolPolicyAnalysisAndResearch/LSMS-Data-Dissemination/raw/main/EPAR_UW_335_AgDev_Indicator_Estimates.xlsx', 'Get Estimates')
          ),
          tags$li(
            a(class='about', href='https://evans.uw.edu/policy-impact/epar/agricultural-development-data-curation', 'About the Data')
          )
      ))),
  useShinyjs(),
  accordion(open=T,
accordion_panel("Show/Hide Filters",
  layout_columns( 
    card(card_header("Select Country and Survey Year(s)"),
         card_body(tags$div(style=".bslib-gap-spacing {gap: 0px !important};",
                            shinyTree("countree", checkbox=T, search=F, multiple=T, themeDots=F, whole_node=T, themeIcons=F, theme='proton')
                            )),
         card_footer(fluidRow(column(12, align="center", actionButton("selectCtry", "Select All", width='25%'),
         actionButton("deselectCtry", "Clear Filter", width='25%'))))
         #treeInput("countree", "", choices=create_tree(countree, levels=c("Geography","Year")), closeDepth=0)
         ),
    card(card_header("Select Indicator(s)"),
         card_body(tags$div(style=".bslib-gap-spacing {gap: 0px !important};", p(style='font-size: 8px; margin-bottom: 0px;', "Search"),
         shinyTree("indics", checkbox=T, search=T, multiple=T, themeDots=F, whole_node=T, themeIcons=F, theme='proton')
         )),
         card_footer(fluidRow(column(12, align="center", actionButton("selectIndics", "Select All", width='25%'),
           actionButton("deselectIndics", "Clear Filter"))))
         #treeInput("indics", "", choices=create_tree(indiclist, levels=c("indicatorcategory", "indicatorname")),closeDepth=0)
         ),
    card(card_header(HTML("Select Gender Disaggregation <i>(Optional)</i>")),
         card_body(tags$div(style=".bslib-gap-spacing {gap: 0px !important};",
                            shinyTree("genders", checkbox=T, search=F, multiple=T, themeDots=F, whole_node=T, themeIcons=F, theme='proton')
                            )),
         card_footer(fluidRow(column(12, align="center", actionButton("selectGender", "Select All", width='25%'),
         actionButton("deselectGender", "Clear Filter", width='25%'))))
         ),
    card(card_header(HTML("Select Farm Size Disaggregation <i>(Optional)</i>")),
         #checkboxGroupInput("farmsizes",label="", choices=farm_sizes),
         card_body(tags$div(style=".bslib-gap-spacing {gap: 0px !important};",
                            shinyTree("farmsizes", checkbox=T, multiple=T, themeDots = F, whole_node=T, themeIcons=F, theme='proton')
                            )),
         card_footer(fluidRow(column(12, align="center", actionButton("selectFarmsize", "Select All", width='25%'),
                 actionButton("deselectFarmsize", "Clear Filter", width='25%'))))
         ),
    
    col_widths=c(-1, 5, 5, -1, -1, 5, 5, -1)
    ))),
  HTML("<br><hr><h3>Results</h3>"),
  downloadButton("dataDL", "Download Data"), HTML("<br>&nbsp;"),
  tags$div(
           DTOutput("dataTab"), style="font-size:80%", margin='0 0 0 -20px'),
  tags$footer(
    tags$div(id='citediv',
      tags$p(class='citation', 'University of Washington, Evans Policy Analysis and Research Group (EPAR) (2024) Living Standards Measurement Study - Integrated Surveys on Agriculture: Processed Datasets for Ethiopia ESS, Malawi IHS, Nigeria GHS, Tanzania NPS, and Uganda NPS from 2009-2022.'
             ),
      tags$p(class='citation', 'This content was originally published in 2019 and last updated on October 24th, 2024')),
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
    #farm_sizes <- as.data.frame(farm_sizes)
    #names(farm_sizes) <- "farmsize"
    #output$farmsizes <- renderTree(dfToTree(farm_sizes, "farmsize"))
    #farm_list <- vector(mode="character", length(farm_sizes))
    #names(farm_list) <- farm_sizes
    output$farmsizes <- renderTree(dfToTree(as.data.frame(farm_sizes$farm_size_html)))
    
    observeEvent(input$selectCtry, {runjs(HTML('$("#countree").jstree("select_all");'))})
    observeEvent(input$selectIndics, {runjs(HTML('$("#indics").jstree("select_all");'))})
    observeEvent(input$selectGender, {runjs(HTML('$("#genders").jstree("select_all");'))})
    observeEvent(input$deselectCtry, {runjs(HTML('$("#countree").jstree("deselect_all");'))})
    observeEvent(input$deselectIndics, {runjs(HTML('$("#indics").jstree("deselect_all");'))})
    observeEvent(input$deselectGender, {runjs(HTML('$("#genders").jstree("deselect_all");'))})
    observeEvent(input$selectFarmsize, {runjs(HTML('$("#farmsizes").jstree("select_all");'))})
    observeEvent(input$deselectFarmsize, {runjs(HTML('$("#farmsizes").jstree("deselect_all");'))})
    
    outtable <- reactive({filterTable(indicators, get_selected(input$countree, format="slices"), get_selected(input$indics, format="slices"), get_selected(input$genders, format="slices"), get_selected(input$farmsizes, format="slices"))})
    output$dataTab <- renderDT({datatable(req(outtable()), extensions='Buttons', 
              options=list(autoWidth=T, 
                           columnDefs=list(list(width='150px', targets=6), list(visible=F, targets=0)),
                           dom='Blfrtip', 
                           scrollX=T,
                           buttons=list(list(extend='colvis', text='Show/Hide Columns', columns=(1:26)))))
      })
  output$dataDL <- downloadHandler(filename="agquery_export.csv",
    content=function(file){
      rows=input$dataTab_rows_all
      cols=lapply(2:length(input$dataTab_state$columns), FUN=function(x){if(input$dataTab_state$columns[[x]]$visible==T) return(x-1)}) |> unlist()
      write.csv(outtable()[rows,cols, drop=F], file, row.names = F)
    }
  )
                               
#onStop(function() dbDisconnect(con))

  }


# Run the application 
shinyApp(ui = ui, server = server)
