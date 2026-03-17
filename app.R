library(shiny)
library(shinyTree)
library(DT)
library(readxl)
library(bslib)
library(dplyr)
library(stringr)
library(shinyjs)
library(shinyWidgets)
library(duckdb)
library(dbplyr)
library(duckplyr)


filterTable <- function(tab, countries, indics, gender, farmsize, commodity, currency){
  
  if(length(countries) > 0) {
    counsout <- treeToDf(countries)[,1:2]
    names(counsout) <- c("Geography","Year")
    tab <- tab |> filter(Geography %in% counsout$Geography, Year %in% counsout$Year)
  }
  if(length(indics) > 0){
    indicsout <- treeToDf(indics)[,1:2]
    names(indicsout) <- c("indicatorcategory", "indicatorname")
    tab <- tab |> filter(indicatorcategory %in% indicsout$indicatorcategory, indicatorname %in% indicsout$indicatorname)
  }
  if(length(gender > 0)){
    gendout <- treeToDf(gender)[,2]
    tab <- tab |> filter(genderdisaggregation %in% gendout)
  }

  if(length(farmsize > 0)){
    farmout <- data.frame(farm_size_html=treeToDf(farmsize)[,1])
    farmout <- inner_join(farmout, farm_sizes, by="farm_size_html")
    tab <- tab |> filter(hhfarmsizedisaggregation %in% farmout$farm_size_raw)
  }
  
  if(length(commodity) > 0){
    commodout <- treeToDf(commodity)[,2]
    tab <- tab |> filter(commoditydisaggregation %in% commodout)
  }
  
  if(length(currency) > 0) {
    currout <- treeToDf(currency)[,1]
    tab <- tab |> filter(currencyconversion %in% currout)
  }
  
  tab <- as.data.frame(tab)
  names(tab) <- table_nicenames
  
  return(tab)
}

# Define UI for application that draws a histogram
ui <- page_fixed(
  includeCSS("www/main.css"),
    tags$head(
      if(file.exists("www/gtag.html")) includeHTML("www/gtag.html"),
      tags$style("
      @import url('https://fonts.googleapis.com/css?family=Encode+Sans:900|Open+Sans');
                 .btn.btn-default.action-button {--bs-btn-line-height: 0.9; font-size:0.7em;}
                  .jstree-proton .jstree-anchor {
                   white-space: normal !important;
                   height : auto !important;
                 }
                 "),
      tags$title("AgQuery | LSMS-ISA Data Access")
  ),
    # Application title
    #headerPanel(HTML("<h1 id='banner'>AgQuery from EPAR</h1>"), windowTitle="AgQuery"),

      tags$header(
        tags$h1(
      id='banner', 'AgQuery from EPAR'
    ),
      tags$nav(
        tags$ul(
          #tags$li(
          #  a(class='dl', href='https://github.com/EvansSchoolPolicyAnalysisAndResearch/LSMS-Data-Dissemination/raw/main/EPAR_UW_335_AgDev_Indicator_Estimates.xlsx', 'Get Estimates')
          #),
          tags$li(
            a(class='about', href='https://evans.uw.edu/policy-impact/epar/agricultural-development-data-curation', 'About the Data')
          ),
    tags$li(style="font-size: 14px",
      a(class='home', href="https://agquery.org/agqueryplus", 'Try AgQuery+ for more options on slicing the data, dynamic filtering, and visualizations')
    )
      )),
    ),
  useShinyjs(),
  accordion(open=T,
accordion_panel(HTML("Show/Hide Filters<br><p style='font-size: 12px'><i>Note: Leaving all filters unchecked will show the whole dataset</i></p>"),
  layout_columns( 
    card(card_header("Select Country and Survey Year(s)"),
         card_body(tags$div(style=".bslib-gap-spacing {gap: 0px !important};",
                            shinyTree("countree", checkbox=T, search=F, multiple=T, themeDots=F, whole_node=T, themeIcons=F, theme='proton')
                            )),
         card_footer(fluidRow(column(12, align="center", actionButton("selectCtry", "Select All", width='40%'),
         actionButton("deselectCtry", "Clear Filter", width='40%'))))
         #treeInput("countree", "", choices=create_tree(countree, levels=c("Geography","Year")), closeDepth=0)
         ),
    card(card_header("Select Indicator(s)"),
         card_body(tags$div(style=".bslib-gap-spacing {gap: 0px !important};",
                            p(style='font-size: 8px; margin-bottom: 0px;', "Search"),
         shinyTree("indics", checkbox=T, search=T, multiple=T, themeDots=F, whole_node=T, themeIcons=F, theme='proton')
         )),
         card_footer(fluidRow(column(12, align="center", actionButton("selectIndics", "Select All", width='40%'),
           actionButton("deselectIndics", "Clear Filter", width='40%'))))
         #treeInput("indics", "", choices=create_tree(indiclist, levels=c("indicatorcategory", "indicatorname")),closeDepth=0)
         ),
    card(card_header(HTML("Select Gender Disaggregation <i>(Optional)</i>")),
         card_body(tags$div(style=".bslib-gap-spacing {gap: 0px !important};",
                            shinyTree("genders", checkbox=T, search=F, multiple=T, themeDots=F, whole_node=T, themeIcons=F, theme='proton')
                            )),
         card_footer(fluidRow(column(12, align="center", actionButton("selectGender", "Select All", width='40%'),
         actionButton("deselectGender", "Clear Filter", width='40%'))))
         ),
    card(card_header(HTML("Select Farm Size Disaggregation <i>(Optional)</i>")),
         card_body(tags$div(style=".bslib-gap-spacing {gap: 0px !important};",
                            shinyTree("farmsizes", checkbox=T, multiple=T, themeDots = F, whole_node=T, themeIcons=F, theme='proton')
                            )),
         card_footer(fluidRow(column(12, align="center", actionButton("selectFarmsize", "Select All", width='40%'),
                 actionButton("deselectFarmsize", "Clear Filter", width='40%'))))
         ),
    card(card_header(HTML("Select Commodity <i>(Optional)</i>")),
         card_body(tags$div(style=".bslib-gap-spacing {gap: 0px !important};",
                            shinyTree("commodities", checkbox=T, multiple=T, themeDots=F, whole_node=T, themeIcons=F, theme='proton')
                   )),
         card_footer(fluidRow(column(12, align="center", actionButton("selectCommods", "Select All", width='40%'),
                                     actionButton("deselectCommods", "Clear Filter", width='40%'))))
    ),
    card(card_header(HTML("Select Currency Conversion <i>(Optional)</i>")),
         card_body(tags$div(style=".bslib-gap-spacing {gap: 0px !important};",
                            shinyTree("currencies", checkbox=T, multiple=T, themeDots=F, whole_node=T, themeIcons=F, theme='proton')
         )),
         card_footer(fluidRow(column(12, align="center", actionButton("selectCurrs", "Select All", width='40%'),
                                     actionButton("deselectCurrs", "Clear Filter", width='40%'))))
         ),
    
    #col_widths=c(-1, 5, 5, -1, -1, 5, 5, -1)
    col_widths=c(4, 8, 3, 3, 3, 3)
    ))),
  HTML("<br><hr><h3>Results</h3>"),
  downloadButton("dataDL", "Download Data"), HTML("<br>&nbsp;"),
  tags$div(
           DTOutput("dataTab"), style="font-size:80%", margin='0 0 0 -20px'),
  tags$footer(
    tags$div(class='citediv',
      tags$p('Citation:'),
      tags$p('University of Washington, Evans Policy Analysis and Research Group (EPAR) (2025) Living Standards Measurement Study - Integrated Surveys on Agriculture: Processed Datasets for Ethiopia ESS, Malawi IHS/IHPS, Nigeria GHS, Tanzania NPS, and Uganda NPS from 2009-2022.'),
      tags$p('This content was originally published in 2019 and last updated on October 16th, 2025'),
      tags$p(class='citation')
      ),
    tags$div(id='credits',
             tags$p(id='footimg', 'EPAR @ University of Washington')),
    tags$div(id='about',
             tags$p(HTML("&copy; Copyright 2019-2025 Evans Policy Analysis and Research Group")),
             tags$p('Established in 2008, the Evans School Policy Analysis and Research Group (EPAR) uses an innovative student-faculty team model to provide rigorous, applied research and analysis to international development stakeholders. Our research focuses on agriculture, development policy, financial services, poverty reduction, gender, and measurement and evaluation.'),
             tags$p('To learn more about EPAR, please visit our ', tags$a(href='https://epar.evans.uw.edu', 'homepage'), ".")
             )
    )
)

server <- function(input, output, session) {
    con <- dbConnect(duckdb(), dbdir="Data/database.duckdb", read_only=T)
    indicators <- tbl(con, "indicators") |> data.frame()
  
    countree <- indicators |> select(Geography, Year) |> distinct() |> as.data.frame()
    indiclist <- indicators |> select(indicatorcategory, indicatorname) |> distinct() |> as.data.frame()
    genders <- indicators |> 
      select(genderdisaggregation) |> 
      distinct() |> 
      as.data.frame() |>
      mutate(level=str_to_title(str_extract(genderdisaggregation, "(households)|(livestock managers)|(plot managers)|(laborers)"))) |>
      arrange(level)
    
    commodities <- indicators |> 
      select(commoditydisaggregation) |> 
      distinct() |> 
      as.data.frame() |>
      filter(commoditydisaggregation!="N/A", !is.na(commoditydisaggregation)) |> 
      mutate(category=ifelse(str_detect(commoditydisaggregation, regex("[rR]uminants|[pP]oultry|[lL]ivestock")), "Livestock", "Crops")) |>
      arrange(commoditydisaggregation)
    
    
    currencies <- indicators |> select(currencyconversion) |> distinct() |> filter(!is.na(currencyconversion), currencyconversion!="N/A") |> as.data.frame()
    #Kludgy solution
    farm_sizes <- data.frame(farm_size_html = factor(seq(1:9),
                                                     labels=c("0 ha", "0&lt;ha&lt;=1", "0&lt;ha&lt;=2", "0&lt;ha&lt;=4", "1&lt;ha&lt;=2", "2&lt;ha&lt;=4", "&gt;4 ha", "All", "N/A"), ordered=T),
                             farm_size_raw = c("0 ha", "0<ha<=1", "0<ha<=2", "0<ha<=4", "1<ha<=2", "2<ha<=4", ">4 ha", "All", "N/A"))
    #farm_sizes <- indicators |> select(hhfarmsizedisaggregation) |> distinct() |> unlist(use.names=F)
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
  
    output$countree <- renderTree(dfToTree(countree, c("Geography","Year")))
    output$indics <- renderTree(dfToTree(indiclist, c("indicatorcategory", "indicatorname")))
    output$genders <- renderTree(dfToTree(genders, c("level", "genderdisaggregation")))
    output$farmsizes <- renderTree(dfToTree(as.data.frame(farm_sizes$farm_size_html)))
    output$commodities <- renderTree(dfToTree(commodities, c("category", "commoditydisaggregation")))
    output$currencies <- renderTree(dfToTree(currencies))
    
    observeEvent(input$selectCtry, {runjs(HTML('$("#countree").jstree("select_all");'))})
    observeEvent(input$selectIndics, {runjs(HTML('$("#indics").jstree("select_all");'))})
    observeEvent(input$selectGender, {runjs(HTML('$("#genders").jstree("select_all");'))})
    observeEvent(input$selectFarmsize, {runjs(HTML('$("#farmsizes").jstree("select_all");'))})
    observeEvent(input$selectCommods, {runjs(HTML('$("#commodities").jstree("select_all");'))})
    observeEvent(input$selectCurrs, {runjs(HTML('$("#currencies").jstree("select_all");'))})
    
    observeEvent(input$deselectCtry, {runjs(HTML('$("#countree").jstree("deselect_all");'))})
    observeEvent(input$deselectIndics, {runjs(HTML('$("#indics").jstree("deselect_all");'))})
    observeEvent(input$deselectGender, {runjs(HTML('$("#genders").jstree("deselect_all");'))})
    observeEvent(input$deselectFarmsize, {runjs(HTML('$("#farmsizes").jstree("deselect_all");'))})
    observeEvent(input$deselectCommods, {runjs(HTML('$("#commodities").jstree("deselect_all");'))})
    observeEvent(input$deselectCurrs, {runjs(HTML('$("#currencies").jstree("deselect_all");'))})
    
    outtable <- reactive({filterTable(indicators, get_selected(input$countree, format="slices"), 
                                      get_selected(input$indics, format="slices"), 
                                      get_selected(input$genders, format="slices"), 
                                      get_selected(input$farmsizes, format="slices"),
                                      get_selected(input$commodities, format="slices"),
                                      get_selected(input$currencies, format="slices")
                                      )})
    
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
 onStop(function() dbDisconnect(con, shutdown=TRUE))

  }


# Run the application 
shinyApp(ui = ui, server = server)
