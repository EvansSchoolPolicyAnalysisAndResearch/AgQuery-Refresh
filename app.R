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


indicators <- read_excel("Data/EPAR_UW_335_AgDev_Indicator_Estimates.xlsx", 
                         sheet = "Estimates by Instrument")
countree <- indicators %>% select(Geography, Year) %>% distinct()
indiclist <- indicators %>% select(indicatorcategory, indicatorname) %>% distinct()
genders <- indicators %>% select(genderdisaggregation) %>% distinct() %>% unlist(use.names=F)
genders <- genders[order(genders)]
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
    tab <- tab %>% filter(genderdisaggregation %in% gender)
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
                        min = signif(min, 4))
  
  names(tab) <- table_nicenames
    
  return(tab)
}

# Define UI for application that draws a histogram
ui <- page_fillable(
  tags$head(
    tags$style("@import url('https://fonts.googleapis.com/css?family=Encode+Sans:900|Open+Sans');"),
    tags$link(rel = "stylesheet", type = "text/css", href = "main.css")
  ),
    # Application title
    titlePanel(HTML("<h1 id='banner'>AgQuery from EPAR</h1>")),

    # Sidebar with a slider input for number of bins 
  layout_column_wrap(width=1/2, 
    card(card_header("Select Country and Survey Year(s)"),
         shinyTree("countree", checkbox=T, search=F, multiple=T, themeDots=F, whole_node=T, themeIcons=F, theme='proton')),
    card(card_header("Select Indicator(s)"),
         shinyTree("indics", checkbox=T, search=F, multiple=T, themeDots=F, whole_node=T, themeIcons=F, theme='proton')),
    card(card_header(HTML("Select Gender Disaggregation <i>(Optional)</i>")),
         checkboxGroupInput("genders",label="", choices=genders)),
    card(card_header(HTML("Select Farm Size Disaggregation <i>(Optional)</i>")),
         checkboxGroupInput("farmsizes",label="", choices=farm_sizes))
    ),
  
  dataTableOutput("dataTab")
)

# Define server logic required to draw a histogram
server <- function(input, output) {

    output$countree <- renderTree(dfToTree(countree, c("Geography","Year")))
    output$indics <- renderTree(dfToTree(indiclist, c("indicatorcategory", "indicatorname")))
    outtable <- reactive({filterTable(indicators, get_selected(input$countree, format="slices"), get_selected(input$indics, format="slices"), input$genders, input$farmsizes)})
    output$dataTab <- renderDataTable(datatable(req(outtable()), extensions='Buttons', options=list(dom='Bfrtip', buttons=list(list(extend='colvis', text='Show/Hide Columns', columns=(1:26))))))
}


# Run the application 
shinyApp(ui = ui, server = server)
