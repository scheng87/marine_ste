elink = function()
  fluidPage(
    tags$head(tags$style(".rightAlign{float:right;}")),
    fluidRow(
      column(3,
          wellPanel(
            style="background-color:##d5e2ea",
            h4(div(strong("FILTERING OPTIONS"),style="color:#0e2f44")),
            selectInput("region1",
                               label="Choose region(s) to display:",
                        c("All","Africa","Asia","Europe","Latin America","Northern America","Oceania","Global","Unknown"),
                        selected=c("All")
            ),
            selectInput("studytype",
                        label="Choose study type to display:",
                        choices=list("All"="All","Experimental" = "exp", "Quasi-experimental" = "quas_exp","Non_experimental" = "non_exp","Systematic review" = "sys_rev","Impact evaluation"="imp_ev","Unknown"="NA"),
                        selected="All"
                        ),
            br(),
            h4(div(strong("DOWNLOAD OPTIONS"),style="color:#0e2f44")),
            hr(),
            downloadButton("downloadBiblio", "Bibliography as .csv"),
            br(),
            downloadButton("downloadFullData", "Data as .csv"),
            br()
           )
      ),
      column(9,
             fluidRow(
               column(3, offset=2,
                      align="center",
                      wellPanel(
                        style="background-color:#31698a;",
                        icon("file-text-o","fa-2x"),
                        p(div(strong("TOTAL ARTICLES:"),style="color:#ffffff")),
                        h4(div(strong(textOutput("elink_us")),style="color:#ff7f50"))
                      )
               ),
               column(3, offset=1,
                      align="center",
                      wellPanel(
                        style="background-color:#31698a;",
                        icon("file-text-o","fa-2x"),
                        p(div(strong("OPEN ACCESS:"),style="color:#ffffff")),
                        h4(div(strong(textOutput("elink_oa")),style="color:#ff7f50"))
                      )
               ),
               hr()
               ),
             fluidRow(
               column(12,
                      hr(),
                      tabsetPanel(
                        type="tabs",
                        tabPanel("Evidence Map",
                                 h5(div(em("Articles can fall into more than one linkage cell. Filter the evidence map by desired region and study type used on the left panel."))),
                                 plotOutput("heatmap",height=562,width=1000)
                        ),
                        tabPanel("Data Table",
                                 br(),
                                 h4("View associated data behind the filtered evidence map."),
                                 hr(),
                                 DT::dataTableOutput("map_data")
                        )
                      )
                      )
               )
             )
    )
  )

eintout = function()
  fluidPage(
    fluidRow(
      h3(div(strong("EXPLORE THE EVIDENCE MAP"),style="color:#006699"),align="center"),
      hr(),
      h5(div(em("Choose filters to display custom summary tables, datasets, and interactive maps.")))
    ),
    fluidRow(
      column(3, offset=2,
             align="center",
             wellPanel(
               style="background-color:#c7eae5;",
               icon("file-text-o","fa-2x"),
               p(div(strong("TOTAL ARTICLES:"))),
               h4(div(strong(textOutput("elink_us_2")),style="color:#FF6633"))
             )
      ),
      column(3, offset=1,
             align="center",
             wellPanel(
               style="background-color:#c7eae5;",
               icon("file-text-o","fa-2x"),
               p(div(strong("OPEN ACCESS:"))),
               h4(div(strong(textOutput("elink_oa_2")),style="color:#FF6633"))
             )
      )
    ),
    fluidRow(
      hr(),
      column(4,
             br(),
             p("GEOGRAPHIC FILTERS",style="font-size:12pt"),
             style="background-color:#40867a",
             p("Filter data by region, subregion, and country",style="font-size:9pt"),
             fluidRow(
               column(6,
                      selectInput("eintout_region","Region",
                                  c("All regions"="All",
                                    "Africa" = "Africa",
                                    "Asia" = "Asia",
                                    "Latin America & the Caribbean" = "Latin America",
                                    "Oceania" = "Oceania",
                                    "Europe" = "Europe",
                                    "Unknown" = "Unknown",
                                    "Global" = "Global"),
                                  selected="All"
                                  )
                      ),
               column(6,
                      selectInput("eintout_subreg","Subregion",c("All subregions"="All")
                                  )
                      )
             ),
             fluidRow(
               column(6, offset=3,
                      selectInput("eintout_country","Country",c("All countries"="All")
                                  )
                      )
             )
             ),
      column(4,
             fluidRow(
               style="background-color:#59BCAB",
               column(12,
                      br(),
                      p("STUDY DESIGN FILTERS",style="font-size:12pt"),
                      p("Filter data by study design and comparator type",style="font-size:9pt")
                      )
             ),
             fluidRow(
               style="background-color:#59BCAB",
               column(6, offset=3,
                      selectInput("eintout_design","Study design type",
                                  c("All"="All",
                                    "Experimental"="exp",
                                    "Quasi-experimental"="quas_exp",
                                    "Non-experimental"="non_exp",
                                    "Non-systematic review"="non_sys_rev",
                                    "Systematic review"="sys_rev",
                                    "Unknown" = "NA"),
                                  selected="All"
                                  ),
                      selectInput("eintout_comp","Comparator type",
                                  c("All"="All",
                                    "Before/after"="ba",
                                    "Presence/absence"="pa",
                                    "Continuous time series"="cont",
                                    "Punctuated time series"="punc",
                                    "Spatial distance"="spat",
                                    "Between groups"="between",
                                    "None"="None",
                                    "Other"="other",
                                    "Unknown"="NA"
                                  ),
                                  selected="All"
                                  )
                      )
             ),
             fluidRow(
               style="background-color:#ffffff",
               column(12,
                      align="center",
                      br(),
                      downloadButton("downloadBiblio_R", "Bibliography as .csv"),
                      downloadButton("downloadFullData_R", "Data as .csv"),
                      br()
                      )
             )
             ),
      column(4,
             br(),
             style="background-color:#9fe6da",
             p("EXPOSURE/OUTCOME FILTERS",style="font-size:12pt"),
             p("Filter by exposures and outcomes",style="font-size:9pt"),
             fluidRow(
               column(6,
                      selectInput("eintout_inttype","Exposure type",
                                  c("All"="All","Forest management"="for_mgmt","Agroforestry"="agrofor","Habitat management"="hab_mgmt","Governance"="gov","Individual rights/empowerment"="emp","Producted capital"="prod_cap","Human capital"="hum_cap","Social capital" = "soc_cap","Linked enterprises & livelihood alternatives"="liv_alt","Identifying and strengthening market forces"="market","Increasing access to markets"="mark_acc","Managing and enhancing ecosystem services"="eco_ser","Strengthening institutions and markets"="instit","Identifying non-monetary benefits"="non_mon"),
                                  selected="All"
                                  )
                      ),
               column(6,
                      selectInput("eintout_scale","Scale of intervention",
                                  c("All"="All", "International"="international","Regional"="region","National"="national","Subnational"="subnational","Local"="local","Unknown"="NA"),
                                  selected="All"
                                  )
                      )
             ),
             fluidRow(
               column(6,
                      selectInput("eintout_outgroup","Outcome group",
                                  c("All"="All","Forest income/consumption"="Forest income/consumption","Capital/assets"="Capital/assets"),
                                  selected="All"
                                  )
                      ),
               column(6,
                      selectInput("eintout_outtype","Outcome sub-type",
                                  c("All subtypes="="All")
                                  )
                      )
             )
             )
    ),
    fluidRow(
      hr(),
      tabsetPanel(
        type="tabs",
        tabPanel("Data Summary",
                 hr(),
                 fluidRow(
                   column(6,
                          wellPanel(
                            p(div(em("Type of conservation intervention")),align="center",style="font-size:9pt"),
                            plotlyOutput("e_int")
                            ),
                          br(),
                          wellPanel(
                            p(div(em("Types of outcome")),align="center",style="font-size:9pt"),
                            plotlyOutput("e_out")
                          )
                          ),
                   column(6,
                          wellPanel(
                            p(div(em("Study designs")),align="center",style="font-size:9pt"),
                            plotlyOutput("e_comp")
                          ),
                          wellPanel(
                            p(div(em("Direction of outcome")),align="center",style="font-size:9pt"),
                            plotlyOutput("e_eco")
                          )
                          )
                 ),
                 fluidRow(
                   column(6,
                          wellPanel(
                            p(div(em("Data type")),align="center",style="font-size:9pt"),
                            plotlyOutput("e_spec")
                          )
                          ),
                   column(6,
                          wellPanel(
                            p(div(em("Implementer type")),align="center",style="font-size:9pt"),
                            plotlyOutput("e_use")
                          )
                 )
                 )
        ),
        tabPanel("Data Table",
                 hr(),
                 DT::dataTableOutput("e_table")
                 ),
        tabPanel("Choropleth map",
                 fluidRow(
                   column(12,
                          br(),
                          h3(div(strong("GLOBAL DISTRIBUTION OF EVIDENCE BASE"),style="color:#006699"),align="center"),
                          hr(),
                          plotlyOutput("map_plotly")
                   )
                 )
                 ),
        tabPanel("Interactive map",
                 p("UPCOMING")
                 )
      )
    )
  )