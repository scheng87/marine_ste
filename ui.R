##Interactive webportal for international wildlife trade evidence map

##ui

#ui for beta tool
source("sources.R")
source("ui_pages.R")
source("ui_explore.R")

shinyUI(
  fluidPage(
    theme="bootstrap.css",
    tags$head(tags$style(HTML("
                              .navbar .navbar-nav {float: right}
                              .navbar .navbar-header {float: right}
                              .navbar .navbar-nav {background-color: #ff7f50}
                              .navbar .navbar-header {background-color: #ff7f50}
                              .navbar {background-color: #ff7f50}
                              .navbar .navbar-default {background-color: #ff7f50}
                              .navbar-collapse {background-color: #994C30}
                              div.hcon {
                              background-color: rgba(0, 0, 0, 0.5);
                              width: 675px;
                              padding: 10px 20px 10px 20px;}
                              div.hcon > h1 {
                              font-weight: 200;
                              color: white;
                              margin-bottom: 0px;
                              padding-top: 0px}
                              "))
    ),
    fluidRow(
      column(12,
             div(class="banner_portal",style="padding:20px 20px 20px 20px",
                 column(8,
                        align="left",
                        br(),
                        br(),
                        br(),
                        br(),
                        div(class="hcon",h1(div(strong("FORESTS & POVERTY"))),p(div(em("EVIDENCE PORTAL"),style="color:#fa8072;font-size:30px")))
                 ),
                 column(4,
                        align="right",
                        br(),
                        br(),
                        br(),
                        br(),
                        br(),
                        br()
                 )
             )
      )
    ),
    navbarPage("",
               id="mainpage",
               tabPanel("HOME",
                        startp()
               ),
               navbarMenu("EXPLORE BY:",
                          tabPanel("Evidence Map",
                                   elink()
                                   ),
                          tabPanel("Dashboard",
                                   eintout()
                                   )
               )
    )
    )
)