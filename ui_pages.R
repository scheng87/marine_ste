startp = function() 
fluidPage(
  tags$style(HTML("
                  body {
                        background-color: #ffffff;
                  }
                  .intro-divider {
                        height: 6px;
                        background: transparent linear-gradient(to right, #0e2f44 0%, #66cccc 100%) repeat scroll 0% 0%;
                        margin-top: 20px;
                        margin-bottom: 20px;
}
                  ")),
  fluidRow(
    column(8, offset=2,
           br(),
           p(div("Welcome to the data portal for Forests and Poverty (a sub-portal of the ", a("Evidence for Nature and People Data Portal",href="http://www.natureandpeopleevidence.org"),").", style="font-size:20px;color:#0e2f44;"),align="left"),
           div(class = "intro-divider"),
           br(),
           h3(div(strong("HOW TO USE"),style="color:#006699"),align="center"),
           hr(),
           p("In general, options for exploration are located in the ",span(strong("EXPLORE"),style="color:#006666")," section. In the ",span(strong("EVIDENCE MAP"),style="color:#006666")," tab, you will find an interactive evidence map that highlights the number of articles that document a specific linkage between a type of intervention and a type of outcome. You can filter this map by region, major habitat type, and study type. You can also view the data table behind this map by clicking on the ",span(strong("DATA TABLE"),style="color:#006666")," tab."),
           p("The ",span(strong("DASHBOARD"),style="color:#006666")," tab allows for filtering by geographic region, habitat/ecoregion type, intervention type, and outcome type. Here you can view summaries of different variables, explore the full data, and visualize spatially on both choropleth and interactive maps."),
           br(),
           h4(div(strong("FEEDBACK"))),
           p("We are constantly developing and honing features in order to tailor this tool for our end-users. Any feedback would be very much appreciated on functionality and utility. Please see the ",span(strong("CONTACT"),style="color:#006666")," tab to fill out a feedback form or you can directly email Samantha Cheng at ",a("cheng@nceas.ucsb.edu",href="cheng@nceas.ucsb.edu"),"."),
           p("For any questions or comments on the wildlife trade evidence map, please contact the project leads, ",a("Samantha Cheng",href="cheng@nceas.ucsb.edu"), " and ", a("Sofia Ahlroth",href="sahlroth@worldbank.org"))
    )
  )
)

contactp=function()
fluidPage(
  fluidRow(
    column(3
           ),
    column(6,
           h3(div(strong("CONTACT US"),style="color:#006699"),align="center"),
           br(),
           p("For more information on Conservation International, please visit our ",a("website",href="http://www.conservation.org/",".")),
           p("If you are interested in submitting evidence relevant to this topic, please contact ",a("Samantha Cheng",href="cheng@nceas.ucsb.edu")," and ",a("Michael Mascia",href="mmascia@conservation.org"),"."),
           br()
           ),
    column(3
           )
    ),
  fluidRow(
    align="center",
    h3(div(strong("FEEDBACK"),style="color:#006699")),
    tags$iframe(src="https://docs.google.com/forms/d/1fjqt6Zb1igR3UxrMcd4SbBmceZWocPAReT-REgOKYC0/viewform?embedded=true",width="760",height="1100",scrolling="no")
    )
  )

newsp=function()
  fluidPage(
    fluidRow(
      column(1
      ),
      column(10,
             fluidRow(
               h3(div(strong("RECENT NEWS"),style="color:#006699"),align="center"),
               hr(),
               br()
             ),
             fluidRow(
               column(4,
                      wellPanel(
                        style="background-color:#eef8f7;",
                        h4(div(strong("Evidence map published as first in Conservation International Working Paper Series"))),
                        p(div(em("July 25, 2017"),style="color:#FF6633")),
                        p("Evidence map details insights and patterns in the evidence base on the effectiveness of international wildlife trade practices and policies, along with the full dataset and bibliography. The data from this article forms the basis of this sub-portal."),
                        p(strong(a("Read more",href="https://conservation.org/wildlifetrade")))
                      )
               )
             )
      )
    )
  )
