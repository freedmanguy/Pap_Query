library(DT)
library(shiny)
library(shinyBS)
library(markdown)
library(lubridate)

shinyUI(           fluidPage(
  tags$head(
    tags$style(HTML("hr {border-top: 1px solid #000000;}"))
  ),
                     titlePanel("Querying PAP Coding"),
                     p(""),
                     sidebarLayout(
                       sidebarPanel(
                         selectInput("source", label = "Source", choices = c("State of the Union Speeches",
                                                                             "Presidential Veto Rhetoric",
                                                                             "Executive Orders",
                                                                             "Democratic Party Platform",
                                                                             "Republican Party Platform",
                                                                             "Congressional Bills",
                                                                             "Congressional Hearings",
                                                                             "Congressional Quarterly Almanac",
                                                                             "Congressional Research Service Reports",
                                                                             "Public Law Titles",
                                                                             "Public Laws",
                                                                             "Roll Call Votes",
                                                                             "Supreme Court Cases",
                                                                             "New York Times Front Page")),
                         textInput("search","Search Terms"),
                         textInput("exclude","AND NOT:", placeholder = "search terms to exclude"),
                         br(),
                         textInput("styear",label = "Start year:",value = 1944),
                         br(),
                         textInput("endyear",label = "End year:",value = year(Sys.Date())),
                         actionButton("Query","Query!"),
                         br(),
                         br(),
                         p("Like what you see? Download it!"),
                         downloadButton("dl", "Download")
                       ),
                       mainPanel(
                         p("Welcome!"),
                         p("This app provides the user with the option of reviewing coding according to the Policy Agendas Project, based on keywords."),
                         p("Click below for Instructions, or skip directly to searching in the sidebar on the left."),
                         br(),
                         p(tagList("This app was created by Guy Freedman for personal use. It is intended for users who are experienced with the Policy Agendas coding scheme. Like what you see? Have suggestions for improvements? Encountered any errors? Contact me at ",a("freedmanguy@utexas.edu",href = "mailto:freedmanguy@utexas.edu"))),
                         bsCollapse(id = "collapseExample", open = "Summary Results",
                                    bsCollapsePanel("Instructions",
                                                    tags$ol(
                                                      tags$li("Begin by choosing a source on the left."),
                                                      tags$li("Next, enter the keywords you would like to search. The app performs a search within the textual field of the chosen source and returns results based on all observations that include that keyword."),
                                                      tags$li("If you'd like to exclude certain terms, enter them in the AND NOT field, separated by commas; else leave empty."),
                                                      tags$li("No need to use wildcards; they are built into the query. I recommend usind stemmed version of keywords. For instance: 'immigr' will return results that include the terms 'immigration','immigrant','immigrants', etc. "),
                                                      tags$li("The search supports regular expressions. Some useful examples:"),
                                                      tags$ol(
                                                        tags$li("Place | between multiple terms to return at least one term. 'veto|immigration' will return all observations that include either 'veto' OR 'immigration' "),
                                                        tags$li("Place , between multiple terms to return all terms. 'veto,immigration' will return all observations that include 'veto' AND 'immigration' "),
                                                        tags$li("Place ^ at the start of a term to only include observations that begin with that term"),
                                                        tags$li("Place $ at the end of a term to only include observations that end with that term"),
                                                        tags$li(tagList("For additional tips on using regular expressions, try ",a("https://www.rexegg.com/regex-quickstart.html",href = "https://www.rexegg.com/regex-quickstart.html")))),
                                                      tags$li("By default, the query searches all available years for all datasets, reporting the available range in the results. You may specify a specific range on the left.")
                                                    ),
                                                    br(),
                                                    p(strong("After you hit the Query button, click on Summary Results and scroll down to see results or choose to view the raw data instead"), style = "color:red"),
                                                    style = "info"),
                                    bsCollapsePanel("Summary Results",
                                                    h3(textOutput("details")),
                                                    textOutput("source"),
                                                    uiOutput("url"),
                                                    br(),
                                                    textOutput("search"),
                                                    textOutput("exclude"),
                                                    textOutput("years"),
                                                    br(),
                                                    uiOutput("pap"),
                                                    hr(),
                                                    h3(textOutput("summary")),
                                                    tableOutput("results"),
                                                    plotOutput("sumplot"),
                                                    br(),
                                                    plotOutput("annualplot"), style = "primary"),
                                    bsCollapsePanel("Raw Data",
                                                    DTOutput("raw"), style = "success"))
                         
                         
                        
                       ) 
                     )
                   )
   )


