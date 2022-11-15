
shinyUI(fluidPage(theme= "yeti.css",
                  includeJqueryUI(),
                  navbarPage("Regional Assessment Metadata Validation",
                             tabPanel("About",
                                      h3('Application Purpose'),
                                      p('This app was created to help assessors attach the correct AU and WQS information to
                                                   stations for each assessment cycle.'),
                                      p(strong('This application is intended only for Regional Assessment Staff. All other
                                                          users are encouraged to seek other applications to meet data analysis needs.')),
                                      
                                      h3('Application Last Updated'),
                                      p('November 15, 2022'),
                                      
                                      h3('Detailed Application Instructions:'),
                                      a(href="https://rconnect.deq.virginia.gov/WQAautomatedAssessmentUserManual/regional-metadata-validation-tool-how-to.html#regional-metadata-validation-tool-how-to",
                                        "Please see Chapter 4 of the DEQ Water Quality Automated Assessment User Guide", target="_blank"),
                                      
                                      h3("Notes for Assessment Staff"),
                                      p('- The Assessment Data Analyst has already run the joining and snapping scripts such
                                                   that the assessor is simply identifying the correct AU/WQS per the spatial snapping 
                                                   suggestion.'),
                                      p(strong('- New for IR2024: Citizen Monitoring and Non Agency stations are available for 
                                                          WQS review inside this application. Because this review process largely occurs prior to
                                                          data solicitation deadlines, the IR2022 citizen monitoring/non agency stations were
                                                          provided for WQS review. At present, there is no standardized
                                                          dataset to verify which IR2022 stations will be sampled in IR2024, so the provided stations
                                                          may not include data in IR2024. However, archiving this WQS information will expedite
                                                          future assessment processes should the station contain data in future assessment cycles.')),
                                      p('- The next expected application update will occur in March 2023 after the DEQ and citizen/non agency
                                                   monitoring data solicitation deadline. Stations provided in that update will include any DEQ stations 
                                                   that were sampled for the first time between October 15, 2022 and the data cutoff (December 31, 2022) 
                                                   as well as all Citizen/Non Agency monitoring stations sampled in 2021 and 2022 that were submitted 
                                                   through established data submission channels prior to the data solicitation deadline.'),
                                      h3("Questions?"),
                                      p("Please reach out to Emma Jones (emma.jones@deq.virginia.gov) should you have any questions regarding this
                                                   application or analysis.")
                             ),
                             navbarMenu("Assessment Unit QA",
                                        
                                        tabPanel("Watershed Selection",
                                                 h4('Assessment Unit QA'),
                                                 sidebarPanel(
                                                   selectInput('assessmentType','Assessment Type', choices = c('Riverine','Lacustrine','Estuarine')),
                                                   uiOutput('AUDEQregionSelection_'),
                                                   uiOutput('AUsubbasinSelection_'),
                                                   #dynamicSelectInput("DEQregionSelection", "Select DEQ Assessment Region", multiple = FALSE),
                                                   #dynamicSelectInput("basinSelection", "Select Major Basin", multiple = FALSE),
                                                   br(),
                                                   actionButton('begin', HTML("Begin Review With Selection <br/>(Clears Cached Results)"),class='btn-block')),
                                                 mainPanel(
                                                   leafletOutput('VAmap'),
                                                   br(),
                                                   h5(strong('Preprocessing Data Recap for Selected Region/Basin/Type Combination')),
                                                   fluidRow(column(4, textOutput('singleSnapSummary1')),
                                                            column(4, textOutput('snapTooManySummary1')),
                                                            column(4, textOutput('noSnapSummary1'))),
                                                   br(), br(), br(), # a little breathing room
                                                   verbatimTextOutput('test'),
                                                   verbatimTextOutput('test2')
                                                 )),
                                        tabPanel("Manual Review",
                                                 h4('Assessment Unit QA'),
                                                 wellPanel(
                                                   fluidRow(column(4, textOutput('singleSnapSummary2'),
                                                                   actionButton('plotSingleSnapSummary', HTML('Plot stations that snapped <br/>to 1 AU Segment'))),
                                                            column(4, textOutput('snapTooManySummary2'),
                                                                   actionButton('plotSnapTooManySummary', HTML('Plot stations that snapped <br/>to > 1 AU Segment'))),
                                                            column(4, textOutput('noSnapSummary2'),
                                                                   actionButton('plotNoSnapSummary', HTML('Plot stations that snapped <br/>to 0 AU segments'))))),
                                                 leafletOutput('AUmap'),
                                                 fluidRow(
                                                   actionButton('clear_allAU', 'Clear Selection', style='color: #fff; background-color: #337ab7; border-color: #2e6da4%', icon=icon('backspace')),
                                                   actionButton('acceptAU', 'Accept', style='color: #fff; background-color: #337ab7; border-color: #2e6da4%', icon=icon('check-circle')),
                                                   actionButton('changeAU', 'Manual AU Adjustment', style='color: #fff; background-color: #337ab7; border-color: #2e6da4%', icon=icon('exchange')),
                                                   #actionButton('checkMeOut', 'Check Me Out', style='color: #fff; background-color: #337ab7; border-color: #2e6da4%', icon=icon('exchange')),
                                                   actionButton('saveAU', label = "Export reviews",style='color: #fff; background-color: #228b22; border-color: #134e13')        ),
                                                 br(),
                                                 #verbatimTextOutput('testAU'),

                                                 tabsetPanel(
                                                   tabPanel(strong('Original User Uploaded and Existing Stations Data'),
                                                            br(),
                                                            h5(strong('Selected Station Information')),
                                                            DT::dataTableOutput('selectedSiteTableAU'),
                                                            h5(strong('Associated AU Information')),
                                                            DT::dataTableOutput('associatedAUTable'),
                                                            br(), br(), br()),
                                                   tabPanel(strong('Updated Stations Data'),
                                                            br(),
                                                            fluidRow(
                                                              h5(strong('Adjusted Station Data')),
                                                              div(DT::dataTableOutput("adjustedStationsTableAU"), style = "font-size:80%")),
                                                            fluidRow(
                                                              h5(strong('Manually QAed AU Information')),
                                                              div(DT::dataTableOutput("associatedAUTableAUQA"), style = "font-size:80%")),

                                                            br(), br(), br()
                                                   )))),
                             navbarMenu("Water Quality Standards QA",
                                        tabPanel("Watershed Selection",
                                                 h4('Water Quality Standards QA'),
                                                 sidebarPanel(
                                                   selectInput('WQSwaterbodyType','Select Waterbody Type', choices = unique(WQSlayerConversion$waterbodyType)),
                                                   uiOutput('WQSDEQregionSelection_'),
                                                   uiOutput('WQSsubbasinSelection_'),
                                                   hr(),
                                                   actionButton('WQSbegin', HTML("Begin Review With Subbasin Selection <br/>(Retrieves Last Saved Result)"),
                                                                class='btn-block')),
                                                 mainPanel(
                                                   leafletOutput('WQSVAmap'),
                                                   h5(strong('Preprocessing Data Recap for Selected Region/Subbasin/Type Combination')),
                                                   fluidRow(column(3, textOutput('singleSnapSummary1WQS')),
                                                            column(3, textOutput('snapTooManySummary1WQS')),
                                                            column(3, textOutput('noSnapSummary1WQS')),
                                                            column(3, textOutput('regionalSitesSummary1WQS'))),
                                                   # verbatimTextOutput('test1'),
                                                   
                                                   br()) ),
                                        tabPanel('Manual Review',
                                                 h4('Water Quality Standards QA'),
                                                 wellPanel(
                                                   fluidRow(column(4, textOutput('singleSnapSummary2WQS'),
                                                                   actionButton('plotSingleSnapSummaryWQS', HTML('Plot stations that snapped <br/>to 1 WQS Segment'))),
                                                            column(4, textOutput('snapTooManySummary2WQS'),
                                                                   actionButton('plotSnapTooManySummaryWQS', HTML('Plot stations that snapped <br/>to > 1 WQS Segment'))),
                                                            column(4, textOutput('noSnapSummary2WQS'),
                                                                   actionButton('plotNoSnapSummaryWQS', HTML('Plot stations that snapped <br/>to 0 WQS segments'))))),
                                                 #column(3, textOutput('regionalSitesSummary2WQS'),
                                                 #       actionButton('plotRegionalSitesSummaryWQS', HTML('Plot all stations in <br/>the selected Region/Basin'))))),
                                                 leafletOutput('WQSmap'),
                                                 fluidRow(
                                                   actionButton('clear_allWQS', 'Clear Selection', style='color: #fff; background-color: #337ab7; border-color: #2e6da4%', icon=icon('backspace')),
                                                   actionButton('acceptWQS', 'Accept', style='color: #fff; background-color: #337ab7; border-color: #2e6da4%', icon=icon('check-circle')),
                                                   actionButton('changeWQS', 'Manual WQS Adjustment', style='color: #fff; background-color: #337ab7; border-color: #2e6da4%', icon=icon('exchange')),
                                                   #actionButton('checkMeOutWQS', 'Check Me Out', style='color: #fff; background-color: #337ab7; border-color: #2e6da4%', icon=icon('exchange')),
                                                   actionButton("saveWQS", label = "Export reviews",style='color: #fff; background-color: #228b22; border-color: #134e13')        ),
                                                 #downloadButton('downloadWQS', label = "Export reviews",style='color: #fff; background-color: #228b22; border-color: #134e13')        ),
                                                 br(),
                                                 tabsetPanel(tabPanel(strong('Stations Data and Spatially Joined WQS'),
                                                                      br(),
                                                                      h5(strong('Selected Station Information')),
                                                                      DT::dataTableOutput('selectedSiteTableWQS'), br(),
                                                                      h5(strong('Spatially Joined WQS Information')),
                                                                      DT::dataTableOutput('associatedWQSTableWQS'),
                                                                      br(), br(), br()),
                                                             tabPanel(strong('Updated Stations Data and Manually QAed WQS'),
                                                                      br(),
                                                                      fluidRow(
                                                                        h5(strong('Adjusted Station Data')),
                                                                        div(DT::dataTableOutput("adjustedStationsTableWQS"), style = "font-size:80%")),
                                                                      fluidRow(
                                                                        h5(strong('Manually QAed WQS Information')),
                                                                        div(DT::dataTableOutput("associatedWQSTableWQSQA"), style = "font-size:80%")),
                                                                      
                                                                      
                                                                      br(), br(), br() ))))
                             
                  )))
