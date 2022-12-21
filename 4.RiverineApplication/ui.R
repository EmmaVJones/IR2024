shinyUI(fluidPage(theme="yeti.css",
                  shinyjs::useShinyjs(),
                  div(
                    id = "loading_page",
                    h1("Loading...")
                  ),
                  hidden(
                    div(
                      id = "main_content",
                      # suppress error messages as data loads, hacky
                      tags$style(type="text/css",
                                 ".shiny-output-error { visibility: hidden; }",
                                 ".shiny-output-error:before { visibility: hidden; }"
                      ),
                      navbarPage(paste("VDEQ ",assessmentCycle," IR Riverine Assessment Tool", sep=''),
                                 # add back in
                                 tabPanel('Data Upload',
                                          h3('Tool Overview'),
                                          p(paste0("The Riverine Assessment Tool is designed to expedite analysis, assessment
                                            decisions, and quality assurance/quality control (QA/QC) procedures for Virginia's "),
                                            assessmentCycle,
                                            " Integrated Report (IR). The data window analyzed covers January 1, ",
                                            year(assessmentPeriod)[1], " to December 31, ", year(assessmentPeriod)[2], 
                                            " . Users can expect significant time savings on repetitive procedures including: "),
                                          tags$ul(
                                            tags$li(" Raw data organization from disparate sources"),
                                            tags$li("Geospatial organization of stations by assessment unit"),
                                            tags$li("Water Quality Standards and criteria calculations"),
                                            tags$li("Enhanced, interactive data visualization.")),
                                          br(),
                                          p('This application represents the third iteration of a riverine automated assessment tool. The datasets
                                            and parameters chosen for analysis readily lend themselves to automated processing. Future versions
                                            of the tool may include additional datasets and analyses.'),
                                          h3('User Resources'),
                                          HTML("<p>For detailed instructions on how to use the tool, please see the
                                               <a href='https://rconnect.deq.virginia.gov/WQAautomatedAssessmentUserManual/riverine-application-how-to.html#riverine-application-how-to' target='_blank'>
                                               Riverine Application How To</a></p>"),
                                          HTML("<p>For detailed explanations on Virginia DEQ's automated assessment process, please see the 
                                               <a href='https://rconnect.deq.virginia.gov/WQAautomatedAssessmentUserManual/' target='_blank'>Automated Assessment User Guide</a></p>"),
                                          p('For feedback, troubleshooting, and questions regarding analyses or missing data, please contact ',
                                            strong('Emma Jones (emma.jones@deq.virginia.gov)')),
                                          
                                          br(),
                                          h3('Tool Inputs'),br(),
                                          h4('Prepopulated Tool Inputs'),
                                          p("Some data sources are compiled for users and programmed into the application, requiring no user manipulation.
                                            These datasets include: "),
                                          tags$ul(
                                            tags$li('Conventionals- CEDS data pulled and analyzed by Roger Stewart (roger.stewart@deq.virginia.gov) for each 
                                            Integrated Report data window.'), 
                                            tags$li('Water Column and Sediment Metals- CEDS data pulled, filtered, and analyzed by Roger Stewart for each 
                                            Integrated Report data window.'), 
                                            tags$li("Statewide Assessment (spatial) layer- The spatial dataset that identifies each regional office's watersheds."),
                                            tags$li("CEDS EDAS- Statewide macroinvertebrate data and stream condition indices are pulled and organized from the 
                                            Integrated Report data window. Contact Jason Hill (jason.hill@deq.virginia.gov) or your regional
                                                    biologist with dataset questions.")  ),
                                          br(),
                                          h4('User Defined Tool Inputs'),
                                          p('In order to allow for more flexible assessment unit updates as required throughout the assessment process,
                                             users must upload certain datasets that follow a specified template. These include their regional
                                             Stations Table (generated by the automated assessment scripts) and Regional Assessment Unit shapefiles.'),
                                          h5('Stations Table- Generated by the Automated Assessment Tool'),
                                          helpText('This dataset is derived before any Riverine Assessment Tool analysis 
                                                   procedures can commence using information provided by assessors from the ',
                                                   span(strong('Regional Assessment Metadata Validation Tool.')), 
                                                   'After completing the necessary WQS and AU attribution steps overviewed in the ',
                                                   span(strong('Regional Assessment Metadata Validation Tool')),'once, users are provided
                                                   an initial automated assessment (Stations Table) to upload to this application and/or
                                                   the ',span(strong('CEDS WQA Stations Table Bulk Upload')), ' tool. Should any assessment 
                                                   units change throughout the assessment process, users have the ability to update their local
                                                   Stations Table and Regional Assessment Units spatial layers with the required changes and 
                                                   upload those new datasets to this application. With that updated information, this tool will
                                                   reflect the changes to the user. This process may be repeated as many times as is necessary 
                                                   throughout the assessment process. After stations and AUs are reviewed, users may upload
                                                   their Stations Table to the ', span(strong('CEDS WQA Stations Table Bulk Upload')), ' tool.'),
                                          helpText(strong('A note on Inversioning spatial data: '),"If assessment unit changes are necessary throughout
                                                   the course of the assessment process, the user should update their local AU spatial dataset, sync 
                                                   these changes to the statewide spatial datasest, and export a local copy of this updated spatial
                                                   dataset for use in the assessment applications. Users should also alter the assessment unit name
                                                   attributed to the station(s) in the Station Table to ensure the assessment applications can appropriately
                                                   reorganize data according to the user's changes."),
                                          fileInput('stationsTable','Upload your Regional Stations Table.', accept = c(".csv")),
                                          helpText('If this is your first time using the tool, please download a copy of the latest automated station
                                                   table output for upload to the tool. You may manipulate this .csv to only reflect your specific region.'),
                                          fluidRow(
                                            downloadButton('downloadTemplate',"Download statewide example dataset to upload to the tool."),
                                            uiOutput('templateLastUpdated_')),
                                          br(),
                                          h5('Regional Assessment Units'),
                                          helpText(span('This shapefile is the current working copy of the regional assessment units.',
                                                        strong('It will be uploaded to the app on startup for you to expedite application rendering
                                                        time.'), ' Any changes to the regional dataset (e.g. split an assessment 
                                                        unit) should be synced with the statewide version and a copy of the ', strong('new spatial 
                                                        dataset should be sent to Emma Jones (emma.jones@deq.virginia.gov) to update on the server.')))
                                          #h6(strong('To view or change the location of the Regional Assessment Units shapefile sourced
                                          #          by the application, see the AUshapefileLocation.R script.'))
                                          #fileInput('regionalAUshapefile','Choose your Regional Assessment Unit shapefile.',
                                          #          accept = c(".shp",#)),# only need .shp for st_read 
                                          #                     ".dbf",".prj",".sbn",".sbx","shp.xml",".shx"), multiple = T),
                                 )
                      )
                    )
                  )
))
                                 