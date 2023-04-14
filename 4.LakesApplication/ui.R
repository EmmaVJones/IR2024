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
                      navbarPage(paste("VDEQ ",assessmentCycle," IR Lakes Assessment Tool", sep=''),
                                 # add back in
                                 
                                 tabPanel('Data Upload',
                                          h3('Tool Overview'),
                                          p(paste0("The Lacustrine Assessment Tool is designed to expedite analysis, assessment
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
                                          p('This application represents the fourth iteration of a riverine automated assessment tool. The datasets
                                            and parameters chosen for analysis readily lend themselves to automated processing. Future versions
                                            of the tool may include additional datasets and analyses.'),
                                          h3('User Resources'),
                                          HTML("<p>For detailed instructions on how to use the tool, please see the
                                               <a href='https://rconnect.deq.virginia.gov/WQAautomatedAssessmentUserManual/lacustrine-application-how-to.html' target='_blank'>
                                               Lacustrine Application How To</a></p>"),
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
                                            tags$li('Conventionals- CEDS data pulled by Roger Stewart (roger.stewart@deq.virginia.gov) for each 
                                                    Integrated Report data window.'), 
                                            tags$li('Citizen and Non Agency Monitoring- non-DEQ data pulled, organized into the "conventionals" data format, 
                                                    and QAed by Reid Downer (horace.downer@deq.virginia.gov) for each Integrated Report data window.'), 
                                            tags$li('Water Column and Sediment Metals- CEDS data pulled by Roger Stewart for each 
                                                    Integrated Report data window.'), 
                                            tags$li('Fish Tissue- metals and PCB data QAed by Gabe Darkwah (gabriel.darkwah@deq.virginia.gov) and organized by 
                                                    Joe Famularo (joseph.famularo@deq.virginia.gov) for each Integrated Report data window.'), 
                                            tags$li("Statewide Assessment (spatial) layer- The spatial dataset that identifies each regional office's watersheds."),
                                            tags$li("CEDS EDAS- Statewide macroinvertebrate data and stream condition indices are pulled and organized for the 
                                                    Integrated Report data window. Contact Emma Jones (emma.jones@deq.virginia.gov) and Jason Hill (jason.hill@deq.virginia.gov) 
                                                    or your regional biologist with dataset questions.")),
                                          br(),
                                          h4('User Defined Tool Inputs'),
                                          p('In order to allow for more flexible assessment unit updates as required throughout the assessment process,
                                             users must upload a regional Stations Table (generated by the automated assessment scripts).'),
                                          h5('Stations Table- Generated by the Automated Assessment Tool'),
                                          helpText('This dataset is derived before any Lacustrine Assessment Tool analysis 
                                                   procedures can commence using information provided by assessors from the ',
                                                   span(strong(a('Regional Assessment Metadata Validation Tool.', 
                                                                 href = "https://rconnect.deq.virginia.gov/RegionalAssessmentMetadataValidationIR2024/", target="_blank"))), 
                                                   'After completing the necessary WQS and AU attribution steps overviewed in the ',
                                                   span(strong(a('Regional Assessment Metadata Validation Tool.', 
                                                                 href = "https://rconnect.deq.virginia.gov/RegionalAssessmentMetadataValidationIR2024/", target="_blank"))),
                                                   'once, users are provided an initial automated assessment (Stations Table) to upload to this application and/or
                                                   the ',span(strong('CEDS WQA Stations Table Bulk Upload')), ' tool. Should any assessment 
                                                   units change throughout the assessment process, users have the ability to update their local
                                                   Stations Table with the required changes and upload that new dataset to this application. With that updated information, this tool will
                                                   reflect the changes to the user. This process may be repeated as many times as is necessary 
                                                   throughout the assessment process. After stations and AUs are reviewed, users may upload
                                                   their Stations Table to the ', span(strong('CEDS WQA Stations Table Bulk Upload')), ' tool.'),
                                          helpText(strong('A note on Inversioning spatial data: '),"If assessment unit changes are necessary throughout
                                                   the course of the assessment process, the user should update their local AU spatial dataset, sync 
                                                   these changes to the statewide spatial datasest, and export a local copy of this updated spatial
                                                   dataset for use in the assessment applications. Because the spatial layers are stored on the R server, 
                                                   users will need to send that updated spatial layer to Emma Jones (emma.jones@deq.virginia.gov) 
                                                   in order for the applications to reflect these changes. Users should also alter the assessment unit name
                                                   attributed to the station(s) in the Station Table to ensure the assessment applications can appropriately
                                                   reorganize data according to the user's changes."),
                                          fileInput('stationsTable','Upload your Regional Stations Table.', accept = c(".csv")),
                                          helpText('If this is your first time using the tool, please download a copy of the latest automated station
                                                   table output for upload to the tool. You may manipulate this .csv to only reflect your specific region.'),
                                          fluidRow(
                                            downloadButton('downloadTemplate',"Download statewide example dataset to upload to the tool."),
                                            uiOutput('templateLastUpdated_')),
                                          br(), br(), br()),
                                 tabPanel('Lake Selection',
                                          sidebarPanel(
                                            p('Run once per user session.'),
                                            helpText("Select Region to Assess and click ",span(strong("Retrieve Assessment Units From Server")), "button
                                                     to bring in spatial data for further analyses."),
                                            uiOutput('DEQregionSelectionUI'),
                                            br(),
                                            actionButton('pullAUs',"Retrieve Assessment Units From Server",class='btn-block'),
                                            hr(),
                                            p('Run as often as necessary per user session.'),
                                            uiOutput('lakeSelection_'),
                                            #dynamicSelectInput("lakeSelection", "Select Lake", multiple = FALSE),
                                            helpText('To begin assessing the selected lake, click the ',
                                                     span(strong('Assessment Unit Review')), ' tab at the top of the navigation bar.')),
                                          mainPanel(
                                            leafletOutput('VAmap'),
                                            br(),
                                            h5(strong('Assessment Units in Selected Lake')),
                                            DT::dataTableOutput('AUSummary'),
                                            br(),
                                            h5(strong('Stations in Selected Lake that were sampled in current window')),
                                            helpText("The stations highlighted in gray can be analyzed by the application. The stations
                                                     highlighted in yellow were sampled in the current window, but cannot be analyzed by the
                                                     application because they are not in the input stations table."),
                                            DT::dataTableOutput('stationSummary'),
                                            br(),
                                            h5(strong("Stations in Selected Lake that have no data in the current window but were carried
                                                      over from last cycle due to an IM designation in one of the 2020IR status fields or
                                                      the 2020 stations table reports the station was carried over from a previous cycle.")),
                                            helpText('These stations can be viewed in the application and stations table, but none of the
                                                     parameter modules will display data as no data is available in the current window.'),
                                            DT::dataTableOutput('carryoverStationSummary'),

                                            br(), br(), br() # a bit of breathing room
                                          )),
                                 tabPanel('Assessment Unit Review',
                                          DT::dataTableOutput('selectedLake'),
                                          hr(),
                                          uiOutput('AUselection_'),
                                          h5(strong('AU information from last cycle')),
                                          DT::dataTableOutput('selectedAU'),br(),
                                          uiOutput('stationSelection_'),
                                          fluidRow(column(4, DT::dataTableOutput('stationInfo')),
                                                   column(4, leafletOutput('stationMap', height = 300, width = 300),
                                                          helpText("The AUs displayed on the map above represent all AUs associated with the selected
                                                                  station (listed in a station's ID305B_1:ID305B_10 fields) for context. ")),
                                                   column(4,
                                                          tabsetPanel(
                                                            tabPanel(paste0(as.numeric(assessmentCycle) - 2,' Station Table'),
                                                                     DT::dataTableOutput('stationHistoricalInfo1')),
                                                            tabPanel(paste0(as.numeric(assessmentCycle) - 4,' Station Table'),
                                                                     DT::dataTableOutput('stationHistoricalInfo2'))))),
                                          hr(),
                                          h3('Station Results for Review'),
                                          helpText('This table outputs the site specific results for direct export to the Station Table. It also serves to highlight
                                                  where exceedances are present and should be reviewed in the individual parameter visualization tabs below.'),
                                          h4('Stations Table Results'),
                                          helpText('Parameters are highlighted
                                                  in different colors to indicate further review may be necessary. Parameters highlighted in yellow have at least one
                                                  violation of a standard. Parameters highlighted in red exceed the 10.5% exceedance rate. Both scenarios warrant further
                                                  investigation and may requre comments in the Station Table and CEDS WQA.'),
                                          h5(strong('If no station table appears, then there is no data within the assessment window for the selected station.'),
                                             'Please investigate the Historical Station Information table above for information as to why this station is
                                             included in the application.'),
                                          DT::dataTableOutput('stationTableDataSummary'), br(),
                                          h4('PWS Criteria'),
                                          h3('PWS Criteria have moved to the Toxics tab; regardless, if a station is within 100 meter of a drinking water intake,
                                             this information is flagged below.'),
                                          uiOutput('intakeProximityFlag'),
                                          uiOutput('nonConventionalsStationFlag'),
                                          # helpText(span("PWS assessments should noted in a station's COMMENT field of the Stations Table. The table below organizes
                                          #         PWS information to expedite the comment process.",
                                          #         strong('Note: Chloride, Sulfate, Total Dissolved Solids, Iron, and Foaming Agents are secondary criteria and are
                                          #                only applicable to data collected at the drinking water intake.'))),
                                          # uiOutput('intakeProximityFlag'),
                                          # DT::dataTableOutput('PWStable'),
                                          br(),hr(),br(),
                                          h3('Assessment Unit Raw Data Review and Visualization'),
                                          tabsetPanel(
                                            tabPanel('Conventionals Data',
                                                     tabsetPanel(
                                                       tabPanel('Raw Data',br(),
                                                                uiOutput("AURawDataFlag"),
                                                                DT::dataTableOutput('AURawData'),
                                                                h4('Data Summary'),
                                                                h5('Records Retrieved in Assessment Unit:'),
                                                                fluidRow(column(1),column(10,textOutput('stationDataTableRecords'))),
                                                                h5('Field and Lab Data in Assessment Window:'),
                                                                fluidRow(column(1),column(10,tableOutput('uniqueStationDataTableRecords'))),
                                                                # h5('Assessment Window:'),
                                                                # fluidRow(column(1),column(10,textOutput('stationDataTableAssessmentWindow'))), 
                                                                br(),br()),
                                                       tabPanel('Thermocline',
                                                                HTML("<p>For detailed explanations on how individual parameters are calculated using the automated assessment process,
                                                                     please see the <a href='https://rconnect.deq.virginia.gov/WQAautomatedAssessmentUserManual/individual-parameter-analyses.html#individual-parameter-analyses' target='_blank'>Automated Assessment User Guide</a></p>"),
                                                                helpText('Review each site using the single site visualization section. The results from this analysis are carried
                                                                         over to temperature and pH assessment decisions.'),
                                                                thermoclinePlotlySingleStationUI('thermocline')),
                                                       tabPanel('Temperature',
                                                                HTML("<p>For detailed explanations on how individual parameters are calculated using the automated assessment process,
                                                                     please see the <a href='https://rconnect.deq.virginia.gov/WQAautomatedAssessmentUserManual/individual-parameter-analyses.html#individual-parameter-analyses' target='_blank'>Automated Assessment User Guide</a></p>"),
                                                                helpText('Review each site using the single site visualization section. The results from this analysis are reflected
                                                                         in the TEMP_EXC, TEMP_SAMP, and TEMP_STAT columns in the station table.',
                                                                         span('Users may view AU level assessment results below.', style="color:red")),
                                                                temperaturePlotlySingleStationUI('temperature')),
                                                       tabPanel('pH',
                                                                HTML("<p>For detailed explanations on how individual parameters are calculated using the automated assessment process, 
                                                                     please see the <a href='https://rconnect.deq.virginia.gov/WQAautomatedAssessmentUserManual/individual-parameter-analyses.html#individual-parameter-analyses' target='_blank'>Automated Assessment User Guide</a></p>"),
                                                                helpText('Review each site using the single site visualization section. The results from this analysis are reflected
                                                                         in the PH_EXC, PH_SAMP, and PH_STAT columns in the station table.',
                                                                         span('Users may view AU level assessment results below.', style="color:red")),
                                                                pHPlotlySingleStationUI('pH')),
                                                       tabPanel('Dissolved Oxygen',
                                                                HTML("<p>For detailed explanations on how individual parameters are calculated using the automated assessment process, 
                                                                     please see the <a href='https://rconnect.deq.virginia.gov/WQAautomatedAssessmentUserManual/individual-parameter-analyses.html#individual-parameter-analyses' target='_blank'>Automated Assessment User Guide</a></p>"),
                                                                helpText('Review each site using the single site visualization section. The results from this analysis are reflected
                                                                         in the DO_EXC, DO_SAMP, and DO_STAT columns in the station table.',
                                                                         span('Users may view AU level assessment results below.', style="color:red")),
                                                                DOPlotlySingleStationUI('DO')),
                                                       tabPanel('Specific Conductance',
                                                                HTML("<p>For detailed explanations on how individual parameters are calculated using the automated assessment process, 
                                                                     please see the <a href='https://rconnect.deq.virginia.gov/WQAautomatedAssessmentUserManual/individual-parameter-analyses.html#individual-parameter-analyses' target='_blank'>Automated Assessment User Guide</a></p>"),
                                                                helpText('Review each site using the single site visualization section. There are no WQS for Specific Conductivity.'),
                                                                SpCondPlotlySingleStationUI('SpCond')),
                                                       tabPanel('E. coli',
                                                                tabsetPanel(
                                                                  tabPanel('Single Station Analysis',
                                                                           EcoliPlotlySingleStationUI('Ecoli')),
                                                                  tabPanel('Assessment Unit Analysis',
                                                                           EcoliPlotlyAUUI('EcoliAU')))),
                                                       tabPanel('Chlorophyll a',
                                                                HTML("<p>For detailed explanations on how individual parameters are calculated using the automated assessment process, 
                                                                     please see the <a href='https://rconnect.deq.virginia.gov/WQAautomatedAssessmentUserManual/individual-parameter-analyses.html#individual-parameter-analyses' target='_blank'>Automated Assessment User Guide</a></p>"),
                                                                helpText('Review each site using the single site visualization section. The results from this analysis are reflected
                                                                         in the NUT_CHLA_EXC, NUT_CHLA_SAMP, and NUT_CHLA_STAT columns in the station table.',
                                                                         span('Users may view AU level assessment results below.', style="color:red")),
                                                                chlAPlotlySingleStationUI('chlA')),
                                                       tabPanel('Total Phosphorus',
                                                                HTML("<p>For detailed explanations on how individual parameters are calculated using the automated assessment process, 
                                                                     please see the <a href='https://rconnect.deq.virginia.gov/WQAautomatedAssessmentUserManual/individual-parameter-analyses.html#individual-parameter-analyses' target='_blank'>Automated Assessment User Guide</a></p>"),
                                                                helpText('Review each site using the single site visualization section. The results from this analysis are reflected
                                                                         in the NUT_TP_EXC, NUT_TP_SAMP, and NUT_TP_STAT columns in the station table.',
                                                                         span('Users may view AU level assessment results below.', style="color:red")),
                                                                TPPlotlySingleStationUI('TP')),
                                                       tabPanel('Trophic State Index',
                                                                HTML("<p>For detailed explanations on how individual parameters are calculated using the automated assessment process, 
                                                                     please see the <a href='https://rconnect.deq.virginia.gov/WQAautomatedAssessmentUserManual/individual-parameter-analyses.html#individual-parameter-analyses' target='_blank'>Automated Assessment User Guide</a></p>"),
                                                                helpText('Review each site using the single site visualization section. The results from this analysis are not reflected
                                                                         in the station table as they are only applicable to non Section 187 lakes.',
                                                                         span('Users may view AU level assessment results below.', style="color:red")),
                                                                TSIPlotlySingleStationUI('TSI') ),
                                                       tabPanel('Ammonia',
                                                                HTML("<p>For detailed explanations on how individual parameters are calculated using the automated assessment process, 
                                                                     please see the <a href='https://rconnect.deq.virginia.gov/WQAautomatedAssessmentUserManual/individual-parameter-analyses.html#individual-parameter-analyses' target='_blank'>Automated Assessment User Guide</a></p>"),
                                                                helpText('Review each site using the single site visualization section. The results from this analysis are reflected
                                                                         in the AMMONIA_EXC and AMMONIA_STAT columns in the station table.'),
                                                                AmmoniaPlotlySingleStationUI('Ammonia')),
                                                       tabPanel('Nitrate',
                                                                HTML("<p>For detailed explanations on how individual parameters are calculated using the automated assessment process, 
                                                                     please see the <a href='https://rconnect.deq.virginia.gov/WQAautomatedAssessmentUserManual/individual-parameter-analyses.html#individual-parameter-analyses' target='_blank'>Automated Assessment User Guide</a></p>"),
                                                                helpText('Review each site using the single site visualization section. Nitrate criteria only apply to stations with PWS designation.'),
                                                                NitratePlotlySingleStationUI('Nitrate')),
                                                       tabPanel('Chloride',
                                                                HTML("<p>For detailed explanations on how individual parameters are calculated using the automated assessment process, 
                                                                     please see the <a href='https://rconnect.deq.virginia.gov/WQAautomatedAssessmentUserManual/individual-parameter-analyses.html#individual-parameter-analyses' target='_blank'>Automated Assessment User Guide</a></p>"),
                                                                helpText('Review each site using the single site visualization section. Chloride PWS criteria only apply at intakes.'),
                                                                ClPlotlySingleStationUI('Cl')),
                                                       tabPanel('Sulfate',
                                                                HTML("<p>For detailed explanations on how individual parameters are calculated using the automated assessment process, 
                                                                     please see the <a href='https://rconnect.deq.virginia.gov/WQAautomatedAssessmentUserManual/individual-parameter-analyses.html#individual-parameter-analyses' target='_blank'>Automated Assessment User Guide</a></p>"),
                                                                helpText('Review each site using the single site visualization section. Sulfate PWS criteria only apply at intakes.'),
                                                                DSulfatePlotlySingleStationUI('DSulfate')))),
                                            tabPanel('Metals Data',
                                                     HTML("<p>For detailed explanations on how individual parameters are calculated using the automated assessment process,
                                                                     please see the <a href='https://rconnect.deq.virginia.gov/WQAautomatedAssessmentUserManual/individual-parameter-analyses.html#individual-parameter-analyses' target='_blank'>Automated Assessment User Guide</a></p>"),
                                                     metalsTableSingleStationUI('metals')),
                                            tabPanel('Toxics Data',
                                                     HTML("<p>For detailed explanations on how individual parameters are calculated using the automated assessment process, 
                                                                     please see the <a href='https://rconnect.deq.virginia.gov/WQAautomatedAssessmentUserManual/individual-parameter-analyses.html#individual-parameter-analyses' target='_blank'>Automated Assessment User Guide</a></p>"),
                                                     toxicsSingleStationUI('PBC')))


                                          ))))))