library(shinydashboard)
library(shiny)
library(ggplot2)

organisms <- c("gga - Gallus gallus (chicken)",
               "bmor - Bombyx mori (domestic silkworm)",
               "acb - Acinetobacter baumannii ATCC 17978",
               "sly - Solanum lycopersicum (tomato)",
               "ssc - Sus scrofa (pig)",
               "ecb - Equus caballus (horse)",
               "hsa - Homo sapiens (human)",
               "hsa2022 - Homo sapiens version 2022 (human)",
               "hsa2018 - Homo sapiens version 2018 (human)",
               "hsa2015 - Homo sapiens version 2015 (human)",
               "cel - Caenorhabditis elegans (nematode)",
               "mcc - Macaca mulatta (rhesus monkey)",
               "bta - Bos taurus (cow)",
               "bna - Brassica napus (rape)",
               "ath - Arabidopsis thaliana (thale cress)",
               "mmu - Mus musculus (house mouse)",
               "dre - Danio rerio (zebrafish)",
               "cfa - Canis lupus familiaris (dog)",
               "oas - Ovis aries (sheep)",
               "brp - Brassica rapa (field mustard)",
               "tgu - Taeniopygia guttata (zebra finch)",
               "pau - Pseudomonas aeruginosa UCBPP-PA14",
               "ola - Oryzias latipes (Japanese medaka)",
               "vvi - Vitis vinifera (wine grape)",
               "pfa - Plasmodium falciparum 3D7",
               "xla - Xenopus laevis (African clawed frog)",
               "ptr - Pan troglodytes (chimpanzee)",
               "rno - Rattus norvegicus (rat)",
               "pps - Pan paniscus (bonobo)",
               "xtr - Xenopus tropicalis (tropical clawed frog)",
               "boe - Brassica oleracea (wild cabbage)",
               "dme - Drosophila melanogaster (fruit fly)"
)


ui <- dashboardPage(
  dashboardHeader(title = "MITHrIL"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Run MITHrIL", tabName = "mithril", icon = icon("dashboard")),
      menuItem("Visualize Data", tabName = "visualizedata", icon = icon("th"))
    )
  ),
  dashboardBody(
    tabItems(
      # First tab content
      tabItem(tabName = "mithril",
          fluidRow(
          tabBox(
          tabPanel("Base MITHrIL",
           status = "primary",
           solidHeader = TRUE,
             fileInput("upload", "Upload a a tab-separated input file:", accept = ".txt"),
             textOutput("inputexplain"),
             textInput("outputpath", "Enter the output file path:", value = "./Output", width = 800),
             selectInput("organism", "Organism used for the analysis:", organisms, selected = "hsa - Homo sapiens (human)", width = 800),
             actionButton("runmithril", "Run MITHrIL")
           ),
          tabPanel("Advanced Options",
           status = "success",
           solidHeader = TRUE,
             selectInput("adjusterVAL", "1. Select p-value adjustment method:", c("Bonferroni", "Holm", "Hochberg", "BH", "BY", "None"), 
                         selected = "BH", width = 300),
             selectInput("combinerVal", "2. Select p-value combination method:", c("Fisher", "Stouffer", "Mean", "Logit", "Wilkinson", "SumOfP", "VoteCounting"),
                         selected = "Stouffer"),
             
             
             radioButtons("binary", "3. Should output be stored in binary format?", c("Not Binary" = "",
                                                                                      "Binary" = " -b "), width = 800),
             radioButtons("decoys", "4. Adds decoy pathways:", c("Do not add decoys" = "", 
                                                                 "Add decoys" = " -decoys ")),
             radioButtons("enrichment", "5. Type of of minimal evidence used when enriching pathways:", c("STRONG" = " -enrichment-evidence-type STRONG ", 
                                                                                                          "WEAK" = " -enrichment-evidence-type WEAK ",
                                                                                                          "PREDICTION" = " -enrichment-evidence-type PREDICTION ",
                                                                                                          "UNKNOWN" = " -enrichment-evidence-type UNKNOWN ", 
                                                                                                          "NONE" = ""), selected =  "STRONG", width = 450),
             radioButtons("metapath", "6. Run all algorithms on a meta-pathway obtained by merging all pathways in the internal repository:", c("Do not merge" = "",
                                                                                                                                                "Merge" = " -m "), width = 800),
             textInput("excludeCAT", "7. List of pathway categories (separated by comma) to exclude when building the meta-pathway environment. Requires merging all pathways in the internal repository (Setting 6):", width = 800),
             textInput("includeCAT", "8. List of pathway categories (separated by comma) to use when building the meta-pathway environment. Only pathways contained in one of these categories will be included in the computation. Requires merging all pathways in the internal repository (Setting 6):", width = 800),

             fileInput("excludePATH", "9. Select a file containing a list of pathways excluded when building the meta-pathway:", accept = ".txt", width = 800),
             fileInput("includePATH", "10. Select a file containing the list of pathways used when building the meta-pathway. Requires merging all pathways in the internal repository (Setting 6):", accept = ".txt", width = 800),
             fileInput("filterOUT", "11. Select a file containing a list of pathways to be shown in the output files:", accept = ".txt", width = 800),
             radioButtons("nocomplete", "12. Complete pathways with missing elements:", c("Complete" = "", 
                                                                                                 "Do not complete" = " -no-complete "), width = 800),
             numericInput("iterationN", "13. Number of iterations for the p-value computation", value = 2001, min = 0, width = 800),
             numericInput("seed", "14. For experimental reproducibility, sets the seed of the random number generator", value = 123, min = 0, width = 800),
             numericInput("threads", "15. Number of threads. If 0 the number is automatically detected:", value = 0, min = 0, width = 800),
             radioButtons("verbose", "16. Show verbose computational outline", c("Not Verbose" = "", "Verbose" = " -verbose "), width = 800),
             textInput("weightcompute", "17. Name of the weight computation method used", value = "default", width = 800),
             radioButtons("noenrich", "18. Disable pathway enrichment with miRNAs. Requires Enrichment set to 'NONE' (Setting 5):", c("Do not disable" ="", "Disable" = " -no-enrichment "), width = 800),
             
             
             
             actionButton("runmithril1", "Run MITrIL")
    )
             ),
      )
      ),
      
      # Second tab content
      tabItem(tabName = "visualizedata",
              fluidRow(
              box(
                title = "Controls",
                width = 3,
                status = "primary", solidHeader = TRUE,
                fileInput("datatoviz", "Please select a file: ", accept = ".txt"),
                checkboxInput("table", "Display Table"),
                checkboxInput("hist", "Display  Histogram")
                
              ),
              box(
                title = "Visualization",
                status = "success", solidHeader = TRUE,
                width = 9,
                conditionalPanel(
                  condition = "input.table == TRUE",
                  tableOutput("tableplot"),
                ),
                conditionalPanel(
                  condition = "input.hist == TRUE",
                  plotOutput("histplot")
                )
                
              )
              )
      )
    )
  )
)


server <- function(input, output, session) {
  
  ###### Run Mithril Server Side
  
  output$inputexplain <- renderText("Upload a a tab-separated input file where each line contains a node identifier (gene, microRNA, metabolite) and its Log-Fold-Change for each experiment.
                                    If the Log-Fold-Change is absent or zero, the gene is assumed as non-differentially expressed.
                                    Genes are identified by EntrezId, microRNA by mature name (miRBase release 21), metabolites or chemicals by KEGG id.
                                    The first line of the file MUST contain names for each experiment. No spaces or symbols are allowed for experiment names.")
  
  # Base Mithril
  filepath <- reactive(input$upload$datapath)
  outputpath <- reactive(input$outputpath)
  organism <- reactive(substr(input$organism, 1, unlist(gregexpr(' ', input$organism))[1]))
  
  # Advanced settings
  
  
  adjusterVAL <- reactive(input$adjusterVAL)
  combinerVAL <- reactive(input$combinerVal)
  binary <- reactive(input$binary)
  decoys <- reactive(input$decoys)
  enrichment <- reactive(input$enrichment)
  
  metapath <- reactive(input$metapath)
  
  excludeCAT <- reactiveValues(excat = NULL)
  observeEvent(input$excludeCAT, {
    if (input$excludeCAT == "") {
      excludeCAT$excat <- ""
    } else {
      excludeCAT$excat <- paste0(" -include-categories ", input$excludeCAT)}
  }, ignoreInit = TRUE)
  
  includeCAT <- reactiveValues(incat = NULL)
  observeEvent(input$includeCAT, {
    if (input$includeCAT == "") {
      includeCAT$incat <- ""
    } else {
      includeCAT$incat <- paste0(" -include-categories ", input$includeCAT)}
  }, ignoreInit = TRUE)
  
  expath <- reactiveValues(expat=NULL)
  observeEvent(input$excludePATH, {
    expath$expat <- paste0(" -exclude-pathways ", input$excludePATH$datapath)
  }, ignoreInit = TRUE)
  
  incpath <- reactiveValues(inpath=NULL)
  observeEvent(input$includePATH, {
    incpath$inpath <- paste0(" -include-pathways ", input$includePATH$datapath)
  }, ignoreInit = TRUE)
  
  filter <- reactiveValues(filterout=NULL)
  observeEvent(input$filterOUT, {
    filter$filterout <- paste0(" -filter-output ", input$filterOUT$datapath)
  }, ignoreInit = TRUE)
  
  nocomplete <- reactive(input$nocomplete)
  noenrich <- reactive(input$noenrich)
  
  iterationN <- reactive(paste0(" -number-of-iterations ", input$iterationN))
  seed <- reactive(paste0(" -seed ", input$seed))
  threads <- reactive(paste0(" -threads ", input$threads))
  
  verbose <- reactive(input$verbose)
  
  weightcomp <- reactive(paste0(" -weight-computation-method ", input$weightcompute))
  
  # Running mithril on button press
  observeEvent(input$runmithril | input$runmithril1, {
    system(paste0("powershell -command \"java -jar ./mithril-standalone-mithril-2/Built/MITHrIL2.jar batch-mithril -i ", filepath(), " -o ", outputpath() ," -organism ", organism(), 
                  " -adjuster ", adjusterVAL(), " -combiner ", combinerVAL(), binary(), decoys(), enrichment(), 
                  metapath(), excludeCAT$excat, includeCAT$incat, expath$expat, incpath$inpath, filter$filterout, verbose(),
                  nocomplete(), iterationN(), seed(), threads(), weightcomp(), noenrich()))
  }, ignoreInit = TRUE
  )
  
  
  ###### Data Visualization server side 
  
  data <- reactive({
    req(input$datatoviz)
    
    ext <- tools::file_ext(input$datatoviz$name)
    switch(ext,
           txt = read.csv(input$datatoviz$datapath, sep = "\t", row.names = 1),
           validate("Invalid file; Please upload a .txt file")
    )
  })
  
  output$tableplot <- renderTable({
    if (input$table == "TRUE") {
      data()
    }
  })
  output$histplot <- renderPlot({
    if (input$hist == "TRUE") {
      df <- data()
      Pathway <- rownames(df)
      IF <- df[[3]]
      ggplot(df, aes(x=Pathway, y= IF, fill = IF < 0)) +
        geom_bar(stat = "identity") +
        scale_fill_manual(guide = FALSE,
                          values = c("forestgreen", "darkred")) +
        coord_flip() +
        labs(
          title = "Impact Factor"
        ) +
        theme(plot.title = element_text(size=22, color = "deepskyblue4"))
    }
  })
  
} #server bracket

shinyApp(ui, server)
