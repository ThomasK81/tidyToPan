##### Log File

ToPan.log <- file("ToPan.log", open = "wt")
sink(ToPan.log , type = c("message"))

##### 0.1, Libraries #######

library(shiny)
library(tidyverse)
library(lda)
library(LDAvis) 
library(data.table) # data.table Server
library(Rtsne) # better than tsne 
library(DT) # DataTables UI

##### 0.2. Functions #######


### Text cleaning function
preprocess_corpus <- function(x) {
  research_corpus <- tolower(x)  # force to lowercase
  research_corpus <- gsub("'", " ", research_corpus)  # remove apostrophes
  research_corpus <- gsub("-", "", research_corpus)  # remove hyphens
  research_corpus <- gsub("[[:punct:]]", " ", research_corpus)  # replace punctuation with space
  research_corpus <- gsub("[[:cntrl:]]", " ", research_corpus)  # replace control characters with space
  research_corpus <- trimws(research_corpus)
  research_corpus <-str_replace_all(research_corpus, "[\r\n]" , "")
  research_corpus <- gsub("[0-9]", "", research_corpus) #remove numbers
  research_corpus <- gsub("^ *|(?<= ) | *$", "", research_corpus, perl = TRUE) # Remove multiple whitespace
  return(research_corpus)
}

# fread specific columns function
read_cols <- function(file_name, colsToKeep, sep) {
  header <- fread(file_name, nrows = 1, header = FALSE, encoding = 'UTF-8')
  columns_in_header <- all(colsToKeep %chin% unlist(header))
  stopifnot(columns_in_header)
  
  fread(file_name, header=TRUE, select=colsToKeep, sep = sep, encoding = 'UTF-8')
}

# tsne implementation

rtsne <- function(x) {
  perp <- 30
  if (3 * perp >= nrow(x) - 1) {
    perp <- floor((nrow(x) - 1) / 3)
  }
  perp <- as.integer(perp)
  tsne_result <- Rtsne(x, check_duplicates = F, perplexity = perp) 
  tsne_result$Y
}

##### 1. User Interface #######

ui <- navbarPage(theme = "bootstrap.min.css", 
                 div(img(
                   src = "melete.png", 
                   height = "25"), 
                   "tidyToPān 1.0.0"), 
                 windowTitle = "tidyToPān 1.0.0",
                 
##### 1.0.1. Home #######
                 tabPanel("Home",
                          fluidRow(column(4, br(), div(img(src = "melete.png", height = "200"))),
                                   column(8, includeMarkdown("md/home.md")))),
                 
                 ##### 1.1. DATA INPUT #######                
                 navbarMenu("Data Input",
                            ##### 1.1.3. Server-Side RDS #######
                            tabPanel("Server-Side RDS",
                                     sidebarLayout(
                                       sidebarPanel(
                                         uiOutput("RDSUI"),
                                         actionButton("RDSgo", "Submit")
                                       ),
                                       mainPanel(
                                         DTOutput("catalogue3")
                                       ))),
                            ##### 1.1.4. CSV INPUT #######
                            tabPanel("CSV",
                                     sidebarLayout(
                                       sidebarPanel(
                                         fileInput('file1', 'Choose CSV File',
                                                   accept=c('text/csv', 
                                                            'text/comma-separated-values,text/plain', 
                                                            '.csv')),
                                         checkboxInput('header', 'Header', TRUE),
                                         radioButtons('sep', 'Separator',
                                                      c(Comma = ',',
                                                        Semicolon = ';',
                                                        Hash = '#',
                                                        Tab = '\t'),
                                                      ','),
                                         textInput('idcolumn', 'ID Column', 'identifier'),
                                         textInput('textcolumn', 'Text Column', 'text'),
                                         textInput('corpusname', 'Corpus Name', placeholder = 'myAwesomeCorpus01'),
                                         textInput('regexCSV', 'RegEx Filter for IDs', placeholder = 'm.shup for "mashup". "mishup", etc.'),
                                         actionButton("CSVgo", "Submit")
                                       ),
                                       mainPanel(
                                         DTOutput("catalogueCSV")
                                       ))),
                            ##### 1.1.5 CEX INPUT #######
                            tabPanel("CEX",
                                     sidebarLayout(
                                       sidebarPanel(
                                         fileInput('fileCEX', 'Choose CEX File',
                                                   accept=c('text/csv', 
                                                            'text/comma-separated-values,text/plain', 
                                                            '.cex',
                                                            '.CEX')),
                                         radioButtons('sepCEX', 'Separator',
                                                      c(Comma=',',
                                                        Hash='#',
                                                        Tab='\t'),
                                                      '#'),
                                         textInput('corpusnameCEX', 'Corpus Name', placeholder = 'myAwesomeCorpus01'),
                                         textInput('regexCEX', 'RegEx Filter for IDs', placeholder = 'm.shup for "mashup". "mishup", etc.'),
                                         actionButton("CEXgo", "Submit")
                                       ),
                                       mainPanel(
                                         DTOutput("catalogueCEX")
                                       )))
                 ),
                 ##### 1.3. Stop Words #######
                 
                 tabPanel("Stop Words",
                          sidebarLayout(
                            sidebarPanel(
                              uiOutput("SWCorpusUI"),
                              numericInput("stopnumber", label = "Number of Stopwords", min = 0, max = 400, value = 200),
                              textInput("add_stopwords", label = "Additional Stopwords", value = ""),
                              textInput("remove_stopwords", label = "Remove Words from Stopword list", value = ""),
                              textInput('nameSW', 'SW-List Name', placeholder = 'myStopwords'),
                              actionButton("stopwordgo", "Submit")
                            ),
                            mainPanel(
                              DTOutput("stopwords")
                            ))),
                 
                 ##### 1.4. Topic Modelling Input #######
                 
                 tabPanel("LDA TM",
                          sidebarLayout(
                            sidebarPanel(
                              uiOutput("ProcessTM"),
                              uiOutput("ProcessSW"),
                              textInput("CTMfile", label = "Name your ctm-File", value = "", placeholder = "output_filename"),
                              numericInput("occurrence", label = "Occurrence threshold", min = 1, max = 5, value = 3),
                              numericInput("seed", label = "Seed", min = 1, max = 1000, value = 73),
                              numericInput("number_topics", label = "Number of Topics", min = 2, max = 200, value = 20),
                              sliderInput("alpha", label = "Alpha", min = 0.00, max = 1.00, value = 0.02),
                              sliderInput("eta", label = "Eta", min = 0.00, max = 1.00, value = 0.02),
                              numericInput("number_terms", label = "Number of Terms Shown", min = 15, max = 50, value = 25),
                              selectInput(inputId = "dimscale", label = "Choose a high-dimensional scaling method:", choices = c("pca", "tsne")),
                              numericInput("iterations", label = "Iterations", min = 50, max = 5000, value = 500),
                              actionButton("TMgo", "Submit")
                            ),
                            mainPanel(
                              DTOutput("topicmodelling")
                            ))),
                 ##### 1.5. Topic Modelling Visualisation Input #######
                 
                 tabPanel("LDAvis", 
                          uiOutput("VISUI"),
                          mainPanel(htmlOutput("topicmodels")
                          )
                 ),
                 ##### 1.6. Topic Modelling Tables #######
                 
                 navbarMenu("LDA Tables", 
                            tabPanel("DocumentTopic (θ)",
                                     uiOutput("thetaUI"),
                                     mainPanel(DTOutput("theta"))),
                            tabPanel("TermTopic (φ)", 
                                     uiOutput("phiUI"),
                                     mainPanel(DTOutput("phi")))
                 )
)

server <- function(input, output, session) {
  
  options(shiny.maxRequestSize=400*1024^2)
  
  # 2.1. Catalogues ####
  # 2.1.1. Output Server-Side RDS ####
  output$RDSUI <- renderUI({
    ServerCorpora <- list.files(path = "./www/corpora", pattern = "*.rds", recursive = TRUE, full.names = TRUE)
    ServerCorpora <- ServerCorpora[which(grepl("theta.rds", ServerCorpora, fixed = TRUE) == FALSE)]
    ServerCorpora <- ServerCorpora[which(grepl("phi.rds", ServerCorpora, fixed = TRUE) == FALSE)]
    ServerCorpora <- ServerCorpora[which(grepl("lda.rds", ServerCorpora, fixed = TRUE) == FALSE)]
    names(ServerCorpora) <- sapply(strsplit(ServerCorpora, "/"), function(x) {x[length(x)]})
    names(ServerCorpora) <- gsub(pattern = ".rds", replacement = "", names(ServerCorpora))
    selectInput("serverRDS", label = "Choose RDS file", choices = ServerCorpora)
  })
  
  inFileRDS <- eventReactive(input$RDSgo, {input$serverRDS
  })
  
  observeEvent(input$RDSgo, {
  output$catalogue3 <- renderDT({
    withProgress(message = 'Reading Texts', value = 0, {
      CSVcatalogue <- readRDS(inFileRDS())
    })
    CSVcatalogue
    })
  })
  
  # 2.1.2. Output CSV Corpus ####
  
  inFileCSV <- eventReactive(input$CSVgo, {input$file1
  })
  header <- eventReactive(input$CSVgo, {input$header
  })
  sep <- eventReactive(input$CSVgo, {input$sep
  })
  idcolumn <- eventReactive(input$CSVgo, {input$idcolumn
  })
  textcolumn <- eventReactive(input$CSVgo, {input$textcolumn
  })
  regexCSV <- eventReactive(input$CSVgo, {input$regexCSV
  })
  corpusName <- eventReactive(input$CSVgo, {
    if (input$corpusname == "") {
      "noName"
    } else {
      input$corpusname
    }
  })
  
  observeEvent(input$CSVgo, {
  output$catalogueCSV <- renderDT({
    
    if (is.null(inFileCSV()))
      return(NULL)
    
    withProgress(message = 'Reading Texts', value = 0, {
      if (header() == F) {
        CSVcatalogue <- fread(inFileCSV()$datapath, header = header(), sep = sep(), encoding = 'UTF-8')
      } else {
        CSVcatalogue <- read_cols(inFileCSV()$datapath, colsToKeep = c(idcolumn(), textcolumn()), sep = input$sep)
      }
    })
    CSVcatalogue <- as_tibble(CSVcatalogue) %>% select(1:2)
    colnames(CSVcatalogue) <- c('identifier', 'text')
    CSVcatalogue <- CSVcatalogue %>%
      filter(str_detect(identifier,regexCSV()))
    withProgress(message = 'Saving Binary...', value = 0, {
      foldername <- paste("./www/corpora", corpusName(), sep = "/")
      dir.create(foldername, recursive = TRUE, showWarnings = F)
      file_name <- paste(foldername, "/", corpusName(), "-", strftime(Sys.time(),"%Y%m%d-%H%M"), ".rds", sep = "")
      saveRDS(CSVcatalogue, file_name)
    })
    CSVcatalogue
  })
  })
  # 2.1.3 Output CEX Corpus ####
  
  inFileCEX <- eventReactive(input$CEXgo, {input$fileCEX
  })
  sepCEX <- eventReactive(input$CEXgo, {input$sepCEX
  })
  regexCEX <- eventReactive(input$CEXgo, {input$regexCEX
  })
  corpusNameCEX <- eventReactive(input$CEXgo, {
    if (input$corpusnameCEX == "") {
      "noName"
    } else {
      input$corpusnameCEX
    }
  })
  
  observeEvent(input$CEXgo, {
  output$catalogueCEX <- renderDT({
    if (is.null(inFileCEX()))
      return(NULL)
    
    withProgress(message = 'Reading Texts', value = 0, {
      CSVcatalogue <- fread(inFileCEX()$datapath, sep = sepCEX(), skip="#!ctsdata", encoding = 'UTF-8')
    })
    CSVcatalogue <- as_tibble(CSVcatalogue)
    colnames(CSVcatalogue) <- c('identifier', 'text')
    CSVcatalogue <- CSVcatalogue %>%
      filter(str_detect(identifier,regexCEX()))
    withProgress(message = 'Saving Binary...', value = 0, {
      foldername <- paste("./www/corpora", corpusNameCEX(), sep = "/")
      dir.create(foldername, recursive = TRUE, showWarnings = F)
      file_name <- paste(foldername, "/", corpusNameCEX(), "-", strftime(Sys.time(),"%Y%m%d-%H%M"), ".rds", sep = "")
      saveRDS(CSVcatalogue, file_name)
    })
    CSVcatalogue
  })
  })
  
  # 2.4. Stopwords ####
  output$SWCorpusUI <- renderUI({
    ServerCorpora <- list.files(path = "./www/corpora", pattern = "*.rds", recursive = TRUE, full.names = TRUE)
    names(ServerCorpora) <- sapply(strsplit(ServerCorpora, "/"), function(x) {x[length(x)]})
    names(ServerCorpora) <- gsub(pattern = ".rds", replacement = "", names(ServerCorpora))
    selectInput("sw_corpus", label = "Choose RDS file", choices = ServerCorpora)
  })
  
  inFileSW <- eventReactive(input$stopwordgo, {input$sw_corpus
  })
  
  nameSW <- eventReactive(input$stopwordgo, {
    input$nameSW
  })
  
  observeEvent(input$stopwordgo, {
  
  output$stopwords <- renderDT({
    if (is.null(inFileSW()))
      return(NULL)
    if (nameSW() == "")
      return(NULL)
    withProgress(message = 'Reading Texts', value = 0, {
      research_corpus <- readRDS(inFileSW())
    })
    identifier <- as.character(research_corpus$identifier)
    stopword_corpus <- as.character(research_corpus$text)
    stopword_corpus <- preprocess_corpus(stopword_corpus)
    
    # tokenize stopword_corpus on space and output as a list:
    doc.list2 <- strsplit(stopword_corpus, "[[:space:]]+")
    
    # compute the table of stop_words:
    all_for_stop_words <- unlist(doc.list2)
    term.table2 <- table(all_for_stop_words)
    term.table2 <- sort(term.table2, decreasing = TRUE)
    
    stop_words <- names(term.table2)
    rm(term.table2)
    stop_words <- stop_words[1:input$stopnumber]
    additional <- unlist(strsplit(input$add_stopwords, ",", fixed = TRUE))
    additional <- gsub("^[[:space:]]+", "", additional) # remove whitespace at beginning of documents
    additional <- gsub("[[:space:]]+$", "", additional) # remove whitespace at end of documents
    stop_words <- c(additional, stop_words)
    less <- unlist(strsplit(input$remove_stopwords, ",", fixed = TRUE))
    less <- gsub("^[[:space:]]+", "", less) # remove whitespace at beginning of documents
    less <- gsub("[[:space:]]+$", "", less) # remove whitespace at end of documents
    stop_words <- stop_words [! stop_words %in% less]
    
    withProgress(message = 'Saving Binary...', value = 0, {
      foldername <- paste("./www/stopwords", nameSW(), sep = "/")
      dir.create(foldername, recursive = TRUE, showWarnings = F)
      file_name <- paste(foldername, "/", nameSW(), "-", strftime(Sys.time(),"%Y%m%d-%H%M"), ".rds", sep = "")
      saveRDS(stop_words, file_name)
    })

    data.frame(Index = c(1:length(stop_words)), Word = stop_words)
  })
  })
  
  ##### 2.5. Processing TM #######
  output$ProcessTM <- renderUI({
    ServerTM <- list.files(path = "./www/corpora", pattern = ".rds", recursive = TRUE, full.names = TRUE)
    names(ServerTM) <- sapply(strsplit(ServerTM, "/"), function(x) {x[length(x)]})
    names(ServerTM) <- gsub(pattern = ".rds", replacement = "", names(ServerTM))
    selectInput("tm_corpus", label = "Choose Corpus", choices = ServerTM)
  })
  
  output$ProcessSW <- renderUI({
    ServerSW <- list.files(path = "./www/stopwords", pattern = ".rds", recursive = TRUE, full.names = TRUE)
    names(ServerSW) <- sapply(strsplit(ServerSW, "/"), function(x) {x[length(x)]})
    names(ServerSW) <- gsub(pattern = ".rds", replacement = "", names(ServerSW))
    selectInput("stopwordlist", label = "Choose SW List", choices = ServerSW)
  })
  
  CTMfile <- eventReactive(input$TMgo, {
    input$CTMfile
  })
  
  observeEvent(input$TMgo, {
  
  output$topicmodelling <- renderDT({
    timestamp <- strftime(Sys.time(),"%Y%m%d-%H%M")
    CTMfilename <- gsub("[^a-zA-Z0-9]", "", CTMfile())
    CTMfoldername <- paste0("./www/models/", CTMfilename, "/", timestamp, "/")
    dir.create(paste0(CTMfoldername, "ctm", sep = ""), recursive = TRUE)
    CTMfilename <- paste0(CTMfoldername, "ctm/", CTMfilename, ".ctm")
    
    inFile <- input$tm_corpus
    
    if (is.null(inFile))
      return(NULL)
    withProgress(message = 'Reading Texts', value = 0, {
      research_corpus <- readRDS(inFile)
    })
    
    inFile <- input$stopwordlist
    
    if (is.null(inFile))
      return(NULL)
    withProgress(message = 'Reading Stopwords', value = 0, {
      stop_words <- readRDS(inFile)
    })
    
    output_names <- as.character(research_corpus$identifier)
    research_corpus <- as.character(research_corpus$text)
    
    withProgress(message = "Normalise IDs...", {output_names <- gsub("^[[:space:]]+", "", output_names) # remove whitespace at beginning of documents
    output_names <- gsub("[[:space:]]+$", "", output_names) # remove whitespace at end of documents
    output_names <- gsub("[[:space:]]+", " ", output_names) # remove multiple whitespace
    output_names <- trimws(output_names)})
    
    withProgress(message = "Prepare topic modelling...", {
      research_corpus <- preprocess_corpus(research_corpus)
      # tokenize on space and output as a list:
      doc.list <- strsplit(research_corpus, "[[:space:]]+")
      all_words <- unlist(doc.list)
      # compute the table of terms:
      term.table <- table(all_words)
      term.table <- sort(term.table, decreasing = TRUE)
      occurences <- input$occurrence
      del <- names(term.table) %in% stop_words | term.table < occurences
      stopwords <- term.table[del]
      stopwords <- names(stopwords)
      term.table <- term.table[!del]
      vocab <- names(term.table)
      
      # now put the documents into the format required by the lda package:
      get.terms <- function(x) {
        index <- match(x, vocab)
        index <- index[!is.na(index)]
        rbind(as.integer(index - 1), as.integer(rep(1, length(index))))
      }
      documents <- lapply(doc.list, get.terms)
      # Compute some statistics related to the data set:
      D <- length(documents)  # number of documents (2,000)
      W <- length(vocab)  # number of terms in the vocab (14,568)
      doc.length <- sapply(documents, function(x) sum(x[2, ]))  # number of tokens per document [312, 288, 170, 436, 291, ...]
      N <- sum(doc.length)  # total number of tokens in the data (546,827)
      term.frequency <- as.integer(term.table)  # frequencies of terms in the corpus [8939, 5544, 2411, 2410, 2143, ...]
    })
    
    # Fit the model:
    withProgress(message = "Modelling (this may take a while)...", {
      seed <- input$seed
      set.seed(seed)
      K <- input$number_topics
      iterations <- input$iterations
      alpha <- input$alpha
      eta <- input$eta
      number_terms <- input$number_terms
      fit <- lda.collapsed.gibbs.sampler(documents = documents, K = K, vocab = vocab, 
                                         num.iterations = iterations, alpha = alpha, 
                                         eta = eta, initial = NULL, burnin = 0,
                                         compute.log.likelihood = TRUE)
    })
    
    withProgress(message = "Structuring results...", {
      theta <- t(apply(fit$document_sums + alpha, 2, function(x) x/sum(x)))
      phi <- t(apply(t(fit$topics) + eta, 2, function(x) x/sum(x)))
      
      research_corpusAbstracts <- list(phi = phi,
                                       theta = theta,
                                       doc.length = doc.length,
                                       vocab = vocab,
                                       term.frequency = term.frequency)
      
      if (input$dimscale == "tsne") {
        json <- createJSON(phi = research_corpusAbstracts$phi, 
                           theta = research_corpusAbstracts$theta, 
                           doc.length = research_corpusAbstracts$doc.length, 
                           vocab = research_corpusAbstracts$vocab, 
                           term.frequency = research_corpusAbstracts$term.frequency,
                           R=number_terms,
                           mds.method = rtsne, 
                           plot.opts = list(xlab="", ylab=""))
      } else { 
        json <- createJSON(phi = research_corpusAbstracts$phi, 
                           theta = research_corpusAbstracts$theta, 
                           doc.length = research_corpusAbstracts$doc.length, 
                           vocab = research_corpusAbstracts$vocab, 
                           term.frequency = research_corpusAbstracts$term.frequency,
                           R=number_terms)
      }    
    })
    
    #Visualise and save
    inFile <- input$tm_corpus
    withProgress(message = 'Reading Texts', value = 0, {
      research_corpus <- readRDS(inFile)
    })
    
    dir.create(paste0(CTMfoldername, "tab"), recursive = TRUE)
    visfolder <- paste0(CTMfoldername, input$dimscale, "_vis")
    dir.create(visfolder, recursive = TRUE)
    ldafolder <- paste0(CTMfoldername, "mod")
    dir.create(ldafolder, recursive = TRUE)
    serVis(json, out.dir = visfolder, open.browser = FALSE)
    
    withProgress(message = 'Generating Tables', value = 0, {
      # Tables
      
      ## reorder phi and theta according to JSON file
      
      new.order <- RJSONIO::fromJSON(json)$topic.order 
      phi <- phi[new.order,]
      theta <- theta[,new.order]
      
      ## generate topicnames
      
      phi.t <- t(phi)
      topicnames <- vector(mode="character", length=K)
      for (i in 1:K){
        topicnames[i] <- paste(rownames(head(phi.t[order(phi.t[,i],decreasing=TRUE),], n=7)), sep="", collapse="_")
        
      }
      
      #phi-table
      phi_folder <- paste0(CTMfoldername, "tab/phi.rds")
      phi_CSVfolder <- paste0(CTMfoldername, "tab/phi.csv")
      theta_folder <- paste0(CTMfoldername, "tab/theta.rds")
      theta_CSVfolder <- paste0(CTMfoldername, "tab/theta.csv")
      lda_folder <- paste0(CTMfoldername, "mod/lda.rds")
      rownames(phi) <- topicnames
      phi <- t(phi)
      saveRDS(phi, file = phi_folder)
      write.csv(phi, file = phi_CSVfolder)
      
      #theta-table
      theta <- cbind(output_names, as.character(research_corpus$text), theta) 
      colnames(theta) <- c("identifier", "text", topicnames)
      saveRDS(theta, file = theta_folder)
      saveRDS(fit, file = lda_folder)
      write.csv(theta, file = theta_CSVfolder)
      
      #ctm_file
      TMvariables <- paste0("K", input$number_topics, "_alpha", gsub("[^a-zA-Z0-9]", "", input$alpha), "_eta", gsub("[^a-zA-Z0-9]", "", input$eta), "_I", input$iterations, "_S", input$seed)
      temp <- as.character(research_corpus$text)
      sink(CTMfilename)
      cat("#!TM_variables")
      cat("\n")
      cat(TMvariables)
      cat("\n")
      cat("Occurrence:", input$occurrence)
      cat("\n\n")
      cat("#!ctsdata")
      cat("\n")
      for(i in 1:length(output_names)){
        cat(paste0(output_names[i], "#", temp[i]))
        cat("\n")
      }
      cat("\n")
      cat("#!stop_words")
      cat("\n") 
      cat(stopwords)
      cat("\n\n")
      cat("#!word_index")
      cat("\n")
      cat(vocab)
      cat("\n\n")
      cat("#!assignments")
      cat("\n")
      for(i in 1:length(output_names)){
        cat(paste0(output_names[i], "#", paste(fit$assignments[[i]], collapse = ",")))
        cat("\n")
      }
      cat("\n")
      cat("#!theta")
      cat("\n")
      sink()
      write.table(theta, append = T, file = CTMfilename, row.names = F, quote = F, sep = "#")
      sink(CTMfilename, append = T)
      cat("\n")
      cat("#!phi")
      cat("\n")
      cat("token#")
      sink()
      write.table(phi, append = T, file = CTMfilename, quote = F, sep = "#")
      sink(CTMfilename, append = T)
      cat("\n")
      sink()
    })
    
    research_corpus
    
  })
  
  })
  
  ##### 2.3. Output LDAvis #######
  output$VISUI <- renderUI({
    ServerLDAVis <- list.files(path = "./www", pattern = "index.html", recursive = TRUE, full.names = TRUE)
    ServerLDAVis <- gsub("./www", "", ServerLDAVis, fixed = TRUE)
    names(ServerLDAVis) <- gsub("/models/", "", ServerLDAVis, fixed = TRUE)
    names(ServerLDAVis) <- gsub("/vis/", "", names(ServerLDAVis), fixed = TRUE)
    names(ServerLDAVis) <- gsub("/index.html", "", names(ServerLDAVis), fixed = TRUE)
    names(ServerLDAVis) <- gsub("/", ":", names(ServerLDAVis), fixed = TRUE)
    selectInput("TModel", label = "Choose TM", choices = ServerLDAVis)
  })
  
  output$topicmodels <- renderUI({
    getPage<-function() {
      return(tags$iframe(src = input$TModel
                         , style="width:150%;",  frameborder="0"
                         ,id="iframe"
                         , height = "800px"))
    }
    getPage()})
  

  ##### 2.6. Output Tables #######
  ##### 2.6.1. Output Theta Table #######
  output$thetaUI <- renderUI({
    ServerTheta <- list.files(path = "./www/models", pattern = "theta.csv", recursive = TRUE, full.names = TRUE)
    names(ServerTheta) <- gsub("./www/models/", "", ServerTheta, fixed = TRUE)
    names(ServerTheta) <- gsub("/tab/theta.csv", "", names(ServerTheta), fixed = TRUE)
    names(ServerTheta) <- gsub("/", ":", names(ServerTheta), fixed = TRUE)
    selectInput("ThetaTable", label = "Choose TM", choices = ServerTheta)
  })
  
  output$theta <- renderDT({
    inFile <- input$ThetaTable
    
    if (is.null(inFile))
      return(NULL)
    
    withProgress(message = 'Reading Texts', value = 0, {
      read.csv(inFile, header = TRUE, sep = ",", quote = "\"")
    })
  })
  
  ##### 2.6.2. Output Phi Table #######
  
  output$phiUI <- renderUI({
    ServerPhi <- list.files(path = "./www", pattern = "phi.csv", recursive = TRUE, full.names = TRUE)
    names(ServerPhi) <- gsub("./www/models/", "", ServerPhi, fixed = TRUE)
    names(ServerPhi) <- gsub("/tab/phi.csv", "", names(ServerPhi), fixed = TRUE)
    names(ServerPhi) <- gsub("/", ":", names(ServerPhi), fixed = TRUE)
    selectInput("PhiTable", label = "Choose TM", choices = ServerPhi)
  })
  
  output$phi <- renderDT({
    inFile <- input$PhiTable
    
    if (is.null(inFile))
      return(NULL)
    
    withProgress(message = 'Reading Texts', value = 0, {
      read.csv(inFile, header = TRUE, sep = ",", quote = "\"")
    })
  })
  
}

##### 3. Start It Up #######

shinyApp(ui = ui, server = server)
