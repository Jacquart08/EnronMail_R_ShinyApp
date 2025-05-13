# ---- Load Required Libraries ----
library(shiny)
library(ggplot2)
library(dplyr)
library(stringr)
library(lubridate)
library(wordcloud)
library(tm)
library(visNetwork)
library(tidytext)
library(shinythemes)
library(shinyBS)
library(shinycssloaders)
library(shinydashboard)

# ---- Load the Enron Dataset ----
my_path <- "C:/Users/CM_LAPTOP/Downloads/Enron.Rdata"
load(my_path)

# ---- Data Cleaning and Preparation ----

# Clean and standardize sender email addresses in the message data
message <- message %>%
  mutate(sender_clean = tolower(trimws(sender)))

# Clean and standardize employee email addresses
employeelist <- employeelist %>%
  mutate(Email_clean = tolower(trimws(Email_id)))

# Get unique employee roles for filtering in the UI
unique_roles <- sort(unique(na.omit(as.character(employeelist$status))))

# Most active employees cleaning and preparation 

# Prepare top senders data with proper status information
top_senders <- message %>%
  count(sender_clean, sort = TRUE) %>%
  left_join(employeelist %>% 
              select(Email_clean, status, firstName, lastName), 
            by = c("sender_clean" = "Email_clean")) %>%
  mutate(
    label = ifelse(!is.na(status),
                   paste0(firstName, " ", lastName, " (", status, ")"),
                   sender_clean)
  )

# Create a data frame with monthly email counts
email_data <- data.frame(
  Date = as.Date(paste0(format(message$date, "%Y-%m"), "-01")),
  Count = 1
)

# Correct errors in the year data in the dataframe
email_data <- email_data %>%
  mutate(Date = case_when(
    format(Date, "%Y") == "0001" ~ as.Date(paste0("2001", format(Date, "-%m-%d"))),
    format(Date, "%Y") == "0002" ~ as.Date(paste0("2002", format(Date, "-%m-%d"))), 
    format(Date, "%Y") == "1979" ~ as.Date(paste0("1997", format(Date, "-%m-%d"))),
    format(Date, "%Y") == "2020" ~ as.Date(paste0("2002", format(Date, "-%m-%d"))),
    TRUE ~ Date
  ))

# Filtering the data set to show only the relevant period
email_data <- email_data[email_data$Date >= as.Date("1999-01-01") & email_data$Date <= as.Date("2002-12-31"), ]

# Group by Date and summarize the count
email_data <- email_data %>%
  group_by(Date) %>%
  summarize(Count = sum(Count))

# Role analysis 

# ---- Prepare Nodes and Edges for Network Visualization ----

# Nodes: Each employee as a node
nodes <- employeelist %>%
  mutate(id = Email_clean, 
         label = paste(firstName, lastName),
         group = status) %>%
  select(id, label, group)

# Edges: Email connections between employees
edges <- recipientinfo %>%
  mutate(rvalue_clean = tolower(trimws(rvalue))) %>%
  left_join(message %>% select(mid, sender), by = "mid") %>%
  left_join(employeelist %>% select(Email_id, Email_clean), by = c("sender" = "Email_id")) %>%
  mutate(from = Email_clean, to = rvalue_clean) %>%
  filter(!is.na(from) & !is.na(to)) %>%
  filter(from %in% nodes$id & to %in% nodes$id) %>%
  group_by(from, to) %>%
  summarize(value = n(), .groups = "drop")

# ---- Define UI for Application ----
ui <- fluidPage(
  theme = shinytheme("flatly"),
  # Place enron_logo.png and dsti_logo.png in the www/ directory in order for the images to be displayed
  div(
    style = "display: flex; align-items: center; justify-content: center; gap: 40px; margin-bottom: 20px;",
    img(src = "enron_logo.png", height = "60px"),
    div(
      style = "text-align: center;",
      HTML("<h1 style='margin-bottom: 0;'>Enron Exploratory App</h1>
            <h4 style='margin: 0;'>R for Big Data final exam</h4>
            <h5 style='margin-top: 0;'>Charles Manil</h5>")
    ),
    img(src = "dsti_logo.png", height = "60px")
  ),
  
  tabsetPanel(
    # Tab 1: Temporal Analysis
    tabPanel(title = tagList(icon("chart-line"), "Temporal Analysis"),
      sidebarLayout(
        sidebarPanel(
          dateRangeInput("date_range", "Select date range:",
                        start = "1999-01-01",
                        end = "2002-12-31",
                        min = "1999-01-01",
                        max = "2002-12-31"),
          checkboxInput("show_events", "Show key events", value = TRUE),
          conditionalPanel(
            condition = "input.$temporal_tab == 'Time Series'",
            helpText("The time series plot shows the monthly volume of emails. Use the date range to filter and optionally display key Enron events.")
          ),
          conditionalPanel(
            condition = "input.$temporal_tab == 'Anomaly Detection'",
            helpText("Anomaly detection highlights months with unusually high or low email activity (red dots). Adjust the date range to explore different periods.")
          )
        ),
        mainPanel(
          tabsetPanel(
            id = "temporal_tab",
            tabPanel("Time Series", plotOutput("timeSeriesPlot"), verbatimTextOutput("eventInfo")),
            tabPanel("Anomaly Detection", plotOutput("anomalyPlot"))
          )
        )
      )
    ),
    
    # Tab 2: Role Analysis
    tabPanel(title = tagList(icon("users"), "Role Analysis"),
      sidebarLayout(
        sidebarPanel(
          conditionalPanel(
            condition = "input.role_tab == 'Role Plot'",
            selectInput("role_metric", "Select metric:",
                        choices = c("Number of Emails", "Average Email Length"),
                        selected = "Number of Emails")
          ),
          conditionalPanel(
            condition = "input.role_tab == 'Most Active Employees'",
            sliderInput("n_senders", "Number of top senders to display:", 
                        min = 5, max = 50, value = 20),
            selectInput("status_filter", "Filter by role:",
                        choices = c("All", unique_roles),
                        selected = "All"),
            bsTooltip("n_senders", "Number of top senders to display", "right")
          ),
          conditionalPanel(
            condition = "input.role_tab == 'Most Active Employees'",
            helpText("This bar chart shows the most active employees by number of emails sent. Use the filters to focus on a specific role.")
          ),
          conditionalPanel(
            condition = "input.role_tab == 'Role Plot'",
            helpText("This plot summarizes email activity or average email length by employee role. Select the metric to view different aspects.")
          ),
          conditionalPanel(
            condition = "input.role_tab == 'Role-to-Role Heatmap'",
            helpText("The heatmap visualizes the volume of emails exchanged between different roles within Enron.")
          )
        ),
        mainPanel(
          tabsetPanel(
            id = "role_tab",
            tabPanel("Most Active Employees", withSpinner(plotOutput("topSendersPlot"))),
            tabPanel("Role Plot", plotOutput("rolePlot")),
            tabPanel("Role-to-Role Heatmap", plotOutput("roleHeatmap"))
          )
        )
      )
    ),
    
    # Tab 3: Content Analysis
    tabPanel(title = tagList(icon("align-left"), "Content Analysis"),
      sidebarLayout(
        sidebarPanel(
          selectInput("sender_content", "Select sender:",
                     choices = unique(top_senders$sender_clean),
                     selected = unique(top_senders$sender_clean)[1]),
          conditionalPanel(
            condition = "input.content_tab == 'Word Cloud'",
            sliderInput("max_words", "Maximum number of words:",
                        min = 10, max = 100, value = 50)
          ),
          conditionalPanel(
            condition = "input.content_tab == 'N-gram Analysis'",
            numericInput("ngram_n", "N for N-gram:", value = 2, min = 2, max = 10, step = 1),
            sliderInput("ngram_top", "Top n-grams to display:", min = 5, max = 30, value = 20)
          ),
          conditionalPanel(
            condition = "input.content_tab == 'Word Cloud'",
            helpText("The word cloud visualizes the most frequent words in the selected sender's emails. You can adjust the number of words to display.")
          ),
          conditionalPanel(
            condition = "input.content_tab == 'Email Length'",
            helpText("This histogram shows the distribution of email lengths for the selected sender.")
          ),
          conditionalPanel(
            condition = "input.content_tab == 'N-gram Analysis'",
            helpText("This plot shows the most frequent n-grams (word sequences) in the selected sender's emails. Adjust N and the number of top n-grams to explore different patterns.")
          )
        ),
        mainPanel(
          tabsetPanel(
            id = "content_tab",
            tabPanel("Word Cloud", plotOutput("wordCloudPlot")),
            tabPanel("Email Length", plotOutput("emailLengthPlot")),
            tabPanel("N-gram Analysis", plotOutput("ngramPlot"))
          )
        )
      )
    ),
    
    # Tab 4 : Connections network
    tabPanel(title = tagList(icon("project-diagram"), "Network Visualization"),
      sidebarLayout(
        sidebarPanel(
          sliderInput("max_edges", "Max edges to display:", min = 10, max = 500, value = 200),
          helpText("This interactive network graph shows email connections between Enron employees. Use the slider to adjust the number of connections displayed.")
        ),
        mainPanel(
          visNetworkOutput("networkPlot", height = "600px")
       )
     )
    ),
    valueBoxOutput("totalEmails")
  ),
  tags$footer(
    style = "text-align:center; margin-top:30px; color: #888;",
    "© 2024 Charles Manil | R for Big Data Final Exam | https://github.com/Jacquart08/EuronMail_R_ShinyApp"
  )
)

server <- function(input, output) {
  
  # ---- Most Active Employees Plot ----
  output$topSendersPlot <- renderPlot({
    filtered_senders <- top_senders
    
    # Apply status filter if not "All"
    if (input$status_filter != "All") {
      filtered_senders <- filtered_senders %>%
        filter(!is.na(status) & status == input$status_filter)
    }
    
    # Get the top N senders after filtering
    plot_data <- filtered_senders %>%
      slice_max(n, n = input$n_senders) %>%
      as.data.frame()  # Convert to regular data frame to avoid JSON warnings
    
    ggplot(plot_data, aes(x = reorder(label, n), y = n)) +
      geom_col(fill = "#2c7fb8") +
      coord_flip() +
      labs(
        title = paste("Top", input$n_senders, "Senders with Job Titles"),
        x = "Sender (Job Title)",
        y = "Number of Emails Sent"
      ) +
      theme_minimal(base_size = 12) +
      theme(axis.text.y = element_text(size = 10))
  })
  
  # ---- Temporal Analysis Plot ----
  output$timeSeriesPlot <- renderPlot({
    filtered_data <- email_data %>%
      filter(Date >= input$date_range[1] & Date <= input$date_range[2])
    
    p <- ggplot(filtered_data, aes(x = Date, y = Count)) +
      geom_line() +
      labs(title = "Monthly Email Counts",
           x = "Date",
           y = "Number of Emails") +
      theme_minimal() +
      scale_x_date(date_breaks = "3 months", date_labels = "%b %Y") +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))
    
    if (input$show_events) {
      p <- p + 
        geom_vline(xintercept = as.Date("2001-09-01"), color = "red", linetype = "dashed") +
        geom_vline(xintercept = as.Date("2001-10-31"), color = "red", linetype = "dashed") +
        geom_vline(xintercept = as.Date("2001-12-02"), color = "red", linetype = "dashed")
    }
    
    p
  })
  
  # Event Information
  output$eventInfo <- renderText({
    if (input$show_events) {
      "Key Events:
      - Sept 1, 2001: Enron claims to be in 'strongest and best shape'
      - Oct 31, 2001: SEC investigation begins
      - Dec 2, 2001: Enron files for bankruptcy"
    }
  })
  
  # ---- Anomaly Detection Plot ----
  output$anomalyPlot <- renderPlot({
    filtered_data <- email_data %>%
      filter(Date >= input$date_range[1] & Date <= input$date_range[2])
    # Calculate z-score for anomaly detection
    filtered_data <- filtered_data %>%
      mutate(z = (Count - mean(Count)) / sd(Count))
    threshold <- 2 # z-score threshold for anomaly
    anomalies <- filtered_data %>% filter(abs(z) > threshold)
    
    ggplot(filtered_data, aes(x = Date, y = Count)) +
      geom_line() +
      geom_point(data = anomalies, aes(x = Date, y = Count), color = "red", size = 2) +
      labs(title = "Anomaly Detection in Monthly Email Counts",
           x = "Date",
           y = "Number of Emails") +
      theme_minimal() +
      scale_x_date(date_breaks = "3 months", date_labels = "%b %Y") +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))
  })
  
  # ---- Role Analysis Plot ----
  output$rolePlot <- renderPlot({
    if (input$role_metric == "Number of Emails") {
      role_data <- employeelist %>%
        left_join(message %>% count(sender_clean), by = c("Email_clean" = "sender_clean")) %>%
        group_by(status) %>%
        summarize(avg_emails = mean(n, na.rm = TRUE)) %>%
        ungroup() %>%
        as.data.frame()  # Convert to regular data frame
      
      ggplot(role_data, aes(x = reorder(status, avg_emails), y = avg_emails)) +
        geom_col(fill = "#2c7fb8") +
        coord_flip() +
        labs(title = "Average Number of Emails by Role",
             x = "Role",
             y = "Average Number of Emails") +
        theme_minimal()
    } else {
      # Email length analysis by role
      role_length_data <- message %>%
        left_join(referenceinfo, by = "mid") %>%  # join to get the email content
        left_join(employeelist, by = c("sender_clean" = "Email_clean")) %>%
        mutate(email_length = str_length(reference)) %>%  # use the correct column
        group_by(status) %>%
        summarize(avg_length = mean(email_length, na.rm = TRUE)) %>%
        ungroup() %>%
        as.data.frame()  # Convert to regular data frame
      
      ggplot(role_length_data, aes(x = reorder(status, avg_length), y = avg_length)) +
        geom_col(fill = "#2c7fb8") +
        coord_flip() +
        labs(title = "Average Email Length by Role",
             x = "Role",
             y = "Average Email Length (characters)") +
        theme_minimal()
    }
  })
  
  # ---- Role-to-Role Heatmap ----
  output$roleHeatmap <- renderPlot({
    # Join sender and recipient roles
    sender_roles <- employeelist %>% select(Email_clean, sender_status = status)
    recipient_roles <- employeelist %>% select(Email_clean, recipient_status = status)
    
    # Prepare sender-recipient pairs from employee emails only
    role_pairs <- recipientinfo %>%
      mutate(rvalue_clean = tolower(trimws(rvalue))) %>%
      left_join(message %>% select(mid, sender_clean), by = "mid") %>%
      left_join(sender_roles, by = c("sender_clean" = "Email_clean")) %>%
      left_join(recipient_roles, by = c("rvalue_clean" = "Email_clean")) %>%
      filter(!is.na(sender_status) & !is.na(recipient_status))
    
    heatmap_data <- role_pairs %>%
      count(sender_status, recipient_status) %>%
      tidyr::pivot_wider(names_from = recipient_status, values_from = n, values_fill = 0)
    
    # Convert to matrix for heatmap
    mat <- as.matrix(heatmap_data[,-1])
    rownames(mat) <- heatmap_data$sender_status
    
    # Plot heatmap
    heatmap_df <- as.data.frame(as.table(mat))
    colnames(heatmap_df) <- c("Sender Role", "Recipient Role", "Count")
    ggplot(heatmap_df, aes(x = `Recipient Role`, y = `Sender Role`, fill = Count)) +
      geom_tile() +
      scale_fill_gradient(low = "white", high = "#2c7fb8") +
      labs(title = "Email Volume Between Roles",
           x = "Recipient Role",
           y = "Sender Role",
           fill = "Email Count") +
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))
  })
  
  # ---- Content Analysis: Word Cloud ----
  output$wordCloudPlot <- renderPlot({
    # Get messages from selected sender
    sender_messages <- message %>%
      filter(sender_clean == input$sender_content) %>%
      left_join(referenceinfo, by = "mid") %>%
      pull(reference) %>%
      as.character()  # Ensure character type
    
    # Create corpus and clean text
    corpus <- Corpus(VectorSource(sender_messages))
    corpus <- tm_map(corpus, content_transformer(tolower))
    corpus <- tm_map(corpus, removePunctuation)
    corpus <- tm_map(corpus, removeWords, stopwords("english"))
    
    # Convert corpus to term-document matrix
    tdm <- TermDocumentMatrix(corpus)
    m <- as.matrix(tdm)
    v <- sort(rowSums(m), decreasing = TRUE)
    d <- data.frame(word = names(v), freq = v)
    
    # Create word cloud
    wordcloud(words = d$word, 
              freq = d$freq, 
              max.words = input$max_words,
              colors = brewer.pal(8, "Dark2"),
              random.order = FALSE)
  })
  
  # ---- Content Analysis: Email Length Distribution ----
  output$emailLengthPlot <- renderPlot({
    length_data <- message %>%
      { if (input$sender_content != "ALL") filter(., sender_clean == input$sender_content) else . } %>%
      left_join(referenceinfo, by = "mid") %>%
      mutate(email_length = str_length(reference)) %>%
      select(email_length) %>%
      as.data.frame()  # Convert to regular data frame
    
    ggplot(length_data, aes(x = email_length)) +
      geom_histogram(bins = 30, fill = "#2c7fb8") +
      labs(title = "Distribution of Email Lengths",
           x = "Email Length (characters)",
           y = "Count") +
      theme_minimal()
  })

  # ---- Content Analysis: N-gram Analysis ----
  output$ngramPlot <- renderPlot({
    sender_messages <- message %>%
      { if (input$sender_content != "ALL") filter(., sender_clean == input$sender_content) else . } %>%
      left_join(referenceinfo, by = "mid") %>%
      pull(reference) %>%
      as.character()
    
    text_df <- tibble(text = sender_messages)
    ngram_n <- input$ngram_n
    ngram_top <- input$ngram_top
    
    ngrams <- text_df %>%
      unnest_tokens(ngram, text, token = "ngrams", n = ngram_n) %>%
      count(ngram, sort = TRUE) %>%
      filter(!is.na(ngram)) %>%
      slice_max(n, n = ngram_top)
    
    ggplot(ngrams, aes(x = reorder(ngram, n), y = n)) +
      geom_col(fill = "#2c7fb8") +
      coord_flip() +
      labs(title = paste("Top", ngram_top, paste0(ngram_n, "-grams"), "in Emails"),
           x = paste0(ngram_n, "-gram"),
           y = "Frequency") +
      theme_minimal()
  })
  
  # ---- Network Visualization ----
  output$networkPlot <- renderVisNetwork({
    # Limit the number of edges for performance
    edges_sample <- edges %>% sample_n(min(nrow(edges), input$max_edges))
    
    visNetwork(nodes, edges_sample) %>%
      visEdges(smooth = FALSE, width = "value") %>%
      visOptions(highlightNearest = TRUE, nodesIdSelection = TRUE) %>%
      visGroups(groupname = "CEO", color = "red") %>%
      visGroups(groupname = "Director", color = "orange") %>%
      visGroups(groupname = "Manager", color = "blue") %>%
      visGroups(groupname = "Employee", color = "green") %>%
      visLegend()
  })

  output$totalEmails <- renderValueBox({
    valueBox(value = nrow(message), subtitle = "Total Emails", icon = icon("envelope"))
  })
}

# Run the app
shinyApp(ui = ui, server = server)

