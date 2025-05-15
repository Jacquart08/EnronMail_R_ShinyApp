# ---- Load Required Libraries ----
library(shiny) #for the app
library(ggplot2) #for the plots
library(dplyr) #for the data manipulation
library(stringr) #for the string manipulation
library(lubridate) #for the date manipulation
library(wordcloud) #for the word cloud
library(tm) #for the text mining
library(visNetwork) #for the network visualization
library(tidytext) #for the text mining
library(shinythemes) #for the theme
library(shinyBS) #for the tooltips
library(shinycssloaders) #for the loading spinners
library(shinydashboard) #for the dashboard

# ---- Load the Enron Dataset ----
my_path <- "C:/Users/CM_LAPTOP/Downloads/Enron.Rdata"
load(my_path)
# print("Loaded Enron data!") # Debug

# ---- Data Cleaning and Preparation ----

# Clean and standardize sender email addresses in the message data
message <- message %>%
  mutate(sender_email = tolower(trimws(sender)))

# Clean and standardize employee email addresses
employeelist <- employeelist %>%
  mutate(emp_email = tolower(trimws(Email_id)))

# Get unique employee roles for filtering in the UI
unique_roles <- sort(unique(na.omit(as.character(employeelist$status))))

# Prepare top senders data with proper status information
# Count emails per sender and join with employee info
# Create a readable label for each sender

# Count emails per sender
sender_counts <- message %>%
  count(sender_email, sort = TRUE)

# Join with employee info and create label
# If status is available, use full name and status, otherwise just show the email
top_senders <- sender_counts %>%
  left_join(employeelist %>% select(emp_email, status, firstName, lastName), by = c("sender_email" = "emp_email")) %>%
  mutate(
    label = ifelse(!is.na(status) & !is.na(firstName) & !is.na(lastName), # If status is available, use full name and status, otherwise just show the email
                   paste0(firstName, " ", lastName, " (", status, ")"),
                   sender_email)
  )

# Create a data frame with monthly email counts
email_data <- data.frame(
  Date = as.Date(paste0(format(message$date, "%Y-%m"), "-01")),
  Count = 1
)

# Correct errors in the year data in the dataframe here 0001, 0002, 1979, 2020 are wrong, 
# we are using 2001, 2002, 1997, 2002 respectively instead
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
  summarize(Count = sum(Count), .groups = "drop")

# Role analysis 

# ---- Prepare Nodes and Edges for Network Visualization ----

# Nodes: Each employee as a node
nodes <- employeelist %>%
  mutate(id = emp_email, # id is the email address so we can use it to connect the nodes
         label = paste(firstName, lastName), # label is the full name of the employee
         group = status) %>% # group is the status of the employee
  select(id, label, group)

# Edges: Email connections between employees
edges <- recipientinfo %>%
  mutate(rvalue_clean = tolower(trimws(rvalue))) %>%
  left_join(message %>% select(mid, sender_email), by = "mid") %>% # left join the message data with the recipientinfo data because not in the same DF
  left_join(employeelist %>% select(emp_email, Email_id), by = c("sender_email" = "emp_email")) %>% # same as above
  mutate(from = sender_email, to = rvalue_clean) %>% # from is the sender and to is the recipient
  filter(!is.na(from) & !is.na(to)) %>% # filter out the NAs as we have some
  filter(from %in% nodes$id & to %in% nodes$id) %>% 
  group_by(from, to) %>%
  summarize(value = n(), .groups = "drop")

# ---- Define UI for Application ----
ui <- fluidPage(
  theme = shinytheme("flatly"),
  # Place enron_logo.png and dsti_logo.png in the www/ directory in order for the images to be displayed
  div(
    style = "display: flex; align-items: center; justify-content: center; gap: 40px; margin-bottom: 20px; background: #f8f9fa; border-radius: 10px; box-shadow: 0 2px 8px #e0e0e0; padding: 10px;",
    img(src = "enron_logo.png", height = "60px", alt = "Enron Logo", style = "border-radius: 8px;"),
    div(
      style = "text-align: center; flex: 1;",
      HTML("<h1 style='margin-bottom: 0; color: #2c3e50;'>Enron Exploratory App</h1>
            <h4 style='margin: 0; color: #2c3e50;'>R for Big Data final exam</h4>
            <h5 style='margin-top: 0; color: #2c3e50;'>Charles Manil</h5>")
    ),
    img(src = "dsti_logo.png", height = "60px", alt = "DSTI Logo", style = "border-radius: 8px;")
  ),
  div(style = "height: 10px;"),
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
          fluidRow(
            column(12, withSpinner(valueBoxOutput("totalEmails_ta")))
          ),
          tabsetPanel(
            id = "temporal_tab",
            tabPanel("Time Series", 
                     withSpinner(div(`aria-label` = "Time Series Plot", plotOutput("timeSeriesPlot"))),
                     div(
                       style = "margin-top: 5px; margin-bottom: 0px; padding: 2px 8px 2px 8px; background: #f8f9fa; border-radius: 6px; width: fit-content;",
                       verbatimTextOutput("eventInfo")
                     )),
            tabPanel("Anomaly Detection", 
                     withSpinner(div(`aria-label` = "Anomaly Detection Plot", plotOutput("anomalyPlot"))))
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
          fluidRow(
            column(12, withSpinner(valueBoxOutput("totalEmails_ra")))
          ),
          tabsetPanel(
            id = "role_tab",
            tabPanel("Most Active Employees", 
                     withSpinner(div(`aria-label` = "Top Senders Plot", plotOutput("topSendersPlot")))),
            tabPanel("Role Plot", 
                     withSpinner(div(`aria-label` = "Role Plot", plotOutput("rolePlot")))),
            tabPanel("Role-to-Role Heatmap", 
                     withSpinner(div(`aria-label` = "Role-to-Role Heatmap", plotOutput("roleHeatmap"))))
          )
        )
      )
    ),
    
    # Tab 3: Content Analysis
    tabPanel(title = tagList(icon("align-left"), "Content Analysis"),
      sidebarLayout(
        sidebarPanel(
          selectInput("sender_content", "Select sender:",
                     choices = unique(top_senders$sender_email),
                     selected = unique(top_senders$sender_email)[1]),
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
          fluidRow(
            column(12, withSpinner(valueBoxOutput("totalEmails_ca")))
          ),
          tabsetPanel(
            id = "content_tab",
            tabPanel("Word Cloud", 
                     withSpinner(div(`aria-label` = "Word Cloud Plot", plotOutput("wordCloudPlot")))),
            tabPanel("Email Length", 
                     withSpinner(div(`aria-label` = "Email Length Plot", plotOutput("emailLengthPlot")))),
            tabPanel("N-gram Analysis", 
                     withSpinner(div(`aria-label` = "N-gram Plot", plotOutput("ngramPlot"))))
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
          fluidRow(
            column(12, withSpinner(valueBoxOutput("totalEmails_nv")))
          ),
          withSpinner(div(`aria-label` = "Network Plot", visNetworkOutput("networkPlot", height = "600px")))
       )
     )
    ),
    # Tab 5: In Depth Analysis
    tabPanel(title = tagList(icon("chart-bar"), "In Depth Analysis"),
      sidebarLayout(
        sidebarPanel(
          conditionalPanel(
            condition = "input.analysis_tab == 'Summary Statistics'",
            helpText("View summary statistics for email activity by role, sender, or over time.")
          ),
          conditionalPanel(
            condition = "input.analysis_tab == 'Hypothesis Testing'",
            selectInput("htest_var", "Variable:", choices = c("Email Length")),
            selectInput("htest_group1", "Group 1 (Role):", choices = unique_roles),
            selectInput("htest_group2", "Group 2 (Role):", choices = unique_roles, selected = unique_roles[2]),
            helpText("Test if the selected variable differs between two roles (t-test).")
          ),
          conditionalPanel(
            condition = "input.analysis_tab == 'Regression'",
            selectInput("regress_y", "Dependent variable:", choices = c("Email Length", "Email Count")),
            selectInput("regress_x", "Predictor:", choices = c("Role")),
            helpText("Fit a simple regression model to explore relationships.")
          ),
          conditionalPanel(
            condition = "input.analysis_tab == 'Clustering'",
            sliderInput("cluster_k", "Number of clusters:", min = 2, max = 6, value = 3),
            helpText("Cluster employees based on their email activity.")
          ),
          conditionalPanel(
            condition = "input.analysis_tab == 'Correlation Matrix'",
            helpText("View the correlation matrix for numeric features (e.g., email count, average length).")
          ),
          conditionalPanel(
            condition = "input.analysis_tab == 'Outlier Detection'",
            helpText("Detect and visualize outliers in email activity or content length.")
          )
        ),
        mainPanel(
          tabsetPanel(
            id = "analysis_tab",
            tabPanel("Summary Statistics", 
                     withSpinner(
                       div(
                         style = "margin-top: 5px; margin-bottom: 0px; padding: 2px 8px 2px 8px; background: #f8f9fa; border-radius: 6px; width: fit-content;",
                         tableOutput("summaryStats")
                       )
                     )),
            tabPanel("Hypothesis Testing", 
                     withSpinner(
                       div(
                         style = "margin-top: 5px; margin-bottom: 0px; padding: 2px 8px 2px 8px; background: #f8f9fa; border-radius: 6px; width: fit-content;",
                         verbatimTextOutput("htestResult")
                       )
                     )),
            tabPanel("Regression", 
                     withSpinner(
                       div(
                         style = "margin-top: 5px; margin-bottom: 0px; padding: 2px 8px 2px 8px; background: #f8f9fa; border-radius: 6px; width: fit-content;",
                         verbatimTextOutput("regressResult")
                       )
                     )),
            tabPanel("Clustering", 
                     withSpinner(div(`aria-label` = "Clustering Plot", plotOutput("clusterPlot")))),
            tabPanel("Correlation Matrix", 
                     withSpinner(div(`aria-label` = "Correlation Matrix Plot", plotOutput("corrPlot")))),
            tabPanel("Outlier Detection", 
                     withSpinner(div(`aria-label` = "Outlier Detection Plot", plotOutput("outlierPlot"))))
          )
        )
      )
    ),
    valueBoxOutput("totalEmails")
  ),
  tags$footer(
    style = "text-align:center; margin-top:30px; color: #888; background: #f8f9fa; padding: 10px; border-radius: 10px; font-size: 16px;",
    HTML("<span tabindex='0' aria-label='Footer'>© 2024 Charles Manil | R for Big Data Final Exam | <a href='https://github.com/Jacquart08/EnronMail_R_ShinyApp' target='_blank' style='color: #2c7fb8;'>GitHub Repo</a></span>")
  )
)

server <- function(input, output) {
  
  # ---- Plot: Most Active Employees ----
output$topSendersPlot <- renderPlot({
  # Start with full sender list
  filtered_senders <- top_senders

  # If the user picked a specific status, narrow it down
  if (input$status_filter != "All") {
    filtered_senders <- filtered_senders %>%
      filter(status == input$status_filter & !is.na(status))  # skip rows with missing status
  }

  # Grab the top N senders from the filtered list
  plot_data <- filtered_senders %>%
    slice_max(n, n = input$n_senders) %>%
    as.data.frame()  # helps avoid weird jsonlite warnings in Shiny

  # Basic horizontal bar plot of senders
  ggplot(plot_data, aes(x = reorder(label, n), y = n)) +
    geom_col(fill = "#2c7fb8") +
    coord_flip() +
    labs(
      title = paste("Top", input$n_senders, "Senders with Job Titles"),
      x = "Sender (Job Title)",
      y = "Emails Sent"
    ) +
    theme_minimal(base_size = 12) +
    theme(axis.text.y = element_text(size = 10))
})
  
# ---- Plot: Emails Over Time ----
output$timeSeriesPlot <- renderPlot({
  # Narrow data to selected date range
  filtered_data <- email_data %>%
    filter(Date >= input$date_range[1], Date <= input$date_range[2])

  # Build base time series plot
  p <- ggplot(filtered_data, aes(x = Date, y = Count)) +
    geom_line() +
    labs(
      title = "Monthly Email Counts",
      x = "Date",
      y = "Number of Emails"
    ) +
    theme_minimal() +
    scale_x_date(date_breaks = "3 months", date_labels = "%b %Y") +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))

  # Optionally add key events (if checkbox is checked)
  if (input$show_events) {
    important_dates <- as.Date(c("2001-09-01", "2001-10-31", "2001-12-02"))

    p <- p + geom_vline(
      xintercept = important_dates,
      color = "red",
      linetype = "dashed"
    )
  }

  # Return the plot
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
  
  # ---- Plot: Role-Based Email Analysis ----
output$rolePlot <- renderPlot({
  if (input$role_metric == "Number of Emails") {
    # Count emails per sender, then calculate average per role
    role_data <- employeelist %>%
      left_join(
        message %>% count(sender_email),
        by = c("emp_email" = "sender_email")
      ) %>%
      group_by(status) %>%
      summarize(avg_emails = mean(n, na.rm = TRUE)) %>%
      ungroup() %>%
      as.data.frame()  # just in case for Shiny render issues

    ggplot(role_data, aes(x = reorder(status, avg_emails), y = avg_emails)) +
      geom_col(fill = "#2c7fb8") +
      coord_flip() +
      labs(
        title = "Avg. Emails Sent by Role",
        x = "Role",
        y = "Avg. Number of Emails"
      ) +
      theme_minimal()
    
  } else {
    # Calculate avg. email length by role
    role_length_data <- message %>%
      left_join(referenceinfo, by = "mid") %>%  # get message content
      left_join(employeelist, by = c("sender_email" = "emp_email")) %>%
      mutate(email_length = str_length(reference)) %>%
      group_by(status) %>%
      summarize(avg_length = mean(email_length, na.rm = TRUE)) %>%
      ungroup() %>%
      as.data.frame()

    ggplot(role_length_data, aes(x = reorder(status, avg_length), y = avg_length)) +
      geom_col(fill = "#2c7fb8") +
      coord_flip() +
      labs(
        title = "Avg. Email Length by Role",
        x = "Role",
        y = "Avg. Email Length (chars)"
      ) +
      theme_minimal()
  }
})
  
  # ---- Role-to-Role Heatmap ----
  output$roleHeatmap <- renderPlot({
    # Join sender and recipient roles
    sender_roles <- employeelist %>% select(emp_email, sender_status = status)
    recipient_roles <- employeelist %>% select(emp_email, recipient_status = status)
    
    # Prepare sender-recipient pairs from employee emails only
    role_pairs <- recipientinfo %>%
      mutate(rvalue_clean = tolower(trimws(rvalue))) %>%
      left_join(message %>% select(mid, sender_email), by = "mid") %>%
      left_join(sender_roles, by = c("sender_email" = "emp_email")) %>%
      left_join(recipient_roles, by = c("rvalue_clean" = "emp_email")) %>%
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
      filter(sender_email == input$sender_content) %>%
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
      { if (input$sender_content != "ALL") filter(., sender_email == input$sender_content) else . } %>%
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

# ---- Plot: N-Gram Frequency ----
output$ngramPlot <- renderPlot({
  # Filter messages if a specific sender is selected
  sender_messages <- message %>%
    { 
      if (input$sender_content != "ALL") {
        filter(., sender_email == input$sender_content)
      } else {
        .
      }
    } %>%
    left_join(referenceinfo, by = "mid") %>%
    pull(reference) %>%
    as.character()

  # Put everything into a tibble
  text_df <- tibble(text = sender_messages)

  # Get settings from UI
  ngram_n <- input$ngram_n
  ngram_top <- input$ngram_top

  # Generate top N n-grams
  ngrams <- text_df %>%
    unnest_tokens(ngram, text, token = "ngrams", n = ngram_n) %>%
    count(ngram, sort = TRUE) %>%
    filter(!is.na(ngram)) %>%
    slice_max(n, n = ngram_top)

  # Plot the result
  ggplot(ngrams, aes(x = reorder(ngram, n), y = n)) +
    geom_col(fill = "#2c7fb8") +
    coord_flip() +
    labs(
      title = paste("Top", ngram_top, paste0(ngram_n, "-grams"), "in Emails"),
      x = paste(ngram_n, "-gram"),
      y = "Frequency"
    ) +
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

  output$totalEmails_ta <- renderValueBox({
    valueBox(value = nrow(message), subtitle = "Total Emails", icon = icon("envelope"), color = "blue")
  })

  output$totalEmails_ra <- renderValueBox({
    valueBox(value = nrow(message), subtitle = "Total Emails", icon = icon("envelope"), color = "blue")
  })

  output$totalEmails_ca <- renderValueBox({
    valueBox(value = nrow(message), subtitle = "Total Emails", icon = icon("envelope"), color = "blue")
  })

  output$totalEmails_nv <- renderValueBox({
    valueBox(value = nrow(message), subtitle = "Total Emails", icon = icon("envelope"), color = "blue")
  })

  # ---- Analysis: Summary Statistics ----
  output$summaryStats <- renderTable({
    # Example: summary of email length by role
    message %>%
      left_join(referenceinfo, by = "mid") %>%
      left_join(employeelist, by = c("sender_email" = "emp_email")) %>%
      mutate(email_length = str_length(reference)) %>%
      group_by(status) %>%
      summarize(
        n_emails = n(),
        avg_length = mean(email_length, na.rm = TRUE),
        sd_length = sd(email_length, na.rm = TRUE)
      )
  })

# ---- Hypothesis Testing: Email Length by Role ----
output$htestResult <- renderPrint({
  # Make sure both group inputs are provided
  req(input$htest_group1, input$htest_group2)

  # Join message text and employee info
  dat <- message %>%
    left_join(referenceinfo, by = "mid") %>%
    left_join(employeelist, by = c("sender_email" = "emp_email")) %>%
    mutate(email_length = str_length(reference)) %>%
    filter(status %in% c(input$htest_group1, input$htest_group2))

  # Pull lengths for the two groups
  group1 <- dat$email_length[dat$status == input$htest_group1]
  group2 <- dat$email_length[dat$status == input$htest_group2]

  # Run t-test if there's enough data
  if (length(group1) > 1 && length(group2) > 1) {
    t.test(group1, group2)
  } else {
    cat("Not enough messages to compare these groups.\nTry selecting roles with more data.")
  }
})

  # ---- Analysis: Regression ----
output$regressResult <- renderPrint({
  # Simple regression to explore role effect on email metrics
  dat <- message %>%
    left_join(referenceinfo, by = "mid") %>%
    left_join(employeelist, by = c("sender_email" = "emp_email")) %>%
    mutate(email_length = str_length(reference))

  if (input$regress_y == "Email Length") {
    # Model: email length ~ role
    fit <- lm(email_length ~ status, data = dat)
    summary(fit)
  } else {
    # Model: email count ~ role
    count_dat <- dat %>%
      group_by(status) %>%
      summarize(email_count = n(), .groups = "drop")

    fit <- lm(email_count ~ status, data = count_dat)
    summary(fit)
  }
})

# ---- Analysis: Clustering ----
output$clusterPlot <- renderPlot({
  # Cluster employees based on email volume and avg length
  dat <- message %>%
    left_join(referenceinfo, by = "mid") %>%
    left_join(employeelist, by = c("sender_email" = "emp_email")) %>%
    mutate(email_length = str_length(reference)) %>%
    group_by(sender_email, status) %>%
    summarize(
      n_emails = n(),
      avg_length = mean(email_length, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    na.omit()

  # Safety check — too few rows for K-means
  if (nrow(dat) < input$cluster_k) {
    cat("Not enough records to form", input$cluster_k, "clusters.")
    return(NULL)
  }

  # Apply K-means
  km <- kmeans(dat[, c("n_emails", "avg_length")], centers = input$cluster_k)
  dat$cluster <- as.factor(km$cluster)

  # Plot clusters
  ggplot(dat, aes(x = n_emails, y = avg_length, color = cluster)) +
    geom_point(size = 3) +
    labs(
      title = "K-means Clustering of Employees",
      x = "Number of Emails",
      y = "Avg. Email Length"
    ) +
    theme_minimal()
})

# ---- Analysis: Correlation Matrix ----
output$corrPlot <- renderPlot({
  # Compare email volume and text length per sender
  dat <- message %>%
    left_join(referenceinfo, by = "mid") %>%
    left_join(employeelist, by = c("sender_email" = "emp_email")) %>%
    mutate(email_length = str_length(reference)) %>%
    group_by(sender_email) %>%
    summarize(
      n_emails = n(),
      avg_length = mean(email_length, na.rm = TRUE),
      .groups = "drop"
    )

  # Compute correlation
  corr_mat <- cor(dat[, c("n_emails", "avg_length")], use = "complete.obs")

  # Display
  corrplot::corrplot(
    corr_mat,
    method = "color",
    addCoef.col = "black",
    tl.col = "black",
    number.cex = 1.2
  )
})

# ---- Analysis: Outlier Detection ----
output$outlierPlot <- renderPlot({
  # Identify employees with unusually high email counts
  dat <- message %>%
    left_join(referenceinfo, by = "mid") %>%
    left_join(employeelist, by = c("sender_email" = "emp_email")) %>%
    mutate(email_length = str_length(reference)) %>%
    group_by(sender_email) %>%
    summarize(
      n_emails = n(),
      avg_length = mean(email_length, na.rm = TRUE),
      .groups = "drop"
    )

  # Basic outlier threshold: mean + 2*sd
  outlier_thresh <- mean(dat$n_emails) + 2 * sd(dat$n_emails)
  dat$outlier <- dat$n_emails > outlier_thresh

  ggplot(dat, aes(x = n_emails, y = avg_length, color = outlier)) +
    geom_point(size = 3) +
    scale_color_manual(values = c("black", "red")) +
    labs(
      title = "Outlier Detection: Emails vs. Length",
      x = "Total Emails Sent",
      y = "Avg. Email Length"
    ) +
    theme_minimal()
})
}

# Run the app
shinyApp(ui = ui, server = server)

