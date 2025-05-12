library(shiny)
library(ggplot2)
library(dplyr)
library(stringr)
library(lubridate)
library(wordcloud)
library(tm)

# Loading the data set
my_path <- "C:/Users/CM_LAPTOP/Downloads/Enron.Rdata"
load(my_path)

# Clean and merge sender info
message <- message %>%
  mutate(sender_clean = tolower(trimws(sender)))

employeelist <- employeelist %>%
  mutate(Email_clean = tolower(trimws(Email_id)))

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

# Define UI for application
ui <- fluidPage(
  titlePanel("Enron Email Database Exploration"),
  
  tabsetPanel(
    # Tab 1: Most Active Employees
    tabPanel("Most Active Employees",
      sidebarLayout(
        sidebarPanel(
          sliderInput("n_senders", "Number of top senders to display:", 
                     min = 5, max = 50, value = 20),
          selectInput("status_filter", "Filter by status:",
                     choices = c("All", sort(unique(employeelist$status[!is.na(employeelist$status)]))),
                     selected = "All")
        ),
        mainPanel(
          plotOutput("topSendersPlot")
        )
      )
    ),
    
    # Tab 2: Temporal Analysis
    tabPanel("Temporal Analysis",
      sidebarLayout(
        sidebarPanel(
          dateRangeInput("date_range", "Select date range:",
                        start = "1999-01-01",
                        end = "2002-12-31",
                        min = "1999-01-01",
                        max = "2002-12-31"),
          checkboxInput("show_events", "Show key events", value = TRUE)
        ),
        mainPanel(
          plotOutput("timeSeriesPlot"),
          verbatimTextOutput("eventInfo")
        )
      )
    ),
    
    # Tab 3: Role Analysis
    tabPanel("Role Analysis",
      sidebarLayout(
        sidebarPanel(
          selectInput("role_metric", "Select metric:",
                     choices = c("Number of Emails", "Average Email Length"),
                     selected = "Number of Emails")
        ),
        mainPanel(
          plotOutput("rolePlot")
        )
      )
    ),
    
    # Tab 4: Content Analysis
    tabPanel("Content Analysis",
      sidebarLayout(
        sidebarPanel(
          selectInput("sender_content", "Select sender:",
                     choices = unique(top_senders$sender_clean),
                     selected = top_senders$sender_clean[1]),
          sliderInput("max_words", "Maximum number of words:",
                     min = 10, max = 100, value = 50)
        ),
        mainPanel(
          plotOutput("wordCloudPlot"),
          plotOutput("emailLengthPlot")
        )
      )
    )
  )
)

server <- function(input, output) {
  
  # Most Active Employees Plot
  output$topSendersPlot <- renderPlot({
    filtered_senders <- top_senders
    
    # Apply status filter if not "All"
    if (input$status_filter != "All") {
      filtered_senders <- filtered_senders %>%
        filter(!is.na(status) & status == input$status_filter)
    }
    
    # Get the top N senders after filtering
    filtered_senders %>%
      slice_max(n, n = input$n_senders) %>%
      ggplot(aes(x = reorder(label, n), y = n)) +
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
  
  # Temporal Analysis Plot
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
  
  # Role Analysis Plot
  output$rolePlot <- renderPlot({
    if (input$role_metric == "Number of Emails") {
      employeelist %>%
        left_join(message %>% count(sender_clean), by = c("Email_clean" = "sender_clean")) %>%
        group_by(status) %>%
        summarize(avg_emails = mean(n, na.rm = TRUE)) %>%
        ggplot(aes(x = reorder(status, avg_emails), y = avg_emails)) +
        geom_col(fill = "#2c7fb8") +
        coord_flip() +
        labs(title = "Average Number of Emails by Role",
             x = "Role",
             y = "Average Number of Emails") +
        theme_minimal()
    } else {
      # Email length analysis by role
      role_length_data <- message %>%
        left_join(employeelist, by = c("sender_clean" = "Email_clean")) %>%
        mutate(email_length = str_length(body)) %>%
        group_by(status) %>%
        summarize(avg_length = mean(email_length, na.rm = TRUE)) %>%
        ungroup() %>%
        as.data.frame()  # Convert to regular data frame
      
      # Create the plot with the ungrouped data
      ggplot(role_length_data, aes(x = reorder(status, avg_length), y = avg_length)) +
        geom_col(fill = "#2c7fb8") +
        coord_flip() +
        labs(title = "Average Email Length by Role",
             x = "Role",
             y = "Average Email Length (characters)") +
        theme_minimal()
    }
  })
  
  # Content Analysis
  output$wordCloudPlot <- renderPlot({
    # Get messages from selected sender
    sender_messages <- message %>%
      filter(sender_clean == input$sender_content) %>%
      pull(body) %>%
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
  
  # Email Length Distribution
  output$emailLengthPlot <- renderPlot({
    length_data <- message %>%
      filter(sender_clean == input$sender_content) %>%
      mutate(email_length = str_length(body)) %>%
      select(email_length) %>%
      as.data.frame()  # Convert to regular data frame
    
    ggplot(length_data, aes(x = email_length)) +
      geom_histogram(bins = 30, fill = "#2c7fb8") +
      labs(title = "Distribution of Email Lengths",
           x = "Email Length (characters)",
           y = "Count") +
      theme_minimal()
  })
}

# Run the app
shinyApp(ui = ui, server = server)

