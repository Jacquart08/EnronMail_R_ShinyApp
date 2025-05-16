# Enron Email Explorer (R Shiny App)

This application is an interactive R Shiny dashboard for exploring and analyzing the Enron Emails database. It provides fast, visual, and statistical insights into one of the most famous corporate scandals in history.

## Features

- **Temporal Analysis:**  
  Visualize email activity over time, detect anomalies, and relate trends to key Enron events.

- **Role Analysis:**  
  Explore how employee roles (e.g., CEO, Manager, Employee) affect email activity, including top senders and role-to-role communication heatmaps.

- **Content Analysis:**  
  Generate word clouds, n-gram frequency plots, and email length distributions for any sender.

- **Network Visualization:**  
  Interactively explore the network of email connections between Enron employees.

- **In-Depth Statistical Analysis:**  
  Run hypothesis tests, regression models, clustering, correlation analysis, and outlier detection on the email data.

## Getting Started

### 1. Clone the Repository

```sh
git clone https://github.com/Jacquart08/EnronMail_R_ShinyApp.git
cd EnronMail_R_ShinyApp
```

### 2. Download the Data

- Download the pre-processed Enron dataset (`Enron.tar.gz`) from the [Enron Email Dataset](https://www.cs.cmu.edu/~enron/) and update the code in order to transform it into Rdata.
- Place the file in a local directory (avoid cloud sync folders like OneDrive for best results).
- Update the `my_path` variable in `app.R` and in `Enron Email Database tatistical Analysis Report` if your file is in a different location.

### 3. Install Required R Packages

Open R or RStudio and run:

```r
install.packages(c(
  "shiny", "ggplot2", "dplyr", "stringr", "lubridate", "wordcloud", "tm",
  "visNetwork", "tidytext", "shinythemes", "shinyBS", "shinycssloaders",
  "shinydashboard", "corrplot"
))
```

### 4. Run the App

In R or RStudio, set your working directory to the project folder and run:

```r
shiny::runApp("app.R")
```

Or simply open `app.R` in RStudio and click "Run App".

## Data Source

- The Enron Email Dataset: [CMU Enron Data](https://www.cs.cmu.edu/~enron/)
- Pre-processed version not provided in this repository.

## Credits

- App developed by Charles Manil for the "R for Big Data" final exam at DSTI.
- Inspired by the Enron scandal and the need for transparent data analysis.

## License

This project is for educational and research purposes.  
See [LICENSE](LICENSE) for details (add a license file if you wish).

---

**For any questions or issues, please open an issue on the [GitHub repository](https://github.com/Jacquart08/EnronMail_R_ShinyApp).**
