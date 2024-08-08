# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~-----------------------------------------

##################################   Packages   ################################

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~-----------------------------------------

pacman::p_load(readxl,tidymodels,tidyverse,lubridate,dplyr,caret,janitor,tidyr, 
               readr, skimr,forecast,e1071,xgboost,patchwork,shiny, 
               caret, vip, Metrics, yardstick,tseries)

"
Fold All	Alt+O
Unfold All	Shift+Alt+O
"

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~-----------------------------------------

##############################   Loading Datasets  #############################

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~-----------------------------------------

# Loading Datasets - Unpropagated ----------------------------------------------

load_unpro <- function(file_path) {
  df <- tibble(read.csv(file_path))
  
  df <- df %>%
    mutate(X = as.POSIXct(X, format = "%Y-%m-%d %H:%M:%S")) %>%
    rename(datetime = X) %>%
    filter(!is.na(datetime)) %>%
    clean_names()
  
  return(df)
}

jason_1 <- load_unpro("unpropagated_elements_Jason-1.csv")
jason_2 <- load_unpro("unpropagated_elements_Jason-2.csv")
jason_3 <- load_unpro("unpropagated_elements_Jason-3.csv")

jason_1_raw <- load_unpro("unpropagated_elements_Jason-1.csv")
jason_2_raw <- load_unpro("unpropagated_elements_Jason-2.csv")
jason_3_raw <- load_unpro("unpropagated_elements_Jason-3.csv")


# Loading Datasets - Manoeuvres ------------------------------------------------

load_man <- function(filepath) {
  read.table(filepath, header = FALSE, sep = "\t", stringsAsFactors = FALSE) %>%
    mutate(Extracted = str_extract(V1, "(\\d{4} \\d{3} \\d{2} \\d{2} \\d{4} \\d{3} \\d{2} \\d{2})")) %>%
    select(Extracted) %>%
    separate(Extracted, into = c("b_y", "b_doy", "b_h", "b_m", "e_y", "e_doy", "e_h", "e_m"), sep = " ") %>%
    mutate(across(everything(), as.numeric)) %>%
    mutate(
      begin_date = as.Date(paste(b_y, b_doy, sep = " "), format = "%Y %j"),
      end_date = as.Date(paste(e_y, e_doy, sep = " "), format = "%Y %j"),
      begin_datetime = make_datetime(b_y, month(begin_date), day(begin_date), b_h, b_m),
      end_datetime = make_datetime(e_y, month(end_date), day(end_date), e_h, e_m)) %>%
    select(begin_datetime, end_datetime) %>%
    mutate(gap_hours = as.numeric(difftime(end_datetime, begin_datetime, units = "hour"))) %>%
    tibble()
}

jason_1_man <- load_man("ja1man.txt")
jason_2_man <- load_man("ja2man.txt")
jason_3_man <- load_man("ja3man.txt")

# Checking duration

max_duration_table <- function() {
  # Predefined datasets
  max_durations <- c(
    max(jason_1_man$gap_hours),
    max(jason_2_man$gap_hours),
    max(jason_3_man$gap_hours)
  )
  
  datasets <- c(
    "jason_1_man",
    "jason_2_man",
    "jason_3_man"
  )
  
  # Create the result table
  result_table <- tibble(
    dataset = datasets,
    max_gap_hours = round(max_durations, 2)
  )
  
  return(result_table)
}

man_gap <- max_duration_table()

"
no man duration is longer than a day, 
therefore, all begin and end datetime will be merged into one datetime
"
# Finalizing man data

finalize_man <- function(data) {
  data %>%
    select(datetime = !!names(.)[1])
}

jason_1_man <- finalize_man(jason_1_man)
jason_2_man <- finalize_man(jason_2_man)
jason_3_man <- finalize_man(jason_3_man)

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~-----------------------------------------

####################################   EDA   ###################################

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~-----------------------------------------

# EDA - Plots - Unpropagated  --------------------------------------------------


# Line plots
line_plots <- function(dataset, title = "Dataset Plot", line = "") {
  
  
  p_right_ascension <- ggplot(dataset, aes(x = datetime, y = right_ascension)) +
    geom_line() +
    labs(title = "Right Ascension") +
    theme_minimal() +
    theme(axis.title.x = element_blank(),
          axis.title.y = element_blank(),
          axis.text.x = element_text(angle = 45, hjust = 1, size = 12),
          plot.title = element_text(size = 20)) +
    scale_x_datetime(date_breaks = "1 year", date_labels = "%Y")
  
  
  p_brouwer_mean_motion <- ggplot(dataset, aes(x = datetime, y = brouwer_mean_motion)) +
    geom_line() +
    labs(title = "Brouwer Mean Motion") +
    theme_minimal() +
    theme(axis.title.x = element_blank(),
          axis.title.y = element_blank(),
          axis.text.x = element_text(angle = 45, hjust = 1, size = 12),
          plot.title = element_text(size = 20)) +
    scale_x_datetime(date_breaks = "1 year", date_labels = "%Y")
  
  p_eccentricity <- ggplot(dataset, aes(x = datetime, y = eccentricity)) +
    geom_line() +
    labs(title = "Eccentricity") +
    theme_minimal() +
    theme(axis.title.x = element_blank(),
          axis.title.y = element_blank(),
          axis.text.x = element_text(angle = 45, hjust = 1, size = 12),
          plot.title = element_text(size = 20)) +
    scale_x_datetime(date_breaks = "1 year", date_labels = "%Y")
  
  p_argument_of_perigee <- ggplot(dataset, aes(x = datetime, y = argument_of_perigee)) +
    geom_line() +
    labs(title = "Argument of Perigee") +
    theme_minimal() +
    theme(axis.title.x = element_blank(),
          axis.title.y = element_blank(),
          axis.text.x = element_text(angle = 45, hjust = 1, size = 12),
          plot.title = element_text(size = 20)) +
    scale_x_datetime(date_breaks = "1 year", date_labels = "%Y")
  
  p_inclination <- ggplot(dataset, aes(x = datetime, y = inclination)) +
    geom_line() +
    labs(title = "Inclination") +
    theme_minimal() +
    theme(axis.title.x = element_blank(),
          axis.title.y = element_blank(),
          axis.text.x = element_text(angle = 45, hjust = 1, size = 12),
          plot.title = element_text(size = 20)) +
    scale_x_datetime(date_breaks = "1 year", date_labels = "%Y")
  
  p_mean_anomaly <- ggplot(dataset, aes(x = datetime, y = mean_anomaly)) +
    geom_line() +
    labs(title = "Mean Anomaly") +
    theme_minimal() +
    theme(axis.title.x = element_blank(),
          axis.title.y = element_blank(),
          axis.text.x = element_text(angle = 45, hjust = 1, size = 12),
          plot.title = element_text(size = 20)) +
    scale_x_datetime(date_breaks = "1 year", date_labels = "%Y")
  
  plot_combined <- 
    (p_eccentricity | p_inclination | p_right_ascension) / 
    (p_argument_of_perigee | p_mean_anomaly | p_brouwer_mean_motion)
  
  plot_combined + 
    plot_annotation(
      title = title,
      tag_levels = "A",
      caption = line,
      theme = theme(
        plot.title = element_text(size = 24, face = "bold"),
        plot.caption = element_text(size = 12)
      )
    )
}

line_jason_1_eda <- line_plots(jason_1_raw, "Jason-1 Orbital Elements Line Plot")
line_jason_2_eda <- line_plots(jason_2_raw, "Jason-2 Orbital Elements Line Plot")
line_jason_3_eda <- line_plots(jason_3_raw, "Jason-3 Orbital Elements Line Plot")

# Hist plots
hist_plots <- function(dataset, title = "Dataset Plot", line = "") {
  
  p_right_ascension <- ggplot(dataset, aes(x = right_ascension)) +
    geom_histogram(bins = 30, colour = "black", fill = "grey", alpha = 0.7) +
    labs(title = "Right Ascension") +
    theme_minimal() +
    scale_x_continuous(labels = label_number(accuracy = 0.1)) +
    theme(axis.title.x = element_blank(),
          axis.title.y = element_blank(),
          axis.text.x = element_text(angle = 45, hjust = 1, size = 12),
          plot.title = element_text(size = 20))
  
  p_brouwer_mean_motion <- ggplot(dataset, aes(x = brouwer_mean_motion)) +
    geom_histogram(bins = 30, colour = "black", fill = "grey", alpha = 0.7) +
    labs(title = "Brouwer Mean Motion") +
    theme_minimal() +
    scale_x_continuous(labels = label_number(accuracy = 0.0001)) +
    theme(axis.title.x = element_blank(),
          axis.title.y = element_blank(),
          axis.text.x = element_text(angle = 45, hjust = 1, size = 12),
          plot.title = element_text(size = 20))
  
  p_eccentricity <- ggplot(dataset, aes(x = eccentricity)) +
    geom_histogram(bins = 30, colour = "black", fill = "grey", alpha = 0.7) +
    labs(title = "Eccentricity") +
    theme_minimal() +
    scale_x_continuous(labels = label_number(accuracy = 0.0001)) +
    theme(axis.title.x = element_blank(),
          axis.title.y = element_blank(),
          axis.text.x = element_text(angle = 45, hjust = 1, size = 12),
          plot.title = element_text(size = 20))
  
  p_argument_of_perigee <- ggplot(dataset, aes(x = argument_of_perigee)) +
    geom_histogram(bins = 30, colour = "black", fill = "grey", alpha = 0.7) +
    labs(title = "Argument of Perigee") +
    theme_minimal() +
    scale_x_continuous(labels = label_number(accuracy = 0.001)) +
    theme(axis.title.x = element_blank(),
          axis.title.y = element_blank(),
          axis.text.x = element_text(angle = 45, hjust = 1, size = 12),
          plot.title = element_text(size = 20))
  
  p_inclination <- ggplot(dataset, aes(x = inclination)) +
    geom_histogram(bins = 30, colour = "black", fill = "grey", alpha = 0.7) +
    labs(title = "Inclination") +
    theme_minimal() +
    scale_x_continuous(labels = label_number(accuracy = 0.001)) +
    theme(axis.title.x = element_blank(),
          axis.title.y = element_blank(),
          axis.text.x = element_text(angle = 45, hjust = 1, size = 12),
          plot.title = element_text(size = 20))
  
  p_mean_anomaly <- ggplot(dataset, aes(x = mean_anomaly)) +
    geom_histogram(bins = 30, colour = "black", fill = "grey", alpha = 0.7) +
    labs(title = "Mean Anomaly") +
    theme_minimal() +
    scale_x_continuous(labels = label_number(accuracy = 0.1)) +
    theme(axis.title.x = element_blank(),
          axis.title.y = element_blank(),
          axis.text.x = element_text(angle = 45, hjust = 1, size = 12),
          plot.title = element_text(size = 20))
  
  plot_combined <- 
    (p_eccentricity | p_inclination | p_right_ascension) / 
    (p_argument_of_perigee | p_mean_anomaly | p_brouwer_mean_motion)
  
  plot_combined + 
    plot_annotation(
      title = title,
      tag_levels = "A",
      caption = line,
      theme = theme(
        plot.title = element_text(size = 24, face = "bold"),
        plot.caption = element_text(size = 12)
        )
      )
}

hist_jason_1_eda <- hist_plots(jason_1_raw, "Jason-1 Orbital Elements Histogram Plots")
hist_jason_2_eda <- hist_plots(jason_2_raw, "Jason-2 Orbital Elements Histogram Plots")
hist_jason_3_eda <- hist_plots(jason_3_raw, "Jason-3 Orbital Elements Histogram Plots")

# Boxplots
box_plots <- function(dataset, title = "Dataset Plot", line = "") {
  
  
  bp_eccentricity <- ggplot(dataset, aes(x = "", y = eccentricity)) +
    geom_boxplot(outlier.colour = "red", width = 0.5) +
    labs(title = "Eccentricity") +
    theme_minimal() +
    theme(axis.title.x = element_blank(),
          axis.title.y = element_blank(),
          plot.title = element_text(size = 20))
  
  # Boxplot for Argument of Perigee
  bp_argument_of_perigee <- ggplot(dataset, aes(x = "", y = argument_of_perigee)) +
    geom_boxplot(outlier.colour = "red", width = 0.5) +
    labs(title = "Argument of Perigee") +
    theme_minimal() +
    theme(axis.title.x = element_blank(),
          axis.title.y = element_blank(),
          plot.title = element_text(size = 20))
  
  # Boxplot for Inclination
  bp_inclination <- ggplot(dataset, aes(x = "", y = inclination)) +
    geom_boxplot(outlier.colour = "red", width = 0.5) +
    labs(title = "Inclination") +
    theme_minimal() +
    theme(axis.title.x = element_blank(),
          axis.title.y = element_blank(),
          plot.title = element_text(size = 20))
  
  # Boxplot for Mean Anomaly
  bp_mean_anomaly <- ggplot(dataset, aes(x = "", y = mean_anomaly)) +
    geom_boxplot(outlier.colour = "red", width = 0.5) +
    labs(title = "Mean Anomaly") +
    theme_minimal() +
    theme(axis.title.x = element_blank(),
          axis.title.y = element_blank(),
          plot.title = element_text(size = 20))
  
  # Boxplot for Brouwer Mean Motion
  bp_brouwer_mean_motion <- ggplot(dataset, aes(x = "", y = brouwer_mean_motion)) +
    geom_boxplot(outlier.colour = "red", width = 0.5) +
    labs(title = "Brouwer Mean Motion") +
    theme_minimal() +
    theme(axis.title.x = element_blank(),
          axis.title.y = element_blank(),
          plot.title = element_text(size = 20))
  
  bp_right_ascension <- ggplot(dataset, aes(x = "", y = right_ascension)) +
    geom_boxplot(outlier.colour = "red", width = 0.5) +
    labs(title = "Right Ascension") +
    theme_minimal() +
    theme(axis.title.x = element_blank(),
          axis.title.y = element_blank(),
          plot.title = element_text(size = 20))
  
  plot_combined <-
    (bp_eccentricity | bp_inclination | bp_right_ascension) /
    (bp_argument_of_perigee | bp_mean_anomaly | bp_brouwer_mean_motion)
  
  plot_combined + 
    plot_annotation(
      title = title,
      tag_levels = "A",
      caption = line,
      theme = theme(
        plot.title = element_text(size = 24, face = "bold"),
        plot.caption = element_text(size = 12)
      )
    )
}

boxplot_jason_1_eda <- box_plots(jason_1, "Jason-1 Orbital Elements Boxplots")
boxplot_jason_2_eda <- box_plots(jason_2, "Jason-2 Orbital Elements Boxplots")
boxplot_jason_3_eda <- box_plots(jason_3, "Jason-3 Orbital Elements Boxplots")

# Saving plots

save_plot <- function(plot) {
  # Generate the filename automatically based on plot name
  filename <- paste0(deparse(substitute(plot)), ".png")
  
  # Save the plot
  ggsave(filename = filename,
         plot = plot,
         path = "D:/Master/Final Project/Figures",
         width = 30,
         height = 15,
         units = "cm",
         dpi = 300)
}

# Saving line plots
save_plot(line_jason_1_eda)
save_plot(line_jason_2_eda)
save_plot(line_jason_3_eda)

# Saving Hist plots
save_plot(hist_jason_1_eda)
save_plot(hist_jason_2_eda)
save_plot(hist_jason_3_eda)

# Saving Boxplots
save_plot(boxplot_jason_1_eda)
save_plot(boxplot_jason_2_eda)
save_plot(boxplot_jason_3_eda)


# EDA - Tables & Plots - Manoeuvres data -----------------------------------------------

# gap table
man_gap

# Create year and month bar plots
bar_ym_plots <- function(data, title) {
  data %>%
    mutate(month_year = format(datetime, "%Y-%m")) %>%
    count(month_year, name = "count") %>%
    ggplot(aes(x = month_year, y = count)) +
    geom_bar(stat = "identity", colour = "black", fill = "grey") +
    labs(title = title, x = NULL, y = NULL) +
    theme_minimal() +
    theme(
      axis.text.x = element_text(angle = 45, hjust = 1, size = 12),
      axis.text.y = element_text(size = 12),
      axis.title.x = element_text(size = 12, face = "bold"),
      plot.title = element_text(size = 24, face = "bold")) +
    scale_x_discrete(
      breaks = function(x) x[seq(1, length(x), by = 2)],  # Adjust 'by' to control frequency of labels
      labels = function(x) format(as.Date(paste0(x, "-01"), format = "%Y-%m-%d"), "%b %Y")
    )
}

jason_1_man_barplot_ym <- bar_ym_plots(jason_1_man, "Jason-1 Manoeuvres by Year & Month")
jason_2_man_barplot_ym <- bar_ym_plots(jason_2_man, "Jason-2 Manoeuvres by Year & Month")
jason_3_man_barplot_ym <- bar_ym_plots(jason_3_man, "Jason-3 Manoeuvres by Year & Month")

# Create year bar plots
bar_y_plots  <- function(data, title) {
  data %>%
    mutate(year = format(datetime, "%Y")) %>%  # Extract year
    count(year, name = "count") %>%  # Count occurrences per year
    ggplot(aes(x = year, y = count)) +
    geom_bar(stat = "identity", colour = "black", fill = "grey") +
    labs(title = title, x = NULL, y = NULL) +
    theme_minimal() +
    theme(
      axis.text.x = element_text(angle = 45, hjust = 1, size = 16, face = "bold"),
      axis.text.y = element_text(size = 12),
      axis.title.x = element_text(size = 12, face = "bold"),
      plot.title = element_text(size = 24, face = "bold")
    )
}

jason_1_man_barplot_y <- bar_y_plots(jason_1_man, "Jason-1 Manoeuvres by Year")
jason_2_man_barplot_y <- bar_y_plots(jason_2_man, "Jason-2 Manoeuvres by Year")
jason_3_man_barplot_y <- bar_y_plots(jason_3_man, "Jason-3 Manoeuvres by Year")

# Create month bar plots
bar_m_plots <- function(data, title) {
  data %>%
    # Extract month as number
    mutate(month = as.numeric(format(datetime, "%m"))) %>%
    count(month, name = "count") %>%
    # Ensure month order from 1 to 12
    mutate(month = factor(month, levels = 1:12, labels = month.abb)) %>%
    ggplot(aes(x = month, y = count)) +
    geom_bar(stat = "identity", colour = "black", fill = "grey") +
    labs(title = title, x = NULL, y = NULL) +
    theme_minimal() +
    theme(
      axis.text.x = element_text(angle = 45, hjust = 1, size = 16, face = "bold"),
      axis.text.y = element_text(size = 12),
      axis.title.x = element_text(size = 12, face = "bold"),
      plot.title = element_text(size = 24, face = "bold")
    )
}

jason_1_man_barplot_m <- bar_m_plots(jason_1_man, "Jason-1 Manoeuvres by Month")
jason_2_man_barplot_m <- bar_m_plots(jason_2_man, "Jason-2 Manoeuvres by Month")
jason_3_man_barplot_m <- bar_m_plots(jason_3_man, "Jason-3 Manoeuvres by Month")

man_ym_plots <- jason_1_man_barplot_ym / jason_2_man_barplot_ym / jason_3_man_barplot_ym

man_y_plots <- jason_1_man_barplot_y / jason_2_man_barplot_y / jason_3_man_barplot_y
man_m_plots <- jason_1_man_barplot_m / jason_2_man_barplot_m / jason_3_man_barplot_m

save_plot(man_ym_plots)
save_plot(man_m_plots)
save_plot(man_y_plots)


# EDA - Plots - Unpro VS Man - Dynamic  ----------------------------------------

dynamic_plot <- function(unpro_data, man_data, title) {
  
  # Ensure datetime is in Date format
  unpro_data$datetime <- as.Date(unpro_data$datetime)
  man_data$datetime <- as.Date(man_data$datetime)
  
  # Define min and max dates for sliderInput
  min_date <- min(unpro_data$datetime)
  max_date <- max(unpro_data$datetime)
  
  ui <- fluidPage(
    titlePanel(paste(title, "Unpro VS Man")),
    fluidRow(
      column(
        width = 12,
        checkboxGroupInput(
          "elements",
          label = "Elements",
          choices = list(
            "Eccentricity" = "eccentricity",
            "Argument of perigee" = "argument_of_perigee",
            "Inclination" = "inclination",
            "Mean anomaly" = "mean_anomaly",
            "Brouwer mean motion" = "brouwer_mean_motion",
            "Right ascension" = "right_ascension",
            "Manoeuvre" = "vline"
          )
        ),
        sliderInput(
          "shift_days",
          label = "Shift Days",
          min = -10,
          max = 10,
          value = 0
        ),
        sliderInput(
          "datetime",
          label = "Datetime",
          min = min_date,
          max = max_date,
          value = c(min_date, max_date),
          width = "80%"
        )
      )
    ),
    fluidRow(
      column(
        width = 12,
        h1(NULL),
        plotOutput("scatter")
      )
    )
  )
  
  server <- function(input, output) {
    
    # Reactive expression to shift manoeuvre data based on input$shift_days
    shifted_man_data <- reactive({
      req(man_data)  # Ensure man_data is available
      
      man_data %>%
        mutate(datetime = datetime + input$shift_days)
    })
    
    output$scatter <- renderPlot({
      # Filter unpropagated data based on selected datetime range
      filtered_unpro_data <- unpro_data %>%
        filter(datetime >= input$datetime[1] & datetime <= input$datetime[2])
      
      # Filter manoeuvres data to match selected datetime range for vlines
      filtered_man_data <- shifted_man_data() %>%
        filter(datetime >= input$datetime[1] & datetime <= input$datetime[2])
      
      # Initialize the plot
      p <- ggplot(filtered_unpro_data, aes(x = datetime))
      
      # Add lines for selected elements with different colors
      if ("eccentricity" %in% input$elements) {
        p <- p + geom_line(aes(y = eccentricity, color = "Eccentricity"), linewidth = 1, linetype = "solid")
      }
      if ("argument_of_perigee" %in% input$elements) {
        p <- p + geom_line(aes(y = argument_of_perigee, color = "Argument of Perigee"), linewidth = 1, linetype = "solid")
      }
      if ("inclination" %in% input$elements) {
        p <- p + geom_line(aes(y = inclination, color = "Inclination"), linewidth = 1, linetype = "solid")
      }
      if ("mean_anomaly" %in% input$elements) {
        p <- p + geom_line(aes(y = mean_anomaly, color = "Mean Anomaly"), linewidth = 1, linetype = "solid")
      }
      if ("brouwer_mean_motion" %in% input$elements) {
        p <- p + geom_line(aes(y = brouwer_mean_motion, color = "Brouwer Mean Motion"), linewidth = 1, linetype = "solid")
      }
      if ("right_ascension" %in% input$elements) {
        p <- p + geom_line(aes(y = right_ascension, color = "Right Ascension"), linewidth = 1, linetype = "solid")
      }
      
      # Add vertical lines for manoeuvres if selected
      if ("vline" %in% input$elements) {
        p <- p + geom_vline(data = filtered_man_data, aes(xintercept = as.numeric(datetime)), color = "black", linewidth = 0.25)
      }
      
      # Apply theme and plot labels
      p <- p + 
        theme_minimal() +
        labs(x = "Date", y = "Values", color = "Elements") +
        theme(axis.title.x = element_blank(),
              axis.title.y = element_blank(),
              axis.text.x = element_text(size = 14),
              legend.position = "bottom", 
              legend.box = "horizontal", 
              legend.title = element_blank(),
              legend.text = element_text(size = 14))
      
      # Return the plot
      print(p)
    })
  }
  
  shinyApp(ui, server)
}

dynamic_plot(jason_1, jason_1_man, title = "Jason-1")
dynamic_plot(jason_2, jason_2_man, title = "Jason-2")
dynamic_plot(jason_3, jason_3_man, title = "Jason-3")
dynamic_plot(saral, saral_man, title = "saral")
dynamic_plot(sentinel_3a, sentinel_3a_man, title = "sentinel-3a")
dynamic_plot(sentinel_3b, sentinel_3b_man, title = "sentinel-3b")
dynamic_plot(sentinel_6a, sentinel_6a_man, title = "sentinel-6a")


# EDA - Plots - Unpro VS Man - Interactive  ------------------------------------

line_plots_with_man <- function(dataset,
                                dataset_man, 
                                beg_date,
                                end_date,
                                title = "Dataset Plot",
                                footnote = "") {
  
  
  dataset <- dataset %>% 
    filter(datetime >= beg_date & datetime <= end_date)
  
  min_datetime <- min(dataset$datetime)
  max_datetime <- max(dataset$datetime)
  
  dataset_man <- dataset_man %>%
    filter(datetime >= min_datetime & datetime <= max_datetime)
  
  p_brouwer_mean_motion <- ggplot(dataset, aes(x = datetime, y = brouwer_mean_motion)) +
    geom_line() +
    geom_vline(data = dataset_man, 
               aes(xintercept = as.numeric(datetime)), 
               color = "red3", 
               size = 0.25) +
    labs(title = "Brouwer Mean Motion") +
    theme_minimal() +
    theme(axis.title.x = element_blank(),
          axis.title.y = element_blank(),
          axis.text.x = element_text(angle = 45, hjust = 1, size = 11),
          plot.title = element_text(size = 20)) +
    scale_x_datetime(date_breaks = "2 month", date_labels = "%y-%b") +
    scale_y_continuous(labels = label_number(accuracy = 0.0000001))
  
  p_eccentricity <- ggplot(dataset, aes(x = datetime, y = eccentricity)) +
    geom_line() +
    geom_vline(data = dataset_man, 
               aes(xintercept = as.numeric(datetime)), 
               color = "red3", 
               size = 0.25) +
    labs(title = "Eccentricity") +
    theme_minimal() +
    theme(axis.title.x = element_blank(),
          axis.title.y = element_blank(),
          axis.text.x = element_text(angle = 45, hjust = 1, size = 11),
          plot.title = element_text(size = 20)) +
    scale_x_datetime(date_breaks = "2 month", date_labels = "%y-%b") +
    scale_y_continuous(labels = label_number(accuracy = 0.00001))
  
  p_argument_of_perigee <- ggplot(dataset, aes(x = datetime, y = argument_of_perigee)) +
    geom_line() +
    geom_vline(data = dataset_man, 
               aes(xintercept = as.numeric(datetime)), 
               color = "red3", 
               size = 0.25) +
    labs(title = "Argument of Perigee") +
    theme_minimal() +
    theme(axis.title.x = element_blank(),
          axis.title.y = element_blank(),
          axis.text.x = element_text(angle = 45, hjust = 1, size = 11),
          plot.title = element_text(size = 20)) +
    scale_x_datetime(date_breaks = "2 month", date_labels = "%y-%b") +
    scale_y_continuous(labels = label_number(accuracy = 0.01))
  
  p_inclination <- ggplot(dataset, aes(x = datetime, y = inclination)) +
    geom_line() +
    geom_vline(data = dataset_man, 
               aes(xintercept = as.numeric(datetime)), 
               color = "red3", 
               size = 0.25) +
    labs(title = "Inclination") +
    theme_minimal() +
    theme(axis.title.x = element_blank(),
          axis.title.y = element_blank(),
          axis.text.x = element_text(angle = 45, hjust = 1, size = 11),
          plot.title = element_text(size = 20)) +
    scale_x_datetime(date_breaks = "2 month", date_labels = "%y-%b") +
    scale_y_continuous(labels = label_number(accuracy = 0.0001))
  
  p_mean_anomaly <- ggplot(dataset, aes(x = datetime, y = mean_anomaly)) +
    geom_line() +
    geom_vline(data = dataset_man, 
               aes(xintercept = as.numeric(datetime)), 
               color = "red3", 
               size = 0.25) +
    labs(title = "Mean Anomaly") +
    theme_minimal() +
    theme(axis.title.x = element_blank(),
          axis.title.y = element_blank(),
          axis.text.x = element_text(angle = 45, hjust = 1, size = 11),
          plot.title = element_text(size = 20)) +
    scale_x_datetime(date_breaks = "2 month", date_labels = "%y-%b") +
    scale_y_continuous(labels = label_number(accuracy = 0.1))
  
  p_right_ascension <- ggplot(dataset, aes(x = datetime, y = right_ascension)) +
    geom_line() +
    geom_vline(data = dataset_man, 
               aes(xintercept = as.numeric(datetime)), 
               color = "red3", 
               size = 0.25) +
    labs(title = "Right Ascension") +
    theme_minimal() +
    theme(axis.title.x = element_blank(),
          axis.title.y = element_blank(),
          axis.text.x = element_text(angle = 45, hjust = 1, size = 11),
          plot.title = element_text(size = 20)) +
    scale_x_datetime(date_breaks = "2 month", date_labels = "%y-%b") +
    scale_y_continuous(labels = label_number(accuracy = 0.1)) 
  

  plot_combined <- 
    (p_eccentricity | p_inclination | p_right_ascension) / 
    (p_argument_of_perigee | p_mean_anomaly | p_brouwer_mean_motion)
  
  
  plot_combined +
    plot_annotation(
      title = title,
      tag_levels = "A",
      caption = footnote,
      theme = theme(
        plot.title = element_text(size = 24, face = "bold"),
        plot.caption = element_text(size = 10)
      )
    )
}


Jason_1_unpro_Man <- line_plots_with_man(jason_1, jason_1_man, 
                    beg_date = "2002-12-22", end_date = "2004-12-22",
                    title = "Jason-1 Unpro VS Man")

Jason_2_unpro_Man <- line_plots_with_man(jason_2, jason_2_man, 
                    beg_date = "2009-07-04", end_date = "2011-07-04",
                    title = "Jason-2 Unpro VS Man")

Jason_3_unpro_Man <- line_plots_with_man(jason_3, jason_3_man, 
                    beg_date = "2017-01-31", end_date = "2019-01-31",
                    title = "Jason-3 Unpro VS Man")

save_plot(Jason_1_unpro_Man)
save_plot(Jason_2_unpro_Man)
save_plot(Jason_3_unpro_Man)
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~-----------------------------------------

###############################    Pre-processing    ###########################

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~-----------------------------------------

# Pre-processing - Extreme outliers --------------------------------------------

# Finding extreme Outliers

find_outliers <- function(dataset, extreme = 3) {
  
  # Internal function to find outliers in a vector
  find_outliers <- function(data) {
    data <- data[!is.na(data)]
    lowerq = quantile(data, probs = 0.25)
    upperq = quantile(data, probs = 0.75)
    iqr = upperq - lowerq # Or use IQR(data)
    # Identify extreme outliers
    extreme.threshold.upper = (iqr * extreme) + upperq
    extreme.threshold.lower = lowerq - (iqr * extreme)
    result <- which(data > extreme.threshold.upper | data < extreme.threshold.lower)
    
    return(result)
  }
  
  # List to store outlier indices
  outlier_indices <- integer(0)
  
  # Iterate over each column except the datetime
  for (col in names(dataset)[-1]) {
    outliers <- find_outliers(dataset[[col]])
    outlier_indices <- c(outlier_indices, outliers)
  }
  
  # Get unique outlier indices
  outlier_indices <- unique(outlier_indices)
  
  # Return the datetimes corresponding to outlier indices
  result <- tibble(dataset$datetime[outlier_indices]) %>% 
    rename(datetime = `dataset$datetime[outlier_indices]`)

  return(result)
}

remove_outliers <- function(dataset, extreme = 3) {
  outlier_datetimes <- find_outliers(dataset, extreme)
  cleaned_dataset <- dataset %>% filter(!datetime %in% outlier_datetimes$datetime)
  return(cleaned_dataset)
}

outliers_percentage <- function() {
  table_data <- data.frame(
    Dataset = c("jason_1", 
                "jason_2", 
                "jason_3"
                ),
    Original = c(nrow(jason_1),
                      nrow(jason_2),
                      nrow(jason_3)
                      ),
    Outliers = c(nrow(find_outliers(jason_1)),
              nrow(find_outliers(jason_2)),
              nrow(find_outliers(jason_3))
              ),
    Percentage = c(round(nrow(find_outliers(jason_1))/nrow(jason_1) * 100,2), 
                    round(nrow(find_outliers(jason_2))/nrow(jason_2) * 100,2), 
                    round(nrow(find_outliers(jason_3))/nrow(jason_3) * 100,2)
    )
  )
  return(table_data)
}

outliers_percentage()

# Removing extreme outliers

jason_1 <- remove_outliers(jason_1)
jason_2 <- remove_outliers(jason_2)
jason_3 <- remove_outliers(jason_3)

# Pre-processing - Initial unstable stage --------------------------------------

"
This section is not included in the prograss report, however, i leave the code
here in case that i might need this in the Final report.

"

row_reduction <- function(df, n) {
  return(df[(n + 1):nrow(df), ])
}

jason_3 <- row_reduction(jason_3,150)

line_jason_3_cut <- line_plots(jason_3, "Jason-3")



# Pre-processing - Data Normalization ------------------------------------------

data_norm_std <- function(df) {

  # Combine columns, scale variables, and convert to data frame
  temp <- as.data.frame(cbind(
    eccentricity = scale(df$eccentricity),
    argument_of_perigee = scale(df$argument_of_perigee),
    inclination = scale(df$inclination),
    mean_anomaly = scale(df$mean_anomaly),
    brouwer_mean_motion = scale(df$brouwer_mean_motion),
    right_ascension = scale(df$right_ascension)
  ))

  colnames(temp) <- c("eccentricity", "argument_of_perigee", "inclination", 
                      "mean_anomaly", "brouwer_mean_motion", "right_ascension")
  
  temp <- temp %>%
    rowwise() %>%
    mutate(std = sd(c_across(everything()))) %>%
    ungroup()
  
  data <- df %>% 
    select(datetime) %>%
    cbind(temp) %>%
    as_tibble()
  
  return(data)
}

jason_1 <- data_norm_std(jason_1)
jason_2 <- data_norm_std(jason_2)
jason_3 <- data_norm_std(jason_3)

# Pre-processing - Create lag & diff terms -------------------------------------

# function of create lag and diff terms
create_lag_diff_terms <- function(data, column, max_lag = 30) {
  result <- data %>%
    select(datetime, all_of(column))
  
  for (i in 1:max_lag) {
    lag_col <- paste0("lag_", i)
    diff_col <- paste0("diff_", i)
    result <- result %>%
      mutate(!!lag_col := lag(!!as.name(column), n = i)) %>%
      mutate(!!diff_col := !!as.name(column) - !!as.name(lag_col))
  }
  
  result <- result %>% na.omit()
  
  return(result)
}

jason_1_ecc <- create_lag_diff_terms(jason_1, "eccentricity")
jason_1_aop <- create_lag_diff_terms(jason_1, "argument_of_perigee")
jason_1_inc <- create_lag_diff_terms(jason_1, "inclination")
jason_1_ma <- create_lag_diff_terms(jason_1, "mean_anomaly")
jason_1_bmm <- create_lag_diff_terms(jason_1, "brouwer_mean_motion")
jason_1_ra <- create_lag_diff_terms(jason_1, "right_ascension")
jason_1_std <- create_lag_diff_terms(jason_1, "std")

jason_2_ecc <- create_lag_diff_terms(jason_2, "eccentricity")
jason_2_aop <- create_lag_diff_terms(jason_2, "argument_of_perigee")
jason_2_inc <- create_lag_diff_terms(jason_2, "inclination")
jason_2_ma <- create_lag_diff_terms(jason_2, "mean_anomaly")
jason_2_bmm <- create_lag_diff_terms(jason_2, "brouwer_mean_motion")
jason_2_ra <- create_lag_diff_terms(jason_2, "right_ascension")
jason_2_std <- create_lag_diff_terms(jason_2, "std")

jason_3_ecc <- create_lag_diff_terms(jason_3, "eccentricity")
jason_3_aop <- create_lag_diff_terms(jason_3, "argument_of_perigee")
jason_3_inc <- create_lag_diff_terms(jason_3, "inclination")
jason_3_ma <- create_lag_diff_terms(jason_3, "mean_anomaly")
jason_3_bmm <- create_lag_diff_terms(jason_3, "brouwer_mean_motion")
jason_3_ra <- create_lag_diff_terms(jason_3, "right_ascension")
jason_3_std <- create_lag_diff_terms(jason_3, "std")

# Pre-processing - Summary -----------------------------------------------------

# Data summary

data_summary <- function() {
  summary <- data.frame(
    Dataset = c("jason_1", 
                "jason_2", 
                "jason_3"
    ),
    Raw = c(nrow(jason_1_raw),
            nrow(jason_2_raw),
            nrow(jason_3_raw)
    ),
    Final = c(nrow(jason_1),
              nrow(jason_2),
              nrow(jason_3)
    )) %>%
    mutate(outliers = Raw - Final) %>%
    mutate(Percetage = round(outliers/Raw * 100, 1)) %>%
    tibble()
  
  return(summary)
  
}
  
data_summary()

# Hist plots
hist_plots_final <- function(dataset, title = "Dataset Plot", line = "") {
  
  p_right_ascension <- ggplot(dataset, aes(x = right_ascension)) +
    geom_histogram(bins = 30, colour = "black", fill = "grey", alpha = 0.7) +
    labs(title = "Right Ascension") +
    theme_minimal() +
    theme(axis.title.x = element_blank(),
          axis.title.y = element_blank(),
          axis.text.x = element_text(angle = 45, hjust = 1, size = 12),
          plot.title = element_text(size = 20))
  
  p_brouwer_mean_motion <- ggplot(dataset, aes(x = brouwer_mean_motion)) +
    geom_histogram(bins = 30, colour = "black", fill = "grey", alpha = 0.7) +
    labs(title = "Brouwer Mean Motion") +
    theme_minimal() +
    theme(axis.title.x = element_blank(),
          axis.title.y = element_blank(),
          axis.text.x = element_text(angle = 45, hjust = 1, size = 12),
          plot.title = element_text(size = 20))
  
  p_eccentricity <- ggplot(dataset, aes(x = eccentricity)) +
    geom_histogram(bins = 30, colour = "black", fill = "grey", alpha = 0.7) +
    labs(title = "Eccentricity") +
    theme_minimal() +
    theme(axis.title.x = element_blank(),
          axis.title.y = element_blank(),
          axis.text.x = element_text(angle = 45, hjust = 1, size = 12),
          plot.title = element_text(size = 20))
  
  p_argument_of_perigee <- ggplot(dataset, aes(x = argument_of_perigee)) +
    geom_histogram(bins = 30, colour = "black", fill = "grey", alpha = 0.7) +
    labs(title = "Argument of Perigee") +
    theme_minimal() +
    theme(axis.title.x = element_blank(),
          axis.title.y = element_blank(),
          axis.text.x = element_text(angle = 45, hjust = 1, size = 12),
          plot.title = element_text(size = 20))
  
  p_inclination <- ggplot(dataset, aes(x = inclination)) +
    geom_histogram(bins = 30, colour = "black", fill = "grey", alpha = 0.7) +
    labs(title = "Inclination") +
    theme_minimal() +
    theme(axis.title.x = element_blank(),
          axis.title.y = element_blank(),
          axis.text.x = element_text(angle = 45, hjust = 1, size = 12),
          plot.title = element_text(size = 20))
  
  p_mean_anomaly <- ggplot(dataset, aes(x = mean_anomaly)) +
    geom_histogram(bins = 30, colour = "black", fill = "grey", alpha = 0.7) +
    labs(title = "Mean Anomaly") +
    theme_minimal() +
    theme(axis.title.x = element_blank(),
          axis.title.y = element_blank(),
          axis.text.x = element_text(angle = 45, hjust = 1, size = 12),
          plot.title = element_text(size = 20))
  
  plot_combined <- 
    (p_eccentricity | p_inclination | p_right_ascension) / 
    (p_argument_of_perigee | p_mean_anomaly | p_brouwer_mean_motion)
  
  plot_combined + 
    plot_annotation(
      title = title,
      tag_levels = "A",
      caption = line,
      theme = theme(
        plot.title = element_text(size = 24, face = "bold"),
        plot.caption = element_text(size = 12)
      )
    )
}

hist_jason_1_final <- hist_plots_final(jason_1, title = "Jason-1 Histogram Plots - Cleaned & Normalized")
hist_jason_2_final <- hist_plots_final(jason_2, title = "Jason-2 Histogram Plots - Cleaned & Normalized")
hist_jason_3_final <- hist_plots_final(jason_3, title = "Jason-3 Histogram Plots - Cleaned & Normalized")

# Final plots
line_plots_final <- function(dataset, title = "Dataset Plot", line = "") {
  
  
  p_right_ascension <- ggplot(dataset, aes(x = datetime, y = right_ascension)) +
    geom_line() +
    labs(title = "Right Ascension") +
    theme_minimal() +
    theme(axis.title.x = element_blank(),
          axis.title.y = element_blank(),
          axis.text.x = element_text(angle = 45, hjust = 1, size = 12),
          plot.title = element_text(size = 20)) +
    scale_x_datetime(date_breaks = "1 year", date_labels = "%Y")
  
  
  p_brouwer_mean_motion <- ggplot(dataset, aes(x = datetime, y = brouwer_mean_motion)) +
    geom_line() +
    labs(title = "Brouwer Mean Motion") +
    theme_minimal() +
    theme(axis.title.x = element_blank(),
          axis.title.y = element_blank(),
          axis.text.x = element_text(angle = 45, hjust = 1, size = 12),
          plot.title = element_text(size = 20)) +
    scale_x_datetime(date_breaks = "1 year", date_labels = "%Y")
  
  p_eccentricity <- ggplot(dataset, aes(x = datetime, y = eccentricity)) +
    geom_line() +
    labs(title = "Eccentricity") +
    theme_minimal() +
    theme(axis.title.x = element_blank(),
          axis.title.y = element_blank(),
          axis.text.x = element_text(angle = 45, hjust = 1, size = 12),
          plot.title = element_text(size = 20)) +
    scale_x_datetime(date_breaks = "1 year", date_labels = "%Y")
  
  p_argument_of_perigee <- ggplot(dataset, aes(x = datetime, y = argument_of_perigee)) +
    geom_line() +
    labs(title = "Argument of Perigee") +
    theme_minimal() +
    theme(axis.title.x = element_blank(),
          axis.title.y = element_blank(),
          axis.text.x = element_text(angle = 45, hjust = 1, size = 12),
          plot.title = element_text(size = 20)) +
    scale_x_datetime(date_breaks = "1 year", date_labels = "%Y")
  
  p_inclination <- ggplot(dataset, aes(x = datetime, y = inclination)) +
    geom_line() +
    labs(title = "Inclination") +
    theme_minimal() +
    theme(axis.title.x = element_blank(),
          axis.title.y = element_blank(),
          axis.text.x = element_text(angle = 45, hjust = 1, size = 12),
          plot.title = element_text(size = 20)) +
    scale_x_datetime(date_breaks = "1 year", date_labels = "%Y")
  
  p_mean_anomaly <- ggplot(dataset, aes(x = datetime, y = mean_anomaly)) +
    geom_line() +
    labs(title = "Mean Anomaly") +
    theme_minimal() +
    theme(axis.title.x = element_blank(),
          axis.title.y = element_blank(),
          axis.text.x = element_text(angle = 45, hjust = 1, size = 12),
          plot.title = element_text(size = 20)) +
    scale_x_datetime(date_breaks = "1 year", date_labels = "%Y")
  
  plot_combined <- 
    (p_eccentricity | p_inclination | p_right_ascension) / 
    (p_argument_of_perigee | p_mean_anomaly | p_brouwer_mean_motion)
  
  plot_combined + 
    plot_annotation(
      title = title,
      tag_levels = "A",
      caption = line,
      theme = theme(
        plot.title = element_text(size = 24, face = "bold"),
        plot.caption = element_text(size = 12)
      )
    )
}

line_jason_1_final <- line_plots_final(jason_1, "Jason-1 Line Plots - Cleaned & Normalized")
line_jason_2_final <- line_plots_final(jason_2, "Jason-2 Line Plots - Cleaned & Normalized")
line_jason_3_final <- line_plots_final(jason_3, "Jason-3 Line Plots - Cleaned & Normalized")

# Save plots
save_plot(line_jason_1_final)
save_plot(line_jason_2_final)
save_plot(line_jason_3_final)

save_plot(hist_jason_1_final)
save_plot(hist_jason_2_final)
save_plot(hist_jason_3_final)

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~-----------------------------------------

####################################    ARIMA    ###############################

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~-----------------------------------------

# ARIMA - Data preparation -----------------------------------------------------

# ACF & PACF

# NO DIFF
par(mfrow = c(2, 3), oma = c(0, 0, 2, 0), cex.main = 1.5)
# Plot ACF for each variable
acf(jason_1_ecc$eccentricity, type = "correlation", main = "Eccentricity ACF", 
    xlim = c(1,20), ylim = c(-1,1))
acf(jason_1_aop$argument_of_perigee, type = "correlation", main = "Argument of Perigee ACF", 
    xlim = c(1,20), ylim = c(-1,1))
acf(jason_1_inc$inclination, type = "correlation", main = "Inclination ACF", 
    xlim = c(1,20), ylim = c(-1,1))
acf(jason_1_ma$mean_anomaly, type = "correlation", main = "Mean Anomaly ACF", 
    xlim = c(1,20), ylim = c(-1,1))
acf(jason_1_bmm$brouwer_mean_motion, type = "correlation", main = "Brouwer Mean Motion ACF", 
    xlim = c(1,20), ylim = c(-1,1))
acf(jason_1_ra$right_ascension, type = "correlation", main = "Right Ascension ACF", 
    xlim = c(1,20), ylim = c(-1,1))
# Add a main title at the top
mtext("ACF Plots for Jason-1 Orbital Elements (No Diff)", outer = TRUE, cex = 1.5)

#DIFF=1
par(mfrow = c(2, 3), oma = c(0, 0, 2, 0), cex.main = 1.5)
# Plot ACF for each variable
acf(jason_1_ecc$diff_1, type = "correlation", main = "Eccentricity ACF", 
    xlim = c(1,20), ylim = c(-0.2,0.2))
acf(jason_1_aop$diff_1, type = "correlation", main = "Argument of Perigee ACF", 
    xlim = c(1,20), ylim = c(-0.2,0.2))
acf(jason_1_inc$diff_1, type = "correlation", main = "Inclination ACF", 
    xlim = c(1,20), ylim = c(-0.2,0.2))
acf(jason_1_ma$diff_1, type = "correlation", main = "Mean Anomaly ACF", 
    xlim = c(1,20), ylim = c(-0.2,0.2))
acf(jason_1_bmm$diff_1, type = "correlation", main = "Brouwer Mean Motion ACF", 
    xlim = c(1,20), ylim = c(-0.2,0.2))
acf(jason_1_ra$diff_1, type = "correlation", main = "Right Ascension ACF", 
    xlim = c(1,20), ylim = c(-0.2,0.2))
# Add a main title at the top
mtext("ACF Plots for Jason-1 Orbital Elements (Diff = 1)", outer = TRUE, cex = 1.5)

#DIFF=2
par(mfrow = c(2, 3), oma = c(0, 0, 2, 0), cex.main = 1.5)
# Plot ACF for each variable
acf(jason_1_ecc$diff_2, type = "correlation", main = "Eccentricity ACF", 
    xlim = c(1,20), ylim = c(-0.2,0.2))
acf(jason_1_aop$diff_2, type = "correlation", main = "Argument of Perigee ACF", 
    xlim = c(1,20), ylim = c(-0.2,0.2))
acf(jason_1_inc$diff_2, type = "correlation", main = "Inclination ACF", 
    xlim = c(1,20), ylim = c(-0.2,0.2))
acf(jason_1_ma$diff_2, type = "correlation", main = "Mean Anomaly ACF", 
    xlim = c(1,20), ylim = c(-0.2,0.2))
acf(jason_1_bmm$diff_2, type = "correlation", main = "Brouwer Mean Motion ACF", 
    xlim = c(1,20), ylim = c(-0.2,0.2))
acf(jason_1_ra$diff_2, type = "correlation", main = "Right Ascension ACF", 
    xlim = c(1,20), ylim = c(-0.2,0.2))
# Add a main title at the top
mtext("ACF Plots for Jason-1 Orbital Elements (Diff = 2)", outer = TRUE, cex = 1.5)

par(mfrow = c(1, 1))


# Extract Diff-1 term

extract_columns <- function(df, column) {
  name_1 <- deparse(substitute(df))
  name_2 <- deparse(substitute(column))
  assign(paste0(name_1, "_", name_2), df[[name_2]], envir = .GlobalEnv)
}

extract_columns(jason_1_ecc, diff_1)
extract_columns(jason_1_aop, diff_1)
extract_columns(jason_1_inc, diff_1)
extract_columns(jason_1_ma, diff_1)
extract_columns(jason_1_bmm, diff_1)
extract_columns(jason_1_ra, diff_1)
extract_columns(jason_1_std, diff_1)

extract_columns(jason_2_ecc, diff_1)
extract_columns(jason_2_aop, diff_1)
extract_columns(jason_2_inc, diff_1)
extract_columns(jason_2_ma, diff_1)
extract_columns(jason_2_bmm, diff_1)
extract_columns(jason_2_ra, diff_1)
extract_columns(jason_2_std, diff_1)

extract_columns(jason_3_ecc, diff_1)
extract_columns(jason_3_aop, diff_1)
extract_columns(jason_3_inc, diff_1)
extract_columns(jason_3_ma, diff_1)
extract_columns(jason_3_bmm, diff_1)
extract_columns(jason_3_ra, diff_1)
extract_columns(jason_3_std, diff_1)

# ARIMA - Tuning matrix --------------------------------------------------------

fit_arima_and_summary <- function(x, xreg, seasonal = TRUE) {
  
  # Fit ARIMA model
  arima_model <- auto.arima(x, xreg = xreg,
                            seasonal = seasonal, 
                            approximation = FALSE)
  
  # Perform Ljung-Box test on residuals
  lb_p_value <- Box.test(residuals(arima_model), type = "Ljung-Box")$p.value
  aic_value <- AIC(arima_model)
  
  # Return results as a list
  result <- list(
    ARIMA_model = paste0("(", arima_model$arma[1],",",arima_model$arma[6]+1,",",arima_model$arma[2],")"),
    Ljung_Box_p_value = lb_p_value,
    AIC = aic_value
  )
  
  return(result)
}

fit_arima_combinations <- function(datasets) {
  # Initialize an empty results data frame
  results_df <- data.frame(
    Dataset = character(),
    Xreg_Dataset = character(),
    ARIMA_model = character(),
    p_value = numeric(),
    AIC = numeric(),
    stringsAsFactors = FALSE
  )
  
  # Iterate through each dataset as x
  for (x_name in datasets) {
    x <- get(x_name)  # Get x dataset
    other_datasets <- datasets[datasets != x_name]  # Get rest of datasets as xreg
    
    # Iterate through each xreg dataset
    for (xreg_name in other_datasets) {
      xreg <- get(xreg_name)  # Get xreg dataset
      
      # Fit ARIMA model and retrieve results
      result <- fit_arima_and_summary(x, xreg, seasonal = TRUE)
      
      # Append results to results_df
      results_df <- rbind(results_df, data.frame(
        X = x_name,
        Xreg = xreg_name,
        ARIMA = result$ARIMA_model,
        P_value = result$Ljung_Box_p_value,
        AIC = result$AIC,
        stringsAsFactors = FALSE
      ))
    }
  }
  
  results_df_sorted <- results_df[order(results_df$AIC), ] %>%
    tibble()
  
  return(results_df_sorted)
}

# Define dataset names

datasets_jason_1 <- c("jason_1_ecc_diff_1","jason_1_aop_diff_1","jason_1_inc_diff_1",
                      "jason_1_ma_diff_1","jason_1_bmm_diff_1","jason_1_ra_diff_1", 
                      "jason_1_std_diff_1")


datasets_jason_2 <- c("jason_2_ecc_diff_1","jason_2_aop_diff_1","jason_2_inc_diff_1",
                      "jason_2_ma_diff_1","jason_2_bmm_diff_1","jason_2_ra_diff_1", 
                      "jason_2_std_diff_1")


datasets_jason_3 <- c("jason_3_ecc_diff_1","jason_3_aop_diff_1","jason_3_inc_diff_1",
                      "jason_3_ma_diff_1","jason_3_bmm_diff_1","jason_3_ra_diff_1", 
                      "jason_3_std_diff_1")


# ARIMA - Tuning models --------------------------------------------------------

results_jason_1 <- fit_arima_combinations(datasets_jason_1)


results_jason_2 <- fit_arima_combinations(datasets_jason_2)


results_jason_3 <- fit_arima_combinations(datasets_jason_3)


# ARIMA - Tuning results  ------------------------------------------------------

print(results_jason_1 %>% arrange(X, AIC), n=42)

print(results_jason_2 %>% arrange(X, AIC), n=42)

print(results_jason_3 %>% arrange(X, AIC), n=42)

jason_1_arima_result <- results_jason_1 %>%
  group_by(X) %>%                 # Group by the X column
  filter(P_value >= 0.05) %>%
  filter(AIC == min(AIC)) %>%    # Keep the row with the lowest AIC in each group
  ungroup()

jason_2_arima_result <- results_jason_2 %>%
  group_by(X) %>%                 # Group by the X column
  filter(P_value >= 0.05) %>%
  filter(AIC == min(AIC)) %>%    # Keep the row with the lowest AIC in each group
  ungroup()

jason_3_arima_result <- results_jason_3 %>%
  group_by(X) %>%                 # Group by the X column
  filter(P_value >= 0.05) %>%
  filter(AIC == min(AIC)) %>%    # Keep the row with the lowest AIC in each group
  ungroup()


"
The key metric to focus on is the p-value. In the context of the Ljung-Box test:
Null Hypothesis (H_0): The residuals are independently distributed (no autocorrelation).
Alternative Hypothesis (H_a): The residuals are not independently distributed 
(presence of autocorrelation).

P-value Interpretation:
If the p-value is low (typically < 0.05), we reject the null hypothesis, 
suggesting that there is significant autocorrelation in the residuals, 
and the model may be inadequate.

If the p-value is high (≥ 0.05), we fail to reject the null hypothesis, 
suggesting that there is no significant autocorrelation in the residuals.

If the p-value from the Ljung-Box test is very low (typically less than 0.05), 
it suggests that there is significant autocorrelation in the residuals of your 
time series model. This indicates that the model has not fully captured the 
underlying patterns in the data, and there are still predictable structures 
remaining in the residuals.
"

# ARIMA - Jason-1 prediction residual plot -------------------------------------

# Define the parameters for each dataset
params_aop <- list(
  sat_x_name = "jason_1_aop_diff_1",
  sat_xreg_name = "jason_1_ma_diff_1",
  sat_data_name = "jason_1_aop",
  sat_man_name = "jason_1_man"
)
params_bmm <- list(
  sat_x_name = "jason_1_bmm_diff_1",
  sat_xreg_name = "jason_1_std_diff_1",
  sat_data_name = "jason_1_bmm",
  sat_man_name = "jason_1_man"
)
params_ecc <- list(
  sat_x_name = "jason_1_ecc_diff_1",
  sat_xreg_name = "jason_1_std_diff_1",
  sat_data_name = "jason_1_ecc",
  sat_man_name = "jason_1_man"
)
params_inc <- list(
  sat_x_name = "jason_1_inc_diff_1",
  sat_xreg_name = "jason_1_aop_diff_1",
  sat_data_name = "jason_1_inc",
  sat_man_name = "jason_1_man"
)
params_ma <- list(
  sat_x_name = "jason_1_ma_diff_1",
  sat_xreg_name = "jason_1_aop_diff_1",
  sat_data_name = "jason_1_ma",
  sat_man_name = "jason_1_man"
)
params_ra <- list(
  sat_x_name = "jason_1_ra_diff_1",
  sat_xreg_name = "jason_1_std_diff_1",
  sat_data_name = "jason_1_ra",
  sat_man_name = "jason_1_man"
)
params_std <- list(
  sat_x_name = "jason_1_std_diff_1",
  sat_xreg_name = "jason_1_bmm_diff_1",
  sat_data_name = "jason_1_std",
  sat_man_name = "jason_1_man"
)

# Arima Plot function
arima_pred_plot <- function(param, m, delay, title) {

  # set parameters
  set_parameters <- function(params) {
    sat_data <<- get(params$sat_data_name)
    sat_x <<- get(params$sat_x_name)
    sat_xreg <<- get(params$sat_xreg_name)
    sat_man <<- get(params$sat_man)
  }
  
  set_parameters(param)
  
  # Fit ARIMA model
  arima_model <- auto.arima(x = sat_x,
                            xreg = sat_xreg,
                            seasonal = TRUE,
                            approximation = FALSE)
  
  # Create pred table
  pred_data <- sat_data %>%
    select(datetime,
           actual = diff_1) %>%
    mutate(pred = as.numeric(fitted(arima_model))) %>%
    mutate(residual = abs(actual - pred))
  
  
  # Define the time range for the plot
  start_time <- min(pred_data$datetime)
  end_time <- start_time + months(m)
  
  # Filter the data based on the time range
  result_data_filtered <- pred_data %>%
    filter(datetime >= start_time & datetime <= end_time)
  
  # Assume `man_filtered` is from another dataset and contains the event dates
  man_filtered <- sat_man %>%
    filter(datetime >= start_time & datetime <= end_time) %>%
    mutate(datetime = datetime + days(delay))  # Adjust if needed
  
  # Plot using ggplot2
  plot <- ggplot(result_data_filtered, aes(x = datetime)) +
    # geom_line(aes(y = actual), color = "black", size = 0.5, linetype = "solid") +
    # geom_line(aes(y = pred), color = "blue", size = 0.5, linetype = "solid") +
    geom_line(aes(y = residual), color = "black", size = 0.6, linetype = "solid") +
    geom_vline(data = man_filtered,
               aes(xintercept = as.numeric(datetime)), size = 0.8,
               linetype = "dashed", color = "red3") +
    labs(x = "Datetime", y = "Residuls",
         title = title) +
    theme_minimal() +
    theme(axis.title.x = element_blank(),
          axis.title.y = element_text(size = 12),
          axis.text.x = element_text(size = 12),
          axis.text.y = element_text(size = 12),
          plot.title = element_text(size = 20, face = "bold")) +
    scale_x_datetime(date_breaks = "2 month", date_labels = "%y-%b")
    
  return(plot)

}

arima_line_jason_1_aop <- arima_pred_plot(params_aop, 24, 3, "Argument of Perigee")
arima_line_jason_1_bmm <- arima_pred_plot(params_bmm, 24, 3, "Brouwer Mean Motion")
arima_line_jason_1_ecc <- arima_pred_plot(params_ecc, 24, 3, "Eccentricity")
arima_line_jason_1_inc <- arima_pred_plot(params_inc, 24, 3, "Inclination")
arima_line_jason_1_ma <- arima_pred_plot(params_ma, 24, 3, "Mean Anomaly")
arima_line_jason_1_ra <- arima_pred_plot(params_ra, 24, 3, "Right Ascension")
arima_line_jason_1_std <- arima_pred_plot(params_std, 24, 3, "Standard Deviation")

arima_line_jason_1_combined <- 
  (arima_line_jason_1_aop / 
     arima_line_jason_1_bmm / 
     arima_line_jason_1_inc / 
     arima_line_jason_1_ecc /
     arima_line_jason_1_ma / 
     arima_line_jason_1_ra / 
     arima_line_jason_1_std) + 
  plot_annotation(
    title = "Jason-1 ARIMA Prediction Residuals",
    theme = theme(
      plot.title = element_text(size = 28, face = "bold")))

# ARIMA - Jason-2 prediction residual plot -------------------------------------

# Define the parameters for each dataset
params_aop <- list(
  sat_x_name = "jason_2_aop_diff_1",
  sat_xreg_name = "jason_2_ecc_diff_1",
  sat_data_name = "jason_2_aop",
  sat_man_name = "jason_2_man"
)
params_bmm <- list(
  sat_x_name = "jason_2_bmm_diff_1",
  sat_xreg_name = "jason_2_std_diff_1",
  sat_data_name = "jason_2_bmm",
  sat_man_name = "jason_2_man"
)
params_inc <- list(
  sat_x_name = "jason_2_inc_diff_1",
  sat_xreg_name = "jason_2_bmm_diff_1",
  sat_data_name = "jason_2_inc",
  sat_man_name = "jason_2_man"
)
params_ecc <- list(
  sat_x_name = "jason_2_ecc_diff_1",
  sat_xreg_name = "jason_2_aop_diff_1",
  sat_data_name = "jason_2_ecc",
  sat_man_name = "jason_2_man"
)
params_ma <- list(
  sat_x_name = "jason_2_ma_diff_1",
  sat_xreg_name = "jason_2_std_diff_1",
  sat_data_name = "jason_2_ma",
  sat_man_name = "jason_2_man"
)
params_ra <- list(
  sat_x_name = "jason_2_ra_diff_1",
  sat_xreg_name = "jason_2_std_diff_1",
  sat_data_name = "jason_2_ra",
  sat_man_name = "jason_2_man"
)
params_std <- list(
  sat_x_name = "jason_2_std_diff_1",
  sat_xreg_name = "jason_2_ma_diff_1",
  sat_data_name = "jason_2_std",
  sat_man_name = "jason_2_man"
)

# Arima Plot function
arima_pred_plot <- function(param, m, delay, title) {
  
  # set parameters
  set_parameters <- function(params) {
    sat_data <<- get(params$sat_data_name)
    sat_x <<- get(params$sat_x_name)
    sat_xreg <<- get(params$sat_xreg_name)
    sat_man <<- get(params$sat_man)
  }
  
  set_parameters(param)
  
  # Fit ARIMA model
  arima_model <- auto.arima(x = sat_x,
                            xreg = sat_xreg,
                            seasonal = TRUE,
                            approximation = FALSE)
  
  # Create pred table
  pred_data <- sat_data %>%
    select(datetime,
           actual = diff_1) %>%
    mutate(pred = as.numeric(fitted(arima_model))) %>%
    mutate(residual = abs(actual - pred))
  
  
  # Define the time range for the plot
  start_time <- min(pred_data$datetime)
  end_time <- start_time + months(m)
  
  # Filter the data based on the time range
  result_data_filtered <- pred_data %>%
    filter(datetime >= start_time & datetime <= end_time)
  
  # Assume `man_filtered` is from another dataset and contains the event dates
  man_filtered <- sat_man %>%
    filter(datetime >= start_time & datetime <= end_time) %>%
    mutate(datetime = datetime + days(delay))  # Adjust if needed
  
  # Plot using ggplot2
  plot <- ggplot(result_data_filtered, aes(x = datetime)) +
    # geom_line(aes(y = actual), color = "black", size = 0.5, linetype = "solid") +
    # geom_line(aes(y = pred), color = "blue", size = 0.5, linetype = "solid") +
    geom_line(aes(y = residual), color = "black", size = 0.6, linetype = "solid") +
    geom_vline(data = man_filtered,
               aes(xintercept = as.numeric(datetime)), size = 0.8,
               linetype = "dashed", color = "red3") +
    labs(x = "Datetime", y = "Residuls",
         title = title) +
    theme_minimal() +
    theme(axis.title.x = element_blank(),
          axis.title.y = element_text(size = 12),
          axis.text.x = element_text(size = 12),
          axis.text.y = element_text(size = 12),
          plot.title = element_text(size = 20, face = "bold")) +
    scale_x_datetime(date_breaks = "2 month", date_labels = "%y-%b")
  
  return(plot)
  
}

arima_line_jason_2_aop <- arima_pred_plot(params_aop, 24, 3, "Argument of Perigee")
arima_line_jason_2_bmm <- arima_pred_plot(params_bmm, 24, 3, "Brouwer Mean Motion")
arima_line_jason_2_inc <- arima_pred_plot(params_inc, 24, 3, "Inclination")
arima_line_jason_2_ecc <- arima_pred_plot(params_ecc, 24, 3, "Eccentricity")
arima_line_jason_2_ma <- arima_pred_plot(params_ma, 24, 3, "Mean Anomaly")
arima_line_jason_2_ra <- arima_pred_plot(params_ra, 24, 3, "Right Ascension")
arima_line_jason_2_std <- arima_pred_plot(params_std, 24, 3, "Standard Deviation")

arima_line_jason_2_combined <- 
  (arima_line_jason_2_aop / 
     arima_line_jason_2_bmm / 
     arima_line_jason_2_inc / 
     arima_line_jason_2_ecc /
     arima_line_jason_2_ma / 
     arima_line_jason_2_ra / 
     arima_line_jason_2_std) + 
  plot_annotation(
    title = "Jason-2 ARIMA Prediction Residuals",
    theme = theme(
      plot.title = element_text(size = 28, face = "bold")))

# ARIMA - Jason-3 prediction residual plot -------------------------------------

# Define the parameters for each dataset
params_aop <- list(
  sat_x_name = "jason_3_aop_diff_1",
  sat_xreg_name = "jason_3_ma_diff_1",
  sat_data_name = "jason_3_aop",
  sat_man_name = "jason_3_man"
)
params_bmm <- list(
  sat_x_name = "jason_3_bmm_diff_1",
  sat_xreg_name = "jason_3_inc_diff_1",
  sat_data_name = "jason_3_bmm",
  sat_man_name = "jason_3_man"
)
params_inc <- list(
  sat_x_name = "jason_3_inc_diff_1",
  sat_xreg_name = "jason_3_ecc_diff_1",
  sat_data_name = "jason_3_inc",
  sat_man_name = "jason_3_man"
)
params_ecc <- list(
  sat_x_name = "jason_3_ecc_diff_1",
  sat_xreg_name = "jason_3_inc_diff_1",
  sat_data_name = "jason_3_ecc",
  sat_man_name = "jason_3_man"
)
params_ma <- list(
  sat_x_name = "jason_3_ma_diff_1",
  sat_xreg_name = "jason_3_aop_diff_1",
  sat_data_name = "jason_3_ma",
  sat_man_name = "jason_3_man"
)
params_ra <- list(
  sat_x_name = "jason_3_ra_diff_1",
  sat_xreg_name = "jason_3_std_diff_1",
  sat_data_name = "jason_3_ra",
  sat_man_name = "jason_3_man"
)
params_std <- list(
  sat_x_name = "jason_3_std_diff_1",
  sat_xreg_name = "jason_3_ra_diff_1",
  sat_data_name = "jason_3_std",
  sat_man_name = "jason_3_man"
)

# ARIMA residual plot function
arima_pred_plot <- function(param, m, delay, title) {
  
  # set parameters
  set_parameters <- function(params) {
    sat_data <<- get(params$sat_data_name)
    sat_x <<- get(params$sat_x_name)
    sat_xreg <<- get(params$sat_xreg_name)
    sat_man <<- get(params$sat_man)
  }
  
  set_parameters(param)
  
  # Fit ARIMA model
  arima_model <- auto.arima(x = sat_x,
                            xreg = sat_xreg,
                            seasonal = TRUE,
                            approximation = FALSE)
  
  # Create pred table
  pred_data <- sat_data %>%
    select(datetime,
           actual = diff_1) %>%
    mutate(pred = as.numeric(fitted(arima_model))) %>%
    mutate(residual = abs(actual - pred))
  
  
  # Define the time range for the plot
  start_time <- min(pred_data$datetime)
  end_time <- start_time + months(m)
  
  # Filter the data based on the time range
  result_data_filtered <- pred_data %>%
    filter(datetime >= start_time & datetime <= end_time)
  
  # Assume `man_filtered` is from another dataset and contains the event dates
  man_filtered <- sat_man %>%
    filter(datetime >= start_time & datetime <= end_time) %>%
    mutate(datetime = datetime + days(delay))  # Adjust if needed
  
  # Plot using ggplot2
  plot <- ggplot(result_data_filtered, aes(x = datetime)) +
    # geom_line(aes(y = actual), color = "black", size = 0.5, linetype = "solid") +
    # geom_line(aes(y = pred), color = "blue", size = 0.5, linetype = "solid") +
    geom_line(aes(y = residual), color = "black", size = 0.6, linetype = "solid") +
    geom_vline(data = man_filtered,
               aes(xintercept = as.numeric(datetime)), size = 0.8,
               linetype = "dashed", color = "red3") +
    labs(x = "Datetime", y = "Residuls",
         title = title) +
    theme_minimal() +
    theme(axis.title.x = element_blank(),
          axis.title.y = element_text(size = 12),
          axis.text.x = element_text(size = 12),
          axis.text.y = element_text(size = 12),
          plot.title = element_text(size = 20, face = "bold")) +
    scale_x_datetime(date_breaks = "2 month", date_labels = "%y-%b")
  
  return(plot)
  
}

arima_line_jason_3_aop <- arima_pred_plot(params_aop, 24, 3, "Argument of Perigee")
arima_line_jason_3_bmm <- arima_pred_plot(params_bmm, 24, 3, "Brouwer Mean Motion")
arima_line_jason_3_inc <- arima_pred_plot(params_inc, 24, 3, "Inclination")
arima_line_jason_3_ecc <- arima_pred_plot(params_ecc, 24, 3, "Eccentricity")
arima_line_jason_3_ma <- arima_pred_plot(params_ma, 24, 3, "Mean Anomaly")
arima_line_jason_3_ra <- arima_pred_plot(params_ra, 24, 3, "Right Ascension")
arima_line_jason_3_std <- arima_pred_plot(params_std, 24, 3, "Standard Deviation")

arima_resid_jason_3_combined <- 
  (arima_line_jason_3_aop / 
     arima_line_jason_3_bmm / 
     arima_line_jason_3_inc / 
     arima_line_jason_3_ecc /
     arima_line_jason_3_ma / 
     arima_line_jason_3_ra / 
     arima_line_jason_3_std) + 
  plot_annotation(
    title = "Jason-3 ARIMA Prediction Residuals",
    theme = theme(
      plot.title = element_text(size = 28, face = "bold")))

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~-----------------------------------------

###################################    XGboost    ##############################

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~-----------------------------------------

# XGBoost Modelling ------------------------------------------------------------

# Function
xgb_modeling <- function(xgb_target, xgb_actual) {
  
  # XGboost - Split dataset
  xgb_data <- xgb_target %>% 
    rename(actual = xgb_actual)
  xgb_split <- initial_time_split(xgb_data, prop = 0.8)
  xgb_train <- training(xgb_split)
  xgb_test <- testing(xgb_split)
  
  # XGboost - Cv_folds
  cv_folds <- vfold_cv(xgb_train, v = 2)
  
  # XGboost - Recipe
  xgb_recipe <- 
    recipe(actual ~ ., data = xgb_train) %>%
    update_role(datetime, new_role = "id") %>% # Exclude datetime from predictors
    update_role(starts_with("lag_"), starts_with("diff_"), new_role = "predictor") %>%
    step_normalize(all_predictors()) %>%
    step_pca()
  
  # XGBoost - Model specification
  xgb_spec <- boost_tree(
    trees = 1000,
    tree_depth = tune(),
    min_n = tune(),
    loss_reduction = tune(),
    sample_size = 1,
    mtry = 0.8,
    learn_rate = tune(),
    stop_iter = 5
  ) %>% 
    set_engine("xgboost",
               counts=FALSE) %>% 
    set_mode("regression")
  
  # XGBoost - Tuning grid 
  xgb_grid <- grid_latin_hypercube(
    tree_depth(),
    min_n(),
    loss_reduction(),
    learn_rate(range = c(0.005, 0.05)),
    size = 30
  )
  
  # XGBoost - Workflow 
  xgb_wf <- workflow() %>%
    add_recipe(xgb_recipe) %>%
    add_model(xgb_spec)
  
  # XGBoost - Tuning
  doParallel::registerDoParallel()
  xgb_tune <- tune_grid(
    xgb_wf,
    resamples = cv_folds,
    grid = xgb_grid,
    control = control_grid(save_pred = TRUE))
  
  # XGBoost - Tuning result metrics
  best_results <- show_best(xgb_tune, metric = "rmse")
  print(best_results)
  
  # XGBoost - Finalizing
  xgb_final <- xgb_wf %>%
    finalize_workflow(select_best(xgb_tune, metric = "rmse")) %>%
    fit(xgb_data)
  
  return(xgb_final)
  
}

# Jason-1 Application
xgb_model_jason_1_aop <- xgb_modeling(jason_1_aop, "argument_of_perigee")
xgb_model_jason_1_bmm <- xgb_modeling(jason_1_bmm, "brouwer_mean_motion")
xgb_model_jason_1_ecc <- xgb_modeling(jason_1_ecc, "eccentricity")
xgb_model_jason_1_inc <- xgb_modeling(jason_1_inc, "inclination")
xgb_model_jason_1_ma <- xgb_modeling(jason_1_ma, "mean_anomaly")
xgb_model_jason_1_ra <- xgb_modeling(jason_1_ra, "right_ascension")
xgb_model_jason_1_std <- xgb_modeling(jason_1_std, "std")

# Jason-2 Application
xgb_model_jason_2_aop <- xgb_modeling(jason_2_aop, "argument_of_perigee")
xgb_model_jason_2_bmm <- xgb_modeling(jason_2_bmm, "brouwer_mean_motion")
xgb_model_jason_2_ecc <- xgb_modeling(jason_2_ecc, "eccentricity")
xgb_model_jason_2_inc <- xgb_modeling(jason_2_inc, "inclination")
xgb_model_jason_2_ma <- xgb_modeling(jason_2_ma, "mean_anomaly")
xgb_model_jason_2_ra <- xgb_modeling(jason_2_ra, "right_ascension")
xgb_model_jason_2_std <- xgb_modeling(jason_2_std, "std")

# Jason-3 Application
xgb_model_jason_3_aop <- xgb_modeling(jason_3_aop, "argument_of_perigee")
xgb_model_jason_3_bmm <- xgb_modeling(jason_3_bmm, "brouwer_mean_motion")
xgb_model_jason_3_ecc <- xgb_modeling(jason_3_ecc, "eccentricity")
xgb_model_jason_3_inc <- xgb_modeling(jason_3_inc, "inclination")
xgb_model_jason_3_ma <- xgb_modeling(jason_3_ma, "mean_anomaly")
xgb_model_jason_3_ra <- xgb_modeling(jason_3_ra, "right_ascension")
xgb_model_jason_3_std <- xgb_modeling(jason_3_std, "std")


# XGBoost - VIP Analysis--------------------------------------------------------

xgb_model_jason_1_aop %>% extract_fit_parsnip() %>% vip()
xgb_model_jason_1_aop %>% extract_fit_parsnip() %>% vi() %>% head()

# XGBoost - Jason-1 prediction residual plot -----------------------------------

# Function
xgb_pred_plot <- function(xgb_target, xgb_actual, xgb_man, xgb_final,m,delay, title) {
  
  xgb_data <- xgb_target %>% 
    rename(actual = xgb_actual)
  xgb_split <- initial_time_split(xgb_data, prop = 0.8)
  xgb_train <- training(xgb_split)
  xgb_test <- testing(xgb_split)
  
  
  # Combine predictions and actual values into a data frame
  pred_data <- data.frame(
    datetime = xgb_data$datetime,
    actual = xgb_data$actual,
    pred = predict(xgb_final, new_data = xgb_data)) %>% 
    rename(pred = .pred) %>% 
    mutate(residual = abs(actual - pred)) %>%
    tibble()
  
  # Define the time range for the plot
  start_time <- min(pred_data$datetime)
  end_time <- start_time + months(m)
  
  # Filter the data based on the time range
  result_data_filtered <- pred_data %>%
    filter(datetime >= start_time & datetime <= end_time)
  
  # Assume `man_filtered` is from another dataset and contains the event dates
  man_filtered <- xgb_man %>%
    filter(datetime >= start_time & datetime <= end_time) %>%
    mutate(datetime = datetime + days(delay))  # Adjust if needed
  
  # Plot using ggplot2
  plot <- ggplot(result_data_filtered, aes(x = datetime)) +
    # geom_line(aes(y = actual), color = "black", size = 0.5, linetype = "solid") +
    # geom_line(aes(y = pred), color = "blue", size = 0.5, linetype = "solid") +
    geom_line(aes(y = residual), color = "black", size = 0.6, linetype = "solid") +
    geom_vline(data = man_filtered,
               aes(xintercept = as.numeric(datetime)), size = 0.8,
               linetype = "dashed", color = "red3") +
    labs(x = "Datetime", y = "Residuls",
         title = title) +
    theme_minimal() +
    theme(axis.title.x = element_blank(),
          axis.title.y = element_text(size = 12),
          axis.text.x = element_text(size = 12),
          axis.text.y = element_text(size = 12),
          plot.title = element_text(size = 20, face = "bold")) +
    scale_x_datetime(date_breaks = "2 month", date_labels = "%y-%b")
  
  
  return(plot)
  
}

# Jason-1 Application
xbg_line_jason_1_aop <- xgb_pred_plot(jason_1_aop, "argument_of_perigee",jason_1_man,xgb_model_jason_1_aop, 24,3,"Argument of Perigee")
xbg_line_jason_1_bmm <- xgb_pred_plot(jason_1_bmm, "brouwer_mean_motion",jason_1_man,xgb_model_jason_1_bmm, 24,3,"Brouwer Mean Motion")
xbg_line_jason_1_ecc <- xgb_pred_plot(jason_1_ecc, "eccentricity",jason_1_man, xgb_model_jason_1_ecc, 24,3,"Eccentricity")
xbg_line_jason_1_inc <- xgb_pred_plot(jason_1_inc, "inclination",jason_1_man,xgb_model_jason_1_inc, 24,3,"Inclination")
xbg_line_jason_1_ma <- xgb_pred_plot(jason_1_ma, "mean_anomaly",jason_1_man,xgb_model_jason_1_ma, 24,3,"Mean Anomaly")
xbg_line_jason_1_ra <- xgb_pred_plot(jason_1_ra, "right_ascension",jason_1_man,xgb_model_jason_1_ra, 24,3,"Right Ascension")
xbg_line_jason_1_std <- xgb_pred_plot(jason_1_std, "std",jason_1_man, xgb_model_jason_1_std, 24,3,"Standard Deviation")

# Combined plot
xbg_line_jason_1_plot_combined <- 
  (xbg_line_jason_1_aop / 
     xbg_line_jason_1_bmm / 
     xbg_line_jason_1_inc / 
     xbg_line_jason_1_ecc /
     xbg_line_jason_1_ma / 
     xbg_line_jason_1_ra / 
     xbg_line_jason_1_std) + 
  plot_annotation(
    title = "Jason-1 XGBoost Prediction Residuals",
    theme = theme(
      plot.title = element_text(size = 28, face = "bold")))

# XGBoost - Jason-2 prediction residual plot -----------------------------------

# Jason-2 Application
xbg_line_jason_2_aop <- xgb_pred_plot(jason_2_aop, "argument_of_perigee",jason_2_man,xgb_model_jason_2_aop, 24,3,"Argument of Perigee")
xbg_line_jason_2_bmm <- xgb_pred_plot(jason_2_bmm, "brouwer_mean_motion",jason_2_man,xgb_model_jason_2_bmm, 24,3,"Brouwer Mean Motion")
xbg_line_jason_2_ecc <- xgb_pred_plot(jason_2_ecc, "eccentricity",jason_2_man, xgb_model_jason_2_ecc, 24,3,"Eccentricity")
xbg_line_jason_2_inc <- xgb_pred_plot(jason_2_inc, "inclination",jason_2_man,xgb_model_jason_2_inc, 24,3,"Inclination")
xbg_line_jason_2_ma <- xgb_pred_plot(jason_2_ma, "mean_anomaly",jason_2_man,xgb_model_jason_2_ma, 24,3,"Mean Anomaly")
xbg_line_jason_2_ra <- xgb_pred_plot(jason_2_ra, "right_ascension",jason_2_man,xgb_model_jason_2_ra, 24,3,"Right Ascension")
xbg_line_jason_2_std <- xgb_pred_plot(jason_2_std, "std",jason_2_man, xgb_model_jason_2_std, 24,3,"Standard Deviation")

# Combined plot
xbg_line_jason_2_plot_combined <- 
  (xbg_line_jason_2_aop / 
     xbg_line_jason_2_bmm / 
     xbg_line_jason_2_inc / 
     xbg_line_jason_2_ecc /
     xbg_line_jason_2_ma / 
     xbg_line_jason_2_ra / 
     xbg_line_jason_2_std) + 
  plot_annotation(
    title = "Jason-2 XGBoost Prediction Residuals",
    theme = theme(
      plot.title = element_text(size = 28, face = "bold")))


# XGBoost - Jason-3 prediction residual plot -----------------------------------

# Jason-3 Application
xbg_line_jason_3_aop <- xgb_pred_plot(jason_3_aop, "argument_of_perigee",jason_3_man,xgb_model_jason_3_aop, 24,3,"Argument of Perigee")
xbg_line_jason_3_bmm <- xgb_pred_plot(jason_3_bmm, "brouwer_mean_motion",jason_3_man,xgb_model_jason_3_bmm, 24,3,"Brouwer Mean Motion")
xbg_line_jason_3_ecc <- xgb_pred_plot(jason_3_ecc, "eccentricity",jason_3_man, xgb_model_jason_3_ecc, 24,3,"Eccentricity")
xbg_line_jason_3_inc <- xgb_pred_plot(jason_3_inc, "inclination",jason_3_man,xgb_model_jason_3_inc, 24,3,"Inclination")
xbg_line_jason_3_ma <- xgb_pred_plot(jason_3_ma, "mean_anomaly",jason_3_man,xgb_model_jason_3_ma, 24,3,"Mean Anomaly")
xbg_line_jason_3_ra <- xgb_pred_plot(jason_3_ra, "right_ascension",jason_3_man,xgb_model_jason_3_ra, 24,3,"Right Ascension")
xbg_line_jason_3_std <- xgb_pred_plot(jason_3_std, "std",jason_3_man, xgb_model_jason_3_std, 24,3,"Standard Deviation")

# Combined plot
xbg_line_jason_3_plot_combined <- 
  (xbg_line_jason_3_aop / 
     xbg_line_jason_3_bmm / 
     xbg_line_jason_3_inc / 
     xbg_line_jason_3_ecc /
     xbg_line_jason_3_ma / 
     xbg_line_jason_3_ra / 
     xbg_line_jason_3_std) + 
  plot_annotation(
    title = "Jason-3 XGBoost Prediction Residuals",
    theme = theme(
      plot.title = element_text(size = 28, face = "bold")))



# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~-----------------------------------------

############################    Precision-Recall curve   #######################

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~-----------------------------------------

"
This is for the next half of the project
"






# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~-----------------------------------------

# Backup code ------------------------------------------------------------------

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~-----------------------------------------





jason_1_ecc <- create_lag_diff_terms(jason_1, "eccentricity", 3)
jason_1_aop <- create_lag_diff_terms(jason_1, "argument_of_perigee", 3)
jason_1_inc <- create_lag_diff_terms(jason_1, "inclination", 3)
jason_1_ma <- create_lag_diff_terms(jason_1, "mean_anomaly", 3)
jason_1_bmm <- create_lag_diff_terms(jason_1, "brouwer_mean_motion", 3)
jason_1_ra <- create_lag_diff_terms(jason_1, "right_ascension", 3)
jason_1_std <- create_lag_diff_terms(jason_1, "std", 3)

jason_2_ecc <- create_lag_diff_terms(jason_2, "eccentricity", 3)
jason_2_aop <- create_lag_diff_terms(jason_2, "argument_of_perigee", 3)
jason_2_inc <- create_lag_diff_terms(jason_2, "inclination", 3)
jason_2_ma <- create_lag_diff_terms(jason_2, "mean_anomaly", 3)
jason_2_bmm <- create_lag_diff_terms(jason_2, "brouwer_mean_motion", 3)
jason_2_ra <- create_lag_diff_terms(jason_2, "right_ascension", 3)
jason_2_std <- create_lag_diff_terms(jason_2, "std", 3)

jason_3_ecc <- create_lag_diff_terms(jason_3, "eccentricity", 3)
jason_3_aop <- create_lag_diff_terms(jason_3, "argument_of_perigee", 3)
jason_3_inc <- create_lag_diff_terms(jason_3, "inclination", 3)
jason_3_ma <- create_lag_diff_terms(jason_3, "mean_anomaly", 3)
jason_3_bmm <- create_lag_diff_terms(jason_3, "brouwer_mean_motion", 3)
jason_3_ra <- create_lag_diff_terms(jason_3, "right_ascension", 3)
jason_3_std <- create_lag_diff_terms(jason_3, "std", 3)

saral_ecc <- create_lag_diff_terms(saral, "eccentricity", 3)
saral_aop <- create_lag_diff_terms(saral, "argument_of_perigee", 3)
saral_inc <- create_lag_diff_terms(saral, "inclination", 3)
saral_ma <- create_lag_diff_terms(saral, "mean_anomaly", 3)
saral_bmm <- create_lag_diff_terms(saral, "brouwer_mean_motion", 3)
saral_ra <- create_lag_diff_terms(saral, "right_ascension", 3)
saral_std <- create_lag_diff_terms(saral, "std", 3)

sentinel_3a_ecc <- create_lag_diff_terms(sentinel_3a, "eccentricity", 3)
sentinel_3a_aop <- create_lag_diff_terms(sentinel_3a, "argument_of_perigee", 3)
sentinel_3a_inc <- create_lag_diff_terms(sentinel_3a, "inclination", 3)
sentinel_3a_ma <- create_lag_diff_terms(sentinel_3a, "mean_anomaly", 3)
sentinel_3a_bmm <- create_lag_diff_terms(sentinel_3a, "brouwer_mean_motion", 3)
sentinel_3a_ra <- create_lag_diff_terms(sentinel_3a, "right_ascension", 3)
sentinel_3a_std <- create_lag_diff_terms(sentinel_3a, "std", 3)

sentinel_3b_ecc <- create_lag_diff_terms(sentinel_3b, "eccentricity", 3)
sentinel_3b_aop <- create_lag_diff_terms(sentinel_3b, "argument_of_perigee", 3)
sentinel_3b_inc <- create_lag_diff_terms(sentinel_3b, "inclination", 3)
sentinel_3b_ma <- create_lag_diff_terms(sentinel_3b, "mean_anomaly", 3)
sentinel_3b_bmm <- create_lag_diff_terms(sentinel_3b, "brouwer_mean_motion", 3)
sentinel_3b_ra <- create_lag_diff_terms(sentinel_3b, "right_ascension", 3)
sentinel_3b_std <- create_lag_diff_terms(sentinel_3b, "std", 3)

sentinel_6a_ecc <- create_lag_diff_terms(sentinel_6a, "eccentricity", 3)
sentinel_6a_aop <- create_lag_diff_terms(sentinel_6a, "argument_of_perigee", 3)
sentinel_6a_inc <- create_lag_diff_terms(sentinel_6a, "inclination", 3)
sentinel_6a_ma <- create_lag_diff_terms(sentinel_6a, "mean_anomaly", 3)
sentinel_6a_bmm <- create_lag_diff_terms(sentinel_6a, "brouwer_mean_motion", 3)
sentinel_6a_ra <- create_lag_diff_terms(sentinel_6a, "right_ascension", 3)
sentinel_6a_std <- create_lag_diff_terms(sentinel_6a, "std", 3)




extract_columns <- function(df, prefix) {
  assign(paste0(prefix, "_ecc"), df$eccentricity, envir = .GlobalEnv)
  assign(paste0(prefix, "_aop"), df$argument_of_perigee, envir = .GlobalEnv)
  assign(paste0(prefix, "_inc"), df$inclination, envir = .GlobalEnv)
  assign(paste0(prefix, "_ma"), df$mean_anomaly, envir = .GlobalEnv)
  assign(paste0(prefix, "_bmm"), df$brouwer_mean_motion, envir = .GlobalEnv)
  assign(paste0(prefix, "_ra"), df$right_ascension, envir = .GlobalEnv)
}

extract_columns(jason_1, "jason_1")
extract_columns(jason_2, "jason_2")
extract_columns(jason_3, "jason_3")


# Splitting

split_data <- function(data, file_name = "data", train = 0.8, 
                       test = 0.10, valid = 0.10) {
  
  names(data)[2] <- "actual"
  
  # Calculate the number of rows
  n <- nrow(data)
  
  # Calculate the split points
  train_split <- floor(train * n)
  valid_split <- floor((train + valid) * n)
  
  # Split the data
  train_data <- data[1:train_split, ] 
  valid_data <- data[(train_split + 1):valid_split, ]
  test_data <- data[(valid_split + 1):n, ]
  
  # Assign the split data to global variables
  assign(paste0(file_name, "_train"), train_data, envir = .GlobalEnv)
  assign(paste0(file_name, "_valid"), valid_data, envir = .GlobalEnv)
  assign(paste0(file_name, "_test"), test_data, envir = .GlobalEnv)
  
}


# split_data(jason_1_ecc)
# split_data(jason_1_aop)
# split_data(jason_1_inc)
# split_data(jason_1_ma)
split_data(jason_1_bmm)
# split_data(jason_1_ra)

# Check the split datasets

skim(data_train)
skim(data_valid)
skim(data_test)

# Convert data to xgb.DMatrix format

c_list <- c("lag_1","lag_2","lag_3",
            "diff_1","diff_2","diff_3")


xgb_train <- xgb.DMatrix(
  data = as.matrix(data_train[, c_list]),
  label = data_train$actual)

xgb_valid <- xgb.DMatrix(
  data = as.matrix(data_valid[, c_list]),
  label = data_valid$actual)

xgb_test <- xgb.DMatrix(
  data = as.matrix(data_test[, c_list]),
  label = data_test$actual)


# Define parameter grid for tuning
xgb_params <- list(
  objective = "reg:squarederror",  # Specify regression as the objective function
  max_depth = 100,        # Maximum depth of trees
  colsample_bytree = 0.8,  # Fraction of features to randomly sample for each tree
  eta = 0.01,             # Learning rate: controls the step size during training
  gamma = 0,             # Minimum loss reduction required to make a further partition on a leaf node
  min_child_weight = 5,  # Minimum sum of instance weight needed in a child
  subsample = 0.8,       # Fraction of training data to randomly sample during each boosting iteration
  lambda = 1,
  alpha = 1
)

# Train the XGBoost model
xgb_model <- xgb.train(params = xgb_params,
                       data = xgb_train,
                       nrounds = 3000,
                       watchlist = list(train = xgb_train, valid = xgb_valid),  # Fixed: use dvalid for validation
                       print_every_n = 50, # print evaluation metric every 50 rounds
                       early_stopping_rounds = 10, # stop if no improvement in 20 rounds
                       maximize = FALSE) # minimize the evaluation metric (default)



xgb_model$best_msg


# Extract evaluation log
eval_log <- xgb_model$evaluation_log

# Convert evaluation log to a data frame for ggplot
eval_log_df <- as.data.frame(eval_log)

# Plot the training and validation error over the number of rounds
ggplot(eval_log_df, aes(x = iter)) +
  geom_line(aes(y = train_rmse, color = "Training RMSE")) +
  geom_line(aes(y = valid_rmse, color = "Validation RMSE")) +
  labs(title = "Training and Validation RMSE over Iterations",
       x = "Iteration",
       y = "RMSE") +
  scale_color_manual("", 
                     breaks = c("Training RMSE", "Validation RMSE"),
                     values = c("blue", "red")) +
  theme_minimal()

# Predict on test data

pred_data <- data.frame(
  datetime = data_test$datetime,
  actual = data_test$actual,
  prediction = predict(xgb_model, xgb_test)) %>%
  mutate(residual = actual - prediction) %>%
  tibble()


# Define the time range for the plot
start_time <- min(pred_data$datetime)
end_time <- start_time + months(36)

# Filter the data based on the time range
result_data_filtered <- pred_data %>%
  filter(datetime >= start_time & datetime <= end_time)
man_filtered <- jason_1_man %>%
  filter(datetime >= start_time & datetime <= end_time) %>%
  mutate(datetime = datetime + days(0))

# Plot using ggplot2
ggplot(result_data_filtered, aes(x = datetime)) +
  # geom_line(aes(y = actual), color = "black", size = 0.5, linetype = "solid") +
  geom_line(aes(y = prediction), color = "blue", size = 0.5, linetype = "solid") +
  # geom_line(aes(y = residual), color = "black", size = 0.5, linetype = "solid") +
  geom_vline(data = man_filtered,
             aes(xintercept = as.numeric(datetime)), size = 0.1,
             linetype = "solid", color = "red3") +
  labs(x = "Datetime", y = "Residuls",
       title = "Prediction vs Man") +
  theme_minimal()







# XGboost - Data preparation

xgb_target <- jason_1_aop
xgb_actual <- "argument_of_perigee"
xgb_man <- jason_1_man

# XGboost - Split dataset 

xgb_data <- xgb_target %>% 
  rename(actual = xgb_actual)
xgb_split <- initial_time_split(xgb_data, prop = 0.8)
xgb_train <- training(xgb_split)
xgb_test <- testing(xgb_split)

# XGboost - Cv_folds 

cv_folds <- vfold_cv(xgb_train, v = 2)

# XGboost - Recipe 
xgb_recipe <- 
  recipe(actual ~ ., data = xgb_train) %>%
  update_role(datetime, new_role = "id") %>% # Exclude datetime from predictors
  update_role(starts_with("lag_"), starts_with("diff_"), new_role = "predictor") %>%
  step_normalize(all_predictors()) %>%
  step_pca()

# xgb_recipe %>% prep() %>% bake(new_data = NULL)

# XGBoost - Model specification
xgb_spec <- boost_tree(
  trees = 1000,
  tree_depth = tune(),
  min_n = tune(),
  loss_reduction = tune(),
  sample_size = 1,
  mtry = 0.8,
  learn_rate = tune(),
  stop_iter = 5
) %>% 
  set_engine("xgboost",
             counts=FALSE) %>% 
  set_mode("regression")


# XGBoost - Tuning grid 
xgb_grid <- grid_latin_hypercube(
  tree_depth(),
  min_n(),
  loss_reduction(),
  learn_rate(range = c(0.005, 0.05)),
  size = 30
)

# XGBoost - Workflow 
xgb_wf <- workflow() %>%
  add_recipe(xgb_recipe) %>%
  add_model(xgb_spec)

# XGBoost - Tuning 
doParallel::registerDoParallel()
xgb_tune <- tune_grid(
  xgb_wf,
  resamples = cv_folds,
  grid = xgb_grid,
  control = control_grid(save_pred = TRUE)
)
# XGBoost - Tuning result metrics

# xgb_tune %>% collect_metrics()
# xgb_tune %>% autoplot()
show_best(xgb_tune, metric = "rmse")

# XGBoost - Finalizing 

xgb_final <- xgb_wf %>%
  finalize_workflow(select_best(xgb_tune, metric = "rmse")) %>%
  fit(xgb_data)





# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~-----------------------------------------



















