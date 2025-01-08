
library(shiny)
library(dplyr)
library(lubridate)
library(ggplot2)
library(tidyr)
library(plotly)
library(ggridges)
library(bslib)
library(fmsb)
library(patchwork)
library(ggalt)


SebastianRaw <- read.csv2("../data/sleepdataSebastian.csv")
PiotrRaw <- read.csv2("../data/sleepdataPiotr.csv")
OlekRaw <- read.csv("../data/sleepdataOlek.csv")


parse_percentage <- function(str) {
  as.numeric(substr(str, 1, nchar(str)-1))/100
}

process_raw <- function(dt) {
  dt|>
    mutate(
      Sleep.Quality = parse_percentage(Sleep.Quality),
      Regularity = parse_percentage(Regularity),
      Did.snore = Did.snore == "true",
      Went.to.bed = ymd_hms(Went.to.bed),
      Woke.up = ymd_hms(Woke.up),
      day = as_date(Woke.up),
      Coughing..per.hour. = as.numeric(Coughing..per.hour.),
      Movements.per.hour = as.numeric(Movements.per.hour),
      General.day.asleep = ifelse(hour(ymd_hms(Went.to.bed)) < 15, day(day)-1,day(day)),
      General.month.asleep = ifelse(hour(ymd_hms(Went.to.bed))< 15 & day(day) == 1, 12, month(day)),
      General.day.asleep = ifelse(General.day.asleep == 0, 31, General.day.asleep)
    ) |>
    filter(day %within% (ymd("2024-12-01") %--% now())) |>
    select(-c(
      City,
      Alertness.score,
      Alertness.reaction.time..seconds.,
      Alertness.accuracy
    ))
}

generate_pom <- function(df){
  df %>%
    mutate(Went.to.bed = update(ymd_hms(Went.to.bed, tz = "UTC"), day = General.day.asleep),
           Went.to.bed = update(Went.to.bed, month = General.month.asleep),
           Went.to.bed = if_else(month(Went.to.bed) == 12,
                                 update(Went.to.bed, year = 2024), 
                                 update(Went.to.bed, year = 2025))) %>%
    mutate(DayOfWeek = wday(ymd_hms(Went.to.bed, tz = "UTC"),
                            label = TRUE, week_start = 1),
           WeekNumber = week(ymd_hms(Went.to.bed, tz = "UTC"))) %>%
    mutate(WeekNumber = if_else(WeekNumber == 1, 54, WeekNumber)) %>%
    arrange(WeekNumber)
}

plot <- function(df){
  ggplot(df, aes(x = DayOfWeek,
                 y = WeekNumber,
                 fill = Sleep.Quality)) +
    geom_tile(color = "white", lwd = 1.5) +
    scale_fill_gradientn(colors = hcl.colors(50, "RdYlGn"), limits = c(0.25, 1)) +
    theme_minimal() +
    theme(
      panel.grid = element_blank()
    ) + labs(x = "Dzień tygodnia", y = "Numer tygodnia w roku") +
    scale_y_continuous(labels = function(x) ifelse(x >= 54, x - 53, x))
}


# Sebastian <- SebastianRaw |>
#   mutate(
#     Sleep.Quality = parse_percentage(Sleep.Quality),
#     Regularity = parse_percentage(Regularity),
#     Did.snore = Did.snore == "true",
#     Went.to.bed = as.POSIXct(Went.to.bed),
#     Woke.up = as.POSIXct(Woke.up),
#     day = as.Date(Woke.up)
#   ) |>
#   select(-c(
#     City,
#     Alertness.score,
#     Alertness.reaction.time..seconds.,
#     Alertness.accuracy
#   ))

Sebastian <- process_raw(SebastianRaw) |> mutate(sleeper = "Sebastian")
Piotr <- process_raw(PiotrRaw) |> mutate(sleeper = "Piotr")
Olek <- process_raw(OlekRaw) |> mutate(sleeper = "Olek")

Data <- bind_rows(Sebastian, Piotr, Olek)

# Define UI for application that draws a histogram
ui1 <- fluidPage(
  titlePanel("Ogólne dane"),
  mainPanel(
    plotOutput("radar_plot"),
    plotOutput("sleep_hour_dist_ridgelines"),
    plotOutput("density_plot")
  )
)



ui2 <- fluidPage(
  
  # Application title
  titlePanel("Indywidualne dane"),
  
  # Sidebar with a slider input for number of bins 
  sidebarLayout(
    sidebarPanel(
      selectInput("selectSleeper",
                  "Select a sleeper(person)",
                  unique(Data$sleeper),
                  selected = "Sebastian")
    ),
    
    # Show a plot of the generated distribution
    mainPanel(
      plotOutput("sleeptimeCrossbar", height = "500px"),
      plotlyOutput("sleepDistractionScatter"),
      plotOutput("acitivityBoxplot"),
      plotOutput("heatmap")
    )
    
  )
)

ui3 <- fluidPage(
  titlePanel("Kumulatywna ilość snu"),
  sidebarLayout(
    sidebarPanel(
      sliderInput("day", "Wybierz dzień:", 
                  min = min(Data$day), 
                  max = max(Data$day), 
                  value = min(Data$day), 
                  step = 1, 
                  animate = animationOptions(interval = 500, loop = FALSE))
    ),
    mainPanel(
      plotOutput("bar_plot")
    )
  )
)


# Define server logic required to draw a histogram
server <- function(input, output) {
  
  ########## ui1 - ogólne informacje ##########
  
  output$radar_plot <- renderPlot({
    filtered_data <- Data %>% 
      select(Sleep.Quality, Asleep.after..seconds., Regularity, Snore.time..seconds.,
             Coughing..per.hour.,Movements.per.hour,sleeper)
    
    min_vals <- apply(filtered_data[, -7], 2, min)
    max_vals <- apply(filtered_data[, -7], 2, max)
    data_norm <- as.data.frame(scale(filtered_data[, -7], center = min_vals, scale = max_vals - min_vals))
    data_norm$sleeper <- filtered_data$sleeper
    
    data_olek <- colMeans(data_norm[data_norm$sleeper == "Olek", -ncol(data_norm)])
    data_seba <- colMeans(data_norm[data_norm$sleeper == "Sebastian", -ncol(data_norm)])
    data_piotr <- colMeans(data_norm[data_norm$sleeper == "Piotr", -ncol(data_norm)])
    
    data_radar <- as.data.frame(rbind(
      rep(1, ncol(data_norm) - 1),  
      rep(0, ncol(data_norm) - 1),  
      data_olek,  
      data_seba,
      data_piotr
    ))
    
    radarchart(data_radar,
               axistype = 1, 
               pcol = c("blue", "red", "green"),  
               pfcol = c("#0000FF50", "#FF000050", "lightgreen"),  
               plwd = 2,  
               cglcol = "grey",  
               cglty = 1, 
               axislabcol = "black",  
               vlcex = 0.8  
    )
    legend("topright", legend = c("Olek", "Seba", "Piotr"), col = c("blue", "red", "green"), lty = 1, lwd = 2)
    
    
  })
  
  
  output$density_plot <- renderPlot({
    mean_df <- Data %>% 
      group_by(sleeper) %>% 
      summarise(mean_SQ = mean(Sleep.Quality))
    
    density_plot <- ggplot(Data, aes(x = Sleep.Quality, fill = sleeper)) + 
      geom_density(alpha = 0.3) +
      geom_vline(data = mean_df, aes(xintercept = mean_SQ, color = sleeper),
                 linetype = "dashed") + 
      theme_minimal()
    density_plot
  })
  
  
  
  
  
  
  ########## ui2 - indywidualne informacje ##########
  
  output$sleeptimeCrossbar <- renderPlot({
    (
      Data |>
        filter(sleeper == input$selectSleeper) |>
        mutate(woke_up_inter = interval(day, Woke.up),
               went_to_bed_inter = interval(day, Went.to.bed),
               fell_asleep_time = Went.to.bed + dseconds(Asleep.after..seconds.)) |>
        mutate(fell_asleep_inter = interval(day, fell_asleep_time))|>
        # View()
        ggplot(aes(x = day, y = fell_asleep_inter)) +
        geom_crossbar(aes(ymin = went_to_bed_inter, ymax = fell_asleep_inter), fill = "#887711", colour = NA) +
        geom_crossbar(aes(ymax = woke_up_inter, ymin = fell_asleep_inter), fill = "#223388", colour = NA) +
        scale_y_time(labels = (\(x) format(make_datetime(sec = x), "%H:%M")),
                     breaks = (\(x) {
                       y <- make_datetime(sec = floor(x[1]):1:(x[2]+1));
                       y <- y[second(y)==0 & minute(y)==0]})) +
        labs(
          y = "time of day"
        ) +
        theme(
          axis.title.x = element_blank(),
          axis.text.x = element_blank(),
          axis.ticks.x = element_blank()
        )
    ) + (
      Data |>
        filter(sleeper == input$selectSleeper) |>
        ggplot(aes(x = day, y = Sleep.Quality)) +
        geom_xspline() +
        ylim(min(Data$Sleep.Quality), NA) +
        scale_y_continuous(
          labels = (\(x) paste(100*x, "%"))
        ) +
        labs(
          y = "sleep quality"
        ) +
        scale_x_date(date_breaks = "3 days",
                     date_labels = "%b %e")
    ) + plot_layout(
      guides = "collect",
      nrow = 2,
      ncol = 1,
      heights = c(0.7, 0.3)
    )
  })
  output$sleepDistractionScatter <- renderPlotly({
    dane <- Data %>% filter(sleeper == input$selectSleeper)
    plot_ly(dane, x = ~Movements.per.hour, y = ~Sleep.Quality,
            text = ~paste("Kaszlnięcia na godzinę: ", Coughing..per.hour.,
                          "<br> Czas chrapania: ", Snore.time..seconds.),
            hoverinfo = "text",
            type = "scatter",
            mode = "markers")
  })
  output$activityBoxplot <- renderPlot({
    Piotr <- Piotr %>% 
      mutate(activity = c(FALSE, FALSE, TRUE, FALSE,
                          FALSE, FALSE, FALSE, FALSE, FALSE, TRUE, FALSE,
                          FALSE, TRUE, FALSE, TRUE, FALSE, FALSE, TRUE,
                          FALSE, TRUE, FALSE))
    Olek <- Olek %>% 
      mutate(activity = c(TRUE, FALSE, FALSE, FALSE,
                          FALSE, FALSE, FALSE, TRUE, TRUE, FALSE, TRUE,
                          FALSE, FALSE, FALSE, FALSE, TRUE, FALSE, FALSE,
                          TRUE))
    
    ggplot(Piotr, aes(x = activity, y = Sleep.Quality)) +
      geom_boxplot() + 
      theme_minimal()
  })
  
  
  output$heatmap <- renderPlot({
    tmp <- Data %>% 
      filter(sleeper == input$selectSleeper)
    pom1 <- generate_pom(tmp)
    p <- plot(pom1)
    p
  })
  
  output$sleep_hour_dist_ridgelines <- renderPlot({
    minutes <- Data |>
      mutate(minute_went_to_bed = unclass(interval(day, Went.to.bed)) %/% 60) |>
      mutate(minute_woke_up = unclass(interval(day, Woke.up)) %/% 60)
    
    samples <- min(minutes$minute_went_to_bed):max(minutes$minute_woke_up)
    
    minutes |>
      cross_join(tibble(val = samples)) |>
      filter(minute_went_to_bed <= val & val <= minute_woke_up) |>
      ggplot(aes(x = val, y = sleeper, fill = factor(sleeper))) +
      stat_density_ridges(alpha = 0.6) +
      scale_x_continuous(labels = (\(x) format(make_datetime(min = x), "%H:%M"))) +
      labs(
        x = "time of day",
        y = NULL,
        fill = "sleeper"
      ) +
      theme_ridges() +
      theme(
        axis.text.y = element_blank()
      )
  })
  
  
  ########## ui3 - aniamacja ##########
  
  
  output$bar_plot <- renderPlot({
    
    filtered_data <- Data %>% 
      filter(day <= input$day) %>%
      group_by(sleeper) %>%
      mutate(CumulativeSleepHours = as.numeric(sum(Time.asleep..seconds.)/ 3600)) %>% 
      summarise(CumulativeSleepHours = max(CumulativeSleepHours, na.rm = TRUE)) %>% 
      arrange(desc(CumulativeSleepHours))
    
    #Set colors for persons
    custom_colors <- c("Sebastian" = "blue", "Piotr" = "red", "Olek" = "green")
    
    
    ggplot(filtered_data, aes(x = reorder(sleeper, CumulativeSleepHours), y = CumulativeSleepHours, fill = sleeper)) +
      geom_col(alpha = 0.8) +
      scale_fill_manual(values = custom_colors) +
      labs(
        title = paste("Kumulatywna ilość snu do dnia", input$day),
        x = "Osoba",
        y = "Czas snu (godziny)"
      ) +
      theme_minimal() +
      theme(legend.position = "none") +
      coord_flip()
    
  })
  
  
  
}


app_ui <- navbarPage(
  title = "Analiza danych",
  tabPanel("Ogólne dane", ui1),
  tabPanel("Indywidualne dane", ui2),
  tabPanel("Animacja", ui3),
  theme = bslib::bs_theme(bootswatch = "darkly", 
                          primary = "#4CAF50",  # Zielony jako kolor główny
                          secondary = "#434343", # Żółty jako kolor akcentu
                          bg = "#222222",       # Tło
                          fg = "#FFFFFF",
                          success = "#00BC8C"),
  header = tags$head(),
  footer = shiny::HTML("
                <footer class='text-center text-sm-start' style='width:100%;'>
                <hr>
                <p class='text-center' style='font-size:12px;'>
                  © 2021 Copyright:
                  <a class='text-dark' href='https://www.mi2.ai/'>MI2</a>
                </p>
                </footer>
                ")
  
)

# Run the application 
shinyApp(app_ui, server)
