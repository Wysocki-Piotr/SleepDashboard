#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    https://shiny.posit.co/
#

library(shiny)
library(dplyr)
library(lubridate)
library(ggplot2)
library(tidyr)
library(plotly)
library(ggridges)
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

Sebastian <- process_raw(SebastianRaw) |> mutate(sleeper = 1)
Piotr <- process_raw(PiotrRaw) |> mutate(sleeper = 2)
Olek <- process_raw(OlekRaw) |> mutate(sleeper = 3)

Data <- bind_rows(Sebastian, Piotr, Olek)

# Define UI for application that draws a histogram
ui <- fluidPage(
  
  # Application title
  titlePanel("Old Faithful Geyser Data"),
  
  # Sidebar with a slider input for number of bins 
  sidebarLayout(
    sidebarPanel(
      selectInput("selectSleeper",
                  "Select a sleeper(person)",
                  1:3)
    ),
    
    # Show a plot of the generated distribution
    mainPanel(
      plotOutput("sleeptimeCrossbar", height = "500px"),
      plotOutput("sleep_hour_dist_ridgelines"),
      plotlyOutput("sleepDistractionScatter"),
      plotOutput("acitivityBoxplot"),
      plotOutput("heatmap1"),
      plotOutput("heatmap2"),
      plotOutput("heatmap3")
    )
    
  )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
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
  output$heatmap1 <- renderPlot({
    pom1 <- generate_pom(Sebastian)
    p <- plot(pom1)
    p
  })
  output$heatmap2 <- renderPlot({
    pom2 <- generate_pom(Piotr)
    p <- plot(pom2)
    p
  })
  output$heatmap3 <- renderPlot({
    pom3 <- generate_pom(Olek)
    p <- plot(pom3)
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
}

# Run the application 
shinyApp(ui = ui, server = server)


