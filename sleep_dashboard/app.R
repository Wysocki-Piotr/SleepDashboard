# ------------------------------------------------------------------------------
# Biblioteki
# ------------------------------------------------------------------------------

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
library(jpeg)
library(shinydashboard)
library(thematic)
library(shinycssloaders)

# ------------------------------------------------------------------------------
# Wgranie danych
# ------------------------------------------------------------------------------

SebastianRaw <- read.csv2("../data/sleepdataSebastian.csv")
PiotrRaw <-  read.csv2("../data/sleepdataPiotr.csv")
OlekRaw <- read.csv("../data/sleepdataOlek.csv")

# ------------------------------------------------------------------------------
# Przetworzenie danych
# ------------------------------------------------------------------------------

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

Sebastian <- process_raw(SebastianRaw) |> mutate(sleeper = "Sebastian")
Piotr <- process_raw(PiotrRaw) |> mutate(sleeper = "Piotr")
Olek <- process_raw(OlekRaw) |> mutate(sleeper = "Olek")

Data <- bind_rows(Sebastian, Piotr, Olek)

# ------------------------------------------------------------------------------
# Dodatkowe funkcje
# ------------------------------------------------------------------------------

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
    labs(x = "Dzień tygodnia", y = "Numer tygodnia w roku") +
    scale_y_continuous(labels = function(x) ifelse(x >= 54, x - 53, x)) +
    theme(
      panel.grid = element_blank(),
      text = theme_get()$text
    ) 
}

# ------------------------------------------------------------------------------
# Wartości do boxów
# ------------------------------------------------------------------------------

Sleep.Time.Mean <- round(sum(Data$Time.asleep..seconds./3600) / nrow(Data),2)
Sleep.Time.Min <- round(min(Data$Time.asleep..seconds.)/3600, 2)
Sleep.Time.Max <- round(max(Data$Time.asleep..seconds.)/3600, 2)

# ------------------------------------------------------------------------------
# UI
# ------------------------------------------------------------------------------

thematic_on()
theme_set(
  theme(
    text = element_text(colour = "white")
  )
)

main_page <- fluidPage(
  tags$head(
    tags$style(HTML("
      .custom-box {
        display: flex;
        flex-direction: column;
        justify-content: center;
        align-items: center;
        font-size: 1.5em;
        font-weight: bold;
        color: white;
        text-align: center;
        border-radius: 5px;
        height: 150px; /* Wysokość prostokąta */
        margin: 5px;  /* Małe odstępy między boxami */
      }
      .navy { background-color: #001f3f; }
      .green { background-color: #2ECC40; }
      .red { background-color: #FF4136; }
    "))
  ),
  
  fluidRow(
    column(4,
           div(class = "custom-box navy", 
               icon("moon"),
               div(
                 style = "font-size: 2em; margin-bottom: 5px;",
                 paste(" ", as.character(Sleep.Time.Mean))
               ),
               div("Average Sleep Time in hours")
           )
    ),
    column(4,
           div(class = "custom-box red", 
               icon("clock"),
               div(
                 style = "font-size: 2em; margin-bottom: 5px;",
                 paste(" ", as.character(Sleep.Time.Min))
               ),
               div("Shortest sleep in hours")
           )
    ),
    column(4,
           div(class = "custom-box green", 
               icon("mattress-pillow"),
               div(
                 style = "font-size: 2em; margin-bottom: 5px;",
                 paste(" ", as.character(Sleep.Time.Max))
               ),
               div("Longest sleep in hours")
           )
    )
  ),
  fluidRow(
    column(12,
           h1("Main Description"),
           p("This is the main description section where you can provide additional details about the content displayed above.")
    )
  ),
  
  fluidRow(
    column(4,
           uiOutput("img1"),
           h3("Opis 1"),
           p("To jest opis pierwszego obrazu.")
    ),
    column(4,
           uiOutput("img2"),
           h3("Opis 2"),
           p("To jest opis drugiego obrazu.")
    ),
    column(4,
           uiOutput("img3"),
           h3("Opis 3"),
           p("To jest opis trzeciego obrazu.")
    )
  )
)

weekday_opts_names <- c(
  "All",
  "Monday",
  "Tuesday",
  "Wednesday",
  "Thursday",
  "Friday",
  "Saturday",
  "Sunday"
)

weekday_opts <- 0:7
names(weekday_opts) <- weekday_opts_names

general_data_page <- fluidPage( # pytanie gdzie dac opisy i do jakich wykresow
  # bo do radar plot chyba bez sensu?
  titlePanel("Who has the best sleep?"),
  fluidRow(
    column(6,
           dateRangeInput("dateSelector", "Select a Date:", 
                          format = "yyyy-mm-dd",
                          start = "2024-12-11",
                          end = "2024-12-31",
                          min = "2024-12-11",
                          max = "2024-12-31") # poprawic na koncu
    )
  ),
  mainPanel(
    width = 12,
    fluidRow(
      column(6,
             h3("Description"),
             p("opis sekcji np. w swieta lepiej, godziny wstawania etc")
      ),
      column(6,
             h5("tytuł"),
             plotOutput("radar_plot", height = "600px") |> 
               withSpinner(type = 7, size = 2, color = "#F39C12")
      )
    ),
    fluidRow(
      column(6,
             h5("How often are we asleep at a certain time"),
             plotOutput("sleep_hour_dist_ridgelines") |>
               withSpinner(type = 7, size = 2, color = "#F39C12"),
             selectInput(
               inputId = "weekday_selector",
               label = "Weekdays: ",
               choices = weekday_opts) 
      ),
      column(6,
             h5("tytuł"),
             plotOutput("density_plot") %>% withSpinner(type = 7, size = 2, color = "#F39C12")
      )
    )
  )
)


individual_data_page <- fluidPage(
  tags$style(HTML("
    .sidebar-layout .sidebar {
      border-right: 3px solid white;  /* Grubość 3px, kolor biały */
    }
  ")),
  titlePanel("Let's dive deeper into each of our sleep!"),
  fluidRow("Opis całej strony"),
  
  fluidRow(
    column(12,
           selectInput("selectSleeper", 
                       "Select a sleeper (person):",
                       unique(Data$sleeper),
                       selected = "Sebastian")
    )),
  sidebarLayout(
    sidebarPanel(
      h2("What happens during our sleep?"),
      h5("Tytuł wykresu"),
      plotlyOutput("sleepDistractionScatter", height = "400px") %>% 
        withSpinner(type = 7, size = 2, color = "#F39C12"),
      h2("Is physical activity related to sleep quality?"),
      h5("Tytuł wykresu"),
      plotOutput("activityBoxplot", height = "400px") %>% withSpinner(type = 7, size = 2, color = "#F39C12"),
    ),
    mainPanel(
      h2("How was our sleep in specific days?"),
      h5("Sleeptime intervals and quality on a daily basis"),
      plotOutput("sleeptimeCrossbar", height = "400px") %>% 
        withSpinner(type = 7, size = 2, color = "#F39C12"),
      h2("Is there a most stressful day of the week for each of us?"),
      h5("Tytuł wykresu"),
      plotOutput("heatmap", height = "500px") %>% withSpinner(type = 7, size = 2,
                                                              color = "#F39C12"),
      p("Ewentualne miejsce na komentarz")
    )
  )
)


animation_page <- fluidPage(
  # mozna tez wrzucic jakis krotki tekst do tej wizualizacji
  titlePanel("Cumulative sleep time"),
  sidebarLayout(
    sidebarPanel(
      sliderInput("day", "Select a day:", 
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

# ------------------------------------------------------------------------------
# serwer
# ------------------------------------------------------------------------------

server <- function(input, output) {
  
  ########## main_page - strona główna ##########
  
  output$img1 <- renderUI({
    img(src = "Olek.jpg", width = 150)
  })
  
  output$img2 <- renderUI({
    img(src = "Piotr.jpg", width = 150)
  })
  
  output$img3 <- renderUI({
    img(src = "Seba_morda_2.jpg", width = 150)
  })
  
  ########## general_data_page - ogólne informacje ##########
  
  output$radar_plot <- renderPlot({
    filtered_data <- Data %>% 
      filter(day %within% interval(input$dateSelector[1],
                                   input$dateSelector[2])) %>% 
      select(Sleep.Quality,
             Asleep.after..seconds.,
             Regularity, Snore.time..seconds.,
             Coughing..per.hour.,
             Movements.per.hour,
             sleeper)
    
    min_vals <- apply(filtered_data[, -7], 2, min)
    max_vals <- apply(filtered_data[, -7], 2, max)
    data_norm <- as.data.frame(scale(filtered_data[, -7],
                                     center = min_vals,
                                     scale = max_vals - min_vals))
    data_norm$sleeper <- filtered_data$sleeper
    
    data_olek <- colMeans(data_norm[data_norm$sleeper == "Olek", -ncol(data_norm)])
    data_seba <- colMeans(data_norm[data_norm$sleeper == "Sebastian", -ncol(data_norm)])
    data_piotr <- colMeans(data_norm[data_norm$sleeper == "Piotr", -ncol(data_norm)])
    
    data_radar <- as.data.frame(rbind(
      rep(1, ncol(data_norm) - 1),  
      rep(0, ncol(data_norm) - 1),  
      data_olek,  
      data_seba,
      data_piotr))
    
    colnames(data_radar) <- c(
      "sleep quality",
      "time taken to fall asleep",
      "regularity",
      "snore time",
      "coughing per hour",
      "movements per hour"
    )
    
    
    clrs <- palette()[1:3]
    # Analogicznie do tego co było ↓
    # clrs_alpha <- c(clrs[1], alpha(clrs[2:3], 0.3125))
    # W ten sposób widać podpisaną oś ↓
    clrs_alpha <- alpha(clrs, 0.3125)
    radarchart(data_radar,
               axistype = 1, 
               # pcol = c("blue", "red", "green"),
               pcol = clrs,
               pfcol = clrs_alpha,
               # c("#0000FF50", "#FF000050", "lightgreen"),
               plwd = 2,  
               cglcol = "grey",  
               cglty = 1, 
               axislabcol = theme_get()$text$colour,
               vlcex = 0.8
    )
    
    legend("topright",
           legend = c("Olek", "Seba", "Piotr"),
           # col = c("blue", "red", "green"),
           col = clrs,
           lty = 1,
           lwd = 2)
  })
  
  output$sleep_hour_dist_ridgelines <- renderPlot({
    minutes <- Data |>
      filter(day %within% interval(input$dateSelector[1], input$dateSelector[2])) |>
      mutate(minute_went_to_bed = unclass(interval(day, Went.to.bed)) %/% 60) |>
      mutate(minute_woke_up = unclass(interval(day, Woke.up)) %/% 60)
    
    samples <- min(minutes$minute_went_to_bed):max(minutes$minute_woke_up)
    
    minutes <- Data |>
      filter(day %within% interval(input$dateSelector[1], input$dateSelector[2])) |>
      mutate(minute_went_to_bed = unclass(interval(day, Went.to.bed)) %/% 60) |>
      mutate(minute_woke_up = unclass(interval(day, Woke.up)) %/% 60)
    
    
    minutes |>
      cross_join(tibble(val = samples)) |>
      filter(input$weekday_selector == 0 |
               input$weekday_selector == wday(day, week_start = 1)) |>
      filter(minute_went_to_bed <= val & val <= minute_woke_up) |>
      ggplot(aes(x = make_datetime(min = val), y = sleeper, fill = factor(sleeper))) +
      stat_density_ridges(alpha = 0.6) +
      scale_x_datetime(
        limits = c(
          make_datetime(hour = -2),
          make_datetime(hour = 13)
        )
      ) +
      labs(
        x = "time of day",
        y = NULL,
        fill = "sleeper"
      ) +
      theme_ridges() +
      theme(
        text = theme_get()$text,
        axis.text = theme_get()$text,
        axis.text.y = element_blank()
      )
  })
  # <<<<<<< HEAD
  #     samples <- min(minutes$minute_went_to_bed):max(minutes$minute_woke_up)
  #     
  #     minutes |>
  #       cross_join(tibble(val = samples)) |>
  #       filter(minute_went_to_bed <= val & val <= minute_woke_up) |>
  #       ggplot(aes(x = val, y = sleeper, fill = factor(sleeper))) +
  #       stat_density_ridges(alpha = 0.6) +
  #       scale_x_continuous(
  #         labels = (\(x) format(make_datetime(min = x), "%H:%M"))
  #       ) +
  #       labs(
  #         x = "time of day",
  #         y = NULL,
  #         fill = "sleeper"
  #       ) +
  #       theme_ridges() +
  #       theme(
  #         text = theme_get()$text,
  #         axis.text = theme_get()$text,
  #         axis.text.y = element_blank()
  #       )
  #   })
  # =======
  #     output$sleep_hour_dist_ridgelines <- renderPlot({
  #     })
  # >>>>>>> local
  
  
  output$density_plot <- renderPlot({
    filtered_data <- Data %>% 
      filter(between(day, as.Date(input$dateSelector[1]), as.Date(input$dateSelector[2])))
    print(input$dateSelector[2])
    print(filtered_data)
    
    mean_df <- filtered_data %>% 
      group_by(sleeper) %>% 
      summarise(mean_SQ = mean(Sleep.Quality))
    
    
    density_plot <- ggplot(filtered_data, aes(x = Sleep.Quality, fill = sleeper)) + 
      geom_density(alpha = 0.3) +
      geom_vline(data = mean_df,
                 aes(xintercept = mean_SQ, color = sleeper),
                 linetype = "dashed") + 
      theme_minimal() +
      theme(
        text = theme_get()$text
      )
    density_plot
  })
  
  ########## individual_data_page - indywidualne informacje ##########
  
  output$sleeptimeCrossbar <- renderPlot({
    plot_data <- Data |>
      mutate(woke_up_inter = interval(day, Woke.up),
             went_to_bed_inter = interval(day, Went.to.bed),
             fell_asleep_time = Went.to.bed + dseconds(Asleep.after..seconds.)) |>
      mutate(fell_asleep_inter = interval(day, fell_asleep_time))
    (
      plot_data |>
        filter(sleeper == input$selectSleeper) |>
        ggplot(aes(x = day, y = fell_asleep_inter)) +
        # geom_crossbar(aes(ymin = went_to_bed_inter, ymax = fell_asleep_inter),
        #               fill = "#887711", colour = NA) +
        geom_crossbar(aes(ymax = woke_up_inter, ymin = fell_asleep_inter),
                      fill = palette()[2],  colour = NA) +
        scale_y_time(labels = (\(x) format(make_datetime(sec = x), "%H:%M")),
                     breaks =  (\(x) {
                       foo <- make_datetime(sec = floor(x[1]):(x[2]+1));
                       foo <- foo[second(foo)==0 & minute(foo)==0]}),
                     limits = c(
                       min(plot_data$went_to_bed_inter),
                       max(plot_data$woke_up_inter)
                     )
        ) +
        labs(
          y = "time of day"
        ) +
        theme(
          axis.title.x = element_blank(),
          axis.text.x = element_blank(),
          axis.ticks.x = element_blank(),
          panel.grid.major.x = element_blank()
        )
    ) + (
      Data |>
        filter(sleeper == input$selectSleeper) |>
        ggplot(aes(x = day, y = Sleep.Quality)) +
        geom_xspline() +
        # ylim(min(Data$Sleep.Quality), NA) +
        scale_y_continuous(
          labels = (\(x) paste(100*x, "%")),
          limits = c(min(Data$Sleep.Quality) - 0.05, NA),
          breaks = 2:5 /5
        ) +
        labs(
          y = "sleep quality"
        ) +
        scale_x_date(date_breaks = "3 days",
                     date_labels = "%b %e")
    ) + plot_layout(
      # guides = "collect",
      nrow = 2,
      ncol = 1,
      heights = c(0.7, 0.3)
    ) &
      theme(
        panel.background = element_blank(),
        panel.grid.major = element_line(
          colour = alpha(theme_get()$text$colour, 0.5)),
        axis.text = element_text(
          size = 12,
          colour = theme_get()$text$colour
        ),
        axis.title.y = element_text(
          colour = theme_get()$text$colour,
          angle = 90,
          size = 14
        )
      )
  })
  
  
  output$sleepDistractionScatter <- renderPlotly({
    dane <- Data %>% filter(sleeper == input$selectSleeper)
    plot_ly(dane, x = ~Movements.per.hour, y = ~Sleep.Quality,
            text = ~paste("Kaszlnięcia na godzinę: ", Coughing..per.hour.,
                          "<br> Czas chrapania: ", Snore.time..seconds., "s"),
            hoverinfo = "text",
            type = "scatter",
            mode = "markers",
            marker = list(
              color = "#F39C12",  
              size = 10,  
              line = list(
                color = "#FFFFFF", 
                width = 1  
              ))) %>% 
      layout(
        paper_bgcolor = "#2C3E50",
        plot_bgcolor = "#2C3E50",  
        font = list(color = "#FFFFFF"),
        xaxis = list(
          title = "Movements per Hour",
          color = "#FFFFFF", 
          gridcolor = "#34495E",
          range(0, 150)
        ),
        yaxis = list(
          title = "Sleep Quality",
          color = "#FFFFFF", 
          gridcolor = "#34495E",
          range = c(0.3, 1.05)
        ),
        hoverlabel = list(
          bgcolor = "#2C3E50", 
          font = list(color = "#FFFFFF")
        )
      )
  })
  
  
  output$activityBoxplot <- renderPlot({
    Piotr <- Piotr %>% 
      mutate(activity = c(FALSE, FALSE, TRUE, FALSE,
                          FALSE, FALSE, FALSE, FALSE, FALSE, TRUE, FALSE,
                          FALSE, TRUE, FALSE, TRUE, FALSE, FALSE, TRUE,
                          FALSE, TRUE, FALSE, TRUE, FALSE, TRUE,
                          TRUE, FALSE, TRUE, FALSE, TRUE, FALSE, TRUE,
                          TRUE, FALSE, TRUE, FALSE, TRUE, FALSE, TRUE, TRUE, FALSE))
    Olek <- Olek %>% 
      mutate(activity = c(TRUE, FALSE, FALSE, FALSE,
                          FALSE, FALSE, FALSE, TRUE, TRUE, FALSE, TRUE,
                          FALSE, FALSE, FALSE, TRUE, TRUE, FALSE, FALSE,
                          TRUE))
    
    p <- ggplot(if (input$selectSleeper == "Piotr") Piotr else Olek,
                aes(x = activity, y = Sleep.Quality)) +
      geom_boxplot() + 
      theme_minimal() +
      theme(
        text = theme_get()$text
      )
    p
    
  })
  
  
  output$heatmap <- renderPlot({
    tmp <- Data %>% 
      filter(sleeper == input$selectSleeper)
    pom1 <- generate_pom(tmp)
    p <- plot(pom1)
    p
  })
  
  
  ########## animation_page - aniamacja ##########
  
  
  output$bar_plot <- renderPlot({
    
    filtered_data <- Data %>% 
      filter(day <= input$day) %>%
      group_by(sleeper) %>%
      mutate(CumulativeSleepHours = as.numeric(sum(Time.asleep..seconds.)/ 3600)) %>% 
      summarise(CumulativeSleepHours = max(CumulativeSleepHours, na.rm = TRUE)) %>% 
      arrange(desc(CumulativeSleepHours))
    
    #Set colors for persons
    # custom_colors <- c("Sebastian" = "blue", "Piotr" = "red", "Olek" = "green")
    
    
    ggplot(filtered_data, aes(x = reorder(sleeper, CumulativeSleepHours), y = CumulativeSleepHours, fill = sleeper)) +
      geom_col(alpha = 0.8) +
      # scale_fill_manual(values = custom_colors) +
      labs(
        title = paste("Cumulative sleep time till", input$day),
        x = "Person",
        y = "Sleep time (hours)"
      ) +
      theme_minimal() +
      theme(
        # legend.position = "none",
        text = theme_get()$text,
        axis.text.x = element_text(
          colour =  theme_get()$text$colour,
          size = 12)
      ) +
      coord_flip()
    
  })
}

# ------------------------------------------------------------------------------
# aplikacja
# ------------------------------------------------------------------------------

app_ui <- navbarPage(
  title = div(
    icon("bed", style = "color: darkblue; font-size: 24px;"), 
    "MiNI REST",
    icon("bed", style = "color: darkblue; font-size: 24px;"),
  ),
  tabPanel("Main page", main_page),
  tabPanel("Ogólne dane", general_data_page),
  tabPanel("Indywidualne dane", individual_data_page),
  tabPanel("Animacja", animation_page),
  theme = bslib::bs_theme(bootswatch = "darkly", 
                          primary = "#F39C12",
                          secondary = "#3498DB",
                          bg = "#2C3E50",   
                          fg = "#FFFFFF",
                          success = "#AED6F1"),
  header = tags$head(),
  footer = shiny::HTML("
                <footer class='text-center text-sm-start' style='width:100%;'>
                <hr>
                <p class='text-center' style='font-size:12px;'>
                  Link do 
                  <a class='highlighted-link' href='https://github.com/SebastianBoteroLeonik/
                  TWD-Projekt_2/'
                  style='color: #007bff; font-weight: bold; text-decoration: underline;'
                  > repozytorium</a>
                </p>
                </footer>
                ")
)

# Run the application 
shinyApp(app_ui, server)






