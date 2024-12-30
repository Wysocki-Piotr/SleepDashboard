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


OlekRaw <- read.csv2("../data/sleepdataOlek.csv")
SebastianRaw <- read.csv2("../data/sleepdataSebastian.csv")
PiotrRaw <- read.csv2("../data/sleepdata.xls.csv")


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
      day = as.Date(Woke.up),
      Coughing..per.hour. = as.numeric(Coughing..per.hour.),
      Movements.per.hour = as.numeric(Movements.per.hour),
    ) |>
    filter(day %within% (ymd("2024-12-01") %--% now())) |>
    select(-c(
      City,
      Alertness.score,
      Alertness.reaction.time..seconds.,
      Alertness.accuracy
    ))
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
          plotOutput("sleeptimeCrossbar")
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
      output$sleeptimeCrossbar <- renderPlot({
        Data |>
          filter(sleeper == input$selectSleeper) |>
          mutate(woke_up_inter = interval(day, Woke.up),
                 went_to_bed_inter = interval(day, Went.to.bed),
                 fell_asleep_time = Went.to.bed + dseconds(Asleep.after..seconds.)) |>
          mutate(fell_asleep_inter = interval(day, fell_asleep_time))|>
          # View()
          ggplot(aes(x = day, y = fell_asleep_inter)) +
          # geom_errorbar()
          geom_crossbar(aes(ymin = went_to_bed_inter, ymax = fell_asleep_inter), fill = "#887711") +
          geom_crossbar(aes(ymax = woke_up_inter, ymin = fell_asleep_inter), fill = "#223388") +
          scale_y_continuous(labels = (\(x) format(make_datetime(sec = x), "%H:%M"))) +
          labs(
            y = "hour"
          )
      })
}

# Run the application 
shinyApp(ui = ui, server = server)
