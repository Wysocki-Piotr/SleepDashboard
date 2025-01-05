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


SebastianRaw <- read.csv2("C:/Users/Admin/Downloads/sleepdataSebastian.csv")
PiotrRaw <- read.csv2("C:/Users/Admin/Downloads/sleepdataPiotr (1).csv")
OlekRaw <- read.csv("C:/Users/Admin/Downloads/sleepdataOlek.csv")


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
      day = ymd_hms(Went.to.bed),
      Coughing..per.hour. = as.numeric(Coughing..per.hour.),
      Movements.per.hour = as.numeric(Movements.per.hour),
      General.day.asleep = ifelse(hour(ymd_hms(Went.to.bed)) < 15, day(day)-1,day(day)),
      General.month.asleep = ifelse(hour(ymd_hms(Went.to.bed))< 15 & day(day) == 1, 12, month(day)) 
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
      geom_crossbar(aes(ymin = went_to_bed_inter, ymax = fell_asleep_inter), fill = "#887711", colour = NA) +
      geom_crossbar(aes(ymax = woke_up_inter, ymin = fell_asleep_inter), fill = "#223388", colour = NA) +
      scale_y_continuous(labels = (\(x) format(make_datetime(sec = x), "%H:%M"))) +
      labs(
        y = "hour"
      )
  })
}

# Run the application 
shinyApp(ui = ui, server = server)
