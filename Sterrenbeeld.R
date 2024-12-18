library(shiny)

# Function to determine zodiac sign
get_zodiac_sign <- function(date) {
  day <- as.integer(format(date, "%d"))
  month <- as.integer(format(date, "%m"))
  year <- as.integer(format(date, "%Y"))
  
  if (year == 1632 && day == 24 && month == 11) {
    return("spinoza")
  } else if ((month == 3 && day >= 21) || (month == 4 && day <= 19)) {
    return("aries")
  } else if ((month == 4 && day >= 20) || (month == 5 && day <= 20)) {
    return("taurus")
  } else if ((month == 5 && day >= 21) || (month == 6 && day <= 20)) {
    return("gemini")
  } else if ((month == 6 && day >= 21) || (month == 7 && day <= 22)) {
    return("cancer")
  } else if ((month == 7 && day >= 23) || (month == 8 && day <= 22)) {
    return("leo")
  } else if ((month == 8 && day >= 23) || (month == 9 && day <= 22)) {
    return("virgo")
  } else if ((month == 9 && day >= 23) || (month == 10 && day <= 22)) {
    return("libra")
  } else if ((month == 10 && day >= 23) || (month == 11 && day <= 21)) {
    return("scorpio")
  } else if ((month == 11 && day >= 22) || (month == 12 && day <= 21)) {
    return("sagittarius")
  } else if ((month == 12 && day >= 22) || (month == 1 && day <= 19)) {
    return("capricorn")
  } else if ((month == 1 && day >= 20) || (month == 2 && day <= 18)) {
    return("aquarius")
  } else if ((month == 2 && day >= 19) || (month == 3 && day <= 20)) {
    return("pisces")
  }
}

# UI
ui <- fluidPage(
  titlePanel("Zodiac Sign Viewer"),
  sidebarLayout(
    sidebarPanel(
      dateInput("date", "Select Your Date of Birth:", value = Sys.Date(), format = "yyyy-mm-dd"),
      br(),
      textOutput("zodiac_sign")
    ),
    mainPanel(
      imageOutput("zodiac_image")
    )
  )
)

# Server
server <- function(input, output) {
  # Reactive zodiac sign
  zodiac_sign <- reactive({
    req(input$date)
    get_zodiac_sign(input$date)
  })
  
  # Output the zodiac sign text
  output$zodiac_sign <- renderText({
    paste("Your Zodiac Sign is:", toupper(zodiac_sign()))
  })
  
  # Output the zodiac image
  output$zodiac_image <- renderImage({
    list(
      src = file.path("www", paste0(zodiac_sign(), ".png")),
      contentType = "image/png",
      alt = zodiac_sign()
    )
  }, deleteFile = FALSE)
}

# Run the app
shinyApp(ui = ui, server = server)
