#################################################################################
# Developer: Benjamin Neely
# Date:      8/11/2014
# Name:      ui.R
# Desc:      This is the user interface file for the prediction model website.
#################################################################################
shinyUI(fluidPage(
  #theme="style.css", 
  list(tags$head(
    HTML('<link rel="icon", href="GARFIELD_AF_logo.png", type="image/png" />'))),
  div(style="padding: 0px 0px; width: '100%'",
      titlePanel(title="", windowTitle="GARFIELD-AF")
  ),
  headerPanel(
    title=div(img(src="GARFIELD_AF_logo.png",width="180",height="50"), "Non-hemorrhagic Stroke or Systemic Embolism", align="left"),
    h4("Source Code")
  ),
  
  sidebarLayout(
    sidebarPanel(
      h4("Risk Factors"),
      uiOutput("Dynamic")
    ),
    mainPanel(
          h4("Risk Plot Over Time"),
          tags$a(href="https://github.com/benneely/garfield_prediction_models", "Application Source Code available on GitHub."),
 #         div("This plot is interactive. Use the mouse to 'click' on a particular point in time on the red line. The interpretation below will reflect the prediction at that point in time."),
          plotOutput('plot', click = "plot_click"),
#           htmlOutput('plot'),
          h4("Explanation of Risk"),
          textOutput("text2")
    )
  )
))# 