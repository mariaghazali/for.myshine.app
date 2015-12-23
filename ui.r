# Example to show correlation between two characteristics of wing shape 
# presented with aspect ratio and wing-loading.
# It includes point-by-point drawing of some winged creature (bird or bat),
# examples of the outline and interpretation plot.

library(shiny)
shinyUI(
        fluidPage(
                headerPanel("Flight performance"),
                helpText("Note: Please look at Tab.1 (Example). Then, move to Tab 2.Draw here! and point-by-point draw your own outline of winged creature."), 
                helpText("As they are bilateral animals, only one half of the outline is sufficient. Click some points, press Draw! and then Go!"),
                helpText("At last, see Tab 3.Interpretation and realize which nature design your creature is close to."),
                tabsetPanel(type="pills",
                        tabPanel("1.Example", 
                                 mainPanel(plotOutput("example"),
                                           h4('Example results'),
                                           verbatimTextOutput("result.example1"),
                                           verbatimTextOutput("result.example2")
                                 ),
                                 sidebarPanel(radioButtons("scale.ex","Scale: 1 unit on plot is",c("1 mm"),
                                                           selected="1 mm",inline=TRUE),
                                              h5("Weight in kg = 0.010"),
                                              h4("Example points"),
                                              verbatimTextOutput("bat"))
                        ),
                        tabPanel("2.Draw here!",
                                 mainPanel(
                                         plotOutput("plot", click = clickOpts(id = "plot_click")),
                                         actionButton("DrawButton","Draw!"),
                                         actionButton("GoButton","Go!"),
                                         h4('Your results'),
                                         verbatimTextOutput("result.text1"),
                                         verbatimTextOutput("result.text2")
                                 ),
                                 sidebarPanel(helpText("if you change smth, press Go!"),
                                              radioButtons("scale","Scale: 1 unit on plot is",c("1 mm","1 cm"),
                                                           selected="1 mm",inline=TRUE),
                                              numericInput("weight","Weight in kg:",0.01,min=0,max=100,step=0.001),
                                              h4("Your points"),
                                              verbatimTextOutput("coordinates"))
                        ),
                        tabPanel("3.Interpretation",
                                 mainPanel(plotOutput("interpretation")),
                                 sidebarPanel(plotOutput("legend")))
                )
        )
)
                
