## this file is solely application presented as one function file.
## all funtions needed to calculate wings characteristics are also presented
## file index.Rmd calls this function.


flight.app <- function (bat) {
        bat<-data.frame(
                x=c(0,  -29,  -60,  -74,  -66, -56, -51, -40, -36, -28, -25, -17, -11,  -7,  -4,  -2,  0),
                y=c(90, 94, 86, 61, 59, 54, 49, 52, 51, 44, 38, 38, 35, 31, 21, 15, 12))
        
        urlfile<-'https://raw.githubusercontent.com/mariaghazali/for.myshine.app/master/wings.dataset.csv'
        # urlfile<-"wings.dataset.csv" ## if you download this file to local folder
        data.interpr<-read.csv(urlfile)
        bat.chull<-with(data.interpr,{data<-data.interpr[mamm=="bat",c("WL","AR")]; tmp<-data[chull(data$WL,data$AR),]; rbind(tmp,tmp[1,])})
        bird.chull<-with(data.interpr,{data<-data.interpr[mamm=="bird",c("WL","AR")]; tmp<-data[chull(data$WL,data$AR),]; rbind(tmp,tmp[1,])})
        xlab<-"Wing loading (log scale)"
        ylab<-"Aspect ratio (log scale)"
        
        # functions which will be used to calculate Aspect ratio and Wing-loading
        polygon.coords<-function(xy){
                xy.coords<-data.frame(x=xy[,1],y=xy[,2])
                xy.coords<-rbind(c(0,90),xy.coords,c(0,tail(xy.coords,1)[,2]))
                xy.coords[xy.coords$x>0,1]=0
                xy.coords
        }
        
        area.polygon<-function(xy){
                xy<-rbind(xy,xy[1,])
                aa<-NULL
                for (i in 1:nrow(xy)) {
                        aa[i]<-xy$x[i]*xy$y[i+1]-xy$y[i]*xy$x[i+1]
                }
                aa<-2*abs(sum(aa,na.rm=T)*0.5)
                aa
        }
        
        wingspan<- function(xy) { return(2*(max(abs(xy[,1])))) }
        
        aspect.ratio<-function(xy){
                area<-area.polygon(xy)
                wingsp<-wingspan(xy)
                AR<-(wingsp^2)/area
                return(AR)
        }
shinyApp( options=list(width="100%", height=770),
        ui = fluidPage(
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
        ),
        server = function(input, output) {
                        ##### 
                        ##### code for tab1 - example outline
                        ##### 
                        output$bat <- renderPrint({ data.frame(x=bat[,1],y=bat[,2]) })
                        
                        output$example<-renderPlot({
                                plot(bat,asp=1,pch=21,bg="red",col="red",xlab="x",ylab="y",xlim=range(-100,100),ylim=range(0,100))
                                points(x=c(0),y=c(90),col="black",pch=21,bg="black",cex=2)
                                legend("topleft",legend="Half of the bat:",bty="n")
                                abline(v=0,lwd=2)
                                text(x=c(0),y=c(95),"start here",pos=3)
                                polygon(bat,col="grey",density=45)
                        })
                        
                        output$result.example1<-renderPrint({cat("aspect ratio =",(wingspan(bat)^2)/(2*area.polygon(bat)),";",
                                                                 "wing loading =",(0.01*9.81)/(2*area.polygon(bat)/1000^2),"( N/m^(-2) )")
                        })
                        
                        output$result.example2<-renderPrint({cat("area =",2*area.polygon(bat)/1000^2,"(m^2)","; ",
                                                                 "wingspan =",wingspan(bat)/1000,"(m)")
                        })
                        ##### 
                        ##### code for tab2 - drawing the bat/bird and calculation of traits
                        #####
                        xy <- reactiveValues(
                                coordinates.x = NULL,    # this stores the range of x  of clicks
                                coordinates.y = NULL    # this stores the range of y of clicks
                        )
                        
                        # Handle clicks on the plot
                        observeEvent(input$plot_click, {
                                if (is.null(xy$coordinates.x)) {
                                        # We don't have a first click, so this is the first click
                                        xy$coordinates.x<-input$plot_click$x
                                        xy$coordinates.y<-input$plot_click$y
                                } else {
                                        # We already had a first click, so this is the second and other clicks.
                                        xy$coordinates.x <- c(xy$coordinates.x, input$plot_click$x)
                                        xy$coordinates.y <- c(xy$coordinates.y, input$plot_click$y)
                                }
                        })
                        
                        output$plot <- renderPlot({
                                plot(1,1,asp=1,type="n",xlab="x",ylab="y",xlim=range(-100,0),ylim=range(1,100))
                                points(x=c(0),y=c(90),col="black",pch=21,bg="black",cex=2)
                                abline(v=0,lwd=2)
                                text(x=c(0),y=c(95),"start here",pos=3)
                                if (is.null(xy$coordinates.x))
                                        legend("center",legend="click some points on this side")
                                else {  points(xy$coordinates.x,xy$coordinates.y,pch=21,bg="red",col="black",type="b")
                                }
                        })
                        
                        output$coordinates <- renderPrint({ data.frame(x=xy$coordinates.x,y=xy$coordinates.y) })
                        
                        xy.coords <- reactive({
                                polygon.coords(data.frame(x=xy$coordinates.x,y=xy$coordinates.y))     # here transformed coordinations will be placed
                        })
                        
                        observeEvent(input$DrawButton, {
                                if(is.null(xy$coordinates.x)) {
                                        output$plot<-renderPlot({
                                                plot(1,1,type="n",axes=FALSE,xlab="",ylab="")
                                                text(1,1,label="Please RELOAD and DRAW some points on plot!",font=1)
                                        })
                                }
                                else {
                                        output$plot<-renderPlot({
                                                plot(xy.coords(),asp=1,pch=21,bg="red",col="red",xlab="x",ylab="y",
                                                     xlim=range(-xy.coords()[,1],xy.coords()[,1]),ylim=range(0,100))
                                                points(x=c(0),y=c(90),col="black",pch=21,bg="black",cex=2)
                                                abline(v=0,lwd=2)
                                                text(x=c(0),y=c(95),"start here",pos=3)
                                                polygon(xy.coords(),col="grey",density=45)
                                                polygon(-xy.coords()[,1],xy.coords()[,2],col="grey",density=45)
                                        }) 
                                }
                        })
                        
                        
                        to.scale <-reactive({
                                if (input$scale=="1 mm") 1/1000 else 1/100
                        })
                        str.scale <-eventReactive(input$GoButton,{
                                if (is.null(xy$coordinates.x)){""} 
                                else {"(m)" }
                        })
                        str.scale.area <-eventReactive(input$GoButton,{
                                if (is.null(xy$coordinates.x)){""} 
                                else {"(m^2)"}
                        })
                        go.area<-eventReactive(input$GoButton, {
                                if (is.null(xy$coordinates.x)){"NA"}
                                else {2*area.polygon(xy.coords())*to.scale()^2}
                        })
                        
                        go.wingspan<-eventReactive(input$GoButton, {
                                if (is.null(xy$coordinates.x)){"NA"}
                                else {wingspan(xy.coords())*to.scale()}
                        })
                        
                        go.aspect.ratio<-eventReactive(input$GoButton, {
                                if (is.null(xy$coordinates.x)){"NA"}
                                else {aspect.ratio(xy.coords())}
                        })
                        
                        weight<-reactive({input$weight})
                        
                        go.wingloading<-eventReactive(input$GoButton, {
                                if (is.null(xy$coordinates.x)){"NA"}
                                else {(weight()*9.81)/go.area()}
                        })
                        
                        output$result.text1<-renderPrint({cat("aspect ratio =",go.aspect.ratio(),";",
                                                              "wing loading = ", go.wingloading(), "( N/m^(-2) )")
                        })
                        
                        output$result.text2<-renderPrint({cat("wingspan =",go.wingspan(),str.scale(),";",
                                                              "area =",go.area(),str.scale.area())
                        })
                        
                        output$interpretation<-renderPlot({
                                if(is.null(xy$coordinates.x)) {        
                                        plot(x=data.interpr$WL,y=data.interpr$AR,xlab=xlab,ylab=ylab,
                                             pch=19,col=data.interpr$mamm,log="xy",cex=0.5)
                                        lines(bat.chull)
                                        lines(bird.chull,col="red")
                                        text(data.interpr$WL,data.interpr$AR,label=data.interpr$comment,font=3,col=unclass(data.interpr$mamm),cex=0.9)
                                        text(data.interpr$WL,data.interpr$AR,label=data.interpr$comment1,pos=1,font=3,col=unclass(data.interpr$mamm),cex=0.9)
                                }
                                else {
                                        plot(x=data.interpr$WL,y=data.interpr$AR,xlab=xlab,ylab=ylab,
                                             xlim=range(data.interpr$WL,go.wingloading()),
                                             ylim=range(data.interpr$AR,go.aspect.ratio()),
                                             pch=19,col=data.interpr$mamm,log="xy",cex=0.8)
                                        lines(bat.chull)
                                        lines(bird.chull,col="red")
                                        text(data.interpr$WL,data.interpr$AR,label=data.interpr$comment,font=3,col=unclass(data.interpr$mamm),cex=1)
                                        text(data.interpr$WL,data.interpr$AR,label=data.interpr$comment1,pos=1,font=3,col=unclass(data.interpr$mamm),cex=1)
                                        points(x=go.wingloading(),y=go.aspect.ratio(),pch=21,bg="orange",cex=2)
                                }
                        })
                        
                        output$legend<-renderPlot({
                                plot(x=1,y=1,axes=FALSE,pch="",xlab="",ylab="")
                                legend("topleft",pch=c(19,19,19,21),lty=c(1,1,0,0),pt.cex=c(0.9,0.9,0.9,2),col=c(1,2,3,1),pt.bg="orange",
                                       legend=c("bats","birds","airplanes","Your creature"))
                        })
                }
)

}
