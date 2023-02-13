library(shiny)
library(fCopulae)
library(copula)
library(VineCopula)
ui<-fluidPage(
  titlePanel("Copula Slider"),
  tabsetPanel(
    tabPanel( title= "Plots",
              sidebarLayout(
                sidebarPanel(
                  #Type of Copula
                  selectInput("types", 
                              "Choose the type of Copula",
                              choices = list("Archimedean Copula",
                                             "Elliptical Copula",
                                             "Extreme Value Copula"),
                              selected=1),
                  
                  #Number of Archimedean Copula   
                  
                  conditionalPanel(condition="input.types=='Archimedean Copula'",
                                   selectInput("no",label="No. of Copula",choices=c(1:22),selected=1),
                                   selectInput("names",label="Names of Copula",choices=list("Clayton"=1,
                                                                                            "Ali-Mikhail-Hag"=3,
                                                                                            "Grumbel-Hongard"=4,
                                                                                            "Frank"=5,
                                                                                            "Joe-Frank"=6,
                                                                                            "Gumbel-Barnett"=9,
                                                                                            "Genest-Ghoudi"=15),selected=1)),                                      

                  
                  #Number of Elliptical Copula                           
                  conditionalPanel(condition="input.types=='Elliptical Copula'&& input.plots=='1'",
                                   selectInput("no2",label="No. and Name of Copula",
                                               choices=list("1-Normal"="norm",
                                                            "2-Cauchy"="cauchy",
                                                            "3-Student t"="t"),selected=1)),
                  
                  conditionalPanel(condition="input.types=='Elliptical Copula'&& (input.plots=='2'||input.plots=='3')",
                                   selectInput("no4",label="No. and Name of Copula",
                                               choices=list("1-Normal"="norm",
                                                            "2-Student t"="t",
                                                            "3-Logistic"="logistic",
                                                            "4-Exponential Power [Laplace|Kotz]"='epower'),selected=1)),
                  
                  
                  #Number of Extreme Value Copula                           
                  conditionalPanel(condition="input.types=='Extreme Value Copula'",
                                   selectInput("no3",label="No.and Name of Copula",
                                               choices=list("1-Gumbel"="gumbel",
                                                            "2-Galambos"="galambos",
                                                            "3-Husler-Reiss"="husler.reiss",
                                                            "4-Tawn"="tawn",
                                                            "5-BB5"="bb5"),selected=1)),
                  
                  #Type of Plot
                  radioButtons("plots", label = "Choose the type of Plots",
                               choices = list("Variate" = 1, "Probability" = 2,"Density"=3), 
                               selected = 1),
                  
                  conditionalPanel(condition="input.plots=='2'||input.plots=='3'",
                                   radioButtons("plot2","Choose the type of Plot",
                                                choices=list("Contour"=1,"Perspective"=2),
                                                selected=1)),
                  
                  
                  #Different ranges for parameterd of the corresponding copula
                  sliderInput("alpha","alpha value",value=4.5,min=-1,max=10,step=0.1),
                  
                  conditionalPanel(condition="input.types=='Elliptical Copula'&& input.no2=='t'",
                                   sliderInput("df","degree of freedom",value=4,min=1,max=100)),
                  conditionalPanel(condition="input.types=='Extreme Value Copula'&& input.no3=='tawn'",
                                   sliderInput("beta","beta value",min=0,max=1,value=0.5),
                                   sliderInput("r","r value",min=1,max=10,value=5.5)),
                  conditionalPanel(condition="input.types=='Extreme Value Copula'&& input.no3=='bb5'",
                                   sliderInput("theta","theta value",min=1,max=10,value=5.5)),
                  
                  
                  conditionalPanel(condition="input.types=='Elliptical Copula'&& (input.plots=='2'&& input.no4=='t'|| input.plots=='3'&& input.no4=='t') ",
                                   sliderInput("nu","nu value",min=1,max=20,value=10.5)),
                  conditionalPanel(condition="input.types=='Elliptical Copula'&& (input.plots=='2'&& input.no4=='epower'|| input.plots=='3'&& input.no4=='epower')",
                                   sliderInput("s","s value",min=0.1,max=5,value=2.5)),
                  
                  
                  conditionalPanel(condition="input.plots=='2'||input.plots=='3'",
                                   sliderInput("N","N Value",min=10,max=100,value=50)),
                  conditionalPanel(condition="(input.plots=='2'||input.plots=='3')&&input.plot2=='1'",
                                   sliderInput("nlevel","Level of Contour",min=5,max=100,value=15)),
                  conditionalPanel(condition="input.plot2=='2'",
                                   sliderInput("theta_plot","Plot: theta value",min=-180,max=180,value=-40),
                                   sliderInput("phi","Plot: phi value",min=0,max=360,value=30)),
                  HTML('View the <a href="https://github.com/KECHEN01/Copulae_Plots">Code</a>.')
                  
                  
                  
                  
                ),
                
                
                
                mainPanel(
                  plotOutput("plots_output"))
              )),
    tabPanel(
      title="Data",
      sidebarLayout(
        sidebarPanel(
          fileInput("file","Upload your file",
                    accept = c(
                      "text/csv",
                      "text/comma-separated-values,text/plain",
                      ".csv")), 
          helpText("File size should not exceed 3MB"),
          br(),
          h5(helpText("Select below parameters to read the file")),
          
          checkboxInput(inputId = 'header', label = 'Header', value = TRUE),
          checkboxInput(inputId = "stringAsFactors", "stringAsFactors", FALSE),
          
          #breakline
          br(),
          h5(helpText("Select the goodness-of-fit test method")),
          radioButtons("method","Method",choices=c("White"="white",
                                                   "Kendall"="kendall"),selected="white")
          
          
          
        ),
        mainPanel(
          tabsetPanel(tabPanel("File Info", tableOutput("filedf")),
                      tabPanel("Data", dataTableOutput("table")),
                      tabPanel("Structure", verbatimTextOutput("fileob")),
                      tabPanel("Summary", tableOutput("sum")),
                      tabPanel("Copula Fit",verbatimTextOutput("copula")),
                      tabPanel("Goodness of Fit",verbatimTextOutput("gof"),
                               fluidRow(splitLayout(column(6,plotOutput(outputId="plot1", width="300px",height="300px")),  
                                                    column(6,plotOutput(outputId="plot2", width="300px",height="300px")))))
        )
      )))))

server<-function(input,output,session){
  
  ############################Variates#################################
  #Change the range and value of alpha according to the No of Archimedean Copula
  observe({
    new_no<-input$no
    new_plot<-input$plots
    types<-input$types
    if(new_no== 1 )
      updateSliderInput(session,"alpha","alpha value",min=-1,max=10,value=4.5)
    
    if((new_no== 2 || new_no==4 || new_no==6 || new_no==8 
        || new_no==12 || new_no==14 || new_no==15 || new_no==21) )
      updateSliderInput(session,"alpha","alpha value",
                        min=1,max=10,step=0.1,value=5)
    
    if(new_no== 3)
      updateSliderInput(session,"alpha",min=-1,max=1,step=0.1,value=0)
    
    if((new_no== 7 || new_no==9 || new_no==10 || new_no==22 ) )
      updateSliderInput(session,"alpha",min=0,max=1,step=0.1,value=0.5)
    
    if((new_no== 13 || new_no==16 || new_no==19 || new_no==20 ))
      updateSliderInput(session,"alpha",min=0,max=10,step=0.1,value=5)
    
    if(new_no== 11 )
      updateSliderInput(session,"alpha",min=0,max=0.5,step=0.1,value=0.2)
    
    if(new_no== 18 )
      updateSliderInput(session,"alpha",min=2,max=10,step=0.1,value=6)
    
    if ((new_no== 5 || new_no==17 ))
      updateSliderInput(session,"alpha",min=-20,max=20,step=0.1,value=1)
    
    if (types=="Elliptical Copula")
      updateSliderInput(session,"alpha",label="rho value",min=-1,max=1,value=0)
    
    if ((types=="Extreme Value Copula") && (input$no3=="gumbel"))
      updateSliderInput(session,"alpha","delta value",min=1,max=10,value=5.5)
    
    if ((types=="Extreme Value Copula")&& (input$no3=="galambos" || input$no3=="husler.reiss"))
      updateSliderInput(session,"alpha","delta value",min=0,max=10,value=5)
    
    if ((types=="Extreme Value Copula") && (input$no3=="tawn") )
      updateSliderInput(session,"alpha","alpha value", min=0,max=1,value=0.5)
    
    if ((types=="Extreme Value Copula" )&&(input$no3=="bb5"))
      updateSliderInput(session,"alpha","delta value",min=0,max=10,value=5)
    
    ##########################Probability###############################################
    
    if( types=="Elliptical Copula"&& new_plot=="2")
      updateSliderInput(session,"alpha","rho value",min=-0.95,max=0.95,step=0.05,value=0)
  })
  
  
  
  
  
  
  
  
  
  ######################################Different Plots#############################################
  output$plots_output<-renderPlot({
    Names = c(
      "- Clayton", "", 
      "- Ali-Mikhail-Hag", 
      "- Gumbel-Hougard", 
      "- Frank",
      "- Joe-Frank", "", "", 
      "- Gumbel-Barnett", "", "", "", "", "", 
      "- Genest-Ghoudi", "", "", "", "", "", "", "")
    
    #################################Variates##################################
    
    ##Archimedean
    
    if (input$types=="Archimedean Copula"&&input$plots=="1"){
      R=tryCatch(rarchmCopula(n = 1000, alpha = input$alpha,type=as.character(input$no)),
                 error = function(e){if (input$no== 2 || input$no==4 || input$no==6 || input$no==8 || input$no==12 || input$no==14 || input$no==15 || input$no==21)
                 {rarchmCopula(n = 1000, alpha =5,type=as.character(input$no))}
                   else if (input$no== 3)
                   {rarchmCopula(n = 1000, alpha =0,type=as.character(input$no))}
                   else if((input$no== 7 || input$no==9 || input$no==10 || input$no==22 ) )
                   {rarchmCopula(n = 1000, alpha =0.5,type=as.character(input$no))}
                   else if((input$no== 13 || input$no==16 || input$no==19 || input$no==20 ))
                   {rarchmCopula(n = 1000, alpha =5,type=as.character(input$no))}
                   else if(input$no== 11 )
                   {rarchmCopula(n = 1000, alpha =0.2,type=as.character(input$no))}
                   else if(input$no== 18 )
                   {rarchmCopula(n = 1000, alpha =6,type=as.character(input$no))}
                   else if ((input$no== 5 || input$no==17 ))
                   {rarchmCopula(n = 1000, alpha =0,type=as.character(input$no))}
                   else (rarchmCopula(n = 1000, alpha =4.5,type=as.character(input$no)))
                 })
      plot(R, xlab = "U", ylab = "V", pch = 19, col = "steelblue")
      grid()
      title(main = paste("Archimedean Copula No:",input$no,Names[as.numeric(input$no)],"\nalpha = ",input$alpha))}
    
    ##Elliptical
    
    if (input$types=="Elliptical Copula"&&input$plots=="1"){ 
      if ( input$no2=="norm" || input$no2=="cauchy"){
        R = rellipticalCopula(n = 2000, rho = input$alpha, param = NULL, type =input$no2)
        plot(x = R[, 1], y = R[, 2], xlim = c(0, 1), ylim = c(0, 1),
             xlab = "u", ylab = "v", pch = 19,col="steelblue")
        grid()
        title(main = paste("Elliptical Copula:",input$no2,"\nrho = ",input$alpha))}
      
      else {
        R = rellipticalCopula(n = 2000, rho = input$alpha, param =input$df, type =input$no2)
        plot(x = R[, 1], y = R[, 2], xlim = c(0, 1), ylim = c(0, 1),
             xlab = "u", ylab = "v", pch = 19,col="steelblue")
        grid()
        title(main = paste("Elliptical Copula:",input$no2,"\nrho = ",input$alpha,"\ndegree of freedom =",input$df))}
    }
    
    ##Extreme Value
    
    if (input$types=="Extreme Value Copula"&&input$plots=="1"){
      if (input$no3=="gumbel" || input$no3=="galambos" || input$no3=="husler.reiss"){
        R = revCopula(2000, param = input$alpha, type = input$no3)
        plot(R, pch = 19, col = "steelblue")
        grid()
        title(main = paste("Extreme Copula:",input$no3,"\ndelta = ",input$alpha))}
      
      else if (input$no3=="tawn"){
        R = revCopula(2000, param =c(input$alpha,input$beta,input$r), type ="tawn")
        plot(R, pch = 19, col = "steelblue")
        grid()
        title(main = paste("Extreme Copula:",input$no3,"\nalpha = ",input$alpha," beta = ",input$beta," r = ",input$r))}
      
      else {
        R = revCopula(2000, param =c(input$alpha,input$theta), type = "bb5")
        plot(R, pch = 19, col = "steelblue")
        grid()
        title(main = paste("Extreme Copula:",input$no3,"\ndelta = ",input$alpha," theta = ",input$theta))}
    }
    
    
    
    
    
    #######################################Probability###################################
    
    ###################Contour
    
    ##Archimedean Copula
    
    if (input$types=="Archimedean Copula"&& input$plots =="2"&&input$plot2=="1" ){
      uv=grid2d(x = (0:input$N)/input$N)
      P=tryCatch(parchmCopula(u =uv, v = uv, alpha =input$alpha, type = as.character(input$no),output = "list"),
                 error = function(e){
                   if (input$no== 2 || input$no==4 || input$no==6 || input$no==8 || input$no==12 || input$no==14 || input$no==15 || input$no==21)
                   {parchmCopula(u =uv, v = uv, alpha =5, type = as.character(input$no),output = "list")}
                   else if (input$no== 3)
                   {parchmCopula(u =uv, v = uv, alpha =0, type = as.character(input$no),output = "list")}
                   else if((input$no== 7 || input$no==9 || input$no==10 || input$no==22 ) )
                   {parchmCopula(u =uv, v = uv, alpha =0.5, type = as.character(input$no),output = "list")}
                   else if((input$no== 13 || input$no==16 || input$no==19 || input$no==20 ))
                   {parchmCopula(u =uv, v = uv, alpha =5, type = as.character(input$no),output = "list")}
                   else if(input$no== 11 )
                   {parchmCopula(u =uv, v = uv, alpha =0.2, type = as.character(input$no),output = "list")}
                   else if(input$no== 18 )
                   {parchmCopula(u =uv, v = uv, alpha =6, type = as.character(input$no),output = "list")}
                   else if ((input$no== 5 || input$no==17 ))
                   {parchmCopula(u =uv, v = uv, alpha =0, type = as.character(input$no),output = "list")}
                   else (parchmCopula(u =uv, v = uv, alpha =4.5, type = as.character(input$no),output = "list"))
                 })
      image(P, col = heat.colors(16) )
      contour(P, xlab = "u", ylab = "v", nlevels =input$nlevel, add = TRUE)
      title(main = paste("Archimedean Copula No:",input$no,Names[input$no],"\nalpha = ",input$alpha,"\nN:",input$N," Level of Contours:",input$nlevel))
    }
    
    ##Elliptical Copula
    
    if (input$types=="Elliptical Copula"&& input$plots =="2"&&input$plot2=="1" ){
      
      if (input$no4=="norm"||input$no4=="logistic"){
        P=pellipticalCopula(u = input$N, v = input$N, rho =input$alpha, param = NULL, type =input$no4,
                            output = "list", border = TRUE)
        image(P, col = heat.colors(16), ylab = "v")
        mtext("u", side = 1, line = 2)
        contour(P, nlevels =input$nlevel, add = TRUE)
        title(main = paste("Elliptical Copula:",input$no4,"\nrho = ",input$alpha,"\nN:",input$N," Level of Contours:",input$nlevel))}
      
      else if (input$no4=="t"){
        P=pellipticalCopula(u = input$N, v = input$N, rho =input$alpha, param =input$nu, type =input$no4,
                            output = "list", border = TRUE)
        image(P, col = heat.colors(16), ylab = "v")
        mtext("u", side = 1, line = 2)
        contour(P, nlevels =input$nlevel, add = TRUE)
        title(main = paste("Elliptical Copula:",input$no4,"\nrho = ",input$alpha," nu = ",input$nu,"\nN:",input$N," Level of Contours:",input$nlevel))}
      
      else {
        P=pellipticalCopula(u = input$N, v = input$N, rho =input$alpha, param =c(input$r,input$s) , type =input$no4,
                            output = "list", border = TRUE)
        image(P, col = heat.colors(16), ylab = "v")
        mtext("u", side = 1, line = 2)
        contour(P, nlevels =input$nlevel, add = TRUE)
        title(main = paste("Elliptical Copula:",input$no4,"\nrho = ",input$alpha," s = ",input$s,"\nN:",input$N," Level of Contours:",input$nlevel))}
    }
    
    
    ##Extreme Value Copula
    
    if (input$types=="Extreme Value Copula"&& input$plots =="2"&&input$plot2=="1" ){
      if (input$no3=="gumbel"||input$no3=="galambos"||input$no3=="husler.reiss"){
        uv = grid2d(x = (0:input$N)/input$N)
        P=pevCopula(u =uv,v=uv, param =input$alpha, type =input$no3,
                    output = "list",alternative=FALSE)
        image(P, col = heat.colors(16) )
        contour(P, nlevels =input$nlevel, add = TRUE)
        title(main = paste("Extreme Value Copula:",input$no3,"\ndelta = ",input$alpha,"\nN:",input$N," Level of Contours:",input$nlevel))}
      
      else if (input$no3=="tawn"){
        uv = grid2d(x = (0:input$N)/input$N)
        P=pevCopula(u =uv,v=uv, param =c(input$alpha,input$beta,input$r), type =input$no3,
                    output = "list",alternative=FALSE)
        image(P, col = heat.colors(16) )
        contour(P, nlevels = input$nlevel, add = TRUE)
        title(main = paste("Extreme Value Copula:",input$no3,"\nalpha = ",input$alpha," beta = ",input$beta," r = ",input$r,
                           "\nN:",input$N," Level of Contours:",input$nlevel))}
      
      else{
        uv = grid2d(x = (0:input$N)/input$N)
        P=pevCopula(u =uv,v=uv, param =c(input$alpha,input$theta), type =input$no3,
                    output = "list",alternative=FALSE)
        image(P, col = heat.colors(16) )
        contour(P, nlevels = input$nlevel, add = TRUE)
        title(main = paste("Extreme Value Copula:",input$no3,"\ndelta = ",input$alpha," theta = ",input$theta,
                           "\nN:",input$N," Level of Contours:",input$nlevel))}
      
    }
    
    
    ######################Perspective
    
    ##Archimedean Copula
    
    if (input$types=="Archimedean Copula"&& input$plots =="2"&&input$plot2=="2" ){
      uv=grid2d(x = (0:input$N)/input$N)
      P=tryCatch(parchmCopula(u =uv, v = uv, alpha =input$alpha, type = as.character(input$no),output = "list"),
                 error = function(e){
                   if (input$no== 2 || input$no==4 || input$no==6 || input$no==8 || input$no==12 || input$no==14 || input$no==15 || input$no==21)
                   {parchmCopula(u =uv, v = uv, alpha =5, type = as.character(input$no),output = "list")}
                   else if (input$no== 3)
                   {parchmCopula(u =uv, v = uv, alpha =0, type = as.character(input$no),output = "list")}
                   else if((input$no== 7 || input$no==9 || input$no==10 || input$no==22 ) )
                   {parchmCopula(u =uv, v = uv, alpha =0.5, type = as.character(input$no),output = "list")}
                   else if((input$no== 13 || input$no==16 || input$no==19 || input$no==20 ))
                   {parchmCopula(u =uv, v = uv, alpha =5, type = as.character(input$no),output = "list")}
                   else if(input$no== 11 )
                   {parchmCopula(u =uv, v = uv, alpha =0.2, type = as.character(input$no),output = "list")}
                   else if(input$no== 18 )
                   {parchmCopula(u =uv, v = uv, alpha =6, type = as.character(input$no),output = "list")}
                   else if ((input$no== 5 || input$no==17 ))
                   {parchmCopula(u =uv, v = uv, alpha =0, type = as.character(input$no),output = "list")}
                   else (parchmCopula(u =uv, v = uv, alpha =4.5, type = as.character(input$no),output = "list"))
                 })
      persp(P, theta =input$theta_plot, phi =input$phi, col = "steelblue", shade = 0.5,
            ticktype = "detailed", cex = 0.5, xlab = "u", ylab = "v",
            zlab = "C(u,v)" )
      title(main = paste("Archimedean Copula No:",input$no,Names[input$no],"\nalpha = ",input$alpha,
                         "\nN:",input$N," phi:",input$phi," theta:",input$theta_plot))}
    
    
    ##Elliptical Copula
    
    if (input$types=="Elliptical Copula"&& input$plots =="2"&&input$plot2=="2" ){
      if (input$no4=="norm"||input$no4=="logistic"){
        P=pellipticalCopula(u = input$N, v = input$N, rho =input$alpha, param = NULL, type =input$no4,
                            output = "list", border = TRUE)
        persp(P, theta = input$theta_plot, phi =input$phi, col = "steelblue", shade = 0.5,
              ticktype = "detailed", cex = 0.5, xlab = "u", ylab = "v",
              zlab = "C(u, v)", xlim = c(0, 1), ylim = c(0, 1), zlim = c(0, 1) )
        title(main = paste("Elliptical Copula:",input$no4,"\nrho = ",input$alpha,
                           "\nN:",input$N," phi:",input$phi," theta:",input$theta_plot))}
      
      else if (input$no4=="t"){
        P=pellipticalCopula(u = input$N, v = input$N, rho =input$alpha, param =input$nu, type =input$no4,
                            output = "list", border = TRUE)
        persp(P, theta = input$theta_plot, phi =input$phi, col = "steelblue", shade = 0.5,
              ticktype = "detailed", cex = 0.5, xlab = "u", ylab = "v",
              zlab = "C(u, v)", xlim = c(0, 1), ylim = c(0, 1), zlim = c(0, 1) )
        title(main = paste("Elliptical Copula:",input$no4,"\nrho = ",input$alpha," nu = ",input$nu,
                           "\nN:",input$N," phi:",input$phi," theta:",input$theta_plot))}
      
      else {
        P=pellipticalCopula(u = input$N, v = input$N, rho =input$alpha, param =c(input$r,input$s) , type =input$no4,
                            output = "list", border = TRUE)
        persp(P, theta = input$theta_plot, phi =input$phi, col = "steelblue", shade = 0.5,
              ticktype = "detailed", cex = 0.5, xlab = "u", ylab = "v",
              zlab = "C(u, v)", xlim = c(0, 1), ylim = c(0, 1), zlim = c(0, 1) )
        title(main = paste("Elliptical Copula:",input$no4,"\nrho = ",input$alpha," s = ",input$s,
                           "\nN:",input$N," phi:",input$phi," theta:",input$theta_plot))}
    }
    
    
    ##Extreme Value Copula
    
    if (input$types=="Extreme Value Copula"&& input$plots =="2"&&input$plot2=="2" ){
      if (input$no3=="gumbel"||input$no3=="galambos"||input$no3=="husler.reiss"){
        uv = grid2d(x = (0:input$N)/input$N)
        P=pevCopula(u =uv,v=uv, param =input$alpha, type =input$no3,
                    output = "list",alternative=FALSE)
        persp(P, theta =input$theta_plot, phi =input$phi, col = "steelblue", shade = 0.5,ticktype = "detailed", cex = 0.5)
        title(main = paste("Extreme Value Copula:",input$no3,"\ndelta = ",input$alpha,
                           "\nN:",input$N," phi:",input$phi," theta:",input$theta_plot))}
      
      else if (input$no3=="tawn"){
        uv = grid2d(x = (0:input$N)/input$N)
        P=pevCopula(u =uv,v=uv, param =c(input$alpha,input$beta,input$r), type =input$no3,
                    output = "list",alternative=FALSE)
        persp(P, theta =input$theta_plot, phi =input$phi, col = "steelblue", shade = 0.5,ticktype = "detailed", cex = 0.5)
        title(main = paste("Extreme Value Copula:",input$no3,"\nalpha = ",input$alpha," beta = ",input$beta," r = ",input$r,
                           "\nN:",input$N," phi:",input$phi," theta:",input$theta_plot))}
      
      else{
        uv = grid2d(x = (0:input$N)/input$N)
        P=pevCopula(u =uv,v=uv, param =c(input$alpha,input$theta), type =input$no3,
                    output = "list",alternative=FALSE)
        persp(P, theta =input$theta_plot, phi =input$phi, col = "steelblue", shade = 0.5,ticktype = "detailed", cex = 0.5)
        title(main = paste("Extreme Value Copula:",input$no3,"\ndelta = ",input$alpha," theta = ",input$theta,
                           "\nN:",input$N," phi:",input$phi," theta:",input$theta_plot))}
      
    }
    
    
    
    #######################################Density###################################
    #################################Contour
    
    ##Archimedean Copula
    
    if (input$types=="Archimedean Copula"&& input$plots =="3"&&input$plot2=="1" ){
      uv = grid2d(x = (1:(input$N-1)/input$N))
      
      D=tryCatch(darchmCopula(u = uv, v = uv, alpha = input$alpha, type =as.character(input$no),output ="list", alternative = FALSE),
                 error = function(e){
                   if (input$no== 2 || input$no==4 || input$no==6 || input$no==8 || input$no==12 || input$no==14 || input$no==15 || input$no==21)
                   {darchmCopula(u = uv, v = uv, alpha = 5, type =as.character(input$no),output ="list", alternative = FALSE)}
                   else if (input$no== 3)
                   {darchmCopula(u = uv, v = uv, alpha = 0.5, type =as.character(input$no),output ="list", alternative = FALSE)}
                   else if((input$no== 7 || input$no==9 || input$no==10 || input$no==22 ) )
                   {darchmCopula(u = uv, v = uv, alpha =0.5, type =as.character(input$no),output ="list", alternative = FALSE)}
                   else if((input$no== 13 || input$no==16 || input$no==19 || input$no==20 ))
                   {darchmCopula(u = uv, v = uv, alpha = 5, type =as.character(input$no),output ="list", alternative = FALSE)}
                   else if(input$no== 11 )
                   {darchmCopula(u = uv, v = uv, alpha = 0.2, type =as.character(input$no),output ="list", alternative = FALSE)}
                   else if(input$no== 18 )
                   {darchmCopula(u = uv, v = uv, alpha = 5, type =as.character(input$no),output ="list", alternative = FALSE)}
                   else if ((input$no== 5 || input$no==17 ))
                   {parchmCopula(u =uv, v = uv, alpha =0, type = as.character(input$no),output = "list")}
                   else (darchmCopula(u = uv, v = uv, alpha =4.5, type =as.character(input$no),output ="list", alternative = FALSE))
                 })
      
      if (input$no==3 && input$alpha==0){
        title(main = paste("There is no known Archimedean Copula  for No:",input$no,Names[input$no],"with alpha = 0"))}
      else{
        image(D, xlim = c(0, 1), ylim = c(0,1), col = heat.colors(16))
        contour(D, xlab = "u", ylab = "v", nlevels = input$nlevel, add = TRUE)
        title(main = paste("Archimedean Copula No:",input$no,Names[input$no],
                           "\nalpha = ", input$alpha,"\nN:",input$N," Level of Contours:",input$nlevel))}
    }
    
    
    ##Elliptical Copula
    
    if (input$types=="Elliptical Copula"&& input$plots =="3"&&input$plot2=="1" ){
      
      if (input$no4=="norm"||input$no4=="logistic"){
        D=dellipticalCopula(u = input$N, v = input$N, rho =input$alpha, param = NULL, type =input$no4,
                            output = "list", border = TRUE)
        image(D, col = heat.colors(16), ylab = "v", xlim = c(0,1), ylim = c(0,1) )
        mtext("u", side = 1, line = 2)
        contour(D, nlevels = input$nlevel, add = TRUE)
        title(main = paste("Elliptical Copula:",input$no4,"\nrho = ",input$alpha,"\nN:",input$N," Level of Contours:",input$nlevel))}
      
      else if (input$no4=="t"){
        D=dellipticalCopula(u = input$N, v = input$N, rho =input$alpha, param =input$nu, type =input$no4,
                            output = "list", border = TRUE)
        image(D, col = heat.colors(16), ylab = "v",xlim = c(0,1), ylim = c(0,1) )
        mtext("u", side = 1, line = 2)
        contour(D, nlevels =input$nlevel, add = TRUE)
        title(main = paste("Elliptical Copula:",input$no4,"\nrho = ",input$alpha," nu = ",input$nu,
                           "\nN:",input$N," Level of Contours:",input$nlevel))}
      
      else {
        D=dellipticalCopula(u = input$N, v = input$N, rho =input$alpha, param =c(input$r,input$s) , type =input$no4,
                            output = "list", border = TRUE)
        image(D, col = heat.colors(16), ylab = "v", xlim = c(0,1), ylim = c(0,1) )
        mtext("u", side = 1, line = 2)
        contour(D, nlevels = input$nlevel, add = TRUE)
        title(main = paste("Elliptical Copula:",input$no4,"\nrho = ",input$alpha," s = ",input$s,
                           "\nN:",input$N," Level of Contours:",input$nlevel))}
    }
    
    
    ##Extreme Value Copula
    if (input$types=="Extreme Value Copula"&& input$plots =="3"&&input$plot2=="1" ){
      
      if (input$no3=="gumbel"||input$no3=="galambos"||input$no3=="husler.reiss"){
        uv = grid2d(x = (1:input$N-1)/input$N)
        D=devCopula(u =uv,v=uv, param =input$alpha, type =input$no3,
                    output = "list",alternative=FALSE)
        image(D, col = heat.colors(16) )
        contour(D, nlevels = input$nlevel, add = TRUE)
        title(main = paste("Extreme Value Copula:",input$no3,"\ndelta = ",input$alpha,
                           "\nN:",input$N," Level of Contours:",input$nlevel))}
      
      else if (input$no3=="tawn"){
        uv = grid2d(x = (1:input$N-1)/input$N)
        D=devCopula(u =uv,v=uv, param =c(input$alpha,input$beta,input$r), type =input$no3,
                    output = "list",alternative=FALSE)
        image(D, col = heat.colors(16) )
        contour(D, nlevels = input$nlevel, add = TRUE)
        title(main = paste("Extreme Value Copula:",input$no3,"\nalpha = ",input$alpha," beta = ",input$beta," r = ",input$r,
                           "\nN:",input$N," Level of Contours:",input$nlevel))}
      
      else{
        uv = grid2d(x = (1:input$N-1)/input$N)
        D=devCopula(u =uv,v=uv, param =c(input$alpha,input$theta), type =input$no3,
                    output = "list",alternative=FALSE)
        image(D, col = heat.colors(16) )
        contour(D, nlevels = input$nlevel, add = TRUE)
        title(main = paste("Extreme Value Copula:",input$no3,"\ndelta = ",input$alpha," theta = ",input$theta,
                           "\nN:",input$N," Level of Contours:",input$nlevel))}
    }
    
    
    
    
    #################################Perspective
    
    ##Archimedean Copula
    
    if (input$types=="Archimedean Copula"&& input$plots =="3"&&input$plot2=="2" ){
      uv = grid2d(x = (1:(input$N-1))/input$N)
      D=darchmCopula(u =uv, v = uv, alpha =input$alpha, type = as.character(input$no),
                     output = "list")
      persp(D, theta =input$theta_plot, phi =input$phi, col = "steelblue", shade = 0.5,
            ticktype = "detailed", cex = 0.5, xlab = "u", ylab = "v",
            zlab = "C(u,v)" )
      title(main = paste("Archimedean Copula No:",input$no,Names[input$no],"\nalpha = ",input$alpha,
                         "\nN:",input$N,"phi = ",input$phi," theta = ",input$theta_plot))
    }
    
    
    ##Elliptical Copula
    
    if (input$types=="Elliptical Copula"&& input$plots =="3"&&input$plot2=="2" ){
      if (input$no4=="norm"||input$no4=="logistic"){
        
        D=dellipticalCopula(u = input$N, v = input$N, rho =input$alpha, param = NULL, type =input$no4,
                            output = "list", border = TRUE)
        Var = var(as.vector(D$z), na.rm = TRUE)
        if (Var < 1.0e-6) {
          Mean = round(1.5*mean(as.vector(D$z), na.rm = TRUE), 2)
          persp(D, theta =input$theta_plot, phi =input$phi, col = "steelblue", shade = 0.5,
                ticktype = "detailed", cex = 0.5, xlab = "u", ylab = "v",
                zlim = c(0, Mean), zlab = "C(u,v)" )
          title(main = paste("Elliptical Copula:",input$no4,"\nrho = ",input$alpha,
                             "\nN:",input$N," phi:",input$phi," theta:",input$theta_plot))
        } else {
          persp(D, theta =input$theta_plot, phi =input$phi, col = "steelblue", shade = 0.5,
                ticktype = "detailed", cex = 0.5, xlab = "u", ylab = "v",
                zlab = "C(u,v)" )
          title(main = paste("Elliptical Copula:",input$no4,"\nrho = ",input$alpha,
                             "\nN:",input$N," phi:",input$phi," theta:",input$theta_plot))
        }
      }
      
      else if (input$no4=="t"){
        D=dellipticalCopula(u = input$N, v = input$N, rho =input$alpha, param =input$nu, type =input$no4,
                            output = "list", border = TRUE)
        Var = var(as.vector(D$z), na.rm = TRUE)
        if (Var < 1.0e-6) {
          Mean = round(1.5*mean(as.vector(D$z), na.rm = TRUE), 2)
          persp(D, theta =input$theta_plot, phi =input$phi, col = "steelblue", shade = 0.5,
                ticktype = "detailed", cex = 0.5, xlab = "u", ylab = "v",
                zlim = c(0, Mean), zlab = "C(u,v)" )
          title(main = paste("Elliptical Copula:",input$no4,"\nrho = ",input$alpha," nu = ",input$nu,
                             "\nN:",input$N," phi:",input$phi," theta:",input$theta_plot))
        } else {
          persp(D, theta =input$theta_plot, phi =input$phi, col = "steelblue", shade = 0.5,
                ticktype = "detailed", cex = 0.5, xlab = "u", ylab = "v",
                zlab = "C(u,v)" )
          title(main = paste("Elliptical Copula:",input$no4,"\nrho = ",input$alpha," nu = ",input$nu,
                             "\nN:",input$N," phi:",input$phi," theta:",input$theta_plot))
        }}
      
      else {
        D=dellipticalCopula(u = input$N, v =input$N, rho =input$alpha, param =c(input$r,input$s) , type =input$no4,
                            output = "list", border = TRUE)
        Var = var(as.vector(D$z), na.rm = TRUE)
        if (Var < 1.0e-6) {
          Mean = round(1.5*mean(as.vector(D$z), na.rm = TRUE), 2)
          persp(D, theta =input$theta_plot, phi =input$phi, col = "steelblue", shade = 0.5,
                ticktype = "detailed", cex = 0.5, xlab = "u", ylab = "v",
                zlim = c(0, Mean), zlab = "C(u,v)" )
          title(main = paste("Elliptical Copula:",input$no4,"\nrho = ",input$alpha," s = ",input$s,
                             "\nN:",input$N," phi:",input$phi," theta:",input$theta_plot))
        } else {
          persp(D, theta =input$theta_plot, phi =input$phi, col = "steelblue", shade = 0.5,
                ticktype = "detailed", cex = 0.5, xlab = "u", ylab = "v",
                zlab = "C(u,v)" )
          title(main = paste("Elliptical Copula:",input$no4,"\nrho = ",input$alpha," s = ",input$s,
                             "\nN:",input$N," phi:",input$phi," theta:",input$theta_plot))
        }}
    }
    
    
    ##Extreme Value Copula
    if (input$types=="Extreme Value Copula"&& input$plots =="3"&&input$plot2=="2" ){
      if (input$no3=="gumbel"||input$no3=="galambos"||input$no3=="husler.reiss"){
        uv = grid2d(x = (0:input$N)/input$N)
        D=devCopula(u =uv,v=uv, param =input$alpha, type =input$no3,
                    output = "list",alternative=FALSE)
        persp(D, theta =input$theta_plot, phi =input$phi, col = "steelblue", shade = 0.5,ticktype = "detailed", cex = 0.5)
        title(main = paste("Extreme Value Copula:",input$no3,"\ndelta = ",input$alpha,
                           "\nN:",input$N," phi:",input$phi," theta:",input$theta_plot))}
      
      else if (input$no3=="tawn"){
        uv = grid2d(x = (0:input$N)/input$N)
        D=devCopula(u =uv,v=uv, param =c(input$alpha,input$beta,input$r), type =input$no3,
                    output = "list",alternative=FALSE)
        persp(D, theta =input$theta_plot, phi =input$phi, col = "steelblue", shade = 0.5,ticktype = "detailed", cex = 0.5)
        title(main = paste("Extreme Value Copula:",input$no3,"\nalpha = ",input$alpha," beta = ",input$beta," r = ",input$r,
                           "\nN:",input$N," phi:",input$phi," theta:",input$theta_plot))}
      
      else{
        uv = grid2d(x = (0:input$N)/input$N)
        D=devCopula(u =uv,v=uv, param =c(input$alpha,input$theta), type =input$no3,
                    output = "list",alternative=FALSE)
        persp(D, theta =input$theta_plot, phi =input$phi, col = "steelblue", shade = 0.5,ticktype = "detailed", cex = 0.5)
        title(main = paste("Extreme Value Copula:",input$no3,"\ndelta = ",input$alpha," theta = ",input$theta,
                           "\nN:",input$N," phi:",input$phi," theta:",input$theta_plot))}
    }
    
  })
  
  
  
  
  
  
  
  #######################################TEXT######################################
  # Change the no of Copula according to the selected name of copula
  observeEvent(input$names, {
    x <- input$names
    updateNumericInput(session, "no", value = x)
  })
  
  #Change the name of copula according to the numeric input 
  observe({
    y <- input$no
    updateSelectInput(session, "names",
                      selected = y)
  })
  
  data <- reactive({
    file1 <- input$file
    if(is.null(file1)){return()} 
    read.table(file=file1$datapath, sep=",", header = input$header, stringsAsFactors = input$stringAsFactors)
  })
  
  #To show the Summary of the input file in a table format
  output$filedf <- renderTable({
    if(is.null(data())){return ()}
    input$file
  })
  
  
  #To show the Structure of the input file
  output$fileob <- renderPrint({
    if(is.null(input$file)){return ()}
    str(input$file)
  })
  
  
  
  
  #this reactive output contains the summary of the dataset and display the summary in table format
  output$sum <- renderTable({
    if(is.null(data())){return ()}
    summary(data())
    
  })
  
  
  # To display the dataset in a table format
  output$table <- renderDataTable({
    if(is.null(data())){return ()}
    data()
  })
  
  
  output$copula<-renderText({
    if(is.null(data())){return ()}
    var_all<-data.frame(pobs(data()))
    var_a <- var_all[,1]
    var_b <- var_all[,2]
    co<- BiCopSelect(var_a, var_b, familyset = NA)
    paste("Number of Observations:",co$nobs,
          "\nCopula Family:",co$family,"-",co$familyname,
          "\nParameter of the copula:",round(co$par,digits=2),
          "\nAIC:",round(co$AIC,digits=2),
          "\nBIC:",round(co$BIC,digits=2),
          "\np-value of the independence test:",round(co$p.value.indeptest,digits=2))
  })
  
  output$gof<-renderText({
    if(is.null(data())){return ()}
    var_all<-data.frame(pobs(data()))
    var_a <- var_all[,1]
    var_b <- var_all[,2]
    co<- BiCopSelect(var_a, var_b, familyset = NA)
    check<-grepl("Tawn",co$familyname)
    if (input$method=="white"){
      if(check==TRUE){
        paste("Copula family not implemented.")
        } else if (co$family %in% c(7, 8, 9, 10, 17, 18, 19, 20, 27, 28, 29, 30, 37, 38, 39, 40)){
        paste("The goodness-of-fit test based on White's information matrix equality is not implemented for the BB copulas.")
        } else if (check==FALSE){
          test<-BiCopGofTest(var_a,var_b,family=co$family,par = co$par,par2 = co$par2,method=input$method)
          paste("Copula Family:",co$familyname,
            "\nMethod of test: White",
            "\nstatistic:",round(test$statistic,digits = 3),
            "\np-value:",test$p.value)}}
    
    else{
      if(check==TRUE){
        paste("Copula family not implemented.")
      } else if (co$family == 2){
        paste("The goodness-of-fit test based on Kendall's process is not implemented for the t-copula.")
      } else if (check==FALSE){
        test<-BiCopGofTest(var_a,var_b,family=co$family,par = co$par,par2 = co$par2,method=input$method)
        paste("Copula Family:",co$familyname,
            "\nMethod of test: Kendall",
            "\nStatistic-CvM:",round(test$statistic.CvM,digits = 3),
            "\nStatistic-KS:",round(test$statistic.KS,digits = 3),
            "\np-value-CvM:",test$p.value.CvM,
            "\np-value-KS:",test$p.value.KS)}}
  })


    output$plot1<-renderPlot({
      if(is.null(data())){return ()}
      var_all<-data.frame(pobs(data()))
      var_a <- var_all[,1]
      var_b <- var_all[,2]
      co<- BiCopSelect(var_a, var_b, familyset = NA)
      obj <-BiCop(family = co$family, par = co$par, par2 = co$par2)
      plot(obj,type="contour",main="Contour Plot")
      })
    
    output$plot2<-renderPlot({
      if(is.null(data())){return ()}
      var_all<-data.frame(pobs(data()))
      var_a <- var_all[,1]
      var_b <- var_all[,2]
      co<- BiCopSelect(var_a, var_b, familyset = NA)
      obj <-BiCop(family = co$family, par = co$par, par2 = co$par2)
      plot(obj,type="surface",main="Density") # surface plot of copula density
      })
}
shinyApp(ui=ui,server=server)