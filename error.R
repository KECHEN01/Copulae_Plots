library(VineCopula)
library(gofCopula)
mydata<-read.csv("mydata2.csv")
var_all<-pobs(mydata)
var_a <- var_all[,1]
var_b <- var_all[,2]
co<- BiCopSelect(var_a, var_b, familyset = NA)
obj <- BiCop(family = co$family, par = co$par, par2 = co$par2)
check<- grepl("Tawn",co$familyname) 
if (check==TRUE)
  paste("check",check)
if (check==FALSE && co$family == 2 && input$method == "kendall")
  paste("The goodness-of-fit test based on Kendall's process is not implemented for the t-copula.")
if(check==FALSE && co$family %in% c(7, 8, 9, 10, 17, 18, 19, 20, 27, 28, 29, 30, 37, 38, 39, 40) && input$method == "white")
  paste("The goodness-of-fit test based on White's information matrix equality is not implemented for the BB copulas.")

