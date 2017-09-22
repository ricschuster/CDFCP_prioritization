output$summary <- renderTable({
  
  test.spf <- cspf()
  #spf <- current.spf(input$range[1],input$range[2],input$iterations) 
  #test.spf <- marxan(spf=spf,puvsp="puvsp.dat")       
  ##################
  ## Get missing values, MPM
  ##################
  test.spf$sums <- test.spf$sums[names(test.spf$sums)]
  
  sumtest.spf <- data.frame(runname=I("a"), Missing_Values=0, MPM=0)
  Cost <- Score <- vector()
  
  for (ff in 2:(length(test.spf$sums))) {
    sumtest.spf[ff,1] <- as.character(names(test.spf$sums)[ff])
    sumtest.spf[ff,2] <- sum(test.spf$sums[[ff]]$Missing_Values)
    sumtest.spf[ff,3] <- sum(test.spf$sums[[ff]]$MPM)
    Cost <- c(Cost,test.spf$sums[[ff]]$Cost)
    Score <- c(Score,test.spf$sums[[ff]]$Score)
    
  }
  sumtest.spf[sumtest.spf$runname!="a",]
})

#output$spf1 <- renderTable({
output$spf1 <- renderTable({
  test.spf <- cspf()  
  test.spf$sums[[2]]
})    

output$spf2 <- renderTable({
  #output$spf2 <- renderTable({
  test.spf <- cspf()  
  test.spf$sums[[3]]
})    

x1 <- tabPanel("Summary", tableOutput("summary"))
x2 <- tabPanel("SPF_1", tableOutput("spf1")) 
x3 <- tabPanel("SPF_2", tableOutput("spf2"))

yy<-list(x1,x2,x3)
output$tabsets <- renderUI({
  #    tabsetPanel(
  #      x1, 
  #      x2, 
  #      x3
  #    )  
  do.call(tabsetPanel, yy)  
})
#    tabsetPanel(
#      tabPanel("Summary", tableOutput("summary")), 
#      tabPanel("SPF_1", tableOutput("spf1")), 
#      tabPanel("SPF_2", tableOutput("spf2"))
#    )  