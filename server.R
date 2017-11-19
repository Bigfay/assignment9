#
# This is the server logic of a Shiny web application. You can run the 
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#

library(shiny)
#main_load<-function(){
if(file.exists("workfile.txt"))
{
        wftc<-read.table("workfile.txt")
} else{
        data(mtcars)
        mtcars2<-mtcars
        mtcars2$vs<-as.factor(mtcars2$vs)
        mtcars2$am<-as.factor(mtcars2$am)
        require(gtools)
        require(car)
        #create all possible combinations with the 10 fields
        ftc<-as.data.frame(permutations(2,10,v=c(0,1),repeats.allowed=TRUE))
        #remove first field from fields name because mpg is the response
        fields<-names(mtcars2)[-1]
        #count number of fields though that already known ;-)
        lfields<-length(fields)
        #name ftc's fieds
        names(ftc)<-fields
        #remove first line because of no interest... only zeros
        ftc = ftc[-1,]
        
        library(stringr)
        #require(dplyr)
        sar2<-c()
        sformula<-c()
        sfstat<-c()
        VIF<-c()
        PValue<-c()
        RES<-c()
        NVAR<-c()
        POK<-c()
        RSS<-c()
        R2<-c()
        for (j in 1:1023){
                test<-"lm(data=mtcars2,mpg~"
                for (i in 1:10){
                        if (ftc[j,i]!=0){
                                test<-paste0(test,names(ftc)[i],sep="+")
                        }
                }
                test<-str_sub(test, 1, str_length(test)-1)
                test<-paste0(test,")")
                fit<-eval(parse(text=test))
                
                #getting the lm formula used
                sformula<-append(sformula,test)
                
                #getting r squared
                SR2<-summary(fit)$r.squared
                R2<-append(R2,SR2)
                
                #getting adjusted r squared
                ar2<-summary(fit)$adj.r.squared
                sar2<-append(sar2,ar2)
                
                #get overal p-value
                f <- summary(fit)$fstatistic
                pv<-pf(f[1], f[2], f[3], lower=FALSE)
                PValue<-append(PValue,pv)
                
                #getting the residual standard error
                sig<-summary(fit)$sigma
                RES<-append(RES,sig)
                
                #getting VIF
                r2<-summary(fit)$r.squared
                svif<-(1-r2)^(-1)
                VIF<-append(VIF,svif)
                
                #getting F statistics
                fstat<-summary(fit)$fstatistic[1]
                sfstat<-append(sfstat,fstat)
                
                #sum of element used per combination
                SOE1<-sum(ftc[j,1:10])
                NVAR<-append(NVAR,SOE1)
                
                #check P-Values smaller than .05
                if (max(summary(fit)$coef[,4])<.05){
                        #if (max(summary(fit)$coef[-1,4])<.05){        
                        ROK="OK"}
                else {
                        ROK="NOK"}
                POK<-append(POK,ROK)
                
                #getting residuals (RSS)
                SRSS<-sum(resid(fit)^2)
                RSS<-append(RSS,SRSS)
        }
        ftc<-cbind(ftc,POK)
        ftc<-cbind(ftc,NVAR)
        ftc<-cbind(ftc,sformula)
        ftc<-cbind(ftc,sar2)
        ftc<-cbind(ftc,R2)
        ftc<-cbind(ftc,RES)
        ftc<-cbind(ftc,PValue)
        ftc<-cbind(ftc,VIF)
        ftc<-cbind(ftc,sfstat)
        ftc<-cbind(ftc,RSS)
        #keep only columns involving am and those with an ok P-values
        wftc<-ftc
        #select columns needed
        wftc<-wftc[,12:20]
        write.table(wftc,file="workfile.txt")
}

# Define server logic required to draw a histogram
shinyServer(function(input, output) {
        
        withProgress(message = 'Preparing datas', value = 0, {
                Sys.sleep(.25)
        })
  output$distPlot <- renderPlot({
    
       sftc<-wftc[wftc$NVAR<=input$nvar,c(1,as.numeric(input$indic),2)]
       y<-input$indic
     
       plot(sftc[,1:2],type="p",xlab="Number of used variables")
       observeEvent(input$showhigh,{
               output$text=renderText("Highest Values")
             output$text2=renderPrint(head(sftc[order(sftc[,2],decreasing = TRUE),]))
       })
       observeEvent(input$showlow,{
               output$text=renderText("Lowest Values")
               output$text2=renderPrint(head(sftc[order(sftc[,2],decreasing = FALSE),]))
               
       })

             output$plot_brushinfo <- renderPrint({
                     res <- brushedPoints(sftc, input$plot_brush, colnames(sftc)[1], colnames(sftc)[2])
                     if (nrow(res) == 0)
                             return()
                     cat("You have selected",nrow(res),"points :\n")

                     res
             })
             output$documentation<-renderPrint(cat("documentation:\n",
         "This application allows to get quickly to main indicators for mtcars table.\n",
         "Those indicators are: Adjusted R-Squared,p-value,Variance Inflation Factor, and so on ...\n",
         "They are GREATLY explained in detail in linear regression course ;-)\n",
         "To use this application : \n",
         "1. Select the number of variables you want to explore\n",
         "2. Select the indicator you want to evaluate\n",
         "3. You have the opportunity to show top highest or lowest value from the result your selection produced\n",
         "4. You can graphically select points of interest that will display their values on the right down panel\n",
         "Displayed Values are:\n",
         "1. First field is the record number\n",
         "2. Second field (NVAR)shows the number of variables tested\n",
         "3. Third field is the indicator value\n",
         "4. Fourth field is the formula used indicating the variables involved to get that result\n",
         "Example\n",
         "- If you select 10 variables (via de slider) and choose Adjusted R-Squared\n",
         "- Now after selecting Show Highest Values, you can see that you have amoung the number variables used\n",
         "4,5 and 6 but no 7,8,9 or 10. That means for that indicator 5 variables gets the best value\n",
         "so no need to deal with 10 variables in that case\n",
         " E N J O Y   !!! "))
             
  })
})

