#################################################################################
# Developer: Benjamin Neely
# Date:      5/10/2015
# Name:      server.R
# Desc:      This is the server.R file for the prediction matrix website.
#################################################################################
#Beging that shiny stuff-----------------------------------------------------------------------------------------------
shinyServer(function(input, output, session) {
  #==========================================
  #Check User Input, if exceeds limits scale 
  #up / back
  #==========================================
  observe({
    if (is.null(input$age)) {
      return(NULL)
    } else {
    for(i in 1:length(upper)){

          if (  eval(parse(text = paste("input$", names(upper)[i], sep="")))
                >
                as.numeric(upper[i])) {
            eval(parse(text = paste0("updateNumericInput(session,\"",names(upper)[i],"\", value = ",
                                     as.numeric(upper[i]),")")))
          } else if (
                eval(parse(text = paste("input$", names(upper)[i], sep="")))
                <
                as.numeric(lower[i])
            ) {
            eval(parse(text = paste0("updateNumericInput(session,\"",names(upper)[i],"\", value = ",
                                     as.numeric(lower[i]),")")))
          }
    }
    }
  })
  #==========================================
  #Create a dynamic data.frame from usr input
  #==========================================
  ## FOR QC PURPOSES ONLY:
  # input <- list(age=70,hxhf="Yes",ap="No",sbp=130,country="USA",ckd="Yes")
  QQ <- reactive({

    if (is.null(input$age)) {
      return(NULL)
    } else {
      QQ <- vector("list",length(betaType))
      for(i in 1:length(betaType)){
          QQ[[ i ]] <- eval(parse(text = paste("input$", names(betaType)[i], sep="")))
          names(QQ[[ i ]]) <- names(betaType)[i]
          }
      cnames <- names(unlist(QQ))
      indx <- which(sapply(QQ, is.character))
      for (z in 1:length(indx)){
        QQ[[indx[z]]] <- unlist(lapply(QQ[indx[z]], checkandconvert ))
      }
      a <- unlist(QQ)
      for (zaz in 1:length(a)) {
        a[zaz] = applyTransformations(a[zaz])
      }
      b <- data.frame(as.list(a),stringsAsFactors=F)
      return(b)
    }
  })
  #==========================================
  #Statistical magic to get risk from coxph
  #==========================================
  x2 <- reactive({

    if (is.null(input$age)) {
      return(NULL)
    } else {
    c                 <- QQ()
    #######################################################################
    #MANUALLY CUREATE TRANSFORMASTIONS HERE (DON"T KNOW HOW TO DO THIS YET)
    #######################################################################
    same              <- intersect(names(c),names(betas))
    same2             <- intersect(names(c),names(referencePoints))

  # this calculation is done in accordance with the PMML Specification
    r                 <- as.matrix(c[same])                 %*% (as.matrix(betas[same]))
    s                 <- t(as.matrix(referencePoints[same2]))  %*% (as.matrix(betas[same2]))
    S_t               <- baseHaz**exp(r-s)
    #S_t               <- exp(-H_t)
    C_t               <- 1-S_t
    final             <- data.frame("Time"=time,
                                    "char_Time"=as.character(time),
                                    "Risk"=C_t)
   ## FOR QC PURPOSES ONLY:
   # x2 <- function() { return(final)}
    #return the predicted risk of the event
    return(final)
    }
  })
  
  #=====================================================================================================================================================
  #=====================================================================================================================================================
  #RENDERING OUTPUT FOR UI
  #=====================================================================================================================================================
  #=====================================================================================================================================================
  #==========================================
  #Dynamic Risk Factors Printed
  #==========================================
  output$Dynamic <- renderUI({

    LL <- vector("list",6)
    
    LL[[1]] <- selectInput(inputId="country", 
                           label="Country of Residence",
                           choices=c("Australia","Austria","Belgium","Brazil","Canada","Chile","Denmark","Spain","Finland","UK","Hungary","Italy",
                                     "Korea","Netherlands","Norway","Thailand","USA"),
                           selected="USA")
    LL[[2]] <- numericInput(inputId="age", 
                            label="Age (yrs. old)", 
                            step=1,
                            min=18, 
                            max=110, 
                            value=55) 
    LL[[3]] <- selectInput(inputId="hxhf", 
                           label="Do you have a history of Heart Failure?",
                           choices=c("Yes","No"),
                           selected="No")
    LL[[4]] <- selectInput(inputId="ckd", 
                           label="Do you have a history of Renal Disfunction (ever have eGFR < 60)?",
                           choices=c("Yes","No"),
                           selected="No")
    LL[[5]] <- numericInput(inputId="sbp", 
                            label="Current Systolic Blood Pressure (mm Hg)", 
                            step=1,
                            min=120, 
                            max=310, 
                            value=150) 
    LL[[6]] <- selectInput(inputId="ap", 
                           label="Are you currently on antiplatelet therapy?",
                           choices=c("Yes","No"),
                           selected="No")
    return(LL) 
  })
  #==========================================
  #Graphics to help with user interpretation
  #==========================================
  output$plot <- renderPlot({
    if (is.null(input$age)) {
      return(NULL)
    } else {
      return(
    ggplot(data=x2()) +
      geom_step(aes(x=Time,y=Risk), direction="hv",size=1, colour="#e41a1c")+
      scale_y_continuous( breaks = seq(0, 1, by = 0.1), limits = c(0,1), labels = percent) +
      scale_x_continuous( breaks = seq(0, max(x2()$Time), by = round(max(x2()$Time)/9,digits=1)), limits = c(0,max(x2()$Time))) +
      theme(panel.grid.major=element_blank(),
            legend.title=element_blank(),
            legend.position="top")+
      theme_bw()+
#       geom_vline(xintercept=isolate(interactiveClickDF()$x),colour="blue",linetype="longdash")+
      ylab("Probability of an Event Occuring")+
      xlab("Days")
      )
    }
})

  #==========================================
  #Text to help with user interpretation
  #==========================================
  output$text2 <- renderText({
    paste("A person with the constellation of risk factors given at the left has a ",
          paste0(formatC(tail(x2()[which(x2()$Time<=320),]$Risk*100,n=1),format="f",digits=2),"%"),
          " chance of experiencing a ",input$model," bleeding event at ", paste0(formatC(tail(320,n=1),format="f",digits=0)),
          " days of follow-up.",
         # "To determine the risk at different points in time, use the mouse and hover over the risk line.",
          sep="")
    })
})