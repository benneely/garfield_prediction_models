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
  # input <- list("stroke"="Yes","age"=60,"bleed"="No","heart_failure"="No","ckd"="Yes","other_region"="No","black"="Afro-Caribean, Mixed Race, Other","oral_anti"="No")
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

    LL <- vector("list",8)
    
    LL[[1]] <- selectInput(inputId="stroke", 
                           label="Medical history of stroke?",
                           choices=c("Yes","No"),
                           selected="No")
    LL[[2]] <- sliderInput(inputId="age", 
                            label="Age (yrs. old)", 
                            step=1,
                            min=19, 
                            max=90, 
                            value=55) 
    LL[[3]] <- selectInput(inputId="bleed", 
                           label="Medical history of bleeding?",
                           choices=c("Yes","No"),
                           selected="No")
    LL[[4]] <- selectInput(inputId="heart_failure", 
                           label="Medical history of heart failure or ejection fraction < 40? (If ejection fraction is not available, use medical history of heart failure only.)",
                           choices=c("Yes","No"),
                           selected="No")
    LL[[5]] <- selectInput(inputId="ckd", 
                           label="Chronic kidney disease levels of III-V.", 
                           choices=c("Yes","No"),
                           selected="No")
    LL[[6]] <- selectInput(inputId="other_region", 
                           label="Australia, Egypt, South Africa?",
                           choices=c("Yes","No"),
                           selected="No")
    LL[[7]] <- selectInput(inputId="black", 
                           label="Race/Ethnicity.",
                           choices=c("Afro-Caribean, Mixed Race, Other","Caucasian, Hispanic/Latino, Asian"),
                           selected="Caucasian, Hispanic/Latino, Asian")
    LL[[8]] <- selectInput(inputId="oral_anti", 
                           label="At enrollment, patient is given or already taking a Vitamin K antagonist or Rivaroxaban or Apixaban or Endoxaban or Dibigatran?",
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
      scale_y_continuous( breaks = seq(0, 0.6, by = 0.1), limits = c(0,0.6), labels = percent) +
      scale_x_continuous( breaks = c(0,30,60,180,365), limits = c(0,370)) +
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
          paste0(formatC(tail(x2()[which(x2()$Time<=365),]$Risk*100,n=1),format="f",digits=2),"%"),
          " chance of experiencing a ",input$model," bleeding event at ", paste0(formatC(tail(365,n=1),format="f",digits=0)),
          " days of follow-up.",
         # "To determine the risk at different points in time, use the mouse and hover over the risk line.",
          sep="")
    })
})