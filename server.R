#source("../../initMlxR.R")  
source("shinymlxTools.R")
library(dataCompareR)
library(gridExtra)
library(ggplot2)
library(mlxR)
library(MESS)
library(plotly)
library(reshape)
library(reshape2)
library(reticulate)
library(RJSONIO)
library(shiny)
library(shinyBS)
library(shinybusy)
library(shinyTime)
library(shinyjs)
server <- shinyServer(function(input, output, session){
  
  output$Llopis <- reactive({
    input$Model == 1
  })
  outputOptions(output, 'Llopis', suspendWhenHidden = FALSE)
  
  output$Revilla <- reactive({
    input$Model == 2 
  })
  outputOptions(output, 'Revilla', suspendWhenHidden = FALSE)
  
  output$Goti <- reactive({
    input$Model == 3 
  })
  outputOptions(output, 'Goti', suspendWhenHidden = FALSE)
  
  output$LD <- reactive({
    input$TypeLD == 1
  })
  outputOptions(output, 'LD', suspendWhenHidden = FALSE)
  
  output$LDWt <- reactive({
    input$TypeLD == 2
  })
  outputOptions(output, 'LDWt', suspendWhenHidden = FALSE)
  
  output$LDCmax <- reactive({
    input$TypeLD == 3
  })
  outputOptions(output, 'LDCmax', suspendWhenHidden = FALSE)
  
  output$value <- reactive({
    input$TypeAMT == 1 
  })
  outputOptions(output, 'value', suspendWhenHidden = FALSE)
  
  output$byweight <- reactive({
    input$TypeAMT == 2 
  })
  outputOptions(output, 'byweight', suspendWhenHidden = FALSE)
  
  output$byauc <- reactive({
    input$TypeAMT == 3 
  })
  outputOptions(output, 'byauc', suspendWhenHidden = FALSE)
  
  output$adjustment <- reactive({
    input$cad == TRUE
  })
  outputOptions(output, 'adjustment', suspendWhenHidden = FALSE)
  
  pkpopLlopis <- reactive({list(
    input$Model, 
    input$Sex, 
    input$Age,
    input$Weight,
    input$Creatinine,
    input$teta1,
    input$teta2,
    input$teta3,
    input$teta4,
    input$teta5)})
  observeEvent(pkpopLlopis(),{
    if(input$Model == 1){
      if(input$Creatinine==0){Creatinine = 1}else{Creatinine = input$Creatinine}
      if (input$Sex == 1){TFG = 0.85*(((140-input$Age)*input$Weight)/(72*Creatinine))} 
      else {TFG = (((140-input$Age)*input$Weight)/(72*Creatinine))}
      TFG = as.numeric(round(TFG,2))
      V_pop1 <- input$teta3*input$Weight
      V_pop2 <- input$teta5*input$Weight
      Cl_pop <- (input$teta1*TFG)+(input$teta2*input$Weight)
      Q <- input$teta4
      output$TFG <- renderValueBox({
        valueBox(
          value = tags$p(round(TFG,2), style = "font-size: 16px",),
          subtitle = tags$p(strong("Clcr (mL/min)"), style = "font-size: 10px",),
          #icon = icon("download")
        )
      })
      output$Vc <- renderValueBox({
        valueBox(
          value = tags$p(round(V_pop1,2), style = "font-size: 16px",),
          subtitle = tags$p(strong("Vc (L)"), style = "font-size: 10px",),
          #icon = icon("download")
        )
      })
      output$Vp <- renderValueBox({
        valueBox(
          value = tags$p(round(V_pop2,2), style = "font-size: 16px",),
          subtitle = tags$p(strong("Vp (L)"), style = "font-size: 10px",),
          #icon = icon("download")
        )
      })
      output$Cl <- renderValueBox({
        valueBox(
          value = tags$p(round(Cl_pop,2), style = "font-size: 16px",),
          subtitle = tags$p(strong("Cl (L/h)"), style = "font-size: 10px",),
          #icon = icon("download")
        )
      })
      output$Q <- renderValueBox({
        valueBox(
          value = tags$p(round(Q,2), style = "font-size: 16px",),
          subtitle = tags$p(strong("Q (L/h)"), style = "font-size: 10px",),
          #icon = icon("download")
        )
      }) 
    }
  })
  
  pkpopRevilla <- reactive({list(
    input$Model, 
    input$Sex, 
    input$Age,
    input$Weight,
    input$Creatinine,
    input$teta11,
    input$teta22,
    input$teta33,
    input$teta44)})
  observeEvent(pkpopRevilla(),{
    if(input$Model == 2){
      if (input$Sex == 1){
        TFG = 0.85*(((140-input$Age)*input$Weight)/(72*input$Creatinine))} 
      else {TFG = (((140-input$Age)*input$Weight)/(72*input$Creatinine))}
      TFG = as.numeric(round(TFG,2))
      if(input$Creatinine > 1){
        A = 1
        V_pop <- (input$teta33*input$teta44^A)*input$Weight
        Cl_pop <- (((input$teta11*TFG)+(input$Age^input$teta22))/1000)*60}
      else{
        A = 0
        V_pop <- (input$teta33*input$teta44^A)*input$Weight
        Cl_pop <- (((input$teta11*TFG)+(input$Age^input$teta22))/1000)*60}
      output$TFG <- renderValueBox({
        valueBox(
          value = tags$p(round(TFG,2), style = "font-size: 16px",),
          subtitle = tags$p(strong("Clcr (mL/min)"), style = "font-size: 10px",),
          #icon = icon("download")
        )
      })
      output$V <- renderValueBox({
        valueBox(
          value = tags$p(round(V_pop,2), style = "font-size: 16px",),
          subtitle = tags$p(strong("V (L)"), style = "font-size: 10px",),
          #icon = icon("download")
        )
      })
      output$ClR <- renderValueBox({
        valueBox(
          value = tags$p(round(Cl_pop,2), style = "font-size: 16px",),
          subtitle = tags$p(strong("Cl (L/h)"), style = "font-size: 10px",),
          #icon = icon("download")
        )
      })
    }
  })
  
  pkpopGoti <- reactive({list(
    input$Model, 
    input$Sex, 
    input$Age,
    input$Weight,
    input$Creatinine,
    input$DIAL,
    input$teta1G,
    input$teta2G,
    input$teta3G,
    input$teta4G,
    input$teta5G)})
  observeEvent(pkpopGoti(),{
    if(input$Model == 3){
      if (input$Age > 65){if(input$Creatinine < 1){Creatinine = 1}else{Creatinine <- input$Creatinine}}else{Creatinine = input$Creatinine}
      if (input$Sex == 1){
        TFG = 0.85*(((140-input$Age)*input$Weight)/(72*Creatinine))
      } else {TFG = (((140-input$Age)*input$Weight)/(72*Creatinine))}
      if(TFG > 150){TFG = 150}else{TFG = as.numeric(round(TFG,2))}
      if (input$DIAL == TRUE){DIAL = 1}else{DIAL = 0}
      V_pop1 <- (input$teta4G*((TFG/70)))*input$teta5G^DIAL
      V_pop2 <- input$V2
      Cl_pop <- (input$teta1G*((TFG/120)^input$teta2G))*input$teta3G^DIAL
      Q <- input$Q
      output$TFG <- renderValueBox({
        valueBox(
          value = tags$p(round(TFG,2), style = "font-size: 16px",),
          subtitle = tags$p(strong("Clcr (mL/min)"), style = "font-size: 10px",),
          #icon = icon("download")
        )
      })
      output$VcG <- renderValueBox({
        valueBox(
          value = tags$p(round(V_pop1,2), style = "font-size: 16px",),
          subtitle = tags$p(strong("Vc (L)"), style = "font-size: 10px",),
          #icon = icon("download")
        )
      })
      output$ClG <- renderValueBox({
        valueBox(
          value = tags$p(round(Cl_pop,2), style = "font-size: 16px",),
          subtitle = tags$p(strong("Cl (L/h)"), style = "font-size: 10px",),
          #icon = icon("download")
        )
      })
    }
  })
  
  LoadingDose <- reactive({
    list(
      input$Model, 
      input$Sex, 
      input$Age,
      input$Weight,
      input$Creatinine,
      input$TypeLD, 
      input$Ld,
      input$LDWt,
      input$Cmax)})
  observeEvent(LoadingDose(),{
    if(input$Model== 1){
      if(input$Creatinine==0){Creatinine = 1}else{Creatinine = input$Creatinine}
      if (input$Sex == 1){TFG = 0.85*(((140-input$Age)*input$Weight)/(72*Creatinine))} 
      else {TFG = (((140-input$Age)*input$Weight)/(72*Creatinine))}
      TFG = as.numeric(round(TFG,2))
      V_pop1 <- input$teta3*input$Weight
      V_pop2 <- input$teta5*input$Weight
      Cl_pop <- (input$teta1*TFG)+(input$teta2*input$Weight)
      Q <- input$teta4 
      k12 = Q/V_pop1
      k21 = Q/V_pop2
      ke = Cl_pop/V_pop1
      a = 1
      b = -(k12 + k21 + ke)
      c = (k21 * ke)
      discriminant <- (b^2)-(4*a*c)
      alpha <- (-b + sqrt(discriminant))/(2*a)
      beta <- (-b - sqrt(discriminant))/(2*a)
      LdCmax =  (input$Cmax/(((k21)/(alpha*beta*V_pop1))-((((alpha-k21)/(alpha*(alpha-beta)*V_pop1)))*exp(-(alpha*1)))-((((k21-beta)/(beta*(alpha-beta)*V_pop1)))*exp(-(beta*1)))))
      Ldwt = input$LDWt*input$Weight
      output$LdCmax <- renderInfoBox({
        infoBox(
          "Dose", paste0(round(LdCmax,0)," mg"), icon = tags$i(class = "fas fa-syringe", style="font-size: 12px"),
          color = "purple"
        )
      })
      output$Ldwt <- renderInfoBox({
        infoBox(
          "Dose", paste0(round(Ldwt,0)," mg"), icon = tags$i(class = "fas fa-syringe", style="font-size: 12px"),
          color = "purple"
        )
      })
    }
    if(input$Model== 2){
      if (input$Sex == 1){
        TFG = 0.85*(((140-input$Age)*input$Weight)/(72*input$Creatinine))
      } else {
        TFG = (((140-input$Age)*input$Weight)/(72*input$Creatinine))
      }
      TFG = as.numeric(round(TFG,2))
      if(input$Creatinine > 1){
        A = 1
        V_pop <- (input$teta33*input$teta44^A)*input$Weight
        Cl_pop <- (((input$teta11*TFG)+(input$Age^input$teta22))/1000)*60
      }else{
        A = 0
        V_pop <- (input$teta33*input$teta44^A)*input$Weight
        Cl_pop <- (((input$teta11*TFG)+(input$Age^input$teta22))/1000)*60
      }
      Ke = Cl_pop/V_pop
      LdCmax =  (input$Cmax*Cl_pop*1)/(1-exp(-(Ke*1)))
      Ldwt = input$LDWt*input$Weight
      output$LdCmax <- renderInfoBox({
        infoBox(
          "Dose", paste0(round(LdCmax,0)," mg"), icon = tags$i(class = "fas fa-syringe", style="font-size: 12px"),
          color = "purple"
        )
      })
      output$Ldwt <- renderInfoBox({
        infoBox(
          "Dose", paste0(round(Ldwt,0)," mg"), icon = tags$i(class = "fas fa-syringe", style="font-size: 12px"),
          color = "purple"
        )
      })
    }
    if(input$Model== 3){
      if (input$Age > 65){if(input$Creatinine < 1){Creatinine = 1}else{Creatinine <- input$Creatinine}}else{Creatinine = input$Creatinine}
      if (input$Sex == 1){
        TFG = 0.85*(((140-input$Age)*input$Weight)/(72*Creatinine))
      } else {TFG = (((140-input$Age)*input$Weight)/(72*Creatinine))}
      if(TFG > 150){TFG = 150}else{TFG = as.numeric(round(TFG,2))}
      if (input$DIAL == TRUE){DIAL = 1}else{DIAL = 0}
      V_pop1 <- (input$teta4G*((TFG/70)))*input$teta5G^DIAL
      V_pop2 <- input$V2
      Cl_pop <- (input$teta1G*((TFG/120)^input$teta2G))*input$teta3G^DIAL
      Q <- input$Q
      k12 = Q/V_pop1
      k21 = Q/V_pop2
      ke = Cl_pop/V_pop1
      a = 1
      b = -(k12 + k21 + ke)
      c = (k21 * ke)
      discriminant <- (b^2)-(4*a*c)
      alpha <- (-b + sqrt(discriminant))/(2*a)
      beta <- (-b - sqrt(discriminant))/(2*a)
      LdCmax =  (input$Cmax/(((k21)/(alpha*beta*V_pop1))-((((alpha-k21)/(alpha*(alpha-beta)*V_pop1)))*exp(-(alpha*1)))-((((k21-beta)/(beta*(alpha-beta)*V_pop1)))*exp(-(beta*1)))))
      Ldwt = input$LDWt*input$Weight
      output$LdCmax <- renderInfoBox({
        infoBox(
          "Dose", paste0(round(LdCmax,0)," mg"), icon = tags$i(class = "fas fa-syringe", style="font-size: 12px"),
          color = "purple"
        )
      })
      output$Ldwt <- renderInfoBox({
        infoBox(
          "Dose", paste0(round(Ldwt,0)," mg"), icon = tags$i(class = "fas fa-syringe", style="font-size: 12px"),
          color = "purple"
        )
      })
    }
  })
  
  EmpiricalDose <- reactive({
    list(
      input$Model, 
      input$Sex, 
      input$Age,
      input$Weight,
      input$Creatinine,
      input$TypeAMT, 
      input$dpk,
      input$AUCMIC,
      input$MIC)})
  observeEvent(EmpiricalDose(),{
    if(input$Model== 1){
      if(input$Creatinine==0){Creatinine = 1}else{Creatinine = input$Creatinine}
      if (input$Sex == 1){TFG = 0.85*(((140-input$Age)*input$Weight)/(72*Creatinine))} 
      else {TFG = (((140-input$Age)*input$Weight)/(72*Creatinine))}
      TFG = as.numeric(round(TFG,2))
      V_pop1 <- input$teta3*input$Weight
      V_pop2 <- input$teta5*input$Weight
      Cl_pop <- (input$teta1*TFG)+(input$teta2*input$Weight)
      Q <- input$teta4 
      k12 = Q/V_pop1
      k21 = Q/V_pop2
      ke = Cl_pop/V_pop1
      a = 1
      b = -(k12 + k21 + ke)
      c = (k21 * ke)
      discriminant <- (b^2)-(4*a*c)
      alpha <- (-b + sqrt(discriminant))/(2*a)
      beta <- (-b - sqrt(discriminant))/(2*a)
      Ldwt = input$LDWt*input$Weight
      LdCmax =  (input$Cmax/(((k21)/(alpha*beta*V_pop1))-((((alpha-k21)/(alpha*(alpha-beta)*V_pop1)))*exp(-(alpha*1)))-((((k21-beta)/(beta*(alpha-beta)*V_pop1)))*exp(-(beta*1)))))
      amt1 <- input$dpk*input$Weight
      amt2 <- (((input$AUC)*Cl_pop)/(24/input$ii))
      if(input$TypeLD == 1){
        ld <- list(time=c(0), amount=input$Ld, rate=input$Ld/1)
        if(input$Ld <= 0){t1 = 0} else {t1 = 1}} 
      if(input$TypeLD == 2){
        ld <- list(time=c(0), amount=Ldwt, rate=Ldwt/1)
        if(input$LDWt <= 0){t1 = 0} else {t1 = 1}}
      if(input$TypeLD == 3){
        ld <- list(time=c(0), amount=LdCmax, rate=LdCmax/1)
        if(input$Cmax <= 0){t1 = 0} else {t1 = 1}}
      param.value=c(V_pop1,V_pop2,Cl_pop, input$omega_V1,input$omega_V2,input$omega_Cl, Q, input$a, input$b)
      t.value=seq(0,100,length.out=101)
      t2=(input$ii*(input$ta/input$ii))-1
      if (t2>=t1){
        if(input$cad == TRUE){ad = input$ad} else {ad = amt2}
        t.dose=seq(t1,t2,by=input$ii)
        t.dose2=seq(input$ta,100,by=input$ii)
        adm <- list(time=t.dose, amount=amt2, rate=amt2/input$tinf)
        ad <- list(time=t.dose2, amount=ad, rate=ad/input$tinf)
      } else {adm <- list(time=t1, amount=0)}
      f <- list(name='Cc',time=t.value)
      p <- list(name=c('V_pop1','V_pop2', 'Cl_pop', 'omega_V1','omega_V2','omega_Cl', 'Q', 'a', 'b'), value=param.value)
      g <- list(size=input$nbsim,level='individual')
      res <- simulx( model     = 'model.txt',
                     treatment = list(adm, ld, ad), 
                     group = g,
                     parameter = p,
                     output    = list(f))
      iTDM <- input$Tt
      fTDM <- input$Tt+24
      AUC <- auc(res$Cc$time[iTDM:fTDM], res$Cc$Cc[iTDM:fTDM])
      
      if(input$AUC == AUC){amt2 = amt2}else{amt2 = amt2*(input$AUC/AUC)}

      output$DoseW <- renderInfoBox({
        infoBox(
          "Dose", paste0(round(amt1,0)," mg"), icon = tags$i(class = "fas fa-syringe", style="font-size: 12px"),
          color = "purple"
        )
      })
      output$DoseA <- renderInfoBox({
        infoBox(
          "Dose", paste0(round(amt2,0)," mg"), icon = tags$i(class = "fas fa-syringe", style="font-size: 12px"),
          color = "purple"
        )
      })
    }
    if(input$Model== 2){
      if (input$Sex == 1){
        TFG = 0.85*(((140-input$Age)*input$Weight)/(72*input$Creatinine))
      } else {
        TFG = (((140-input$Age)*input$Weight)/(72*input$Creatinine))
      }
      TFG = as.numeric(round(TFG,2))
      if(input$Creatinine > 1){
        A = 1
        V_pop <- (input$teta33*input$teta44^A)*input$Weight
        Cl_pop <- (((input$teta11*TFG)+(input$Age^input$teta22))/1000)*60
      }else{
        A = 0
        V_pop <- (input$teta33*input$teta44^A)*input$Weight
        Cl_pop <- (((input$teta11*TFG)+(input$Age^input$teta22))/1000)*60
      }
      Ke = Cl_pop/V_pop
      amt1 = input$dpk*input$Weight
      amt2 =  (((input$AUC)*Cl_pop)/(24/input$ii))
      output$DoseW <- renderInfoBox({
        infoBox(
          "Dose", paste0(round(amt1,0)," mg"), icon = tags$i(class = "fas fa-syringe", style="font-size: 12px"),
          color = "purple"
        )
      })
      output$DoseA <- renderInfoBox({
        infoBox(
          "Dose", paste0(round(amt2,0)," mg"), icon = tags$i(class = "fas fa-syringe", style="font-size: 12px"),
          color = "purple"
        )
      })
    }
    if(input$Model== 3){
      if (input$Age > 65){if(input$Creatinine < 1){Creatinine = 1}else{Creatinine <- input$Creatinine}}else{Creatinine = input$Creatinine}
      if (input$Sex == 1){
        TFG = 0.85*(((140-input$Age)*input$Weight)/(72*Creatinine))
      } else {TFG = (((140-input$Age)*input$Weight)/(72*Creatinine))}
      if(TFG > 150){TFG = 150}else{TFG = as.numeric(round(TFG,2))}
      if (input$DIAL == TRUE){DIAL = 1}else{DIAL = 0}
      V_pop1 <- (input$teta4G*((TFG/70)))*input$teta5G^DIAL
      V_pop2 <- input$V2
      Cl_pop <- (input$teta1G*((TFG/120)^input$teta2G))*input$teta3G^DIAL
      Q <- input$Q
      k12 = Q/V_pop1
      k21 = Q/V_pop2
      ke = Cl_pop/V_pop1
      a = 1
      b = -(k12 + k21 + ke)
      c = (k21 * ke)
      discriminant <- (b^2)-(4*a*c)
      alpha <- (-b + sqrt(discriminant))/(2*a)
      beta <- (-b - sqrt(discriminant))/(2*a)
      Ldwt = input$LDWt*input$Weight
      LdCmax =  (input$Cmax/(((k21)/(alpha*beta*V_pop1))-((((alpha-k21)/(alpha*(alpha-beta)*V_pop1)))*exp(-(alpha*1)))-((((k21-beta)/(beta*(alpha-beta)*V_pop1)))*exp(-(beta*1)))))
      amt1 <- input$dpk*input$Weight
      amt2 <- (((input$AUC)*Cl_pop)/(24/input$ii))
      if(input$TypeLD == 1){
        ld <- list(time=c(0), amount=input$Ld, rate=input$Ld/1)
        if(input$Ld <= 0){t1 = 0} else {t1 = 1}} 
      if(input$TypeLD == 2){
        ld <- list(time=c(0), amount=Ldwt, rate=Ldwt/1)
        if(input$LDWt <= 0){t1 = 0} else {t1 = 1}}
      if(input$TypeLD == 3){
        ld <- list(time=c(0), amount=LdCmax, rate=LdCmax/1)
        if(input$Cmax <= 0){t1 = 0} else {t1 = 1}}
      param.value=c(V_pop1, V_pop2, Cl_pop, input$omega_V1G,input$omega_V2G,input$omega_ClG, Q, input$aG, input$bG)
      t.value=seq(0,100,length.out=101)
      t2=(input$ii*(input$ta/input$ii))-1
      if (t2>=t1){
        if(input$cad == TRUE){ad = input$ad} else {ad = amt2}
        t.dose=seq(t1,t2,by=input$ii)
        t.dose2=seq(input$ta,100,by=input$ii)
        adm <- list(time=t.dose, amount=amt2, rate=amt2/input$tinf)
        ad <- list(time=t.dose2, amount=ad, rate=ad/input$tinf)
      } else {adm <- list(time=t1, amount=0)}
      f <- list(name='Cc',time=t.value)
      p <- list(name=c('V_pop1','V_pop2', 'Cl_pop', 'omega_V1G','omega_V2G','omega_ClG', 'Q', 'aG', 'bG'), value=param.value)
      g <- list(size=input$nbsim,level='individual')
      res <- simulx( model     = 'modelG.txt',
                     treatment = list(adm, ld, ad), 
                     group = g,
                     parameter = p,
                     output    = list(f))
      iTDM <- input$Tt
      fTDM <- input$Tt+24
      AUC <- auc(res$Cc$time[iTDM:fTDM], res$Cc$Cc[iTDM:fTDM])
      
      if(input$AUC == AUC){amt2 = amt2}else{amt2 = amt2*(input$AUC/AUC)}
      
      output$DoseW <- renderInfoBox({
        infoBox(
          "Dose", paste0(round(amt1,0)," mg"), icon = tags$i(class = "fas fa-syringe", style="font-size: 12px"),
          color = "purple"
        )
      })
      output$DoseA <- renderInfoBox({
        infoBox(
          "Dose", paste0(round(amt2,0)," mg"), icon = tags$i(class = "fas fa-syringe", style="font-size: 12px"),
          color = "purple"
        )
      })
    }
  })
  
  Plot <- reactive({list(
    input$Llim, 
    input$Mlim, 
    input$Ulim, 
    input$MIC, 
    input$Tt)})
  observeEvent(Plot(),{
    r <- reactive({
      V1 = 1
      V2 = 1
      Cl = 1
      Q = 1
      k12 = Q/V1
      k21 = Q/V2
      ke = Cl/V1
      amt = 0
      param.value=c(V1,V2,Cl,Q)
      t.value=seq(0,100,length.out=101)
      t1=0
      t2=100
      if (t2>=t1){
        t.dose=seq(t1,t2,by=12)
        adm <- list(time=t.dose, amount=amt, rate=amt/1)
      } else {adm <- list(time=t1, amount=0)}
      f <- list(name='Cc',time=t.value)
      p <- list(name=c('V1','V2', 'Cl', 'Q'), value=param.value)
      res <- simulx( model     = 'ModelDummie.txt',
                     treatment = list(adm),
                     parameter = p,
                     output    = list(f))
      iTDM <- input$Tt+1
      fTDM <- input$Tt+24
      AUC <- auc(res$Cc$time[iTDM:fTDM], res$Cc$Cc[iTDM:fTDM])
      AUCMIC <- AUC/input$MIC
      fTMICa <- 0
      for (i in iTDM:fTDM) {
        if(res$Cc$Cc[i] >= input$MIC){
          fTMIC1 <- 1
          fTMICa <- fTMICa + fTMIC1 }
        fTMIC <- (fTMICa/24)*100}    
      CmaxMIC <- max(res$Cc$Cc[iTDM:fTDM])/input$MIC
      output$AUC <- renderValueBox({
        valueBox(
          value = tags$p(round(AUC,2), style = "font-size: 16px",),
          subtitle = tags$p(strong("AUC (mg/L h)"), style = "font-size: 10px",),
          #icon = icon("download")
        )
      })
      
      output$AUCMIC <- renderValueBox({
        valueBox(
          value = tags$p(round(AUCMIC,2), style = "font-size: 16px",),
          subtitle = tags$p(strong("AUC/MIC (h)"), style = "font-size: 10px",),
          #icon = icon("download")
        )
      })
      output$FTMIC <- renderValueBox({
        valueBox(
          value = tags$p(round(fTMIC,2), style = "font-size: 16px",),
          subtitle = tags$p(strong("fT/MIC (%)"), style = "font-size: 10px",),
          #icon = icon("download")
        )
      })
      output$CMAXMIC <- renderValueBox({
        valueBox(
          value = tags$p(round(CmaxMIC,2), style = "font-size: 16px",),
          subtitle = tags$p(strong("Cmax/MIC"), style = "font-size: 10px",),
          #icon = icon("download")
        )
      })

      ro <- res[['Cc']]
    })
    output$plot1 <- renderPlotly({
      withProgress(message = 'Creating plot', detail='Wait...', value = 0.1, expr={
        r=r()
        a <- plot(qr <- prctilemlx(r,band=list(level=input$level,number=4))) + geom_hline(yintercept=input$Llim, linetype="dashed", color = "green") + geom_hline(yintercept=input$Mlim, linetype="dashed", color = "orange") + geom_hline(yintercept=input$Ulim, linetype="dashed", color = "red") + geom_hline(yintercept=input$MIC, linetype="dashed", color = "blue") + geom_vline(xintercept=input$Tt, linetype="dashed", color = "black") + geom_vline(xintercept=input$Tt+24, linetype="dashed", color = "black") + ylab("concentration (mg/L)")
        p <- a + annotate(geom = "point", x = input$tTDM, y = input$TDM, colour = "orange", size = 3) + theme(legend.position="bottom")
        ggplotly(p) %>%
          config(
            displaylogo = FALSE,
            modeBarButtonsToRemove = c("lasso2d","zoomIn2d", "zoomOut2d","pan2d","autoScale2d","zoom2d","hoverClosestCartesian"))
      })
    })
  })
  
  TDM <- reactive({list(input$TDM)})
  observeEvent(TDM(),{
    output$predicted <- renderInfoBox({
      infoBox(
        "Prediction", paste0(NA), icon = tags$i(class = "fas fa-eye", style="font-size: 12px"),
        color = "purple"
      )
    })
    output$accuracy <- renderInfoBox({
      infoBox(
        "Accuracy", paste0(NA), icon = tags$i(class = "fas fa-bullseye", style="font-size: 12px"),
        color = "purple"
      )
    })
    output$bias <- renderInfoBox({
      infoBox(
        "Bias", paste0(NA), icon = tags$i(class = "fas fa-bullseye", style="font-size: 12px"),
        color = "purple"
      )
    })
    output$mse <- renderInfoBox({
      infoBox(
        "MSE", paste0(NA), icon = tags$i(class = "fas fa-bullseye", style="font-size: 12px"),
        color = "purple"
      )
    })
  })
  
  observeEvent(input$test,{
    show_modal_gif(src="https://i.pinimg.com/originals/cc/d4/09/ccd409e4173f80a4f39f69aa29a5e150.gif",
                   text = NULL,height = "200px",
                   width = "100%",
                   modal_size = "s")
    miniconda_path <- function(){Sys.getenv("/home/r/.local/share/r-miniconda", unset = miniconda_path_default())}
    py_run_string(
      "from potentiostat import Potentiostat

port = '/dev/COM3'       # Serial port for potentiostat device
datafile = 'data.txt'       # Output file for time, curr, volt data

test_name = 'cyclic'        # The name of the test to run
curr_range = '100uA'        # The name of the current range [-100uA, +100uA]
sample_rate = 100.0         # The number of samples/second to collect

volt_min = -0.1             # The minimum voltage in the waveform (V)
volt_max =  1.0             # The maximum voltage in the waveform (V)
volt_per_sec = 0.050        # The rate at which to transition from volt_min to volt_max (V/s)
num_cycles = 1              # The number of cycle in the waveform

# Convert parameters to amplitude, offset, period, phase shift for triangle waveform
amplitude = (volt_max - volt_min)/2.0            # Waveform peak amplitude (V) 
offset = (volt_max + volt_min)/2.0               # Waveform offset (V) 
period_ms = int(1000*4*amplitude/volt_per_sec)   # Waveform period in (ms)
shift = 0.0                                      # Waveform phase shift - expressed as [0,1] number
                                         # 0 = no phase shift, 0.5 = 180 deg phase shift, etc.

# Create dictionary of waveform parameters for cyclic voltammetry test
test_param = {
'quietValue' : 0.0,
'quietTime'  : 0,
'amplitude'  : amplitude,
'offset'     : offset,
'period'     : period_ms,
'numCycles'  : num_cycles,
'shift'      : shift,
}

# Create potentiostat object and set current range, sample rate and test parameters
dev = Potentiostat(port)     
dev.set_curr_range(curr_range)   
dev.set_sample_rate(sample_rate)
dev.set_param(test_name,test_param)

# Run cyclic voltammetry test
t,volt,curr = dev.run_test(test_name,display='pbar',filename=datafile)"
    )
    maxC = max(py$curr)
    minC = min(py$curr)
    minCxV = match(c(minC),py$curr)
    maxCxV = match(c(maxC),py$curr)
    OxDataV <- py$volt[502:700]
    OxDataC <- py$curr[502:700]
    ReDataV <- py$volt[1:50]
    ReDataC <- py$curr[1:50]
    Epa <- py$volt[minCxV]
    Epc <- py$volt[maxCxV]
    model1 <- lm(OxDataC ~ OxDataV)
    model2 <- lm(ReDataC ~ ReDataV)
    IpOx = coef(model1)[[2]]*Epa+coef(model1)[[1]]
    IpRe = coef(model2)[[2]]*Epc+coef(model2)[[1]]
    Ipa = minC - IpOx
    Ipc = maxC - IpRe
    delE = Epa - Epc
    Eavg = (Epa+Epc)/2
    pI = abs(Ipa/Ipc)
    CV <- c(Epa, Epc, delE, Eavg, Ipa, Ipc, pI)
    x <- c(Epa, -0.5)
    y <- c(coef(model1)[[2]]*Epa+coef(model1)[[1]], coef(model1)[[2]]*(-0.5)+coef(model1)[[1]])
    x1 <- c(0, Epc)
    y1 <- c(coef(model2)[[2]]*0+coef(model2)[[1]], coef(model2)[[2]]*(Epc)+coef(model2)[[1]])
    x2 <- c(Epc, Epc)
    y2 <- c(maxC, IpRe)
    x3 <- c(Epa, Epa)
    y3 <- c(minC, IpOx)
    CpMB = 7.9580 + (-2.4806*Ipa) + (3.4241*Ipc) + (-6.2627*pI)
    output$CP <- renderText({paste(round(CpMB,2))})
    output$CP2 <- renderText({paste(round(CpMB,2))})
    output$Ipa <- renderText(paste("Ipa :",round(Ipa,2)))
    output$Ipc <- renderText(paste("Ipc :",round(Ipc,2)))
    output$delE <- renderText(paste("delE :",round(delE,2)))
    output$Eavg <- renderText(paste("Eavg :",round(Eavg,2)))
    output$pI <- renderText(paste("pI :",round(pI,2)))
    ## Grafica Potenciostato
    output$distPlot <- renderPlot({
      plot(
        py$volt, 
        py$curr, 
        type="l",
        lwd=2,
        main = "Voltamograma Cíclico",
        ylab="uA",
        xlab="V",
        xlim = rev(range(py$volt)))
      lines(x,y,col="green")
      lines(x1,y1,col="green")
      lines(x2,y2,col="red")
      lines(x3,y3,col="blue")})
    remove_modal_gif()
    CpM = CpMB
  })
  
  observeEvent(input$EstimatePk,{
    # Llopis
    if (input$Model == 1){
      r <- reactive({
        if (input$tinf>input$ii){stop("The infusion time should not exceed the interdose interval")}
        if(input$Creatinine==0){Creatinine = 1}else{Creatinine = input$Creatinine}
        if (input$Sex == 1){TFG = 0.85*(((140-input$Age)*input$Weight)/(72*Creatinine))
        } else {TFG = (((140-input$Age)*input$Weight)/(72*Creatinine))}
        TFG = as.numeric(round(TFG,2))
        V_pop1 <- input$teta3*input$Weight
        V_pop2 <- input$teta5*input$Weight
        Cl_pop <- (input$teta1*TFG)+(input$teta2*input$Weight)
        Q <- input$teta4
        k12 = Q/V_pop1
        k21 = Q/V_pop2
        ke = Cl_pop/V_pop1
        a = 1
        b = -(k12 + k21 + ke)
        c = (k21 * ke)
        discriminant <- (b^2)-(4*a*c)
        alpha <- (-b+sqrt(discriminant))/(2*a)
        beta <- (-b-sqrt(discriminant))/(2*a)
        Ldwt = input$LDWt*input$Weight
        LdCmax =  (input$Cmax/(((k21)/(alpha*beta*V_pop1))-((((alpha-k21)/(alpha*(alpha-beta)*V_pop1)))*exp(-(alpha*1)))-((((k21-beta)/(beta*(alpha-beta)*V_pop1)))*exp(-(beta*1)))))
        if(input$TypeLD == 1){
          ld <- list(time=c(0), amount=input$Ld, rate=input$Ld/1)
          if(input$Ld <= 0){t1 = 0} else {t1 = 1}} 
        if(input$TypeLD == 2){
          ld <- list(time=c(0), amount=Ldwt, rate=Ldwt/1)
          if(input$LDWt <= 0){t1 = 0} else {t1 = 1}}
        if(input$TypeLD == 3){
          ld <- list(time=c(0), amount=LdCmax, rate=LdCmax/1)
          if(input$Cmax <= 0){t1 = 0} else {t1 = 1}}
        if(input$TypeAMT == 1){
          amt <- input$amt}
        if(input$TypeAMT == 2){
          amt <- input$dpk*input$Weight}
        if(input$TypeAMT == 3){
          amt <- (((input$AUC)*Cl_pop)/(24/input$ii))}
        omega_V1 <- (input$omega_V1/100)*V_pop1
        omega_V2 <- (input$omega_V2/100)*V_pop2
        omega_Cl <- (input$omega_Cl/100)*Cl_pop
        param.value=c(V_pop1, V_pop2, Cl_pop, omega_V1, omega_V2, omega_Cl, Q, input$a, input$b)
        t.value=seq(0,100,length.out=101)
        t2=(input$ii*(input$ta/input$ii))-1
        if (t2>=t1){
          if(input$cad == TRUE){ad = input$ad} else {ad = amt}
          t.dose=seq(t1,t2,by=input$ii)
          t.dose2=seq(input$ta,100,by=input$ii)
          adm <- list(time=t.dose, amount=amt, rate=amt/input$tinf)
          ad <- list(time=t.dose2, amount=ad, rate=ad/input$tinf)
        } else {adm <- list(time=t1, amount=0)}
        f <- list(name='Cc',time=t.value)
        p <- list(name=c('V_pop1','V_pop2', 'Cl_pop', 'omega_V1','omega_V2','omega_Cl', 'Q', 'a', 'b'), value=param.value)
        g <- list(size=input$nbsim,level='individual')
        pres0 <- simulx( model     = 'model.txt',
                       treatment = list(adm, ld, ad), 
                       group = g,
                       parameter = p,
                       output    = list(f))
        
        iTDM <- input$Tt
        fTDM <- input$Tt+24
        AUC <- auc(pres0$Cc$time[iTDM:fTDM], pres0$Cc$Cc[iTDM:fTDM])
        if(input$AUC == AUC){amt = amt}else{amt = amt*(input$AUC/AUC)}
        if(input$TypeLD == 1){
          ld <- list(time=c(0), amount=input$Ld, rate=input$Ld/1)
          ldT <- input$Ld
          if(input$Ld <= 0){t1 = 0} else {t1 = 1}} 
        if(input$TypeLD == 2){
          ld <- list(time=c(0), amount=Ldwt, rate=Ldwt/1)
          ldT <- Ldwt
          if(input$LDWt <= 0){t1 = 0} else {t1 = 1}}
        if(input$TypeLD == 3){
          ld <- list(time=c(0), amount=LdCmax, rate=LdCmax/1)
          ldT <- LdCmax
          if(input$Cmax <= 0){t1 = 0} else {t1 = 1}}
        if(input$TypeAMT == 1){
          amt <- input$amt}
        if(input$TypeAMT == 2){
          amt <- input$dpk*input$Weight}
        if(input$TypeAMT == 3){
          amt <- amt}
        omega_V1 <- (input$omega_V1/100)*V_pop1
        omega_V2 <- (input$omega_V2/100)*V_pop2
        omega_Cl <- (input$omega_Cl/100)*Cl_pop
        param.value=c(V_pop1,V_pop2,Cl_pop, omega_V1, omega_V2, omega_Cl, Q, input$a, input$b)
        t.value=seq(0,100,length.out=101)
        t2=(input$ii*(input$ta/input$ii))-1
        if (t2>=t1){
          if(input$cad == TRUE){ad = input$ad} else {ad = amt}
          t.dose=seq(t1,t2,by=input$ii)
          t.dose2=seq(input$ta,100,by=input$ii)
          adm <- list(time=t.dose, amount=amt, rate=amt/input$tinf)
          ad <- list(time=t.dose2, amount=ad, rate=ad/input$tinf)
        } else {adm <- list(time=t1, amount=0)}
        f <- list(name='Cc',time=t.value)
        p <- list(name=c('V_pop1','V_pop2', 'Cl_pop', 'omega_V1','omega_V2','omega_Cl', 'Q', 'a', 'b'), value=param.value)
        g <- list(size=input$nbsim,level='individual')
        res <- simulx( model     = 'model.txt',
                       treatment = list(adm, ld, ad), 
                       group = g,
                       parameter = p,
                       output    = list(f))
        
        iTDM <- input$Tt
        fTDM <- input$Tt+24
        iTDM1 <- input$Tt
        fTDM1 <- input$Tt+24
        iTDM2 <- input$Tt
        fTDM2 <- input$Tt+24
        AUC <- auc(res$Cc$time[iTDM:fTDM], res$Cc$Cc[iTDM:fTDM])
        
        for (i in c(fTDM,fTDM-12,fTDM-13,fTDM-14,fTDM-15,fTDM-16,fTDM-17,fTDM-18,fTDM-19,fTDM-20,fTDM-21,fTDM-22,fTDM-23,fTDM-24)) {
          if(res$Cc$Cc[i] >= input$MIC){
            iTDM1 = i
            if(iTDM1 == iTDM){iTDM1 = i}else{iTDM1 = i-1}
          }
        }
        for (i in c(iTDM,iTDM+1,iTDM+2,iTDM+3,iTDM+4,iTDM+5,iTDM-6,iTDM+7,iTDM+8,iTDM+9,iTDM+10,iTDM+11,iTDM+12)) {
          if(res$Cc$Cc[i] >= input$MIC){
            fTDM1 = i
            if(fTDM1 == fTDM-12){fTDM1 = i}else{fTDM1 = i-1}
          }
        }
        AUCMIC1 <- auc(res$Cc$time[iTDM1:fTDM1]-input$MIC, res$Cc$Cc[iTDM1:fTDM1]-input$MIC)
      
        for (i in c(fTDM,fTDM-1,fTDM-2,fTDM-3,fTDM-4,fTDM-5,fTDM-6,fTDM-7,fTDM-8,fTDM-9,fTDM-10,fTDM-11,fTDM-12)) {
          if(res$Cc$Cc[i] >= input$MIC){
            iTDM2 = i
            if(iTDM2 == iTDM+12){iTDM2 = i}else{iTDM2 = i-1}
          }
        }
        for (i in c(iTDM,iTDM+12,iTDM+13,iTDM+14,iTDM+15,iTDM+16,iTDM-7,iTDM+18,iTDM+19,iTDM+20,iTDM+21,iTDM+22,iTDM+23,iTDM+24)) {
          if(res$Cc$Cc[i] >= input$MIC){
            fTDM2 = i
            if(fTDM2 == fTDM){fTDM2 = i}else{fTDM2 = i-1}
          }
        }
        
        AUCMIC2 <- auc(res$Cc$time[iTDM2:fTDM2]-input$MIC, res$Cc$Cc[iTDM2:fTDM2]-input$MIC)
        
        AUCMIC <- AUCMIC2 + AUCMIC1
        
        fTMICa <- 0
        iTDMfT <- iTDM+1
        for (i in iTDMfT:fTDM) {
          if(res$Cc$Cc[i] >= input$MIC){
            fTMIC1 <- 1
            fTMICa <- fTMICa + fTMIC1 }
          fTMIC <- (fTMICa/24)*100}    
        CmaxMIC <- max(res$Cc$Cc[iTDM:fTDM])/input$MIC
        output$DoseA <- renderInfoBox({
          infoBox(
            "Dose", paste0(round(amt,0)," mg"), icon = tags$i(class = "fas fa-syringe", style="font-size: 12px"),
            color = "purple"
          )
        })
        output$AUC <- renderValueBox({
          valueBox(
            value = tags$p(round(AUC,0), style = "font-size: 16px",),
            subtitle = tags$p(strong("AUC (mg/L h)"), style = "font-size: 10px",),
            #icon = icon("download")
          )
        })
        output$AUCMIC <- renderValueBox({
          valueBox(
            value = tags$p(round(AUCMIC,0), style = "font-size: 16px",),
            subtitle = tags$p(strong("AUC/MIC (h)"), style = "font-size: 10px",),
            #icon = icon("download")
          )
        })
        output$FTMIC <- renderValueBox({
          valueBox(
            value = tags$p(round(fTMIC,0), style = "font-size: 16px",),
            subtitle = tags$p(strong("fT/MIC (%)"), style = "font-size: 10px",),
            #icon = icon("download")
          )
        })
        output$CMAXMIC <- renderValueBox({
          valueBox(
            value = tags$p(round(CmaxMIC,0), style = "font-size: 16px",),
            subtitle = tags$p(strong("Cmax/MIC"), style = "font-size: 10px",),
            #icon = icon("download")
          )
        })
        
        tdm <- matrix(ncol=1, nrow=99)
        for (i in 0:99) {
          tdmv = 100*i+((input$tTDM+1)+i)
          tdm[i] <- res$Cc$Cc[tdmv]
        }
        PrD <- median(tdm)
        print(PrD)
        
        table = matrix(
          c(input$Sex, 
            input$Age, 
            input$Weight, 
            input$Creatinine, 
            input$ii, 
            input$tinf,
            round(ldT,0),
            round(amt,0),
            round(TFG,2), 
            round(V_pop1,2), 
            round(V_pop2,2), 
            round(Cl_pop,2), 
            Q, 
            input$tTDM,
            input$TDM,
            round(PrD,2), 
            round(PrD/input$TDM,2), 
            round((abs(PrD-input$TDM)/input$TDM)*100,2),
            if(input$TDM == 0){"Inf"}else{round((PrD-input$TDM)^2,2)}
          ),
          nrow=1,
          ncol=19,
          byrow = TRUE)
        dimnames(table) = list(c("Value"),c(
          "Sex", 
          "Age", 
          "Weight", 
          "Creatinine", 
          "Interdose interval", 
          "Infusion time", 
          "Loading dose",
          "Amount",
          "TFGe", 
          "Vc", 
          "Vp", 
          "Cl", 
          "Q", 
          "Time TDM",
          "TDM",
          "Predicted", 
          "Accuracy", 
          "Bias", 
          "MSE"))
        output$downloadData <- downloadHandler(
          filename = function() {
            paste("VanPOC", ".csv", sep = "")},
          content = function(file) {
            write.csv(table, file, row.names = FALSE)})
        
        output$predicted <- renderInfoBox({
          infoBox(
            "Prediction", paste0(round(PrD,2)," mg/L"), icon = tags$i(class = "fas fa-eye", style="font-size: 12px"),
            color = "purple"
          )
        })
        output$accuracy <- renderInfoBox({
          infoBox(
            "Accuracy", paste0(round(PrD/input$TDM,2)), icon = tags$i(class = "fas fa-bullseye", style="font-size: 12px"),
            color = "purple"
          )
        })
        output$bias <- renderInfoBox({
          infoBox(
            "Bias", paste0(round((abs(PrD-input$TDM)/input$TDM)*100,2),"%"), icon = tags$i(class = "fas fa-bullseye", style="font-size: 12px"),
            color = "purple"
          )
        })
        output$mse <- renderInfoBox({
          infoBox(
            "MSE", paste0(if (input$TDM==0){"Inf"}else{round((PrD-input$TDM)^2,2)}), icon = tags$i(class = "fas fa-bullseye", style="font-size: 12px"),
            color = "purple"
          )
        })
        
        output$DATA <- reactive({
          AUC > 0
        })
        outputOptions(output, 'DATA', suspendWhenHidden = FALSE)
        
        ro <- res[['Cc']]
      })
      output$plot1 <- renderPlotly({
        withProgress(message = 'Creating plot', detail='Wait...', value = 0.1, expr={
          r=r()
          a <- plot(qr <- prctilemlx(r,band=list(level=input$level,number=4))) + geom_hline(yintercept=input$Llim, linetype="dashed", color = "green") + geom_hline(yintercept=input$Mlim, linetype="dashed", color = "orange") + geom_hline(yintercept=input$Ulim, linetype="dashed", color = "red") + geom_hline(yintercept=input$MIC, linetype="dashed", color = "blue") + geom_vline(xintercept=input$Tt, linetype="dashed", color = "black") + geom_vline(xintercept=input$Tt+24, linetype="dashed", color = "black") + geom_vline(xintercept=input$tTDM, linetype="dashed", color = "purple") + ylab("concentration (mg/L)")
          p <- a + annotate(geom = "point", x = input$tTDM, y = input$TDM, colour = "orange", size = 3) + theme(legend.position="bottom")
          ggplotly(p) %>%
            config(
              displaylogo = FALSE,
              modeBarButtonsToRemove = c("lasso2d","zoomIn2d", "zoomOut2d","pan2d","autoScale2d","zoom2d","hoverClosestCartesian"))
        })
      }) 
    }
    # Revilla  
    if (input$Model == 2){
      r <- reactive({  
        if (input$tinf>input$ii){stop("The infusion time should not exceed the interdose interval")}
        if (input$Sex == 1){
          TFG = 0.85*(((140-input$Age)*input$Weight)/(72*input$Creatinine))
        } else {
          TFG = (((140-input$Age)*input$Weight)/(72*input$Creatinine))
        }
        TFG = as.numeric(round(TFG,2))
        if(input$Creatinine > 1){
          A = 1
          V_pop <- (input$teta33*input$teta44^A)*input$Weight
          Cl_pop <- (((input$teta11*TFG)+(input$Age^input$teta22))/1000)*60
        }else{
          A = 0
          V_pop <- (input$teta33*input$teta44^A)*input$Weight
          Cl_pop <- (((input$teta11*TFG)+(input$Age^input$teta22))/1000)*60
        }
        Ke = Cl_pop/V_pop
        LdCmax =  (input$Cmax*Cl_pop*1)/(1-exp(-(Ke*1)))
        Ldwt = input$LDWt*input$Weight
        if(input$TypeLD == 1){
          ld <- list(time=c(0), amount=input$Ld, rate=input$Ld/1)
          if(input$Ld <= 0){t1 = 0} else {t1 = 1}} 
        if(input$TypeLD == 2){
          ld <- list(time=c(0), amount=Ldwt, rate=Ldwt/1)
          if(input$LDWt <= 0){t1 = 0} else {t1 = 1}}
        if(input$TypeLD == 3){
          ld <- list(time=c(0), amount=LdCmax, rate=LdCmax/1)
          if(input$Cmax <= 0){t1 = 0} else {t1 = 1}}
        if(input$TypeAMT == 1){
          amt <- input$amt}
        if(input$TypeAMT == 2){
          amt <- input$dpk*input$Weight}
        if(input$TypeAMT == 3){
          amt <- (((input$AUC)*Cl_pop)/(24/input$ii))}
        omega_V <- (input$omega_V/100)*V_pop
        omega_ClR <- (input$omega_ClR/100)*Cl_pop
        param.value=c(V_pop, Cl_pop, omega_V, omega_ClR, input$c)
        t.value=seq(0,100,length.out=101)
        t2=(input$ii*(input$ta/input$ii))-1
        if (t2>=t1){
          if(input$cad == TRUE){ ad = input$ad}else{ad = amt}
          t.dose=seq(t1,t2,by=input$ii)
          t.dose2=seq(input$ta,100,by=input$ii)
          adm <- list(time=t.dose, amount=amt, rate=amt/input$tinf)
          ad <- list(time=t.dose2, amount=ad, rate=ad/input$tinf)
        }else{adm <- list(time=t1, amount=0)}
        f <- list(name='Cc',time=t.value)
        p <- list(name=c('V_pop', 'Cl_pop', 'omega_V','omega_ClR', 'c'), value=param.value)
        g <- list(size=input$nbsim,level='individual')
        pres0 <- simulx( model     = 'modelRevilla.txt',
                       treatment = list(adm, ld, ad), 
                       group = g,
                       parameter = p,
                       output    = list(f))
        iTDM <- input$Tt
        fTDM <- input$Tt+24
        AUC <- auc(pres0$Cc$time[iTDM:fTDM], pres0$Cc$Cc[iTDM:fTDM])
        if(input$AUC == AUC){amt = amt}else{amt = amt*(input$AUC/AUC)}
        if(input$TypeLD == 1){
          ld <- list(time=c(0), amount=input$Ld, rate=input$Ld/1)
          ldT <- input$Ld
          if(input$Ld <= 0){t1 = 0} else {t1 = 1}} 
        if(input$TypeLD == 2){
          ld <- list(time=c(0), amount=Ldwt, rate=Ldwt/1)
          ldT <- Ldwt
          if(input$LDWt <= 0){t1 = 0} else {t1 = 1}}
        if(input$TypeLD == 3){
          ld <- list(time=c(0), amount=LdCmax, rate=LdCmax/1)
          ldT <- LdCmax
          if(input$Cmax <= 0){t1 = 0} else {t1 = 1}}
        if(input$TypeAMT == 1){
          amt <- input$amt}
        if(input$TypeAMT == 2){
          amt <- input$dpk*input$Weight}
        if(input$TypeAMT == 3){
          amt <- amt}
        omega_V <- (input$omega_V/100)*V_pop
        omega_ClR <- (input$omega_ClR/100)*Cl_pop
        param.value=c(V_pop, Cl_pop, omega_V, omega_ClR, input$c)
        t.value=seq(0,100,length.out=101)
        t2=(input$ii*(input$ta/input$ii))-1
        if (t2>=t1){
          if(input$cad == TRUE){ ad = input$ad}else{ad = amt}
          t.dose=seq(t1,t2,by=input$ii)
          t.dose2=seq(input$ta,100,by=input$ii)
          adm <- list(time=t.dose, amount=amt, rate=amt/input$tinf)
          ad <- list(time=t.dose2, amount=ad, rate=ad/input$tinf)
        }else{adm <- list(time=t1, amount=0)}
        f <- list(name='Cc',time=t.value)
        p <- list(name=c('V_pop', 'Cl_pop', 'omega_V','omega_ClR', 'c'), value=param.value)
        g <- list(size=input$nbsim,level='individual')
        res <- simulx( model     = 'modelRevilla.txt',
                       treatment = list(adm, ld, ad), 
                       group = g,
                       parameter = p,
                       output    = list(f))
        iTDM <- input$Tt
        fTDM <- input$Tt+24
        iTDM1 <- input$Tt
        fTDM1 <- input$Tt+24
        iTDM2 <- input$Tt
        fTDM2 <- input$Tt+24
        AUC <- auc(res$Cc$time[iTDM:fTDM], res$Cc$Cc[iTDM:fTDM])
        
        for (i in c(fTDM,fTDM-12,fTDM-13,fTDM-14,fTDM-15,fTDM-16,fTDM-17,fTDM-18,fTDM-19,fTDM-20,fTDM-21,fTDM-22,fTDM-23,fTDM-24)) {
          if(res$Cc$Cc[i] >= input$MIC){
            iTDM1 = i
            if(iTDM1 == iTDM){iTDM1 = i}else{iTDM1 = i-1}
          }
        }
        for (i in c(iTDM,iTDM+1,iTDM+2,iTDM+3,iTDM+4,iTDM+5,iTDM-6,iTDM+7,iTDM+8,iTDM+9,iTDM+10,iTDM+11,iTDM+12)) {
          if(res$Cc$Cc[i] >= input$MIC){
            fTDM1 = i
            if(fTDM1 == fTDM-12){fTDM1 = i}else{fTDM1 = i-1}
          }
        }
        AUCMIC1 <- auc(res$Cc$time[iTDM1:fTDM1]-input$MIC, res$Cc$Cc[iTDM1:fTDM1]-input$MIC)
        
        for (i in c(fTDM,fTDM-1,fTDM-2,fTDM-3,fTDM-4,fTDM-5,fTDM-6,fTDM-7,fTDM-8,fTDM-9,fTDM-10,fTDM-11,fTDM-12)) {
          if(res$Cc$Cc[i] >= input$MIC){
            iTDM2 = i
            if(iTDM2 == iTDM+12){iTDM2 = i}else{iTDM2 = i-1}
          }
        }
        for (i in c(iTDM,iTDM+12,iTDM+13,iTDM+14,iTDM+15,iTDM+16,iTDM-7,iTDM+18,iTDM+19,iTDM+20,iTDM+21,iTDM+22,iTDM+23,iTDM+24)) {
          if(res$Cc$Cc[i] >= input$MIC){
            fTDM2 = i
            if(fTDM2 == fTDM){fTDM2 = i}else{fTDM2 = i-1}
          }
        }
        AUCMIC2 <- auc(res$Cc$time[iTDM2:fTDM2]-input$MIC, res$Cc$Cc[iTDM2:fTDM2]-input$MIC)
        
        AUCMIC <- AUCMIC2 + AUCMIC1
        
        fTMICa <- 0
        iTDMfT <- iTDM+1
        for (i in iTDMfT:fTDM) {
          if(res$Cc$Cc[i] >= input$MIC){
            fTMIC1 <- 1
            fTMICa <- fTMICa + fTMIC1 }
          fTMIC <- (fTMICa/24)*100}    
        CmaxMIC <- max(res$Cc$Cc[iTDM:fTDM])/input$MIC
        
        output$DoseA <- renderInfoBox({
          infoBox(
            "Dose", paste0(round(amt,0)," mg"), icon = tags$i(class = "fas fa-syringe", style="font-size: 12px"),
            color = "purple"
          )
        })
        
        output$AUC <- renderValueBox({
          valueBox(
            value = tags$p(round(AUC,0), style = "font-size: 16px",),
            subtitle = tags$p(strong("AUC (mg/L h)"), style = "font-size: 10px",),
            #icon = icon("download")
          )
        })
        output$AUCMIC <- renderValueBox({
          valueBox(
            value = tags$p(round(AUCMIC,0), style = "font-size: 16px",),
            subtitle = tags$p(strong("AUC/MIC (h)"), style = "font-size: 10px",),
            #icon = icon("download")
          )
        })
        output$FTMIC <- renderValueBox({
          valueBox(
            value = tags$p(round(fTMIC,0), style = "font-size: 16px",),
            subtitle = tags$p(strong("fT/MIC (%)"), style = "font-size: 10px",),
            #icon = icon("download")
          )
        })
        output$CMAXMIC <- renderValueBox({
          valueBox(
            value = tags$p(round(CmaxMIC,0), style = "font-size: 16px",),
            subtitle = tags$p(strong("Cmax/MIC"), style = "font-size: 10px",),
            #icon = icon("download")
          )
        })
        
        tdm <- matrix(ncol=1, nrow=99)
        for (i in 0:99) {
          tdmv = 100*i+((input$tTDM+1)+i)
          tdm[i] <- res$Cc$Cc[tdmv]
        }
        PrD <- median(tdm)
        print(PrD)
        
        table = matrix(
          c(input$Sex, 
            input$Age, 
            input$Weight, 
            input$Creatinine, 
            input$ii, 
            input$tinf,
            round(ldT,0),
            round(amt,0),
            round(TFG,2), 
            round(V_pop,2), 
            round(Cl_pop,2),
            input$tTDM,
            input$TDM,
            round(PrD,2), 
            round(PrD/input$TDM,2), 
            round((abs(PrD-input$TDM)/input$TDM)*100,2),
            if(input$TDM == 0){"Inf"}else{round((PrD-input$TDM)^2,2)}
          ),
          nrow=1,
          ncol=17,
          byrow = TRUE)
        dimnames(table) = list(c("Value"),c(
          "Sex", 
          "Age", 
          "Weight", 
          "Creatinine", 
          "Interdose interval", 
          "Infusion time", 
          "Loading dose",
          "Amount",
          "TFGe", 
          "V", 
          "Cl", 
          "Time TDM",
          "TDM",
          "Predicted", 
          "Accuracy", 
          "Bias", 
          "MSE"))
        output$downloadData <- downloadHandler(
          filename = function() {
            paste("VanPOC", ".csv", sep = "")},
          content = function(file) {
            write.csv(table, file, row.names = FALSE)})
        
        output$predicted <- renderInfoBox({
          infoBox(
            "Prediction", paste0(round(PrD,2)," mg/L"), icon = tags$i(class = "fas fa-eye", style="font-size: 12px"),
            color = "purple"
          )
        })
        output$accuracy <- renderInfoBox({
          infoBox(
            "Accuracy", paste0(round(PrD/input$TDM,2)), icon = tags$i(class = "fas fa-bullseye", style="font-size: 12px"),
            color = "purple"
          )
        })
        output$bias <- renderInfoBox({
          infoBox(
            "Bias", paste0(round((abs(PrD-input$TDM)/input$TDM)*100,2),"%"), icon = tags$i(class = "fas fa-bullseye", style="font-size: 12px"),
            color = "purple"
          )
        })
        output$mse <- renderInfoBox({
          infoBox(
            "MSE", paste0(if (input$TDM==0){"Inf"}else{round((PrD-input$TDM)^2,2)}), icon = tags$i(class = "fas fa-bullseye", style="font-size: 12px"),
            color = "purple"
          )
        })
        
        output$DATA <- reactive({
          AUC > 0
        })
        outputOptions(output, 'DATA', suspendWhenHidden = FALSE)
        
        ro <- res[['Cc']]
      })
      output$plot1 <- renderPlotly({
        withProgress(message = 'Creating plot', detail='Wait...', value = 0.1, expr={
          r=r()
          a <- plot(qr <- prctilemlx(r,band=list(level=input$level,number=4))) + geom_hline(yintercept=input$Llim, linetype="dashed", color = "green") + geom_hline(yintercept=input$Mlim, linetype="dashed", color = "orange") + geom_hline(yintercept=input$Ulim, linetype="dashed", color = "red") + geom_hline(yintercept=input$MIC, linetype="dashed", color = "blue") + geom_vline(xintercept=input$Tt, linetype="dashed", color = "black") + geom_vline(xintercept=input$Tt+24, linetype="dashed", color = "black") + geom_vline(xintercept=input$tTDM, linetype="dashed", color = "purple") + ylab("concentration (mg/L)")
          p <- a + annotate(geom = "point", x = input$tTDM, y = input$TDM, colour = "orange", size = 3) + theme(legend.position="bottom")
          ggplotly(p) %>%
            config(
              displaylogo = FALSE,
              modeBarButtonsToRemove = c("lasso2d","zoomIn2d", "zoomOut2d","pan2d","autoScale2d","zoom2d","hoverClosestCartesian"))
        })
      })
    }
    # Goti
    if (input$Model == 3){
      r <- reactive({
        if (input$tinf>input$ii){stop("The infusion time should not exceed the interdose interval")}
        Creatinine = input$Creatinine
        if (input$Age > 65){if(input$Creatinine < 1){Creatinine = 1}else{Creatinine <- input$Creatinine}}else{Creatinine = input$Creatinine}
        if (input$Sex == 1){
          TFG = 0.85*(((140-input$Age)*input$Weight)/(72*Creatinine))
        } else {TFG = (((140-input$Age)*input$Weight)/(72*Creatinine))}
        if(TFG > 150){TFG = 150}else{TFG = as.numeric(round(TFG,2))}
        if (input$DIAL == TRUE){DIAL = 1}else{DIAL = 0}
        V_pop1 <- (input$teta4G*((TFG/70)))*input$teta5G^DIAL
        V_pop2 <- input$V2
        Cl_pop <- (input$teta1G*((TFG/120)^input$teta2G))*input$teta3G^DIAL
        Q <- input$Q
        k12 = Q/V_pop1
        k21 = Q/V_pop2
        ke = Cl_pop/V_pop1
        a = 1
        b = -(k12 + k21 + ke)
        c = (k21 * ke)
        discriminant <- (b^2)-(4*a*c)
        alpha <- (-b+sqrt(discriminant))/(2*a)
        beta <- (-b-sqrt(discriminant))/(2*a)
        Ldwt = input$LDWt*input$Weight
        LdCmax =  (input$Cmax/(((k21)/(alpha*beta*V_pop1))-((((alpha-k21)/(alpha*(alpha-beta)*V_pop1)))*exp(-(alpha*1)))-((((k21-beta)/(beta*(alpha-beta)*V_pop1)))*exp(-(beta*1)))))
        if(input$TypeLD == 1){
          ld <- list(time=c(0), amount=input$Ld, rate=input$Ld/1)
          if(input$Ld <= 0){t1 = 0} else {t1 = 1}} 
        if(input$TypeLD == 2){
          ld <- list(time=c(0), amount=Ldwt, rate=Ldwt/1)
          if(input$LDWt <= 0){t1 = 0} else {t1 = 1}}
        if(input$TypeLD == 3){
          ld <- list(time=c(0), amount=LdCmax, rate=LdCmax/1)
          if(input$Cmax <= 0){t1 = 0} else {t1 = 1}}
        if(input$TypeAMT == 1){
          amt <- input$amt}
        if(input$TypeAMT == 2){
          amt <- input$dpk*input$Weight}
        if(input$TypeAMT == 3){
          amt <- (((input$AUC)*Cl_pop)/(24/input$ii))}
        omega_V1G <- (input$omega_V1G/100)*V_pop1
        omega_V2G <- (input$omega_V2G/100)*V_pop2
        omega_ClG <- (input$omega_ClG/100)*Cl_pop
        param.value=c(V_pop1, V_pop2, Cl_pop, omega_V1G, omega_V2G, omega_ClG, Q, input$aG, input$bG)
        t.value=seq(0,100,length.out=101)
        t2=(input$ii*(input$ta/input$ii))-1
        if (t2>=t1){
          if(input$cad == TRUE){ad = input$ad} else {ad = amt}
          t.dose=seq(t1,t2,by=input$ii)
          t.dose2=seq(input$ta,100,by=input$ii)
          adm <- list(time=t.dose, amount=amt, rate=amt/input$tinf)
          ad <- list(time=t.dose2, amount=ad, rate=ad/input$tinf)
        } else {adm <- list(time=t1, amount=0)}
        f <- list(name='Cc',time=t.value)
        p <- list(name=c('V_pop1','V_pop2', 'Cl_pop', 'omega_V1G','omega_V2G','omega_ClG', 'Q', 'aG', 'bG'), value=param.value)
        g <- list(size=input$nbsim,level='individual')
        pres0 <- simulx( model     = 'modelG.txt',
                         treatment = list(adm, ld, ad), 
                         group = g,
                         parameter = p,
                         output    = list(f))
        
        iTDM <- input$Tt
        fTDM <- input$Tt+24
        AUC <- auc(pres0$Cc$time[iTDM:fTDM], pres0$Cc$Cc[iTDM:fTDM])
        if(input$AUC == AUC){amt = amt}else{amt = amt*(input$AUC/AUC)}
        if(input$TypeLD == 1){
          ld <- list(time=c(0), amount=input$Ld, rate=input$Ld/1)
          ldT <- input$Ld
          if(input$Ld <= 0){t1 = 0} else {t1 = 1}} 
        if(input$TypeLD == 2){
          ld <- list(time=c(0), amount=Ldwt, rate=Ldwt/1)
          ldT <- Ldwt
          if(input$LDWt <= 0){t1 = 0} else {t1 = 1}}
        if(input$TypeLD == 3){
          ld <- list(time=c(0), amount=LdCmax, rate=LdCmax/1)
          ldT <- LdCmax
          if(input$Cmax <= 0){t1 = 0} else {t1 = 1}}
        if(input$TypeAMT == 1){
          amt <- input$amt}
        if(input$TypeAMT == 2){
          amt <- input$dpk*input$Weight}
        if(input$TypeAMT == 3){
          amt <- amt}
        omega_V1G <- (input$omega_V1G/100)*V_pop1
        omega_V2G <- (input$omega_V2G/100)*V_pop2
        omega_ClG <- (input$omega_ClG/100)*Cl_pop
        param.value=c(V_pop1, V_pop2, Cl_pop, omega_V1G, omega_V2G, omega_ClG, Q, input$aG, input$bG)
        t.value=seq(0,100,length.out=101)
        t2=(input$ii*(input$ta/input$ii))-1
        if (t2>=t1){
          if(input$cad == TRUE){ad = input$ad} else {ad = amt}
          t.dose=seq(t1,t2,by=input$ii)
          t.dose2=seq(input$ta,100,by=input$ii)
          adm <- list(time=t.dose, amount=amt, rate=amt/input$tinf)
          ad <- list(time=t.dose2, amount=ad, rate=ad/input$tinf)
        } else {adm <- list(time=t1, amount=0)}
        f <- list(name='Cc',time=t.value)
        p <- list(name=c('V_pop1','V_pop2', 'Cl_pop', 'omega_V1G','omega_V2G','omega_ClG', 'Q', 'aG', 'bG'), value=param.value)
        g <- list(size=input$nbsim,level='individual')
        res <- simulx( model     = 'modelG.txt',
                       treatment = list(adm, ld, ad), 
                       group = g,
                       parameter = p,
                       output    = list(f))
        iTDM <- input$Tt
        fTDM <- input$Tt+24
        iTDM1 <- input$Tt
        fTDM1 <- input$Tt+24
        iTDM2 <- input$Tt
        fTDM2 <- input$Tt+24
        AUC <- auc(res$Cc$time[iTDM:fTDM], res$Cc$Cc[iTDM:fTDM])
        
        for (i in c(fTDM,fTDM-12,fTDM-13,fTDM-14,fTDM-15,fTDM-16,fTDM-17,fTDM-18,fTDM-19,fTDM-20,fTDM-21,fTDM-22,fTDM-23,fTDM-24)) {
          if(res$Cc$Cc[i] >= input$MIC){
            iTDM1 = i
            if(iTDM1 == iTDM){iTDM1 = i}else{iTDM1 = i-1}
          }
        }
        for (i in c(iTDM,iTDM+1,iTDM+2,iTDM+3,iTDM+4,iTDM+5,iTDM-6,iTDM+7,iTDM+8,iTDM+9,iTDM+10,iTDM+11,iTDM+12)) {
          if(res$Cc$Cc[i] >= input$MIC){
            fTDM1 = i
            if(fTDM1 == fTDM-12){fTDM1 = i}else{fTDM1 = i-1}
          }
        }
        AUCMIC1 <- auc(res$Cc$time[iTDM1:fTDM1]-input$MIC, res$Cc$Cc[iTDM1:fTDM1]-input$MIC)
        
        for (i in c(fTDM,fTDM-1,fTDM-2,fTDM-3,fTDM-4,fTDM-5,fTDM-6,fTDM-7,fTDM-8,fTDM-9,fTDM-10,fTDM-11,fTDM-12)) {
          if(res$Cc$Cc[i] >= input$MIC){
            iTDM2 = i
            if(iTDM2 == iTDM+12){iTDM2 = i}else{iTDM2 = i-1}
          }
        }
        for (i in c(iTDM,iTDM+12,iTDM+13,iTDM+14,iTDM+15,iTDM+16,iTDM-7,iTDM+18,iTDM+19,iTDM+20,iTDM+21,iTDM+22,iTDM+23,iTDM+24)) {
          if(res$Cc$Cc[i] >= input$MIC){
            fTDM2 = i
            if(fTDM2 == fTDM){fTDM2 = i}else{fTDM2 = i-1}
          }
        }
        
        AUCMIC2 <- auc(res$Cc$time[iTDM2:fTDM2]-input$MIC, res$Cc$Cc[iTDM2:fTDM2]-input$MIC)
        
        AUCMIC <- AUCMIC2 + AUCMIC1
        
        fTMICa <- 0
        iTDMfT <- iTDM+1
        for (i in iTDMfT:fTDM) {
          if(res$Cc$Cc[i] >= input$MIC){
            fTMIC1 <- 1
            fTMICa <- fTMICa + fTMIC1 }
          fTMIC <- (fTMICa/24)*100}    
        CmaxMIC <- max(res$Cc$Cc[iTDM:fTDM])/input$MIC
        
        output$DoseA <- renderInfoBox({
          infoBox(
            "Dose", paste0(round(amt,0)," mg"), icon = tags$i(class = "fas fa-syringe", style="font-size: 12px"),
            color = "purple"
          )
        })
        output$AUC <- renderValueBox({
          valueBox(
            value = tags$p(round(AUC,0), style = "font-size: 16px",),
            subtitle = tags$p(strong("AUC (mg/L h)"), style = "font-size: 10px",),
            #icon = icon("download")
          )
        })
        output$AUCMIC <- renderValueBox({
          valueBox(
            value = tags$p(round(AUCMIC,0), style = "font-size: 16px",),
            subtitle = tags$p(strong("AUC/MIC (h)"), style = "font-size: 10px",),
            #icon = icon("download")
          )
        })
        output$FTMIC <- renderValueBox({
          valueBox(
            value = tags$p(round(fTMIC,0), style = "font-size: 16px",),
            subtitle = tags$p(strong("fT/MIC (%)"), style = "font-size: 10px",),
            #icon = icon("download")
          )
        })
        output$CMAXMIC <- renderValueBox({
          valueBox(
            value = tags$p(round(CmaxMIC,0), style = "font-size: 16px",),
            subtitle = tags$p(strong("Cmax/MIC"), style = "font-size: 10px",),
            #icon = icon("download")
          )
        })
        
        tdm <- matrix(ncol=1, nrow=99)
        for (i in 0:99) {
          tdmv = 100*i+((input$tTDM+1)+i)
          tdm[i] <- res$Cc$Cc[tdmv]
        }
        PrD <- median(tdm)
        print(PrD)
        
        output$predicted <- renderInfoBox({
          infoBox(
            "Prediction", paste0(round(PrD,2)," mg/L"), icon = tags$i(class = "fas fa-eye", style="font-size: 12px"),
            color = "purple"
          )
        })
        
        output$accuracy <- renderInfoBox({
          infoBox(
            "Accuracy", paste0(round(PrD/input$TDM,2)), icon = tags$i(class = "fas fa-bullseye", style="font-size: 12px"),
            color = "purple"
          )
        })
        output$bias <- renderInfoBox({
          infoBox(
            "Bias", paste0(round((abs(PrD-input$TDM)/input$TDM)*100,2),"%"), icon = tags$i(class = "fas fa-bullseye", style="font-size: 12px"),
            color = "purple"
          )
        })
        output$mse <- renderInfoBox({
          infoBox(
            "MSE", paste0(if (input$TDM==0){"Inf"}else{round((PrD-input$TDM)^2,2)}), icon = tags$i(class = "fas fa-bullseye", style="font-size: 12px"),
            color = "purple"
          )
        })
        
        table = matrix(
          c(input$Sex, 
            input$Age, 
            input$Weight, 
            input$Creatinine, 
            input$ii, 
            input$tinf,
            round(ldT,0),
            round(amt,0),
            round(TFG,2), 
            round(V_pop1,2), 
            round(V_pop2,2), 
            round(Cl_pop,2), 
            Q, 
            input$tTDM,
            input$TDM,
            round(PrD,2), 
            round(PrD/input$TDM,2), 
            round((abs(PrD-input$TDM)/input$TDM)*100,2),
            if(input$TDM == 0){"Inf"}else{round((PrD-input$TDM)^2,2)}
          ),
          nrow=1,
          ncol=19,
          byrow = TRUE)
        dimnames(table) = list(c("Value"),c(
          "Sex", 
          "Age", 
          "Weight", 
          "Creatinine", 
          "Interdose interval", 
          "Infusion time", 
          "Loading dose",
          "Amount",
          "TFGe", 
          "Vc", 
          "Vp", 
          "Cl", 
          "Q", 
          "Time TDM",
          "TDM",
          "Predicted", 
          "Accuracy", 
          "Bias", 
          "MSE"))
        output$downloadData <- downloadHandler(
          filename = function() {
            paste("VanPOC", ".csv", sep = "")},
          content = function(file) {
            write.csv(table, file, row.names = FALSE)})
        
        output$DATA <- reactive({
          AUC > 0
        })
        outputOptions(output, 'DATA', suspendWhenHidden = FALSE)
        
        ro <- res[['Cc']]
      })
      output$plot1 <- renderPlotly({
        withProgress(message = 'Creating plot', detail='Wait...', value = 0.1, expr={
          r=r()
          a <- plot(qr <- prctilemlx(r,band=list(level=input$level,number=4))) + geom_hline(yintercept=input$Llim, linetype="dashed", color = "green") + geom_hline(yintercept=input$Mlim, linetype="dashed", color = "orange") + geom_hline(yintercept=input$Ulim, linetype="dashed", color = "red") + geom_hline(yintercept=input$MIC, linetype="dashed", color = "blue") + geom_vline(xintercept=input$Tt, linetype="dashed", color = "black") + geom_vline(xintercept=input$Tt+24, linetype="dashed", color = "black") + geom_vline(xintercept=input$tTDM, linetype="dashed", color = "purple") + ylab("concentration (mg/L)")
          p <- a + annotate(geom = "point", x = input$tTDM, y = input$TDM, colour = "orange", size = 3) + theme(legend.position="bottom")
          ggplotly(p) %>%
            config(
              displaylogo = FALSE,
              modeBarButtonsToRemove = c("lasso2d","zoomIn2d", "zoomOut2d","pan2d","autoScale2d","zoom2d","hoverClosestCartesian"))
        })
      })  
    }
  })
})