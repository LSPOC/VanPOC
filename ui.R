library(shiny)
library(ggplot2)
library(gridExtra)
library(shinydashboard)
library(shinyjs)
library(shinyBS)
library(plotly)
library(shinybusy)
library(reticulate)
library(RJSONIO)
library(shinyTime)
library(dataCompareR)
ui <- dashboardPage(skin = "purple",
                    dashboardHeader(title = "VanPOC", titleWidth = 126),
                    dashboardSidebar(disable = TRUE),
                    dashboardBody(tags$head(
                      tags$style(HTML('.value-box {font-size: 11px;} 
                                      .info-box {min-height: 45px; margin-top: 10px; padding: 0px; font-size: 11px;} 
                                      .info-box-icon {height: 45px; line-height: 0px; padding-top: 12px; margin-right: 6px; width: 20%} 
                                      .info-box-content {margin: 0px; padding-top: 0px; padding-bottom: 0px; text-align: left;font-size: 8px;}'))),
                      column(6, style = "padding:0px",
                             box(width = 12, #height = "610px",
                                 tabBox(id = "Steps", width = 12,
                                        tabPanel("Model",
                                          fluidRow(
                                            column(3, 
                                                   selectInput(
                                                     "Model", 
                                                     "PkPop Model", 
                                                     choices = list(
                                                       "Llopis-Salvia et al. (2006)" = 1, 
                                                       "Revilla et al. (2010)" = 2, 
                                                       "Goti et al. (2018)" = 3), 
                                                     selected = 1)),
                                            column(width = 9,
                                                   column(3, selectInput("Sex", "Sex",choices = list("Women" = 1,"Man" = 2),selected = 2)),
                                                   column(3, numericInput("Age", "Age",50,min = 17,max = 100)),
                                                   column(3, numericInput("Weight", "TBW (Kg)",70,min = 20,max = 150)),
                                                   column(3, numericInput("Creatinine", "Cr (mg/dL)", 0.8, min = 0.1, max = 10.0, step=0.01)),
                                                   conditionalPanel(condition = "output.Goti", column(4, checkboxInput("DIAL", "DIAL", width =3)))))),
                                        tabPanel("Loading Dose",
                                                 fluidRow(
                                                   column(4,
                                                          selectInput(
                                                            "TypeLD",
                                                            "Type",
                                                            choices = list(
                                                              "Value" = 1,
                                                              "By Weight" = 2,
                                                              "By Cmax" = 3),
                                                            selected = 1)),
                                                   conditionalPanel(
                                                     condition = "output.LD",
                                                     column(4, numericInput("Ld", "Dose (mg)", value = 0, min = 0, max = 2000, step=1))),
                                                   conditionalPanel(
                                                     condition = "output.LDWt",
                                                     column(4, numericInput("LDWt", "mg x Kg", value = 20, min = 0, max = 2000, step=1)),
                                                     infoBoxOutput("Ldwt", width = 4)),
                                                   conditionalPanel(
                                                     condition = "output.LDCmax",
                                                     column(4, numericInput("Cmax", "Cmax (mg/L)", value = 40, min = 0, max = 2000, step=1)),
                                                     infoBoxOutput("LdCmax", width = 4)))),
                                        tabPanel("Empirical Dose",
                                                 fluidRow(
                                                   column(4, 
                                                          selectInput(
                                                            "TypeAMT",
                                                            "Type",
                                                            choices = list(
                                                              "Value" = 1,
                                                              "By Weight" = 2,
                                                              "By AUC" = 3),
                                                            selected = 1)),
                                                   conditionalPanel(
                                                     condition = "output.value", 
                                                     column(4, numericInput("amt", "Amount (mg)", value = 0, min = 0, max = 2000, step=1))),
                                                   conditionalPanel(
                                                     condition = "output.byweight", 
                                                     column(4, numericInput("dpk", "mg x Kg", value = 15, min = 0, max = 30, step=1)),
                                                     infoBoxOutput("DoseW", width = 4)),
                                                   conditionalPanel(
                                                     condition = "output.byauc", 
                                                     column(4, numericInput("AUC", "AUC (mg/L h)", value = 500, min = 0, max = 700, step=1)),
                                                     infoBoxOutput("DoseA", width = 4)))),
                                        tabPanel("Adjusted Dose",
                                                 fluidRow(
                                                   column(4, checkboxInput("cad", "Adjustment", width =3)),
                                                   conditionalPanel(
                                                     condition = "output.adjustment",
                                                     column(4, numericInput("ad", "Dose (mg)", min = 0, max = 3000, value = 1000, step=1)),
                                                     column(4, numericInput("ta", "Time (h)", min = 0, max = 72, value = 48, step=1))))),
                                        tabPanel("Dosage",
                                                 fluidRow(
                                                   column(4, numericInput("nd", "Number of doses", value=14, min=1, max = 20, step=1)),
                                                   column(4, numericInput("ii", "Interdose interval (h)", value = 12, min = 1, max = 48, step=1)),
                                                   column(4, numericInput("tinf", "Infusion time (h)", value = 4, min = 0, max = 12, step=0.1))))),
                                 tabBox(id = "PkPop", width = 12,
                                        tabPanel("Pharmacokinetics",
                                                 fluidRow(style = "padding: 0px;", 
                                                          valueBoxOutput("TFG", width = 3),
                                                          conditionalPanel(
                                                            condition = "output.Llopis",
                                                            valueBoxOutput("Cl", width = 2),
                                                            valueBoxOutput("Vc", width = 2),
                                                            valueBoxOutput("Vp", width = 2),
                                                            valueBoxOutput("Q", width = 2)),
                                                          conditionalPanel(
                                                            condition = "output.Revilla",
                                                            valueBoxOutput("ClR", width = 2),
                                                            valueBoxOutput("V", width = 2)),
                                                          conditionalPanel(
                                                            condition = "output.Goti",
                                                            valueBoxOutput("ClG", width = 2),
                                                            valueBoxOutput("VcG", width = 2),
                                                            column(2, numericInput("V2", "Vp", value = 38.4, min = 0, max = 10, step=0.01)),
                                                            column(2, numericInput("Q", "Q", value = 6.5, min = 0, max = 20, step=0.01))))),
                                        tabPanel("Coefficients",
                                                 fluidRow(
                                                   conditionalPanel(
                                                     condition = "output.Llopis",
                                                     column(2, numericInput("teta1", "Θ_1", value = 0.034, min = 0, max = 10, step=0.01)),
                                                     column(2, numericInput("teta2", "Θ_2", value = 0.015, min = 0, max = 10, step=0.01)),
                                                     column(2, numericInput("teta3", "Θ_3", value = 0.414, min = 0, max = 10, step=0.01)),
                                                     column(2, numericInput("teta4", "Θ_4", value = 7.48, min = 0, max = 10, step=0.01)),
                                                     column(2, numericInput("teta5", "Θ_5", value = 1.32, min = 0, max = 20, step=0.01))),
                                                   conditionalPanel(
                                                     condition = "output.Revilla",
                                                     column(2, numericInput("teta11", "Θ_1", value = 0.67, min = 0, max = 10, step=0.01)),
                                                     column(2, numericInput("teta22", "Θ_2", value = -0.24, min = -1, max = 10, step=0.01)),
                                                     column(2, numericInput("teta33", "Θ_3", value = 0.82, min = 0, max = 10, step=0.01)),
                                                     column(2, numericInput("teta44", "Θ_4", value = 2.49, min = 0, max = 10, step=0.01))),
                                                   conditionalPanel(
                                                     condition = "output.Goti",
                                                     column(2, numericInput("teta1G", "Θ_1", value = 4.5, min = 0, max = 10, step=0.01)),
                                                     column(2, numericInput("teta2G", "Θ_2", value = 0.8, min = -1, max = 10, step=0.01)),
                                                     column(2, numericInput("teta3G", "Θ_3", value = 0.7, min = 0, max = 10, step=0.01)),
                                                     column(2, numericInput("teta4G", "Θ_4", value = 58.4, min = 0, max = 10, step=0.01)),
                                                     column(2, numericInput("teta5G", "Θ_5", value = 0.5, min = 0, max = 20, step=0.01))))),
                                        tabPanel("Equations",
                                                 fluidRow(
                                                   conditionalPanel(
                                                     condition = "output.Llopis",
                                                     withMathJax(),
                                                     box(width = 4, helpText('$$Cl = \\theta_1 \\cdot Cl_{cr} + \\theta_2 \\cdot TBW $$', style = "font-size:10px",)),
                                                     box(width = 3, helpText('$$V_c = \\theta_3 \\cdot TBW $$', style = "font-size:10px",)),
                                                     box(width = 3, helpText('$$V_p = \\theta_5 \\cdot TBW $$', style = "font-size:10px",)),
                                                     box(width = 2, helpText('$$Q = \\theta_4 $$', style = "font-size:10px"))),
                                                   conditionalPanel(
                                                     condition = "output.Revilla",
                                                     withMathJax(),
                                                     box(width = 4, helpText('$$Cl = \\theta_1 \\cdot Cl_{cr} +  TBW^{\\theta_2} $$', style = "font-size:10px")),
                                                     box(width = 3, helpText('$$V = \\theta_3 \\cdot \\theta_4^{A} $$', style = "font-size:10px")),
                                                     box(width = 5, helpText('$$A = 0 \\text{ if } Cr < 1 \\text{; } A = 1 \\text{ if } Cr > 1$$', style = "font-size:10px"))),
                                                   conditionalPanel(
                                                     condition = "output.Goti",
                                                     withMathJax(),
                                                     box(width = 4, helpText('$$Cl = \\theta_1 \\cdot \\left(\\frac{Cl_{cr}}{120}\\right)^{\\theta_2} \\cdot \\theta_3^{DIAL} $$', style = "font-size:10px")),
                                                     box(width = 4, helpText('$$V_c = \\theta_4 \\cdot \\left(\\frac{TBW}{70}\\right) \\cdot \\theta_5^{DIAL} $$', style = "font-size:10px")),
                                                     box(width = 4, helpText('Clcr = 150 mL/min if Clcr > 150 mL/min. SCr = 1 mg/dL if SCr < 1 mg/dL and age > 65 years.', style = "font-size:10px")))))),
                                 tabBox(id = "Configuration", width = 12,
                                        tabPanel("Limits",
                                                 fluidRow(
                                                   column(2, numericInput("Llim", "Lower  (mg/L)", value = 10, min = 0, max = 15, step=1)),
                                                   column(2, numericInput("Mlim", "Middle (mg/L)", value = 15, min = 0, max = 20, step=1)),
                                                   column(2, numericInput("Ulim", "Upper (mg/L)", value = 20, min = 0, max = 20, step=1)),
                                                   column(2, numericInput("Tt", "Time (+24h)", value = 36, min = 0, max = 72)),
                                                   column(2, numericInput("MIC", "MIC (mg/L)", value = 1, min = 0, max = 10, step=0.5)),
                                                   column(2, numericInput("tTDM", "TDM Time (h)", value = 36, min = 0, max = 100, step=1)))),
                                        tabPanel("Parameters",
                                                 fluidRow(
                                                   conditionalPanel( 
                                                     condition = "output.Llopis",
                                                     column(2, numericInput("omega_V1", "Omega Vc", value = 0, min = 0, max = 20, step=0.1)),
                                                     column(2, numericInput("omega_V2", "Omega Vp", value = 0, min = 0, max = 70, step=0.1)),
                                                     column(2, numericInput("omega_Cl", "Omega Cl", value = 0, min = 0, max = 10, step=0.1)),
                                                     column(2, numericInput("a", "Sigma 1", value = 0, min = 0, max = 60, step=1)),
                                                     column(2, numericInput("b", "Sigma 2", value = 0, min = 0, max = 60, step=1))),
                                                   conditionalPanel(
                                                     condition = "output.Revilla",
                                                     column(2, numericInput("omega_V", "Omega V", value = 0, min = 0, max = 20, step=0.1)),
                                                     column(2, numericInput("omega_ClR", "Omega Cl", value = 0, min = 0, max = 10, step=0.1)),
                                                     column(2, numericInput("c", "Sigma 1", value = 0, min = 0, max = 60, step=1))),
                                                   conditionalPanel( 
                                                     condition = "output.Goti",
                                                     column(2, numericInput("omega_V1G", "Omega Vc", value = 0, min = 0, max = 20, step=0.1)),
                                                     column(2, numericInput("omega_V2G", "Omega Vp", value = 0, min = 0, max = 70, step=0.1)),
                                                     column(2, numericInput("omega_ClG", "Omega Cl", value = 0, min = 0, max = 10, step=0.1)),
                                                     column(2, numericInput("aG", "Sigma 1", value = 0, min = 0, max = 60, step=1)),
                                                     column(2, numericInput("bG", "Sigma 2", value = 0, min = 0, max = 60, step=1))))), 
                                        tabPanel("Settings",
                                                 fluidRow(
                                                   column(2,numericInput("level", "Level (%)", min = 5, max = 95, value = 95, step=5)),
                                                   column(4,numericInput("nbsim", "Number of simulations", min = 1, max = 1000, value = 1000, step=100))))),
                                 column(12, style="margin-top: 2px; text-align:right", 
                                        column(10, style="margin-top: 10px", conditionalPanel(condition = "output.DATA", downloadButton(style = "color:black;background-color:lightgray;font-size:12px;", "downloadData", "Download"))),
                                        column(2, style="margin-top: 10px", actionButton(style = "color:white; background-color:green;font-size:12px;", "EstimatePk", strong(" Simulate"), icon = icon("play")))))),
                      column(6, style = "padding:0px",
                             box(width = 12, status = "info",
                                 column(9, 
                                        infoBoxOutput("predicted", width = 6),
                                        infoBoxOutput("accuracy", width = 6),
                                        infoBoxOutput("bias", width = 6),
                                        infoBoxOutput("mse", width = 6)),
                                 column(3,
                                        fluidRow(
                                          column(12, numericInput("TDM", "TDM (mg/L)", value = 0, min = 0, max = 1000, step=0.5)),
                                          column(12, style = "padding-top:10px;text-align:center",
                                                 actionButton(style = "color:black;background-color:orange;font-size:12px;",
                                                              "tabBut", icon = tags$i(class = "fas fa-vial", style="font-size: 12px"),"Biosensor"),
                                                 bsModal("modal", "Biosensor", "tabBut", size = "large",
                                                         fluidRow(column(7, plotOutput(width = "100%", height = "400px", outputId="distPlot")),
                                                         column(5,
                                                                style = "padding-top:0px",
                                                                box(width=12,status = "success",style = "background-color:rgb(229, 231, 233)",
                                                                    h4(textOutput(outputId = "Ipa"),
                                                                       style="background-color:blue;padding:10px;color:white;text-align:center"),
                                                                    h4(textOutput(outputId = "Ipc"),
                                                                       style="background-color:red;padding:10px;color:white;text-align:center"),
                                                                    h4(textOutput(outputId = "delE"),
                                                                       style="background-color:lightgray;padding:10px;color:black;text-align:center"),
                                                                    h4(textOutput(outputId = "Eavg"),
                                                                       style="background-color:lightgray;padding:10px;color:black;text-align:center"),
                                                                    h4(textOutput(outputId = "pI"),
                                                                       style="background-color:lightgray;padding:10px;color:black;text-align:center")),
                                                                infoBox(
                                                                  width= 7, 
                                                                  NULL,
                                                                  textOutput(outputId = "CP2"),
                                                                  p("mg/L"), 
                                                                  color = "purple",
                                                                  icon = tags$i(class = "fas fa-vial", style="font-size: 26px")),
                                                                actionButton(
                                                                  "test",
                                                                  "Test",
                                                                  color="green",
                                                                  style = "position:relative; 
                                                                  font-size:20px; 
                                                                  color:white; 
                                                                  background-color:rgb(46, 134, 193); 
                                                                  padding:0px; 
                                                                  margin-bottom:0px; 
                                                                  margin-top:20px"))))))),
                                 column(12, style = "margin-top:0px",
                                        fluidRow(
                                          column(12, plotlyOutput("plot1",  height="327px")),
                                          column(12, style = "padding:20px",
                                                 valueBoxOutput("AUC", width = 3),
                                                 valueBoxOutput("AUCMIC", width = 3),
                                                 valueBoxOutput("FTMIC", width = 3),
                                                 valueBoxOutput("CMAXMIC", width = 3)))))),
                      column(12, style = "text-align:center;font-size:10px;padding-top:0px", img(src="https://upload.wikimedia.org/wikipedia/commons/a/af/Unisabana.png", style="width:42px;"), strong("Universidad de La Sabana"))))
