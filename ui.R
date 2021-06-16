

mobileDetect <- function(inputId, value = 0) {
  tagList(
    singleton(tags$head(tags$script(src = "js/mobile.js"))),
    tags$input(id = inputId,
               class = "mobile-element",
               type = "hidden")
  )
}

ui <- fluidPage(
  tags$head(
    tags$link(rel = "stylesheet", type = "text/css", href = "custom_styles2.css")
  ),
  sidebarLayout(
    sidebarPanel(
      fluidRow(
        column(12, div(style="height:200px;"),
               box(width = 12, title = "sélectionner une commune", background = 'orange',
                   # Liste déroulante pour choix des communes
                   selectInput(inputId = "codeinsee",
                               label = "",
                               choices = choixcom,
                               multiple = FALSE,
                               selected = NULL
                   )
               ),
               box(width = 12, title = "choisir l'année de référence", background = 'navy',    
                   # Slider range pour choix de l annee
                   sliderTextInput(inputId = "annee",
                                   label = "",
                                   choices = head(anneesref, -1),
                                   selected = c(1990)
                   )
               ),
               box(width = 12, title = "sélectionner un territoire", background = 'olive',   
                   # Liste déroulante pour territoire de comparaison
                   selectInput(inputId = "id_zone",
                               label = "",
                               choices = choixzone,
                               multiple = FALSE
                   )
               )
        )
      ),
      width = 3),
    
    mainPanel(
      navbarPage (
         id = "app_navbar",
         selected = NULL,
         inverse = FALSE,
         collapsible = T,
         fluid = T,
         responsive = T,
         theme = NULL,
         tags$link(rel = "stylesheet", type = "text/css", href = "custom_styles2.css"),
         windowTitle = "Dynamiques d'artificialisation par le bâti des territoires du Doubs",
         header = tagList(
           useShinydashboard()
         ),
        
        
        # ---- PAGE D'ACCUEIL -----------------------------------
        
        
        
        
        tabPanel("Accueil",
                 #div(tags$img(src="logo_app222.png", width=200), style="padding-top:15px;margin-top:-15px;margin-left:-40px;"),
                 mobileDetect('isMobile'),
                 
                 fluidRow(
                   fluidRow(column(12, offset=1,div(style="height:70px;"))
                   ),
                   
                   ## BANDEAU DU HAUT
                   uiOutput("UI_bandeau_visuel"),
                   
                   ## NAVIGATION
                   column(4, div(class="voir_container1",
                                 tags$span(tags$b("Cartographie communale"), style="font-size:1.5em"),
                                 tags$br(),
                                 tags$br(),
                                 actionButton("page1", "Voir"),
                                 div(class="voir",""), style="margin-top:5px;")),
                   
                   column(4, div(class="voir_container2",
                                 tags$span(tags$b("Indicateurs territoriaux"), style="font-size:1.5em"),
                                 tags$br(),
                                 tags$br(),
                                 actionButton("page2", "Voir"),
                                 div(class="voir",""), style="margin-top:5px;")),
                   
                   column(4, div(class="voir_container3",
                                 tags$span(tags$b("Comparaison commune / territoire"), style="font-size:1.5em"),
                                 tags$br(),
                                 tags$br(),
                                 actionButton("page3", "Voir"),
                                 div(class="voir",""), style="margin-top:5px;")),
                   
                   column(12,div(style="height:10px;")),
                   column(12, offset = 1,
                          box(
                            title = tags$b(HTML("VERSION DE TRAVAIL JUIN 2021 </br> Données et indicateurs en cours de consolidation </br> NE PAS DIFFUSER"), style = "font-size:1.5em; text-align: center"),
                            background = "red",
                            solidHeather = TRUE,
                            width = 10
                          )
                          
                   ),
                   ## TEXTE DE PRESENTATION
                   fluidRow(
                     column(12,
                            fluidRow(
                              column(10, offset=1,
                                     tags$br(),
                                     tags$br(),
                                     tags$br(),            
                                     
                                     tags$span(
                                       "Visualisez les dynamiques d'artificialisation du sol par le bâti dans le Doubs",style="font-weight: bold;color:rgba(51, 71, 91, 1);"),
                                     tags$br(), 
                                     tags$span(
                                       "Décryptez ces informations grâce à un jeu d'indicateurs",style="color:rgba(51, 71, 91, 1);"),
                                     tags$br(), 
                                     tags$span(
                                       "et comparez des territoires entre eux",style="color:rgba(51, 71, 91, 1);")
                              )
                            )
                     )
                   ),
                   
                   ## INFO METHODE
                   
                   fluidRow(
                     column(4),
                     column(4, div(class="voir_container6",
                                   tags$span(tags$b("Comprendre la méthode utilisée"), style="font-size:1.5em"),
                                   tags$br(),
                                   tags$br(),
                                   actionButton("methodo", "En savoir plus"),
                                   div(class="voir",""), style="margin-top:5px;"))
                   )
                 )
        ),
        
        ## ---- VISUALISATION A LA COMMUNE ----------------------------------------
        
        tabPanel("Cartographie communale",
                 
                 # resultats
                 fluidRow(
                   column(12,

                          leafletOutput("carte", height = 700),
                          tags$br(),
                          box(
                            title = textOutput("nomcom"),
                            width = 12,
                            background = 'orange',
                            height = 30,
                          ),
                          tags$br(),
                          div(class = "col-sm-12 col-md-6 col-lg-4", 
                                 fluidRow(
                                   uiOutput("indicateur11"), 
                                   uiOutput("indicateur12"),
                                   uiOutput("indicateur13")
                                 )
                          ),
                          div(class = "col-sm-12 col-md-6 col-lg-4", 
                            box(
                              title = "progression de la surface artificialisée par le bâti",
                              solidHeather = TRUE,
                              status = "primary",
                              plotlyOutput(outputId = "barres12", height = 300), 
                              width = '100%'
                            )
                          ),
                          div(class = "col-sm-12 col-md-6 col-lg-4", 
                            box(
                              title = "évolution comparée SAB et population",
                              solidHeather = TRUE,
                              status = "primary",
                              plotlyOutput(outputId = "lines11", height = 300), 
                              width = '100%'
                            )
                          )
                          
                   )
                 )
        ),
        
        ## --- VISUALISATION SUR UN TERRITOIRE
        
        tabPanel("Indicateurs territoriaux",
                 
                 fluidRow(
                   column(12,
                          
                          # sélection de l'indicateur
                          box(width = 12, title = "sélectionner l'indicateur à représenter", background = 'olive',    
                              selectInput(inputId = "indicateur",
                                          label = "",
                                          choices = ind,
                                          multiple = FALSE,
                                          selected = ind[1]
                              )
                          ),
                          
                          # resultats
                          
                          
                          leafletOutput("carteter", height = 700),
                          tags$br(),
                          box(
                            title = textOutput("nomzone"),
                            width = 12,
                            background = 'olive',
                            height = 30,
                          ),
                          tags$br(),
                          div(class = "col-sm-12 col-md-6 col-lg-4", 
                                 fluidRow(
                                   uiOutput("indicateur21"), 
                                   uiOutput("indicateur22"),
                                   uiOutput("indicateur23"),
                                 )
                          ),
                          div(class = "col-sm-12 col-md-6 col-lg-4",
                            box(
                              title = "progression de la surface artificialisée par le bâti",
                              solidHeather = TRUE,
                              status = "primary",
                              plotlyOutput(outputId = "barres22", height = 300), 
                              width = '100%'
                            )
                          ),
                          div(class = "col-sm-12 col-md-6 col-lg-4",
                            box(
                              title = "évolution comparée SAB et population",
                              solidHeather = TRUE,
                              status = "primary",
                              plotlyOutput(outputId = "lines21", height = 300), 
                              width = '100%'
                            )
                          )

                   )
                 )
        ),
        
        ## --- COMPARAISON COMMUNE / TERRITOIRE
        
        tabPanel("Comparaison commune / territoire",
                 
                 # résultats
                 fluidRow(
                   div(class = "col-sm-12 col-md-6 col-lg-4",
                   box(
                     title = ind1,
                     solidHeather = TRUE,
                     status = "primary",
                     plotlyOutput(outputId = "gauge31", height = 300), 
                     width = '100%',
                     height = '300px'
                   )),
                   div(class = "col-sm-12 col-md-6 col-lg-4",
                   box(
                     title = textOutput("nomind2"),
                     solidHeather = TRUE,
                     status = "primary",
                     plotlyOutput(outputId = "gauge32", height = 300), 
                     width = '100%',
                     height = '300px'
                   )),
                   div(class = "col-sm-12 col-md-6 col-lg-4",
                   box(
                     title = textOutput("nomind3"),
                     solidHeather = TRUE,
                     status = "primary",
                     plotlyOutput(outputId = "gauge33", height = 300), 
                     width = '100%',
                     height = '300px'
                   )),
                   div(class = "col-sm-12 col-md-6 col-lg-4",
                   box(
                     title = ind4,
                     solidHeather = TRUE,
                     status = "primary",
                     plotlyOutput(outputId = "gauge34", height = 300), 
                     width = '100%',
                     height = '300px'
                   )),
                   div(class = "col-sm-12 col-md-6 col-lg-4",
                   box(
                     title = ind5,
                     solidHeather = TRUE,
                     status = "primary",
                     plotlyOutput(outputId = "gauge35", height = 300), 
                     width = '100%',
                     height = '300px'
                   )),
                   div(class = "col-sm-12 col-md-6 col-lg-4",
                   box(
                     title = ind6,
                     solidHeather = TRUE,
                     status = "primary",
                     plotlyOutput(outputId = "gauge36", height = 300), 
                     width = '100%',
                     height = '300px'
                   )),
                   div(class = "col-sm-12 col-md-6 col-lg-4",
                   box(
                     title = ind7,
                     solidHeather = TRUE,
                     status = "primary",
                     plotlyOutput(outputId = "gauge37", height = 300), 
                     width = '100%',
                     height = '300px'
                   )),
                   div(class = "col-sm-12 col-md-6 col-lg-4",
                   box(
                     title = paste0(ind8, " entre 2012 et 2017"),
                     solidHeather = TRUE,
                     status = "primary",
                     plotlyOutput(outputId = "gauge38", height = 300), 
                     width = '100%',
                     height = '300px'
                   ))
                 )
        ),
        
        # autres onglets
        
        tabPanel("Méthodologie",
                 fluidRow(
                   fluidRow(column(10, offset=1,div(style="height:50px;")
                   )),
                   column(2),
                   column(8,includeHTML("./html/methodo.html")),
                   column(2),
                   tags$br(),
                   tags$br()
                 )
        ),
        
        tabPanel("Mentions légales",
                 
                 fluidRow(
                   fluidRow(column(10, offset=1,div(style="height:50px;")
                   )),
                   column(2),
                   column(8,includeHTML("./html/mentionslegales.html")),
                   column(2),
                   tags$br(),
                   tags$br()
                 )
        ),
        
        
        ## FOOTER ---------------------------
        
        footer = div(
          tags$img(src="bloc_logo_rs.png"),
          actionLink("mentions", "Mentions légales",style="color : black; margin-right:50px"),
          "Copyright 2021 © - Cerema",
          style="text-align:center;
      border-top : 0.2px solid #d2d2d2;
      padding-top: 10px;
      height: 30px;
      margin-top: 50px;
                 "
        ),
        tags$head(tags$script(src = "./www/test.js"))
        
        
        
        
        
      ) ### End Navbar
      
    ) ### End mainpanel
    
  ) ### End sidebarlayout
  
) ### End FluidPage

shinyUI <- shinyUI(
  ui
)



