
# FONCTION input / output
shinyServer(function(input, output, session) {
  
  ## Navigation -------------------
  
  observeEvent(input$page1, {
    updateNavbarPage(session, "app_navbar", selected = "Cartographie communale")
  })
  
  observeEvent(input$page2, {
    updateNavbarPage(session, "app_navbar", selected = "Indicateurs territoriaux")
  })
  
  observeEvent(input$page3, {
    updateNavbarPage(session, "app_navbar", selected = "Comparaison commune / territoire")
  })
  
  observeEvent(input$methodo, {
    updateNavbarPage(session, "app_navbar", selected = "Méthodologie")
  })
  
  observeEvent(input$mentions, {
    updateNavbarPage(session, "app_navbar", selected = "Mentions légales")
  })
  
  ## [Bandeau du haut]-------------
  
  output$UI_bandeau_visuel <- renderUI({
    
    
    fluidRow(id="bandeau_visuel",
             fluidRow(
               column(12, offset=0,
                      fluidRow(tags$p("Dynamiques d'artificialisation par le bâti des territoires du Doubs", style="font-size:3em;"))
                      )
             )
    ) 
  })
  
  # variables réactives
  
  v <- reactiveValues(
    annee0 = annee_t0,
    commune_select = NULL,
    zone_select = NULL,
    annees = anneesref,
    boite = NULL,
    com_date = NULL,
    com_nondate = NULL,
    com_bati = NULL,
    com_indic = NULL,
    zone_date = NULL,
    zone_nondate = NULL, 
    zone_indic = NULL,
    com_tempo = NULL,
    zone_tempo = NULL,
    zone_indictempo = NULL,
    tempo = NULL,
    ind2 = NULL,
    ind3 = NULL,
    comind2 = NULL,
    comind3 = NULL
    
  )
  
  # inputs réactifs

  

# réactivité quand changement de choix de commune

  observeEvent(
    input$codeinsee, {

      # choix par défaut de l'EPCI d'appartenance de la commune
      updateSelectInput(session, "id_zone", selected = dcommunes[dcommunes$insee_com == input$codeinsee, "code_epci"])
      
      v$commune_select = dcommunes %>% filter(insee_com == input$codeinsee)
      
      # boite englobante
      b <- st_bbox(v$commune_select)
      v$boite <- lapply(split(b, names(b)), unname) # permet de convertir un vecteur en liste

      # extraction des couches enveloppe et bati
      com <- paste(unlist(input$codeinsee), collapse = "', '")
      requete_date <- paste("SELECT * FROM env_date WHERE code_insee IN ('", com, "')", sep ="")
      requete_nondate <- paste("SELECT * FROM env_nondate WHERE code_insee IN ('", com, "')", sep ="")
      requete_bati <- paste("SELECT * FROM bati WHERE insee_com IN ('", com, "')", sep ="")

      v$com_date <- st_as_sf(dbGetQuery(conn, requete_date), crs = 4326)
      v$com_nondate <- st_as_sf(dbGetQuery(conn, requete_nondate), crs = 4326)
      v$com_bati <- st_as_sf(dbGetQuery(conn, requete_bati), crs = 4326)

      # sélection des indicateurs
      v$com_indic <- dindic %>% filter (insee_com %in% input$codeinsee)
      v$com_tempo <- dtempo %>% filter (insee_com %in% input$codeinsee)

      })

  # réactivité quand changement de zonage territorial
  
  observeEvent(
    input$id_zone, {
      ifelse(input$id_zone == 'dept',
      v$zone_select <- dcommunes,
      v$zone_select <- dcommunes %>% 
        filter (code_epci %in% input$id_zone | code_scot %in% input$id_zone | code_pnr %in% input$id_zone)
      )
      
      # extraction des indicateurs
      comzone1 <- v$zone_select %>% select(insee_com)
      v$zone_indic <- dindic %>% filter(insee_com %in% comzone1$insee_com)
      v$zone_tempo <- dtempo %>% filter(insee_com %in% comzone1$insee_com)
      
    }
  )
  
  # réactivité quand changement d'année de référence
  
  observeEvent(
    input$annee, {
      v$annees <- anneesref[sapply(anneesref, function(x) x >= input$annee)]
  
      v$tempo <- dtempo %>% filter(annee == input$annee)
      
      }
    )
  
  # indicateurs qui dépendent à la fois de l'année de référence et du zonage
  
  # observeEvent(
  #   c(input$annee, input$code_insee), {
  #     v$comind2 <- v$com_indic$sartif - v$com_tempo[v$com_tempo$annee == input$annee, "stot"][1]
  #     v$comind3 <- 100*v$comind2 / v$com_tempo[v$com_tempo$annee == input$annee, "stot"][1]
  #   }
  # )
  
  observeEvent(
    c(input$annee, input$id_zone, input$code_insee), {
      
      v$zone_indictempo <- v$zone_indic %>%
        dplyr::left_join(v$tempo, by = NULL, copy = FALSE) %>%
        mutate(sartif_evo = sartif - stot) %>%
        mutate(partif_evo = 100*(sartif - stot)/ stot)
      
      v$ind2 <- v$zone_indictempo %>%
        select(nom_com, sartif_evo)
      
      v$ind3 <- v$zone_indictempo %>%
        select(nom_com, partif_evo)
      
      v$comind2 <- v$com_indic$sartif - v$com_tempo[v$com_tempo$annee == input$annee, "stot"][1]
      v$comind3 <- 100*v$comind2 / v$com_tempo[v$com_tempo$annee == input$annee, "stot"][1]
      
    }
  )
  

  

  
## --------------- OUTPUTS COMMUNE -----------------------------------------------
  
  output$nomcom <- renderText({v$commune_select$nom_com})
  
  # indicateur surface artificialisée
  
  output$indicateur11 <- renderUI({
    infoBox(
      value = paste0(round(v$com_indic$senv17 + v$com_indic$senvnd), " ha"), 
      title = HTML("surface artificialisée <br/> par le bâti en 2017"), 
      icon = icon("new-window", lib = "glyphicon"),
      color = 'orange',
      width = 12,
      fill = TRUE
    )
  })
  
  # indicateur densité artificialisée
  
  output$indicateur12 <- renderUI({
    infoBox(
      value = paste0(round(1000000 *(v$com_indic$senv17 + v$com_indic$senvnd)/ sum(v$commune_select$surface),1), " %"), 
      title = HTML("part de la commune <br/> artificialisée par le bâti en 2017"), 
      icon = icon("new-window", lib = "glyphicon"),
      color = 'orange',
      width = 12,
      fill = TRUE
    )
  })
  
  # indicateur surface artificialisée par occupant potentiel
  
  output$indicateur13 <- renderUI({
    infoBox(
      value = HTML(paste0(round(v$com_indic$sartif_par_op, 0), " m<sup>2</sup>")), 
      title = HTML("surface artificialisée par<br/> occupant potentiel en 2017"), 
      icon = icon("new-window", lib = "glyphicon"),
      color = 'orange',
      width = 12,
      fill = TRUE
    )
  })
  
 
  
  # barplot progression SAB
  
  sabcom <- reactive({ v$com_tempo %>%
      select(annee, stot) %>% 
      filter(annee >= input$annee) %>%
      arrange(annee) %>%
      mutate(stot_prec = dplyr::lag(stot),
             annee_prec = dplyr::lag(annee),
             annees_ecart = paste0(annee_prec, ' - ', annee),
             surface_prog = (stot - stot_prec) / (annee - annee_prec)
      ) %>%
      filter(!is.na(annee_prec)) %>%
      select (annees_ecart, surface_prog)
  })
  
  output$barres12 <- renderPlotly({
    plot_ly(sabcom(),
    x = ~annees_ecart,
    y = ~surface_prog,
    type = 'bar',
    marker = list(color = col1)
    ) %>%
    layout(xaxis = list(title = 'années'), 
           yaxis = list(title = 'ha par an'))
  })
  
  # courbes évolution SAB versus population
  
  popcom <- reactive({ v$com_tempo %>%
      select(annee, population, stot) %>% 
      filter (annee >= input$annee) %>%
      arrange(annee)
      })
  
  output$lines11 <- renderPlotly({
    plot_ly()%>%
      add_trace(x = popcom()$annee,
                y = 100 * popcom()$stot / popcom()$stot[1],
                name = 'surface artificialisée par le bâti',
                type = 'scatter',
                mode = 'lines+markers',
                marker = list(color = col1),
                line = list(color = col1)
      )%>%
      add_trace(x = popcom()$annee,
                y = 100 * popcom()$population / popcom()$population[1],
                name = 'population',
                type = 'scatter',
                mode = 'lines+markers',
                marker = list(color = col3),
                line = list(color = col3)
      )%>%
      layout(yaxis = list(title = paste('base 100 en ', input$annee, sep = '')),
             legend = list(x = 0, y = 1.1))
  })
  
  
  # carte leaflet commune

  gr1 <- 'surface artificialisée 2017'
  gr2 <- 'surface artificialisée référence'
  gr5 <- 'surface artificialisée non datée'
  gr3 <- 'commune sélectionnée'
  gr4 <- 'bati'
  
  
  # fond de carte et légende
  output$carte <- renderLeaflet({
    leaflet() %>% 
      fitBounds(lat1 = v$boite$ymin, lng1 = v$boite$xmin, lat2 = v$boite$ymax, lng2 = v$boite$xmax) %>%
      addTiles(group = "OSM") %>%
      addProviderTiles(providers$GeoportailFrance.orthos, group = "orthophoto IGN") %>%
      addProviderTiles(providers$Stamen.TonerBackground, group = "OSM (NB)") %>%
      addMapPane("envref", zIndex = 420) %>%
      addMapPane("envact", zIndex = 410) %>%
      addMapPane("bati", zIndex = 430) %>%
      addLayersControl(
        baseGroups = c("OSM", "orthophoto IGN", "OSM (NB)"),
        overlayGroups = c(gr1, gr2, gr5, gr3, gr4),
        options = layersControlOptions(collapsed = TRUE)
      ) %>%
      addPolygons(data = req(v$commune_select),
                  color = col1,
                  fillColor = col1,
                  fillOpacity = 0.2, 
                  group = gr3
      )%>%
      
      addPolygons(data = req(v$com_date) %>% filter(datation == 2016), 
                  fillColor = 'red',
                  fillOpacity = 0.7,
                  stroke = FALSE,
                  group = gr1, 
                  options = pathOptions(pane = "envact")
      )%>% 
      addPolygons(data = req(v$com_nondate), 
                  fillColor = 'grey',
                  fillOpacity = 0.7,
                  stroke = FALSE,
                  group = gr5, 
                  options = pathOptions(pane = "envact")
      )%>% 
      addPolygons(data = req(v$com_date) %>% filter(datation == input$annee - 1), 
                  fillColor = 'orange',
                  fillOpacity = 0.7,
                  stroke = FALSE,
                  group = gr2, 
                  options = pathOptions(pane = "envref")
      )%>%        
      addPolygons(data = req(v$com_bati),
                  stroke = FALSE,
                  fillColor = 'black',
                  fillOpacity = 0.7, 
                  group = gr4, 
                  options = pathOptions(pane = "bati")
      )%>%
      addScaleBar(position = 'bottomleft', options = scaleBarOptions(imperial = FALSE))
      
  })
  
  # recale la carte et met à jour la couche des enveloppes si changement de commune sélectionnée
  observeEvent(input$codeinsee, {
    leafletProxy("carte") %>%
      fitBounds(lat1 = v$boite$ymin, lng1 = v$boite$xmin, lat2 = v$boite$ymax, lng2 = v$boite$xmax)
      leafletProxy("carte") %>%
        clearShapes() %>%
        addPolygons(data = req(v$commune_select),
                    color = col1,
                    fillColor = col1,
                    fillOpacity = 0.2, 
                    group = gr3
                    )%>%

        addPolygons(data = req(v$com_date) %>% filter(datation == 2016), 
                  fillColor = 'red',
                  fillOpacity = 0.7,
                  stroke = FALSE,
                  group = gr1, 
                  options = pathOptions(pane = "envact")
                  )%>% 
        addPolygons(data = req(v$com_nondate), 
                    fillColor = 'grey',
                    fillOpacity = 0.7,
                    stroke = FALSE,
                    group = gr5, 
                    options = pathOptions(pane = "envact")
        )%>% 
        addPolygons(data = req(v$com_date) %>% filter(datation == input$annee - 1), 
                    fillColor = 'orange',
                    fillOpacity = 0.7,
                    stroke = FALSE,
                    group = gr2, 
                    options = pathOptions(pane = "envref")
        )%>%        
        addPolygons(data = req(v$com_bati),
                    stroke = FALSE,
                    fillColor = 'black',
                    fillOpacity = 0.7, 
                    group = gr4, 
                    options = pathOptions(pane = "bati")
        )
  })
  
  # met à jour la couche des enveloppes si changement d'année sélectionnée
  observeEvent(input$annee,{
      leafletProxy("carte") %>%
      clearGroup(group = gr2)%>%
      addPolygons(data = req(v$com_date) %>% filter(datation == input$annee - 1), 
                fillColor = 'orange',
                fillOpacity = 0.7,
                stroke = FALSE,
                group = gr2, 
                options = pathOptions(pane = "envref")
      )

      v$annee0 <- input$annee
  })
  
  
  ## --------------- OUTPUTS TERRITOIRE -----------------------------------------------
  
  output$nomzone <- renderText({dzonages[dzonages$id_zone == input$id_zone, "nom_zone"]})
  
  # indicateur surface artificialisée
  
  output$indicateur21 <- renderUI({
    infoBox(
      value = paste0(round(sum(v$zone_indic$senv17) + sum(v$zone_indic$senvnd)), " ha"), 
      title = HTML("surface artificialisée <br/> par le bâti en 2017"), 
      icon = icon("new-window", lib = "glyphicon"),
      color = 'olive',
      width = 12,
      fill = TRUE
    )
  })
  
  # indicateur densité artificialisée
  
  output$indicateur22 <- renderUI({
    infoBox(
      value = paste0(round(1000000 *(sum(v$zone_indic$senv17) + sum(v$zone_indic$senvnd))/ sum(v$zone_select$surface),1), " %"), 
      title = HTML("part du territoire <br/> artificialisé par le bâti en 2017"), 
      icon = icon("new-window", lib = "glyphicon"),
      color = 'olive',
      width = 12,
      fill = TRUE
    )
  })
  
  # indicateur SAB par occupant potentiel
    
    output$indicateur23 <- renderUI({
      infoBox(
        value = HTML(paste0(round(10000 *(sum(v$zone_indic$senv17) + sum(v$zone_indic$senvnd))/(sum(v$zone_indic$occpot17)),0), " m<sup>2</sup>")), 
        title = HTML("surface artificialisée par <br/> occupant potentiel en 2017"), 
        icon = icon("new-window", lib = "glyphicon"),
        color = 'olive',
        width = 12,
        fill = TRUE
      )
  })
  
  # barplot progression SAB
  
  
  sabzone <- reactive({ v$zone_tempo %>%
      select(annee, stot) %>% 
      filter(annee >= input$annee) %>% 
      group_by(annee) %>%
      summarise(surface = sum(stot)) %>%
      mutate(surface_prec = dplyr::lag(surface),
             annee_prec = dplyr::lag(annee),
             annees_ecart = paste0(annee_prec, ' - ', annee),
             surface_prog = (surface - surface_prec) / (annee - annee_prec)
      ) %>%
      filter(!is.na(annee_prec)) %>%
      select (annees_ecart, surface_prog)
  })
  
  output$barres22 <- renderPlotly({
    plot_ly(sabzone(),
            x = ~annees_ecart,
            y = ~surface_prog,
            type = 'bar',
            marker = list(color = col2)
    ) %>%
      layout(xaxis = list(title = 'années'), 
             yaxis = list(title = 'ha par an'))
  })
  
  # courbes évolution SAB versus population
  
  popzone <- reactive({ v$zone_tempo %>%
      select(annee, population, stot) %>%
      filter(annee >= input$annee) %>% 
      group_by(annee) %>%
      summarise(surface = sum(stot), population = sum(population)) %>%
      arrange(annee)
    })
  
  output$lines21 <- renderPlotly({
    plot_ly()%>%
      add_trace(x = popzone()$annee,
                y = 100 * popzone()$surface / popzone()$surface[1],
                name = 'surface artificialisée par le bâti',
                type = 'scatter',
                mode = 'lines+markers',
                marker = list(color = col2),
                line = list(color = col2)
      )%>%
      add_trace(x = v$annees,
                y = 100 * popzone()$population / popzone()$population[1],
                name = 'population',
                type = 'scatter',
                mode = 'lines+markers',
                marker = list(color = col3),
                line = list(color = col3)
      )%>%
      layout(yaxis = list(title = paste('base 100 en ', input$annee, sep = '')),
             legend = list(x = 0, y = 1.1))
  })
  
  # carte leaflet territoire de référence
  
  # préparation data pour carte leaflet
  datas <- reactive({ v$zone_indictempo %>%
      select(c("nom_com", vars[as.numeric(input$indicateur)])) %>%
      rename(indic = vars[as.numeric(input$indicateur)] )
  })
    

  
  
  # fond de carte et légende
  
 
  output$carteter <- renderLeaflet({
    leaflet() %>% 
      # gère la position en arrière plan de la couche des communes
      addMapPane("tuile_com", zIndex = 300) %>%
      
      # couche des communes
      addPolygons(
        data = v$zone_select,
        weight = 1,
        color = 'white',
        fillColor = 'grey',
        fillOpacity = 0,
        group = 'communes',
        options = pathOptions(pane = "tuile_com")
      )%>%
      addTiles(group = "OSM") %>%
      addLayersControl(
        overlayGroups = c("intervalles égaux", "quantiles"),
        baseGroups = c("communes", "OSM"),
        options = layersControlOptions(collapsed = TRUE)
      ) %>%
      addPolygons(data = datas(),
                   label = ~nom_com,
                   popup = ~paste0("<b>", nom_com, "</b><br>", names(ind[as.numeric(input$indicateur)]), " : ", round(indic), unites[as.numeric(input$indicateur)]),
                   fillColor = colorBin("YlOrRd", domain = datas()$indic, bins = 5)(datas()$indic),
                   color = 'white',
                   weight = 1,
                   fillOpacity = 0.8,
                   group = 'intervalles égaux'
                   )  %>%
       addLegend(pal = colorBin("YlOrRd", domain = datas()$indic, bins = 5),
                 values = datas()$indic,
                 position = "bottomright",
                 title = ifelse(input$indicateur %in% c('2', '3'),
                   paste0(names(ind[as.numeric(input$indicateur)]), " entre ", input$annee," et 2017"),             
                   names(ind[as.numeric(input$indicateur)])
                   ),
                 labFormat = labelFormat(suffix = unites[as.numeric(input$indicateur)], big.mark = " "),
                 group = 'intervalles égaux'
       ) %>%
      addPolygons(data = datas(),
                  label = ~nom_com,
                  popup = ~paste0("<b>", nom_com, "</b><br>", names(ind[as.numeric(input$indicateur)]), " : ", round(indic), unites[as.numeric(input$indicateur)]),
                  fillColor = colorQuantile("YlOrRd", domain = datas()$indic, n = 5)(datas()$indic),
                  color = 'white',
                  weight = 1,
                  fillOpacity = 0.8,
                  group = 'quantiles'
      )  %>%
      addLegend(pal = colorQuantile("YlOrRd", domain = datas()$indic, n = 5),
                values = datas()$indic,
                position = "bottomright",
                title = ifelse(input$indicateur %in% c('2', '3'),
                               paste0(names(ind[as.numeric(input$indicateur)]), " entre ", input$annee," et 2017"),             
                               names(ind[as.numeric(input$indicateur)])
                ),
                labFormat = function(type, cuts, p) {
                  n = length(cuts)
                  paste0(as.integer(cuts)[-n], " &ndash; ", as.integer(cuts)[-1], unites[as.numeric(input$indicateur)])},
                group = 'quantiles'
      ) %>%
      hideGroup(c("communes", "quantiles")) %>%
      addScaleBar(position = 'bottomleft', options = scaleBarOptions(imperial = FALSE))
  })

  
  # tableau des indicateurs communaux
  
  output$tableauindic <- renderDataTable(as.data.table(v$zone_indic) %>% 
                                                         select(c(nom_com, insee_com, sartif, partif, cos, sartif_par_hab, sartif_par_op, sartif_evo_men)))
  
  ## OUTPUTS COMPARAISON
  
  gauge_plot <- function(indiv, pop, format = "", suffix = ""){
    p <- plot_ly(
      domain = list(x = c(0, 1), y = c(0, 1)),
      value = indiv,
      type = "indicator",
      mode = "gauge+number",
      gauge = list(
        axis = list(
          range = list(NULL, max(indiv, max(pop)))
        ),
        bar = list(color = col1),
        steps = list(
          list(range = c(min(pop), max(pop)), color = 'lightgray'),
          list(range = c(quantile(pop, 0.25, na.rm = TRUE), quantile(pop, 0.75, na.rm = TRUE)), color = col2)
        )
      ),
      number = list(
        valueformat = format,
        suffix = suffix
      )
    ) %>%
      layout(margin = list(l=20,r=30),
             font = list(color = col1))
    return(p)
  }
  
  output$nomind2 <- renderText({paste0(ind2, " entre ", input$annee, " et 2017")})
  output$nomind3 <- renderText({paste0(ind3, " entre ", input$annee, " et 2017")})
  
  output$gauge31 <- renderPlotly({
    gauge_plot(indiv = v$com_indic$sartif, pop = v$zone_indic$sartif, suffix = " ha")
  })
  
  output$gauge32 <- renderPlotly({
    gauge_plot(indiv = as.numeric(v$comind2), pop = v$ind2$sartif_evo, suffix = " ha")
  })
  
  output$gauge33 <- renderPlotly({
    gauge_plot(indiv = as.numeric(v$comind3), pop = v$ind3$partif_evo, format = ".0f", suffix = " %")
  })
  
  output$gauge34 <- renderPlotly({
    gauge_plot(indiv = v$com_indic$partif, pop = v$zone_indic$partif, format = ".1f", suffix = " %")
  })
  
  output$gauge35 <- renderPlotly({
    gauge_plot(indiv = v$com_indic$cos, pop = v$zone_indic$cos, format = ".0f", suffix = " %")
  })
  
  output$gauge36 <- renderPlotly({
    gauge_plot(indiv = v$com_indic$sartif_par_hab, pop = v$zone_indic$sartif_par_hab, suffix = " m2")
  })
  
  output$gauge37 <- renderPlotly({
    gauge_plot(indiv = v$com_indic$sartif_par_op, pop = v$zone_indic$sartif_par_op, suffix = " m2")
  })
  
  output$gauge38 <- renderPlotly({
    gauge_plot(indiv = v$com_indic$sartif_evo_men, pop = v$zone_indic$sartif_evo_men)
  })
  
  
})
