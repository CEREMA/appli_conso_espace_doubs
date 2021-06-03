
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
    updateNavbarPage(session, "app_navbar", selected = "Comparaison entre commune et territoire")
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
                      fluidRow(tags$p("Observatoire départemental de la consommation d'espaces", style="font-size:3em;"))
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
    pal1 = NULL,
    pal2 = NULL,
    pal3 = NULL,
    pal4 = NULL,
    pal5 = NULL,
    pal6 = NULL,
    pal7 = NULL,
#    pal8 = NULL,
    pal9 = NULL,
    com_tempo = NULL,
    zone_tempo = NULL,
    zone_indictempo = NULL,
    tempo = NULL,
    ind2 = NULL,
    ind4 = NULL,
#    ind8 = NULL, 
    comind2 = NULL,
    comind4 = NULL
#    comind8 = NULL
    
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
      
      # palettes de couleur pour la carte dynamique
      v$pal1 <- colorBin("YlOrRd", domain = v$zone_indic$sartif, bins = classIntervals(v$zone_indic$sartif, style = "jenks", n = 6)$brks)

      v$pal3 <- colorBin("YlOrRd", domain = v$zone_indic$partif, bins = 5)

      v$pal5 <- colorBin("YlOrRd", domain = v$zone_indic$cos, bins = 5)
      v$pal6 <- colorBin("YlOrRd", domain = v$zone_indic$sartif_par_hab, bins = 5)
      v$pal7 <- colorBin("YlOrRd", domain = v$zone_indic$sartif_par_op, bins = 5)
      
      v$pal9 <- colorBin("YlOrRd", domain = v$zone_indic$sartif_evo_men, bins = 5)

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
  
  observeEvent(
    c(input$annee, input$id_zone), {
      
      v$zone_indictempo <- v$zone_indic %>%
        dplyr::left_join(v$tempo, by = NULL, copy = FALSE)
      
      v$ind2 <- v$zone_indictempo %>%
        mutate(sartif_evo = sartif - stot) %>%
        select(nom_com, sartif_evo)
      
      v$pal2 <- colorBin("YlOrRd", domain = v$ind2$sartif_evo, bins = classIntervals(v$ind2$sartif_evo, style = "jenks", n = 6)$brks)
      
      
      v$ind4 <- v$zone_indictempo %>%
        mutate(partif_evo = (sartif - stot)/ stot) %>%
        select(nom_com, partif_evo)
      
      v$pal4 <- colorBin("YlOrRd", domain = v$ind4$partif_evo, bins = 5)
      
      # v$ind8 <- v$zone_indictempo %>%
      #   mutate(sartif_evo_par_op = 10000 * (sartif - stot)/(occpot17 - ocpot)) %>%
      #   mutate(sartif_evo_par_op = ifelse(sartif_evo_par_op > 0, sartif_evo_par_op, NA)) %>%
      #   select(nom_com, sartif_evo_par_op)
      # 
      # v$pal8 <- colorBin("YlOrRd", domain = v$ind8$sartif_evo_par_op)
      
    }
  )
  
  observeEvent(
    c(input$annee, input$code_insee), {
      v$comind2 <- v$com_indic$sartif - v$com_tempo[v$com_tempo$annee == input$annee, "stot"][1]
      v$comind4 <- v$comind2 / v$com_tempo[v$com_tempo$annee == input$annee, "stot"][1]
#      v$comind8 <- 10000 * v$comind2 / (v$com_indic$occpot17 - v$com_tempo[v$com_tempo$annee == input$annee, "ocpot"][1])
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
#      addProviderTiles(providers$GeoportailFrance.ignMaps, group = "IGN")%>%
      addProviderTiles(providers$Stamen.Toner, group = "Stamen") %>%
      addMapPane("envref", zIndex = 420) %>%
      addMapPane("envact", zIndex = 410) %>%
      addMapPane("bati", zIndex = 430) %>%
      addLayersControl(
        baseGroups = c("OSM", "IGN", "Stamen"),
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
      )
      
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
  
  
  # fond de carte et légende
  
  gr11 <- "territoire de référence"

  
  output$carteter <- renderLeaflet({
    leaflet() %>% 
      # gère la position en arrière plan des couches région, départements, PNR, EPCI, SCOT
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
#      setView(lng = 6, lat = 47, zoom = 10) %>%
      addTiles(group = "OSM") %>%
      addLayersControl(
        baseGroups = c(i1, i2, i3, i4, i5, i6, i7, i9),
        overlayGroups = c("communes", "OSM"),
        options = layersControlOptions(collapsed = TRUE)
      ) %>%
      addPolygons(data = req(v$zone_indic),
                   label = ~nom_com,
                   popup = ~paste0("<b>", nom_com, "</b><br>", ind1, " : ", round(sartif), " ha"),
                   fillColor = ~v$pal1(sartif),
                   color = 'white',
                   weight = 1,
                   fillOpacity = 0.8,
                   group = i1
                   ) %>%
      addPolygons(data = req(v$ind2),
                  label = ~nom_com,
                  popup = ~paste0("<b>", nom_com, "</b><br>", ind2, " entre ", input$annee, " et 2017 : ", round(sartif_evo), " ha"),
                  fillColor = ~v$pal2(sartif_evo),
                  color = 'white',
                  weight = 1,
                  fillOpacity = 0.8,
                  group = i2
      ) %>%
      addPolygons(data = req(v$zone_indic),
                  label = ~nom_com,
                  popup = ~paste0("<b>", nom_com, "</b><br>", ind3, " : ", round(100 * partif, 1), " %"),
                  fillColor = ~v$pal3(partif),
                  color = 'white',
                  weight = 1,
                  fillOpacity = 0.8,
                  group = i3
      ) %>%
      addPolygons(data = req(v$ind4),
                  label = ~nom_com,
                  popup = ~paste0("<b>", nom_com, "</b><br>", ind4, " entre ", input$annee, " et 2017 : ", round(100 * partif_evo, 1), " %"),
                  fillColor = ~v$pal4(partif_evo),
                  color = 'white',
                  weight = 1,
                  fillOpacity = 0.8,
                  group = i4
      ) %>%
      addPolygons(data = req(v$zone_indic),
                  label = ~nom_com,
                  popup = ~paste0("<b>", nom_com, "</b><br>", ind5, " : ", round(100 * cos), " %"),
                  fillColor = ~v$pal5(cos),
                  color = 'white',
                  weight = 1,
                  fillOpacity = 0.8,
                  group = i5
      ) %>%
      addPolygons(data = req(v$zone_indic),
                  label = ~nom_com,
                  popup = ~paste0("<b>", nom_com, "</b><br>", ind6, " : ", round(sartif_par_hab), " m2"),
                  fillColor = ~v$pal6(sartif_par_hab),
                  color = 'white',
                  weight = 1,
                  fillOpacity = 0.8,
                  group = i6
      ) %>%    
      addPolygons(data = req(v$zone_indic),
                  label = ~nom_com,
                  popup = ~paste0("<b>", nom_com, "</b><br>", ind7, " : ", round(sartif_par_op), " m2"),
                  fillColor = ~v$pal7(sartif_par_op),
                  color = 'white',
                  weight = 1,
                  fillOpacity = 0.8,
                  group = i7
      ) %>%
      # addPolygons(data = req(v$ind8),
      #             label = ~nom_com,
      #             popup = ~paste0("<b>", nom_com, "</b><br>", ind8, " : ", round(sartif_evo_par_op), " m2"),
      #             fillColor = ~v$pal8(sartif_evo_par_op),
      #             color = 'white',
      #             weight = 1,
      #             fillOpacity = 0.8,
      #             group = i8
      # ) %>%
      addPolygons(data = req(v$zone_indic),
                  label = ~nom_com,
                  popup = ~paste0("<b>", nom_com, "</b><br>", ind9, " entre 2012 et 2017 : ", round(sartif_evo_men), " ménages"),
                  fillColor = ~v$pal9(sartif_evo_men),
                  color = 'white',
                  weight = 1,
                  fillOpacity = 0.8,
                  group = i9
      ) %>%
      addLegend(pal = v$pal1, 
                values = v$zone_indic$sartif,
                position = "bottomright",
                title = ind1,
                group = i1,
                className = paste0("info legend ", i1),
                labFormat = labelFormat(suffix = " ha", big.mark = " ")
      ) %>%
      addLegend(pal = v$pal2, 
                values = v$ind2,
                position = "bottomright",
                title = ind2,
                group = i2,
                className = paste0("info legend ", i2),
                labFormat = labelFormat(suffix = " ha", big.mark = " ")
      ) %>%
      addLegend(pal = v$pal3, 
                values = v$zone_indic$partif,
                position = "bottomright",
                title = ind3,
                group = i3,
                className = paste0("info legend ", i3),
                labFormat = labelFormat(suffix = " %", transform = function(x) 100 * x, big.mark = " ")
      ) %>%
      addLegend(pal = v$pal4, 
                values = v$ind4,
                position = "bottomright",
                title = ind4,
                group = i4,
                className = paste0("info legend ", i4),
                labFormat = labelFormat(suffix = " %", transform = function(x) 100 * x, big.mark = " ")
      ) %>%
      addLegend(pal = v$pal5, 
                values = v$zone_indic$cos,
                position = "bottomright",
                title = ind5,
                group = i5,
                className = paste0("info legend ", i5),
                labFormat = labelFormat(suffix = " %", transform = function(x) 100 * x, big.mark = " ")
      ) %>%
      addLegend(pal = v$pal6, 
                values = v$zone_indic$sartif_par_hab,
                position = "bottomright",
                title = ind6,
                group = i6,
                className = paste0("info legend ", i6),
                labFormat = labelFormat(suffix = " m2", big.mark = " ")
      ) %>%
      addLegend(pal = v$pal7, 
                values = v$zone_indic$sartif_par_op,
                position = "bottomright",
                title = ind7,
                group = i7,
                className = paste0("info legend ", i7),
                labFormat = labelFormat(suffix = " m2", big.mark = " ")
      ) %>%
      # addLegend(pal = v$pal8, 
      #           values = v$ind8,
      #           position = "bottomright",
      #           title = i8,
      #           group = i8,
      #           className = paste0("info legend ", i8),
      #           labFormat = labelFormat(suffix = " m2", big.mark = " ")
      # ) %>%
      addLegend(pal = v$pal9, 
                values = v$zone_indic$sartif_evo_men,
                position = "bottomright",
                title = ind9,
                group = i9,
                className = paste0("info legend ", i9),
                labFormat = labelFormat(suffix = " ménages", big.mark = " ")
      ) %>%
      hideGroup(c("OSM")) %>%
      
      # fonction qui permet de gérer l'affichage de la légende pour les basegroup : voir https://github.com/rstudio/leaflet/issues/477
      htmlwidgets::onRender("
      function(el, x) {
         var updateLegend = function () {
            var selectedGroup = document.querySelectorAll('input:checked')[0].nextSibling.innerText.substr(1);

            document.querySelectorAll('.legend').forEach(a => a.hidden=true);
            document.querySelectorAll('.legend').forEach(l => {
               if (l.classList.contains(selectedGroup)) l.hidden=false;
            });
         };
         updateLegend();
         this.on('baselayerchange', el => updateLegend());
      }"
      )
  })
  observeEvent(c(input$codeinsee, input$annee, input$id_zone), {
    leafletProxy("carteter") %>%
      htmlwidgets::onRender("
      function(el, x) {
         var updateLegend = function () {
            var selectedGroup = document.querySelectorAll('input:checked')[0].nextSibling.innerText.substr(1);

            document.querySelectorAll('.legend').forEach(a => a.hidden=true);
            document.querySelectorAll('.legend').forEach(l => {
               if (l.classList.contains(selectedGroup)) l.hidden=false;
            });
         };
         updateLegend();
         this.on('baselayerchange', el => updateLegend());
      }"
      )
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
  output$nomind4 <- renderText({paste0(ind4, " entre ", input$annee, " et 2017")})
  
  output$gauge31 <- renderPlotly({
    gauge_plot(indiv = v$com_indic$sartif, pop = v$zone_indic$sartif, suffix = " ha")
  })
  
  output$gauge32 <- renderPlotly({
    gauge_plot(indiv = as.numeric(v$comind2), pop = v$ind2$sartif_evo, suffix = " ha")
  })
  
  output$gauge33 <- renderPlotly({
    gauge_plot(indiv = v$com_indic$partif, pop = v$zone_indic$partif, format = ".0%")
  })
  
  output$gauge34 <- renderPlotly({
    gauge_plot(indiv = as.numeric(v$comind4), pop = v$ind4$partif_evo, format = ".0%")
  })
  
  output$gauge35 <- renderPlotly({
    gauge_plot(indiv = v$com_indic$cos, pop = v$zone_indic$cos, format = ".0%")
  })
  
  output$gauge36 <- renderPlotly({
    gauge_plot(indiv = v$com_indic$sartif_par_hab, pop = v$zone_indic$sartif_par_hab, suffix = " m2")
  })
  
  output$gauge37 <- renderPlotly({
    gauge_plot(indiv = v$com_indic$sartif_par_op, pop = v$zone_indic$sartif_par_op, suffix = " m2")
  })
  
  # output$gauge38 <- renderPlotly({
  #   gauge_plot(indiv = as.numeric(v$comind8), pop = v$ind8$sartif_evo_par_op)
  # })
  
  output$gauge39 <- renderPlotly({
    gauge_plot(indiv = v$com_indic$sartif_evo_men, pop = v$zone_indic$sartif_evo_men)
  })
  
  
})
