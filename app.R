library(dplyr)
library(sf)
library(shiny)
library(leaflet)
library(htmltools)
library(htmlwidgets)
library(RColorBrewer)

Remerciements <- list(
  tags$p("Cette interface a été réalisée en R Shiny à partir des données élaborées par l'Atelier des communes"),
  tags$a(href= "https://www.data.gouv.fr/fr/datasets/croisement-des-donnees-demographique-insee-2020-avec-les-resultats-des-elections-europeennes-2024/", "Lien vers les données"),
  tags$p(""),
  tags$p("Code et données accessibles sur Github"),
  tags$a(href= "https://github.com/ClaudeGrasland/geocitoyens", "lien vers github"),
  tags$p(""),
  tags$p("Contacter l'auteur : claude.grasland@wanadoo.fr"))



# load data in 'global' chunk so it can be shared by all users of the dashboard
mapcirc<-readRDS("data_shiny/mapcirc.RDS") %>% st_transform(4326)
#mapcom<-readRDS("france/mapcom.RDS") %>% st_transform(4326)
mapdon<-readRDS("data_shiny/iris_don.RDS") %>% st_transform(4326)
coo<-st_coordinates(st_centroid(mapdon))
mapdon$lng<-coo[,1]
mapdon$lat<-coo[,2]

mapcirc<-mapcirc[order(mapcirc$id_circo),]
selcirc<-mapcirc$libelle
names(selcirc)<-selcirc
#x<-"Tout"
#names(x)<-"Tout"
#selcirc<-c(x,selcirc)
head(selcirc)


Exploration <- fluidRow(
  headerPanel(h4("PARAMETRES")),
  column(4,
         wellPanel(
           selectInput("Circo", label = "Choix de la circnscription",
                       choices = selcirc, 
                       selected = "Val-de-Marne - 4e circonscription"  ),
           radioButtons("cercle","Visualiser les effectifs ?",
                        choices=c("Oui"=TRUE,"Non"=FALSE),
                        selected=FALSE),
    #       selectInput("Variable", label = "Choix du bloc politique",
    #                   choices = c("FrontPopulaire","CentreDroite","ExtremeDroite","Divers"), 
    #                   selected = "FrontPopulaire"),

           h4(" "),
           h4("_____________________"),
           h4(" "),
           h4("Plus de détails ? "),
           h4(" "),          
           h5("Cliquer sur un un quartier Iris .."),
           h4(" "),
           h4("_____________________"),
           h4(" "),
           h4("Problème de chargement "),
           h4(" "),
           h6("Le nombre maximum de connexions simultanées est de 40 utilisateurs"),
           h6("Si le délais est trop long, essayez ultérieurement")
         )),
  column(8,
         tabsetPanel(
           tabPanel("Vote par bloc",
                    selectInput("Variable", label = "Choix du bloc politique",
                                choices = c("FrontPopulaire","CentreDroite","ExtremeDroite","Divers"), 
                                selected = "FrontPopulaire"),
                    leafletOutput("mymap2", height = 600)),
            tabPanel("Abstention",
                    leafletOutput("mymap3", height = 600)),
           tabPanel("Bloc Majoritaire",
                    leafletOutput("mymap1", height = 600)),
           
          
           tabPanel("Sources", 
                    Remerciements)


         )

  ))




ui<- navbarPage("ANALYSE DES BLOCS POLITIQUES ISSUS DES ELECTIONS EUROPEENNES DE 2024",
            tabPanel("(c) Grasland C., Université Paris Cité, UMR 8504 Géographie-cités & FR 2007 CIST, Juin 2024",Exploration)

)


# Define server logic required to draw a histogram
server <- function(input, output) {
  
  output$mymap1<- renderLeaflet({
    mapiris<-mapdon %>% filter(Circonscription_nom==input$Circo)
    macirc<-mapcirc %>% filter(libelle==input$Circo)
 #   latref<-mean(mapiris$lat)
#    lngref<-mean(mapiris$lng)
#    zoomref<-12
    # Choix de la variable
    myvar <-as.factor(mapiris$Bloc_maximum)
  
    
    factpal <- colorFactor(c("blue","sienna","deeppink"), mapdon$Bloc_maximum)
    

 
    

    # Préparation des popups
    mypopups <- lapply(seq(nrow(mapiris)), function(i) {
      paste0(  paste("Circonscription   : ",mapiris$Circonscription_nom[i]), '<br>',
               paste("Commune           : ",mapiris$Commune[i]), '<br>',
               paste("Iris              : " ,mapiris$Iris[i]), '<br>', 
               paste("Inscrits : " ,mapiris$nb_inscrits[i]), '<br>',                      
               paste("Votants  : " ,mapiris$nb_votants[i]), '<br>', 
               paste("Taux d'abstention : " ,mapiris$tx_abstention[i]), '<br>', 
               paste("Bloc vainqueur : " ,mapiris$Bloc_maximum[i]), '<br>', 
               paste("% Front Populaire : " ,mapiris$pct_FrontPopulaire[i]), '<br>', 
               paste("% Centre + Droite : " ,mapiris$pct_CentreDroite[i]), '<br>', 
               paste("% RN + Reconquête : " ,mapiris$pct_ExtremeDroite[i]), '<br>', 
               paste("% Autres listes   : " ,mapiris$pct_Divers[i]), '<br>', 
               paste("Nb. Front Populaire : " ,mapiris$nb_FrontPopulaire[i]), '<br>', 
               paste("Nb. Centre + Droite : " ,mapiris$nb_CentreDroite[i]), '<br>', 
               paste("Nb. RN + Reconquête : " ,mapiris$nb_ExtremeDroite[i]), '<br>', 
               paste("Nb. Autres listes   : " ,mapiris$nb_Divers[i])
               
      ) 
    })
    mypopups<-lapply(mypopups, htmltools::HTML)
    
    
    
    
    # Réalisation de la carte
    map <- leaflet() %>% 
      #    addTiles() %>%
      addProviderTiles('Esri.WorldTopoMap') %>%
#      setView(lat = latref, lng=lngref) %>% 
      addPolygons(data = mapiris,
                  fillColor = ~factpal(myvar),
                  fillOpacity = 0.5,
                  color = "white",
                  label = myvar,
                  popup = mypopups,
                  weight = 1,
                  highlightOptions = highlightOptions(weight = 3, color = 'green')) %>%

#      addPolygons(data = mapcom,
#                  fill = FALSE,
#                  color = "black",
#                  weight = 2) %>%
      addPolygons(data = macirc,
                  fill = FALSE,
                  color = "red",
                  weight = 3) 
  
    map
  })
  
  
  output$mymap2<- renderLeaflet({
   mapiris<-mapdon %>% filter(Circonscription_nom==input$Circo)
   macirc<-mapcirc %>% filter(libelle==input$Circo)
    mapiris$Z<-mapiris[[paste0("pct_",input$Variable)]]
    mapiris$V<-mapiris[[paste0("nb_",input$Variable)]]  
    mapiris$P<-mapiris$nb_votants
    # Choix de la variable
    myvar <-mapiris$Z
    # Choix des classes 
    #    mycut<-c(0,1,2, 5, 10, 20,40,100)
    mycut<-quantile(mapiris$Z,c(0,1/5,2/5,3/5,4/5,1))
    # Choix de la palette (c'est une fonction !)
    mypal <- colorBin('RdYlBu', 
                      myvar,
                      bins=mycut,
                      reverse = TRUE)
    
    
    # Calcul du diamètre des cercles
    myradius <-10*sqrt(mapiris$V/max(mapiris$P))
    
    # Préparation des popups
    mypopups <- lapply(seq(nrow(mapiris)), function(i) {
      paste0(  paste("Circonscription   : ",mapiris$Circonscription_nom[i]), '<br>',
               paste("Commune           : ",mapiris$Commune[i]), '<br>',
               paste("Iris              : " ,mapiris$Iris[i]), '<br>', 
               paste("Inscrits : " ,mapiris$nb_inscrits[i]), '<br>',                      
               paste("Votants  : " ,mapiris$nb_votants[i]), '<br>', 
               paste("Taux d'abstention : " ,mapiris$tx_abstetntion[i]), '<br>', 
               paste("Bloc vainqueur : " ,mapiris$Bloc_maximum[i]), '<br>', 
               paste("% Front Populaire : " ,mapiris$pct_FrontPopulaire[i]), '<br>', 
               paste("% Centre + Droite : " ,mapiris$pct_CentreDroite[i]), '<br>', 
               paste("% RN + Reconquête : " ,mapiris$pct_ExtremeDroite[i]), '<br>', 
               paste("% Autres listes   : " ,mapiris$pct_Divers[i]), '<br>', 
               paste("Nb. Front Populaire : " ,mapiris$nb_FrontPopulaire[i]), '<br>', 
               paste("Nb. Centre + Droite : " ,mapiris$nb_CentreDroite[i]), '<br>', 
               paste("Nb. RN + Reconquête : " ,mapiris$nb_ExtremeDroite[i]), '<br>', 
               paste("Nb. Autres listes   : " ,mapiris$nb_Divers[i])
               
      ) 
    })
    mypopups<-lapply(mypopups, htmltools::HTML)
    
    
    
    
    # Réalisation de la carte
    map <- leaflet() %>% 
      #    addTiles() %>%
      addProviderTiles('Esri.WorldTopoMap') %>%
   #    setView(lat = latref, lng=lngref) %>% 
      addPolygons(data = mapiris,
                  fillColor = ~mypal(Z),
                  fillOpacity = 0.5,
                  color = "white",
                  label = ~Z,
                  popup = mypopups,
                  weight = 1,
                  highlightOptions = highlightOptions(weight = 3, color = 'green')) %>%
      addLegend(data = mapiris,
                pal = mypal, 
                title = "% votants",
                values =~Z, 
                position = 'topright') %>%

#      addPolygons(data = mapcom,
#                  fill = FALSE,
#                  color = "gray80",
#                  weight = 1) %>%
      addPolygons(data = macirc,
                  fill = FALSE,
                  color = "red",
                 weight = 2) 
    
    
    if (input$cercle) { map<-map %>% addCircleMarkers(data=mapiris,
                                         lat = ~lat,
                                         lng = ~lng,
                                         radius = myradius,
                                         stroke = FALSE,
                                         fillColor = "black",
                                         fillOpacity = 0.5)  }
    map
  })
  
  
  
  output$mymap3<- renderLeaflet({
    mapiris<-mapdon %>% filter(Circonscription_nom==input$Circo)
    macirc<-mapcirc %>% filter(libelle==input$Circo)
    mapiris$Z<-mapiris$tx_abstention
    mapiris$V<-mapiris$nb_abstention  
    mapiris$P<-mapiris$nb_inscrits
    # Choix de la variable
    myvar <-mapiris$Z
    # Choix des classes 
    #    mycut<-c(0,1,2, 5, 10, 20,40,100)
    mycut<-quantile(mapiris$Z,c(0,1/5,2/5,3/5,4/5,1))
    # Choix de la palette (c'est une fonction !)
    mypal <- colorBin('YlOrBr', 
                      myvar,
                      bins=mycut,
                      reverse = FALSE)
    
    
    # Calcul du diamètre des cercles
    myradius <-10*sqrt(mapiris$V/max(mapiris$P))
    
    # Préparation des popups
    
    
    # Préparation des popups
    mypopups <- lapply(seq(nrow(mapiris)), function(i) {
      paste0(  paste("Circonscription   : ",mapiris$Circonscription_nom[i]), '<br>',
               paste("Commune           : ",mapiris$Commune[i]), '<br>',
               paste("Iris              : " ,mapiris$Iris[i]), '<br>', 
               paste("Inscrits : " ,mapiris$nb_inscrits[i]), '<br>',                      
               paste("Votants  : " ,mapiris$nb_votants[i]), '<br>', 
               paste("Taux d'abstention : " ,mapiris$tx_abstention[i]), '<br>', 
               paste("Bloc vainqueur : " ,mapiris$Bloc_maximum[i]), '<br>', 
               paste("% Front Populaire : " ,mapiris$pct_FrontPopulaire[i]), '<br>', 
               paste("% Centre + Droite : " ,mapiris$pct_CentreDroite[i]), '<br>', 
               paste("% RN + Reconquête : " ,mapiris$pct_ExtremeDroite[i]), '<br>', 
               paste("% Autres listes   : " ,mapiris$pct_Divers[i]), '<br>', 
               paste("Nb. Front Populaire : " ,mapiris$nb_FrontPopulaire[i]), '<br>', 
               paste("Nb. Centre + Droite : " ,mapiris$nb_CentreDroite[i]), '<br>', 
               paste("Nb. RN + Reconquête : " ,mapiris$nb_ExtremeDroite[i]), '<br>', 
               paste("Nb. Autres listes   : " ,mapiris$nb_Divers[i])
               
      ) 
    })
    mypopups<-lapply(mypopups, htmltools::HTML)
    
    
    
    
    # Réalisation de la carte
    map <- leaflet() %>% 
      #    addTiles() %>%
      addProviderTiles('Esri.WorldTopoMap') %>%
#      setView(lat = latref, lng=lngref) %>% 
      addPolygons(data = mapiris,
                  fillColor = ~mypal(Z),
                  fillOpacity = 0.5,
                  color = "white",
                  label = ~Z,
                  popup = mypopups,
                  weight = 1,
                  highlightOptions = highlightOptions(weight = 3, color = 'green')) %>%
      addLegend(data = mapiris,
                pal = mypal, 
                title = "% inscrits",
                values =~Z, 
                position = 'topright') %>%

#      addPolygons(data = mapcom,
#                  fill = FALSE,
#                  color = "gray80",
#                  weight = 1) %>%
      addPolygons(data = macirc,
                  fill = FALSE,
                  color = "red",
                 weight = 2) 
    
    if (input$cercle) {map <- map %>% addCircleMarkers(data=mapiris,
                                        lat = ~lat,
                                        lng = ~lng,
                                        radius = myradius,
                                        stroke = FALSE,
                                        fillColor = "black",
                                        fillOpacity = 0.5)}
    map
  })
  
  
}
# Run the application 
shinyApp(ui = ui, server = server)













