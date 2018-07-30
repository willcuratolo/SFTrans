#Author: Will Curatolo 2017
#Data source: Planning Department, City and County of San Francisco


library(RDSTK)
library(leaflet)
library(shiny)
library(shinythemes)
library(htmltools)

#only for testing on my machine
#set working directory
#setwd("/Users/Will.Curatolo/Documents/Website/R code/test")

#########
#DATA
#########

#read in data file
mapping <- read.csv(file="mapping.csv",head=TRUE,sep=",")

#
numProjects <- nrow(mapping)
  #length(mapping$PROJECT.ADDRESS)

#create empty lists
latitude<-list()
longitude<-list()

#get lat & long
for(i in 1:numProjects){
  #current <- toString(mapping[i,"PROJECT.ADDRESS"])
  #have to append city & state
  newAd <- paste(toString(mapping[i,"PROJECT.ADDRESS"]),", San Francisco, CA")

  #get dataframe for address
  info <- street2coordinates(toString(newAd))

  #get lat & long
  oldlat <- latitude
  oldlong <- longitude

  latitude <- c(oldlat,info["latitude"])
  longitude <- c(oldlong,info["longitude"])
}
#forget old temp variables
rm(info)
rm(i)
rm(newAd)
rm(oldlat)
rm(oldlong)

#create dataframe with latitude and longitude
df<-data.frame(cbind(latitude),cbind(longitude))
#merge old mapping dataframe with lat&long df
newmapping <-cbind(mapping,df)

#forget variables
rm(df)
rm(latitude)
rm(longitude)

#########
# Create Map
#########

#create a leaflet object with the dataframe
m <- leaflet()
#set default view to SF
m <- setView(m, lng=-122.44, lat=37.77, zoom = 12)

m <- addTiles(m)

#create markers for the projects
for(i in 1:numProjects){

#set lat/long for the project
long1 <- newmapping[[i,"longitude"]]
lat1 <- newmapping[[i,"latitude"]]

#create text content for popup
content <- paste(sep = "<br/>",
                 htmlEscape(paste("Case #",toString(newmapping[[i,"CASE.."]]))),
                 htmlEscape(toString(newmapping[[i,"PROJECT.ADDRESS"]])),
                 htmlEscape(paste("Memo: ", toString(newmapping[[i,"TIS.Circ.Memo."]]))),
                 htmlEscape(paste("Significant impacts? ", toString(newmapping[[i,"Significant.Impacts.if.any."]]))),
                 htmlEscape(toString(newmapping[[i,"Consultant"]])),
                 htmlEscape(toString(newmapping[[i,"Recommended.Mitigation.Measures"]])),
                 htmlEscape(toString(newmapping[[i,"Recommended.Improvement.Measures"]])),
                 htmlEscape(toString(newmapping[[i,"Recommended.TDM.Measures"]]))
)


m<- addMarkers(m, lng = long1, lat = lat1, popup = content)
}

#forget variables
rm(long1)
rm(lat1)
rm(i)
rm(content)




#########
#Create Shiny Object (web app)
#########



server <- function(input, output, session) {
  
  #Renders the base map with project markers
  output$mymap <- renderLeaflet(m)
  
  #notices when user pushes button; saves address input at time of press
  # address <- eventReactive(input$updateButton, {
  #   input$addressInput
  #   
  # })
  
  #when address is updated, prints the address in 2nd box
  #observe(updateTextInput(session, "errors", value = address()))
 
  
 
  

  #requested radius is .25 mi = 402.336 meters
  rad<- 402.336

  #only update when the button is pushed
  observeEvent(input$updateButton, {


    #the user's entered address
    #entAdd<-toString("1551 Masonic Ave, San Francisco, CA")
    entAdd<-htmlEscape(toString(input$addressInput))

    #get dataframe for address
    info <- street2coordinates(entAdd)

    #get lat & long
    entLat<-info[["latitude"]]
    entLng<-info[["longitude"]]

    if(typeof(entLat)=="double" && typeof(entLat)=="double"){
    #Add circle at the inputted location
    leafletProxy("mymap") %>%
      clearShapes() %>%
      addCircles(lng=entLng, lat=entLat, weight = 1,
                         radius = rad, popup=paste(" 1/4 Mile circle around ",entAdd))
      
      if(info[["confidence"]]<.5){
        updateTextInput(session, "errors", value = "Unsure: be more specific please!")
      } else {
        updateTextInput(session, "errors", value = "We are pretty certain we found it!")
        
      }
    
    }else {
      #just clear the map
      leafletProxy("mymap") %>%
        clearShapes()
      
      updateTextInput(session, "errors", value = "Cant't find it: be more specific please!")
    }
    })

}

ui <- fluidPage(theme=shinytheme("cosmo"),
                #Title
                titlePanel("Transportation-Related Capital Projects in San Francisco"),
                
                sidebarLayout(
                  
                  sidebarPanel(
                    #User Address Input field
                    textInput(inputId = "addressInput", label = "Find projects within 1/4 mile of the entered address", value = "Enter a full address..."),
                    #Update button
                    actionButton("updateButton", "Find Projects"),
                    
                    #Just for testing
                    textInput("errors", "Error notifications:", "None: you haven't searched anything yet!")
                  ), #endsidebarpanel
                  
                  mainPanel(
                    #Map box
                    leafletOutput("mymap"),
                    
                    p("William Curatolo - 2017"),
                    p("Data source: Planning Department, City and County of San Francisco")
                  )#end mainpanel
                )# end sidebarlayout
                
                
                
                
                
                
                
)

shinyApp(ui, server)



