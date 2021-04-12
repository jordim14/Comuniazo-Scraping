#------------------------------------------------------#
#               Pràctica 1: Scraping                   #
#------------------------------------------------------#

# Necesssary libraries

library(rvest)
library(XML)
library(dplyr)
library(stringr)
library(tidyr)

# Links
url <- "http://www.comuniazo.com/comunio/jugadores"
url_pos <- "http://www.comuniazo.com/comunio-apuestas/jugadores?posicion="
pos <- c("porteros", "defensas", "centrocampistas", "delanteros")

# Create df and aux variable
df <- data.frame(matrix(ncol = 20, nrow = 50*11))
k<-0

# Aux function to obtains url's
scraplinks <- function(url){
  # Create an html document from the url
  webpage <- xml2::read_html(url)
  # Extract the URLs
  url_ <- webpage %>%
    rvest::html_nodes("a") %>%
    rvest::html_attr("href")
  # Extract the link text
  link_ <- webpage %>%
    rvest::html_nodes("a") %>%
    rvest::html_text()
  return(tibble(link = link_, url = url_))
}



# Scraping comuniazo stats by player
for(i in 1:11){
  for (j in 1:50){
    
    k<-k+1
    
    # Names
    df[k,1]<-tryCatch(html_nodes(read_html(URLencode(as.character(url))),xpath=paste(paste('/html/body/div[4]/div/div[1]/div[3]/div[2]/table/tbody[',i,']/tr[',j,']/td[1]',sep=''),sep="", collapse=NULL))%>%html_text(),
                      error=function(c)"NA")
    if(df[k,1]!=""){
    # Names with -
    df[k,2]<-gsub(' ','-',df[k,1])
    #print(df[k,2])
    
    # Link for player stats
    df[k,3]<-paste('http://www.comuniazo.com/comunio/jugadores/',df[k,2],sep='')
    
    # Points
    x<-tryCatch(html_nodes(read_html(URLencode(as.character( df[k,3]))),xpath=paste(paste('/html/body/div[4]/div/div[1]/div[3]/div[1]/div[1]/div[1]/div[1]',sep=''),sep="", collapse=NULL))%>%html_text(),error=function(c)"NA")
    if(x=='NA'){
      a<-str_extract(df[k,1], "\\s.+")
      a<-trimws(a, "l")
      df[k,2]<-gsub(' ','-',a)
      df[k,3]<-paste('http://www.comuniazo.com/comunio/jugadores/',df[k,2],sep='')
      x<-tryCatch(html_nodes(read_html(URLencode(as.character( df[k,3]))),xpath=paste(paste('/html/body/div[4]/div/div[1]/div[3]/div[1]/div[1]/div[1]/div[1]',sep=''),sep="", collapse=NULL))%>%html_text(),error=function(c)"NA")
    }
    if(!identical(x,character(0))){ 

      df[k,4]<-x
      
      
      # Home points
      x<-tryCatch(html_nodes(read_html(URLencode(as.character( df[k,3]))),xpath=paste(paste('/html/body/div[4]/div/div[1]/div[3]/div[1]/div[1]/div[2]/div[1]',sep=''),sep="", collapse=NULL))%>%html_text(),error=function(c)"NA")
      df[k,5]<-x
      
      # Away points
      df[k,6]<-as.numeric(df[k,4])-as.numeric(df[k,5])
      
      # Mean
      x<-tryCatch(html_nodes(read_html(URLencode(as.character( df[k,3]))),xpath=paste(paste('/html/body/div[4]/div/div[1]/div[3]/div[1]/div[2]/div[1]/div[1]',sep=''),sep="", collapse=NULL))%>%html_text(),error=function(c)"NA")
      df[k,7]<-gsub(",",".", x)
      
      # Mean home
      x<-tryCatch(html_nodes(read_html(URLencode(as.character( df[k,3]))),xpath=paste(paste('/html/body/div[4]/div/div[1]/div[3]/div[1]/div[2]/div[2]/div[1]/div',sep=''),sep="", collapse=NULL))%>%html_text(),error=function(c)"NA")
      df[k,8]<-gsub(",",".", x)
      
      # Mean away
      x<-tryCatch(html_nodes(read_html(URLencode(as.character( df[k,3]))),xpath=paste(paste('/html/body/div[4]/div/div[1]/div[3]/div[1]/div[2]/div[2]/div[2]/div',sep=''),sep="", collapse=NULL))%>%html_text(),error=function(c)"NA")
      df[k,9]<-gsub(",",".", x)
        
      # In-game value
      x<-tryCatch(html_nodes(read_html(URLencode(as.character( df[k,3]))),xpath=paste(paste('/html/body/div[4]/div/div[1]/div[3]/div[1]/div[3]/div/div[1]',sep=''),sep="", collapse=NULL))%>%html_text(),error=function(c)"NA")
      df[k,10]<-(gsub("[.]"," ", x))
          
      # Game played
      x<-tryCatch(html_nodes(read_html(URLencode(as.character( df[k,3]))),xpath=paste(paste('/html/body/div[4]/div/div[1]/div[3]/div[2]/div[1]/div[1]/div[1]',sep=''),sep="", collapse=NULL))%>%html_text(),error=function(c)"NA")
      df[k,11]<-x
       if (df[k,11] != "0") {    
      # Minutes per game
      x<-tryCatch(html_nodes(read_html(URLencode(as.character( df[k,3]))),xpath=paste(paste('/html/body/div[4]/div/div[1]/div[3]/div[2]/div[1]/div[2]/div/div[1]',sep=''),sep="", collapse=NULL))%>%html_text(),error=function(c)"NA")
      df[k,12]<-as.numeric(gsub("[']","", x))
      
      # Goals
      x<-tryCatch(html_nodes(read_html(URLencode(as.character( df[k,3]))),xpath=paste(paste('/html/body/div[4]/div/div[1]/div[3]/div[2]/div[2]/div/div[1]',sep=''),sep="", collapse=NULL))%>%html_text(),error=function(c)"NA")
      df[k,13]<-x      
      
      # Assists 
      x<-tryCatch(html_nodes(read_html(URLencode(as.character( df[k,3]))),xpath=paste(paste('/html/body/div[4]/div/div[1]/div[3]/div[2]/div[3]/div/div[1]',sep=''),sep="", collapse=NULL))%>%html_text(),error=function(c)"NA")
      df[k,14]<-x        
    
      # Cards          
      x<-tryCatch(html_nodes(read_html(URLencode(as.character( df[k,3]))),xpath=paste(paste('/html/body/div[4]/div/div[1]/div[3]/div[2]/div[4]/div/div[1]',sep=''),sep="", collapse=NULL))%>%html_text(),error=function(c)"NA")
      df[k,15]<-x
                    
      # Age
      x<-tryCatch(html_nodes(read_html(URLencode(as.character( df[k,3]))),xpath=paste(paste('/html/body/div[4]/div/div[1]/div[3]/div[3]/div[1]/div/div[1]',sep=''),sep="", collapse=NULL))%>%html_text(),error=function(c)"NA")
      if(!identical(x,character(0))){ 
      df[k,16]<-x
      }              
      # Height
      x<-tryCatch(html_nodes(read_html(URLencode(as.character( df[k,3]))),xpath=paste(paste('/html/body/div[4]/div/div[1]/div[3]/div[3]/div[2]/div/div[1]',sep=''),sep="", collapse=NULL))%>%html_text(),error=function(c)"NA") 
      if(!identical(x,character(0))){ 
      df[k,17]<-gsub(",",".", x)
      }
      # Weight
      x<-tryCatch(html_nodes(read_html(URLencode(as.character( df[k,3]))),xpath=paste(paste('/html/body/div[4]/div/div[1]/div[3]/div[3]/div[3]/div/div[1]',sep=''),sep="", collapse=NULL))%>%html_text(),error=function(c)"NA") 
      if(!identical(x,character(0))){ 
      df[k,18]<-gsub(",",".", x)
      }
      # Nacionality
      x<-tryCatch(html_nodes(read_html(URLencode(as.character( df[k,3]))),xpath=paste(paste('/html/body/div[4]/div/div[1]/div[3]/div[3]/div[4]/div/div[1]',sep=''),sep="", collapse=NULL))%>%html_text(),error=function(c)"NA") 
      if(!identical(x,character(0))){ 
      df[k,19]<-tryCatch(strsplit(x," ")[[1]][2],error=function(c)"NA")
      }
      # Market value
      x<-tryCatch(html_nodes(read_html(URLencode(as.character( df[k,3]))),xpath=paste(paste('/html/body/div[4]/div/div[1]/div[3]/div[3]/div[5]/div/div[1]',sep=''),sep="", collapse=NULL))%>%html_text(),error=function(c)"NA") 
      if(!identical(x,character(0))){ 
      df[k,20]<-tryCatch(strsplit(x,"M")[[1]][1],error=function(c)"NA")
      }
      # Team
      if(df[k,4] != "NA"){
      x<-scraplinks(df[k,3])
      df[k,21]<-tryCatch(gsub("https://www.comuniazo.com/comunio-apuestas/equipos/","", x[18,2]),error=function(c)"NA")
       
      # Position
      p<-0
      z<-0
      while(p == 0){
        z<-z+1
        if(grepl(df[k,1], tryCatch(html_nodes(read_html(URLencode(as.character( paste(url_pos,pos[z],sep='')))),
                                              xpath=paste(paste('/html/body/div[4]/div',sep=''),sep="", collapse=NULL))%>%html_text(), error=function(c)"NA")) 
        ){
          df[k,22]<-pos[z]
          p<-1
        }
      }
      }
         
    }
    
    }
    }
  }}

# Create the final df and export to csv
Stats_comunio_20_21 <- na.omit(data.frame(df[,1],df[,16:19], df[,21:22], df[,11:15], df[,4:10],df[,20]))
colnames(Stats_comunio_20_21) <- c("Nombre", "Edad", "Altura", "Peso", "Nacionalidad", "Equipo", "Posición", "Partidos", "Minutos por partido", "Goles",
                                   "Asistencias", "Targetas", "Puntos", "Puntos local", "Puntos visitante", "Media total", "Media local", "Media visitante",
                                   "Valor de Comunio", "Valor de mercado" )
write.csv(Stats_comunio_20_21, "Stats_comunio_20_21.csv", fileEncoding = "UTF-8")


    
