#EJD 2019
#This is a Shiny web application. You can run the application by:
#library(shiny)
#runGitHub( "shinyparasitesthroughtime", "eddydowle")



if(!require('shiny'))install.packages('shiny');
library('shiny')
if(!require('tidyverse'))install.packages('tidyverse');
library('tidyverse')
if(!require('RColorBrewer'))install.packages('RColorBrewer');
library('RColorBrewer')
if(!require('rworldmap'))install.packages('rworldmap');
library('rworldmap')
if(!require('colorRamps'))install.packages('colorRamps');
library('colorRamps')


#library(shiny)
#library(tidyverse)
#library(RColorBrewer)
#library(rworldmap)
#library(colorRamps)


#change to your working directory
#setwd("~/Documents/CoAuthorMS/parasitebibsearch/GroupResults/")


#readFilesmod<-function (...) 
#{
#  arguments <- as.list(...) 
#  k = length(arguments)
#  D = list()
#  enc = "UTF-8"
#  for (i in 1:k) {
#    D[[i]] = suppressWarnings(readLines(arguments[[i]], encoding = enc))
#  }
#  D = unlist(D)
#  return(D)
#}

#bring in data from all parasites
#allozyme
#file_list<-list.files(path='AllozymeEraData/Data/AllozymeParasite/',pattern='*.bib',full.names=T)
#citations_genomics_P<-readFilesmod(dput(as.character(file_list))) 
#citations_df_genomics_P_alo<- citations_df_genomics_P 

#nucleotide
#file_list<-list.files(path='NucleotideData/AllParasites102files/',pattern='.*.bib',full.names=T)
#citations_genomics_P<-readFilesmod(dput(as.character(file_list))) 
#citations_df_genomics_P_nuc<- citations_df_genomics_P 

#genomics
#file_list<-list.files(path='GenomicsData/P/',pattern='.*.bib',full.names=T)
#citations_genomics_P<-readFilesmod(dput(as.character(file_list))) 
#citations_df_genomics_P_gen<- citations_df_genomics_P 

#citations_df_genomics_P_alo_ana <- biblioAnalysis(citations_df_genomics_P_alo, sep = ";")
#citations_df_genomics_P_gen_ana <- biblioAnalysis(citations_df_genomics_P_gen, sep = ";")
#citations_df_genomics_P_nuc_ana <- biblioAnalysis(citations_df_genomics_P_nuc, sep = ";")

#citations_df_genomics_P_alo_ana.count<-data.frame(country=citations_df_genomics_P_alo_ana$CO,year=citations_df_genomics_P_alo_ana$Years)

#citations_df_genomics_P_nuc_ana.count<-data.frame(country=citations_df_genomics_P_nuc_ana$CO,year=citations_df_genomics_P_nuc_ana$Years)

#citations_df_genomics_P_gen_ana.count<-data.frame(country=citations_df_genomics_P_gen_ana$CO,year=citations_df_genomics_P_gen_ana$Years)

#write.table(citations_df_genomics_P_alo_ana.count,"citations_df_genomics_P_alo_ana.count",sep="\t",row.names=F,quote=F)
#write.table(citations_df_genomics_P_nuc_ana.count,"citations_df_genomics_P_nuc_ana.count",sep="\t",row.names=F,quote=F)
#write.table(citations_df_genomics_P_gen_ana.count,"citations_df_genomics_P_gen_ana.count",sep="\t",row.names=F,quote=F)

citations_df_genomics_P_alo_ana.count<-read.table("citations_df_genomics_P_alo_ana.count",header=T,row.names=NULL,sep='\t')
citations_df_genomics_P_nuc_ana.count<-read.table("citations_df_genomics_P_nuc_ana.count",header=T,row.names=NULL,sep='\t')
citations_df_genomics_P_gen_ana.count<-read.table("citations_df_genomics_P_gen_ana.count",header=T,row.names=NULL,sep='\t')


ui <- fluidPage(
  
  # Application title
  titlePanel("Parasitology Research by Country"),
  
  # Sidebar with a slider input for number of bins 
  sidebarLayout(
    sidebarPanel(
      radioButtons('period', label = 'Period:',
                   choices = c("Allozyme","Nucleotide","Genomic"),selected='Allozyme'),
      
      radioButtons('timecourse', label = 'Time span:',
                   choices = c("All","Span"),selected='All'),
      radioButtons('colour', label = 'Colour:',
                   choices = c("Yellow-Red","Red","Blue","Pink"),selected='Yellow-Red'),
      
      conditionalPanel(
        condition = "input.timecourse == 'Span'",
        sliderInput("slider2", label = h3("Slider Range"), min = 1961, 
                    max = 2018, value = c(1961, 2018),sep = ""))),
    
    # Show a plot of the generated distribution
    mainPanel(
      plotOutput("main_plot")
    )
  )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  
  
  output$main_plot <-renderPlot({
    if(input$colour=="Yellow-Red"){
      colpalmap<-"heat"
    }
    else if(input$colour=="Red"){
      colpalmap<-c('#fee5d9','#fcae91','#fb6a4a','#de2d26','#a50f15')
    }
    else if(input$colour=="Blue"){
      colpalmap<-c('#f0f9e8','#bae4bc','#7bccc4','#43a2ca','#0868ac')
    }
    else if(input$colour=="Pink"){
      colpalmap<-c('#feebe2','#fbb4b9','#f768a1','#c51b8a','#7a0177')
    }
    if(input$period=="Allozyme") {
      test<-citations_df_genomics_P_alo_ana.count
      titmap<-"Allozyme Period"
    }
    else if(input$period=="Nucleotide") {
      test<-citations_df_genomics_P_nuc_ana.count 
      titmap<-"Nucleotide Period"
    }
    else if(input$period=="Genomic") {
      test<-citations_df_genomics_P_gen_ana.count
      titmap<-"Genomic Period"
    }
    #  else{test<-citations_ana.sum_allozyme_P}
    
    if(input$timecourse=="All") {
      b<-test %>% count(.,country)
      b<-data.frame(country=b$country,value=b$n) %>% na.omit()
      b$country<-gsub('ANTIGUA','Antigua and Barbuda',b$country)
      b$country<-gsub('U ARAB EMIRATES','United Arab Emirates',b$country)
      b$country<-gsub('BOSNIA','Bosnia and herzegovina',b$country)
      P_map_genomics <- joinCountryData2Map(b, joinCode="NAME", nameJoinColumn="country",  verbose=TRUE)
      mapCountryData(P_map_genomics, nameColumnToPlot="value", mapTitle=titmap, catMethod = "fixedWidth", colourPalette = colpalmap, numCats = 90)
    }
    
    else if (input$timecourse=="Span") {
      #      c<-test[between(test$year,2000,2005),]
      c<-test[between(test$year,input$slider2[1],input$slider2[2]),]
      c<-c %>% count(.,country)
      c<-data.frame(country=c$country,value=c$n) %>% na.omit()
      c$country<-gsub('ANTIGUA','Antigua and Barbuda',c$country)
      c$country<-gsub('U ARAB EMIRATES','United Arab Emirates',c$country)
      c$country<-gsub('BOSNIA','Bosnia and herzegovina',c$country)
      P_map_genomics <- joinCountryData2Map(c, joinCode="NAME", nameJoinColumn="country",  verbose=TRUE)
      mapCountryData(P_map_genomics, nameColumnToPlot="value", mapTitle=titmap, catMethod = "fixedWidth", colourPalette = colpalmap, numCats = 90)
    }
  })
}

# Run the application 
shinyApp(ui = ui, server = server)
