 

##MC2 data pre-processing
library(DT)
library(raster)
library(tidyverse)
library(tmap)
library(clock)
library(rgdal)
library(lubridate)
library(sf)
library(shiny)
library(htmltools)
library(dygraphs)
library(tm)
library(SnowballC)
library(wordcloud)
library(stopwords)
library(ggplot2)
library(syuzhet)
library(tidytext)
library(igraph)
library(visNetwork)
library(networkD3)
library(tidygraph)
library(ggraph)
library(widyr)
library(ggwordcloud)
library(textplot)
library(proustr)
library(topicmodels)
library(LDAvis)
library(textmineR)
library(servr)
library(stringi)
library(textclean)
library(plotly)

gps=read_csv("data/gps.csv")
names(gps)[names(gps) == 'Timestamp'] = 'timestamp'
gps_locationT=read_csv("data/gps_locationT.csv")
ap=raster("data/MC2-tourist.tif")
car_ass=read_csv("data/car-assignments.csv")

#MC3 part



#MC3 data pre-processing
MC3_1<- read.csv("data/csv-1700-1830.csv")
MC3_2<- read.csv("data/csv-1831-2000.csv")
MC3_3<- read.csv("data/csv-2001-2131.csv")

MC3 <- rbind(MC3_1,MC3_2,MC3_3)
MC3$time<-ymd_hms(MC3$date.yyyyMMddHHmmss.)
MC3$message<-MC3$message%>%tolower()
MC3$author<-MC3$author%>%tolower()
pr_stem_words(MC3,message, language = "english")

#Remove stop words
MC3$message<-MC3$message%>%removeWords(stopwords('english'))%>%  
  # Replace all contraction word to normal  length  
  replace_contraction()%>%  
  # Trim white space in eachsentence sentence    
  str_squish()%>%  
  #Remove number
  gsub(pattern ='[0-9]*',replacement = "",MC3$message)%>%
  # Remove '#'  
  gsub(pattern ='#[a-z]+',replacement = "",MC3$message)%>%  
  # Remove '@' 
  gsub(pattern ='@[a-z]+',replacement = "",MC3$message)%>%
  #Remove 'rt'  
  gsub(pattern ='rt',replacement = "",MC3$message)%>% 
  # Remove punctuation and special symptom 
  gsub(pattern ='[!-$.,?+-=%&]',replacement = "",MC3$message)%>%
  replace_word_elongation()

MC3<-base::subset(MC3,select=c("message","author","type",'time','location'))

CALL<- MC3%>%filter(str_detect(MC3$type,'ccdata'))
MIBLOG<-MC3%>%filter(str_detect(MC3$type,'mbdata'))

MIBLOG$ID <- seq.int(nrow(MIBLOG))
MIBLOG$time_period = cut(MIBLOG$time, breaks="30 min")
MIBLOG$time_period<-MIBLOG$time_period %>% str_replace_all("2014-01-23","")  

MIBLOG$time_period_min = cut(MIBLOG$time, breaks="1 min")
MIBLOG$time_period_min<-MIBLOG$time_period_min %>% str_replace_all("2014-01-23","")

MIBLOG_TOPIC<-MIBLOG%>%
  group_by(time_period) %>% 
  unnest_tokens(word, message) %>%
  dplyr::count(word,sort= TRUE)


set.seed(1234)
MIBLOG_TOPIC %>%
  group_by(time_period) %>% 
  slice_max(order_by = n, n = 8) %>% 
  ggplot(aes(label = word,
             size = n)) +
  geom_text_wordcloud() +
  theme_minimal() +
  facet_wrap(~time_period)

#MC1 part 
employee_records <- read_csv("data/EmployeeRecords.csv")

employee_records <- employee_records %>%
  mutate(Name=paste(FirstName, LastName, sep=".")) %>%
  transform(Name=sub(" ", ".", Name)) %>%
  transform(BirthDate=parse_date_time(BirthDate,"%d/%m/%y"),
            CitizenshipStartDate=parse_date_time(CitizenshipStartDate, "%d/%m/%y"),
            PassportIssueDate=parse_date_time(PassportIssueDate, "%d/%m/%y"),
            PassportExpirationDate=parse_date_time(PassportExpirationDate,
                                                   "%d/%m/%y"),
            CurrentEmploymentStartDate=parse_date_time(CurrentEmploymentStartDate,
                                                       "%d/%m/%y"),
            MilitaryDischargeDate=year(parse_date_time(MilitaryDischargeDate,
                                                       "%d/%m/%y"))) %>%
  mutate(Age=ifelse(year(BirthDate)>2014, 
                    2014-year(BirthDate)+100, 2014-year(BirthDate))) %>%
  dplyr::select(Name, Age,everything()) %>% #deplr
  arrange(Name)

#for network 
employee_email <- read_csv("data/email headers.csv")
employee_email_agg <- employee_email %>%
  transform(Subject=stringi::stri_enc_toascii(Subject)) %>%
  separate_rows(To, sep=",") %>%
  separate(From, c("From","FromEmail"), sep="@") %>%
  separate(To, c("To","ToEmail"), sep="@") %>%
  mutate(To=str_trim(To)) %>%
  transform(Date=parse_date_time(Date, c("mdy_hm","mdy"))) %>%
  transform(SentDate=date(Date),
            SentTime=format(Date, format="%H:%M")) %>%
  mutate(Type=ifelse(str_detect(Subject, "RE:")==TRUE, "reply","direct")) %>%
  dplyr::select(From, To, SentDate, SentTime, Type, Subject) %>%
  dplyr::rename(Source=From, Target=To) %>%  #dplyr
  transform(Source=sub(" ", ".", Source),
            Target=sub(" ", ".", Target))

#remove those who send to all
email_eachday <- employee_email_agg %>%
  filter(Source!=Target) %>%
  group_by(Source, SentDate, Type, Subject) %>%
  dplyr::summarise(n=n()) %>%
  filter(n==53) %>%
  dplyr::select(Source, SentDate, Subject) %>%
  arrange(Source,Subject)

employee_email_agg2 <- email_eachday %>%
  mutate(delete="yes") %>% #mark all emails in email_eachday as "yes" in delete
  right_join(employee_email_agg, by=c("Source"="Source",
                                      "Subject"="Subject",
                                      "Type"="Type",
                                      "SentDate"="SentDate")) %>%
  filter(is.na(delete)) %>% #only keep those without a label in delete
  filter(Source!=Target) %>%
  dplyr::select(Source, Target, SentDate, SentTime, Subject, Type)

#node
employee_nodes <- employee_records %>%
  dplyr::select(Name,Gender,Age,CurrentEmploymentType,CurrentEmploymentTitle,
                CitizenshipCountry) %>%
  dplyr::rename(label=Name, Department=CurrentEmploymentType,
                Title=CurrentEmploymentTitle, Country=CitizenshipCountry) %>%
  arrange(label) %>%
  rowid_to_column("id")

employee_nodes <- employee_nodes %>%
  #add tooltip column
  mutate(title=paste("<p>",label,Gender,Age,"</br>",
                     "<br>",Department, Title,"</br></p>",sep=" ")) %>%
  dplyr::rename(group=Department)

#edges
employee_edges <- employee_email_agg2 %>%
  left_join(employee_nodes%>%dplyr::select(id,label),
            by=c("Source"="label"), suffix=c(".Source", ".Target")) %>%
  left_join(employee_nodes%>%dplyr::select(id,label),
            by=c("Target"="label"), suffix=c(".Source", ".Target")) %>%
  dplyr::rename(from=id.Source, to=id.Target) %>%
  group_by(from, to, Source, Target, Type) %>%
  dplyr::summarise(Sum=n()) %>%
  ungroup() %>%
  group_by(from, to, Source, Target) %>%
  mutate(value=sum(Sum)) %>%
  mutate(Tooltip=paste(Sum, Type)) %>%
  dplyr::select(-Sum, -Type) %>%
  mutate(title=paste("<p>from",Source,"to",Target, "</br>")) %>%
  ungroup() %>%
  distinct(from, to, value, title)





# Define UI for application that draws a histogram
ui <-navbarPage('Multi-row layout with drop-down menus',
  tabPanel('Mini-Challenge 1', fluidPage(
    titlePanel("Employee details and email exchanges within GASTech"),
    
    sidebarLayout(
      sidebarPanel(
        selectInput("employee", "Employee", 
                    choices = employee_records$Name,
                    selected = "Ada.Campo-Corrente"),
        br(),
        tableOutput("record")
      ),
      
      mainPanel(
        tabsetPanel(
          tabPanel("Email Network", visNetworkOutput( "network")),
          tabPanel("Email Detail", 
                   plotOutput("frequency"),
                   br(),
                   dataTableOutput("email")
          )
        ))))),
     ####   
  tabPanel('Mini-Challenge 2',
              fluidPage(

                titlePanel("Employee Car routine tracking"),
                sidebarLayout(
                  sidebarPanel(
                    sliderInput("range", "Time:",
                                min = 0, max = 23,
                                value = c(0,23)),
                    textInput(
                      "first_name",
                      label = "FirstName/id",
                      placeholder = "Enter FirstName/id to track path"),
                    actionButton("goButton", "Go!"),

                   checkboxInput("show_data1",
                                  label = "Show GPS data table",
                                  value = TRUE),
                    checkboxInput("show_data2",                                  label = "Show Car Assignment table",
                                  value = FALSE)
                  ),
               mainPanel(
                    tmapOutput("distPlot"),
                    DT::dataTableOutput("gps_locationT"),
                        DT::dataTableOutput("car_ass")
                    )
                  ))),

  navbarMenu('Mini-Challenge 3',
        tabPanel('Key Word',
          fluidPage(
    titlePanel("Microblog Visualization"),
    sidebarLayout(
            sidebarPanel(
                textInput(
                  'keyword1',
                    label='keyword',
                    placeholder = 'Enter text to be used as key word'),
                textInput(
                    'keyword2',
                    label='keyword2',
                    placeholder = 'Enter text to be used as key word'),
               sliderInput( 'number',
                           label='Maximum Words In Cloud',min=1,max=20,value=5)

                 ),
            mainPanel(
                plotOutput("WordCloud"),
                plotlyOutput('Barchart')
        )
))),

tabPanel('Miblog Table', 
    titlePanel("Microblog Table And Topic Model ") , 
    sidebarLayout(
    sliderInput('number2',
                label='Topics Number',min=1,max=10,value=5),
    mainPanel(
        DT::dataTableOutput('MC33'),
        plotOutput('Topic')
    )
)
)))



server <- function(input,output){
    output$WordCloud <- renderPlot({
        set.seed(1234)
        MIBLOG_TOPIC %>%
            group_by(time_period) %>% 
            slice_max(order_by = n, n = input$number) %>% 
            ggplot(aes(label = word,
                       size = n)) +
            geom_text_wordcloud() +
            theme_minimal() +
            facet_wrap(~time_period)+
            ggtitle('Word frequency in different time periods')})
    
        
output$Barchart<-renderPlotly({pal <- c("red","purple","blue","green")
        MIBLOG_rally<-MIBLOG%>%filter(str_detect(message,input$keyword1)|
                                    str_detect(message,input$keyword2))%>%
        group_by(time_period_min) %>%
        dplyr::summarise(number = n()) 
                                    
        plot_ly(data = MIBLOG_rally,
                x = ~time_period_min,
                y = ~number,
                color = ~time_period_min,
                colors = pal)%>%
                layout(title = 'Event of Abila City Park Rally')
    })

    output$MC33<-DT::renderDataTable({
    
        DT:: datatable(data=MC3%>%select(1:4),
                      options=list(pageLength=5),
                      rownames=FALSE)})
        
        output$Topic<- renderPlot({
            MIBLOG_DTM <-MIBLOG_TOPIC%>%
                cast_dtm(time_period,word,n)
            
            MIBLOG_LDA <- LDA(MIBLOG_DTM, k =input$number2, control = list(seed = 1234))
            
            ap_topics <- tidy(MIBLOG_LDA, matrix = "beta")
            
            ap_top_terms <- ap_topics %>%
                group_by(topic) %>%
                slice_max(beta, n = 5) %>% 
                ungroup() %>%
                arrange(topic, -beta)
            
            ap_top_terms %>%
                mutate(term = reorder_within(term, beta, topic)) %>%
                ggplot(aes(beta, term, fill = factor(topic))) +
                geom_col(show.legend = FALSE) +
                facet_wrap(~ topic, scales = "free") +
                scale_y_reordered()
            })
  
        output$network <- renderVisNetwork({
          visNetwork(employee_nodes, employee_edges) %>%
            visEdges(arrows = 'to', ) %>%
            visOptions(highlightNearest = TRUE, nodesIdSelection = TRUE) %>%
            visLegend() %>%
            visIgraphLayout(layout="layout_with_fr")
        })
        
        output$record <-renderTable({
          employee_records %>%
            dplyr::select(Name,FirstName, LastName, Age, BirthDate,Gender, BirthCountry, CitizenshipCountry,
                          CurrentEmploymentType, CurrentEmploymentTitle, 
                          CurrentEmploymentStartDate, 
                          MilitaryServiceBranch, MilitaryDischargeType,
                          MilitaryDischargeDate) %>%
            filter(Name==input$employee) %>%
            t() %>%
            as.data.frame() %>%
            rownames_to_column(var="Field") %>%
            dplyr::rename(Details="V1")
        })
        
        
        to <- reactive({
          employee_email_agg %>%
            group_by(Source, Target) %>%
            dplyr::summarise(n=n()) %>%
            filter(Source==input$employee) %>%
            ungroup() %>%
            dplyr::select(Target, n)
        })
        
        from <- reactive({
          employee_email_agg %>%
            group_by(Source, Target) %>%
            dplyr::summarise(n=n()) %>%
            filter(Target==input$employee) %>%
            ungroup() %>%
            dplyr::select(Source, n)
        })
        
        output$frequency <- renderPlot(
          merge(x=from(), y=to(), all=TRUE,by.x="Source", by.y="Target") %>%
            mutate(n.x=replace(n.x, is.na(n.x), 0),
                   n.y=replace(n.y, is.na(n.y), 0)) %>%
            dplyr::rename(from=n.x, to=n.y, Name="Source") %>%
            pivot_longer(!Name, names_to="type", values_to="count") %>%
            ggplot(aes(x=reorder(Name, -count), y=count, fill=type)) + 
            geom_bar(stat="identity", position = position_dodge()) +
            theme(plot.title=element_text(face="bold", size=18),
                  axis.title=element_text(size=13),
                  axis.text.x=element_text(angle=45, hjust=1, size=10),
                  legend.position="left") + 
            labs(title=paste("Number of email exchanges with",input$employee),
                 x="Email exchanges from/to employee", 
                 y= "Frequency")
        )
        
        output$email <- renderDataTable(
          employee_email_agg %>%
            filter(Source == input$employee | Target == input$employee),
          options = list(pageLength = 5, autoWidth = FALSE))

##############

output$distPlot <- renderTmap({
  
  input$goButton
  
  tempid=as.numeric(isolate({input$first_name}))
  if(is.na(tempid))
    tempid=car_ass$CarID[which(car_ass$FirstName ==isolate({input$first_name}))]
  
  
  if(length(tempid)==0)
    tempidf=FALSE
  else
    tempidf=TRUE
  
  gps$timestamp=date_time_parse(gps$timestamp,
                                zone = "",
                                format="%m/%d/%Y %H:%M:%S")
  gps$id=as_factor(gps$id)
  gps=gps %>%
    filter(hour(timestamp)>=isolate({input$range[1]}))%>%
    filter(hour(timestamp)<=isolate({input$range[2]}))
  gps_sf=st_as_sf(gps,
                  coords = c("long","lat"),
                  crs=4326)
  if(tempidf){
    gps_path=gps_sf %>%
      group_by(id) %>%
      dplyr::summarize(m=mean(timestamp),
                       do_union=FALSE) %>%
      st_cast("LINESTRING")
    gps_path_selected=gps_path %>%
      filter(id==tempid)
    tmap_mode("view")
    tm_shape(ap)+
      tm_rgb(ap,r=1,g=2,b=3,
             alpha=NA,
             saturation=1,
             interpolate=TRUE,
             max.value=255)+
      tm_shape(gps_path_selected,is.master = NA, projection = NULL, simplify = 1)+
      tm_lines()}
  else{
    tm_shape(ap)+
      tm_rgb(ap,r=1,g=2,b=3,
             alpha=NA,
             saturation=1,
             interpolate=TRUE,
             max.value=255)
  }
  
})


output$gps_locationT <- DT::renderDataTable({
  if(input$show_data1){
    DT::datatable(data = gps_locationT )
  }
})
output$car_ass <- DT::renderDataTable({
  if(input$show_data2){
    DT::datatable(data = car_ass )
  }
})  
}
#####################################
shinyApp(ui=ui,server=server)
