library("tidyverse")
library('dplyr')
library('lubridate')
library('visNetwork')
library('networkD3')
library('ggplot2')


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
  select(Name, Age,everything()) %>%
  arrange(Name)

#for network 
employee_email <- read.csv("data/email headers.csv", encoding="UTF-8")
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
  select(From, To, SentDate, SentTime, Type, Subject) %>%
  rename(Source=From, Target=To) %>%
  transform(Source=sub(" ", ".", Source),
            Target=sub(" ", ".", Target))


#node
employee_nodes <- employee_records %>%
  select(Name,Gender,Age,CurrentEmploymentType,CurrentEmploymentTitle,
         CitizenshipCountry) %>%
  rename(label=Name, Department=CurrentEmploymentType,
         Title=CurrentEmploymentTitle, Country=CitizenshipCountry) %>%
  arrange(label) %>%
  rowid_to_column("id")

employee_nodes <- employee_nodes %>%
  #add tooltip column
  mutate(title=paste("<p>",label,Gender,Age,"</br>",
                     "<br>",Department, Title,"</br></p>",sep=" ")) %>%
  rename(group=Department)

#edges
employee_edges <- employee_email_agg %>%
  left_join(employee_nodes%>%select(id,label),
            by=c("Source"="label"), suffix=c(".Source", ".Target")) %>%
  left_join(employee_nodes%>%select(id,label),
            by=c("Target"="label"), suffix=c(".Source", ".Target")) %>%
  rename(from=id.Source, to=id.Target) %>%
  group_by(from, to, Source, Target, Type) %>%
  summarise(Sum=n()) %>%
  ungroup() %>%
  group_by(from, to, Source, Target) %>%
  mutate(value=sum(Sum)) %>%
  mutate(Tooltip=paste(Sum, Type)) %>%
  select(-Sum, -Type) %>%
  mutate(Tooltip=paste(Tooltip, collapse=", ")) %>%
  mutate(title=paste("<p>from",Source,"to",Target, "</br>",
                     Tooltip, "</br>")) %>%
  ungroup() %>%
  distinct(from, to, value, title)

 
#define UI
mc1UI <- function(id) { 
  tagList(
    fluidPage(
      titlePanel("Employee details and email exchanges within GASTech"),
     
       sidebarLayout(
        sidebarPanel(
          selectInput(NS(id, "employee"), "Employee", 
                      choices = employee_records$Name,
                      selected = "Ada.Campo-Corrente"),
          br(),
          tableOutput(NS(id,"record"))
        ),
        
        mainPanel(
          tabsetPanel(
            tabPanel("Email Network", visNetworkOutput(NS(id, "network"))),
            tabPanel("Email Detail", 
                     plotOutput(NS(id,"frequency")),
                     br(),
                     dataTableOutput(NS(id,"email"))
                     )
            )
          )
        
        ) #end of sidebarLayout
      ) #end of fluidPage
  ) #end of taglist
} #end of UI

#define server

mc1Server <- function(id) {
  moduleServer(id, function(input, output, session) {
    output$network <- renderVisNetwork({
      visNetwork(employee_nodes, employee_edges) %>%
        visEdges(arrows = 'to', ) %>%
        visOptions(highlightNearest = TRUE, nodesIdSelection = TRUE) %>%
        visLegend() %>%
        visIgraphLayout(layout="layout_with_fr")
    })
    
    output$record <-renderTable({
      employee_records %>%
        select(Name,FirstName, LastName, Age, BirthDate,Gender, BirthCountry, CitizenshipCountry,
               CurrentEmploymentType, CurrentEmploymentTitle, 
               CurrentEmploymentStartDate, 
               MilitaryServiceBranch, MilitaryDischargeType,
               MilitaryDischargeDate) %>%
        filter(Name==input$employee) %>%
        t() %>%
        as.data.frame() %>%
        rownames_to_column(var="Field") %>%
        rename(Details="V1")
       })
    
    
    to <- reactive({
      employee_email_agg %>%
      group_by(Source, Target) %>%
      summarise(n=n()) %>%
      filter(Source==input$employee) %>%
      ungroup() %>%
      select(Target, n)
      })
    
    from <- reactive({
      employee_email_agg %>%
      group_by(Source, Target) %>%
      summarise(n=n()) %>%
      filter(Target==input$employee) %>%
      ungroup() %>%
      select(Source, n)
    })
    
    output$frequency <- renderPlot(
      merge(x=from(), y=to(), all=TRUE,by.x="Source", by.y="Target") %>%
        mutate(n.x=replace(n.x, is.na(n.x), 0),
               n.y=replace(n.y, is.na(n.y), 0)) %>%
        rename(from=n.x, to=n.y, Name="Source") %>%
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
  })
}

