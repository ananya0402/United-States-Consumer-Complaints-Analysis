#Load the libraries
library("shiny")
library("ggplot2")
library("dplyr")
library("shinyWidgets")
library("data.table")
library("stringr")
library("tm")
library("SnowballC")
library("wordcloud")
library("RColorBrewer")
library("leaflet")


#user-interface script
ui<-fluidPage(
  
  #set a background for the shiny application
  setBackgroundImage(
   src = "b_g.png"
  ),
  
  #title is given to the visualisation
  headerPanel("Consumer Complaints Database of the United States from 2016 to 2019"),
  #font size and the color of the text is adjusted
  tags$head(tags$b(tags$style('h1 {color:white;font-size: 40px}'))),
  
  #logo for the title 
  titlePanel(title=div(img(src="cfpb logo.png"),height = 30, width = 200)),
  
  #some description is provided regarding the CFPB
  HTML(
    paste(
      h3("The Consumer Financial Protection Bureau works to hold financial institutions accountable in matters 
         related to financial products & services provided by them and manages a database which is a collection of 
         complaints about consumers’ issues with the utilities provided to them by particular companies.
         In recent times, the number of complaints is increasing at alarming levels as the purchasing capacity for financial
        products and services is increasing for the residents of the USA. It is predicted that because of the rise in awareness 
         among people about such a platform where their voice can be heard, there will be a rise in the number of complaints.")
      )
      ),
  #font size and the color of the text is adjusted
  tags$head(tags$style("h3 {color: white;
                       font-size: 20px;
                       }"
                         )
  ),
  
  #description about the first graph which shows the year wise distribution of complaints
  mainPanel(
    HTML(
      paste(
        h3("The volume of complaints for each of the years is shown in the graph below.Initially, a rise in the number of 
           complaints can be seen per year with the main reason of people getting aware of such a platform available. 
           But we see a great dip in the number of complaints in 2018, which is due to the market crash held in 
           that year. 2019 sees a boom in the market with people purchasing more financial products and thus more 
          complaints being registered.")
  )
    )),
  
  #the first graph showing the year wise distribution of complaints is plotted

  mainPanel(fluidRow(
              plotOutput("month"))
            
  ),
  sidebarPanel(
    #input for the year is taken from a slider bar
           sliderInput("Year", "Please Select year",
                         min = 2016, max = 2019,value = 2016, sep = "")
                         
  ),
  
  mainPanel(
    HTML(
      paste(
        h3("This is a performance chart of the companies showing the top 20 companies 
           with the maximum number of complaints for the chosen year. For the entire period, EQUIFAX, Experian Information Solutions, Bank of America and TRANSUNION Intermediate Holdings 
           are the companies attracting the largest amount of complaints.")
      )
    )),
  mainPanel(
    plotOutput("Companies")
  ),
 
  mainPanel(
    HTML(
      paste(
        h3("The mode of complaint submission, the amount of disputed complaints, the amount of resolved complaints, and the products for which the complaints have been 
           registered against a company in a particular year can be seen below.")
      )
    ),
    uiOutput("features")
    
  ),
  #input for the company who's features the user wants to see can be selected from a drop down menu
  sidebarPanel(
    selectInput("Company","Select Company",
                choices = c("BANK OF AMERICA, NATIONAL ASSOCIATION","EQUIFAX, INC.","Experian Information Solutions Inc.","TRANSUNION INTERMEDIATE HOLDINGS, INC.",
                            "TD BANK US HOLDING COMPANY", "PORTFOLIO RECOVERY ASSOCIATES INC", "PNC Bank N.A.", 
                            "ONEMAIN FINANCIAL HOLDINGS, LLC.","WELLS FARGO & COMPANY", "JPMORGAN CHASE & CO.", 
                            "CITIBANK, N.A.", "DISCOVER BANK", "Ditech Financial LLC", "ENCORE CAPITAL GROUP INC.", "U.S. BANCORP",
                            "CAPITAL ONE FINANCIAL CORPORATION", "NATIONSTAR MORTGAGE", "AMERICAN EXPRESS COMPANY", "SYNCHRONY FINANCIAL"))
  ),
    #the feature the user wants to see can be chosen from the drop down menu
    sidebarPanel(
      fluidRow(
        column(width = 12, offset = 0,
               selectInput("Feature","Select feature",
                           choices = c("Mode of Submission", "Resolved complaints","Products","Company's Response"))))
    ),
  
  mainPanel(
    HTML(
      paste(
        h3("The distribution of the number of complaints for each of the states can be seen from the map
             below.California, Texas, Florida and New York are the states that receive the highest number of 
           complaints from consumers.")
      )
    ),
    #a leaflet map showing the distribution of complaints in the various states of america can be seen
      leafletOutput("states"), 
    HTML(
      paste(
        h3("Established and prosperous localities tend to register a higher number of complaints
            i.e. localities with people having a higher wage tend to buy and invest more in financial products. 
            The total wage of that particular area can be seen by the size of the circle.
           The larger the circle, the larger the wage for that area. ")
        )
        )),
  mainPanel(
    plotOutput("zip_relation")
    ),
  sidebarPanel(
    fluidRow(
      column(width = 12, offset = 0,
             #Take the state as input from a drop down menu 
             selectInput("State", "Please Select state",
                         choices=c("NY","TX","GA","FL","IL","PA","CA","NC","VA",
                                   "AZ","MD","NJ","NV"), selected = "TX")))
  )
    
)



# server script
server<-function(input, output){
  options(scipen = 999)
  #read the data
  
  #As the complaints data is too large, complaints for 2015, 2016, 2017, 2018 and 2019 have been filtered and the the columns not needed for the analysis have been removed
  new_complaints <- read.csv("new_complaints.csv",stringsAsFactors=FALSE)
  #The columns not useful for our analysis have been removed from the data
  zipcode_data <- read.csv("zipcode_data.csv",stringsAsFactors = FALSE)
 
  #extract years from the date
  years <- format(as.Date(new_complaints$Date.received, format="%d/%m/%Y"),"%Y")
  new_complaints['Year'] <- years
  
  #extract months from the date
  months <- format(as.Date(new_complaints$Date.received, format="%d/%m/%Y"),"%m")
  #making a column for years
  new_complaints['Months'] <- months
  
  #complaints for each year each month
  month_year <- data.frame(with(new_complaints,table(Months,Year)))
  
  in_favour_names = c("Closed with explanation","Closed with monetary relief","Closed with relief")
  
  #the data for the input company is filtered
  filtered_data_company <- reactive({
    xyz <- new_complaints[new_complaints$Company == input$Company,]
    xyz$check <- str_detect(xyz$Company.response.to.consumer, in_favour_names)
    xyz$Favour <- ifelse(xyz$check == TRUE,"in favour","not in favour")
    return(xyz)
  })
  
  mycols<-c("blue","coral4","mediumpurple","yellow","cyan","pink")
  
  #the data for the input company's mode of submission is filteerd out
  filtered_data_mode <- reactive({
    xyz <- new_complaints[new_complaints$Company == input$Company,] 
    xyz <- xyz[xyz$Year == input$Year,]
    
    
    #abc <- new_complaints[new_complaints$Company == input$Company,]%>%
      abc <- xyz %>% group_by(Submitted.via) %>% 
      summarise(count=n()) %>% 
      mutate(perc=count/sum(count)) %>% arrange((perc)) %>% mutate(ypos=cumsum(perc)-0.5*perc)
    abc <- as.data.frame(abc)
    abc$perc<-format(abc$perc*100,digits=0)
    abc$ypos<-format(abc$ypos*100,digits=1)
    
    abc$perc<-as.numeric(abc$perc)
    abc$ypos<-as.numeric(abc$ypos)
    
    return(abc)
    
  })
  
 
  #Filter the data just to show the ouput for the requested year
  filtered_data <- reactive({
    dplyr::filter(month_year, Year == input$Year)
  })
  
  
  #Filter the data just to show the products for the requested company
  filtered_data_product <- reactive({
    xyz <- new_complaints[new_complaints$Company == input$Company,] 
    xyz <- xyz[xyz$Year == input$Year,]
    company_product <- xyz
      #new_complaints[new_complaints$Company == input$Company,]
    prod <- company_product$Product
    prod <- gsub(","," ",prod)
    writeLines(prod,"products.txt")
    return ("products.txt")
    
  })
  
  #Filter the data just to show the performance of the companies' for the input year and chose the top 20 out of them
  filtered_data_yr_company <- reactive({
    yr_company <- new_complaints %>% filter(Year==input$Year) %>% group_by(Company) %>%
      summarise(No_of_complaints=n_distinct(Complaint.ID)) %>% arrange(desc(No_of_complaints))
    return (yr_company[1:20,])
  })
  
  #Filter the data just to show the input company's respnse to the customers
  filtered_data_response <- reactive({
    
    xyz <- new_complaints[new_complaints$Company == input$Company,] 
    xyz <- xyz[xyz$Year == input$Year,]
    c_r <- xyz%>%
      group_by(Company.response.to.consumer) %>% 
      summarise(count=n()) %>% 
      mutate(perc=count/sum(count)) %>% arrange((perc)) %>% mutate(ypos=cumsum(perc)-0.5*perc)
    c_r<-data.frame(c_r)
    
    c_r$perc<-format(c_r$perc*100,digits=0)
    c_r$ypos<-format(c_r$ypos*100,digits=1)
    
    c_r$perc<-as.numeric(c_r$perc)
    c_r$ypos<-as.numeric(c_r$ypos)
    
    return(c_r)
    
  })
  
  
  #shows the line chart for the monthly distribution of complaints
  output$month <- renderPlot({
    ggplot(filtered_data(), aes(x=Months,y=Freq,group=input$Year,color=Months))+ ggtitle("Monthly distribution of complaints")+
      facet_wrap(.~input$Year)+geom_point()+geom_line() +theme(legend.position = "none")
    
  },height = 400, width = 600 )
  
 # bar chart showing the performance of the top 20 companies with maximum number of complaints for the chosen year
  output$Companies <- renderPlot({
    ggplot(filtered_data_yr_company(), aes(x = Company, y = No_of_complaints, fill=Company)) +geom_col() + 
      theme(axis.text.x = element_text(size=8,angle=90)) + theme(legend.position = "none") + facet_wrap(.~input$Year)+
      ggtitle("Top 20 companies with maximum complaints for the chosen year")
  } )
  
  
 #Mode of submission  used for lodging complaints against a particular company
  output$mode <- renderPlot({
    
    ggplot(data = filtered_data_mode(),aes(x="",y=perc,fill=Submitted.via))+
      geom_bar(width=1,stat="identity",color="white") + coord_polar("y",start = 0)+
      scale_fill_manual(values=mycols)+
      theme_void() + ggtitle("Mode of complaint submission") + facet_wrap(.~input$Company) 
    
  } )
  
  # Proportion of complaints resoved by a particular company
  output$resolved <- renderPlot({
  
    ggplot(na.omit(filtered_data_company()), aes(x = factor(Favour))) +  
      geom_bar(aes(y = (..count..)/sum(..count..)),width=0.4,fill="purple") +  
      scale_y_continuous(labels = scales::percent)+ xlab("Favour")+ylab("Percentage") + 
      ggtitle("Percentage of complaints in favour and not in favour of consumers")+ facet_wrap(.~input$Company)
    
    })
  
  #word cloud showing the products for which complaints are registered againt the chosen company
  output$products <- renderPlot({
    filePath <- filtered_data_product()
    text <- readLines(filePath) # Load the data as a corpus and save a copy as 'input'
    input <- Corpus(VectorSource(text))
    docs <- input
    
    dtm <- TermDocumentMatrix(docs)
    m <- as.matrix(dtm)
    v <- sort(
      rowSums(m),
      decreasing = TRUE
    )
    d <- data.frame(
      words = names(v),
      freq = v
    )
    wordcloud(
      words = d$words,
      freq = d$freq,
      min.freq = 1,
      max.words = 200,
      random.order = FALSE,
      rot.per = 0.35,
      colors = brewer.pal(8,"Dark2")
    )
  }, width = 600)
  
  
  

#plot the company's response to the consumers in the form of a bar chart  
  output$response <- renderPlot({
    
    ggplot(filtered_data_response(), aes(Company.response.to.consumer,perc, fill = Company.response.to.consumer)) + geom_col()+ 
      theme(axis.text.x = element_text(size=10))+xlab("Company's response")+ylab("Percentage")+ facet_wrap(.~input$Company) +theme(legend.position = "none")
    
    
  })
  
  #plot the output according to the choice of the useri.e. the feature the user wants to see
  output$features <- renderUI({
    if(input$Feature == "Mode of Submission")
      fluidPage(plotOutput("mode"))
    else if(input$Feature == "Resolved complaints")
      fluidPage(plotOutput("resolved"))
    else if(input$Feature == "Products")
      fluidPage(plotOutput("products"))
    else
      plotOutput("response")
  
})
  
  #plot the count of complaints state wise on a leaflet
  output$states <- renderLeaflet({
    #filter out the data and make the data ready for plotting on the map
    new_complaints$State[new_complaints$State==""]<-NA
    states <- na.omit(new_complaints$State)
    states_count <- as.data.frame(table(states))
    colnames(states_count) <- c("State","No_of_complaints")
    
    zip_data <- data.frame(zipcode_data %>% select(Lat, Long, State) %>% group_by(State) %>% 
                             summarise(lat=mean(Lat),long=mean(Long))) 
    
    x<-complete.cases(zip_data)
  
    final_zip_data <- zip_data[x,]
    
    final <- states_count %>% left_join(final_zip_data,key="State")
    y <- complete.cases(final)
    final <- final[y,]
    mybins <- c(0,1000,2000,5000,7000,10000,50000,70000,100000,Inf)
    mypalette <- colorBin( palette="YlOrBr", domain=final$No_of_complaints, na.color="transparent", bins=mybins)
    
    # the text as a tooltip
    mytext <- paste(
      "State: ", final$State,"<br/>", 
      "Count: ", final$No_of_complaints, 
      sep="") %>%
      lapply(htmltools::HTML)
    
    # Final Map
    x <- leaflet(final) %>% 
      addTiles()  %>% 
      setView( lat=80, lng=-160, zoom=1) %>%
      addCircleMarkers( 
        fillColor = ~mypalette(No_of_complaints), 
        stroke=TRUE, 
        fillOpacity = 0.9, 
        color="white", 
        weight=0.3,
        label = mytext,
        labelOptions = labelOptions( 
          style = list("font-weight" = "normal", padding = "3px 8px"), 
          textsize = "13px", 
          direction = "auto"
        )
      ) %>%
      addLegend(pal=mypalette, values=~No_of_complaints, opacity=0.9, title = "No. of Complaints", position = "bottomright" )
    
    #to set the view on the application page
    x %>%setMaxBounds(-130,50,-50,30)%>% fitBounds(-130,50,-50,30)
    
  })
  
  #chose the top 1000 complaints and join the two data sets in order to get some inferences on the effect of the neighbourhood on the complaints' volume.
  
 new_complaints_zip <- new_complaints %>% select(ZIP.code,Complaint.ID) 
  names(new_complaints_zip)[names(new_complaints_zip) == 'ZIP.code'] <- 'Zipcode'
  zipcode_consent<-(str_sub(new_complaints_zip$Zipcode,4,5)=="XX")
  new_complaints_zip<- new_complaints_zip[zipcode_consent==FALSE,]
  top_complaints_zipcode<-data.frame(new_complaints_zip %>% group_by(Zipcode) %>% 
                    summarise(Unique_Elements = n_distinct(Complaint.ID)) %>% arrange(desc(Unique_Elements)))
  
  #as data is very huge, top 1000 complaints are considered for further analysis
  top1000_complaints_zipcode<-head(top_complaints_zipcode,1000)
  
  zip_wages<-is.na(str_trim(zipcode_data$TotalWages))
  zipcode_data<- zipcode_data[zip_wages==FALSE,]
  
  #select the columns needed from the data frame
  zip_data_wage <- data.frame(zipcode_data %>% select(Zipcode, City, State, TotalWages))
  
  top1000_complaints_zipcode<-data.table(top1000_complaints_zipcode)
  top1000_complaints_zipcode$Zipcode<-as.integer(top1000_complaints_zipcode$Zipcode)
  zip_data_wage<-data.table(zip_data_wage)
  zip_data_wage$TotalWages<-as.character(zip_data_wage$TotalWages)
  zip_data_wage$Zipcode<-as.character(zip_data_wage$Zipcode)
  top1000_complaints_zipcode$Zipcode<-as.character(top1000_complaints_zipcode$Zipcode)
  setkey(top1000_complaints_zipcode,Zipcode)
  setkey(zip_data_wage,Zipcode)
   
  # the two data frames are then merged to form a result data frame
  Result_wage <- merge(top1000_complaints_zipcode,zip_data_wage, all.x =TRUE,by="Zipcode")
  
  #Filter the result data just to show the ouput for the requested state
  filtered_data_state <- reactive({
    result_state <- Result_wage[Result_wage$State==input$State]
    result_state <- result_state[order(-result_state$Unique_Elements),]
    result_state <- result_state[1:20,]
    return (result_state)
  })
  
  #shows a scatter plot showing the effect of the area on the number of complaints
  output$zip_relation <- renderPlot({
    size = c(1000,2000,3000,4000,5000,6000,7000,8000,9000,10000)
    ggplot(filtered_data_state(),aes(Zipcode,Unique_Elements,size=TotalWages,color=TotalWages)) + 
      geom_point() + ylab("Complaints filed")  + theme(legend.position="none") + 
      ggtitle("Relation of zipcode vs complaints filed with respect to the total wages")
      })
}


#to run the app
shinyApp(ui = ui, server = server)
