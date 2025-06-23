install.packages("shiny")
install.packages("shinydashboard")
install.packages("readxl")
install.packages("arules")
install.packages("dplyr")
install.packages("ggplot2")
install.packages("scales")

library(shiny)
library(shinydashboard)
library(readxl)
library(arules)
library(dplyr)
library(ggplot2)
library(scales)

#Define UI
ui <- dashboardPage(
  dashboardHeader(title = "options"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("View Data", tabName = "page0"),
      menuItem("K-means", tabName = "page1"),
      menuItem("Association Rules",tabName = "page2"),
      menuItem("Data visualization", tabName = "page3")
    )
  ),
  dashboardBody(
    tabItems(
      tabItem(tabName = "page0",h2("View Data"),
              fileInput("file_dataset","upload dataset csv file"),
              actionButton("button_dataset","Submit"),
              verbatimTextOutput("result_dataset")),
      
      tabItem(tabName = "page1",h2("K-means"),
              numericInput("num_clus","The number of culsters",value = 0,min = 2,max = 4),
              actionButton("button_kmeans","Submit"),
              verbatimTextOutput("result_kmeans")),
      
      tabItem(tabName = "page2",h2("Association Rules"),
              fileInput("file_arules","upload TXT file for association rules"),
              numericInput("num1", "Support:", value = 0, min = 0.001, max = 1),
              numericInput("num2", "Confidence:", value = 0, min = 0.001, max = 1),
              numericInput("num3", "Min Length:", value = 0),
              actionButton("button_arules", "submit"),
              verbatimTextOutput("result_arules")),
      
      tabItem(tabName = "page3",h2("Visualization"),
              #fileInput("file_visualization","upload csv file for visualazation"),
              #Sidebar with a slider input for number of bins 
              fluidRow(
                column(3, plotOutput("plot1")),  
                column(3, plotOutput("plot2")),  
                column(3, plotOutput("plot3")),
                column(3, plotOutput("plot4"))
              )
      )
    )
  )
)

#Define Server
server <- function(input, output ,session) {
  #store the dataset
  last_version <- reactiveVal()
  
  #Data cleaning
  observeEvent(input$button_dataset, {
    req(input$file_dataset)
    dataset <- read.csv(input$file_dataset$datapath, sep = ",")
    
    dataset <- as.data.frame(dataset)
    dataset <- dataset %>% arrange(desc(total)) # arrange total
    
    # delete duplicated and repeated
    dataset <- distinct(dataset)
    dataset <- na.omit(dataset)
    
    # convert data types if necessary
    dataset$count <- as.numeric(dataset$count)
    dataset$total <- as.numeric(dataset$total)
    dataset$rnd <- as.numeric(dataset$rnd)
    dataset$age <- as.numeric(dataset$age)
    dataset$items <- as.character(dataset$items)
    dataset$customer <- as.character(dataset$customer)
    dataset$city <- as.character(dataset$city)
    dataset$paymentType <- as.character(dataset$paymentType)
    
    # delete outliers
    outliers <- boxplot(dataset$count, plot = FALSE)$out
    dataset <- dataset[!(dataset$count %in% outliers), ]
    
    last_version(dataset)
    # render the cleaned dataset
    output$result_dataset <- renderPrint({
      print(dataset)
    })
  })
  
  #K-means
  observeEvent(input$button_kmeans, {
    data <- last_version()
    req(data)
    
    aggregated_data <- data %>%
      group_by(rnd, age, customer) %>%
      summarise(total_spent = sum(total), .groups = "drop")
    aggregated_data_df <- as.data.frame(aggregated_data)
    data_km <- aggregated_data_df[, c("age", "total_spent")]
    num_cluster<-input$num_clus
    
    if (num_cluster >= 2 && num_cluster <= 4) {
      kmeans_clustering <- kmeans(data_km, centers = num_cluster)
      aggregated_data_df$cluster <- kmeans_clustering$cluster
      output$result_kmeans<-renderPrint(aggregated_data_df)
    } else {
      output$result_kmeans<-renderText("Invalid input. Please enter a number (between 2 & 4):")
    }
  })
  
  #association rules
  observeEvent(input$button_arules, {
    req(input$file_arules)
    my <-read.transactions(input$file_arules$datapath, format = "basket", sep = ",")
    
    s <- input$num1
    c <- input$num2
    m <- input$num3
    
    if (is.na(s) || is.na(c) || is.na(m) || s > 1 || s < 0.001 || c > 1 || c < 0.001) {
      output$result_arules <- renderText("Please enter valid values.")
    } else {
      rules <- apriori(my, parameter = list(supp = s, conf = c, minlen = m))
      output$result_arules <- renderPrint(inspect(rules))
    }
  })
  #Data visualization
  observe({
    sorted_Dataset_in_ <- last_version()
    req(sorted_Dataset_in_)
    #sorted_Dataset_in_<-read.csv(input$file_visualization$datapath, sep = ",")
    
    grrc<-sorted_Dataset_in_%>%
      group_by(paymentType)%>%
      summarize(total_spending=sum(total))  
    agee<-sorted_Dataset_in_%>%
      group_by(age)%>%  
      summarize(totall=sum(total))
    cityy<-sorted_Dataset_in_%>%
      group_by(city)%>%
      summarize(spending=sum(total))
    Data<-data.frame(cityy)
    
    # first plot
    output$plot1 <- renderPlot({
      ggplot(agee,aes(x=age,y=totall))+
        geom_point(color="pink",size=3)+
        geom_line(linetype="solid",color="purple",size=1)+
        scale_x_continuous(limits = c(20, 65), breaks = seq(20, 65, by = 2)) +
        ggtitle("Total spending per Age")+
        theme_grey()
      
    })
    #second plot
    output$plot2<-renderPlot({ 
      Data <- Data[order(Data$spending, decreasing = FALSE), ]
      Data$city<-factor(Data$city,levels = rev(Data$city)) ##علشان ارتب المدن تنازلي
      ggplot(Data,aes(x=city,y=spending,fill = city)) +#fill دي انا كده حددت هو هيلون ايه 
        geom_bar(stat = "identity")+
        theme_gray()+
        labs(title = "comparison between cities and total spending",
             x="city",
             y="total spending ")+
        scale_color_brewer()
    })
    #third plot
    output$plot3<-renderPlot({
      grrc$fraction = grrc$total_spending / sum(grrc$total_spending)
      grrc$percentage = round(100 * grrc$fraction, 1)
      ggplot(grrc,aes(x = "", y =fraction, fill = paymentType)) + 
        geom_bar(stat = "identity", width = 1) +  # يخلي العمود عريض كفايه انه يعمل دايره
        coord_polar("y") +  #علشان يحول الرسم لدايره
        geom_text(aes(label = paste(percentage, "%")), #النسب
                  position = position_stack(vjust = 0.5), hjust = -0.1) +
        ggtitle("Cash vs Credit") +
        theme_void() 
      
    })
    #fourth plot
    output$plot4<-renderPlot({
      ggplot(sorted_Dataset_in_, aes(y =total)) +
        geom_boxplot()+
        ggtitle("The distribution of total spending")+
        theme_grey()
    })
  })
  
  
}

#run application 
shinyApp(ui = ui, server = server)

