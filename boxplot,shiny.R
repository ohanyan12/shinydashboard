install.packages("ggplot2")
library("ggplot2")
library("shiny")
install.packages("viridis")
library("viridis")

#Homework_2, Q1
#Tutorial for THE BOXPLOT
#A boxplot gives a nice summary of one or more numeric variables. 
#A boxplot is composed of several elements.

#Let`s take an example and visulaize it by BOXPLOT

data_box<- read.csv("Fortune 500 Companies US.csv",stringsAsFactors = FALSE)
colnames(data_box)
data_box[!is.na(data_box$Revenue.Per.Share)&
           !data_box$Sector %in% c("Media","Food & Staples Retailing",
                                   "Technology Hardware & EquipmenT",
                                   "Food $ Staples Retailing",
                                   "Retailing","Transportation"),]
data_box<- data_box[data_box$Revenue.Per.Share<250,]
ggplot(data_box,aes(x=Sector, y= Revenue.Per.Share))+geom_boxplot()

#The line that divides the box into 2 parts represents the median of the data.
#The difference between Quartiles 1 and 3 is called the interquartile rangeand 
#As we see, we need to use  "theme" function to make axis x more readable 

data_box<- data_box[data_box$Revenue.Per.Share<250,]
ggplot(data_box,aes(x=Sector, y= Revenue.Per.Share))+geom_boxplot()+
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

#we can change the color, shape and size of outlier to make them more visible

data_box<- data_box[data_box$Revenue.Per.Share<250,]
ggplot(data_box,aes(x=Sector, y= Revenue.Per.Share))+geom_boxplot(outlier.colour="orange", outlier.shape=10,
                                                                  outlier.size=4)+
  theme(axis.text.x = element_text(angle = 90, hjust = 1))




#The function stat_summary() can be used to add mean points to a box plot :
#So we can conclude the difference of mean and median

data_box<- data_box[data_box$Revenue.Per.Share<250,]
ggplot(data_box,aes(x=Sector, y= Revenue.Per.Share,color="red"))+geom_boxplot(outlier.colour="orange", outlier.shape=10,
                                                                  outlier.size=4)+
  theme(axis.text.x = element_text(angle = 90, hjust = 1))+
  stat_summary(fun.y=mean, geom="point", shape=8, size=3, col="red")



#making notch+jitter
#if the 'notch' parameter is 'TRUE', a notch is drawn in each side of the boxes. 
#If the notches of two plots do not overlap this is 'strong evidence' that the two medians differ.
data_box<- data_box[data_box$Revenue.Per.Share<250,]
ggplot(data_box,aes(x=Sector, y= Revenue.Per.Share,color="red"))+geom_boxplot(outlier.colour="orange", outlier.shape=10,
                                                                              outlier.size=4,notch = TRUE)+
  theme(axis.text.x = element_text(angle = 90, hjust = 1))+
  stat_summary(fun.y=mean, geom="point", shape=8, size=3, col="red")+
  scale_fill_viridis(discrete = TRUE) +
  geom_jitter(color="blue", size=0.7, alpha=0.5) +
  ggtitle("Boxplot") +
  xlab("Sectors")
#please zoom before watching 

#Homework_2, Q2

#creating data
movies <- list("SO8 E01: Winterfell","SO8 E02:  A Knight of the Seven Kingdoms","SO8 E03: The Long Night",
               "SO8 E04: The Last of the Starks","SO8 E05: The Bells","SO8 E06: The Iron Throne")
release <- list("April 14, 2019", "April 21, 2019", "April 28, 2019",
                "May 5, 2019", "May 12, 2019", "May 19, 2019")


ui <- fluidPage(
  titlePanel(h1("Game of thrones season 8")),
  sidebarLayout(position = "right",
                sidebarPanel(
                  #widget 1
                  fluidRow(
                    column(6,
                           selectInput("release", h4("Choose the release date to see the episode name"), 
                                       choices =  release, selected = 1)
                    ),
                    #widget 2
                    column(6,
                           dateRangeInput("dates", label = h4("Date range"), start = "2019-05-01")
                    )
                  ),
                  #widget 3
                  fluidRow(
                    column(12,
                           checkboxGroupInput("cbox_movies", label = h4("Movie names"),
                                              choices = movies, selected = 1)
                    )
                  ),
                  #action button
                  fluidRow(
                    column(12,
                           actionButton("button", label = "orange button", 
                                        style = "color: white; background-color: orange")
                    )
                  )
                ),
                mainPanel(
                  br(),
                  br(),
                  h5(div(textOutput("difference_date"), style = "color: blue")),
                  br(),
                  br(),
                  h4(em(textOutput("selected_movies"))),
                  br(),
                  br(),
                  h3(strong(textOutput("release_date")))
                )
  )
)

# server starts here

server <- function(input,output,session){
  output$selected_movies <- renderText({
    episode <- paste(input$cbox_movies, collapse = ", ")
    paste("You have selected the following episode(s) of SW:", episode)})
  output$difference_date <- renderText({
    validate(need(input$dates[2] > input$dates[1], "end date is earlier than start date"))
    paste("The difference between selected dates is", difftime(input$dates[2], input$dates[1], units="days"),"days")
  })
  output$release_date <- renderText({
    choosed_date <- if(input$release == "April 14, 2019"){paste("SO8 E01: Winterfell")} else if(
      input$release == "April 21, 2019"){paste("SO8 E02:  A Knight of the Seven Kingdoms")} else if(
      input$release == "April 28, 2019"){paste("SO8 E03: The Long Night")} else if(
      input$release == "May 5, 2019"){paste("SO8 E04: The Last of the Starks")} else if(
      input$release == "May 12, 2019"){paste("SO8 E05: The Bells")} else if(
      input$release == "May 19, 2019"){paste("SO8 E06: The Iron Throne")} else {paste("")}
    paste("In this date the movie", choosed_date, "was released")
  })
}

shinyApp(ui=ui, server=server)



#Homework_2, Q3
df<- data.frame(var1 = rnorm(1000), var2=rnorm(1000))
df
plot1<- ggplot(data=df,mapping=aes(x=var1))+geom_histogram()
plot1

#transparent light blue bars (color given using RGB notation),
plot1<- ggplot(data=df,mapping=aes(x=var1))+geom_histogram(fill=seq(173,216,230),alpha=0.25)
plot1

#grey borders (color given using hexadecimal notation),
plot1<- ggplot(data=df,mapping=aes(x=var1))+geom_histogram(col="#545454",fill=seq(173,216,230),alpha=0.25)
plot1

#no grid on the plot background (by default there is a grid),
plot1<- ggplot(data=df,mapping=aes(x=var1))+geom_histogram(col="#545454",fill=I("blue"),alpha=0.25)+
        theme(panel.grid.major = element_blank(),panel.grid.minor = element_blank())
plot1

#customized x and y axis labels, title, caption and subtitle
plot1<- ggplot(data=df,mapping=aes(x=var1))+geom_histogram(col="#545454",fill=I("blue"),alpha=0.25)+
  theme(panel.grid.major = element_blank(),panel.grid.minor = element_blank())+
  labs(x="Random values",y="Quantity", title = "Graphic", caption = "Graphic by Ohanyan ",subtitle = "Random numbers")
plot1

#density plot plotted over the histogram with light red color
plot1<- ggplot(data=df,mapping=aes(x=var1))+geom_histogram(aes(y=..density..),col="#545454",fill=I("blue"),alpha=0.25)+
  theme(panel.grid.major = element_blank(),panel.grid.minor = element_blank())+
  labs(x="Random values",y="Quantity", title = "Graphic", caption = "Graphic by Ohanyan ",subtitle = "Random numbers")+
  geom_density(fill="#ffcccb")
plot1





