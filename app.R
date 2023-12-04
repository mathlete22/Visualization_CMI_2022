library(shiny)
library(shinydashboard)
library(tidyverse)
library(dplyr)
library(knitr)
ui=dashboardPage(skin="yellow",
  dashboardHeader(title="Numero Uno!"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Introduction",tabName="Introduction"),
      menuItem("Data Sets",tabName="Data"),
      menuItem("Basic Analysis",tabName="Ana"),
      menuItem("Top Players",tabName="Pla"),
      menuItem("Deeper Analysis",tabName="DA"),
      menuItem("Conclusion",tabName="Conc")
    )
  ),
  dashboardBody(
    tabItems(
      tabItem(
        tabName="Introduction",
        h1("About the Project",align='centre'),br(),p("Hi, I am Aniket Saha, student of Chennai Mathematical Institute. I have created this project on the topic 'FIFA World Cup 2022' using RStudio with the help of R programming language in November-December 2022."),
        br(),
        p("Football is the most popular sport in the world. FIFA World Cup is the biggest sporting event in the world in terms of watching and people involved. According to FIFA, around 3.75 billion people around the world witnessed the previous edition in 2018, held in Russia. Now, that is almost half the planet.
This year 2022, 22nd edition of this famed competition will be held in Qatar. We are doing this project to get some insights about the teams participating. Moreover, we'll try to get some idea about which players or teams will perform well in this upcoming  mega event."),
        h3("1. FIFA World Cup"),
        br(),
        p("FIFA World cup started in 1930 in Uruguay. Uruguay won the inaugural event in their country. After that we have witnessed 20 more editions of this huge sporting event. This year it is the 22nd World Cup which is going on in Qatar. We have had several teams lifting the world cup over the years with Brazil winning it the most, 5 times. In this year's world cup, 32 teams are participating. We will know a bit more about these teams soon.")
      ,
      h3("2. Reason for Naming the Project 'Numero Uno'"),br(),
      p("'Numero Uno' means 'Number One' in Spanish. Since football is the biggest or so to say number 1 sport in the world, and this project is about that sport and specifically the biggest sporting event in the world in terms of spectators, we have decided to name the project such. Also the choice of the language Spanish was due to the fact that the official language of many countries in this year's competition is Spanish (Spain, Mexico, Argentina etc)."),br(),
      p("Now we will move forward to the details of the data sets used for this project.")
      ),
    tabItem(
      tabName="Conc",strong("In The End..."),br(),"We have done some basic data analysis on the history of FIFA World Cup and tried to predict the winner of this year's world cup. Also we predict that the average goal per game in this year's world cup will be 2.75, which is lower than the previous two world cups. At the end of our analysis, we have found that Brazil is most likely to win the world cup. However, France, Argentina and Spain would be tough competitors for the trophy.",
      tabBox(id="Conc1",width=15,
tabPanel(strong("References"),"We have used the following references to do this project.",br(),
"1. ",a("Google",href="https://www.google.co.in"),br(),
"2. ",a("Stack Overflow",href="https://stackoverflow.com"),br(),
"3. ",a("FIFA official website",href="https://www.fifa.com"),br(),
"4. ",a("Kaggle",href="https://www.kaggle.com"),br(),
"5. ",a("Wikipedia",href="https://www.wikipedia.org")),
      tabPanel(strong("Acknowledgement"),"I am thankful to Mr. Sourish Das, professor at Chennai Mathematical Institute for guiding me through this project and helping me whenever needed.")
      )
  ),
  tabItem(tabName="Data",
  h3("Data Sets Used in This Project"),
  p("Now we will give some description about the data sets used and some of the important variables. "),
  p(span("Source of Data: ",style="color:blue"),"We have collected some data from official FIFA website and some other secondary data from Kaggle. We will perform our analysis based on these data sets."),
  fluidRow(
    tabBox(
      id = "DS", height = "200px",width=8,
      tabPanel(strong("Teams2022"), "In this data set, we have: teams(country names), latest FIFA points of each team, ranks of each team based on points, region(continents).",br(),strong("Please Note: "),"Australia participates through Asian qualification bracket, hence Australia is in group Asia.",br(),strong("Source of Data: "),"Self collected from FIFA website. ", a("Link.",href="https://www.fifa.com/fifa-world-ranking/men"),br(),"A glimpse of data is given below:",tableOutput("A")),
      tabPanel(strong("WorldCup_All"), "In this data set, we have the data of all the world cup matches since 1930. The variables are: year, host country, home team, away team, goals scored by home team, goals scored by away team, whether the game was played in a neutral venue and whether the home team won, drew or lost.",br(),strong("Source of Data: "),"Kaggle. ",a("Link.",href="https://www.kaggle.com/datasets/ccsitai/worldcupdataset"),br(),"A glimpse of data is given below:",tableOutput("B")),
      tabPanel(strong("WorldCup_History"),"In this data set, we have the years in which world cup were held over the years, number of teams, winner, runner up, whether host country reached the final or the semi finals, total number of matches, total number of goals scored, average goals per game.",br(),strong("Source of Data: "),"Self collected from Wikipedia. ",a("Link.",href="https://en.wikipedia.org/wiki/FIFA_World_Cup"),br(),"A glimpse of data is given below:",tableOutput("C")),
      tabPanel(strong("Players_FIFA23"),"This is a subset of a very large data set containing all the details of all the football players around the world based on the EA Sports' game FIFA23 ratings which has very realistic data about the players.",br(),"In this data set, we have the top 120 players in the game. The variables are: player name, country that they play for, overall rating, their position of playing (forward, midfielder, defender, goalkeeper).",br(),strong("Source of Data: "),"Kaggle. ",a("Link.",href="https://www.kaggle.com/datasets/bryanb/fifa-player-stats-database?select=FIFA23_official_data.csv"),br(),"A glimpse of data is given below:",tableOutput("D"))
    )
  )),
  tabItem(tabName="Ana",h3("First Steps..."),
  p("Now we will try to gather some basic ideas and information from the data sets."),
  fluidRow(
    tabBox(
      id="Ana",height="400px",width=12,
     tabPanel(strong("Origin of Teams"),"We will see where the participant teams are from.",plotOutput("a"),"We can see that Europe has the most representative countries, 13, followed by Asia with 6, Africa and South America with 5 and then North America with 4."),
     tabPanel(strong("Champions"),"Now we will see the teams who have won the world cup before, and how many times they have won it.",plotOutput("b"),"We can see that Brazil won the world cup 5 times, the most any country has achieved. Moreover, all the teams who have won the world cup are either from Europe or South America. Also European teams have won the tournament 12 times and South American nations have won it 9 times."),
     tabPanel(strong("World Cup Winners"),"We are showing a simple plot which will tell us the number of times a team has won the world cup.",br(),"Note that only the countries who have won the trophy at least once are here.",fluidRow(
       box(
         radioButtons("wins","Number of Titles",choices = c("All","5 World Cups","4 World Cups","3 World Cups","2 World Cups","1 World Cup")),width=3,background = "aqua"
       ),
       box(
         plotOutput("Plot",height=400),width=8
       )),"Here an error message would mean that no team has won that many world cups."),
     tabPanel(strong("Win Based on Regions"),"We will see whether playing in a particular region helps certain countries.",plotOutput("c"),"Here we can see that when the world cup is hosted in Europe, European countries dominate over others. Whereas while the world cup is hosted in American continents, South American teams dominate over others. In neutral countries, we get that the contest is even."),
     tabPanel(strong("Team Rankings"),"We will see the top 10 teams in the world according to latest FIFA rankings which were released before the world cup began.",tableOutput("d")),
     tabPanel(strong("Host Performance"),"We will see whether playing in your own country helps the teams to perform better. We will see whether a host team have reached the final or the semi finals in that particular world cup.",plotOutput("e"),"So we observe that the host teams have finished in top 2 in more than 35% times of all world cups and they have finished in top 4 more than 60% of times. This can be regarded as a valid evidence that the host countries perform quite well in the world cups.")
    )
  )
  ),
  tabItem(tabName="Pla",h3("About the Warriors"),
          p("We will now try to gather some knowledge about the top players participating in this year's world cup. The data about the players have been collected from the Players_FIFA23 data set."),
          fluidRow(
            tabBox(
              id="Pla",height="400px",width=12,
              tabPanel(strong("Best Players"),"Firstly, we will see who the top 10 footballers in the world are.",tableOutput("AA"),"Note that only Mohamed Salah (from this list) will not be playing in this year's world cup."),
              tabPanel(strong("Country"),"Now we will look at the countries from which these players hail from. We will only show top 10 countries with respect to number of elite players they have. Also from now we will only focus on the teams which are playing in the world cup this year.",plotOutput("AC"),"We can see that Spain has the most talented players in this year's world cup. However, France, Germany, England and Brazil also have some great footballers in their team."),
              tabPanel(strong("Age"), "We now try to understand the age-distribution of the top players in the world",plotOutput("AB"),"We can see that more than half of the top players are above 28 years old."),
              tabPanel(strong("Position"),"Now we are interested in finding out the position which has the most talented players playing in it.",plotOutput("AD"),"We can see that most of the top players in the world are midfilders, closely followed by forwards."),
              tabPanel(strong("Preferred Foot"),"Now we are interested in the preferred foot for playing of the top players",plotOutput("AE"),"We observe that there are significantly more right footed players than the left footed players among the top 120 players in the world.")
            )
          )
          ),
  tabItem(tabName="DA",h3("Predictions About the World Cup"),
          p("We will now try to predict some information about this year's world cup. Mainly we are interested in predicting which team is going to win the world cup. Moreover, we will try to predict the average goal per game amount for this year's world cup."),
          tabBox(
            id="DA",height="550px",width=12,
            tabPanel(strong("Historical Data"),"We will see the hosts, winners, runners up, average goal per game in all the previous editions of the world cup.",
                     tabBox(
                       id="DA1",width=10,
                       tabPanel(strong("1930-1962"),"Here we see the details of the first 7 editions of the world cup.",tableOutput("BA")),
                       tabPanel(strong("1966-1990"),"Here we see the details of the next 7 editions of the world cup.",tableOutput("BB")),
                       tabPanel(strong("1994-2018"),"Here we see the details of the last 7 editions of the world cup.",tableOutput("BC"))
                     )),
            tabPanel(strong("Goal per Game (gpg)"),"Now we will try to predict the average goal per game amount of 2022 world cup. We will model the time series data in three ways- linear, quadratic, cubic.",
                     tabBox(
                       id="DA2",width=10,
                       tabPanel(strong("GPG Over The Years"),"We will see how were the average goals per game values in the previous world cups.",plotOutput("AAA"),"We can see the GPG has decreased over the years but is somewhat on the rise since 2010 world cup."),
                       tabPanel(strong("Linear Model"),"Here we have used a linear model.",plotOutput("Ca"),"We observe that the predicted average goal per game of this year's world cup from the linear model is negative and hence not acceptable."),
                       tabPanel(strong("Quadratic Model"),"Here we have used a quadratic model.",plotOutput("Cb"),"We observe that the predicted average goal per game of this year's world cup from the quadratic model is approximately 2.5. This is less than the last two world cups, but more than the 2006, 2010 world cups."),
                       tabPanel(strong("Cubic Model"),"Here we have used a cubic model.",plotOutput("Cc"),"We observe that the predicted average goal per game of this year's world cup from the cubic model is approximately 3.0, which is higher than last 12 world cups."),
                       tabPanel(strong("Conclusion"),"Based on the plots, we can say that the quadratic model and the cubic model fit the data better than the linear model. And hence we take the mean of these two estimates as an estimate of average GPG of 2022 world cup. Therefore the average GPG of this year's world cup will approximately be (2.5+3)/2=2.75")
                     )),
            tabPanel(strong("Predicting The Winner"),"Now we are going to predict which team is most likely to win the world cup.",
                     tabBox(
                       id="DA3",width=10,
                       tabPanel(strong("A Measure"),"We are proposing a measure 'Team_Index' which will be used to predict the winner.",br(),"Now, Team_Index will use the fact that a team won the world cup previously or not. This is useful because experience in the highest level helps a team in the latter stages of the tournament.",br(),"The measure will also take into account the number of elite players a team has. This is a hint towards how strong the team is.",br(),"Lastly, we will include standardized FIFA points in the measure. This will indicate the current form of the team and hence is very useful. We have used standardization because the variation in the FIFA points among the teams is quite high.",br(),"We will use a simple linear model. We define Team_Index as follows:",br(),"Team_Index = (wonp) + (# elite players)/10 + ((FIFA Points)-mean(FIFA Points))/sd(FIFA Points)",br(),"Here, (wonp) is a variable, which takes value 1 if the team has previously won the world cup, 0 otherwise. (# elite players) is the number of elite players the team has out of top 120 players according to the FIFA23 data set."),
                       tabPanel(strong("Calculations"),"Now we will calculate the Team_Index value for each team.",br(),"Here we will show the Team_index value for the nine strongest teams.",tableOutput("EA")),
                       tabPanel(strong("Visualizing"),"We present the Team_Index of the strongest 9 teams using the following plot.",plotOutput("EB")),
                       tabPanel(strong("Conclusion"),"So using our measure we have reached to the conclusion that Brazil are most likely to win the world cup this year. They have already won it a record 5 times and they are the number 1 ranked team in the world.",br(),"Just behind Brazil, our measure tells us that defending champion France are the second most likely team to win the trophy.",br(),"Two-time champions Argentina and 2010 champions Spain are respectively third and fourth most likely teams to win the world cup.")
                     )
                     )
          )
          )
  )))





server=function(input,output){
  wch=read.csv("WorldCup_History.csv")
  data=read.csv("Teams2022.csv")
  al=read.csv("WorldCup_All.csv")
  fifa=read.csv("Players_FIFA23.csv")
  tab=table(wch$Winner)
  x=reactive({
    if(input$wins=="All"){
      tab
    }
    else if(input$wins=="5 World Cups"){
      tab[tab>4]
    }
    else if(input$wins=="4 World Cups"){
      tab[tab==4]
    }
    else if(input$wins=="3 World Cups"){
      table()
    }
    else if(input$wins=="2 World Cups"){
      tab[tab==2]
    }
    else if(input$wins=="1 World Cup"){
      tab[tab==1]
    }
  })
  output$Plot <- renderPlot({
    barplot(x(), cex.names=0.7,col="brown3",border="black",main="Barplot Showing World Cup Winners")
  })
  output$Plot2 <- renderPlot({
    hist(rnorm(1000),col="steelblue")
  })
  output$A=renderTable({
    head(data,4)
  })
  output$B=renderTable({
    head(al,4)
  })
  output$C=renderTable({
    head(wch%>%select(1:6),4)
  })
  output$D=renderTable({
    head(fifa,7)
  })
  output$a=renderPlot({
    t=table(data$Region)
    lab=c("Africa","Asia","Europe","North America","South America")
    lab=paste(lab,"(",t,sep="")
    lab=paste(lab," teams)",sep="")
    par(mfrow=c(1,2))
    barplot(t,col="steelblue4",border="black",cex.names = 0.7,ylim=c(0,15))
    pie(t,col=gray(seq(0.2,1,length=5)),border="black",labels=lab)
  })
  output$b=renderPlot({
    par(mfrow=c(1,2))
    barplot(table(wch$Winner),cex.names=0.7,col="yellow2",main="Barplot Showing World Cup Winners")
    lab2=c("Argentina(2 times)","Brazil(5 times)","England(1 time)","France(2 times)","Germany(4 times)","Italy(4 times)","Spain(1 time)","Uruguay(2 times)")
    pie(table(wch$Winner),col=gray(seq(0.1,1,length=8)),border="black",labels=lab2)
  })
  output$c=renderPlot({
    par(mfrow=c(1,2))
    win=matrix(c(10,1,1,7,1,1),ncol=3)
    barplot(win,width=c(50,50,50),main="World Cup Wins Based on Regions",ylab="Number of Wins",col=c("steelblue1","steelblue4"),ylim=c(0,12),names.arg=c("Europe","Americas","Neutral"),xlab="World Cups Hosted In",legend.text=c("Europe","South America"),args.legend = list("topright"))
  })
  output$d=renderTable({
    head(data%>%select(1:3),10)
  })
  output$e=renderPlot({
    host=matrix(c(8,13),ncol=2)
    host1=matrix(c(13,8),ncol=2)
    par(mfrow=c(1,2),cex=0.9)
    pie(host,main="Top 2 Finish",col=c("lightblue","grey"),labels=c("Successful(38%)","Unsuccessful"))
    pie(host1,main="Top 4 Finish",col=c("steelblue","grey"),labels=c("Successful(62%)","Unsuccessful"))
  })
  output$AA=renderTable({
    head(fifa%>%select(1,3),10)
  })
  output$AB=renderPlot({
    par(mfrow=c(1,2))
    hist(fifa$Age,xlab="Age",ylab="Number of players",main="Age-distribution of Top Players",col="lightpink")
  })
  output$AC=renderPlot({
    fi_fil=filter(fifa,fifa$Country %in% data$Country)
    fi=table(fi_fil$Country)
    s=sort(fi,decreasing=T)
    barplot(s[1:10],cex.names=1.2,ylim=c(0,20),col="red4",main="Barplot Showing Distribution of Top Players")
  })
  output$AD=renderPlot({
    fi_fil=filter(fifa,fifa$Country %in% data$Country)
    pos_tab=table(fi_fil$Position)
    par(mfrow=c(1,2))
    barplot(pos_tab,cex.names=0.7,col="lightyellow",main="Barplot Showing Distribution of Players",names.arg=c("Defender","Forward","Goalkeeper","Midfielder"))
  })
  output$AE=renderPlot({
    pf_tab=table(fifa$Preferred.Foot)
    par(mfrow=c(1,2))
    barplot(pf_tab,cex.names=1,col="lightgreen",main="Barplot Showing Distribution of Players",names.arg=c("Left Footed","Right Footed"))
  })
  output$BA=renderTable({
    head(wch%>%select(1,2,3,4,12),7)
  })
  output$BB=renderTable({
    (wch%>%select(1,2,3,4,12))%>%slice(8:14)
  })
  output$BC=renderTable({
    tail(wch%>%select(1,2,3,4,12),7)
  })
  output$AAA=renderPlot({
    plot(wch$Year,wch$Average.GPG,type='b',lty=3,xlab="Year",ylab="Average Goals per Game",col="black",lwd=3.5,main="Average GPG Over The Years",xlim=c(1928,2022))
  })
  output$Ca=renderPlot({
    plot(wch$Year,wch$Average.GPG,type='b',lty=3,xlab="Year",ylab="Average Goals per Game",col="black",lwd=3.5,main="Linear Model",xlim=c(1928,2022))
    par(new=T)
    lines(wch$Year, lwd=3.5,fitted(lm(wch$Average.GPG~wch$Year), type="l",xlab='',ylab=''),col="blue")
  })
  output$Cb=renderPlot({
    plot(wch$Year,wch$Average.GPG,type='b',lty=3,xlab="Year",ylab="Average Goals per Game",col="black",lwd=3.5,main="Quadratic Model",xlim=c(1928,2022))
    par(new=T)
    lines(wch$Year,lwd=3.5, fitted(lm(wch$Average.GPG~wch$Year+I(wch$Year^2)), type="b",xlab='',ylab=''),col="red")
  })
  output$Cc=renderPlot({
    plot(wch$Year,wch$Average.GPG,type='b',lty=3,xlab="Year",ylab="Average Goals per Game",col="black",lwd=3.5,main="Cubic Model",xlim=c(1928,2022))
    par(new=T)
    lines(wch$Year, lwd=3.5,fitted(lm(wch$Average.GPG~wch$Year+I(wch$Year^2)+I(wch$Year^3))), type="l",xlab='',ylab='',col="darkgreen")
  })
  output$EA=renderTable({
    fi_fil=filter(fifa,fifa$Country %in% data$Country)
    fi=table(fi_fil$Country)
    s=sort(fi,decreasing=T)
    t=data.frame(s)
    x=t$Var1[1:10]
    y=data$Country[1:10]
    z=intersect(x,y)
    df2=data.frame(table(wch$Winner))
    z2=intersect(z,df2$Var1)
    f1=filter(data, data$Country %in% z)
    f0=filter(t,t$Var1 %in% z)
    df00=data.frame(Teams=f0,FiP=c(1715.22,1759.78,1650.21,1728.47,1841.3,1676.56,1773.88,1816.71,1694.51))
    FIFA_P2=(df00$FiP - mean(df00$FiP))/sd(df00$FiP)
    Elite_Players2=f0$Freq/10
    wonp=c(1,1,1,1,1,0,1,0,0)
    xyz=(FIFA_P2)+(Elite_Players2)+wonp
    df0=data.frame(Teams=z, FIFA_Points=df00$FiP, Elite_Players=f0$Freq, wonp,Team_Index=xyz)
  })
  output$EB=renderPlot({
    fi_fil=filter(fifa,fifa$Country %in% data$Country)
    fi=table(fi_fil$Country)
    s=sort(fi,decreasing=T)
    t=data.frame(s)
    x=t$Var1[1:10]
    y=data$Country[1:10]
    z=intersect(x,y)
    df2=data.frame(table(wch$Winner))
    z2=intersect(z,df2$Var1)
    f1=filter(data, data$Country %in% z)
    f0=filter(t,t$Var1 %in% z)
    df00=data.frame(Teams=f0,FiP=c(1715.22,1759.78,1650.21,1728.47,1841.3,1676.56,1773.88,1816.71,1694.51))
    FIFA_P2=(df00$FiP - mean(df00$FiP))/sd(df00$FiP)
    Elite_Players2=f0$Freq/10
    wonp=c(1,1,1,1,1,0,1,0,0)
    xyz=(FIFA_P2)+(Elite_Players2)+wonp
    barplot(xyz,names.arg=z,col="lightyellow",cex.names=0.8,main="Barplot Showing Team_Index",ylim=c(-0.5,4))
  })
}
shinyApp(ui,server)