shinyServer(function(input,output){
  #----------------------------------------------------------------------------------------------------------------------------
  #Data Visualization
  #----------------------------------------------------------------------------------------------------------------------------
  
  #Data Visualization - Events
  #----------------------------------------------------------------------------------------------------------------------------
  library(dplyr)
  library(ggplot2)
  library(ggmap)
  library(ggthemes)
  library(leaflet)
  library(lubridate)
  library(googleVis)
  library(reshape)
  Fighters=read.csv('Fighters_Updated.csv')[,-1]
  Fights=read.csv('Fights_Updated.csv')[,-1]
  EventsInfo=read.csv('EventsInfo.csv')[,-1]
  Stadiums=read.csv('Stadiums.csv')[,-1]
  
  gvisOptions=list(width='100%',height='400px',backgroundColor='transparent',vAxis='{textStyle:{color:"white"}}',hAxis='{textStyle:{color:"white"}}',titleTextStyle='{color:"white"}',legend='{textStyle:{color:"white"}}')
  
  output$summary=renderText(paste('Till 02/10/2017, in the history of Ultimate Fighting Champion(UFC), there are ',length(unique(Fights$Event_id)),' events has been successfully held ( 2 canceled ), with ', nrow(Fights), ' matches.',sep=''))
  #Plot
  event_Data=Stadiums %>% group_by(lat,long) %>% summarise(Times=sum(freq)) 
  for (i in seq(1,nrow(event_Data))){
    event_Data$Address[i]=as.character(Stadiums$x[max(which(Stadiums$lat==event_Data$lat[i] & Stadiums$long==event_Data$long[i]))])
  }
  World=map_data('world')
  
  g_Events=NULL
  g_Events=leaflet(event_Data)%>%addTiles()%>% addCircleMarkers(radius = ~Times, color = 'purple', fill = FALSE, popup = ~paste(Address,'<br>','Events Held: ',Times),stroke = TRUE,fillOpacity = 0.5)
  output$Events=renderLeaflet(g_Events)
  
  event_Data$Country=gsub('.*, ','',event_Data$Address)
  event_Country=event_Data %>% group_by(Country) %>% summarise(Events=sum(Times))
  event_Country=event_Country[order(event_Country$Events,decreasing=TRUE),]
  g_event_country=gvisBarChart(event_Country,options=gvisOptions[-2])
  output$g_event_country=renderGvis(g_event_country)
  
  #Data Visualization - Fighters
  #------------------------------------------------------------------------------------------------------------------------------
  #Plot Country
  ########################################Shinny App to Chose Countries###########################################
  g_world=NULL
  World=map_data('world')
  
  #Matching Country Name
  C_D=Fighters %>% group_by(Country) %>% summarise(count=n())
  #C_D[!C_D[,1] %in% World$region,]
  C_D=C_D[C_D$Country!='',]
  
  #g_World=ggplot()+geom_map(data=World,map=World,aes(x=long,y=lat,map_id=region),fill='grey50',color='#ffffff',size=0.15)
  #g_World=g_World+geom_map(data=C_D,map=World,aes(fill=count,map_id=Country),color='#ffffff',size=0.15)
  #g_World=g_World+scale_fill_gradient(low='hotpink1',high='purple4',guide='colorbar',name='Total UFC fighters by Country')
  #g_World=g_World+labs(x=NULL,y=NULL)
  #g_World=g_World+theme(panel.border=element_blank())
  #g_World=g_World+theme(panel.background = element_blank())
  #g_World=g_World+theme(axis.ticks = element_blank())
  #g_World=g_World+theme(axis.text = element_blank())
  #g_World
  
  plot_World=function(included){
    g_World=gvisGeoChart(C_D[! C_D$Country %in% included,], locationvar="Country", 
                   colorvar="count",
                   options=gvisOptions[1:3])
    return(g_World)
  }
  output$g_World=renderGvis(plot_World(input$Excluded_Country))
  
  #Plot Classes
  Classes=Fighters %>% group_by(Class) %>% summarise(numbers=n())
  Classes$Class=ordered(Classes$Class,levels=c('Strawweight','Flyweight','Bantamweight','Featherweight','Lightweight','Welterweight','Middleweight','Light Heavyweight','Heavyweight','Super Heavyweight'))
  Classes=Classes[order(Classes$Class),]
  Classes=Classes[is.na(Classes$Class)==FALSE,]
  #g_Classes=ggplot(Classes)+geom_bar(aes(x=Class,y=numbers),stat='identity')
  g_Classes=gvisColumnChart(Classes,options=gvisOptions)
  output$g_Classes=renderGvis(g_Classes)
  #Plot ages
  
  Fights$Fighter1_id=gsub('.*-','',Fights$Fighter1_url)
  Fights$Fighter2_id=gsub('.*-','',Fights$Fighter2_url)
  Fights=merge(Fights,Fighters[,c('Birth_Date','Fighter_id')],by.x='Fighter1_id',by.y='Fighter_id')
  Fights=merge(Fights,Fighters[,c('Birth_Date','Fighter_id')],by.x='Fighter2_id',by.y='Fighter_id')
  Fights$Date=dmy(Fights$Date)
  Fights$Birth_Date.x=mdy(Fights$Birth_Date.x)
  Fights$Birth_Date.y=mdy(Fights$Birth_Date.y)
  #Calculating Fighting Age.
  Fights$Fighter1_Age=year(Fights$Date)-year(Fights$Birth_Date.x)
  Fights$Fighter2_Age=year(Fights$Date)-year(Fights$Birth_Date.y)
  
  Fighting_Age=as.data.frame(unlist(Fights[,c('Fighter1_Age','Fighter2_Age')]))
  colnames(Fighting_Age)='Age'
  Fighting_Age=Fighting_Age %>% group_by(Age) %>% summarise(Count=n())
  g_Age=gvisColumnChart(Fighting_Age,options=gvisOptions)
  output$g_Age=renderGvis(g_Age)
  summary(Fighting_Age)
  
  
  #Win-Rate against Age
  #Win-Rate drop with Age
  Fights$Win=1
  Fights$Loss=1
  Fights$Draw=0
  Fights$Win[Fights$Method %in% c('Draw','No Contest','No Decision')]=0
  Fights$Loss[Fights$Method %in% c('Draw','No Contest','No Decision')]=0
  Fights$Draw[Fights$Method %in% c('Draw')]=1
  
  Win_by_Age=Fights[,c('Fighter1_Age','Win')]
  Loss_by_Age=Fights[,c('Fighter2_Age','Loss')]
  temp1=Fights[,c('Fighter1_Age','Draw')]
  temp2=Fights[,c('Fighter2_Age','Draw')]
  colnames(temp1)=c('Age','Draw')
  colnames(temp2)=c('Age','Draw')
  Draw_by_Age=rbind(temp1,temp2)
  rm(temp1)
  rm(temp2)
  
  #Win-Rate By Age
  Win_by_Age=Win_by_Age %>% group_by(Fighter1_Age) %>% summarise(Numbers=sum(Win))
  Loss_by_Age=Loss_by_Age %>% group_by(Fighter2_Age) %>% summarise(Numbers=sum(Loss))
  Draw_by_Age=Draw_by_Age%>% group_by(Age) %>% summarise(Numbers=sum(Draw))
  
  Win_Rate_by_Age=merge(merge(Win_by_Age,Loss_by_Age,by.x='Fighter1_Age',by.y='Fighter2_Age'),Draw_by_Age,by.x='Fighter1_Age',by.y='Age')
  colnames(Win_Rate_by_Age)=c('Age','Wins','Loss','Draws')
  Win_Rate_by_Age$Win_Rate=Win_Rate_by_Age$Wins/(Win_Rate_by_Age$Wins+Win_Rate_by_Age$Loss+Win_Rate_by_Age$Draws)
  g_Win_Rate_by_Age=ggplot(data=Win_Rate_by_Age,aes(x=Age,y=Win_Rate))+geom_line(color='white')+geom_smooth(se=FALSE)
  g_Win_Rate_by_Age=g_Win_Rate_by_Age+theme_hc()
  g_Win_Rate_by_Age=g_Win_Rate_by_Age+labs(x='Age',y='Win Rate')
  g_Win_Rate_by_Age=g_Win_Rate_by_Age+theme(axis.text=element_text(size=14,colour='white'),axis.title=element_text(size=20,colour='white'),plot.title=element_text(size=25,colour='white'))
  g_Win_Rate_by_Age=g_Win_Rate_by_Age+theme(panel.background = element_blank(),panel.grid.major = element_line(colour='white'),plot.background = element_rect(fill='#272b30'))
  g_Win_Rate_by_Age=g_Win_Rate_by_Age+scale_color_manual(values=c('white','blue'))
  output$g_Win_Age=renderPlot(g_Win_Rate_by_Age)
  
  
  
  #Win-Rate by Country
  Fights=merge(merge(Fights,Fighters,by.x='Fighter1_id',by.y='Fighter_id'),Fighters,by.x='Fighter2_id',by.y='Fighter_id',suffixes=c('_1','_2'))
  
  Win_by_Country=Fights[,c('Country_1','Win')]
  Loss_by_Country=Fights[,c('Country_2','Loss')]
  temp1=Fights[,c('Country_1','Draw')]
  temp2=Fights[,c('Country_2','Draw')]
  colnames(temp1)=c('Country','Draw')
  colnames(temp2)=c('Country','Draw')
  Draw_by_Country=rbind(temp1,temp2)
  rm(temp1)
  rm(temp2)
  
  Win_by_Country=Win_by_Country %>% group_by(Country_1) %>% summarise(Numbers=sum(Win))
  Loss_by_Country=Loss_by_Country %>% group_by(Country_2) %>% summarise(Numbers=sum(Loss))
  Draw_by_Country=Draw_by_Country%>% group_by(Country) %>% summarise(Numbers=sum(Draw))
  Win_Rate_by_Country=merge(merge(Win_by_Country,Loss_by_Country,by.x='Country_1',by.y='Country_2'),Draw_by_Country,by.x='Country_1',by.y='Country')
  colnames(Win_Rate_by_Country)=c('Country','Wins','Loss','Draws')
  Win_Rate_by_Country$Win_Rate=Win_Rate_by_Country$Wins/(Win_Rate_by_Country$Wins+Win_Rate_by_Country$Loss+Win_Rate_by_Country$Draws)
  Win_Rate_by_Country=Win_Rate_by_Country[Win_Rate_by_Country$Country!='',]
  
  #g_Rate=ggplot()+geom_map(data=World,map=World,aes(x=long,y=lat,map_id=region),fill='grey50',color='#ffffff',size=0.15)
  #g_Rate=g_Rate+geom_map(data=Win_Rate_by_Country,map=World,aes(fill=Win_Rate,map_id=Country),color='#ffffff',size=0.15)
  #g_Rate=g_Rate+scale_fill_gradient(low='green',high='red',guide='colorbar',name='Win-Rate by Country')
  #g_Rate=g_Rate+labs(x=NULL,y=NULL)
  #g_Rate=g_Rate+theme(panel.border=element_blank())
  #_Rate=g_Rate+theme(panel.background = element_blank())
  #_Rate=g_Rate+theme(axis.ticks = element_blank())
  #_Rate=g_Rate+theme(axis.text = element_blank())
  
  g_Rate=gvisGeoChart(Win_Rate_by_Country, locationvar="Country", 
                      colorvar="Win_Rate",
                      options=c(colors='["lawngreen","khaki","orangered"]',gvisOptions[1:3]))
  output$g_Rate=renderGvis(g_Rate)
  
  
  #Matches Method
  Method=Fights %>% group_by(Method) %>% summarise(Count=n())
  g_Method=gvisPieChart(Method,options=gvisOptions)
  output$g_Method=renderGvis(g_Method)
  #Winning Techniques.
  
  #No obvious trend in Winning Techniques by Ages.
  Method_by_Age=Fights[!(Fights$Method %in% c('No Contest','Draw','No Decision','DQ')),]
  Method_by_Age=Method_by_Age %>% group_by(Fighter1_Age,Method) %>% summarise(Counts=n()) %>% mutate(Percentage=Counts/sum(Counts))
  #g_Method_by_Age=ggplot(data=Method_by_Age[Method_by_Age$Fighter1_Age>=20 & Method_by_Age$Fighter1_Age<=40,])+geom_area(aes(x=Fighter1_Age,y=Percentage,fill=Method, color=Method), position='stack', alpha=0.6, stat='identity')
  Method_by_Age=cast(Method_by_Age[,-3],Fighter1_Age~Method)
  Method_by_Age=Method_by_Age[Method_by_Age$Fighter1_Age>=20 & Method_by_Age$Fighter1_Age<=40,]
  g_Method_by_Age=gvisAreaChart(Method_by_Age,options=c(isStacked=TRUE,gvisOptions))
  output$g_Method_Age=renderGvis(g_Method_by_Age)
  
  ###################################Shinny App to Chose Win method to draw a pie chart#######################################
  plot_Technique=function(Method){
    #temp=Fights[Fights$Method==Method,]%>%group_by(Method_D) %>% summarise(Count=n()) %>% mutate(pos=sum(Count)-(cumsum(Count)-Count/2))
    #g_Technique=ggplot(data=temp,aes(x=1,y=Count,fill=factor(Method_D))) + geom_bar(stat='identity',na.rm=TRUE) 
    #g_Technique=g_Technique+geom_bar(stat='identity',color='black')+guides(fill=guide_legend(override.aes = list(colour=NA)))
    #g_Technique=g_Technique+geom_text(aes(x=1,y=pos,label=Method_D))
    #g_Technique=g_Technique+ coord_polar(theta='y')
    #g_Technique
    
    temp=Fights[Fights$Method==Method,]%>%group_by(Method_D) %>% summarise(Count=n()) %>% mutate(pos=sum(Count)-(cumsum(Count)-Count/2))
    temp=temp[temp$Method_D!='',]
    g_Technique=gvisPieChart(temp,options=c(title='Winning Technique',gvisOptions))
    return(g_Technique)
  }
  output$g_Technique=renderGvis(plot_Technique(input$Method))
  ###################################Shinny App to Show Detaild Fighter Data##################################################
  find_Fighter=function(name,country,class){
    Pre_filtered=Fighters[,!names(Fighters) %in% c('Url','Height','Fighter_id','PhotoURL')]
    Pre_filtered=Pre_filtered[,c('Name','NickName','Birth_Date','Age','Country','Class','Feet','Inch','Weight')]
#    if (name!=''){
#      Pre_filtered=Pre_filtered[grep(name,Pre_filtered$Name,ignore.case=TRUE),]
#    }
#    if (country!=''){
#      Pre_filtered=Pre_filtered[grep(country,Pre_filtered$Country,ignore.case=TRUE),]#    }
#    if (class!=''){
#      Pre_filtered=Pre_filtered[grep(class,Pre_filtered$Class,ignore.case=TRUE),]
#    }
    return(Pre_filtered)
  }
  output$Fighter_List=renderDataTable(find_Fighter(input$name,input$country,input$class),options=list(pageLength=10,autoWidth=TRUE))
  
  
  Fighter_Exist=function(Name){
  if(nrow(Fighters[Fighters$Name==Name,])!=0){
    Fighter=Fighters[Fighters$Name==Name,]
    return(as.character(Fighter$PhotoURL[1]))
    }else{
      return('Unknown_Fighter.png')
    }
  }
  
  Given=eventReactive(input$Search,{input$Fighter_Name})
  
  N_Wins=Fights %>% group_by(Fighter1_id) %>% summarise(Wins=sum(Win))
  N_Loss=Fights %>% group_by(Fighter2_id) %>% summarise(Loss=sum(Loss))

  
  N_Wins=merge(N_Wins,Fighters[,c('Name','Fighter_id')],by.x='Fighter1_id', by.y='Fighter_id')
  N_Loss=merge(N_Loss,Fighters[,c('Name','Fighter_id')],by.x='Fighter2_id', by.y='Fighter_id')
  
  Ratio_Pie=eventReactive(input$Search,{
    id=Fighters[Fighters$Name==input$Fighter_Name,'Fighter_id']
    per_Win=Fights[Fights$Fighter1_id==id & Fights$Win==1,]
    per_Win$OutCome='Win'
    per_Loss=Fights[Fights$Fighter2_id==id & Fights$Loss==1,]
    per_Loss$OutCome='Loss'
    Ratio=rbind(per_Win,per_Loss) %>% group_by(OutCome) %>% summarise(Count=n())
    gvisPieChart(Ratio,options=c(title='Win-Rate',gvisOptions[-2]))
  })
  
  Win_Pie=eventReactive(input$Search,{
    id=Fighters[Fighters$Name==input$Fighter_Name,'Fighter_id']
    per_Win=Fights[Fights$Fighter1_id==id & Fights$Win==1,]
    per_Win$OutCome='Win'
    per_Win=per_Win %>% group_by(Method) %>% summarise(Total=n())
    gvisPieChart(per_Win,options=c(title='Wins',gvisOptions[-2]))
  })
  
  Loss_Pie=eventReactive(input$Search,{
    id=Fighters[Fighters$Name==input$Fighter_Name,'Fighter_id']
    per_Loss=Fights[Fights$Fighter2_id==id & Fights$Loss==1,]
    per_Loss$OutCome='Loss'
    per_Loss=per_Loss %>% group_by(Method) %>% summarise(Total=n())
    gvisPieChart(per_Loss,options=c(title='Losses',gvisOptions[-2]))
  })
  output$Picture=renderUI({tags$img(src=Fighter_Exist(Given()),style='height:100%;width:100%')})
  output$Name=renderText(Given())
  output$Place=renderText(paste0(Fighters[Fighters$Name==Given(),'Birth_Place'],', ',Fighters[Fighters$Name==Given(),'Country']))
  output$Height=renderText(paste0(Fighters[Fighters$Name==Given(),'Feet'],"'",Fighters[Fighters$Name==Given(),'Inch'],'"'))
  output$Weight=renderText(paste0(Fighters[Fighters$Name==Given(),'Weight'],' lbs'))
  output$Association=renderText(as.character(Fighters[Fighters$Name==Given(),'Association']))
  output$Class=renderText(as.character(Fighters[Fighters$Name==Given(),'Class']))
  output$Wins=renderText(N_Wins$Wins[N_Wins$Name==Given()])
  output$Loss=renderText(N_Loss$Loss[N_Loss$Name==Given()])
  output$Win_Rate=renderGvis(Ratio_Pie())
  
  output$WinPlot=renderGvis(Win_Pie())
  output$LossPlot=renderGvis(Loss_Pie())
})