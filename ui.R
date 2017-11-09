library(shinythemes)
shinyUI(fluidPage(theme=shinytheme('slate'),
  div(style='height:80px;width:100%;background:#0a0a0a',
      img(src='ufc-logo-black-bkgd.jpg',id='logo',width='500px',height='80px',align='right')
  ),
  titlePanel("UFC Data Analysis"),
  
  hr(),

  h2('What is UFC?'),
  p('The Ultimate Fighting Championship (UFC) is an American mixed martial arts promotion company, a subsidiary of the parent company William Morris Endeavor, based in Las Vegas, Nevada. It is the largest MMA promoter in the world and features most of the top-ranked fighters in the sport. Based in the United States, the UFC produces events worldwide that showcase ten weight divisions and abide by the Unified Rules of Mixed Martial Arts. The UFC has held over 300 events to date. Dana White serves as the president of the UFC.
  '),
  hr(),
  
  tabsetPanel(

              tabPanel('Summary',
                       h2('Summary:'),
                       p(textOutput('summary')),

                       sidebarLayout(
                          sidebarPanel(
                          h3('Histroy Events:'),
                          p('From the map on the right side, we can see that UFC has held events all over the world in its history. However since UFC is a American based competition, it is very obvious that most of the events were held in the United States, especially in Las Vegas. Besides United States, Brazil held the second most events and Canada the third. You can zoom-in (out) to see how many events has been held at each specific stadium.'),
                          htmlOutput('g_event_country')
                          ),

                          mainPanel(
                            p(),
                            leafletOutput('Events')
                          )

                       ),

                        sidebarLayout(
                          sidebarPanel(
                            h3('Number of Fighters from Each Country:'),
                            p('As we can expect, it is pretty obvious that most of the fighters come from United States, followed by Brazil and Canada. Although there are also fighters from other countries, as these 3 contries have significantly head-count than the other countries, they have dominated the map. Thus, we give an option to exclude them in order to exhibit a map of \'the other countries\'.'),
                            checkboxGroupInput('Excluded_Country','Exclude the Country listed below:',c('United State'='USA','Brazil'='Brazil','Canada'='Canada'))
                          ),
                          mainPanel(
                            p(),
                            htmlOutput('g_World',style='padding:2%;')
                          )
                        ),
                       sidebarLayout(
                         sidebarPanel(
                           h3('Win-Rate by Country:'),
                           p('Another interesting factor is that: '),
                           p('Although United States owns the most UFC fighters and has held the most UFC events in the world, its win-rate is just slightly less than 50%. Sames story for Canada. There are countries having high win-rate however with low participating. Thus their win-rate can not precisely indicate their martial-art level. Among all the country which has more than 20 fighters participating in the UFC, Russia has the highest win-rate at 61.6%, followed by Mexico with 57.8% and Brazil with 55.1%'),
                           p('You can move your mouse onto the each and check the win-rate of each country.')
                         ),
                         mainPanel(
                           htmlOutput('g_Rate',style='padding:2%;')
                         )
                       )

                      ),

                tabPanel('Fighters',
                         sidebarLayout(
                           sidebarPanel(
                             h3('Classes'),
                             p('In UFC, there are totally 10 Classes, corresponding to 10 different weigth ranges. The detail of all different classes is shown below:'),
                             a(href='https://en.wikipedia.org/wiki/Ultimate_Fighting_Championship',img(src='Classes.png',style='width:100%;height:70%'))
                           ),
                           mainPanel(
                             htmlOutput('g_Classes')
                           )
                         ),

                         sidebarLayout(
                           sidebarPanel(
                             h3('Age Groups'),
                             p(em('Fighting is a game of the young.'),'It is not all true here. The barchart on the right is a summary regarding the ages of each fighter when he/she fought in a match. Most of the fighters are aged between 20~40 years old, with the majority at 28. UFC fighters require years of intensive training. So this graph makes proper sense. Also, as a sport, martial art requires the participant\'s physical strength, which is highly correlated with age.')
                           ),
                           mainPanel(
                             htmlOutput('g_Age')
                           )
                         ),

                         sidebarLayout(
                           sidebarPanel(
                             h3('Win-Rate by Age'),
                             p('But how would a fighter\'s age affect his win-rate?'),
                             p('From the line graph we can see a clear negative correlation of win-rate and age. The win-rate drops from almost 68% all the way down to almost 42%, and recurves back a little bit after age of 40. After Age of 40, the win-rate bounces up and down because of its low population (Thus, the outcome of one match can affect the win-rate a lot). So, as a UFC fighter, one should really give everything he/she got when they are still young. Because the older they get, the harder they can win a game.')
                           ),
                           mainPanel(align='center',
                             plotOutput('g_Win_Age',width='85%')
                           )
                         )
                      ),

                tabPanel('Fights',
                         sidebarLayout(
                           sidebarPanel(
                             h3('Match Outcomes:'),
                             p(em(strong('There are 9 outcomes of a match: ')),br(),
                               em(strong('No Contest:')),'a fighter is rendered unable to continue or compete effectively as a result of an unintentional illegal elstrongent or move and there is not a sufficient number of finished rounds to be judged to make a technical decision viable, or both fighters are rendered unable to continue or compete effectively.',br(),
                               em(strong('DQ (Disqualification): ')),'a fighter intentionally executes an illegal move that is considered by the referee or opponent to be injurious or significant enough to negatively alter the opponent\'s performance should the fight continue, resulting in the opponent\'s victory.',br(),
                               em(strong('KO (Knockout): ')),'a fighter is put into a state of unconsciousness resulting from any legal strike.',br(),
                               em(strong('Submission: ')),'a fighter clearly taps the mat or his opponent, verbally submits, or clearly communicates being in pain (such as by yelling) to a degree that causes the referee to stop the fight. Also, a technical submission may be called when a fighter either loses consciousness or is on the verge of or suffers serious injury while in a hold.',br(),
                               em(strong('Decision: ')),'by scoring',br(),
                               em(strong('TKO (Technical Knockout): ')),'If the referee decides a fighter cannot continue, the fight is ruled as a technical knockout.',br(),
                               em(strong('Technical Submission: ')),'Same idea as Techinical Knockout',br(),
                               em(strong('Draw: ')),'Draw',br(),
                               em(strong('Others')),'Anything not listed above'
                             )
                           ),
                           mainPanel(
                             htmlOutput('g_Method')
                           )
                         ),

                         sidebarLayout(
                           sidebarPanel(
                             h3('Winning Method by Age: '),
                             p('Since age has an effect on fighter\'s win-rate. Would it afftect a fighter\'s winning technique?',br(),'From the area graph on the right, we can see that might not be the case. All trend lines moves almost horizontally along the x-axis. But we might have observed an increase of KO ratio in the graph ( from 14% to 20%). A probable explanation is that with more fighting experience, a fighter can better use strategies and take use of opponent\'s openings')
                           ),
                           mainPanel(
                             htmlOutput('g_Method_Age')
                           )
                         ),
                         sidebarLayout(
                           sidebarPanel(
                             h3('Winning Techniques: '),
                             p('As for each way of winning, there are detailed technique used to secure that win. You can select a winning method below. And the pie chart on the right will show the proportion of each technique.'),
                             selectInput(
                               'Method','Selet a method below:',list('Decision','DQ','Draw','KO','No Contest','Do Decision','Submission','Technical Decision','Technical Submission','TKO')
                             ),
                             p('As an summary from the pie-charts: ',br(),
                               'A significant finding is that: among all techniques used to secure a \'KO\', a punch is the most effective way to do the job, regardless of all different martial art systems.',br(),
                               'Among all techniques used to secure a \'Submission\',  head-lock, arm-bar and guillotine are mostly used.',br(),
                               'This prove the saying: ',em('You should put most of your time on the basics, since that\'s going to be the most effctive way to end your enemy'))
                           ),
                           mainPanel(
                             htmlOutput('g_Technique')

                           )
                         )
                      ),

                tabPanel('Fighter Search Wizzard',
                         verticalLayout(
                          dataTableOutput('Fighter_List')
                         ),
                         sidebarLayout(
                           sidebarPanel(
                             textInput('Fighter_Name','Please give a fighter name to see his/her detailed information'),
                             actionButton('Search','Search',style='align:right')
                           ),
                           mainPanel(
                             div(style='height:788px',
                               div(style='width:30%;height:80%;float:left;border-style:solid;border-width:4px',htmlOutput('Picture')),
                               div(style='width:30%;height:80%;float:left;margin-left:5%',
                                   span(style='font-size:20px',
                                   p(strong(em('Name: ')),textOutput('Name')),
                                   p(strong(em('From: ')),textOutput('Place')),
                                   p(strong(em('Height: ')),textOutput('Height')),
                                   p(strong(em('Weight: ')),textOutput('Weight')),
                                   p(strong(em('Association: ')),textOutput('Association')),
                                   p(strong(em('Class: ')),textOutput('Class')),
                                   p(strong(em('Total Wins: ')),textOutput('Wins')),
                                   p(strong(em('Total Losses: ')),textOutput('Loss'))
                                   )),

                               div(style='width:30%;height:80%;float:left;margin-left:5%',
                                   htmlOutput('Win_Rate'),
                                   htmlOutput('WinPlot'),
                                   htmlOutput('LossPlot')
                                   )
                             )

                           )
                         )
                         )

  )
))
