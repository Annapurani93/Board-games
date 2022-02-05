library(tidyverse)
library(tidytuesdayR)
library(ggrepel)

tuesdata <- tidytuesdayR::tt_load('2022-01-25')
tuesdata$details->details
details%>%
  filter(yearpublished==2020|
           yearpublished==2021)%>%
  distinct()->data1

glimpse(details)
tuesdata$ratings->ratings
glimpse(ratings)

data1%>%
  left_join(ratings,by="id")->data2

glimpse(data2)

unique(data2$boardgamedesigner)
data2%>%
  distinct()%>%
  select(name,year,rank,average, bayes_average,playingtime,minage,boardgameartist)->data


ggplot(data,aes(x=playingtime,y=bayes_average,label=name,fill=as.factor(year)))+
  geom_jitter(colour="white",pch=21,alpha=0.84,size=3)+
  labs(fill="Year: ")+
  geom_text_repel(colour="white",fontface="bold",size=3,hjust=-1.2,max.overlaps = 1)+
  theme(plot.background=element_rect(fill="black"),
        panel.background = element_rect(fill="black"),
        legend.background = element_rect(fill="black"),
        legend.text = element_text(colour="white",face = "bold",margin = margin(l=5)),
        legend.title = element_text(colour="white",face = "bold",size=12,margin = margin(b=5)),
        legend.box.margin = unit(c(1,2,1,2),"cm"),
        legend.key = element_rect(fill="black"),
        plot.margin = unit(c(1,2,1,2),"cm"),
        panel.grid = element_blank(),
        axis.ticks = element_blank(),
        axis.text = element_text(colour="white",face = "bold"),
        axis.title.x = element_text(colour="white",face="bold",size=12, margin=margin(t=25)),
        axis.title.y = element_text(colour="white",face="bold",size=12, margin=margin(r=25)),
        plot.title.position = "plot",
        plot.caption.position = "plot",
        plot.title=element_text(size=14, face="bold",colour="white",margin=margin(b=15)),
        plot.subtitle = element_text(size=12, colour="white",margin=margin(b=25)),
        plot.caption=element_text(size=10,colour="white",hjust=0,margin=margin(t=20)))+
  labs(x="------PLAYING TIME IN MINUTES------",
       y="------AVERAGE BAYES RATING------",
       title="WHAT BOARD GAMES DID PEOPLE PLAY THE MOST DURING COVID?",
       subtitle=str_wrap("Around 1,575 board games were played in 2020 and 2021, and all of them except for 7 games were played for less than 24 hours. 
                         While The Third Winter: The Battle for the Ukraine September 1943 - April 1944 was the most played board game, Gloomhaven: Jaws of the Lion was the highest rated.",100),
       caption = "Data via Tidy Tuesday| Analysis and design: @annapurani93")->plot



ggsave("boardgames.png",plot,width = 12,height=8.4)
