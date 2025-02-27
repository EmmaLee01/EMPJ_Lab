
footballsub<- footballdata[c(12,19,22,26,29,30)]
footballsub2<-footballsub[footballsub$down==4,]


                           
footballsub3<-footballsub2[footballsub2$play_type=="pass"|
                              footballsub2$play_type=="run"|
                              footballsub2$play_type=="field_goal"|
                              footballsub2$play_type=="punt",]
footballclean<-na.omit(footballsub3)

footballclean