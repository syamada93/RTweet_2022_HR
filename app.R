library(shiny)

if(!require(data.table)){
  install.packages("data.table")
  library(data.table)
}
if(!require(dplyr)){
  install.packages("dplyr")
  library(dplyr)
}
if(!require(tidyr)){
  install.packages("tidyr")
  library(tidyr)
}
if(!require(stringi)){
  install.packages("stringi")
  library(stringi)
}
if(!require(rtweet)){
  install.packages("rtweet")
  install.packages('https://cran.r-project.org/src/contrib/Archive/rtweet/rtweet_0.7.0.tar.gz', repos=NULL, type='source')
  library(rtweet)
}
if(!require(lubridate)){
  install.packages("lubridate")
  library(lubridate)
}
if(!require(ggplot2)){
  install.packages("ggplot2")
  library(ggplot2)
}
if(!require(dygraphs)){
  install.packages("dygraphs")
  library(dygraphs)
}
if(!require(magick)){
  install.packages("magick")
  library(magick)
}
if(!require(tidytext)){
  install.packages("tidytext")
  library(tidytext)
}
if(!require(topicmodels)){
  install.packages("topicmodels")
  library(topicmodels)
}
if(!require(tidygraph)){
  install.packages("tidygraph")
  library(tidygraph)
}
if(!require(ggraph)){
  install.packages("ggraph")
  library(ggraph)
}
if(!require(bit64)){
  install.packages("bit64")
}

Unzip <- function(...) rbind(data.frame(), ...)
ggColorHue <- function(n, l=65) {
  hues <- seq(15, 375, length=n+1)
  hcl(h=hues, l=l, c=100)[1:n]
}

#UI####
ui <- fluidPage(
  # Application title
  titlePanel("「大雨」ツイートの添付画像と共起ネットワーク"),
  
  # Sidebar with a slider input for number of bins 
  fluidRow(
    sidebarPanel(
      h4(column(2,
                radioButtons(inputId = "sort",
                             label = "画像順序",
                             choices = c("出現頻度"=1,"最新投稿"=2),
                             selected = 1,
                             inline = T))),
      br(),
      br(),
      width = 12
    ),
    
    # Show a plot of the generated distribution
    
    mainPanel(
      # dygraphOutput("Hdy",height="110px"),
      column(6,plotOutput("ggraph",height = "100%",width = "100%")),
      column(6,plotOutput("RTweet",height = "100%",width = "100%",hover = hoverOpts(id ="plot_hover"))),
      verbatimTextOutput("info"),
      width = 12
    ),
    tags$style(type="text/css",
               "#info  {font-size: 2vh !important;}",
               "#RTweet{height:  75vh !important;
                        margin-left: auto;
                        margin-right: auto;}",
               "#ggraph{height:  75vh !important;
                        margin-left: auto;
                        margin-right: auto;}")
    
  )
)

#SERVER####
server <- function(input, output) {
  refreshPlot0 <- reactiveTimer(intervalMs = 120000)
  
  # wd="大雨"
  sort=2
 
  observe({
    refreshPlot0()
    print(Sys.time())
    sort=input$sort
    
    TDPC <-
      fread("https://raw.githubusercontent.com/syamada93/RTweet_2022_HR/master/Tweet_Photo.tsv") %>%
      # mutate(JTime=JTime+9*60*60) %>%
      mutate(RTime=RTime+9*60*60) 
    print(max(TDPC$JTime))
    print(Sys.time()-max(TDPC$RTime-9*60*60))
    
    if(sort==1){
      TDPC0 <-
        TDPC %>%
        # filter(nf>0 | nr>0) %>%
        filter(!grepl("おは",text)) %>%
        filter(!grepl("^@",text)) %>%
        mutate(Rank=frank(-n,ties.method = "max")) %>%
        arrange(Rank,desc(nf),desc(nr),RID) %>%
        filter(Rank<=20|n==max(n)) %>%
        filter(RID %in% unique(RID)[1:20]) %>%
        ungroup()
    }
    
    if(sort==2){
      TDPC0 <-
        TDPC %>%
        # filter(nf>0 | nr>0) %>%
        filter(!grepl("おは",text)) %>%
        filter(!grepl("^@",text)) %>%
        mutate(Rank=frank(-n,ties.method = "max")) %>%
        arrange(desc(RTime),Rank,desc(nf),desc(nr),RID) %>%
        filter(RID %in% unique(RID)[1:20]) %>%
        ungroup()
    }
    
    
    ID=unique(TDPC0$RID)
    XY=data.frame(RID=ID,sx=0,lx=0,sy=0,ly=0)
    n=nrow(XY)
    ro=round(sqrt(n))
    co=ceiling(n/ro)
    r=ro
    p=ggplot() +
      scale_x_continuous(limits = c(0,co),expand = c(0,0)) +
      scale_y_continuous(limits = c(0,ro),expand = c(0,0)) +
      theme(axis.title = element_blank()) +
      theme(axis.text  = element_blank()) +
      theme(axis.ticks = element_blank())
    
    i=1
    for (i in 1:n) {
      id=ID[i]
      XY[i,-1] = c(floor((i-1)/ro),floor((i-1)/ro)+1,r,r-1)
      
      (JPG <-
          try(image_scale(
            image_read(TDPC$Purl[which(TDPC$RID==id)]), geometry = 960)))
      if(sum(class(JPG)=="try-error"))
        next
      
      GIF <-
        image_append(JPG, stack = F)
      if(length(JPG)>2){
        GIF12 <-
          image_append(JPG[1:2], stack = T)
        GIF34 <-
          image_append(JPG[-(1:2)], stack = T)
        GIF <-
          image_append(image_scale(c(GIF12,GIF34)), stack = F)
      }
      
      p <-
        p +
        annotation_raster(GIF,XY$sx[i],XY$lx[i],XY$sy[i],XY$ly[i])
      # plot(p)
      r=r-1
      if(r==0)
        r=ro
    }
    
    output$RTweet <- renderPlot({
      plot(p)
    })
    
    TDPCS <-
      TDPC0 %>%
      distinct(RID,.keep_all = T) %>%
      left_join(XY)
    
    
    output$info <- renderPrint({
      if(!is.null(input$plot_hover)){
        hover=input$plot_hover
        hover$x=hover$x*co
        hover$y=hover$y*ro
        w=which(hover$x>XY$sx&hover$x<XY$lx&hover$y<XY$sy&hover$y>XY$ly)
        paste(TDPCS$RTime[w],paste0(TDPCS$n[w],"ツイート"),paste0(TDPCS$nf[w],"いいね"),TDPCS$text[w])
      }
    })
    
    TFSS <-
      fread("https://raw.githubusercontent.com/syamada93/RTweet_2022_HR/master/Tweet_word_RmeCab.tsv",encoding = "UTF-8") %>%
      filter(Purl %in% TDPC0$Purl)
      # filter(status_id %in% TDPC0$RID)
    if(sort==2)
    TFSS <-
      fread("https://raw.githubusercontent.com/syamada93/RTweet_2022_HR/master/Tweet_word_RmeCab.tsv",encoding = "UTF-8") %>%
      # FSS %>%
      # filter(Purl %in% TDPC0$Purl)
      filter(status_id %in% TDPC0$RID)
      # distinct(RID,word,.keep_all = T) %>%
      # # filter(!N1=="大雨") %>%
      # group_by(N1) %>%
      # mutate(total=n()) %>%
      # group_by(ID) %>%
      # mutate(word1=N1) %>%
      # mutate(total1=total) %>%
      # mutate(word2=lead(word1)) %>%
      # mutate(total2=lead(total1)) %>%
      # mutate(POS12=lead(POS1)) %>%
      # mutate(POS22=lead(POS2)) %>%
      # mutate(word=paste0(word1,word2)) %>%
      # ungroup()
    
    TFSC <-
      TFSS %>%
      distinct(status_id,Tweet,JTime,RID,RTime,word1,word2,total1,total2) %>%
      # distinct(Tweet,word1,word2,.keep_all = T) %>%
      group_by(word1,word2,total1,total2) %>%
      summarise(n=n(),mid=min(RID)) %>%
      mutate(rate1=n/total1) %>%
      mutate(rate2=n/total2) %>%
      mutate(Rate0=rate1*rate2) %>%
      ungroup() %>%
      na.omit() %>%
      filter(word1!=word2) %>%
      mutate(Rank=frank(-n,ties.method = "min")) %>%
      mutate(Rankd=frank(-n,ties.method = "dense")) %>%
      arrange(Rank) %>%
      mutate(word=paste0(word1,word2),drow=ifelse(total1>total2,paste0(word1,word2),paste0(word2,word1))) %>%
      arrange(Rank,desc(rate1),desc(total1)) %>%
      ungroup()
    
    TFSC0 <-
      TFSC %>%
      filter(total1>1) %>%
      filter(total2>1) %>%
      mutate(rRank=frank(-rate1,ties.method = "min")) %>%
      mutate(Rank=ifelse(rep(sort,n())==2,rRank,Rank)) %>%
      # mutate(no=1:n()) %>%
      # group_by(drow) %>%
      filter(Rank<=50) %>%
      ungroup()
    
    Word50=unique(c(TFSC0$word1,TFSC0$word2))
    
    df_id_tokens <-
      TFSS %>%
      filter(word1 %in% Word50 | word2 %in% Word50) %>%
      filter(total>1) %>%
      select(status_id,word1) %>%
      mutate(n=1)
    
    k=5
    DTM_id <- cast_dtm(df_id_tokens, document = "status_id", term = "word1", value = "n")
    res_lda <- LDA(DTM_id,k=k)
    topic_num <- topics(res_lda)
    # topic_num
    
    TN <-
      topic_num %>%
      data.frame() %>%
      rename(Topic=1) %>%
      add_rownames("status_id") %>%
      mutate(Topic=as.numeric(Topic)) %>%
      ungroup()
    
    TFSS2 <-
      TFSS %>%
      # distinct(status_id,word1,word2,.keep_all = T) %>%
      select(status_id,ID,JTime,RID,RTime,Tweet,wn,OW,OWS,word1,word2,Purl) %>%
      mutate(status_id=as.character(status_id)) %>%
      inner_join(TFSC) %>%
      left_join(TN) %>%
      mutate(tw1=paste0(word1,"_",Topic)) %>%
      mutate(tw2=paste0(word2,"_",Topic)) %>%
      group_by(tw1) %>%
      mutate(Ttw1=n()) %>%
      group_by(tw2) %>%
      mutate(Ttw2=n()) %>%
      ungroup() %>%
      filter(word %in% TFSC0$word) %>%
      arrange(desc(status_id),wn) %>%
      mutate(rate=rate1) %>%
      mutate(nn=n) %>%
      mutate(Year=year(JTime)) %>%
      mutate(Month=month(JTime)) %>%
      mutate(Day=mday(JTime)) %>%
      mutate(Hour=hour(JTime)) %>%
      group_by(Topic,word) %>%
      mutate(tn=n()) %>%
      ungroup() %>%
      mutate(Stime=as.POSIXct(paste(Year,Month,Day,Hour),format="%Y %m %d %H"))
    
    # if(sort==1)
    {
      TFSS3 <-
        TFSS2 %>%
        distinct(status_id,word1,word2,.keep_all = T) %>%
        count(word1,word2,n,nn,Rank,rate,Topic,tw1,tw2,tn) %>%
        arrange(Rank,desc(tn)) %>%
        group_by(Topic) %>%
        mutate(tn2=n()) %>%
        select(tw1,tw2,one_of(colnames(TFSS2))) %>%
        ungroup()
      
    TFSS4 <-
      TFSS3 %>%
      group_by(word1,word2,Topic) %>%
      mutate(wc11=sum(tw1 %in% TFSS3$tw1)) %>%
      mutate(wc12=sum(tw1 %in% TFSS3$tw2)) %>%
      mutate(wc21=sum(tw2 %in% TFSS3$tw1)) %>%
      mutate(wc22=sum(tw2 %in% TFSS3$tw2)) %>%
      filter(wc12>0|wc21>0) %>%
      # arrange(desc(tn2)) %>%
      # select(word1,word2,one_of(colnames(.))) %>%
      # distinct(word1,word2,.keep_all = T) %>%
      ungroup()
    
    TFSS_topic <-
      data.frame() %>%
      rbind(TFSS2 %>%
              select(word=tw1,freq=Ttw1,Cluster=Topic)) %>%
      rbind(TFSS2 %>%
              select(word=tw2,freq=Ttw2,Cluster=Topic)) %>%
      distinct() %>%
      group_by(word) %>%
      mutate(n=max(freq)) %>%
      ungroup() %>%
      distinct(word,n,.keep_all = T) %>%
      arrange(desc(n)) %>%
      mutate(Cluster=factor(Cluster)) %>%
      # mutate(word=gsub("_[[:digit:]]","",word))%>%
      # distinct(word,.keep_all = T) %>%
      ungroup()
    
    g <- as_tbl_graph(TFSS4, directed = T) %>%
      left_join(TFSS_topic,by=c("name"="word")) %>%
      ungroup()
    
    G <- data.frame(g) %>%
      mutate(Cluster=as.numeric(as.character(Cluster))) %>%
      mutate(word=name) %>%
      left_join(TFSS4 %>% distinct(tw1,word1),by=c("name"="tw1")) %>%
      left_join(TFSS4 %>% distinct(tw2,word2),by=c("name"="tw2")) %>%
      mutate(word=ifelse(!is.na(word1),word1,word2)) %>%
      # mutate(word=iconv(word,"CP932","UTF-8")) %>%
      ungroup()
    
    mn=max(G$n)
    keta=floor(log10(mn))-1
    fn=mn/11/10^keta
    ziku=ifelse(fn<5,5*10^(keta),10^(keta+1))
    al=c(min(TFSS4$rate),max(TFSS4$rate))
    es=min(TFSS4$nn)/max(TFSS4$nn)*3
    cs=min(G$n)/max(G$n)*20
    
    (p2<-
        g %>%
        ggraph(layout ="nicely") +
        geom_node_point(aes(size = n),col=ggColorHue(min(k,10))[G$Cluster]) +
        geom_edge_link(aes(width = nn, alpha = rate),color="#0F0F0F", #
                       arrow = arrow(length = unit(5,'mm')), end_cap = circle(7,'mm'),force_flip = F) +
        geom_node_text(aes(),label = G$word, repel = F, size=10) +
        ggtitle(paste0("「大雨」を含む画像付きツイートの要約共起ネットワーク\n",min(TDPC$JTime),"~",max(TDPC$JTime)," ",length(unique(G$word)),"単語")) + #,k,"ツイート　","\n",nrow(TFS00),"ルール"
        theme_graph(title_size = 30) +
        scale_edge_alpha(range = c(0.1,1)) +
        scale_edge_width(range = c(es,3)) +
        scale_size_continuous(range = c(cs,20+cs),breaks = seq(0,mn,ziku)) + #,breaks = seq(0,floor(mn/keta)*keta,by = keta)
        theme( legend.text =  element_text(size = 30), # 凡例
               legend.title = element_text(face = "bold", size = 10, hjust = 0)) +
        guides(alpha = guide_legend(title = "LEFT", title.position = "left")) +
        guides(colour = guide_legend(order=1 , override.aes = list(size=10)),
               size   = F, #guide_legend(order=2)
               edge_width = F,
               edge_alpha = F))
    }
    # if(sort==2){
    #   TFSS3 <-
    #     TFSS2 %>%
    #     distinct(status_id,word1,word2,.keep_all = T) %>%
    #     count(word1,word2,n,nn,Rank,rate,Topic,tw1,tw2,tn) %>%
    #     arrange(Rank,desc(tn)) %>%
    #     group_by(Topic) %>%
    #     mutate(tn2=n()) %>%
    #     # select(tw1,tw2,one_of(colnames(TFSS2))) %>%
    #     ungroup()
    #   TFSS4 <-
    #     TFSS3 %>%
    #     group_by(word1,word2,Topic) %>%
    #     mutate(wc11=sum(tw1 %in% TFSS3$tw1)) %>%
    #     mutate(wc12=sum(tw1 %in% TFSS3$tw2)) %>%
    #     mutate(wc21=sum(tw2 %in% TFSS3$tw1)) %>%
    #     mutate(wc22=sum(tw2 %in% TFSS3$tw2)) %>%
    #     filter(wc12>0|wc21>0) %>%
    #     arrange(desc(tn2)) %>%
    #     select(word1,word2,one_of(colnames(.))) %>%
    #     distinct(word1,word2,.keep_all = T) %>%
    #     ungroup()
    #   
    #   TFSS_topic <-
    #     data.frame() %>%
    #     rbind(TFSS4 %>%
    #             select(word=word1,freq=n,Cluster=Topic)) %>%
    #     rbind(TFSS4 %>%
    #             select(word=word2,freq=n,Cluster=Topic)) %>%
    #     distinct() %>%
    #     group_by(word) %>%
    #     mutate(n=max(freq,na.rm = T)) %>%
    #     ungroup() %>%
    #     distinct(word,n,.keep_all = T) %>%
    #     arrange(desc(n)) %>%
    #     mutate(Cluster=factor(Cluster)) %>%
    #     # mutate(word=gsub("_[[:digit:]]","",word))%>%
    #     distinct(word,.keep_all = T) %>%
    #     ungroup()
    #   
    #   g <- as_tbl_graph(TFSS4, directed = T) %>%
    #     left_join(TFSS_topic,by=c("name"="word")) %>%
    #     ungroup()
    #   
    #   G <- data.frame(g) %>%
    #     mutate(Cluster=as.numeric(as.character(Cluster))) %>%
    #     mutate(word=name) %>%
    #     # left_join(TFSS4 %>% distinct(tw1,word1),by=c("name"="tw1")) %>%
    #     # left_join(TFSS4 %>% distinct(tw2,word2),by=c("name"="tw2")) %>%
    #     # mutate(word=ifelse(!is.na(word1),word1,word2)) %>%
    #     # mutate(word=iconv(word,"CP932","UTF-8")) %>%
    #     ungroup()
    #   
    #   mn=max(G$n)
    #   keta=floor(log10(mn))-1
    #   fn=mn/11/10^keta
    #   ziku=ifelse(fn<5,5*10^(keta),10^(keta+1))
    #   al=c(min(TFSS4$rate),max(TFSS4$rate))
    #   es=min(TFSS4$nn)/max(TFSS4$nn)*3
    #   cs=min(G$n)/max(G$n)*10
    #   
    #   (p2<-
    #       g %>%
    #       ggraph(layout ="nicely") +
    #       geom_node_point(aes(size = n),col=ggColorHue(min(k,10))[G$Cluster]) +
    #       geom_edge_link(aes(width = nn, alpha = rate),color="#0F0F0F", #
    #                      arrow = arrow(length = unit(5,'mm')), end_cap = circle(7,'mm'),force_flip = F) +
    #       geom_node_text(aes(),label = G$word, repel = F, size=10) +
    #       ggtitle(paste0("「大雨」を含む画像付きツイートの要約共起ネットワーク\n",min(TDPC$JTime),"~",max(TDPC$JTime)," ",length(unique(G$word)),"単語")) + #,k,"ツイート　","\n",nrow(TFS00),"ルール"
    #       theme_graph(title_size = 30) +
    #       scale_edge_alpha(range = c(0.1,1)) +
    #       scale_edge_width(range = c(es,3)) +
    #       scale_size_continuous(range = c(cs,10+cs),breaks = seq(0,mn,ziku)) + #,breaks = seq(0,floor(mn/keta)*keta,by = keta)
    #       theme( legend.text =  element_text(size = 30), # 凡例
    #              legend.title = element_text(face = "bold", size = 10, hjust = 0)) +
    #       guides(alpha = guide_legend(title = "LEFT", title.position = "left")) +
    #       guides(colour = guide_legend(order=1 , override.aes = list(size=10)),
    #              size   = F, #guide_legend(order=2)
    #              edge_width = F,
    #              edge_alpha = F))
    # }
    
    output$ggraph <- renderPlot({
      plot(p2)
    })
  })
}

# Run the application 
shinyApp(ui = ui, server = server)




