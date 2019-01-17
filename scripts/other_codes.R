## for visulization :    
if(F){
  output$chooseIP_checkbox <- renderUI({
    tmp1=glob_values$results
    if(! is.null(tmp1)){ 
      currentIPs=sort(unique(tmp1$IP))
      checkboxGroupInput("choosed_IPs","Choose IP(s):",currentIPs,inline = T) 
    }else{
      return(NULL)
    }
    
  })
  
  observe({
    tmp1=glob_values$results
    if(! is.null(tmp1)){ 
      currentIPs=sort(unique(tmp1$IP))
      if(input$selectALL_button == 0) return(NULL)
      else if (input$selectALL_button%%2 == 0)
      {
        updateCheckboxGroupInput(session,"choosed_IPs","Choose IP(s):",choices=currentIPs,inline = T)     
      }
      else
      {
        updateCheckboxGroupInput(session,"choosed_IPs","Choose IP(s):",choices=currentIPs,selected=currentIPs,inline = T)
      }
    }else{
      return(NULL)
    }
  })
  
  observeEvent(input$drawSushi,{
    tmp1=glob_values$results
    if(! is.null(tmp1)){ 
      if( ! is.null(input$choosed_IPs)){
        a=tmp1[ tmp1$IP %in% input$choosed_IPs,]
        
        dat=a[,c('chrom' ,'start','end')]
        dat$name=paste(a$cellline,a$IP,a$GSM,sep = '_')
        dat$score=0
        dat$strand='.'
        dat= dat[order(dat$name),]
        dat$row=as.numeric(factor(dat$name))
      }else{ ## if there's no IPs
        return(NULL)
      }
    }else{ ## if there's no peaks
      return(NULL)
    }
    glob_values$sushi_dat =dat
  })
  
  
  output$sushi_peaks <-renderPlot( {
    
    dat=glob_values$sushi_dat 
    if( ! is.null( dat )){
      
      chrom=dat$chrom[1] 
      chromstart=min(c(dat$start,dat$end))-500
      chromend=max(c(dat$start,dat$end))+500
      par(mar=c(5,15,5,5))
      plotBed(beddata    = dat,chrom = chrom,
              chromstart = chromstart,chromend =chromend,
              rownumber  = dat$row, type = "region",
              color=dat$color,row="given",
              plotbg="grey95",rowlabels=unique(dat$name),
              rowlabelcol=unique(dat$color),rowlabelcex=0.75)
      labelgenome(chrom,chromstart,chromend,n=3,scale="Kb")
      mtext("ChIP-seq",side=3, adj=-0.065,line=0.5,font=2)
      
    }else{ ## if there's no IPs
      return(NULL)
    } 
  },
  height = function() {
    #session$clientData$output_plot1_width 
    400+10*nrow(glob_values$sushi_dat) }
  
  )  ## end for sushi_peaks
  
  
}



## page for statistics




# output$stat_figure <- renderPlot({
#   metadata_tab=paste0(input$stat_database,'_metadata')
#   sql=paste0(" select * from ",metadata_tab," where species=",shQuote(input$stat_species)," and type=",shQuote(input$stat_IP))
#   dat <- mysql_getData(sql)
#   
#   tmp=sort(table(dat$bs1),decreasing = T);tmp=tmp[tmp>20] 
# 
#   dat1=data.frame(name=names(tmp),number= as.numeric(tmp),stringsAsFactors = F)
#   dat1$name <- factor( dat1$name,  levels=dat1$name )
#   dat1$type='cellline'
#   
#   tmp=sort(table(dat$IP),decreasing = T);tmp=tmp[tmp>10]
#   dat2=data.frame(name=names(tmp),number= as.numeric(tmp),stringsAsFactors = F)
#   dat2$name <- factor( dat2$name,  levels=dat2$name )
#   dat2$type='IP'
#   
#   
#  
#   
#   get_a <- function(){
#     dat=dat1
#     p <- ggplot(dat, aes(x = name, y = number , label = number,theme_set(theme_bw()) )) +
#       geom_bar(stat = "identity",fill='steelblue')+ylab('Number of samples')+xlab('Cell Line')+
#       geom_text(size = 4,color='white', position = position_stack(vjust = 0.5))+coord_flip()+
#       theme_set(theme_set(theme_bw(base_size=20)))+
#       theme(text=element_text(face='bold'),
#             axis.text.x=element_text(angle=30,hjust=1,size =15),
#             plot.title = element_text(hjust = 0.5) ,
#             panel.grid = element_blank() 
#       )
#     return(p)
#   }
#   
#   get_b <- function(){
#     dat=dat2
#     p <- ggplot(dat, aes(x = name, y = number , label = number,theme_set(theme_bw()) )) +
#       geom_bar(stat = "identity",fill='steelblue') +ylab('Number of samples')+xlab('Factors/Marks')+
#       geom_text(size = 3,color='white', position = position_stack(vjust = 0.5))+coord_flip()+
#       theme_set(theme_set(theme_bw(base_size=10)))+
#       theme(text=element_text(face='bold'),
#             axis.text.x=element_text(angle=30,hjust=1,size =8),
#             plot.title = element_text(hjust = 0.5) ,
#             panel.grid = element_blank() 
#       )
#     return(p)
#   }
#   
#   first_row <- plot_grid(get_a(), ncol = 1, labels = c("cell-line" ))
#   second_row <- plot_grid(get_b(), ncol = 1, labels = c("IP"))
#   plot_grid(first_row, second_row, ncol = 2,nrow = 1)
#   
#   
#   
# })
# 
# 