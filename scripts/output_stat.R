## for homepage 
output$results_stat <- renderPlot({
  tmp1=glob_values$results
  if(! is.null(tmp1)){
    tmp=unique(tmp1[,c('sampleID','IP')])
    tmp=sort( table( tmp$IP ),decreasing = T)
    #barplot(tmp,las=2)
    library(ggplot2)
    dat=data.frame(IP=names(tmp),number= as.numeric(tmp),stringsAsFactors = F)
    dat$IP <- factor( dat$IP,  levels=dat$IP )
    
    p <-  ggplot(dat, aes(x = IP, y = number , label = number,theme_set(theme_bw()) )) +
      geom_bar(stat = "identity",fill='steelblue',color='red') +ylab('Number of samples')+
      geom_text(size = 6,color='white', position = position_stack(vjust = 0.5))+
      theme_set(theme_set(theme_bw(base_size=20)))+
      theme(text=element_text(face='bold'),
            axis.text.x=element_text(angle=30,hjust=1,size =15),
            plot.title = element_text(hjust = 0.5) ,
            panel.grid = element_blank() 
      )
    
    #ggplotly(p) 
    p
  }
})

## for statistics page

output$stat_table <-  DT::renderDataTable({
  metadata_tab=paste0(input$stat_database,'_metadata')
  sql=paste0("select * from ",metadata_tab," where species=",shQuote(input$stat_species)," and type=",shQuote(input$stat_IP))
  dat <- mysql_getData(sql)
  if(input$stat_database == 'cistrome'){
    dat=dat[,-ncol(dat)];dat=dat[,-ncol(dat)];
    
    dat$GSM=createLink(paste0("https://www.ncbi.nlm.nih.gov/geo/query/acc.cgi?acc=",dat$GSM),dat$GSM )
    dat$sampleID=createLink(paste0("http://epigenomegateway.wustl.edu/browser/?genome=",'hg38',
                                   "&datahub=http://dc2.cistrome.org/api/datahub/",
                                   dat$sampleID,"&gftk=refGene,full " 
    )
    ,dat$sampleID) 
    colnames(dat)=c('Sample ID','GSM ID','Biological Source 1','Biological Source 2','Biological Source 3','IP')
  }else{
    dat=dat[,-ncol(dat)];dat=dat[,-ncol(dat)];dat=dat[,-ncol(dat)]; 
    dat$sampleID=createLink(paste0("https://www.encodeproject.org/files/",dat$sampleID),dat$sampleID)
    dat$uniqID=createLink(paste0("https://www.encodeproject.org/experiments/",dat$uniqID),dat$uniqID)
    colnames(dat)=c('Sample ID','Unique ID','Biological Source 1','Biological Source 2','Biological Source 3',"Gender","Age",'IP')
  }
  
  
  dat
}
,rownames = FALSE, options = list( 
  pageLength = 10, 
  lengthMenu = list(c(10, 50, 100,-1), c('10', '50','100', 'All')) 
), 
class = 'cell-border stripe',
filter = 'top',
escape = FALSE
)

