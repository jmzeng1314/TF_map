output$results <- DT::renderDataTable( {
  tmp1=glob_values$results
  cat(as.character(Sys.time()),"Check the peaks table column!\n",file=stderr())
  cat(colnames(tmp1),'\n\n', file=stderr()); 
  
  # https://www.encodeproject.org/experiments/ENCSR541AOQ/
  # https://www.encodeproject.org/files/ENCFF132KNA/
  # https://www.ncbi.nlm.nih.gov/geo/query/acc.cgi?acc=GSM530178
  
  if(! is.null(tmp1)){
    if(glob_values$database == 'cistrome'){
      ## if choose the cistrome database:
      
      tmp1$GSM=createLink(paste0("https://www.ncbi.nlm.nih.gov/geo/query/acc.cgi?acc=",tmp1$GSM),tmp1$GSM)
      ##   http://epigenomegateway.wustl.edu/browser/?genome=hg38&
      ##  datahub=http://dc2.cistrome.org/api/datahub/9216&gftk=refGene,full&coordinate=chr6:26099558-26299988
      genome=ifelse(glob_values$species=='human','hg38','mm10')
      
      WashU_link=createLink(paste0("http://epigenomegateway.wustl.edu/browser/?genome=",genome,
                                   "&datahub=http://dc2.cistrome.org/api/datahub/",
                                   tmp1$sampleID,"&gftk=refGene,full&coordinate=",
                                   tmp1$chrom,":",tmp1$start-1000,"-",tmp1$end+1000
      )
      
      ,'WashU')
      UCSC_link=createLink(paste0( "http://dc2.cistrome.org/api/hgtext/", tmp1$sampleID,
                                   "/?db=",genome,
                                   "&position=",  tmp1$chrom,":",tmp1$start-1000,"-",tmp1$end+1000
      )
      
      ,'UCSC')
      tf = tmp1$IP
      knockTF_link=createLink(paste0("http://www.licpathway.net/KnockTF/search/search_tf_result.php?tf_name=",tf)
                              
                              ,"knockTF")
      
      tmp1$Visualize=paste(WashU_link,UCSC_link)
      tmp1$sequence=createLink(paste0(
        "http://genome.ucsc.edu/cgi-bin/das/",genome,"/dna?segment=",
        paste0(tmp1$chrom,':',tmp1$start,',',tmp1$end)
      )
      , paste0(tmp1$chrom,':',tmp1$start,',',tmp1$end) )
      
      #tmp1<-tmp1[, c('GSM','IP','Visualize','sequence','dis','score','attri','bs1','bs2','bs3')]
      tmp1<-tmp1[, c('GSM','sampleID','IP','Visualize','sequence','dis','score','p','q','attri','title','source_name_ch1')]
      tmp1$p=round(tmp1$p,2)
      tmp1$q=round(tmp1$q,2)
      tmp1$score=round(tmp1$score,2)
      names(tmp1)=c('GSM','sampleID','IP','Visualization','Sequence','Distance','Score','-log10(p value)','-log10(q value)','Attribute','Title','Source Name')
      tmp1$Attribute = as.factor(as.character(sapply(tmp1$Attribute,function(x) strsplit(x,'\\(')[[1]][1])))
      tmp1$knockTF = knockTF_link
      return(tmp1)
      
    }else{
      ## if choose the ENCODE database:
      #return(NULL)
      genome=ifelse(glob_values$species=='human','hg38','mm10')
      
      tmp1$Visualize=createLink(paste0( "http://genome.ucsc.edu/cgi-bin/hgTracks?hubClear=https://www.encodeproject.org/", 
                                        tmp1$uniqID,
                                        "/@@hub/hub.txt&db=",genome,
                                        "&position=",  tmp1$chrom,":",tmp1$start-1000,"-",tmp1$end+1000
      )
      ,'go to UCSC')
      tmp1$sampleID=createLink(paste0("https://www.encodeproject.org/files/",tmp1$sampleID),tmp1$sampleID)
      tmp1$uniqID=createLink(paste0("https://www.encodeproject.org/experiments/",tmp1$uniqID),tmp1$uniqID)
      
      tmp1$sequence=createLink(paste0(
        "http://genome.ucsc.edu/cgi-bin/das/",genome,"/dna?segment=",
        paste0(tmp1$chrom,':',tmp1$start,',',tmp1$end)
      )
      , paste0(tmp1$chrom,':',tmp1$start,',',tmp1$end) )
      tmp1<-tmp1[, c('uniqID','sampleID','IP','Visualize','sequence','dis','score','p','q','attri','bs1','bs2','bs3')]
      names(tmp1)=c('UniqID','SampleID','IP','Visualize','Sequence','Distance','Score','-log10(p value)','-log10(q value)','Attribute','Bs1','Bs2','Bs3')
      ## to change the intron (NM_006755, intron 4 of 7)', which just have 8 category 
      tmp1$Attribute = as.factor(as.character(sapply(tmp1$Attribute,function(x) strsplit(x,'\\(')[[1]][1])))
      
      return(tmp1)
      
    }
    
    
  }else{
    return(NULL)
  }
  
  
  
  
}
, rownames= FALSE,extensions = c('Scroller'), options = list(  
  pageLength = 50, 
  lengthMenu = list(c(10, 50, 100,-1), c('10', '50','100', 'All')),
  columnDefs = list(list(className = 'dt-center', targets = 0 :8)),
  scrollX = TRUE,
  fixedHeader = TRUE,
  fixedColumns = TRUE ,
  deferRender = TRUE 
),
filter = 'top',
escape = FALSE
)