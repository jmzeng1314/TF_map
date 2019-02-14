library(RMySQL)
host <<- "127.0.0.1"
port <<- 3306
user <<- "tfmapperuser"
password <<-  'tfmapper_@Abc'
library(RMySQL)
con <- dbConnect(MySQL(), host=host, port=port, user=user, password=password)
sql="USE tfmapperdb;"
dbSendQuery(con, sql)
sql='show tables;'
dbGetQuery(con, sql)
options(stringsAsFactors = F)
#  dbDisconnect(con)
## firstly upload the gene information from GENCODE database 
if(F){
  a=read.table('files/gencode_v29_human_gene_info',sep = '\t')
  head(a)
  colnames(a)=c('symbol'   ,  'type' ,   'ensembl'   , 'chr' ,'start', 'end' )
  dbWriteTable(con, 'gencode_v29_human_gene_info', a, append=F,row.names=F)
  sql='show tables;'
  dbGetQuery(con, sql)
  
  a=read.table('files/gencode_vM20_mouse_gene_info',sep = '\t')
  head(a)
  colnames(a)=c('symbol'   ,  'type' ,   'ensembl'   , 'chr' ,'start', 'end' )
  dbWriteTable(con, 'gencode_vM20_mouse_gene_info', a, append=F,row.names=F)
  sql='show tables;'
  dbGetQuery(con, sql)
  
  
}

## then upload gene name information from org.eg... package(R)
if(F){
  library(org.Hs.eg.db)
  g2s=toTable(org.Hs.egSYMBOL)
  g2n=toTable(org.Hs.egGENENAME)
  s2n=merge(g2s,g2n,by='gene_id')
  dbWriteTable(con, 'human_genename',s2n, append=F,row.names=F)
  library(org.Mm.eg.db)
  g2s=toTable(org.Mm.egSYMBOL)
  g2n=toTable(org.Mm.egGENENAME)
  s2n=merge(g2s,g2n,by='gene_id')
  head(s2n)
  dbWriteTable(con, 'mouse_genename',s2n, append=F,row.names=F)
}

## Then upload the cistrome_metadata 
if(F){
  # 8 files
  tmp = lapply(list.files(path = 'files/metaFiles/',pattern = 'data_information.txt'),
               function(x){
    a=read.table(file.path('files/metaFiles/',x),sep='\t',fill = T,quote = "")
    a$species=strsplit(x,'_')[[1]][2]
    a$type=strsplit(x,'_')[[1]][1]
    return(a)
  })
  tmp =  do.call(rbind,tmp)[,c(1,2,4:7,9:10)]
  head(tmp)
  colnames(tmp)= strsplit('sampleID       GSM    bs1                 bs2      bs3       IP species    type','\\s+' )[[1]]
  head(tmp)
  dbWriteTable(con, 'cistrome_metadata', tmp , append=F,row.names=F)
  write.csv(tmp,'cistrome_metadata.csv')
  #  dbGetQuery(con,'select * from cistrome_metadata limit 10')
}


# Then upload the cistrome_GSM_metadata
if(F){
  # BiocManager::install('GEOmetadb')
  library(GEOmetadb)
  ## please download the GEOmetadb.sqlite by yourself, it's a extremely big file( > 7Gb)
  db='/Users/jmzeng/data/project/IDmap/GEOmetadb.sqlite'
  if(!file.exists(db)) getSQLiteFile()
  file.info(db)
  conGEO <- dbConnect(SQLite(),db)
  dbListTables(conGEO)
  dbListFields(conGEO,"gsm")
  dbGetQuery(conGEO,"select * from gsm where gsm='GSM1267210' ")
  gsm_list=tmp$GSM
  length(unique(gsm_list))
  this_sql=paste0("select * from gsm where gsm in ('",paste(gsm_list,collapse = "','"),"')")
  print(this_sql)
  #tmp=dbGetQuery(con,'select * from gsm ')
  tmp2=dbGetQuery(conGEO,this_sql)
  dbDisconnect(conGEO)
  ## some of the GSM id missed in the GEOmetadb.sqlite, I didn't check the reason.
  dbWriteTable(con, 'cistrome_GSM_metadata', tmp2 , append=F,row.names=F)
  write.csv(tmp2,'cistrome_GSM_metadata.csv')
}

# then upload the encode_metadata
if(F){
  setwd('files/metaFiles/')
  a=read.table('human_histone_GRCh38.replicated.peaks.bed.list.txt',sep = '\t',
               colClasses=c('character'),stringsAsFactors = F,quote = '') 
  # OLD :'cellline','celltype','tissue' 
  # NEW : 'bs1','bs2','bs3'
  colnames(a)=c('sampleID','uniqID','bs1','bs2','bs3','gender','age','IP','url')
  a$species='human';a$type='histone'
  head(a)
  # dbGetQuery(con,"DROP TABLE encode_metadata")
  dbWriteTable(con, 'encode_metadata', a, append=F,row.names=F)
  
  a=read.table('human_TF_GRCh38.conservative.bed.list.txt',sep = '\t',
               colClasses=c('character'),stringsAsFactors = F,quote = '') 
  colnames(a)=c('sampleID','uniqID','bs1','bs2','bs3','gender','age','IP','url')
  a$species='human';a$type='TF'
  dbWriteTable(con, 'encode_metadata', a, append=T,row.names=F)
  
  a=read.table('mouse_histone_mm10.replicated.peaks.bed.list.txt',sep = '\t',
               colClasses=c('character'),stringsAsFactors = F,quote = '') 
  colnames(a)=c('sampleID','uniqID','bs1','bs2','bs3','gender','age','IP','url')
  a$species='mouse';a$type='histone'
  dbWriteTable(con, 'encode_metadata', a, append=T,row.names=F)
  
  a=read.table('mouse_TF_mm10.conservative.peaks.bed.list.txt',sep = '\t',
               colClasses=c('character'),stringsAsFactors = F,quote = '') 
  colnames(a)=c('sampleID','uniqID','bs1','bs2','bs3','gender','age','IP','url')
  a$species='mouse';a$type='TF'
  dbWriteTable(con, 'encode_metadata', a, append=T,row.names=F)
  setwd('../../')
}


all_tables<-dbListTables(con)
all_tables
root_dir = '/Users/jmzeng/data/project/IDmap/db'
## totally 100Gb files
all_files=list.files(root_dir,pattern='*merge.txt',all.files=T,recursive=T)
all_files 
lapply(all_files, function(x){
  # x=all_files[1]
  this_file=file.path(root_dir,x)
  print(this_file)
  print(Sys.time())
  this_table=strsplit(gsub('\\/','_', x ),"\\.")[[1]][1] 
  this_table
  #if( this_table %in% all_tables){
  if(T){
    dbSendQuery(con,paste0(" drop table  if exists ",this_table))  
    sql=paste0(" CREATE TABLE   ",this_table, 
               " (sampleID VARCHAR(25) NOT NULL,  feature_type tinyint(1) NOT NULL DEFAULT 0,  start int(10) DEFAULT NULL,  end int(10) DEFAULT NULL,   score double DEFAULT NULL,  dis int(10) DEFAULT NULL,  symbol VARCHAR(50) DEFAULT NULL,   attri VARCHAR(100)  DEFAULT NULL ,p double DEFAULT NULL,q double DEFAULT NULL, KEY idx_symbol (symbol),KEY idx_position (start,end) ) ENGINE=InnoDB DEFAULT CHARSET=latin1  " 
    ) 
    #print(sql)
    dbSendQuery(con,sql)
    dat=read.table(this_file,stringsAsFactors = F,sep='\t',quote = "")
    colnames(dat)=c('sampleID','feature_type','start','end','score','dis','symbol','attri','p','q')
    dbWriteTable(con, this_table, dat, append=T,row.names=F)
    print(this_table)
  }
  
  
})













