## for ENCODE human TF
mkdir -p ENCODE/human-TF
cd ENCODE/human-TF
mkdir hg19_peaks GRCh38_peaks
## you shoul download the metadata.tsv  from ENCODE website by yourself.
grep conservative metadata.tsv |grep -v bigBed |grep hg19 |cut -f 1,4,7-11,17,41 >hg19.conservative.bed.list.txt
grep conservative metadata.tsv |grep -v bigBed |grep GRCh38 |cut -f 1,4,7-11,17,41 >GRCh38.conservative.bed.list.txt
cut -f 9 hg19.conservative.bed.list.txt >hg19_peaks/address.txt
cut -f 9 GRCh38.conservative.bed.list.txt >GRCh38_peaks/address.txt
cd GRCh38_peaks
nohup  wget -c -i address.txt >/dev/null 2>&1 &


## for ENCODE human histone
mkdir -p ENCODE/human-histone
cd ENCODE/human-histone
mkdir hg19_peaks GRCh38_peaks
## you shoul download the metadata.tsv  from ENCODE website by yourself.
cat metadata.tsv |grep replicated |grep -w bed |grep GRCh38 |cut -f 1,4,7-11,17,41 >GRCh38.replicated.peaks.bed.list
cut -f 9 GRCh38.replicated.peaks.bed.list >GRCh38_peaks/address.txt
cd GRCh38_peaks
nohup  wget -c -i GRCh38_peaks/address.txt >/dev/null 2>&1 &

## for ENCODE mouse histone
mkdir -p ENCODE/mouse-histone
cd ENCODE/mouse-histone
mkdir mm10_peaks
## you shoul download the metadata.tsv  from ENCODE website by yourself.
cat metadata.tsv |grep replicated |grep -w bed |grep mm10 |cut -f 1,4,7-11,17,41 >mm10.replicated.peaks.bed.list
cut -f 9 mm10.replicated.peaks.bed.list >mm10_peaks/address.txt
cat mm10_peaks/address.txt |head
cd mm10_peaks
nohup  wget -c -i address.txt >/dev/null 2>&1 &

## for ENCODE mouse TF
mkdir -p ENCODE/mouse-TF
cd ENCODE/mouse-TF
mkdir mm10_peaks
## you shoul download the metadata.tsv  from ENCODE website by yourself.
cat metadata.tsv |grep conservative |grep -w bed |grep mm10 |cut -f 1,4,7-11,17,41 >mm10.conservative.peaks.bed.list
mkdir mm10_peaks
cut -f 9 mm10.conservative.peaks.bed.list >mm10_peaks/address.txt
cat mm10_peaks/address.txt |head
cd mm10_peaks
nohup  wget -c -i address.txt >/dev/null 2>&1 &

# the you should annotate those peaks file with BED format by HOMER.

