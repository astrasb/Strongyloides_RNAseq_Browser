# This script checks the qualitiy of our fastq files and performs an alignment to the Strongyloides stercoralis cDNA transcriptome reference with Kallisto.
# To run this 'shell script' you will need to open your terminal and navigate to the directory where this script resides on your computer.
# This should be the same directory where you fastq files and reference fasta file are found.
# Change permissions on your computer so that you can run a shell script by typing: 'chmod u+x readMapping.sh' (without the quotes) at the terminal prompt 
# Then type './readMapping.sh' (without the quotes) at the prompt.  
# This will begin the process of running each line of code in the shell script.

# first use fastqc to check the quality of our fastq files:
fastqc *.gz -t 8

# next, we want to build an index from our reference fasta file 
# I got my reference Strongyloides sterocralis transcripts from WormBase Parasite
kallisto index -i strongyloides_stercoralis.PRJEB528.WBPS14.mRNA_transcripts.index strongyloides_stercoralis.PRJEB528.WBPS14.mRNA_transcripts.fa

# now map reads to the indexed reference host transcriptome
# use as many 'threads' as your machine will allow in order to speed up the read mapping process.
# note that we're also including the '&>' at the end of each line
# this takes the information that would've been printed to our terminal, and outputs this in a log file that is saved in /data/course_data

# Free-Living Females
kallisto quant -i strongyloides_stercoralis.PRJEB528.WBPS14.mRNA_transcripts.index -o ERR146941 -t 14 ERR146941_1.fastq.gz ERR146941_2.fastq.gz&> ERR146941.log
kallisto quant -i strongyloides_stercoralis.PRJEB528.WBPS14.mRNA_transcripts.index -o ERR146942 -t 14 ERR146942_1.fastq.gz ERR146942_2.fastq.gz&> ERR146942.log
kallisto quant -i strongyloides_stercoralis.PRJEB528.WBPS14.mRNA_transcripts.index -o ERR146943 -t 14 ERR146943_1.fastq.gz ERR146943_2.fastq.gz&> ERR146943.log

# L3i+ (Activated iL3s)
kallisto quant -i strongyloides_stercoralis.PRJEB528.WBPS14.mRNA_transcripts.index -o ERR146944 -t 14 ERR146944_1.fastq.gz ERR146944_2.fastq.gz&> ERR146944.log
kallisto quant -i strongyloides_stercoralis.PRJEB528.WBPS14.mRNA_transcripts.index -o ERR146945 -t 14 ERR146945_1.fastq.gz ERR146945_2.fastq.gz&> ERR146945.log
kallisto quant -i strongyloides_stercoralis.PRJEB528.WBPS14.mRNA_transcripts.index -o ERR146946 -t 14 ERR146946_1.fastq.gz ERR146946_2.fastq.gz&> ERR146946.log

# L3i
kallisto quant -i strongyloides_stercoralis.PRJEB528.WBPS14.mRNA_transcripts.index -o ERR146947 -t 14 ERR146947_1.fastq.gz ERR146947_2.fastq.gz&> ERR146947.log
kallisto quant -i strongyloides_stercoralis.PRJEB528.WBPS14.mRNA_transcripts.index -o ERR146948 -t 14 ERR146948_1.fastq.gz ERR146948_2.fastq.gz&> ERR146948.log
kallisto quant -i strongyloides_stercoralis.PRJEB528.WBPS14.mRNA_transcripts.index -o ERR146949 -t 14 ERR146949_1.fastq.gz ERR146949_2.fastq.gz&> ERR146949.log

# Post-Free-Living L1s
kallisto quant -i strongyloides_stercoralis.PRJEB528.WBPS14.mRNA_transcripts.index -o ERR146950 -t 14 ERR146950_1.fastq.gz ERR146950_2.fastq.gz&> ERR146950.log
kallisto quant -i strongyloides_stercoralis.PRJEB528.WBPS14.mRNA_transcripts.index -o ERR146951 -t 14 ERR146951_1.fastq.gz ERR146951_2.fastq.gz&> ERR146951.log
kallisto quant -i strongyloides_stercoralis.PRJEB528.WBPS14.mRNA_transcripts.index -o ERR146952 -t 14 ERR146952_1.fastq.gz ERR146952_2.fastq.gz&> ERR146952.log

# Post-Parasitic L1s
kallisto quant -i strongyloides_stercoralis.PRJEB528.WBPS14.mRNA_transcripts.index -o ERR146953 -t 14 ERR146953_1.fastq.gz ERR146953_2.fastq.gz&> ERR146953.log
kallisto quant -i strongyloides_stercoralis.PRJEB528.WBPS14.mRNA_transcripts.index -o ERR146954 -t 14 ERR146954_1.fastq.gz ERR146954_2.fastq.gz&> ERR146954.log
kallisto quant -i strongyloides_stercoralis.PRJEB528.WBPS14.mRNA_transcripts.index -o ERR146955 -t 14 ERR146955_1.fastq.gz ERR146955_2.fastq.gz&> ERR146955.log

# Post-Parasitic L1s
kallisto quant -i strongyloides_stercoralis.PRJEB528.WBPS14.mRNA_transcripts.index -o ERR146956 -t 14 ERR146956_1.fastq.gz ERR146956_2.fastq.gz&> ERR146956.log
kallisto quant -i strongyloides_stercoralis.PRJEB528.WBPS14.mRNA_transcripts.index -o ERR146957 -t 14 ERR146957_1.fastq.gz ERR146957_2.fastq.gz&> ERR146957.log
kallisto quant -i strongyloides_stercoralis.PRJEB528.WBPS14.mRNA_transcripts.index -o ERR146958 -t 14 ERR146958_1.fastq.gz ERR146958_2.fastq.gz&> ERR146958.log

# Parasitic Females
kallisto quant -i strongyloides_stercoralis.PRJEB528.WBPS14.mRNA_transcripts.index -o ERR146959 -t 14 ERR146959_1.fastq.gz ERR146959_2.fastq.gz&> ERR146959.log
kallisto quant -i strongyloides_stercoralis.PRJEB528.WBPS14.mRNA_transcripts.index -o ERR146960 -t 14 ERR146960_1.fastq.gz ERR146960_2.fastq.gz&> ERR146960.log
kallisto quant -i strongyloides_stercoralis.PRJEB528.WBPS14.mRNA_transcripts.index -o ERR146961 -t 14 ERR146961_1.fastq.gz ERR146961_2.fastq.gz&> ERR146961.log



# summarize fastqc and kallisto mapping results in a single summary html using MultiQC
multiqc -d . 

echo "Finished"

