### Source

The *S. venezuelensis* data included in this repository was originally published by [Hunt *et al* 2018](https://www.nature.com/articles/s41598-018-23514-z). Raw reads were downloaded from the European Nucleotide Archive - study accession number [PRJDB3457](https://www.ebi.ac.uk/ena/browser/view/PRJDB3457). 

### Samples

This dataset consists of 12 samples, representing 2 life stages with 3
biological replicates and 2 technical replicates for each life stage:

-   Free-living adult females (FLF)
-   Parasitic adult females (PF)

### A note re: *S. venezuelensis* sample availability

Samples included in study PRJDB3457 were prepared using different libary construction methods (amplified vs non-amplified), sequencing run batches, and machines [(Hunt *et al* 2018)](https://www.nature.com/articles/s41598-018-23514-z). Dividing the experiments based on sequencing instrument produces two batches, both of which contain data from Free-living females and thereotically permit batch correction. However, following limma-based batch correction there were still substantial differences between FLF groups from the two batches. We therefore take the conservative approach of treating these two batches separately.   
Thus, we define three functional groups for processing and analysis:

  1. Group FLF_PF: This set includes 2 life stages with 3 biological replicates and two technical replicates per life stage:  
    i) Free-living females (FLF)  
    ii) Parasitic adult females (PF)   
  2. Group iL3_extended:   
    i) Egg: 1 biological replicate and two technical replicates  
    ii) 1st stage larvae (L1): 1 biological replicate and two technical replicates  
    iii) Infective third-stage larvae (iL3): 1 biological replicate and two technical replicates  
    iv) activated iL3s (iL3a) - 1 day: 1 biological replicate  
    v) activated iL3s (iL3a) - 5 day: 1 biological replicate  
    vi) iL3s removed from the lungs (iL3_lung): 1 biological replicate and two technical replicates  
    vii) Young free-living adult females (Young_FLF): 1 biological replicate and two technical replicates  
    viii) Free-living adult females (FLF): 1 biological replicate and two technical replicates    

Currently, only Group FLF_PF is included in the *Strongyloides* RNAseq Browser.