### Source

The *S. venezuelensis* data included in the *Strongyloides* RNA-seq Browser was originally published by [Hunt *et al* 2018](https://www.nature.com/articles/s41598-018-23514-z). Raw reads were downloaded from the European Nucleotide Archive - study accession number [PRJDB3457](https://www.ebi.ac.uk/ena/browser/view/PRJDB3457). 

### Samples

This dataset consists of 12 samples, representing 2 life stages with 3
biological replicates and 2 technical replicates for each life stage:

-   Free-living adult females (FLF)
-   Parasitic adult females (PF)

### A note regarding *S. venezuelensis* sample availability

Samples included in study PRJDB3457 were prepared using different libary construction methods (amplified vs non-amplified), sequencing run batches, and machines [(Hunt *et al* 2018)](https://www.nature.com/articles/s41598-018-23514-z). Dividing the experiments based on sequencing instrument produces two batches: 

  1. Batch FLF_PF: This set includes 2 life stages with 3 biological replicates and two technical replicates per life stage:  
    i) Free-living females (FLF)  
    ii) Parasitic adult females (PF)   
  2. Batch iL3_extended:   
    i) Egg: 1 biological replicate and two technical replicates  
    ii) 1st stage larvae (L1): 1 biological replicate and two technical replicates  
    iii) Infective third-stage larvae (iL3): 1 biological replicate and two technical replicates  
    iv) activated iL3s (iL3a) - 1 day: 1 biological replicate  
    v) activated iL3s (iL3a) - 5 day: 1 biological replicate  
    vi) iL3s removed from the lungs (iL3_lung): 1 biological replicate and two technical replicates  
    vii) Young free-living adult females (Young_FLF): 1 biological replicate and two technical replicates  
    viii) Free-living adult females (FLF): 1 biological replicate and two technical replicates    
Both batches contain data from free-living females, which thereotically permits batch correction and analysis of all samples. However, following limma-based batch correction there were still substantial differences between FLF samples from the two batches. We therefore take the conservative approach of treating these two batches separately. 

Thus, we have only included the batch containing multiple biological replicates for all life stages (Batch FLF_PF) in the *Strongyloides* RNA-seq Browser.