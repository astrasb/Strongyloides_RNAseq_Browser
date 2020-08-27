# Independent script that generates an R object containing gene family designations for genes from the following
# Strongyloides species: S. stercoralis, S. ratti, S. venezuelensis, S. papillosus
# Gene family designations are taken from the ensembl Compara database assembled by Hunt et al 2016

library(openxlsx)
library(tidyverse)

ensComp.geneIDs <- read.xlsx ("../Data/Hunt_Parasite_Ensembl_Compara.xlsx", 
                              sheet = 1) %>%
    as_tibble() %>%
    dplyr::select(-Family.members) %>%
    pivot_longer(cols = -Compara.family.id, values_to = "geneID") %>%
    dplyr::select(-name) %>%
    dplyr::filter(grepl("SSTP_|SRAE_|SPAL_|SVE_", geneID))

ensComp.geneIDs$geneID <- str_remove_all(ensComp.geneIDs$geneID, "\\.[0-9]$")
ensComp.geneIDs$geneID <- str_remove_all(ensComp.geneIDs$geneID, "[a-z]$")


# Make a list of genes
ensComp.familyIDs <- read.xlsx ("../Data/Hunt_Parasite_Ensembl_Compara.xlsx", 
                                sheet = 2,
                                cols = c(1,4:6)) %>%
    as_tibble() %>%
    dplyr::mutate(Family_Description = dplyr::coalesce(.$Description, 
                                                       .$`Top.product.(members.with.hit)`, 
                                                       .$`Interpro.top.hit.(members.with.hit)`)
    ) %>%
    dplyr::select(Compara.family.id, Family_Description)

ensComp <- left_join(ensComp.geneIDs, ensComp.familyIDs, by = "Compara.family.id") %>%
    dplyr::select(-Compara.family.id) %>%
    dplyr::rename(gs_name = Family_Description) %>%
    dplyr::relocate(gs_name, geneID)

rm(ensComp.geneIDs, ensComp.familyIDs)

# Save database of parasite Gene Sets for import into Shiny App. ----
save(ensComp,
     file = "../Strongyloides_RNAseq_Browser/Data/Str_parasiteGeneSets")