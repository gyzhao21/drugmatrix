# DrugMatrix ApicalXOmics
DrugMatrix ApicalXOmics is an application for iscovering genes associated with phenotypical changes (i.e., histopathologies, clinical pathologies, and organ weight changes) from DrugMatrix database. 

# Toxicogenomics and DrugMatrix Database
Toxicogenomics is the study of how genes and proteins respond to toxic substances, providing valuable insights into the molecular mechanisms behind adverse drug reactions and environmental toxicity. It explores how exposure to toxicants influences gene expression and the resulting biological effects (i.e., histopatholgy, clinical pathology, and organ weight change).

The DrugMatrix database is an integrated large-scale Rat toxicogenomic repository that contains the short-term rat toxicity study results from approximately 637 pharmaceuticals and environmental chemicals performed. The complete information about the database is available at CEBS site (https://cebs.niehs.nih.gov/cebs/paper/15670) at NIEHS.

The database has played a crucial role in toxicology research. It was initially developed to consolidate a wide range of toxicological and genomic data, enabling researchers to identify molecular signatures indicative of toxicity, diagnose pathological changes, and predict alterations in clinical pathology. The history and foundation of DrugMatrix have been well-documented, including in the book chapter provided as a reference.


> ## DrugMatrix Data Sets include:
>
     - ~700 Short-term toxicity studies (0.25 to 5 days) in male SD rats
     - ~637 compounds studied at multiple doses, time points and tissues
     - ~4,000 dose-time-tissue combinations (biological triplicates)
     - ~13,000 CodeLink RU1 Microarrays
     - ~5,000 Affymetrix RG230-2 Arrays
     - ~8,000 BioSpyder S1500+/GENIE Temp-SEQ
     - ~127,000 histopathology measurements
     - ~100,000 hematology and chemistry measurements
     - ~130 different in vitro assays (not yet in the tool)
     - ~900 chemicals with detailed literature curation


> ## Gene Expression Platforms
> There are four gene expression platforms utilized in DrugMatrix: Codelink RU1, Affymetrix RG230-2, Sciome GENIE, and BioSpyder S1500+.
> ### CodeLink RU1
> First generation high-density microarray platform developed by GE Healthcare that measures approximately 10000 genes and transcripts. Details on the microarray expresion platform can be found here: https://www.ncbi.nlm.nih.gov/geo/query/acc.cgi?acc=GPL5424  https://www.ncbi.nlm.nih.gov/geo/query/acc.cgi?acc=GPL5425   https://www.ncbi.nlm.nih.gov/geo/query/acc.cgi?acc=GPL5426  
> ### Affymetrix RG230-2
> A part of the Affymetrix GeneChip family, designed specifically for rat model research. The Affymetrix Rat Genome 230 2.0 Array includes over 31,000 probe sets representing more than 30,000 genes and transcripts, offering comprehensive coverage of the rat genome. The RG230-2 array provides high specificity and sensitivity, making it a popular choice for detailed transcriptomic studies in rat models. Details on the microarray expresion platform can be found here: https://www.ncbi.nlm.nih.gov/geo/query/acc.cgi?acc=GPL1355
> ### BioSpyder S1500+
> A targeted, sequencing-based platform technology developed by BioSpyder that is based on the TempO-Seq technology. The rat S1500+ platform used to assess the DrugMatrix samples measures the expression of a specific set of 1,500 genes plus additional custom-selected genes for total of approximately 2700 rat genes. The gene selection process for the rat s1500+ platform  used a hybrid approach comprised of five sequential modules to identify the optimal set of genes that best represents biological diversity, addresses gene-gene co-expression relationships, and represents known pathways adequately. 
> ### Sciome GeniE
> Extrapolated data derived from a model (referred to as GeniE; https://www.sciome.com/genie/) developed by Sciome LLC, which levels covariance in gene expression to infer whole genome expression from the BioSpyder S1500+ which measured ~ 2700 genes.

# Search Strategy
A combination of endpoints were accessed in each study including target organ histopathology, clinical chemistry and target organ toxicogenomics. This design allows for derivation of relationships between the different endpoint (e.g. identification of transcriptional biomarkers of pathology).

To this end we have created a Shiny web application on top of the DrugMatrix database that allows users to
- query a gene and identify its relationship to all diagnosed pathologies, clinical pathologies, and experimental animal organ weight changes due to the chemial compound treatment.
- query a specific pathology, clinical pathology, organ weight change to identify the most strongly associated genes.
- identify chemical treatments linked to apical endpoint finding grouped by treatments in toxicological profile.

Users can refine the search by selecting the criteria of duration of exposure, organ/tissue source of gene expression, gene probe, histopathology, etc., on a microarray platform (CodeLink and/or Affymetrix) and then click the SUBMIT button. Results can be visualized graphically and are downloadable in multiple formats.

