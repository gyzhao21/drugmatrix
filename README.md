# DrugMatrix ApicalXOmics
Discovering genes associated with phenotypical changes (i.e., histopathologies, clinical pathologies, and organ weight changes) from DrugMatrix database

# Toxicogenomics and DrugMatrix Database
Toxicogenomics is the study of how genes and proteins respond to toxic substances, providing valuable insights into the molecular mechanisms behind adverse drug reactions and environmental toxicity. It explores how exposure to toxicants influences gene expression and the resulting biological effects (i.e., histopatholgy, clinical pathology, and organ weight change).

The DrugMatrix database is an integrated toxicogenomic repository that contains the short-term rat toxicity study results from approximately 637 pharmaceuticals and environmental chemicals performed. The complete DrugMatrix database (plain text link: https://cebs.niehs.nih.gov/cebs/paper/15670) is available on NIEHS CEBS site .

# Data set included in DrugMatrix
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

# Gene Expression Platforms
There are four gene expression platforms utilized in DrugMatrix: Codelink RU1, Affymetrix RG230-2, Sciome GENIE, and BioSpyder S1500+.
## CodeLink RU1
a high-density microarray platform developed by GE Healthcare. It was part of the CodeLink family of bioarray systems, which were designed for gene expression analysis. The RU1 arrays offer comprehensive coverage, allowing researchers to analyze the expression levels of thousands of genes simultaneously. RU1 array enhances the hybridization efficiency and signal-to-noise ratio. This results in highly reliable data, even when dealing with low-abundance transcripts.
## Affymetrix RG230-2
a part of the Affymetrix GeneChip family, designed specifically for rat model research. This microarray includes over 31,000 probe sets representing more than 30,000 genes and transcripts, offering comprehensive coverage of the rat genome. The RG230-2 array provides high specificity and sensitivity, making it a popular choice for detailed transcriptomic studies in rat models.
## Sciome GENIE
a commonly platform used in toxicogenomic, drug development, and environmental health research. It facilitates the integration and analysis of gene expression data, aiding researchers in understanding gene-environment interactions and identifying potential biomarkers. The Gene Expression Network Informatics Environment (GENIE) chips, developed by Sciome, are part of a suite of bioinformatics solutions designed specifically for gene expression analysis. These chips utilize expression signals from a small fraction (approximately 5-15%) of representative genes to accurately predict the expression of the remaining genes. Additionally, GENIE supports the analysis of differentially expressed genes (DEGs) and differentially enriched pathways (DEPs) by allowing users to define pairwise comparisons of sample groups using user-provided contrasts.
## BioSpyder S1500+
is a targeted sequencing technology designed to measure the expression of a specific set of 1,500 genes plus additional custom-selected genes initially. Unlike traditional microarrays, the S1500+ platform combines the benefits of both microarrays and RNA sequencing, offering a more cost-effective solution for large-scale gene expression studies. In National Toxicology Program of U.S. Department of Health and Human Services, the Tox21 Working Group developed and used a hybrid approach comprised of five sequential modules to identify the optimal set of genes that best represents biological diversity, addresses gene-gene co-expression relationships, and represents known pathways adequately. This hybrid approach accurately balances data-driven and knowledge-based evidence while allowing for performance assessment of the selected genes with respect to the gene set's ability to extrapolate whole transcriptome changes, both at the individual gene level and pathway level. The S1500+ platform has run ~3000 genes at NIEHS by expanding its flexibility and customization, allowing researchers to tailor the gene panel to their specific needs.

# Search Strategy
A combination of endpoints were accessed in each study including target organ histopathology, clinical chemistry and target organ toxicogenomics. This design allows for derivation of relationships between the different endpoint (e.g. identification of transcriptional biomarkers of pathology).

To this end we have created a Shiny web application on top of the DrugMatrix database that allows users to
- query a gene and identify its relationship to all diagnosed pathologies, clinical pathologies, and experimental animal organ weight changes due to the chemial compound treatment.
- query a specific pathology, clinical pathology, organ weight change to identify the most strongly associated genes.
- identify chemical treatments linked to apical endpoint finding grouped by treatments in toxicological profile.

Users can refine the search by selecting the criteria of duration of exposure, organ/tissue source of gene expression, gene probe, histopathology, etc., on a microarray platform (CodeLink and/or Affymetrix) and then click the SUBMIT button. Results can be visualized graphically and are downloadable in multiple formats.

