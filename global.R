library(shiny)
library(shinydashboard)
library(ggplot2)
library(pool)
library(dplyr, warn.conflicts = FALSE)
library(data.table)
require("RPostgres")
#require("RPostgreSQL")
library(forcats)
library(config)  # this is the new package install for config.yml
library(DT)
library(tidyverse)
library(tidyr)
library(shinyWidgets)
library(readxl)
library(survPen)
library(enrichR)

# needs to set R_CONFIG_ACTIVE in .Renviron
dw <- config::get("drugmatrix")

pobj <- pool::dbPool(
  RPostgres::Postgres(),
  host = dw$server,
  user    = dw$uid,
  password    = dw$pwd,
  port   = dw$port,
  dbname = dw$database
)

# # read data
# pool <- dbPool(
#   drv = RPostgres::Postgres(),
# #  drv = RPostgreSQL::PostgreSQL(),
#   dbname = "drugmatrix",
#   host = "ehsntpld03",
#   port = 5432,
#   user = "ntp_group",
#   password = "ntp_group"
# )    

onStop(function() {
  poolClose(pobj)
})

datQuery <- paste0("SELECT * FROM bsb_chip_histopath_duration;")

dat <- dbGetQuery(pobj, datQuery)

gQuery <- paste0("Select DISTINCT concat(symbol, ' | ', gene_name) gene_name from all_gene_fc;") 
df_pgQuery <- dbGetQuery(pobj, gQuery)
gene <- df_pgQuery$gene_name

## chemEnrichQuery <- paste0("SELECT * FROM bsb_chemical_enricher_menu;")
chemEnrichQuery <- paste0("SELECT compound_name, time, dose, chip_name, 
                                     CASE
                                        WHEN tissue =5 THEN 'HEART'::text
                                        WHEN tissue =8  THEN 'LIVER'::text
                                        WHEN tissue =7  THEN 'KIDNEY'::text
                                        WHEN tissue =11  THEN 'SPLEEN'::text
                                        WHEN tissue =1  THEN 'BONE MARROW'::text
                                        WHEN tissue =16  THEN 'THIGH MUSCLE'::text
                                        WHEN tissue =3  THEN 'HEPATOCYTE'::text
                                        WHEN tissue =6  THEN 'INTESTINE'::text
                                        WHEN tissue =2 THEN 'BRAIN'::text
                                        ELSE NULL::text
                                     END AS tissue_name
                             FROM bsb_chemical_enricher_menu_vu; ")
chem_enrich_dat <- dbGetQuery(pobj, chemEnrichQuery)

###################################  cpdatQuery:   For Genes to Clinical Pathology #####################################################################
cpdatQuery <- paste0("SELECT assay_name, category, time, tissue, tissue_num, chip_name, type FROM bsb_clinical_assay_list;")
cpdat <- dbGetQuery(pobj, cpdatQuery)


typelist <- c("BLOOD_CHEM", "HEMATOLOGY")
tissuelist2 <- c("1", "2", "5", "6", "8", "7", "11", "16") # This is for Clinical Pathology to Genes tab

########################################  citationQuery:   For Scientific Citations ####################################################################
# citationQuery <- paste0("SELECT author, topic, year, journal, page, pubmed, doctype FROM bsb_paxgene_citation;")
citationQuery <- paste0("SELECT author, topic, year, journal, page, concat('https://pubmed.ncbi.nlm.nih.gov/', pubmed) pubmed, doctype FROM bsb_paxgene_citation order by doctype;")
df_citation <- dbGetQuery(pobj, citationQuery)


typelist <- c("BLOOD_CHEM", "HEMATOLOGY")
tissuelist2 <- c("1", "2", "5", "6", "8", "7", "11", "16") # This is for Clinical Pathology to Genes tab

########################################################  gwdatQuery: For Genes to Organ Weight ########################################################
gwdatQuery <- paste0("SELECT chip_name, organ, status, duration, signature FROM bsb_gene_weight_change_list;")
gwdat <- dbGetQuery(pobj, gwdatQuery)

tox_profileQuery<-paste0("SELECT chemical, casrn, dtxsid, concat('https://comptox.epa.gov/dashboard/chemical/details/', dtxsid) dtxlink,  duration, dose, dose_unit, type, vehicle, route, assayname, tissue, measurement, unit, lower_bound_of_normal, upper_bound_of_normal, call
                          FROM   bsb_toxicological_profile;")
# initially, I used bsb_toxicological_profile;

dfprofile <- dbGetQuery(pobj, tox_profileQuery)


################################################# Global Functions to swap tissue number and tissue name ###############################################

timeinput <- function(a) { 
    # b=paste0('in(', paste0(a, collapse=","),")")
      b=paste0('(', paste0(a, collapse=","),")")
      return(b)
 }

########### The following tissue_num2name() is to convert tissue numbers to tissue names 
tissue_num2name <- function(num) {
  name = c()
  for (i in 1:length(num)) {
    if (num[i] == 8) {
      name[i] = 'LIVER'
    } else if (num[i]== 7) {
      name[i] = 'KIDNEY'
    } else if (num[i] == 6) {
      name[i] = 'INTESTINE'
    } else if (num[i] == 5) {
      name[i] = 'HEART'
    } else if (num[i] == 1) {
      name[i] = 'BONE MARROW'
    } else if (num[i] == 2) {
      name[i] = 'BRAIN'
    } else if (num[i] == 11) {
      name[i] = 'SPLEEN'
    } else {
      name[i] = 'THIGH MUSCLE'
    }
  }
  return (name)
} 

########### The following tissue_name2num() is to convert tissue names to tissue numbers
tissue_name2num <- function(name) {
  num = c()
  # num = list()
  for(i in 1:length(name)) {
    if (name[i] == 'LIVER') {
      num[i] = 8
    } else if (name[i] == 'KIDNEY') {
      num[i] = 7
    } else if (name[i] == 'INTESTINE') {
      num[i] = 6
    } else if (name[i] == 'HEART') {
      num[i] = 5
    } else if (name[i] == 'BONE MARROW') {
      num[i] = 1
    } else if (name[i] == 'BRAIN') {
      num[i] = 2
    } else if (name[i] == 'SPLEEN') {
      num[i] = 11
    } else {
      num[i] = 16  # 'THIGH MUSCLE' is 16
    }
  }
  return(num)
}


################################################################################################################################################################

assaylistQuery <-paste0("SELECT assay_name, tablename, stat_tablename, type from bsb_clin_pathology_list;")  
df_assaylistQuery <-dbGetQuery(pobj, assaylistQuery)

assaylist<- df_assaylistQuery$assay_name       ### Retrieve assay_name from table bsb_clin_pathology_list and store as a vector
tablelist<- df_assaylistQuery$stat_tablename   ### Retrieve stat_tablename from table bsb_clin_pathology_list and store as a vector
detailtablelist <- df_assaylistQuery$tablename ### Retrieve tablename with detail information including experiment bsb_clin_pathology_list and store as a vector
typelist <- c("BLOOD_CHEM", "HEMATOLOGY")

# doselistQuery <- paste0("SELECT distinct dose FROM all_transcript_vu_fc order by 1;")
# df_doselist <-dbGetQuery(pool, doselistQuery)
# doselist <- df_doselist$dose

doselist <- c(0.0007, 0.001, 0.0033, 0.005, 0.01, 0.02, 0.03, 0.033, 0.0375, 0.04, 0.043, 0.05, 0.06, 0.065, 0.07, 0.08, 0.1, 0.125, 0.15, 0.2, 0.24, 0.25, 0.26, 0.3, 0.44, 0.5, 0.56, 0.6, 0.625, 0.65, 0.72, 0.77, 0.9, 1, 1.1, 1.17, 1.25, 1.3, 1.31, 1.5, 1.6, 1.67, 1.7, 1.75, 2, 2.3, 2.5, 2.7, 2.75, 2.8, 3, 3.25, 3.5, 3.75, 4, 4.2, 4.5, 4.6, 5, 5.4, 6, 6.5, 6.6, 7, 7.5, 8, 8.5, 8.75, 9, 9.6, 10, 10.9, 11, 11.3, 12, 12.5, 13, 14, 14.6, 15, 16, 16.5, 17, 17.5, 18, 19, 20, 20.4, 21, 21.95, 22, 23, 24, 25, 26, 26.7, 27, 28, 29, 30, 31, 32, 33, 34, 35, 36, 37, 37.04, 37.5, 39, 39.6, 40, 41, 43, 43.9, 44, 45, 46, 47, 48, 50, 51, 52, 54, 56, 57, 58, 59, 60, 61.7, 62, 62.25, 63, 64, 66, 67, 68, 69, 70, 71, 72, 73, 74, 75, 76, 77, 78, 79, 80, 81, 82, 83, 84, 85, 88, 89, 90, 92, 93, 94, 95, 99, 100, 104, 108, 110, 111, 112, 113, 114, 115, 116, 119, 120, 121, 122, 125, 126, 130, 132, 134, 138, 140, 143, 144, 147, 148, 150, 155, 156, 160, 161, 162, 164, 165, 166.67, 167, 175, 178, 184, 185, 188, 190, 193.7, 195, 196, 197, 200, 201, 206, 207, 210, 212, 215, 220, 223, 224, 225, 227, 230, 235, 239, 240, 243, 250, 263, 266, 267, 269, 275, 280, 296, 300, 310, 312, 313, 320, 325, 330, 333, 334, 338, 350, 352, 360, 364, 365, 368, 375, 380, 386, 387.5, 390, 394, 400, 404, 414, 415, 420, 430, 435, 439, 444, 448, 450, 453, 456, 470, 474, 476, 483, 486, 487, 488, 490, 500, 501, 520, 526, 535, 550, 560, 570, 572, 580, 588, 600, 610, 615, 617, 619, 620, 625, 633, 636, 645, 650, 656.6, 657, 667, 671, 688, 700, 710, 714, 735, 739, 740, 750, 755, 763, 770, 775, 800, 833, 850, 852, 877, 895, 900, 920, 930, 972, 980, 998, 1000, 1005, 1022, 1024, 1093, 1100, 1120, 1125, 1170, 1175, 1200, 1230, 1255, 1300, 1320, 1340, 1400, 1402, 1470, 1473, 1480, 1481, 1500, 1503, 1540, 1550, 1560, 1600, 1636, 1650, 1653, 1667, 1695, 1700, 1750, 1800, 1868, 1915, 1950, 2000, 2100, 2222, 2337, 2500, 2550, 2600, 2625, 2629, 2900, 3000, 3168, 3178, 3525, 3855, 4000, 5000, 6000, 6584, 6666, 7000, 11000, 13500, 14800, 23000, 65000, 100000, 180000) 
#################################################  End of Defining Variables for Clinical Pathology to Genes ###################################################


################################################# The following compound list is for the Gene Expression#### ###################################################

# compoundQuery <- paste0("SELECT distinct compound_name  FROM All_transcript_vu_fc order by 1;")
# df_compoundQuery <-dbGetQuery(pool, compoundQuery)
# compoundlist <- df_compoundQuery$compound_name

# compoundlist <- c('1,1-DICHLOROETHENE', '1,2,3-TRICHLOROPROPANE', '1,4-DICHLOROBENZENE', '17-METHYLTESTOSTERONE', '1-NAPHTHYL ISOTHIOCYANATE', '2,3,7,8-TETRACHLORODIBENZO-P-DIOXIN', '2,4-DIAMINOPHENOL', '2-ACETYLAMINOFLUORENE', '2-AMINO-4-NITROPHENOL', '2\'-BETA-FLUORO-2\',3\'-DEOXYADENOSINE', '2-NITROANISOLE', '3,3\',4\',5-TETRACHLOROSALICYLANILIDE', '3,3\',5-TRIIODO-L-THYRONINE', '3-ACETAMIDOPHENOL', '3-CHLOROANILINE', '3-METHYLCHOLANTHRENE', '4,4\'-DIETHYLAMINOETHOXYHEXESTROL', '4,4\'-METHYLENEDIANILINE', '4-CHLORO-2-NITROANILINE', '4-CHLOROANILINE', '4-METHYLPYRAZOLE', '4-NITROBENZOIC ACID', '4-NITROTOLUENE', '4-NONYLPHENOL', '4-OCTYLPHENOL', '5-FLUORO-2\'-DEOXYURIDINE', '6-MERCAPTOPURINE', '6-METHOXY-2-NAPHTHYLACETIC ACID', 'ABAMECTIN', 'ACARBOSE', 'ACECLOFENAC', 'ACEMETACIN', 'ACETAMINOPHEN', 'ACETAZOLAMIDE', 'ACETONE', 'ACONITINE', 'ACROLEIN', 'ACYCLOVIR', 'AFLATOXIN B1', 'ALBENDAZOLE', 'ALDOSTERONE', 'ALENDRONIC ACID', 'ALFACALCIDOL', 'ALLOPURINOL', 'ALLYL ALCOHOL', 'ALLYLAMINE', 'ALPHA-NAPHTHOFLAVONE', 'ALPRAZOLAM', 'ALTRETAMINE', 'AMANTADINE', 'AMIKACIN', 'AMILORIDE', 'AMINEPTINE', 'AMINOCAPROIC ACID', 'AMINOGLUTETHIMIDE', 'AMINOSALICYLIC ACID', 'AMIODARONE', 'AMITRAZ', 'AMITRIPTYLINE', 'AMLODIPINE', 'AMOXAPINE', 'AMOXICILLIN', 'AMPICILLIN', 'AMPIROXICAM', 'AMPRENAVIR', 'ANASTROZOLE', 'ANGIOTENSIN II HUMAN', 'ANISINDIONE', 'ANTIMYCIN A', 'ANTIPYRINE', 'ARTEMETHER', 'ARTEMISININ', 'ASCORBIC ACID', 'ASPIRIN', 'ATENOLOL', 'ATORVASTATIN', 'ATROPINE', 'AURANOFIN', 'AZARIBINE', 'AZASETRON', 'AZATHIOPRINE', 'AZAURIDINE', 'AZITHROMYCIN', 'AZLOCILLIN', 'AZTREONAM', 'BACITRACIN', 'BACLOFEN', 'BALSALAZIDE', 'BENAZEPRIL', 'BENOXAPROFEN', 'BENZETHONIUM CHLORIDE', 'BENZOCAINE', 'BENZOIC ACID', 'BENZOTHIAZYL DISULFIDE', 'BENZYL ACETATE', 'BETA-ESTRADIOL', 'BETA-ESTRADIOL 3-BENZOATE', 'BETAHISTINE', 'BETAMETHASONE', 'BETA-NAPHTHOFLAVONE', 'BEZAFIBRATE', 'BIS(2-ETHYLHEXYL)PHTHALATE', 'BISACODYL', 'BISPHENOL A', 'BITHIONOL', 'BLEOMYCIN A2', 'BROMFENAC', 'BROMHEXINE', 'BROMISOVALUM', 'BROMOBENZENE', 'BROMODICHLOROMETHANE', 'BUFLOMEDIL', 'BUPROPION', 'BUSPIRONE', 'BUSULFAN', 'BUTENAFINE', 'BW-723C86', 'CADMIUM ACETATE', 'CADMIUM CHLORIDE', 'CAFFEINE', 'CALCITRIOL', 'CAMPTOTHECIN', 'CANDESARTAN', 'CAPSAICIN', 'CAPTOPRIL', 'CARBACHOL', 'CARBAMAZEPINE', 'CARBIMAZOLE', 'CARBON TETRACHLORIDE', 'CARBOPLATIN', 'CARMOFUR', 'CARMUSTINE', 'CARVEDILOL', 'CATECHOL', 'CEFOTAXIME', 'CELECOXIB', 'CEPHALEXIN', 'CERAMIDE C2', 'CERIVASTATIN', 'CETRAXATE', 'CETYLPYRIDINIUM BROMIDE', 'CHLORAMBUCIL', 'CHLORAMPHENICOL', 'CHLORDIAZEPOXIDE', 'CHLORMADINONE ACETATE', 'CHLOROBENZENE', 'CHLOROFORM', 'CHLOROQUINE', 'CHLOROXYLENOL', 'CHLORPROMAZINE', 'CHLORPROPAMIDE', 'CHLORPYRIFOS', 'CHLORTETRACYCLINE', 'CHLORZOXAZONE', 'CHOLECALCIFEROL', 'CHOLIC ACID', 'CHOLINE CHLORIDE', 'CILOSTAZOL', 'CIMETIDINE', 'CINNARIZINE', 'CIPROFLOXACIN', 'CISAPRIDE', 'CISPLATIN', 'CITALOPRAM', 'CITRIC ACID', 'CLARITHROMYCIN', 'CLEMASTINE', 'CLINDAMYCIN', 'CLOBETASOL PROPIONATE', 'CLOFIBRATE', 'CLOFIBRIC ACID', 'CLOMIPHENE', 'CLOMIPRAMINE', 'CLONAZEPAM', 'CLONIDINE', 'CLOPIDOGREL', 'CLOSANTEL', 'CLOTRIMAZOLE', 'CLOXACILLIN', 'CLOZAPINE', 'COBALT(II) ACETYLACETONATE', 'COBALT (II) CHLORIDE', 'COLCHICINE', 'COLISTIN', 'CORTISONE', 'COUMARIN', 'CROMOLYN', 'CROTAMITON', 'CYCLANDELATE', 'CYCLOHEXIMIDE', 'CYCLOPHOSPHAMIDE', 'CYCLOPROPANE CARBOXYLIC ACID', 'CYCLOSPORIN A', 'CYPROHEPTADINE', 'CYPROTERONE ACETATE', 'CYTARABINE', 'CYTOCHALASIN B', 'DACTINOMYCIN', 'DANAZOL', 'DAUNORUBICIN', 'DEFERIPRONE', 'DESLORATADINE', 'DEXAMETHASONE', 'DEXCHLORPHENIRAMINE', 'DEXFENFLURAMINE', 'DEXIBUPROFEN', 'D-GALACTOSAMINE', 'DIAZEPAM', 'DIBROMOCHLOROMETHANE', 'DICHLORVOS', 'DICLOFENAC', 'DICUMAROL', 'DICYCLOMINE', 'DIDANOSINE', 'DIDEOXYCYTIDINE', 'DIETHANOLAMINE', 'DIETHYLSTILBESTROL', 'DIFLUNISAL', 'DIGITONIN', 'DIGITOXIN', 'DIGOXIN', 'DIISOPROPYL FLUOROPHOSPHATE', 'DIMENHYDRINATE', 'DIMETHYL METHYLPHOSPHONATE', 'DIPHENHYDRAMINE', 'DIPHENIDOL', 'DIPYRIDAMOLE', 'DIPYRONE', 'DISULFIRAM', 'D-LIMONENE', 'DOBUTAMINE', 'DOXAPRAM', 'DOXAZOSIN', 'DOXEPIN', 'DOXIFLURIDINE', 'DOXOFYLLINE', 'DOXORUBICIN', 'DOXYCYCLINE', 'DROPERIDOL', 'D-TUBOCURARINE CHLORIDE', 'DYPHYLLINE', 'EBASTINE', 'ECONAZOLE', 'EMETINE', 'ENALAPRIL', 'ENOXACIN', 'ENROFLOXACIN', 'EPALRESTAT', 'EPERISONE', 'EPINEPHRINE', 'EPIRUBICIN', 'ERGOCALCIFEROL', 'ERLOTINIB', 'ERYTHROMYCIN', 'ESMOLOL', 'ESTRIOL', 'ETHACRYNIC ACID', 'ETHAMBUTOL', 'ETHANOL', 'ETHINYLESTRADIOL', 'ETHISTERONE', 'ETHOSUXIMIDE', 'ETHYLENE GLYCOL', 'ETHYLESTRENOL', 'ETIDRONATE', 'ETODOLAC', 'ETOPOSIDE', 'EUCALYPTOL', 'FAMCICLOVIR', 'FAMOTIDINE', 'FENBENDAZOLE', 'FENOFIBRATE', 'FENOPROFEN', 'FERROCENE', 'FERULIC ACID', 'FINASTERIDE', 'FLAVOXATE', 'FLUBENDAZOLE', 'FLUCONAZOLE', 'FLUDROCORTISONE ACETATE', 'FLUNARIZINE', 'FLUOCINOLONE ACETONIDE', 'FLUOROURACIL', 'FLUOXETINE', 'FLUPHENAZINE', 'FLURBIPROFEN', 'FLUTAMIDE', 'FLUVASTATIN', 'FOSCARNET', 'FURAN', 'FUROSEMIDE', 'GABAPENTIN', 'GADOPENTETATE DIMEGLUMINE', 'GALANTAMINE', 'GALLAMINE TRIETHIODIDE', 'GATIFLOXACIN', 'GEFITINIB', 'GEMCITABINE', 'GEMFIBROZIL', 'GENISTEIN', 'GENTAMICIN', 'GENTIAN VIOLET', 'GERANIOL', 'GLICLAZIDE', 'GLIMEPIRIDE', 'GLIPIZIDE', 'GLYBURIDE', 'GLYCIDOL', 'GLYCINE', 'GOLD SODIUM THIOMALATE', 'GRANISETRON', 'GRISEOFULVIN', 'GUANETHIDINE', 'HALOPERIDOL', 'HARRINGTONINE', 'HEXACHLORO-1,3-BUTADIENE', 'HEXACHLOROETHANE', 'HEXACHLOROPHENE', 'HYDRALAZINE', 'HYDRAZINE', 'HYDROCHLOROTHIAZIDE', 'HYDROCORTISONE', 'HYDROQUINONE', 'HYDROXYUREA', 'IBUFENAC', 'IBUPROFEN', 'IDARUBICIN', 'IDEBENONE', 'IFOSFAMIDE', 'IMATINIB', 'INDINAVIR', 'INDOMETHACIN', 'INSULIN', 'INTERLEUKIN 1 BETA, RAT', 'INTERLEUKIN 6, RAT', 'IPRIFLAVONE', 'IPRONIAZID', 'IRINOTECAN', 'ISOEUGENOL', 'ISONIAZID', 'ISOPRENALINE', 'ISOTRETINOIN', 'ITRACONAZOLE', 'IVERMECTIN', 'KANAMYCIN', 'KETAMINE', 'KETOCONAZOLE', 'KETOPROFEN', 'KETOROLAC', 'LABETALOL', 'LACIDIPINE', 'LAMIVUDINE', 'LAMOTRIGINE', 'LANSOPRAZOLE', 'L-BUTHIONINE SULFOXIMINE', 'LEAD (II) ACETATE', 'LEAD(IV) ACETATE', 'LEFLUNOMIDE', 'LETROZOLE', 'LEUCOVORIN', 'LEVAMISOLE', 'LEVODOPA', 'LEVOFLOXACIN', 'LEVOSULPIRIDE', 'LIDOCAINE', 'LINCOMYCIN', 'LIPOPOLYSACCHARIDE E. COLI O55:B5', 'LISINOPRIL', 'LITHOCHOLIC ACID', 'LOMEFLOXACIN', 'LOMUSTINE', 'LOPERAMIDE', 'LORATADINE', 'LORAZEPAM', 'LOSARTAN', 'LOVASTATIN', 'LOXOPROFEN', 'L-PHENYLEPHRINE', 'MANGANESE (II) CHLORIDE', 'MANNITOL', 'MAPROTILINE', 'MARIMASTAT', 'MEBENDAZOLE', 'MEFENAMIC ACID', 'MEGESTROL ACETATE', 'MELATONIN', 'MELOXICAM', 'MELPHALAN', 'MERCURIC CHLORIDE', 'MEROPENEM', 'MESNA', 'MESTRANOL', 'METHAPYRILENE', 'METHIMAZOLE', 'METHOCARBAMOL', 'METHOTREXATE', 'METHYLDOPA', 'METHYL METHANESULFONATE', 'METHYLPARABEN', 'METHYL SALICYLATE', 'METOCLOPRAMIDE', 'METOPROLOL', 'METRONIDAZOLE', 'MEVASTATIN', 'MEXILETINE', 'MIANSERIN', 'MICONAZOLE', 'MICROCYSTIN-LR', 'MIFEPRISTONE', 'MINOXIDIL', 'MITOMYCIN C', 'MITOTANE', 'MITOXANTRONE', 'MLN-518', 'MOCLOBEMIDE', 'MODAFINIL', 'MONOCROTALINE', 'MOSAPRIDE', 'MOXONIDINE', 'MYCOPHENOLATE MOFETIL', 'MYRTECAINE', 'NABUMETONE', 'NADOLOL', 'NAFENOPIN', 'NALOXONE', 'NAPROXEN', 'NATEGLINIDE', 'NEOMYCIN', 'NEOSTIGMINE BROMIDE', 'NETILMICIN', 'NEVIRAPINE', 'NIACIN', 'NIACINAMIDE', 'NICOTINE', 'NIFEDIPINE', 'NIMESULIDE', 'NIMETAZEPAM', 'NIMODIPINE', 'NISOLDIPINE', 'NITRAZEPAM', 'NITRENDIPINE', 'NITROBENZENE', 'NITROFURANTOIN', 'NIZATIDINE', 'N,N-DIMETHYLFORMAMIDE', 'N,N\'-DIPHENYL-P-PHENYLENEDIAMINE', 'N-NITROSODIETHYLAMINE', 'N-NITROSODIMETHYLAMINE', 'NOREPINEPHRINE', 'NORETHINDRONE', 'NORETHINDRONE ACETATE', 'NORFLOXACIN', 'NORTRIPTYLINE', 'NOSCAPINE', 'NYSTATIN', 'OFLOXACIN', 'OLANZAPINE', 'OMEPRAZOLE', 'ONDANSETRON', 'OUABAIN', 'OXALIPLATIN', 'OXAPROZIN', 'OXCARBAZEPINE', 'OXFENDAZOLE', 'OXICONAZOLE', 'OXYBUTYNIN', 'OXYMETAZOLINE', 'OXYMETHOLONE', 'OXYQUINOLINE', 'OXYTETRACYCLINE', 'PANTOPRAZOLE', 'PAPAVERINE', 'PARAQUAT DICHLORIDE', 'PAROXETINE', 'PEMOLINE', 'PENCICLOVIR', 'PENICILLAMINE', 'PENICILLIN G', 'PENTAMIDINE', 'PENTOBARBITAL', 'PENTOXIFYLLINE', 'PERGOLIDE', 'PERHEXILINE', 'PHALLOIDIN', 'PHENACEMIDE', 'PHENACETIN', 'PHENELZINE', 'PHENOBARBITAL', 'PHENOTHIAZINE', 'PHENTOLAMINE', 'PHENYLBUTAZONE', 'PHENYLHYDRAZINE', 'PHENYTOIN', 'PICLAMILAST', 'PIOGLITAZONE', 'PIRACETAM', 'PIRINIXIC ACID', 'PIROXICAM', 'PODOPHYLLOTOXIN', 'PRALIDOXIME CHLORIDE', 'PRAMOXINE', 'PRAVASTATIN', 'PRAZIQUANTEL', 'PRAZOSIN', 'PREDNISOLONE', 'PREDNISONE', 'PRIMAQUINE', 'PRIMIDONE', 'PRINOMASTAT', 'PROBENECID', 'PROCAINAMIDE', 'PROCAINE', 'PROCARBAZINE', 'PROCHLORPERAZINE', 'PROGESTERONE', 'PROGLUMIDE', 'PROMAZINE', 'PROMETHAZINE', 'PROPRANOLOL', 'PROPYLENE GLYCOL', 'PROPYLTHIOURACIL', '(+)-PULEGONE', 'PYRAZINAMIDE', 'PYRIDINE', 'PYRILAMINE', 'PYROGALLOL', 'QUETIAPINE', 'QUINAPRIL', 'RABEPRAZOLE', 'RACEPINEPHRINE', 'RALOXIFENE', 'RAMIPRIL', 'RANITIDINE', 'RAPAMYCIN', '(R)-BICALUTAMIDE', 'RIFABUTIN', 'RIFAMPIN', 'RIFAPENTINE', 'RISPERIDONE', 'RITONAVIR', 'ROFECOXIB', 'ROFLUMILAST', 'ROLIPRAM', 'ROSIGLITAZONE', 'ROXARSONE', 'ROXITHROMYCIN', 'SAFROLE', 'SALICYLAMIDE', 'SALICYLIC ACID', 'SAQUINAVIR', 'S(-)-CARBIDOPA', 'SCH-351591', 'SECOBARBITAL', 'SERTRALINE', 'SIBUTRAMINE', 'SILDENAFIL', 'SIMVASTATIN', 'SISOMICIN', 'SODIUM ARSENITE', 'SODIUM NITROPRUSSIDE', 'SODIUM ORTHOVANADATE', 'SODIUM SELENATE', 'SODIUM SELENITE', 'SOTALOL', 'SPARFLOXACIN', 'SPARTEINE', 'SPIRONOLACTONE', 'SPORIDESMIN A', 'STANNOUS FLUORIDE', 'STANOZOLOL', 'STAUROSPORINE', 'STAVUDINE', 'STREPTOMYCIN', 'STREPTOZOTOCIN', 'SULBACTAM', 'SULCONAZOLE', 'SULFADIAZINE', 'SULFADIMETHOXINE', 'SULFADOXINE', 'SULFAMETHOXAZOLE', 'SULFANILAMIDE', 'SULFAPHENAZOLE', 'SULFATHIAZOLE', 'SULFINPYRAZONE', 'SULFISOXAZOLE', 'SULINDAC', 'SULPIRIDE', 'SUMATRIPTAN', 'SUXAMETHONIUM CHLORIDE', 'TACRINE', 'TACROLIMUS', 'TAMOXIFEN', 'TAZOBACTAM', 'TEGAFUR', 'TEICOPLANIN', 'TEMAFLOXACIN', 'TENIDAP', 'TERAZOSIN', 'TERBINAFINE', 'TERBUTALINE', 'TERFENADINE', 'TESTOSTERONE', 'TETRACAINE', 'TETRACYCLINE', 'TGF BETA-1, HUMAN RECOMBINANT', 'THALIDOMIDE', 'THEOPHYLLINE', 'THIABENDAZOLE', 'THIAMPHENICOL', 'THIMEROSAL', 'THIOACETAMIDE', 'THIOGUANINE', 'THIORIDAZINE', 'TIAPRIDE', 'TICLOPIDINE', 'TICRYNAFEN', 'TIMOLOL', 'TINIDAZOLE', 'TNF-ALPHA, RAT', 'TOBRAMYCIN', 'TOCAINIDE', 'TOLAZAMIDE', 'TORSEMIDE', 'TOSUFLOXACIN', 'TRAMADOL', 'TRANEXAMIC ACID', 'TRANILAST', 'TRANS-PLATINUM(II)DIAMMINE DICHLORIDE', 'TRETINOIN', 'TRIACETIN', 'TRIAMTERENE', 'TRICHLOROACETIC ACID', 'TRICHLOROETHANE', 'TRICHLOROETHYLENE', 'TRIHEXYPHENIDYL', 'TRIMETHADIONE', 'TROGLITAZONE', 'TROPISETRON', 'TROVAFLOXACIN', 'TROXIPIDE', 'UBENIMEX', 'URETHANE', 'VALACYCLOVIR', 'VALDECOXIB', 'VALERIC ACID', 'VALPROIC ACID', 'VALSARTAN', 'VANCOMYCIN', 'VECURONIUM BROMIDE', 'VENLAFAXINE', 'VERAPAMIL', 'VINBLASTINE', 'VINCRISTINE', 'VINORELBINE', 'WARFARIN', 'ZALEPLON', 'ZIDOVUDINE', 'ZILEUTON', 'ZIRCONIUM(IV) CHLORIDE', 'ZOMEPIRAC', 'ZOPICLONE') 

#################################################### EnrichR Connection and Manipulation ######################################################################

websiteLive <- getOption("enrichR.live")
if (websiteLive) {
  listEnrichrSites()
  setEnrichrSite("Enrichr") # Human genes   
}

# setEnrichrSite("Enrichr") # Human genes
# websiteLive <- TRUE
dbs <- listEnrichrDbs()
#################### The following is to print out the list of dbs available ###################
############### Note: dbs is a data frame.   The sample output is in the following (copy 5 lines only)
############### I will only need the libraryName column 
# geneCoverage genesPerTerm                                       libraryName                                                                      link numTerms                                  appyter categoryId
# 1          13362          275                               Genome_Browser_PWMs                  http://hgdownload.cse.ucsc.edu/goldenPath/hg18/database/      615 ea115789fcbf12797fd692cec6df0ab4dbc79c6a          1
# 2          27884         1284                          TRANSFAC_and_JASPAR_PWMs                                  http://jaspar.genereg.net/html/DOWNLOAD/      326 7d42eb43a64a4e3b20d721fc7148f685b53b6b30          1
# 3           6002           77                         Transcription_Factor_PPIs                                                                                290 849f222220618e2599d925b6b51868cf1dab3763          1
# 4          47172         1370                                         ChEA_2013                            http://amp.pharm.mssm.edu/lib/cheadownload.jsp      353 7ebe772afb55b63b41b79dd8d06ea0fdd9fa2630          7
# 5          47107          509                  Drug_Perturbations_from_GEO_2014                                          http://www.ncbi.nlm.nih.gov/geo/      701 ad270a6876534b7cb063e004289dcd4d3164f342          7

dbs <- sort(dbs$libraryName)   # This makes the data frame as a vector containing the library names ONLY.
# ori_dbs <- dbs
# 
# print("Here is the orignal list of enrichr databases")
# print(ori_dbs)

# The following I shorten the dbs vector elements after I discussed with Scott
dbs <- c ("BioPlanet_2019",  "ENCODE_and_ChEA_Consensus_TFs_from_ChIP-X", "GO_Biological_Process_2021", "GO_Cellular_Component_2021", "GO_Molecular_Function_2021", "KEGG_2021_Human",
          "MSigDB_Hallmark_2020", "Reactome_2022", "TG_GATES_2020", "WikiPathway_2021_Human")

# dbs <- c("KEGG_2021_Human", "WikiPathway_2021_Human")
# if (websiteLive) enriched[["KEGG_2021_Human"]]
# if (websiteLive) enriched[["WikiPathway_2021_Human"]]


