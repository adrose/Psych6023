rm(list=ls(all=TRUE))

# ---- load-sources -------------------------------------------------------------------


# ---- load-packages -----------------------------------------------------------
library(miechv3)
library(magrittr) #Pipes
library(ggplot2)
library(MplusAutomation)
library(dplyr)
library(lme4)

# ---- declare-globals ---------------------------------------------------------
orgwd <- "C:/Users/arosen/Documents/GitHub/Psych6023"

create_nu_mod <- function(item_num) {
  nu_mod <- paste0(
    "MODEL: f by \n",
    paste0("V",(1:24)[-item_num],"*",collapse = "\n"),
    paste0("\nV",item_num," (alpha);"),
    "\n!V1 - V22*
!V23 (alpha)
!V24;
!Factor variances set to 1;
f@1;
!Group predicts latent factor;
f on P2amp P2lat P2int;
!Define interaction between latent factor and group;

!Group and interaction predict Question 10;",
    paste0("\nV",item_num, " on ", "P2amp", " P2lat", " P2int;"),
    "
!create new variable for discrimination parameter
!MODEL CONSTRAINT: new (discamp);
!discamp = alpha + gammaalpha;"
  )

  return(nu_mod)
}

cat(create_nu_mod(item_num=2),"\n")


# ---- load-data ---------------------------------------------------------------
irt_dat <- readr::read_csv("./data/forMIMICUnh.csv")
colnames(irt_dat)[1:24] <- paste("V", 1:24, sep='')
#sapply(irt_dat,class)


# ---- tweak-data --------------------------------------------------------------
#as.numeric(scale(irt_dat$X.Crying_Lat))
cat_cut <- 0.5
mp_irt_dat <- irt_dat %>%
  mutate(
    my_id = as.numeric(factor(record_id)),
    X.Crying_Amp = as.numeric(scale(ace_score)),
    X.Crying_Lat = as.numeric(scale(dose_hv_visit_count)),
    P2int = X.Crying_Amp*X.Crying_Lat,
    hamphlat = (X.Crying_Amp > cat_cut & X.Crying_Lat > cat_cut)*1,
    hampllat = (X.Crying_Amp > cat_cut & X.Crying_Lat <= cat_cut*-1)*1,
    lamphlat = (X.Crying_Amp <= cat_cut*-1 & X.Crying_Lat > cat_cut)*1,
    lampllat = (X.Crying_Amp <= cat_cut*-1 & X.Crying_Lat <= cat_cut*-1)*1,
    otherval = ((hamphlat+hampllat+lamphlat+lampllat) == 0)*1,
    catgrp = dplyr::case_when(
      hamphlat == 1 ~ "hh",
      hampllat == 1 ~ "hl",
      lamphlat == 1 ~ "lh",
      lampllat == 1 ~ "ll",
      otherval == 1 ~ "other",
      TRUE ~ NA_character_
    )
  ) %>% #View()
  dplyr::select(
    my_id
    , dose = dose_hv_visit_count
    , P2amp = X.Crying_Amp
    , P2lat = X.Crying_Lat
    , P2int
    , dplyr::everything()
    , -record_id
  )

mp_irt_dat %>%
  dplyr::group_by(catgrp) %>%
  dplyr::count()


# ---- mplus-mimic-models --------------------------------------------------------------

create_mp_mod <- function(item_num){
  mimic_mod <- mplusObject(
    autov = FALSE,
    TITLE  = "MIMIC DIF model Item 10 Non-Uniform DIF;",
    #DATA = "VARIANCES = NOCHECK;",#Type = imputation;",
    VARIABLE = paste0("categorical = ",paste0("V",1:24,collapse = "\n"),";"),
    ANALYSIS = "ESTIMATOR = MLR;\n!Random type required for latent variable interactions\nType=random;",
    MODEL = create_nu_mod(item_num),
    #OUTPUT = "sampstat; tech1; TECH8;",
    rdata=mp_irt_dat,
    usevariables = c(paste0("V",1:24),"P2amp","P2lat","P2int"),
    imputed = FALSE#,
    #MODELCONSTRAINT = paste0("0 = ",paste0("m",1:3,collapse = " + "),";")
  )
  return(mimic_mod)
}

MplusAutomation::cd(file.path(orgwd,"data")); nu_model <- mplusModeler(create_mp_mod(item_num=22),dataout="numod.txt",modelout="numod.inp",run=1); setwd(orgwd)
# nu_model$results$parameters$unstandardized

nures <- list()
for(iter in 1:24){
  # Create a directory for every item
  MplusAutomation::cd(file.path(orgwd,"data/"));
  nu_model <- mplusModeler(create_mp_mod(item_num = iter),dataout="numod.txt",modelout=paste("V", iter, "numod.inp", sep=''),run=1); setwd(orgwd)
  nures[[iter]] <- nu_model$results$parameters$unstandardized  %>% as.data.frame() %>%
    dplyr::filter(paramHeader %in% c(paste0("V",iter,".ON"),"F.ON"))
  nures[[iter]]$item <- iter
}

## Save these models for export
save(nures, file="./data/uniModels.RData")
