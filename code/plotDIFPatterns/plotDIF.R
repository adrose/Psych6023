### This script will be used to plot the models that exhibited non uniform DIF from the MIMIC models
### This will only be done for th eunhappy contrast though!

# ---- load-packages --------------------------------------------------------
source("~/adroseHelperScripts/R/afgrHelpFunc.R")
install_load("GGally", "tidyverse")


# ---- declare-functions --------------------------------------------------------
## Write a function which will return a vector of probabilities given a vector of theta estimates; demo vars; and slopes for each logistic coefficient
## It will also require a difficulty param (intercept) but I am going to begin with just the intercept == 0
calcLogit <- function(demo.val = NULL, coef.val = NULL, intercept=NULL){
  
}

  
logit2prob <- function(logit){
    odds <- exp(logit)
    prob <- odds / (1 + odds)
    output <- as.data.frame(cbind(odds, prob))
    colnames(output) <- c("odds", "probs")
    return(output)
  }


# ---- load-data --------------------------------------------------------
load("./data/uniModels.RData")
nures_uniform <- nures

load("./data/nunimModels.RData")
nures_nunifrom <- nures

## Load the demo data
load("./data/fname.gz")
ds_demo <- out.data[[9]]

# ---- plot-models --------------------------------------------------------
## First grab all of the significant non uniform models
uniDIF <- dplyr::bind_rows(nures_uniform) %>%
  dplyr::filter(paramHeader %in% paste0("V",c(4,10,14,20,22,24),".ON")) %>%
  dplyr::filter(param == "P2INT") %>%
  dplyr::filter(pval < 0.05)
uniDIF$item <- strSplitMatrixReturn(uniDIF$paramHeader, ".O")[,1]

## Now grab all of the param values
mod.one <- dplyr::bind_rows(nures_uniform) %>% 
  dplyr::filter(item=="4")

## Now plot these accordingly
## First do the ICC for an individual with the mean dosage & ace score
seq.vals <- seq(-3, 3, .001)

## The intercept and slope will be taken directly from the Mplus output, it is not available to this script!!!!
## The intercept value for question 4 is: -.227
## The slope value for question 4 is: 1.291
## So the predicted probs for an individual at the mean of ace & dosage is:
logit.vals.mean <- (seq.vals * 1.291) - .227  
prob.vals.mean <- logit2prob(logit.vals.mean)
prob.vals.mean$contrast <- "Mean/Mean"


## Now do the mean logit vals for an individual @ + 1 sd for both cats
logit.vals.plus1.b <- (seq.vals * 1.291) - .227  -.6 + .234 + .664 
prob.vals.plus1.b <- logit2prob(logit.vals.plus1.b)
prob.vals.plus1.b$contrast <- "+2ACE/+2HV"


## Now do the mean logit vals for an individual @ - 1 sd for both cats
logit.vals.minus.b <- (seq.vals * 1.291) - .227 + .6  -.234  -.664 
prob.vals.minus.b <- logit2prob(logit.vals.minus.b)
prob.vals.minus.b$contrast <- "-2ACE/-2HV"

## Now do the mean logit vals for an individual @ - 1 sd for both cats
logit.vals.minus.a <- (seq.vals * 1.291)-.227 - .6  + .234  -.664 
prob.vals.minus.a <- logit2prob(logit.vals.minus.a)
prob.vals.minus.a$contrast <- "2ACE/-2HV"

## Now do the mean logit vals for an individual @ - 1 sd for both cats
logit.vals.minus.c <- (seq.vals * 1.291)-.227  +.6 - .234 + .664 
prob.vals.minus.c <- logit2prob(logit.vals.minus.c)
prob.vals.minus.c$contrast <- "-2ACE/2HV"


## Now combine these into one plot
all.prob.vals <- rbind(prob.vals.mean, prob.vals.plus1.b, prob.vals.minus.b, prob.vals.minus.a, prob.vals.minus.c)
all.prob.vals$theta <- rep(seq.vals, 5)

## Now plot this
all.prob.vals %>% ggplot(., aes(x=theta, y=probs, group=contrast, fill=contrast, color=contrast)) +
  geom_line() +
  ggtitle("Uniform DIF: Unhappy Question 4") +
  theme_bw() +
  ylab("Probability of Endorsement") +
  xlab("Theta")
