### This script will be used to fit IRT models for the emotion identification data
### It will be used to compare data 

# ---- load-packages --------------------------------------------------------
source("~/adroseHelperScripts/R/afgrHelpFunc.R")
install_load("reshape2", "progress", "mirt", "psych", "tidyverse", "kableExtra", "polycor", "magick", "webshot", "lavaan")
## Declare any functions
binary.flip <- function (x) {
  x * -1 + 1
}

# ---- read.data --------------------------------------------------------
## Load item demographics
item.demo <- read.csv('./data/itemDemographics.csv', na.strings = ".")

## Now begin by preparing the item response data from the raw data
base.dir <- c("./data/idMod/")

## Now find all of the subject id values to use
# ## This is going to have to be done in a loop
all.files.pic <- system("ls ./data/idMod/pic*", intern = T)
## First grab the id
tmp <- strSplitMatrixReturn(charactersToSplit = all.files.pic, splitCharacter = '/')[,4]
## Now isolate the id
p1 <- strSplitMatrixReturn(charactersToSplit = tmp, splitCharacter = 'c')[,2]
p2 <- strSplitMatrixReturn(charactersToSplit = p1, splitCharacter = '\\.')[,1]
print(p2)

## Now loop through every id to prepare each individual's data
all.out <- NULL
for(i in p2){
  ## Now decalre the files
  file.1 <- paste(base.dir, "pic", i,".csv", sep='')
  file.2 <- paste(base.dir, "resp", i,".csv", sep='')
  # Now read the data
  in.file.1 <- read.csv(file.1, header=F, sep='\t')
  in.file.2 <- read.csv(file.2, header=F, sep='\t')
  all.data <- merge(in.file.1, in.file.2, by=c("V1", "V2"))
  if(dim(table(table(all.data$V2)))>1){
    #print("Duplicate")
    ## Now find which rows are duplicates
    row.vals <- names(which(table(all.data$V2)>1))
    ## Now go through each of the row vals and take the latest response
    # First create an index for the rows to keep
    rows.to.lose <- NULL
    for(w in row.vals){
      # Find max response time
      rt.max <- max(as.numeric(as.character(all.data[which(all.data$V2==w),"V6.y"])))
      row.index <- which(as.numeric(as.character(all.data[which(all.data$V2==w),"V6.y"]))==rt.max)
      row.to.vom <- which(all.data$V2==w)[-row.index]
      rows.to.lose <- c(rows.to.lose, row.to.vom)
    }
    all.data <- all.data[-rows.to.lose,]
  }
  ## Now fix column names
  colnames(all.data) <- c("fileID", "itemVal", "Pre", "PictureDisplayed", "timeShown", "Null", "Null2", "respVal", "ResponseGiven", "timeGiven", "responseTime", "Null3")
  all.data$participantID <- i
  all.out <- rbind(all.out, all.data)
}

# ---- prep-data --------------------------------------------------------
## Now remove all NULL columns
null.col <- grep("Null", names(all.out))
all.out <- all.out[,-null.col]

## Now go through each of the ids and turn each item from long to wide data
id.vals <- names(table(all.out$participantID))
picture.vals <- names(table(all.out$PictureDisplayed))
all.out.wide <- NULL
real.all.wide <- NULL
real.all.wide.rt <- NULL
question.vals <- names(table(all.out.wide$PictureDisplayed))
out.best.guesses <- NULL
pb <- progress_bar$new(total = length(id.vals))
for(i in id.vals){
  # Isolate the data of interest
  data.to.use <- all.out[which(all.out$participantID==i),]
  ## Now go through an isolate the questions of interest
  # First create the participant specific output
  participant.wide <- NULL
  index <- 1
  irt.tmp <- i
  irt.tmp2 <- i
  for(p in picture.vals){
    pic.data.to.use <- data.to.use[which(data.to.use$PictureDisplayed==p),]
    ## Now check to see if our dim is greater than 0
    if(dim(pic.data.to.use)[1]>0){
      ## Add a order column
      pic.data.to.use$order <- rank(as.numeric(as.character(pic.data.to.use$timeGiven)))
      ## Now reshape using the order as the time val
      resp.given <- dcast(pic.data.to.use, formula = PictureDisplayed + participantID ~ order, value.var = c('ResponseGiven'))
      resp.time <- dcast(pic.data.to.use, formula = PictureDisplayed + participantID ~ order, value.var = 'responseTime')
      # Now merge these
      row.out <- merge(resp.time, resp.given, by=c("PictureDisplayed", "participantID"), suffixes = c("_responseTime", "_responseGiven"))
      # Now make sure the out row is 1x10
      if(!identical(dim(row.out), c(1, 10))){
        #print("short")
        ## Find which column names are not in here
        all.names <- c("PictureDisplayed", "participantID", "1_responseGiven", "2_responseGiven", "3_responseGiven", "4_responseGiven", "1_responseTime", "2_responseTime", "3_responseTime", "4_responseTime")
        names.noin <- which(all.names %in% names(row.out)=="FALSE")
        ## Now create the noin names columns
        for(z in names.noin){
          row.out[all.names[z]] <- NA
        }
      }
      ## Now merge
      if(index ==1){
        participant.wide <- rbind(participant.wide, row.out)
      }else{
        participant.wide <- merge(row.out, participant.wide, by=c("PictureDisplayed", "participantID"), all=T)
      }
      ## Now prepare the response given IRT data
      row.out.irt <- row.out[, c("PictureDisplayed", "participantID", "1_responseGiven", "2_responseGiven", "3_responseGiven", "4_responseGiven")]
      colnames(row.out.irt)[3:6] <- paste(colnames(row.out.irt)[3:6], row.out.irt$PictureDisplayed, sep='_')
      row.out.irt <- row.out.irt[,3:6]
      irt.tmp <- unlist(as.array(c(irt.tmp, row.out.irt)))
      ## Now do the same on RT
      row.out.irt2 <- row.out[,c("PictureDisplayed", "participantID", "1_responseTime", "2_responseTime", "3_responseTime", "4_responseTime")]
      colnames(row.out.irt2)[3:6] <- paste(colnames(row.out.irt2)[3:6], row.out.irt2$PictureDisplayed, sep='_')
      row.out.irt2 <- row.out.irt2[,3:6]
      irt.tmp2 <- unlist(as.array(c(irt.tmp2, row.out.irt2)))
    }
  }
  all.out.wide <- rbind(all.out.wide, participant.wide)
  real.all.wide <- rbind(real.all.wide, irt.tmp)
  real.all.wide.rt <- rbind(real.all.wide.rt, irt.tmp2)
  
  ## Now find the best estimate for the emotion values
  # First collapse the emotions into the four possible emotions
  data.to.use$emotion <- "unhappy"
  data.to.use$emotion[grep("^N", data.to.use$PictureDisplayed)] <- "neutral"
  data.to.use$emotion[grep("^H", data.to.use$PictureDisplayed)] <- "happy"
  data.to.use$emotion[grep("^C", data.to.use$PictureDisplayed)] <- "crying"
  ## now create an output with the best guess
  out.best.guess <- matrix(NA, nrow=1, ncol = 5)
  colnames(out.best.guess) <- c("record_id", rownames(table(data.to.use$emotion, as.character(data.to.use$ResponseGiven))))
  response.patterns <- table(data.to.use$emotion, data.to.use$ResponseGiven)
  ## Now go through each row and find the maximum value -- and return a flag if double!
  for(W in row.names(response.patterns)){
    row.to.check <- response.patterns[W,]
    val.max <- names(which(row.to.check == max(row.to.check)))
    if(length(val.max)==1){
      out.best.guess[,W] <- val.max
    }
    if(length(val.max)>1){
      out.best.guess[,W] <- "Tie"
    }
  }
  out.best.guess[,"record_id"] <- i
  out.best.guesses <- rbind(out.best.guesses, out.best.guess)
  pb$tick()
}
## It looks like this method failed and there is far too much confusion -- I am going to grab the left vs right estimates from the
## createConfusionMatrix.R script
## Here if the value is right the emotions will be:
# happy == 18 ; neutral == 20 ; unhappy == 22 ; crying == 24
## if emtoins are left then:
# happy == 24 ; unhappy == 20 ; neutral == 22 ; crying == 18
direction.vals <- read.csv('./data/leftRightValues.csv')
direction.vals$record_id <- strSplitMatrixReturn(direction.vals$V1, "_")[,1]
direction.vals <- merge(direction.vals, out.best.guesses, all=T)
### Looks like I am missing data for 118-81 and 118-103 from the original
### Looks like I am missing data for 118-62 from the new

## Now create new datasets using the best guesses as the item answers
## Now go through and change to the correct emotion triggers
## Manually flip the 117-114 patterns!
direction.vals$Guess[which(direction.vals$record_id=="117-114")] <- "Left"
## Also need to manually flip: 118-286; I am not sure why this individual failed?
direction.vals$Guess[which(direction.vals$record_id=="118-286")] <- "Right"
direction.vals$Guess[which(direction.vals$record_id=="118-300")] <- "Right"
direction.vals$crying <- 24
direction.vals$crying[direction.vals$Guess=="Right"] <- 18
direction.vals$unhappy <- 22
direction.vals$unhappy[direction.vals$Guess=="Right"] <- 20
direction.vals$neutral <- 20
direction.vals$neutral[direction.vals$Guess=="Right"] <- 22
direction.vals$happy <- 18
direction.vals$happy[direction.vals$Guess=="Right"] <- 24

## Go through and make the proper changes
# Adding a tmp id vals as we are missing some data somewhere along this line
id.vals <- direction.vals$record_id[complete.cases(direction.vals)]
all.out.wide2 <- NULL
for(i in id.vals){
  ## Isolate the individual
  data.to.use <- all.out.wide[which(all.out.wide$participantID==i),]
  if(dim(data.to.use)[1]==0){next}
  ## answer values
  mod.vals <- direction.vals[which(direction.vals$record_id==i),]
  ## Now go through each of the emotions and change each of the response values appropriately
  orig.vals <- c(18, 20, 22, 24)
  for(W in orig.vals){
    # Find the emotion to use
    new.val <- colnames(mod.vals)[which(mod.vals==W)]
    # Now find all of the indices
    orig.indices <- which(data.to.use==W)
    if(length(orig.indices)==0){
      print(paste("Participtant:", i, "Missing:", W,";", new.val, sep = " "))
      next
    }
    orig.indices <- matrix(rc.ind(data.to.use, orig.indices), byrow=F, ncol=2)
    ## Now loop through the indices
    for(L in 1:dim(orig.indices)[1]){
      data.to.use[orig.indices[L,1], orig.indices[L,2]] <- new.val
    }
  }
  ## Now attach this to the new output
  all.out.wide2 <- rbind(all.out.wide2, data.to.use)
}

## This code was added 2020-10-06 because ADROSE found some issues with the correct vs incorrect coding!!!
## Now go through and double check the direction vals
all.out.wide2$emotion <- "crying"
all.out.wide2$emotion[grep("^H", all.out.wide2$PictureDisplayed)] <- "happy"
all.out.wide2$emotion[grep("^U", all.out.wide2$PictureDisplayed)] <- "unhappy"
all.out.wide2$emotion[grep("^N", all.out.wide2$PictureDisplayed)] <- "neutral"
## This code was added 2020-10-06 because ADROSE found some issues with the correct vs incorrect coding!!!
## Now go through and double check the direction vals
all.out.wide2$emotion <- "crying"
all.out.wide2$emotion[grep("^H", all.out.wide2$PictureDisplayed)] <- "happy"
all.out.wide2$emotion[grep("^U", all.out.wide2$PictureDisplayed)] <- "unhappy"
all.out.wide2$emotion[grep("^N", all.out.wide2$PictureDisplayed)] <- "neutral"

## Now double check the scores by participant
crying.response <- table(all.out.wide2$emotion, all.out.wide2$participantID, all.out.wide2$"1_responseGiven")[,,1]
happy.response <- table(all.out.wide2$emotion, all.out.wide2$participantID, all.out.wide2$"1_responseGiven")[,,2]
neutral.response <- table(all.out.wide2$emotion, all.out.wide2$participantID, all.out.wide2$"1_responseGiven")[,,3]
unhappy.response <- table(all.out.wide2$emotion, all.out.wide2$participantID, all.out.wide2$"1_responseGiven")[,,4]

## Now do the real.all.wide
real.all.wide2 <- NULL
real.all.wide <- as.data.frame(real.all.wide)
for(i in id.vals){
  ## Isolate the individual
  data.to.use <- real.all.wide[which(real.all.wide[,1]==i),]
  if(dim(data.to.use)[1]==0){next}
  ## answer values
  mod.vals <- direction.vals[which(direction.vals$record_id==i),]
  ## Now go through each of the emotions and change each of the response values appropriatly
  orig.vals <- c(18, 20, 22, 24)
  for(W in orig.vals){
    # Find the emotion to use
    new.val <- colnames(mod.vals)[which(mod.vals==W)]
    # Now find all of the indices
    orig.indices <- which(data.to.use==W)
    if(length(orig.indices)==0){
      print(paste("Participtant:", i, "Missing:", W,";", new.val, sep = " "))
      next
    }
    ## Now change the values to the new value
    data.to.use[,orig.indices] <- new.val
  }
  ## Now attach this to the new output
  real.all.wide2 <- rbind(real.all.wide2, data.to.use)
}

## Make the variables factors so we can compute the alpha for them
all.out.wide2[,7] <- factor(all.out.wide2[,7], c("crying", "unhappy", "neutral", "happy"))
all.out.wide2[,8] <- factor(all.out.wide2[,8], c("crying", "unhappy", "neutral", "happy"))
## Add in a checkpoint here!!!
all.out.wide2[,9] <- factor(all.out.wide2[,9], c("crying", "unhappy", "neutral", "happy"))
all.out.wide2[,10] <- factor(all.out.wide2[,10], c("crying", "unhappy", "neutral", "happy"))

## Now play with the IRT models here
# first orgainze the data
real.all.wide <- as.data.frame(real.all.wide2)
real.all.wide.1 <- real.all.wide[,c(1, grep("1_", names(real.all.wide)))]
real.all.wide.2 <- real.all.wide[,c(1, grep("2_", names(real.all.wide)))]
real.all.wide.3 <- real.all.wide[,c(1, grep("3_", names(real.all.wide)))]
real.all.wide.4 <- real.all.wide[,c(1, grep("4_", names(real.all.wide)))]

names(real.all.wide.1) <- gsub(names(real.all.wide.1), pattern = '1_', replacement = '')
names(real.all.wide.2) <- gsub(names(real.all.wide.2), pattern = '2_', replacement = '')
names(real.all.wide.3) <- gsub(names(real.all.wide.3), pattern = '3_', replacement = '')
names(real.all.wide.4) <- gsub(names(real.all.wide.4), pattern = '4_', replacement = '')

## Now combine these and run IRT
for.irt <- rbind(real.all.wide.1, real.all.wide.2, real.all.wide.3, real.all.wide.4)
for.irt3 <- matrix(NA, ncol = 96, nrow = dim(for.irt)[1])
# I need to go through and find the correct string for each column and then identify the correct responses within each cell
for(i in 2:97){
  string.index <- for.irt[,i]
  irt_three_col <- i -1
  ## First grab the correct index
  # Grab the character from the colname
  char.val <- tolower(substr(strSplitMatrixReturn(colnames(for.irt)[i], "_")[,2][1], 1, 1))
  print(char.val)
  if(char.val == 'c') cor.index <- which(string.index=="crying")
  if(char.val == 'n') cor.index <- which(string.index=="neutral")
  if(char.val == 'u') cor.index <- which(string.index=="unhappy")
  if(char.val == 'h') cor.index <- which(string.index=="happy")
  ## Now change the values
  for.irt3[which(!is.na(for.irt[,i])),irt_three_col] <- 0
  for.irt3[cor.index,irt_three_col] <- 1
}

for.irt3 <- as.data.frame(for.irt3)
for.irt3$record_id <- for.irt$V1

## Now salvage some of the record id values
for.irt3 <- for.irt3 %>% mutate(record_id = as.character(record_id)) %>% 
  mutate(record_id = if_else(record_id %in% "117-115", "118-64", record_id)) %>% 
  mutate(record_id = if_else(record_id %in% "118-101", "117-80",record_id))


## Load the demo data
load("./data/fname.gz")
ds_demo <- out.data[[9]]

## Now merge the data
for.mimic <- merge(for.irt3, ds_demo, by="record_id", all=T)

# ---- dim-assess --------------------------------------------------------
# First calc the cor mat
cor.mat <- polychoric(for.irt3[,1:96])
pca.decomp <- principal(cor.mat$rho, nfactors = 4, rotate = "varimax")
plot(pca.decomp$values)
fa.decomp <- fa(cor.mat$rho, nfactors = 4, rotate = "varimax")
## Now do a scree plot
fa.parallel(cor.mat$rho, fa = "fa")
