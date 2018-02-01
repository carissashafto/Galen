#This function is used in program dismissal analyses.  The goal is to compare the
#students who were dismissed or withdrew during a particular academic term to students
#who withdrew or were dismissed in the immediately preceding terms.
#This can be done across campuses/programs or for each program/campus separately; 
#****this version requires campuses to be in separate files

#***the code in this file is for comparing summer 2013 dismissals/drops to the spring 2013 quarter
#***and all 4 quarters of 2012

#1)conducts a manova across all predictor variables, with term as the fixed factor; basically asking
##	whether any of the predictors predict which term the student dropped
#2)prints the F results and descriptives and saves them for printing
#3)compares each of the last 4 terms to the current term on all the predictor variables
#4)saves the p values 

#******************************************************
#********************GETTING READY*********************

require(abind)
require(class)
require(e1071)
require(car)
require(RODBC)
require(gplots) 

#change the campus name here to the appropriate campus data file
#be sure to list all variables that you are interested in
#note that this step is not necessary, it just truncates the data set so that it is easier to view inside of R; 
##it works perfectly well to skip this step and run analyses using the entire data set, with all 100+ variables
MyRNVars <- names(TexData) %in% c("PNGPA", "GalenPNGrad", "PAXRNCompositeRawScore", "RN_CompletedHours", 
	"RNStartTypeID", "Age", "EFC", "GalenPellRecipient", "MaternalEducationLevelID", 
	"ChildrenToSupport", "RNLeaveTerm")

#change the campus name here to the appropriate campus data file
#the data set "Truncated" only contains the variables that I am going to use in my analyses
Truncated <- TexData[MyRNVars]

sortedData <- Truncated[order(Truncated$RNLeaveTerm),]

#all the output vectors need to be set-up in advance
Pillai1 <- NULL
DfAnova1 <- NULL
pValAnova1 <- NULL

Pillai2 <- NULL
DfAnova2 <- NULL
pValAnova2 <- NULL

Pillai3 <- NULL
DfAnova3 <- NULL
pValAnova3 <- NULL

Pillai4 <- NULL
DfAnova4 <- NULL
pValAnova4 <- NULL

#I set up 'RNLeaveTerm' as a factor
sortedData$RNLeaveTerm <- as.factor(sortedData$RNLeaveTerm)


#******************************************************
#****************RUNNING THE ANALYSES******************

#xvar is Term student left (e.g., spring/first quarter 2012, or term code 1201)
#yvar are all the predictors (e.g., EFC, RN_CompletedTerms)

## run the comparisons on each subset to do the pairwise comparisons
## be sure to list all variables that you are interested in; change if necessary

#FIXME: it doesn't know what to do when there is no value for a variable
# it ends up filling in the next one so that all values are shifted 1 row...

#------------------------------------------------------
## comparing summer 2013 to spring 2013
#------------------------------------------------------

mod1302v1301 <- manova(cbind(PNGPA, GalenPNGrad, PAXRNCompositeRawScore, RN_CompletedHours, RNStartTypeID, 
	Age, EFC, GalenPellRecipient, MaternalEducationLevelID, ChildrenToSupport)
	~ RNLeaveTerm, data=sortedData, subset=RNLeaveTerm %in% c("1302", "1301"))

#this summary shows the overall comparison
sum1 <- summary(mod1302v1301)
	# save p value, F statistic (actually Pillai stat), and Df to print in table at the end
	Pillai1 <- format(sum1$stats[3], digits=2)[1]
	DfAnova1 <- sum1$stats[9][1]
	pValAnova1 <- format(sum1$stats[11], digits=3)[1]

# this summary shows the stats for individual variables
# can use the command 'str' to see the internal components; for example, 'str(pVal_children)'
sum1 <- summary.aov(mod1302v1301)
	# save p value for each individual comparison
	# the first number is the variable number, the second number is the item number, not sure 
	# about the third number; the final [1] saves the value as a vector

	pVal_PNGPA <- round(sum1[[1]][[5]][[1]] [1], digits=3)
	pVal_galenGrad <- round(sum1[[2]][[5]][[1]] [1], digits=3)
	pVal_paxRN <- round(sum1[[3]][[5]][[1]] [1], digits=3)
	pVal_hours <- round(sum1[[4]][[5]][[1]] [1], digits=3)
	pVal_restart <- round(sum1[[5]][[5]][[1]] [1], digits=3)
	pVal_age <- round(sum1[[6]][[5]][[1]] [1], digits=3)
	pVal_EFC <- round(sum1[[7]][[5]][[1]] [1], digits=3)
	pVal_pell <- round(sum1[[8]][[5]][[1]] [1], digits=3)
	pVal_maternalEd <- round(sum1[[9]][[5]][[1]] [1], digits=3)
	pVal_children <- round(sum1[[10]][[5]][[1]] [1], digits=3)

#combines all the individual p values into 1 object
pVal_1302v1301 <- rbind(pVal_PNGPA, pVal_galenGrad, pVal_paxRN, pVal_hours, pVal_restart, 
	pVal_age, pVal_EFC, pVal_pell, pVal_maternalEd, pVal_children)


#-----------------------------------------------------
## comparing summer 2013 to winter 2012
#-----------------------------------------------------

mod1302v1204 <- manova(cbind(PNGPA, GalenPNGrad, PAXRNCompositeRawScore, RN_CompletedHours, RNStartTypeID, 
	Age, EFC, GalenPellRecipient, MaternalEducationLevelID, ChildrenToSupport)
	~ RNLeaveTerm, data=sortedData, subset=RNLeaveTerm %in% c("1302", "1204"))

# can use 'summary.aov(mod1302v1204)' to see the stats for individual variables
sum2 <- summary(mod1302v1204)

	# save p value, F statistic (actually Pillai stat), and Df to print in table at the end
	Pillai2 <- format(sum2$stats[3], digits=2)[1]
	DfAnova2 <- sum2$stats[9][1]
	pValAnova2 <- format(sum2$stats[11], digits=3)[1]

# this summary shows the stats for individual variables
sum1 <- summary.aov(mod1302v1204)
	# save p value for each individual comparison
	pVal_PNGPA <-round(sum1[[1]][[5]][[1]] [1], digits=3)
	pVal_galenGrad <- round(sum1[[2]][[5]][[1]] [1], digits=3)
	pVal_paxRN <- round(sum1[[3]][[5]][[1]] [1], digits=3)
	pVal_hours <- round(sum1[[4]][[5]][[1]] [1], digits=3)
	pVal_restart <- round(sum1[[5]][[5]][[1]] [1], digits=3)
	pVal_age <- round(sum1[[6]][[5]][[1]] [1], digits=3)
	pVal_EFC <- round(sum1[[7]][[5]][[1]] [1], digits=3)
	pVal_pell <- round(sum1[[8]][[5]][[1]] [1], digits=3)
	pVal_maternalEd <- round(sum1[[9]][[5]][[1]] [1], digits=3)
	pVal_children <- round(sum1[[10]][[5]][[1]] [1], digits=3)

#combines all the individual p values into 1 object
pVal_1302v1204 <- rbind(pVal_PNGPA, pVal_galenGrad, pVal_paxRN, pVal_hours, pVal_restart, pVal_age, 
	pVal_EFC, pVal_pell, pVal_maternalEd, pVal_children)


#----------------------------------------------------
## comparing summer 2013 to fall 2012
#----------------------------------------------------

mod1302v1203 <- manova(cbind(PNGPA, GalenPNGrad, PAXRNCompositeRawScore, RN_CompletedHours, RNStartTypeID, 
	Age, EFC, GalenPellRecipient, MaternalEducationLevelID, ChildrenToSupport)
	~ RNLeaveTerm, data=sortedData, subset=RNLeaveTerm %in% c("1302", "1203"))

# can use 'summary.aov(mod1302v1203)' to see the stats for individual variables
sum3 <- summary(mod1302v1203)

	# save p value, F statistic (actually Pillai stat), and Df to print in table at the end
	Pillai3 <- format(sum3$stats[3], digits=2)[1]
	DfAnova3 <- sum3$stats[9][1]
	pValAnova3 <- format(sum3$stats[11], digits=3)[1]

# this summary shows the stats for individual variables
sum1 <- summary.aov(mod1302v1203)
	# save p value for each individual comparison
	pVal_PNGPA <-round(sum1[[1]][[5]][[1]] [1], digits=3)
	pVal_galenGrad <- round(sum1[[2]][[5]][[1]] [1], digits=3)
	pVal_paxRN <- round(sum1[[3]][[5]][[1]] [1], digits=3)
	pVal_hours <- round(sum1[[4]][[5]][[1]] [1], digits=3)
	pVal_restart <- round(sum1[[5]][[5]][[1]] [1], digits=3)
	pVal_age <- round(sum1[[6]][[5]][[1]] [1], digits=3)
	pVal_EFC <- round(sum1[[7]][[5]][[1]] [1], digits=3)
	pVal_pell <- round(sum1[[8]][[5]][[1]] [1], digits=3)
	pVal_maternalEd <- round(sum1[[9]][[5]][[1]] [1], digits=3)
	pVal_children <- round(sum1[[10]][[5]][[1]] [1], digits=3)

#combines all the individual p values into 1 object
pVal_1302v1203 <- rbind(pVal_PNGPA, pVal_galenGrad, pVal_paxRN, pVal_hours, pVal_restart, 
	pVal_age, pVal_EFC, pVal_pell, pVal_maternalEd, pVal_children)


#-----------------------------------------------------
## comparing summer 2013 to summer 2012
#-----------------------------------------------------

mod1302v1202 <- manova(cbind(PNGPA, GalenPNGrad, PAXRNCompositeRawScore, RN_CompletedHours, RNStartTypeID, 
	Age, EFC, GalenPellRecipient, MaternalEducationLevelID, ChildrenToSupport)
	~ RNLeaveTerm, data=sortedData, subset=RNLeaveTerm %in% c("1302", "1202"))

# can use 'summary.aov(mod1302v1202)' to see the stats for individual variables
sum4 <- summary(mod1302v1202)

	# save p value, F statistic (actually Pillai stat), and Df to print in table at the end
	Pillai4 <- format(sum4$stats[3], digits=2)[1]
	DfAnova4 <- sum4$stats[9][1]
	pValAnova4 <- format(sum4$stats[11], digits=3)[1]

# this summary shows the stats for individual variables
sum1 <- summary.aov(mod1302v1202)
	# save p value for each individual comparison
	pVal_PNGPA <-round(sum1[[1]][[5]][[1]] [1], digits=3)
	pVal_galenGrad <- round(sum1[[2]][[5]][[1]] [1], digits=3)
	pVal_paxRN <- round(sum1[[3]][[5]][[1]] [1], digits=3)
	pVal_hours <- round(sum1[[4]][[5]][[1]] [1], digits=3)
	pVal_restart <- round(sum1[[5]][[5]][[1]] [1], digits=3)
	pVal_age <- round(sum1[[6]][[5]][[1]] [1], digits=3)
	pVal_EFC <- round(sum1[[7]][[5]][[1]] [1], digits=3)
	pVal_pell <- round(sum1[[8]][[5]][[1]] [1], digits=3)
	pVal_maternalEd <- round(sum1[[9]][[5]][[1]] [1], digits=3)
	pVal_children <- round(sum1[[10]][[5]][[1]] [1], digits=3)

#combines all the individual p values into 1 object
pVal_1302v1202 <- rbind(pVal_PNGPA, pVal_galenGrad, pVal_paxRN, pVal_hours, pVal_restart, pVal_age, 
	pVal_EFC, pVal_pell, pVal_maternalEd, pVal_children)


#******************************************************
#*************CREATING THE RESULT TABLES***************


#this creates a table with the MANOVA stats that were saved: Pillai stat, Df, p value
#cbind is to bind the column vectors
table1 <- cbind(Pillai1, DfAnova1, pValAnova1)
table2 <- cbind(Pillai2, DfAnova2, pValAnova2)
table3 <- cbind(Pillai3, DfAnova3, pValAnova3)
table4 <- cbind(Pillai4, DfAnova4, pValAnova4)

#use rbind to bind all the results into 1 table
#each "table" is a term comparison
ManovaTable <- cbind(table1, table2, table3, table4)


#combine all p values into a single table
pVals <- cbind(pVal_1302v1301, pVal_1302v1204, pVal_1302v1203, pVal_1302v1202)
colnames(pVals) <- c("1302v.1301", "1302v.1204", "1302v.1203", "1302v.1202")

#print the tables to the screen
ManovaTable
pVals
