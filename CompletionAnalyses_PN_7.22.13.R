#This script is used to run logistic regression analyses.  The goal is to compare the
#characteristics of students who completed their PN/VN program to students who did not complete 

# this subsets the dataset to only include students with a PNCompletion score
# of 0 (did not complete) or 1 (graduated); this excludes all active students
Dataset <- subset(PNdata, PNdata$PNCompletion %in% c(0, 1))

#******************************************************
#********************GETTING READY*********************
require(abind)
require(class)
require(e1071)
require(gplots)

#subset the data by campus; be sure the data file name is accurate
# (might need to change for diff program)


#change the campus name here to the appropriate campus data file
#note that this step is not necessary, it just truncates the data set so that it is easier to view inside of R; 
##it works perfectly well to skip this step and run analyses using the entire data set, with all 100+ variables
MyPNVars <- names(Dataset) %in% c("PNStartTypeID", "PAXPNCompositeRawScore", "PNCompletion", 
	"PAXPNCompositePercentileMathPercentile", "PAXPNCompositePercentileSciencePercentile", 
	"PAXPNCompositePercentileVerbalPercentile")

#change the campus name here to the appropriate campus data file
#the data set "sortedData" only contains the variables that I am going to use in my analyses
sortedData <- Dataset[MyPNVars]
sortedData$PNCompletion <- as.factor(sortedData$PNCompletion)  


#******************************************************
#****************RUNNING THE ANALYSES******************
#all the output vectors need to be set-up in advance

startType_pVal <- NULL
pax_pVal <- NULL
paxMath_pVal <- NULL
paxScience_pVal <- NULL
paxVerbal_pVal <- NULL

startType_coef <- NULL
pax_coef <- NULL
paxMath_coef <- NULL
paxScience_coef <- NULL
paxVerbal_coef <- NULL

startType_prob <- NULL
pax_prob <- NULL
paxMath_prob <- NULL
paxScience_prob <- NULL
paxVerbal_prob <- NULL

countNoncomp <- NULL
correctNoncomp <- NULL

# run logistic regression 
	model <- glm(PNCompletion~ PNStartTypeID + PAXPNCompositeRawScore +  
		PAXPNCompositePercentileMathPercentile + PAXPNCompositePercentileSciencePercentile + 
		PAXPNCompositePercentileVerbalPercentile, data=sortedData, family="binomial") 

	#turns the model summary into an object
	mod <- summary(model)

			# save the coefficient for each predictor
			# if the coef value is 0.05, then a 1 unit increase in the coef increases 
			# the log odds of the outcome by 0.05; 
			startType_coef <- as.numeric(format(mod$'coefficients'[2], digits=2)) [1]
			pax_coef <- as.numeric(format(mod$'coefficients'[3], digits=2)) [1]
			paxMath_coef <- as.numeric(format(mod$'coefficients'[4], digits=2)) [1]
			paxScience_coef <- as.numeric(format(mod$'coefficients'[5], digits=2)) [1]
			paxVerbal_coef <- as.numeric(format(mod$'coefficients'[6], digits=2)) [1]

	
			# convert log odds to probability (because log odds are difficult to discuss)
			startType_prob <- round(1/ (1 + exp(-startType_coef)), digits=2)
			pax_prob <- round(1/ (1 + exp(-pax_coef)), digits=2) [1]
			paxMath_prob <- round(1/ (1 + exp(-paxMath_coef)), digits=2) [1]
			paxScience_prob <- round(1/ (1 + exp(-paxScience_coef)), digits=2) [1]
			paxVerbal_prob <- round(1/ (1 + exp(-paxVerbal_coef)), digits=2) [1]


		#****BE CAREFUL HERE; if a variable does not enter due to NA, then the locations
		#****will be different
		startType_pVal <- format(mod$'coefficients'[20], digits=3) [1]
		pax_pVal <- format(mod$'coefficients'[21], digits=3) [1]
		paxMath_pVal <- format(mod$'coefficients'[22], digits=3) [1]
		paxScience_pVal <- format(mod$'coefficients'[23], digits=3) [1]
		paxVerbal_pVal <- format(mod$'coefficients'[24], digits=3) [1]

	# this creates an object to hold the predicted values from the glm model
	predictions <- fitted(model)

		# this creates a classification table noting the number of the predicted probabilities
		# that actually agree with the data (i.e., correct classifications)	
				
		classifTable <- table(predictions>.5, sortedData$PNCompletion)
	
			#this extracts the count of correct classifications of non-completers
			countNoncomp <- classifTable[2,1]

			#this counts up the total of non-completers for that course
			#FIXME: need to deal with cases that do not have any non-completers
			##this causes error: "subscript out of bounds"
			numNoncomp <- classifTable[2,1] + classifTable[1,1]
		
			# this calculates the percentage of correct classifications of non-completers
			correctNoncomp <- round((classifTable[2,1] / numNoncomp)*100)



#******************************************************
#*********CREATING & PRINTING THE RESULT TABLES********

#this combines all probability values into a column
probs <- as.numeric(rbind(startType_prob, pax_prob, paxMath_prob, 
	paxScience_prob, paxVerbal_prob))

#this combines all p values into a column
pVals <- as.numeric(rbind(startType_pVal, pax_pVal, paxMath_pVal, 
	paxScience_pVal, paxVerbal_pVal))

probTable <- cbind(probs, pVals)

rownames(probTable) <- c("Start Type", "PAX-PN Composite", "PAX-PN Math", 
	"PAX-PN Science", "PAX-PN Verbal")

# change p values of 0.000 to be labeled as <.001
tmpZero = probTable[,2]<.001
probTable[tmpZero,2] = "<.001***"

probTable