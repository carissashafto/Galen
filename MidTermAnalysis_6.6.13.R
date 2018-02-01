#This function is used in mid-term/final grade analyses.  The goal is to compare the
#mid-term/final grades for a particular course to the mid-term/final grades for that same
#course in the immediately preceding terms.
#This can be done across campuses/programs or for each program/campus separately; 
#****this version requires campuses to be in separate files

#1)searches a data set to pull out a course code, one at a time
#2)conducts an anova on that course code
#3)prints the F results and descriptives and saves them for printing
#4)Tukey HSD runs all possible pairwise comparisons
#5)prints the post-hoc results
#6)saves the post-hoc stats to a file for printing 
#7)loops back to course code to do the same analyses for the next course
#8)prints a summary of all the F stats in one table and highlights significant ones (p<.05)
#9)prints a summary of the post-hoc stats for only courses that had significant F
# 	-- one table for comparison1 and one for comparison2


#******************************************************
#********************GETTING READY*********************
require(abind)
require(class)
require(e1071)
require(car)
require(RODBC)
require(beanplot)
require(gplots) #for textplot function


#be sure to list all variables that you are interested in
#note that this step is not necessary, it just truncates the data set so that it is easier to view 
##inside of R; it works perfectly well to skip this step and run analyses using the entire data set, 
##with all 100+ variables

MyVars <- names(Dataset) %in% c("GradeAnalysisID", "MidTermNumericGrade", "Term", "CourseType", 
	"DeliveryMethod")

#the data set "Truncated" only contains the variables that I am going to use in my analyses
Truncated <- Dataset[MyVars]

sortedData <- Truncated[order(Truncated$GradeAnalysisID),]
courses <- sortedData$GradeAnalysisID   
courseCodes <- levels(courses)
numCourseCodes <- length(courseCodes)

#******************************************************
#****************RUNNING THE ANALYSES******************
#xvar is Term (e.g., spring 2012, or term code 1201)
#yvar is MidTermNumericGrade (could be FinalNumericGrade)

#all the output vectors need to be set-up in advance
FstatCourse <- NULL
DfCourse <- NULL
pValCourse <- NULL

lastVsPrevious <- NULL
lastVsPenultimate <- NULL

#Tukey HSD will not work with numeric variables, so I set 'Term' up as a factor
sortedData$Term <- as.factor(sortedData$Term)

numTermsAll <- NULL

#this for loop runs the analyses for each course, one at a time
for (j in seq(numCourseCodes)) {

	# select the subset of the data that we are interested in (this course)
	thisCourse <- which(sortedData$GradeAnalysisID %in% courseCodes[j])   
	thisCourseData <- sortedData[thisCourse,]

	# deal with NAs
	notNAs <- !is.na(thisCourseData$MidTermNumericGrade)
	thisCourseData <- thisCourseData[notNAs,]

	# find out how many terms
	acadTerms <- factor(thisCourseData$Term)
	terms <- levels(acadTerms)
  	numTerms <- length(terms)

	# run anova across all academic terms/quarters
	numTermsAll[j] <- numTerms
	if (numTerms > 1) {
		AnovaModel.1 <- aov(MidTermNumericGrade~Term, data=thisCourseData)

		# save p value, F statistic, and Df for the anova to print in table at the end
		pValCourse[j] <- round(anova(AnovaModel.1)$'Pr(>F)'[1], digits=3)
		FstatCourse[j] <- anova(AnovaModel.1)$'F value'[1]
		DfCourse[j] <- anova(AnovaModel.1)$'Df'[1]

		# all TukeyHSD (all pairwise comparisons, regardless of original F value)
		Comparisons <- TukeyHSD(AnovaModel.1, na.action=na.exclude)[1] 
		
		# save the stats for the Tukey table
		
		# NOTE: this if deals with the fact that in some cases there are so many NAs that
		#       nothing would be entered here.
		if (length(Comparisons$Term[,1])==0) {
			lastVsPrevious <- rbind(lastVsPrevious, NA)
		} else {	
			lastVsPrevious <- rbind(lastVsPrevious, round(Comparisons$Term[length(Comparisons$Term[,1]),], digits=3))
		}


		if (length(Comparisons$Term[length(Comparisons$Term[,1])-1,])==0) {
			lastVsPenultimate <- rbind(lastVsPenultimate, NA)
		} else {
	 		lastVsPenultimate <- rbind(lastVsPenultimate, round(Comparisons$Term[length(Comparisons$Term[,1])-1,], digits=3))
		}
	}
}


#******************************************************
#*********CREATING & PRINTING THE RESULT TABLES********

#this creates a table with the F stats that were saved: F stat, Df, p value
#cbind is to bind the column vectors; the t(t) is to transpose the data so that they are columns instead of rows
fTable <- cbind(format(t(t(FstatCourse)), digits=2),t(t(DfCourse)), t(t(pValCourse)))

# pick out p values less than .05 from the anova results
len = length(fTable[1,])
these = (!is.na(as.numeric(fTable[,len]))) & as.numeric(fTable[,len])<.05
fTable[these,len]=paste(fTable[these,len],"*",sep="") # add asterisks to cases less than .05

# change p values of 0.000 to be labeled as <.001
tmpZero = fTable[,len]<.001
fTable[tmpZero,len] = "<.001***"

#adds labels to fTable
rownames(fTable) <- c(courseCodes)
colnames(fTable) <- c("      F Statistic", "             Df  ", "      p value ") #added all the spaces for alignment

#this creates a table with the post-hoc stats for first pairwise comparison (1302 v. 1301)
CompTable1 <- lastVsPrevious 
rownames(CompTable1) <- c(courseCodes[numTermsAll>1])
colnames(CompTable1) <- c("Tukey HSD Diff", "Lower Bound", "Upper Bound", "Adjusted p value")
CompTable1onlySig <- format(CompTable1, digits=3)

# pick out p values less than .05 from the Tukey results
lenComp = length(CompTable1onlySig[1,])
theseComp = (!is.na(as.numeric(CompTable1onlySig[,lenComp]))) & as.numeric(CompTable1onlySig[,lenComp])<.05
CompTable1onlySig[theseComp,lenComp] = paste(CompTable1onlySig[theseComp,lenComp],"*", sep="") # add asterisks to cases less than .05

# change p values of 0.000 to be labeled as <.001
tmpZero = as.numeric(CompTable1onlySig[,lenComp])<.001
CompTable1onlySig[tmpZero,lenComp] = "<.001***"

#this creates a table with the post-hoc stats for second pairwise comparison (1302 v 1204)
CompTable2 <- lastVsPenultimate 
rownames(CompTable2) <- c(courseCodes[numTermsAll>1])
colnames(CompTable2) <- c("Tukey HSD Diff", "Lower Bound", "Upper Bound", "Adjusted p value")
CompTable2onlySig <- format(CompTable2, digits=3)

# pick out p values less than .05 from the Tukey results
lenComp2 = length(CompTable2onlySig[1,])
these2 = (!is.na(as.numeric(CompTable2onlySig[,lenComp2]))) & as.numeric(CompTable2onlySig[,lenComp2])<.05
CompTable2onlySig[these2,lenComp2] = paste(CompTable2onlySig[these2,lenComp2],"*", sep="") # add asterisks to cases less than .05

# change p values of 0.000 to be labeled as <.001
tmpZero = as.numeric(CompTable2onlySig[,lenComp2])<.001
CompTable2onlySig[tmpZero,lenComp2] = "<.001***"

#this set of commands prints the result tables on graphics plots
#dev.new() creates a new graphic each time so the plots don't overwrite each other
#cex.main sets the font size of the main title, relative to the default size of '1'
dev.new()
textplot(fTable, valign=c("top"), halign=c("center"), hadj=.8)
title(main="Cincinnati Mid-Term Grades by Term\n(Spring 2012 - Summer 2013): \n ANOVA Summary", cex.main=0.7) #change campus/program

dev.new() 
textplot(CompTable1onlySig, halign=c("right"),valign=c("top"),hadj=.8)
title(main="Cincinnati Mid-Term Grades \n Summer 2013 v Spring 2013: \n Tukey HSD Summary", cex.main=0.9) #change campus/program

dev.new()
textplot(CompTable2onlySig, halign=c("right"),valign=c("top"),hadj=.8)
title(main="Cincinnati Mid-Term Grades \n Summer 2013 v Winter 2012: \n Tukey HSD Summary", cex.main=0.9) #change campus/program
