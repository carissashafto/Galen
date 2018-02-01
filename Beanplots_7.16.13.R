##this script creates beanplots for each course in the file
##each page of plots shows the mean final or mid-term grade for a single course for the last 5 academic terms

require(beanplot)

#subsets the data to only use Theory courses; don't believe this works...
#TheoryData <- subset(Dataset, CourseType == "Theory")

#be sure to list all variables that you are interested in
#note that this step is not necessary, it just truncates the data set so that it is easier to view inside of R; 
##it works perfectly well to skip this step and run analyses using the entire data set, with all 100+ variables

MyVars <- names(Dataset) %in% c("GradeAnalysisID", "FinalNumericGrade", "Term")

#the data set "Truncated" only contains the variables that I am going to use in my analyses
Truncated <- Dataset[MyVars]

sortedData <- Truncated[order(Truncated$GradeAnalysisID),]

#set Term to be a factor
sortedData$Term <- as.factor(sortedData$Term)

courses <- sortedData$GradeAnalysisID
courseCodes <- levels(courses)
numCourseCodes <- length(courseCodes)

Figure <- NULL

# Open a new default device to be saved as pdf
pdfPath="filename"  #always change file name
pdf(file=pdfPath, height=9, width=7.5)


for (j in seq(numCourseCodes)){
  
	# select the subset of the data that we are interested in (this course)
	thisCourse <- which(sortedData$GradeAnalysisID %in% courseCodes[j])   
	thisCourseData <- sortedData[thisCourse,]

	terms <- thisCourseData$Term
	Terms <- levels(terms)	
	numTerms <- length(Terms)
	
		# select the subset of the data that we are interested in 
		## (only courses with >1 term of data)
		if (numTerms > 1) {

		# Create the subplots; be sure to change the variable names to what you want them to be
		Figure[j] <- beanplot(FinalNumericGrade~Term, data=thisCourseData, main=courseCodes[j], 
		xlab="Academic Term", ylab="Final Grade Average", varwidth=TRUE, ylim=c(0, 120),
		col = c("light gray", "blue"), border = "black", bw="nrd0")
	
		}
		
}
dev.off()

