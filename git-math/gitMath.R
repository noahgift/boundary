#R Code Metrics Library about a Git Repository
#Relates to 'Use of Relative Code Churn Measures to Predict System Defect Density'

#Usage example:
#source('/Users/noah/src/boundary/git-math/gitMath.R')
#gs <- churn()
#OR
#churnplot()

#TO DO
#1.  Consider what to do with files that have never changed (1 value)

#Generates churned file list for git repository via git log shell command
gitchurn <- function() {
	args <- "log --all -M -C --name-only --format='format:' | grep -v '^$'"
	churnoutput <- system2("git", args, stdout=TRUE, stderr=TRUE)
	return(churnoutput)
}

#Helper function generates frequency matrix of churned files
churnhelper <- function(churnfiles) {
	gitstats <- as.matrix(table(churnfiles))
	colnames(gitstats) <- c("churn")
	return(gitstats)
}

#Returns code statistics matrix
churn <- function() {
	churnfiles <- gitchurn()
	gsmatrix <- churnhelper(churnfiles)
	return(gsmatrix)
}

churnplot <- function() {
	plot(churn(), ylab="file churn", xlab="file count")
}
