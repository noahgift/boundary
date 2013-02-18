#R Code Metrics Library about a Git Repository
#Relates to 'Use of Relative Code Churn Measures to Predict System Defect Density'

#Usage example:
#source('/Users/noah/src/boundary/git-math/gitMath.R')
#churnfiles <- gitchurn()
#gitstats <- churn(churnfiles)
#plot(gitstats, ylab="file churn", xlab="file count")
#OR
#genplot()

#TO DO
#1.  Consider what to do with files that have never changed (1 value)

#Generates churned file list for git repository via git log shell command
gitchurn <- function() {
	args <- "log --all -M -C --name-only --format='format:' | grep -v '^$'"
	churnoutput <- system2("git", args, stdout=TRUE, stderr=TRUE)
	return(churnoutput)
}

#Generates frequency matrix of churned files
churn <- function(churnfiles) {
	gitstats <- as.matrix(table(churnfiles))
	colnames(gitstats) <- c("churn")
	return(gitstats)
}

genplot <- function() {
	churnfiles <- gitchurn()
	gstats <- churn(churnfiles)
	plot(gstats, ylab="file churn", xlab="file count")
}

