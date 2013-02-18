#R Code Metrics Library about a Git Repository
#Relates to 'Use of Relative Code Churn Measures to Predict System Defect Density'

#Usage example:
#source('/Users/noah/src/boundary/git-math/gitMath.R')
#gs <- churn()
#OR
#churnplot()

#TO DO
#1.  Consider what to do with files that have never changed (1 value)
#2.  Break out a new matrix for each file extension

#Get linecount from file
linecount <- function(filename){
	args <- sprintf("%s | awk '{print $1}'", filename)
	lines <- system2("wc",args, stdout=TRUE, stderr=TRUE)
	return(as.numeric(lines))
}

#Generates churned file list for git repository via git log shell command
#To query churn by time range pass in i.e. "1 month ago"
gitchurn <- function(git_date_range) {
	if(missing(git_date_range))
		args <- "log --all -M -C --name-only --format='format:' | grep -v '^$'" 
		else 
		args <- sprintf("log --all -M -C --name-only --format='format:' --since='%s' | grep -v '^$'", git_date_range)
	churnoutput <- system2("git", args, stdout=TRUE, stderr=TRUE)
	return(churnoutput)
}

#Helper function generates frequency matrix of churned files
#Accepts the output of gitchurn
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
