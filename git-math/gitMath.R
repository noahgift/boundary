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
	sargs <- sprintf(" %s | awk '{print $1}'", filename)
	lines <- system2("wc",sargs, stdout=TRUE)
	#Only give line counts if the file exists
	if (length(lines) > 0){
			lines <- as.numeric(lines)
		}
	else{
		lines <- NA 
	}
	return(lines) 

}

#Generates churned file list for git repository via git log shell command
#To query churn by time range pass in i.e. "1 month ago"
gitchurn <- function(git_date_range) {
	if(missing(git_date_range)){
		sargs <- "log --all -M -C --name-only --format='format:' | grep -v '^$'" 
	}
		else{
		sargs <- sprintf("log --all -M -C --name-only --format='format:' --since='%s' | grep -v '^$'", git_date_range)
	} 
	churnoutput <- system2("git", sargs, stdout=TRUE, stderr=TRUE)
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
churn <- function(git_date_range) {
	if(missing(git_date_range)){
		churnfiles <- gitchurn()
	}
		else{
		churnfiles <- gitchurn(git_date_range)
	}
	gsmatrix <- churnhelper(churnfiles)
	return(gsmatrix)
}

#Gets linescounts from matrix of source code files
mklinecount <- function(churnmatrix) {
	clcmatrix <- sapply(row.names(churnmatrix),linecount)
	#colnames(clcmatrix) <- c("linecount")
	return(clcmatrix)
}

#Merges linecount and code churn matrix
churnline <- function (cmatrix, lmatrix){
	churnlinematrix <- merge(cmatrix, lmatrix, by = "row.names", all = TRUE)
	return(churnlinematrix)
}

#Generates churn/line matrix and returns merged matrix
mkchurnline <- function () {
	cmatrix <- churn()
	lmatrix <- mklinecount(cmatrix)
	clcmatrix <- churnline(cmatrix, lmatrix)
	clcmatrix <- na.omit(clcmatrix)
	colnames(clcmatrix) <- c("files", "churn", "linecount")
	return(clcmatrix)
}

churnplot <- function() {
	plot(churn(), ylab="file churn", xlab="file count")
}

churnlineplot <- function () {
	res <- mkchurnline()
	plot(res$churn, res$linecount)
}

