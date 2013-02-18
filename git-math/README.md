Process to extract data:

1.  Connect to github repository:  http url
2.  Checkout repo
3.  Shell out to git log command
A.  Figure out easy way to put excludes 'grep -v' statements into a file
4.  Retrieve output and store in memory:

integer file-name

5.  Complexity metric:  Shell out to "X", in this case flog.
A.  Should figure out a way for this to be any complexity tool.
B.  For example, for Python code, I may want to use pylint


