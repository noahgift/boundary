#Tweaked from here:  https://github.com/garybernhardt/dotfiles/blob/master/bin/git-churn
#Finds churn in git repo

git log --all -M -C --name-only --format='format:' "$@"\
 | sort | grep -v '^$' | grep -v 'CHANGELOG' | grep -v 'Rakefile'|\
  grep -v '*.md' | grep -v 'Gemfile'| grep -v '*.rdoc' | uniq -c |\
   sort | awk 'BEGIN {print "count\tfile"} {print $1 "\t" $2}' | sort -g
