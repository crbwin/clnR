
install.packages("roxygen2")
install.packages("devtools")
devtools::create("clnR2")

devtools::document()

setwd("./clnR2")
devtools::document()


# initiate the upstream tracking of the project on the GitHub repo
git remote add origin https://github.com/crbwin/clnR.git

# pull all files from the GitHub repo (typically just readme, license, gitignore)
git pull origin master

# set up GitHub repo to track changes on local machine
git push -u origin master
