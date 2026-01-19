# SFR 605 Github Repository

This is the Github Repository for the University of Maine SFR 605 Course Modeling Ecologial Data.

## Setting up Git and R studio to access the exercises

All exercises will be available at http://github.com/rabramoff and assume that you will be working from within the RStudio editor with git installed. This requires a few steps:

0. Before installing RStudio, you'll want to make sure you have [installed R](http://cran.us.r-project.org/)
1. RStudio can be downloaded for free [here](https://posit.co/download/rstudio-desktop/)
2. You will need to install Git, which you can do by following the instructions at [RStudio Support - Version Control with Git and SVN](https://support.rstudio.com/hc/en-us/articles/200532077-Version-Control-with-Git-and-SVN).
3. You will need to make sure RStudio knows where git is installed. 
  + In RStudio click on Tools > Global Options > Git/SVN
  + Make sure the "Git executable" path is set to where git was installed 
  + Make sure "Enable version control interface" is checked
4. You will need to introduce yourself to git.
  + From RStudio click Tools > Terminal
  + To set your name, enter the following & hit Return: 
```
git config -–global user.name “your name”
```
  + To set your email, enter the following & hit Return: 
```
git config -–global user.email “your.email@gmail.com”
```
  + You can now close this Terminal window
  
The best way to get course exercises onto your computer is to click on the project pull-down menu in the top-right corner of RStudio and select "New Project...".  

Next, click "Version Control" and then "Git".

Enter the following address in the Repository URL: https://github.com/rabramoff/SFR605.git

You can optionally choose to name the folder something different than SFR605 (though be aware that many activities will assume this is the name of the folder) or have the folder saved somewhere other than your Home directory.

When you hit "Create Project" this will clone a copy of the git project from the github.com website.

If you anticipate making changes to a git project on GitHub then, instead of cloning from https://github.com/rabramoff/SFR605 you should go to that website and click on the "Fork" button in the top right corner. This requires that you have an account on GitHub, which is free and easy to set up. Once you fork the repository, this will make a copy to https://github.com/username/SFR605 where _username_ is your GitHub username, and you can use that as your Repository URL.

If you just can't get Git to work, you can download this activity from the same website, https://github.com/rabramoff/SFR605, by clicking on the "Download ZIP" button in the bottom right.

Finally, in R you can go to File > Open File and open the exercise of the day.

## Acknowledgements

Many thanks to Michael Dietze for making his learning materials open and encouraging others to use them - I have drawn heavily from his resources.
