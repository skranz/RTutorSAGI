# RTutorSAGI: Analyse Submissions to Grade and Improve Problem Sets
 
**Author: Sebastian Kranz, Ulm University** 

This is a companion package to [RTutor](https://github.com/skranz/RTutor/). RTutor allows to create interactive R problem sets that students can solve at home or in the cloud (e.g. via https://rstudio.cloud/). 

Students can create submission files of their solutions (either by calling `make.submission` in the RStudio version, or by pressing the corresponding button in the shiny web version). 

This package helps teachers to handle the submissions, in particular for two tasks:

1. Grading: Combine the submissions of several students and several problem sets and automatically create csv files that show the total points achieved.

2. Problem Set Improvement: By default a submission file contains a log that contains the code from all attempts of a user to solve the tasks. This helps to understand where students systematically got stuck when solving the problem set and how they misunderstood particular tasks. One can use the insights to improve the problem set by adapting the hints or by changing problematic tasks. RTutorSAGI has functions that facilitate this process.

First some important notes.

## DISCLAIMER: NO WARRANTY
I wrote this R package for my own usage. While everybody is free to use it, I provide no warranty whatsoever.
While best to my knowledge the software works fine, I cannot rule out with 100% certainty that there may be a bug in the code such that points are incorrectly counted or incorrectly added up in some circumstances.

## Security

For security reasons the functions described below don't evaluate the code that students have submitted. For grading they rely on the statistics created on students' computers when `make.submission` was called. Nevertheless, the submission files from all students are loaded. While I don't know how (and did not find any specific warnings on Google) this potentially may open some attack vector. Also you may manually run students' code when improving your problem set and may overlook some code with undesired consequences.

For better security, you can analyse the submissions inside an [RStudio docker container](https://hub.docker.com/r/rocker/rstudio/) with restricted permissions.

## Installation

My packages are not hosted on CRAN (while CRAN is great, it takes a lot of time to maintain several packages there). You can install `RTutorSAGI` with all dependencies from my own Github based R repository as following:

```r
install.packages("RTutorSAGI",repos = c("https://skranz-repo.github.io/drat/",getOption("repos")))
```
Alternatively, you can directly install from Github using `devtools::install_github("skranz/RTutorSAGI")`.


## Loading submission

We first have to load all relevant submissions. A submission file that a student has created with `make.submission` has the file type `.sub`.

You can load a single submission file with the function `load.sub` (which is basically a wrapper around base R `load`):

```r
sub = load.sub("Max_Mustermann__Intro.sub")
```
The variable `sub` is an environment with several elements For example
```r
sub$rmd.code
```
contains the content of the student's solution Rmd file.

We typically don't want to analyse single submissions but bulk load all submission. For this purpose, you should put all submitted submission files into some directory and then use `load.subs` with a correct `stud.name.fun` as explained below. The directory can have subdirectories, e.g. for different problem sets. 

### Example using Moodle

At my university, we use [Moodle](https://moodle.org/) as course management system. My students solve the problem sets at home and then shall upload their submission files on Moodle. Moodle then allows me to bulk download for each problem set a big ZIP file with all submission as small zip files. 

The following code uses a simple `RTutorSAGI` convenience function to unpack all ZIPs from Moodle, which I stored in subdirectory `moodle_zip`

```r
# Make sure you are in the right working directory
getwd()
# Possible adapt via setwd()
unpack.moodle.sub.zips(zip.dir = "moodle_zip", sub.dir="sub")
```
The subdirectory `sub` then contains all submission files.

We now load all submissions into R:
```r
sub.li = load.subs(sub.dir = "sub", stud.name.fun=moodle.stud.name.fun)
```
When loading the submissions, we also want to assign the correct student name for each submission. While students shall specify a user name in the Rmd file that contains their solution, these specified user names may not be completely reliable. Instead, I want to use the student names from Moodle. Moodle encodes the student's name in the file name of the submission. Currently (Summer 2019) the format has the structure:

`Upload_MoodleTaskName----StudentName--OriginalFileName.sub`

The argument `stud.name.function` of `load.subs` is a function that takes the file name of the submission file and its loaded content `sub` as arguments and must return the student's name. For example, to extract the sudent name from the Moodle file format above, we could use the function:
```r
my.stud.name.fun = function(file, sub, ...) {
  stud.name = stringtools::str.between(basename(file),"----","--")
  stud.name
}
```
The function `moodle.stud.name.fun` does a little bit more in order to deal with UTF-8 encoding problems.

If no `stud.name.fun` is provided, we use by default the user.name specified in the problem set. But as mentioned, that might be unreliable.

Having loaded a list of all submissions `sub.li`, we can then procceed with grading or analysing the logs.

### Alternatives to Moodle

Of course, there are many other ways how you can create a directory with all submission files. There is absolutely no need to use Moodle. You can use any other course management system were students can upload files and you can download them. You could also write and host your own shiny app where students can make their submissions.

## Grading

If `sub.li` contains a loaded list of submissions, we can summarize total points by calling:

```r
grade.subs(sub.li=sub.li, grade.dir="grades")
```

It then generates in the `grade.dir` directory a csv file with total points of every student and another file with points per problem set. The tables also contain information about how many hints were asked. Hints have no impact on the points, however.

If submissions also contain log files, there is also a csv file that estimates work time on the problem sets.

What you do with the total points is up to you. Perhaps, you want to set a minimal point requirement to be allowed to participate in the final exam. Alternatively, you may make the points a small part of the total grade.

From my experience, even some small relevance for final grades provides nice incentives for students to work on the problem sets. I would not recommend to make the points from RTutor problem sets count much more than 10% of the total grade, however. I don't see an effective way how you can rule out cheating from students who copy the solutions from other students. The main reason for a grade optimizing student to work himself on the problem sets should be that it provides good preparation for the final exam.


Loading submissions and grading creates and appends to a log file `grading_log.txt` in your working directory. The file can give information about irregularities. Here is an example:
```
grade_log.txt

The following submissions have not the extension .sub and will be ignored:

 Upload_PS_Intro----Max_Mustermann--Max_Mustermann_Intro.Rmd
```

In my courses, it happens from time to time that students upload the wrong file, like the Rmd file instead of the submission file created with `make.submission`. This is noted in the log. If you are lenient, you could then write the student an email and ask the student to upload the correct file and then repeat the whole process.

## Analyse logs in order to improve problem sets

In general, you would like to put students some effort into solving the problem set and enjoy sweet success after some struggeling. But then sometimes students can get stuck badly in a way you did not intend.

When designing a problem set, it is hard to predict all the ways how students can get stuck. While automatic hints and tests should cover many cases, they are not perfect. Also sometimes you may want to write custom hints, but don't know all special cases a hint should cover.

The natural approach seems to have an iterative process, where one improves a problem set after seeing in detail how students got stuck.

Information about solution attempts is stored in the logs that are by default part of students' submission files. To convert the information into a more convenient format call:

```r
sub.li = load.subs(sub.dir="sub")
write.chunk.logs(sub.li, logs.dir = "chunk_logs",  rps.dir="org_ps")
```

Make sure that you store in the directory specified by `rps.dir` all rps files of the original (unimproved) versions of your problem sets. The call  to `write.chunk.logs` creates and fills the directory specified in `logs.dir` with subdirectories for each problem set. Each subdirectory contains an .R file for each chunk of the problem set that requires the user to enter code. An example file name is

`3_e (e 88 u 13 h 11).R`

This file contains a log of all failed solution attempts of chunk `3 e` in the problem set. The information in brackets of the file name tells us that `e=88` times the check failed. There were `u=13` users who at least once entered some wrong code. Also in total `h=11` times a user asked for a hint. So by scanning this info in the file names, you get some first idea of which chunks of a problem set might be problematic.

If you want to analyse these error and hint statistics in a nice R data frame, you can run
```r
res = analyse.subs(sub.li,rps.dir="org_ps")
sum.df = res$sum.df
```
and then look at `sum.df`.

### Example: Improving with an adaptive hint

For an example, of how the logs can help improving a problem set, consider the following following task from Excercise 3 e) of my Intro to R problem set.

> e) Let z be a variable that shows the first 100 square numbers, i.e. 1,4,9,... Then show z.

Here is the log of one user from `3_e (e 88 u 13 h 11).R`:

```r
# NEW USER **********************************


z=(seq(1:10))^2
z


# *** 26 secs later...  asked for hint.

# *** 58 secs later... 

z=1:10*1:10
z


# *** 65 secs later... 

z=(1:10)*(1:10)
z


# *** 23 secs later...  asked for hint.

# *** 32 secs later... 

z=(1:10)^2
z


# *** 22 secs later... solved succesfully!
```

Looking at the whole log file, I found that several students assigned `z=(1:10)^2` instead of `z=(1:100)^2`.

The sample solution was `z = 1:100 * 1:100`. The automatic hint for such a formula would have looked like this:
```
You have to assign a correct formula to the variable 'z'.
Here is a scrambled version of my solution with some
characters being hidden by ?:

 z = 1??00 * 1:?0?
```

When designing the problem set, I thought the that the automatic hint gave away too much information here and therefore specified a custom hint. The custom hint just showed the following message:
```
There is a simple one-line formula to compute the 100 first square numbers.
Just combine what you have learned in exercise 2 f) and in exercise 3 b).")
```

Based on the log files, I have now updated to an adaptive hint that provides more detailed information for this common mistake. In the solution file the chunk now has the following code:
```r
z = 1:100 * 1:100
#< hint
if (true(identical(z,1:10*1:10))) {
  cat("Huh, you made a common mistake. Read the task precisely.
  You shall assign to z the first 100 square numbers,
  not only all square numbers that are less or equal to 100")
} else if (true(length(z)!=100)) {
  cat("Your variable z must have 100 elements, but in your solution z has", length(z),"elements.")  
} else {
  cat("There is a simple one-line formula to compute the first 100 square numbers.
  Just combine what you have learned in exercise 2 f) and in exercise 3 b).
")
}
#>
z
```

The code in the hint block will be evaluated in an environment in which the student's code has been evaluated and in which all variables defined in earlier solved chunks are known.

The function `true` is a robust version of `isTRUE` that never throws an error, but simply returns `FALSE` if the expression cannot be evaluated. This is useful, because 
whether `z` exists in the hint environment depends on whether the student has defined it in her solution for the chunk or not. A normal call to `isTRUE` would throw an error if `z` does not exist.

Note that you and your students need at least version 2019.07.22 (yes my version numbers are just dates) of RTutor for those adaptive hints to work.

### Another Example: Adding conditional hint to automatic hint.

Let us look at another example. Here is a task from another problem set:

> Using the command `cbind`, generate the matrix X of explanatory variable whose first column consists of 1 and the second column consists of p.

Here is an excerpt from the chunk log:
```
# NEW USER **********************************


cbind(1,p)

# *** 29 secs later... 

x <- cbind(1,p)

# *** 13 secs later... 

x = cbind(1,p)

# *** 98 secs later...  asked for hint.
# NEW USER **********************************

# *** 5.983333 mins later... 

x= cbind(1,p)

# NEW USER **********************************


x=cbind(rep(1,T),p)


# ***  22 secs later... 

x=cbind(rep(1,T),p)


# *** 103 secs later...  asked for hint.
```

In the original problem set, only the automatic hint was shown. I wanted to keep it, since it provides valuable information for different mistakes. But it did not help users that mixed up `X` and `x`. Here is the modified chunk in the solution file:

```r
X = cbind(1,p)
#< add_to_hint
if (exists("x") & !exists("X")) {
  cat("It looks like you assigned the value to 'x' (lowercase), but you shall assign the value to 'X' (uppercase).")
}
#>
```
Using an `#<add_to_hint` blocks means that the automatic hint will still be always shown. The message from the adaptive custom hint will be added below.

## Note: Improvements of automatic tests with RTutor version 2019.07.22

Working through the logs I found some systematic cases were RTutor rejected seemingly correct solutions without helpful failure message to the students. If you analyse a log were students still had older version, a lot of problems may go away once everybody updates to the new RTutor version.


For example, I had the following task:
> Show the column names of `dat`.

```
    ```{r "1__c"}
    colnames(dat)
    #< hint
    display('Google for something like "R show column names of data frame" to find the function to show column names.')
    #>
    ```
```
There are surprisingly many ways to get the column names. Here is an excerpt from the chunk log:
```r
# NEW USER **********************************


names(dat)

# *** 223 secs later... 

names(dat)

# ***  18 secs later...  asked for hint.

# *** 109 secs later... 

names(dat)
# NEW USER **********************************


dat[]


# *** 2645 secs later... 

variable.names(dat)

# ***   25 secs later... 

variable.names(dat)

# ***   39 secs later...  solved succesfully!
# NEW USER **********************************

ls(dat)
# ...
```

If in a task a variable shall be assigned then by default every solution where that variable has the right values is accepted. In contrast if just a call like `colnames("dat")` is given in the sample solution then RTutor requires the user to use exactly that function `colnames`. Unfortunately, there were no informative automatic messages that made the student aware of this issue. In the new RTutor version, the user gets a better message if she got an equivalent solution with a different call. E.g. if she typed

```r
names(dat)
```
A check of the chunk now automatically returns the following message
```
Check chunk 1__c...Error: 
Ok, in chunk 1__c your command

    names(dat)

indeed yields the correct result. But in this task you shall learn
how to directly call the function 'colnames', like 

    colnames(dat)

Please change your code.
For a hint, type hint() in the console and press Enter.
```

I guess this change should avoid a lot of warranted frustration. (Even though it would not help if the user typed `ls(dat)`, as one user did, since this changes the ordering of columns.)

Another issue is that RTutor cannot handle well chunks in which the same variable is assigned twice. For example, recall the task already discussed above:

> e) Let z be a variable that shows the first 100 square numbers, i.e. 1,4,9,... Then show z.

Some students solved it with the following perfectly correct code:
```r
z=(1:100)
z=z*z
z
```
Unfortunately, RTutor does not consider the solution correct. It gets confused since `z` is assigned twice in the chunk. Even worse the old version of RTutor it did not provide a helpful error message.

(The reasons that RTutor cannot handle such code correctly are complicated. It has to due with the fact that RTutor tests by default separately each command in a given chunk of the sample solution. This means it has to match the commands from the student's code to the corresponding command in the sample solution. If `z` was assigned twice, old RTutor assumed that one of the two lines must be completely correct in itself instead of being correct only when run together.)

With the new RTutor version there is a strict requirement that a given variable name like `z` can only assigned once in a chunk. If it is assigned multiple times, the user now gets automatically an informative error message, like
```
Error: 
You have assigned 2 times a value to the variable z in your chunk.
While you might get the correct result in RStudio, RTutor can only check your result
if you assign a value to z only once in your chunk.

For a hint, type hint() in the console and press Enter.
```

## Workflow for improving problem sets

Here is a suggestion for a rough workflow to improve problem sets.

+ Wait after the solutions for all problem sets of your course have been submitted. Then create a new working directory (possible as an RStudio project) and copy all problem set `_sol.Rmd` files and `.rps` files into two the sub directories folders and `org_ps`, `new_ps`. Extract the solutions to a `sol` directory (see explanation further above).

+ Create all chunk logs with
```r
sub.li = load.subs(sub.dir="sub", rps.dir="org_ps")
write.chunk.logs(sub.li, logs.dir = "chunk_logs")
```

+ Set RStudio shortcuts:
  + On "Run Setup Chunk". Allows you to quickly recompile a changed problem set. This is at least the case if you follow the structure of all example problem sets where the first chunk (inside an ignore block) contains the code to create the problem set.
  + On RTutor's RStudio addin "Check Problemset" to quickly check the hints and tests for a student's solution.

+ Open the solution file of a problem set (starting with the first) in the subdirectory `new_ps`. Let us assume your problem set is call "myps".

+ Run the setup chunk to compile the problem set and then open the `myps_sample_solution.Rmd` file.

+ Now look at the chunk logs for the current problem set and open the logs with relatively many errors (or users or hints).

+ You may copy some alternative solutions that failed into the corresponding chunk of the `myps_sample_solution.Rmd` file. Then check the problem set, look at the error message and possible call `hint()`.

+ Possible adapt your `myps_sol.Rmd` file, recompile and check the sample solution with the problematic code again.

+ Repeat for all chunks and problem sets until you are happy with the new versions. 
