# Functions to directly check submitted Rmd files
# Not yet working

NOT.WORKING.check.submitted.rmd = function(sub.df, run.dir=paste0(base.dir,"/run"), res.dir = paste0(base.dir,"/runzip"),base.dir = getwd(),copy.zip = TRUE,ask = TRUE, verbose=TRUE) {

  restore.point("check.submitted.rmd")

  if (NROW(sub.df) == 0) {
    cat("\nsub.df is empty")
    return(NULL)
  }

  if (ask) {
    cat("\nAre you sure that you want to run and check ", NROW(sub.df), "  submitted rmd files on your own computer? Be sure that the rmd files from your students don't contain malicious code. You can take a look the source files. Furthermore, I would strongly recommend to run the files  as a system user that has limited priviliges on your computer (not with a user that has administrator rights). The system user needs, however, read write access to the run.dir =",run.dir, " and the res.dir = ", res.dir,"\n")
    res = readline(prompt="Type y and press Enter if you want to continue: ")
    if (!tolower(res)=="y") {
      cat("\nCancelled solving .")
      return(invisible(NULL))
    }
  }

  i = 1
  for (i in seq_along(sub.df$rmd.file)) {
    su = sub.df[i,]

    # copy files to run.dir
    file.copy(from = su$rmd.file,to = paste0(run.dir,"/",su$rmd.org.file),overwrite = TRUE)
    file.copy(from = su$log.file,to = paste0(run.dir,"/",su$log.org.file),overwrite = TRUE)
    file.copy(from = su$ups.file,to = paste0(run.dir,"/",su$ups.org.file),overwrite = TRUE)

    # check problem set
    setwd(run.dir)
    cat("\nCheck problemset ", su$ps.name, " of ", su$stud.name,"\n")

    new.sub = try(grade.ps(ps.name=su$ps.name, stud.path=run.dir, stud.short.file=su$rmd.org.file,reset = TRUE,user.name = su$user.name))

    if (!identical(oug$num.success, nug$num.success)) {
      write.grade.log(paste0("Recheck ", su$ps.name, " of ", su$stud.name,"\n",
        "num.success has changed from ", oug$num.success, " to ", nug$num.success))
    } else {
      cat("\nno change in num.success for ", su$ps.name, " of ", su$stud.name,"\n")
    }

    # Zip solution and copy to zip.dir
    zip.name = zip.solution(dir = run.dir, ask=FALSE)

    if (copy.zip) {
      new.zip = paste0(su$zip.dir,"/",basename(su$zip))
      file.copy(from=zip.name, to = new.zip, overwrite=TRUE)
      cat("\nZIP saved in ",new.zip)
    }
  }

}
