
#' Load all submissions from a directory
#'
#' See README.md for usage
load.subs = function(base.dir=NULL, files=NULL, stud.name.fun=NULL, sub.dir = file.path(base.dir, "sub"), warn=TRUE) {
  restore.point("load.subs")

  if (is.null(files)) {
    #files = list.files(sub.dir,pattern = glob2rx("*.sub"),full.names = TRUE, recursive = TRUE)
    files = list.files(sub.dir,pattern = glob2rx("*.*"),full.names = TRUE, recursive = TRUE)
    wrong.files = files[tools::file_ext(files) != "sub"]
    if (length(wrong.files)>0) {
      files = setdiff(files, wrong.files)
      str = paste0("\nThe following submissions have not the extension .sub and will be ignored:\n\n ", basename(wrong.files), collapse=",\n ")
      write.grade.log(str,console = if (warn) "warning" else "cat")
    }
  }
  sub.li = lapply(files, load.sub, stud.name.fun=stud.name.fun)
  sub.li
}

#' Load a single submission file
#'
#' See README.md for usage
load.sub = function(file, stud.name.fun=NULL) {
  restore.point("load.sub.with.stud.name")

  load(file)

  if (!is.null(stud.name.fun)) {
    stud.name = stud.name.fun(file, sub)
    sub$stud.name = stud.name
  } else {
    sub$stud.name = stud$user.name
  }
  sub$log.df$stud.name = sub$stud.name
  sub$log.df$ps.name = sub$ps.name
  sub
}
