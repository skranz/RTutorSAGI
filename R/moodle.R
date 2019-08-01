# Functions specialized for submissions that were downloaded
# as big zip files from Moodle assignments

example.moodle = function() {
  base.dir = "D:/libraries/RTutor/ps2019"
  setwd(base.dir)
  unpack.moodle.sub.zips(zip.dir = "moodle_zip", sub.dir="sub")
  sub.li = load.subs(sub.dir = "sub", stud.name.fun=moodle.stud.name.fun)
  grade.subs(sub.li=sub.li)
}

#' Load submission files that follow
#' Moodle's naming convention
load.moodle.subs = function(sub.dir="sub",stud.name.fun=moodle.stud.name.fun,...) {
  load.subs(sub.dir = sub.dir, stud.name.fun=stud.name.fun,...)
}



#' Takes assignment ZIPs with all students' solutions
#' and unpacks them into separate folders for each assignment
#'
#' @param zip.dir directory with big ZIP files from Moodle. Each ZIP file contains all submissions of one problem set
#' @param sub.dir directory into which sub files shall be extracted
unpack.moodle.sub.zips = function(zip.dir = "moodle_zip", sub.dir = "sub",  prefix="", postfix=".zip") {
  restore.point("unpack.moodle.sub.zips")

  # Extract big ZIP files
  # We have one big zip file per assignment
  # it contains all the zip files submitted
  # by students for this assignment


  bzips = list.files(zip.dir)
  # Example file name: "WiWi39-SS15-Assignment ps_1a-33751.zip"

  # Extract names of assignments
  ass = suppressWarnings(str.between(bzips,prefix,postfix))
  i = 1
  for (i in seq_along(ass)) {
    as = ass[i]
    as.dir = paste0(sub.dir,"/",as)
    if (!dir.exists(as.dir))
      dir.create(as.dir, recursive=TRUE)

    bzip = paste0(zip.dir,"/",bzips[i])
    unzip(bzip, exdir=as.dir)

    files = list.files(as.dir)
  }
  return(invisible(ass))
}

moodle.stud.name.fun = function(file, sub) {
  stud.name = str.between(basename(file),"----","--")
  Encoding(stud.name) <- "UTF-8"
  stud.name = mark_utf8(stud.name)
  stud.name = correct.stud.names(stud.name)
  stud.name
}
