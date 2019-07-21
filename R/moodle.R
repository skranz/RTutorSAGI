# Functions specialized for submissions that were downloaded
# as big zip files from Moodle assignments

example.moodle = function() {
  base.dir = "D:/libraries/RTutor/ps2019"
  setwd(base.dir)
  sub.li = load.moodle.subs(warn=FALSE)
  grade.subs(sub.li=sub.li)
}

load.moodle.subs = function(base.dir=getwd(), prefix="",postfix=".zip", grade.dir = paste0(base.dir,"/grades"), zip.dir = paste0(base.dir,"/bigzip"), sub.dir=file.path(base.dir,"sub"), force.unpack=FALSE,...) {
  restore.point("grade.moodle.subs")
  setwd(base.dir)
  if (!dir.exists(sub.dir) | force.unpack) {
    res = unpack.moodle.sub.zips(base.dir, prefix = prefix, postfix=postfix, zip.dir = zip.dir, sub.dir=sub.dir)
  } else {
    cat("\nSkip unpacking of Moodle ZIP files since sub directory already exists and force.unpack==FALSE.\n")
  }
  sub.li = load.subs(sub.dir = sub.dir, stud.name.fun=moodle.stud.name.fun,...)
  sub.li
}



#' Takes assignment ZIPs with all students' solutions
#' and unpacks them into separate folders for each assignment
unpack.moodle.sub.zips = function(base.dir, prefix="", postfix=".zip", zip.dir = file.path(base.dir,"bigzip"), sub.dir = file.path(base.dir,"sub")) {
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
