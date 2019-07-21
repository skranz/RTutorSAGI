
correct.stud.names = function(txt) {
  restore.point("correct.stud.names")

  txt = gsub("Ã¤","ä",txt,fixed=TRUE)
  txt = gsub("Ã¶","ö",txt,fixed=TRUE)

  txt
}
