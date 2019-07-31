example.write.chunk.logs = function() {
  base.dir = "D:/libraries/RTutor/ps2019"
  setwd(base.dir)
  sub.li = load.moodle.subs(warn=FALSE)
  write.chunk.logs(sub.li, rps.dir = "org_ps")

}

#' Creates for each problem set and each chunk
#' an R file that protocols the solution attempts by students.
#'
#' See README.md for details
write.chunk.logs = function(sub.li, logs.dir = "chunk_logs", rps.dir="org_ps") {
  restore.point("write.chunk.logs")

  sub.li = load.moodle.subs(warn=FALSE)
  res = analyse.subs(sub.li,no.summary=TRUE,rps.dir = rps.dir)
  solve.df = res$solve.df
  err.df = res$err.df
  hint.df = res$hint.df
  log.df = bind_rows(err.df, hint.df)

  first.solve.df = res$first.solve.df %>%
    semi_join(log.df, by=c("ps.name","stud.name","chunk"))


  log.df = bind_rows(log.df, first.solve.df) %>%
    arrange(ps.name, chunk, stud.name, time)

  # df = log.df
  # df = filter(df, ps.name=="Intro", stud.name == "Tim_Schneider", chunk==4, ok==TRUE)

  ps.names = unique(log.df$ps.name)
  ps.name = ps.names[1]
  chunk = 4
  for (ps.name in ps.names) {
    dir = file.path(logs.dir,ps.name)
    if (!dir.exists(dir))
      dir.create(dir, recursive=TRUE)
    df = log.df[log.df$ps.name == ps.name,]
    chunks = unique(df$chunk)
    for (chunk in chunks) {
      write.chunk.log(log.df, ps.name, chunk, solve.df=solve.df, dir=dir)
    }
  }
  invisible(res$sum.df)
}

write.chunk.log = function(log.df, ps.name, chunk, solve.df, dir=NULL, verbose=TRUE) {
  #restore.point("write.chunk.log")
  rows = log.df$ps.name == ps.name & log.df$chunk == chunk
  df = log.df[rows,]
  df$time = as.POSIXct(df$time)

  df = df %>%
    arrange(stud.name,time)

  dupl = duplicated(select(df, chunk, stud.name,type, ok)) & df$ok  & df$type == "check_chunk"
  df = df[!dupl,]

  rows = solve.df$ps.name == ps.name & solve.df$chunk == chunk
  solve.df = solve.df[rows,]

  df$code = gsub("\n# enter your code here ...","",df$code,fixed=TRUE)

  li = split(df, df$stud.name)
  all.txt = lapply(li, function(udf) {
    stud.name = udf$stud.name[[1]]
    n = NROW(udf)
    txt = rep("", n)
    txt[udf$type == "hint"] = " asked for hint."
    rows = udf$type == "check_chunk" & udf$ok == FALSE
    txt[rows] = paste0("\n",udf$code[rows])
    num.fails = sum(rows)
    rows = udf$type == "check_chunk" & udf$ok == TRUE
    txt[rows] = " solved succesfully!"

    time.diff = c("",paste0("\n# *** ",format(difftime(udf$time[-1],udf$time[-n])), " later... "))
    txt = paste0(time.diff,txt)

    us = solve.df[solve.df$stud.name == stud.name,]
    paste0("\n\n# NEW USER ",ifelse(true(us$solved[1]),"solved"," unsolved"), " after ",num.fails, " failures **********************************\n",merge.lines(txt))
  })

  res = paste0(all.txt, collapse="")

  chunk.name = df$chunk.name[1]
  rows =  df$ok == FALSE & df$type == "check_chunk"
  num.err = sum(rows)
  num.user = n_distinct(df$stud.name[rows])
  num.hint = sum(df$type == "hint")
  num.solved = sum(solve.df$solved)
  num.failed = sum(!solve.df$solved)

  res = paste0("# ",ps.name, " ", chunk.name, "\n# ",num.solved, " users solved and ", num.failed, " not.\n# Failed checks: ",num.err, " Hints: ",num.hint, res)

  if (!is.null(dir)) {

    file = paste0(chunk.name," (e ",num.err," u ",num.user," h ",num.hint, ").R")
    writeLines(res, file.path(dir, file))
    if (verbose) {
      cat("\nWrote ", file.path(dir, file))
    }
  }
  invisible(res)
}
