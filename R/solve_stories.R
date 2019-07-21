make.solve.stories = function(sub.li, stories.dir = "solve_stories") {
  base.dir = "D:/libraries/RTutor/ps2019"
  setwd(base.dir)
  sub.li = load.moodle.subs(warn=FALSE)
  res = analyse.subs(sub.li,no.summary=TRUE)
  err.df = res$err.df
  hint.df = res$hint.df
  log.df = bind_rows(err.df, hint.df)

  first.solve.df = res$first.solve.df %>%
    semi_join(log.df, by=c("ps.name","stud.name","chunk"))

  log.df = bind_rows(log.df, first.solve.df) %>%
    arrange(ps.name, chunk, stud.name, time)

  ps.names = unique(log.df$ps.name)
  ps.name = ps.names[1]
  chunk = 1
  for (ps.name in ps.names) {
    dir = file.path(stories.dir,ps.name)
    if (!dir.exists(dir))
      dir.create(dir, recursive=TRUE)
    df = log.df[log.df$ps.name == ps.name,]
    chunks = unique(df$chunk)
    for (chunk in chunks) {
      make.chunk.solve.story(log.df, ps.name, chunk, dir=dir)
    }
  }

}

make.chunk.solve.story = function(log.df, ps.name, chunk, dir=NULL, verbose=TRUE) {
  restore.point("make.chunk.solve.story")
  rows = log.df$ps.name == ps.name & log.df$chunk == chunk
  df = log.df[rows,]
  df$time = as.POSIXct(df$time)

  df = df %>%
    arrange(user,time)

  dupl = duplicated(select(df, chunk, user,ok)) & df$ok  & df$type == "check_chunk"
  df = df[!dupl,]


  df$code = gsub("\n# enter your code here ...","",df$code,fixed=TRUE)

  li = split(df, df$user)
  all.txt = lapply(li, function(udf) {
    restore.point("skfhdfhk")
    n = NROW(udf)
    txt = rep("", n)
    txt[udf$type == "hint"] = " asked for hint."
    rows = udf$type == "check_chunk" & udf$ok == FALSE
    txt[rows] = paste0("\n",udf$code[rows])
    rows = udf$type == "check_chunk" & udf$ok
    txt[rows] = " solved succesfully!"

    time.diff = c("",paste0("\n# *** ",format(difftime(udf$time[-1],udf$time[-n])), " later... "))
    txt = paste0(time.diff,txt)
    paste0("\n# NEW USER **********************************\n",merge.lines(txt))
  })

  res = paste0(all.txt, collapse="")

  chunk.name = df$chunk.name[1]
  rows =  df$ok == FALSE & df$type == "check_chunk"
  num.err = sum(rows)
  num.user = n_distinct(df$stud.name[rows])
  num.hint = sum(df$type == "hint")

  res = paste0("# ",ps.name, " ", chunk.name, "\n","# Errors: ",num.err," (",num.user," users) Hints: ",num.hint,"\n\n",res)

  if (!is.null(dir)) {

    file = paste0(chunk.name," (e ",num.err," u ",num.user," h ",num.hint, ").R")
    writeLines(res, file.path(dir, file))
    if (verbose) {
      cat("\nWrote ", file.path(dir, file))
    }
  }
  invisible(res)
}
