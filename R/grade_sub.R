# Grade submitted files in .sub format

grade.log = as.environment(list(current=NULL, all=NULL, file="grade_log.txt"))

examples.grade.sol = function() {
  base.dir = "D:/lehre/empIOUlm/rtutor/ps2019"
  setwd(base.dir)
  grade.sol()
}


grade.subs = function(sub.li, base.dir=getwd(), grade.dir = paste0(base.dir,"/grades")) {
  restore.point("grade.subs")
  setwd(base.dir)

  grade.total.points(sub.li)
  log.df = import.logs(sub.li)

  df = filter(log.df, stud.name == log.df$stud.name[1], ps.name ==log.df$ps.name[1], type != "check_chunk")
  wt.df = log.df %>% group_by(ps.name, stud.name) %>%
    summarise(mins = estimate.working.time(times=time, break.min=20)) %>% ungroup()

  write.csv(wt.df, file = paste0(grade.dir,"/estimated work time.csv"), row.names = FALSE)

  cat("Files have been written to ", grade.dir)
}

estimate.working.time = function(times=df$time, break.min = 10) {
  restore.point("estimate.working.time")
  times = as.numeric(as.POSIXct(times))
  times = sort(times)
  secs = diff(times)
  mins = secs / 60
  mins
  mins[mins >= break.min] = 0
  sum(mins)
}



write.grade.log = function(msg, console="cat") {
  grade.log$current = c(grade.log$current,msg)
  grade.log$all = c(grade.log$all,msg)
  if (console=="cat") {
    cat("\n",msg)
  }  else if (console=="warning") {
    warning(msg)
  }
  writeLines(grade.log$all, grade.log$file)
}

grade.total.points = function(sub.li, grade.dir = paste0(getwd(),"/grades")) {
  restore.point("grade.total.points")

  if (!dir.exists(grade.dir))
    dir.create(grade.dir)

  # Grade all rows of sub.df separately
  li = lapply(sub.li, function(sub) {
    res = sub$total
    res$user.name = sub$stud.name
    res
  })
  sub.df = bind_rows(li)
  sub.df$user.name = correct.stud.names(sub.df$user.name)


  sub.df = mutate(group_by(sub.df, ps.name),
    time.rank = rank(finished.time, ties.method="min"),
    perc.time.rank = 100*time.rank / length(time.rank),

    success.rank = rank(-points, ties.method="min"),
    hints.rank = rank(num.hints, ties.method="min")
  )

  sub.df = arrange(sub.df,user.name)
  write.csv(sub.df,paste0(grade.dir,"/ps_points.csv"),row.names = FALSE)

  temp.df = sub.df %>% group_by(ps.name) %>% summarise(points = first(max.points))
  total.points = sum(temp.df$points)

  tot.df = sub.df %>% group_by(user.name) %>% summarise(
    max.points = total.points,
    points = sum(points),
    share.solved = points / max.points,
    num.hints = sum(num.hints),
    num.ps = n()
  )

  write.csv(tot.df,paste0(grade.dir,"/total_points.csv"),row.names = FALSE)

  invisible(list(sub.df=sub.df, tot.df=tot.df))
}




