# Grade submitted files in .sub format

grade.log = as.environment(list(current=NULL, all=NULL, file="grade_log.txt"))

examples.grade.sub = function() {
  base.dir = "D:/libraries/RTutor/ps2019"
  setwd(base.dir)
  sub.li = load.subs(sub.dir = "sub", stud.name.fun=moodle.stud.name.fun)
  grade.subs(sub.li=sub.li, grade.dir="grades")
}

#' Combine for each student the points from all problem sets and create
#' csv files with the total points.
#'
#' See README.md for usage
grade.subs = function(sub.li, grade.dir = "grades", rename.ps=NULL, delete.ps = NULL) {
  restore.point("grade.subs")

  res = grade.total.points(sub.li, rename.ps=rename.ps, delete.ps=delete.ps)
  log.df = import.logs(sub.li)

  df = filter(log.df, stud.name == log.df$stud.name[1], ps.name ==log.df$ps.name[1], type != "check_chunk")
  wt.df = log.df %>% group_by(ps.name, stud.name) %>%
    summarise(mins = estimate.working.time(times=time, break.min=20)) %>% ungroup()

  write.csv(wt.df, file = paste0(grade.dir,"/estimated work time.csv"), row.names = FALSE)
  print(as.data.frame(make.ps.stats(res$sub.df)))

  cat("\nFiles have been written to ", grade.dir)

  invisible(res$sub.df)
}

make.ps.stats = function(sub.df) {
  sub.df %>%
    group_by(ps.name) %>%
    summarize(
      ps.name = first(ps.name),
      students = n(),
      max.points = paste0(sort(unique(max.points)), collapse=" or "),
      all.correct = paste0(round((mean(points==max.points)*100),1),"%"),
      mean.solved = paste0(round((mean(share.solved)),1),"%"),
      min.solved = paste0(round((min(share.solved)),1),"%")
    )
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

sub.li.to.checked.sub.df = function(sub.li=NULL, rename.ps = NULL, delete.ps=NULL) {
  restore.point("sub.li.to.checked.sub.df")
  # Grade all rows of sub.df separately
  li = lapply(sub.li, function(sub) {
    res = sub$total
    res$user.name = sub$stud.name
    res
  })
  sub.df = bind_rows(li)
  sub.df$user.name = correct.stud.names(sub.df$user.name)

  for (i in seq_along(rename.ps)) {
    rows = which(sub.df$ps.name == rename.ps[i])
    if (length(rows)>0) {
      new.name = names(rename.ps)[i]
      cat(paste0("\nRename wrong problem set ", sub.df$ps.name[rows], " to ", new.name ," (from ", sub.df$user.name[rows],")"), collapse="")
      sub.df$ps.name[rows] = new.name
    }
  }
  for (i in seq_along(delete.ps)) {
    rows = which(sub.df$ps.name == rename.ps[i])
    if (length(rows)>0) {
      cat(paste0("\nDelete wrong named problem set ", sub.df$ps.name[rows] ," from ", sub.df$user.name[rows]), collapse="")
      sub.df = sub.df[-rows,]
    }
  }

  # Check if some problem sets were submitted twice
  sub.df = sub.df %>%
    group_by(user.name, ps.name) %>%
    arrange(desc(points)) %>%
    mutate(
      num.submit = n(),
      duplicate = (1:n() != 1)
    )

  dupl.user = unique(sub.df$user.name[sub.df$duplicate])

  for (user in dupl.user) {
    rows = which(sub.df$user.name == user)
    dupl.rows = which(sub.df$user.name == user & sub.df$duplicate)
    cat(paste0("\n\n",user, " has submitted the problem set ", paste0(sub.df$ps.name[dupl.rows], collapse=", "), " twice. Duplicates with fewer points are deleted.In total she submitted the problem sets ", paste0(sub.df$ps.name[rows], collapse=", ")))
  }
  cat("\n")
  sub.df = filter(sub.df, !duplicate)
  sub.df
}

grade.total.points = function(sub.li, grade.dir = paste0(getwd(),"/grades"), rename.ps=NULL, delete.ps = NULL) {
  restore.point("grade.total.points")

  if (!dir.exists(grade.dir))
    dir.create(grade.dir)

  sub.df = sub.li.to.checked.sub.df(sub.li, rename.ps=rename.ps, delete.ps = delete.ps)


  sub.df = mutate(group_by(sub.df, ps.name),
    time.rank = rank(finished.time, ties.method="min"),
    perc.time.rank = 100*time.rank / length(time.rank),

    success.rank = rank(-points, ties.method="min"),
    hints.rank = rank(num.hints, ties.method="min")
  )

  sub.df = arrange(sub.df,user.name)
  write.csv(sub.df,paste0(grade.dir,"/ps_points.csv"),row.names = FALSE)

  temp.df = sub.df %>% group_by(ps.name) %>% summarise(points = median(max.points))
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




