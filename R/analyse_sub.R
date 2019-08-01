
examples.analyse.sub = function() {
  base.dir = "D:/lehre/empIOUlm/rtutor/ps2019"
  setwd(base.dir)
  sub.li = load.moodle.subs(warn=FALSE)
  res = analyse.subs(sub.li, rps.dir = "org_ps")
  saveRDS(res, "analysis.Rds")

  res = readRDS("analysis.Rds")
  sum.df = res$sum.df
  hint.df = res$hint.df
  err.df = res$err.df

  app = psImproveApp(sum.df, err.df, hint.df, sol.dir="new_rmd")

  viewApp(app)
}

#' Summarize number of errors and hints by
#' looking at the submission logs
#'
#' See README.md for usage
#'
#' @param sub.li A list of loaded submission files. See \code{\link{load.subs}}
#' @param rps.dir The directory in which you have all rps files of the problem sets (original versions)
#' @param just.summary if TRUE just return the chunk summary data frame. If FALSE (default) return a list that also contains several intermediate data frames.
#' @param no.summary if TRUE just return list without summary data frame (used by write.chunk.logs)
#' @param protracted.minutes Used to classify whether it took a long time, possible due to a break when gotten stuck for a student to solve the chunk. We measure the duration between the first failed attempt and the time the chunk is first solved. If this is longer (in minutes) than \code{protracted.minutes} (default = 30), we classify that it was protracted for the student to solve the chunk. The summary in sum.df counts for how many students it was protracted to solve the chunk.
analyse.subs = function(sub.li, rps.dir="org_ps",just.summary=FALSE, no.summary = FALSE, protracted.minutes = 30) {
  restore.point("analyse.subs")
  log.df = import.logs(sub.li)

  ps.names = unique(log.df$ps.name)
  cdt = bind_rows(lapply(ps.names,function(ps.name){
    rps = load.rps(file.path(rps.dir,ps.name))
    cdt = rps$cdt
    cdt$ps.name = ps.name
    cdt
  }))

  log.df = log.df %>%
    left_join(select(cdt, ps.name, chunk=chunk.ps.ind, task.txt, chunk.name), by=c("chunk", "ps.name")) %>%
    mutate(time = as.POSIXct(time))

  hint.df = log.df %>%
    filter(type=="hint")

  # err.df contains check codes calls
  # with wrong and non-empty code
  empty.codes = c(
    "\n# enter your code here ...\n",
    "\n# enter your code here ...\n\n\n# enter your code here ...\n"
  )
  err.df = log.df %>%
    filter(type=="check_chunk", !ok, code!=task.txt, !code %in% empty.codes)

  first.solve.df = log.df %>%
    filter(type=="check_chunk", ok) %>%
    group_by(ps.name, stud.name, chunk) %>%
    top_n(-1,time) %>%
    slice(1) %>%
    ungroup() %>%
    unique()

  first.err.df = err.df %>%
    group_by(ps.name, stud.name, chunk) %>%
    top_n(-1,time) %>%
    slice(1) %>%
    ungroup() %>%
    unique()

  solve.df = bind_rows(lapply(sub.li, function(sub) {
    sub$by.chunk %>%
      select( ps.name=ps.name,chunk = chunk.ps.ind,share.solved = share.solved,  ups.hints = num.hints) %>%
      mutate(stud.name = sub$stud.name,solved = share.solved == 100)
  }))


  solve.df = solve.df %>% left_join(
      select(first.solve.df,ps.name, stud.name, chunk, time.solved=time),
      by = c("ps.name","stud.name","chunk")
    ) %>%
    left_join(
      select(first.err.df,ps.name, stud.name, chunk, time.first.fail=time),
      by = c("ps.name","stud.name","chunk")
    ) %>%
    mutate(
      duration.sec = suppressWarnings(as.integer(time.solved)-as.integer(time.first.fail)),
      solved.in.log = !is.na(time.solved),
      protracted = is.true(duration.sec*60 > protracted.minutes)
    )


  # Note: first.solve.df has more rows than solve.df because it also
  # contains information about full task chunks that don't give any points

  #df = anti_join(first.solve.df, solve.df,  by = c("ps.name","stud.name","chunk"))

  #df = filter(solve.df, solved != solved.in.log)

  sub = sub.li[[1]]

  if (no.summary) {
    return(list(err.df=err.df, hint.df=hint.df, first.solve.df=first.solve.df, solve.df = solve.df))
  }

  #  group_by()

  hint.sum = hint.df %>%
    group_by(ps.name, chunk) %>%
    summarize(
      hint = n(),
      users.hint = n_distinct(user)
    ) %>%
    arrange(desc(hint))

  hint.sum


  err.sum = err.df %>%
    group_by(ps.name, chunk) %>%
    summarize(
      err = n(),
      users.err = n_distinct(user)
    ) %>%
    arrange(desc(err))

  err.sum

  solve.sum = solve.df %>%
    group_by(ps.name, chunk) %>%
    summarize(
      solved = sum(solved),
      unsolved = sum(!solved),
      protracted = sum(solved & protracted)
    )

  na.to.zero = function(x) {
    x[is.na(x)] = 0
    x
  }

  sum.df = log.df %>%
    select(ps.name, chunk, chunk.name) %>%
    unique() %>%
    left_join(solve.sum, by=c("chunk", "ps.name")) %>%
    left_join(err.sum, by=c("chunk", "ps.name")) %>%
    left_join(hint.sum, by=c("chunk", "ps.name")) %>%
    filter(!is.na(chunk)) %>%
    dplyr::mutate_if(is.numeric,na.to.zero) %>%
    arrange(-err,-hint) %>%
    filter(solved >0 | unsolved >0)

  if (just.summary)
    return(sum.df)

  list(sum.df = sum.df, err.df=err.df, hint.df = hint.df, solve.df = solve.df)
}


import.logs = function(sub.li) {
  li = lapply(sub.li, function(sub) {
    sub$log.df
  })
  bind_rows(li)
}

