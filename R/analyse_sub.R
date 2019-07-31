
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
analyse.subs = function(sub.li, base.dir=getwd(), rps.dir=file.path(base.dir,"ps"), no.summary = FALSE) {
  restore.point("analyse.subs")
  setwd(base.dir)
  log.df = import.logs(sub.li)

  ps.names = unique(log.df$ps.name)
  cdt = bind_rows(lapply(ps.names,function(ps.name){
    rps = load.rps(file.path(rps.dir,ps.name))
    cdt = rps$cdt
    cdt$ps.name = ps.name
    cdt
  }))

  log.df = log.df %>%
    left_join(select(cdt, ps.name, chunk=chunk.ps.ind, task.txt, chunk.name), by=c("chunk", "ps.name"))

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

  solve.df = bind_rows(lapply(sub.li, function(sub) {
    sub$by.chunk %>%
      select( ps.name=ps.name,chunk = chunk.ps.ind,share.solved = share.solved,  ups.hints = num.hints) %>%
      mutate(stud.name = sub$stud.name,solved = share.solved == 100)
  }))
  solve.df = solve.df %>% left_join(
      select(first.solve.df,ps.name, stud.name, chunk, time.solved=time),
      by = c("ps.name","stud.name","chunk")
    ) %>%
    mutate(solved.in.log = !is.na(time.solved))

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
      num_hint = n(),
      users_hint = n_distinct(user)
    ) %>%
    arrange(desc(num_hint))

  hint.sum


  err.sum = err.df %>%
    group_by(ps.name, chunk) %>%
    summarize(
      num_err = n(),
      users_err = n_distinct(user),
      #err.codes = list(combine_errors(user, code))
    ) %>%
    arrange(desc(num_err))

  err.sum

  na.to.zero = function(x) {
    x[is.na(x)] = 0
    x
  }

  chunk.sum = log.df %>%
    select(ps.name, chunk, chunk.name) %>%
    unique() %>%
    left_join(hint.sum, by=c("chunk", "ps.name")) %>%
    left_join(err.sum, by=c("chunk", "ps.name")) %>%
    filter(!is.na(chunk)) %>%
    dplyr::mutate_if(is.numeric,na.to.zero) %>%
    arrange(-num_hint, -num_err)

  list(sum.df = chunk.sum, err.df=err.df, hint.df = hint.df, solve.df = solve.df)
}


import.logs = function(sub.li) {
  li = lapply(sub.li, function(sub) {
    sub$log.df
  })
  bind_rows(li)
}

