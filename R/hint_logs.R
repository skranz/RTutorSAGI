write.hint.log = function(df, out.file = "hints.Rmd") {
  restore.point("write.hint.log")
  ps.names = unique(df$ps.name)

  txt = NULL
  ps.name = ps.names[1]
  for (ps.name in ps.names) {
    txt = paste0(txt,"\n## Problem Set ", ps.name,"")
    ps.df = df[df$ps.name == ps.name,]
    chunk.inds = unique(ps.df$chunk)
    chunk = chunk.inds[1]
    for (chunk in chunk.inds) {
      chunk.df = ps.df[ps.df$chunk == chunk,]
      chunk.df = chunk.df %>%
        group_by(user) %>%
        mutate(user.hints = n(), new.user = (1:n() == 1)) %>%
        mutate(user.label = ifelse(!new.user,"",
          paste0("\n********************* User with ", user.hints, " hints ********\n")
        )) %>%
        ungroup() %>%
        arrange(desc(user.hints))


      txt = paste0(txt,"\n### ", chunk.df$chunk.name[1], " ",
        chunk.df$total.hints[1], " hints, ", chunk.df$students.with.hint[1], " students")
      txt = paste0(txt,"\n**************************************\n```{r \"solution_", chunk.df$chunk.name[1], " (",chunk.df$total.hints[1]," total hints)\"}\n",
        chunk.df$sol.txt[1],"\n```\n**************************************")

      txt = paste0(txt,"\n",paste0(collapse="\n", chunk.df$user.label,
        "\n```{r \"", chunk.df$chunk.name[1], "-", seq_len(NROW(chunk.df)), "\"}\n",
        chunk.df$code,"\n````\nHint:\n", chunk.df$hint.txt,"\n\n(",chunk.df$hints.later, " hints and ", chunk.df$checks.later, " checks afterward)")
      )
    }
  }
  writeLines(txt, out.file)
  cat("\nWrote hint report to ", out.file)
}

#' Run all submissions versions were student asked for hint
#' and collect hint message
#'
#' WARNING: Only run in a secure environment. Arbitrary student code
#' can be executed.
#'
#' Return
create.sub.li.hints = function(sub.li, rps.dir=getwd(), ps.names=NULL) {
  if (is.null(ps.names))
    ps.names = sapply(sub.li, function(sub) sub$ps.name) %>% unique()

  li = bind_rows(lapply(ps.names, function(ps.name) {
    res = try(create.sub.li.hints.for.ps(sub.li, ps.name,rps.dir))
    if (is(res,"try-error")) return(NULL)
    return(res)
  }))

}


create.sub.li.hints.for.ps = function (sub.li, ps.name, rps.dir=getwd())
{
  restore.point("create.sub.li.hints.for.ps")
  oldwd = getwd()
  setwd(rps.dir)

  ps = init.ps(ps.name)
  if (!ps$rps$has.sol) {
    stop(paste0("The rps file for problem set ", ps.name, " has no sample solution."))
  }

  subs = filter.ps.sub.li(sub.li, ps.name)
  res = analyse.subs(subs,no.summary=TRUE,rps.dir = getwd())

  err.df = res$err.df
  hint.df = res$hint.df
  log.df = bind_rows(err.df, hint.df)

  first.solve.df = res$first.solve.df %>%
    semi_join(log.df, by=c("ps.name","stud.name","chunk"))
  df = bind_rows(log.df, first.solve.df) %>%
    arrange(ps.name, chunk, stud.name, time) %>%
    mutate(
      inside = lead(user) == user & lead(chunk) == chunk,
      before.hint = lead(type) == "hint" & inside
    ) %>%
    group_by(ps.name, chunk, stud.name) %>%
    mutate(
      checks.later = sum(type=="check_chunk")-cumsum(type=="check_chunk"),
      hints.later = sum(type=="hint")-cumsum(type=="hint")-1
    ) %>%
    ungroup() %>%
    filter(before.hint & type=="check_chunk") %>%
    group_by(ps.name, chunk) %>%
    mutate(
      total.hints = n(),
      students.with.hint = n_distinct(stud.name)
    ) %>%
    ungroup() %>%
    mutate(
      code = gsub(".PH_._","___",code, fixed=TRUE)
    )

  df$hint.txt = ""
  df$sol.txt = ""

  set.ps(ps)
  chunk.ind = 1
  n = NROW(ps$cdt)
  hint.txt = rep("", n)
  cdt = ps$cdt
  ps$cdt$sol.txt

  copy.into.env(dest = ps$ps.baseenv, source = list(
    install.packages = function(...) {
      stop("Please don't call the function install.packages in your RTutor problem set. To load an already installed package, use the function library. To install packages, use a separate R script in which you write and run the install.packages command.")
    }
  ))


  for (chunk.ind in 1:n) {
    rows = which(df$chunk == chunk.ind)
    for (row in rows) {
      restore.point("kmklsdmlksmdlksmd")
      stud.code = df$code[row]
      res = try(check.chunk(chunk.ind = chunk.ind, stud.code = stud.code),silent = TRUE)
      #copy.into.env(ps$stud.env, .GlobalEnv)
      df$hint.txt[row] = merge.lines(capture.output(try(hint(),silent = TRUE)))
      df$sol.txt[row] = ps$cdt$sol.txt[[chunk.ind]]
      df$chunk.name[row] = ps$cdt$chunk.name[[chunk.ind]]

    }
    restore.point("kmklsdmlksmdlksmd")
    stud.code = ps$cdt$sol.txt[[chunk.ind]]
    check.chunk(chunk.ind = chunk.ind, stud.code = stud.code)
  }
  setwd(oldwd)
  return(df[,c("ps.name", "time","user","chunk.name","chunk", "e.ind","code","hint.txt","sol.txt","task.txt", "checks.later","hints.later","total.hints","students.with.hint","message")])
}

filter.ps.sub.li = function(sub.li, ps.name) {
  is.ps = sapply(sub.li, function(sub) sub$ps.name == ps.name)
  sub.li[is.ps]

}
