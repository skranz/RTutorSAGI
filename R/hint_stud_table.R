examples.hint.templates = function() {
  base.dir = "D:/lehre/empIOUlm/rtutor/ps2019"
  setwd(base.dir)
  sub.li = load.moodle.subs(warn=FALSE, max.files=10)


  res = analyse.subs(sub.li, rps.dir = "org_ps",no.summary = TRUE)

  saveRDS(res, "res.Rds")

  res = readRDS("res.Rds")
  err.df = res$err.df
  es = err.sol.table(err.df, rps.dir="org_ps")
  saveRDS(es, "err_sol.Rds")

  es = readRDS("err_sol.Rds")

  hs = hint.stud.table(es,min.users = 1)
  hs
}

call.name = function(call) {
  if (length(call)>1) return(as.character(call[[1]]))
  as.character(call)
}

#' Creates a table with hint.stud templates from an err.sol.table
#' @param es a data frame returned by err.sol.table, err.sol.table.for.ps or err.sol.table.for.chunk
#' @param min.users only keep wrong calls that at least min.users made
#' @param default.filters if TRUE remove some rows that are unlikely to be used as custom hints (e.g. if in assignment both the assigned variable and the rhs differ).
hint.stud.table = function(es, min.users=1, default.filters=TRUE) {
  restore.point("hint.stud.table")
  if (default.filters) {
    es = es %>%
      filter(!startsWith(sol.code, "{"), !startsWith(sol.code,"structure("))
  }
  hs = es %>%
    group_by(ps.name, chunk, chunk.name, e.ind, stud.code, sol.code, stud.is.assign, sol.is.assign, stud.var, sol.var) %>%
    summarize(stud.call = first(stud.call), sol.call = first(sol.call), num.users = n_distinct(user), num.error = n()) %>%
    mutate(stud.fun = sapply(stud.call, call.name), sol.fun = sapply(sol.call, call.name)) %>%
    filter(num.users >= min.users) %>%
    arrange(ps.name, chunk, e.ind)

  # Do some further filters
  if (default.filters) {
    hs = hs %>%
      filter(sol.fun == stud.fun | sol.var == stud.var)
  }

  hs$hint.stud = ifelse(hs$stud.is.assign,
    paste0('hint.stud.assign("', hs$stud.var,'", ', hs$stud.code, ', msg ="Your hint.")'),
    paste0('hint.stud.call(', hs$stud.code, ', msg ="Your hint.")')
  )
  hs$hint.stud.fun = paste0('hint.stud.fun("', hs$stud.fun, '", msg ="Your hint.")')

  hs$hint.block = paste0("#< hint\n", hs$hint.stud, "\nauto.hint.else()\n#>")
  hs = select(hs, ps.name, chunk.name, stud.code, sol.code, num.users, hint.stud, hint.stud.fun, hint.block, everything())
  hs
}

#' Create an err.sol.table as input for hint.stud.table
#'
#' @param err.df If res is the the returned list from a call to analyse.subs() it is the elemet res$err.df
#' @param rps.dir The directory in which all problem set rps files can be found of the problem sets that have been analysed by analyse.subs.
err.sol.table = function(err.df, rps.dir=getwd()) {
  restore.point("err.sol.table")
  ps.names = unique(err.df$ps.name)
  res.li = lapply(ps.names, function(ps.name) {
    err.sol.table.for.ps(err.df, ps.name, rps.dir=rps.dir)
  })
  bind_rows(res.li)
}

#' Create an err.sol.table from a single problem set
#'
#' see err.sol.table for a basic description
err.sol.table.for.ps = function(err.df, ps.name, rps.dir=getwd(), rps=load.rps(file.path(rps.dir,ps.name))) {
  restore.point("hint.template.for.ps")
  rows = err.df$ps.name == ps.name
  err.df = err.df[rows,]

  chunk.inds = sort(unique(err.df$chunk))

  res.li = lapply(chunk.inds, function(chunk.ind) {
    err.sol.table.for.chunk(err.df, chunk.ind, ps.name, rps=rps)
  })
  bind_rows(res.li)
}

err.sol.table.for.chunk = function(err.df, chunk.ind, ps.name, rps.dir=getwd(), rps=load.rps(file.path(rps.dir,ps.name))) {
  restore.point("err.sol.table.for.chunk")

  rows = err.df$chunk == chunk.ind & err.df$ps.name == ps.name
  err.df = err.df[rows,]
  if (NROW(err.df)==0) return(NULL)

  cdt = rps$cdt
  e.li = cdt$e.li[[chunk.ind]]
  if (length(e.li)==0) return(NULL)
  sol.code.li = sapply(e.li, deparse1, collapse="\n")


  e.li.info = RTutor::make.expr.li.info(e.li, do.unlist=TRUE)
  e.li = e.li.info$matched.expr

  row = 1
  res.li = lapply(seq_len(NROW(err.df)), function(row) {
    #restore.point("dfhdhfh")

    e.ind = err.df$e.ind[row]
    if (e.ind==0 | e.ind > length(e.li)) return(NULL)

    stud.code = err.df$code[row]
    stud.expr.li = code.to.expr.li(stud.code)

    stud.expr.li = setdiff(stud.expr.li, e.li)
    if (length(stud.expr.li)==0) return(NULL)
    stud.li.info =  RTutor::make.expr.li.info(stud.expr.li, do.unlist=TRUE)

    stud.code = sapply(stud.expr.li,deparse1, collapse="\n")
    tibble(e.ind=e.ind, user=err.df$user[row],stud.code=stud.code,stud.is.assign=stud.li.info$is.assign, stud.var = stud.li.info$var, stud.call=stud.li.info$matched.expr)
  })
  res.df = bind_rows(res.li)
  if (NROW(res.df)==0) return(NULL)
  res.df$sol.code = sol.code.li[res.df$e.ind]
  res.df$sol.is.assign = e.li.info$is.assign[res.df$e.ind]
  res.df$sol.var = e.li.info$var[res.df$e.ind]
  res.df$sol.call = e.li.info$matched.expr[res.df$e.ind]
  res.df$ps.name=ps.name
  res.df$chunk = chunk.ind
  res.df$chunk.name = err.df$chunk.name[[1]]
  res.df
}


code.to.expr.li = function(code) {
  if (is.null(code)) return(NULL)
  expr = parse(text = code)
  expr = unlist(expr)
  expr = lapply(expr, match.call.object)
  expr
}
