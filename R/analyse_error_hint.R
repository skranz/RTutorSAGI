
examples.analyse.sub = function() {
  base.dir = "D:/lehre/empIOUlm/rtutor/ps2019"
  setwd(base.dir)
  res = analyse.sub()
  saveRDS(res, "analysis.Rds")

  res = readRDS("analysis.Rds")
  sum.df = res$sum.df
  hint.df = res$hint.df
  err.df = res$err.df

  app = psImproveApp(sum.df, err.df, hint.df, sol.dir="new_rmd")

  viewApp(app)
}

analyse.subs = function(sub.li, base.dir=getwd(), rps.dir=base.dir) {

  library(RTutor)
  restore.point("analyse.sub")

  setwd(base.dir)

  if (unpack.big) {
    res = unpack.big.zips(base.dir, prefix = prefix, postfix=postfix)
    ass = res$ass
  } else {
    ass = list.dirs(file.path(base.dir,"sub"),full.names = FALSE)
    ass = ass[nchar(ass)>0]
  }
  as = ass[1]

  sub.li = load.subs(ass, base.dir = base.dir)
  log.df = import.logs(sub.li)

  ps.names = unique(log.df$ps.name)
  cdt = bind_rows(lapply(ps.names,function(ps.name){
    rps = load.rps(ps.name)
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

  list(sum.df = chunk.sum, err.df=err.df, hint.df = hint.df)
}

combine_errors = function(df =quick.df(user=user,code=code),user=NULL, code=NULL, as.df=FALSE) {
  res.df = df %>%
    group_by(code) %>%
    summarize(
      num_user = n_distinct(user),
      num = n(),
      res_code = paste0("# n=",num, " users =",num_user,"\n",first(code))
    ) %>%
    arrange(-num_user,-num)
  if (as.df) {
    return(res.df)
  }
  res.df$res_code
}


load.cdt = function()


import.logs = function(sub.li) {
  li = lapply(sub.li, function(sub) {
    sub$log.df
  })
  bind_rows(li)
}


load.sub.with.name = function(file) {
  restore.point("load.sub.with.name")

  load(file)
  stud.name = str.between(basename(file),"----","--")
  Encoding(stud.name) <- "UTF-8"
  stud.name = mark_utf8(stud.name)
  sub$stud.name = correct.stud.names(stud.name)
  sub$log.df$stud.name = sub$stud.name
  sub$log.df$ps.name = sub$ps.name

  sub
}

load.subs = function(as=NULL, base.dir=NULL, files=NULL) {
  restore.point("load.subs")

  if (is.null(files)) {
    ass = as
    li = lapply(ass, function(as) {
      dir = paste0(base.dir,"/sub/",as)
      list.files(dir,pattern = glob2rx("*.sub"),full.names = TRUE)
    })
    files = do.call("c",li)
  }
  sub.li = lapply(files, load.sub.with.name)
  sub.li
}


correct.stud.names = function(txt) {
  restore.point("correct.stud.names")

  txt = gsub("Ã¤","ä",txt,fixed=TRUE)
  txt = gsub("Ã¶","ö",txt,fixed=TRUE)

  txt
}

#' Takes assignment ZIPs with all students' solutions
#' and unpacks them into separate folders for each assignment
unpack.big.zips = function(base.dir, prefix="", postfix=".zip") {
  library(stringtools)


  # Extract big ZIP files
  # We have one big zip file per assignment
  # it contains all the zip files submitted
  # by students for this assignment

  bzip.dir = paste0(base.dir,"/bigzip")
  bzips = list.files(bzip.dir)
  # Example file name: "WiWi39-SS15-Assignment ps_1a-33751.zip"

  # Extract names of assignments
  ass = str.between(bzips,prefix,postfix)

  as.base.dir = paste0(base.dir,"/sub")
  i = 1
  for (i in seq_along(ass)) {
    as = ass[i]
    as.dir = paste0(as.base.dir,"/",as)
    if (!dir.exists(as.dir))
      dir.create(as.dir, recursive=TRUE)

    bzip = paste0(bzip.dir,"/",bzips[i])
    unzip(bzip, exdir=as.dir)

    files = list.files(as.dir)
    library(tools)
    exts = file_ext(files)
    rows = exts != "sub"
    if (any(rows)) {
      str = paste0("\nThe following submissions have not the extension .sub and will be ignored:\n\n ", paste0(files[rows], collapse=",\n "))
      write.grade.log(str,"warning")
    }
  }
  return(list(ass=ass))
}


my.html.table = function(df,id="mytab", sel.row=NULL, col.names=TRUE, row.names=FALSE, border=TRUE, bg.color =c("#dddddd","#ffffff"), font.size="80%", round.digits=8, signif.digits=8,col.tooltips=NULL, tr.id = paste0(id,"_tr_",seq_len(NROW(df))), ...) {
  restore.point("html.table")
  n = NROW(df)

  #bg.color =c("#ededfe","#fcfcff")
  #F7F7F7
  row.bgcolor = rep(bg.color,length=n)

  if (!is.null(sel.row)) {
    row.bgcolor[sel.row]='#ffdc98'
    row.bgcolor[sel.row]='#00ff00'
  }

  if (is.character(col.names)) {
    colnames = col.names
    col.names = TRUE
  } else if (isTRUE(col.names)) {
    colnames = colnames(df)
  }
  if (col.names) {
    if (is.null(col.tooltips)) {
      inner = colnames
    } else {
      inner = paste0('<span title="', col.tooltips,'">', colnames, '<span>')
      #inner[nchar(col.tooltips)==0] = colnames
    }

    head = paste0('<th class="data-frame-th">',inner,'</th>', collapse="")
    head = paste0('<tr>', head, '</tr>')
  } else {
    head = ""
  }

  my.format.vals = function(vals) {
    format.vals(vals, signif.digits=signif.digits, round.digits=round.digits)
  }


  td.class = rep("data-frame-td", NROW(df))
  if (length(td.class)>0) {
    td.class[length(td.class)]="data-frame-td-bottom"
  }

  cols = 1:NCOL(df)
  code = paste0('"<td class=\\"",td.class,"\\" nowrap bgcolor=\\"",row.bgcolor,"\\">", my.format.vals(df[[',cols,']]),"</td>"', collapse=",")
  code = paste0('paste0("<tr id=\\"",tr.id,"\\">",',code,',"</tr>", collapse="\\n")')
  call = parse(text=code)
  main = eval(parse(text=code))

  tab = paste0('<table class="data-frame-table" id="',id,'">\n', head, main, "\n</table>")

  #th.style='font-weight: bold; margin: 3px; padding: 3px; border: solid 1px black; text-align: center;'
  #td.style='font-weight: normal; margin: 3px; padding: 3px; border: solid 1px black; font-family: monospace ; text-align: left;'

  th.style='font-weight: bold; margin: 3px; padding: 3px; border: solid 1px black; text-align: center;'
  td.style='font-family: Verdana,Geneva,sans-serif; margin: 0px 3px 1px 3px; padding: 1px 3px 1px 3px; border-left: solid 1px black; border-right: solid 1px black; text-align: left;'

  if (!is.null(font.size)) {
    th.style = paste0(th.style, "font-size: ", font.size,";")
    td.style = paste0(td.style, "font-size: ", font.size,";")

  }

  tab = paste0("<style>",
    " table.data-frame-table {	border-collapse: collapse;  display: block; overflow-x: auto;}\n",
    " td.data-frame-td {", td.style,"}\n",
    " td.data-frame-td-bottom {", td.style," border-bottom: solid 1px black;}\n",
    " th.data-frame-th {", th.style,"}\n",
    " table.data-frame-table tbody>tr:last-child>td {
      border-bottom: solid 1px black;
    }\n",
    "</style>",tab
  )

  #writeLines(tab, "test.html")
  tab

  return(tab)
}
