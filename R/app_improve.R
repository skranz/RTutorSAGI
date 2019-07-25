# Try to analyse the submission and find out which exercises have been particularly difficult.

grade.log = as.environment(list(current=NULL, all=NULL, file="grade_log.txt"))

examples.improve.app = function() {
  base.dir = "D:/libraries/RTutor/ps2019"
  setwd(base.dir)
  sub.li = load.moodle.subs(warn=FALSE)
  res = analyse.subs(sub.li)
  saveRDS(res, "analysis.Rds")

  library(RTutorSAGI)
  base.dir = "D:/libraries/RTutor/ps2019"
  setwd(base.dir)
  res = readRDS("analysis.Rds")
  sum.df = res$sum.df
  hint.df = res$hint.df
  err.df = res$err.df

  sum.df = sum.df %>%
    filter(ps.name=="ps_1b1_ols") %>%
    arrange(chunk)
  app = psImproveApp(sum.df, err.df, hint.df, ps.dir="ps")

  viewApp(app)
}

psImproveApp = function(sum.df, err.df=NULL, hint.df=NULL, log.df = rbind(err.df, hint.df), ps.dir = "ps", story.file="error_story.r") {
  restore.point("psImproveApp")
  library(shinyEvents)
  library(shinyAce)
  library(DT)
  app = eventsApp()

  tab = my.html.table(sum.df,"df_tab")

  app$ui = fluidPage(
    div(style="height: 20em; overflow-y: scroll;",HTML(tab)),
    aceEditor("edit",value = "",mode="r")
  )

  customEventHandler("tabClick",css.locator = "#df_tab tr", event="click", function(id,..., app) {
    args = list(...)
    restore.point("hskfhkdhfkd")
    row = as.integer(str.right.of(id,"tr_"))
    cd = sum.df[row,]


    txt = write.chunk.log.txt(log.df, cd$ps.name, cd$chunk)

    txt = paste0(cd$ps.name, " ", cd$chunk.name, "\n\n",txt)

    writeLines(txt, story.file)
    shinyAce::updateAceEditor(getAppSession(),editorId = "edit", txt,mode = "r")

    try(open.sol.rmd(cd$ps.name, cd$chunk.name, ps.dir=ps.dir))
    #cat("\nClick!!!")
  })
  app
}


open.sol.rmd = function(ps.name,chunk.name=NULL, sol.postfix = "_sol.Rmd", ps.dir=getwd(), make.org.copy=TRUE) {
  restore.point("open.sol.rmd")
  library(rstudioapi)

  file = file.path(ps.dir, paste0(ps.name,sol.postfix))
  if (!file.exists(file)) {
    stop(paste0("Cannot find solution file ", file))
  }
  if (make.org.copy) {
    org.file = file.path(ps.dir, paste0("org.",ps.name,sol.postfix))
    if (!file.exists(org.file)) {
      file.copy(file, org.file, overwrite = FALSE)
    }
  }
  txt = readLines(file,warn = FALSE)
  txt = name.rmd.chunks(txt = txt)
  pattern = paste0('```{r "', chunk.name)
  line = which(has.substr(txt, pattern))[1]
  if (!is.na(line)) {
    navigateToFile(file,min(line+10,NROW(txt)), 1)
  } else {
    navigateToFile(file)
  }
}
