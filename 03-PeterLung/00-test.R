# µº»Î trd.cndalym ------------------------------------------------
dir <- "C:/Users/rossz/OneDrive/PR/Database-CSMar-Updating/trd.cndalym-2017-09-10"
file.paths <- list.files(path = dir,
                         pattern = "^TRD_Cndalym.txt$",
                         full.names = T)
file.names <- list.files(path = dir,
                         pattern = "^TRD_Cndalym.txt$",
                         full.names = F)
file.list <- list()
for (i in seq_along(file.paths)) {
    path <- file.paths[i]
    name <- file.names[i]
    file <- read.delim(path, head = T, stringsAsFactors = F,
                       fileEncoding = "UTF-8",
                       colClasses = c("character",
                                      "character",
                                      rep("numeric", 9))) %>% setDT()
    file[, ":="(Trddt = as.Date(Trddt, format = "%Y-%m-%d"))]
    file.list[[name]] <- file
}
trd.cndalym <- rbindlist(file.list, use.names = T)
setnames(trd.cndalym, names(trd.cndalym), tolower(names(trd.cndalym)))

f.dmkt <- trd.cndalym[markettype == "21", .(date = trddt, dret.mkt = cdretmdos * 100, damt.mkt = cnvaltrdtl)
    ][order(date)
    ] %>% unique() %>% na.omit()

n <- 120
p.dmkt <- f.dmkt[, lapply((n + 1):.N, function(i) {
    dret <- dret.mkt[(i - n):i];
    skew <- skewness(dret);
    kurt <- kurtosis(dret);
    rv <- sd(dret);
    list(date = date[i], dret.mkt = dret.mkt[i], damt.mkt = damt.mkt[i], skew = skew, kurt = kurt, rv = rv)
    }) %>% rbindlist()]

sv(p.dmkt)

p.wmkt <- p.dmkt[, .(wret.mkt = prod(1 + dret.mkt / 100) - 1, 
    skew = skewness(dret.mkt), 
    kurt = kurtosis(dret.mkt),
    rv = sd(dret.mkt)), 
    keyby = .(year = year(date), week = week(date))
    ][f.ywd, on = .(year ,week), nomatch = 0]