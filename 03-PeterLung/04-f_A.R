# 这个脚本用来按照 d/w/y将trader分类。只是用tf进行分类
ld(p.cube.rb)
ld(f.sp.owner)
ld(f.cu)
ld(f.cubelife)

# 按照整个span分类 (仅包括sp.owner)
ap.all <- p.cube.rb[, .(tf = .N, tf.buy = sum(amt > 0)), keyby = .(cube.symbol)
    ][f.cubelife, on = .(cube.symbol), nomatch = 0
    ][f.cu, on = .(cube.symbol), nomatch = 0
    ][f.sp.owner, on = .(cube.symbol), nomatch = 0
    ][, .(user.id, cube.symbol, tf = tf / life * 7, tf.buy = tf.buy / life * 7, cube.type)
    ][, ":="(is.a = (tf > median(tf, na.rm = T)))]

# 按周统计，不区分策略 ----
ld(p.cube.rb)
ld(p.nwk.size)
ld(p.nwk.cmt.n)
ld(f.ywd)
ld(p.wmkt)

# 只计算SP的交易频率
ap.wk <- p.cube.rb[cube.type == "SP"
    ][, .(tf = .N), keyby = .(cube.symbol, year = year(date), week = week(date))]

ap.level <- ap.wk[, .(tf.q0.5 = quantile(tf, 0.5, type = 2), tf.q0.75 = quantile(tf, 0.75, type = 2), tf.q0.8 = quantile(tf, 0.8, type = 2)), keyby = .(year, week)
    ][, ":="(tf.q0.8.lag1 = shift(tf.q0.8))
    ] %>% na.omit()

f <- ap.wk[ap.level, on = .(year, week), nomatch = 0
    ][p.nwk.size, on = .(year, week), nomatch = 0 # 添加nwk.size
    ][, .(f = sum(tf >= 3) / sp.n[1], sp.n = sp.n[1]), keyby = .(year, week)
    ][f.ywd, on = .(year, week), nomatch = 0
    ][order(date)
    ][p.wmkt, on = .(year, week), nomatch = 0 # 添加 wmkt
    ][, ":="(is.ret.event = ifelse(abs(wret.mkt) >= quantile(abs(wret.mkt), 0.95), T, F),
        is.amt.event = ifelse((wamt.mkt > quantile(wamt.mkt, 0.95)) | (wamt.mkt < quantile(wamt.mkt, 0.05)), T, F)) # wret和wamt 首位5%标记为异常事件
    ][p.nwk.cmt.n, on = .(year, week), nomatch = 0 # 添加cmt.n
    ][, ":="(i.date = NULL)] %>% na.omit()

fit <- 
    lm(I(shift(f, 0, type = "lead") * 100) ~ I(seq_along(date)) + wret.mkt + I(log(wamt.mkt)) + I(skew - shift(skew)) + I(rv - shift(rv)) + is.ret.event + is.amt.event + I(sp.cmt.n.user / sp.n), data = f[date >= as.Date("2016-08-01")]) %>% summary()

file <- "C:/Users/rossz/OneDrive/SNT/03-PeterLung/reg.html"
stars <- c(0.01, 0.05, 0.1)
htmlreg(fit, stars = stars, file = file, digits = 4)

# 按照周统计，区分策略 ----
# 只计算SP的交易频率
ld(p.rb.char)

firmchar[, .(divrank = ), keyby = .(stkcd, year, week)]

ap.wk <- p.cube.rb[cube.type == "SP"
    ][, .(tf = .N), keyby = .(cube.symbol, year = year(date), week = week(date))]


# test  ----
f[date >= as.Date("2016-08-01")] %>%
    ggplot(aes(x = date, y = f)) +
    geom_smooth(se = F, span = 0.6)