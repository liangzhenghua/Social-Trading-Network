# 03-postfilter用于创建一些常用的 f-prefix 表，例如 f.yw, f.cu
# 03-postfilter很多命令都无法在Yu-TF上完成，强烈建议在211-Server上完成本命令

# 计算cube周收益、月收益 ----
ld(f.cube.ret)
# 计算周收益
f.cube.wret <- f.cube.ret[, ":="(year = year(date), week = week(date))
    ][order(cube.symbol, year, week, -date)
    ][, .SD[1], keyby = .(cube.symbol, year, week)
    ][, ":="(wret = growth(value) * 100), keyby = cube.symbol
    ][!is.na(wret)]
# 周收益进行1%上下trim
f.cube.wret <- f.cube.wret[wret %between% c(quantile(wret, 0.01), quantile(wret, 0.99))]
sv(f.cube.wret)

# 计算月收益
f.cube.mret <- f.cube.ret[, ":="(year = year(date), month = month(date))
    ][order(cube.symbol, year, month, -date)
    ][, .SD[1], keyby = .(cube.symbol, year, month)
    ][, ":="(mret = growth(value) * 100), keyby = cube.symbol
    ][!is.na(mret)]
# 月收益进行1%上下缩尾
f.cube.mret <- f.cube.mret[mret %between% c(quantile(mret, 0.01), quantile(mret, 0.99))]
sv(f.cube.mret)

# 建立year-week和date之间的对应表 f.yw，用于绘图 ----
ld(f.cube.ret)
f.yw <- f.cube.ret[, .(date = unique(date))][order(date)][, ":="(year = year(date), week = week(date))
    ][order(year, week, -date)][, .SD[1], keyby = .(year, week)]
sv(f.yw)

# 建立year-week和cube.ret之间的对应表 f.cyw----
ld(f.cube.wret)
f.cyw <- f.cube.wret[, .(cube.symbol, year, week)] %>% unique() %>% setorder(cube.symbol, year, week)
sv(f.cyw)

# 建立cube和owner之间的关系 ----
ld(f.cube.info)
f.cu <- f.cube.info[, .(cube.symbol, user.id = owner.id)] %>% unique()
sv(f.cu)