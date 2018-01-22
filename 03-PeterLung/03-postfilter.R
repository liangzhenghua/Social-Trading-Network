# 03-postfilter���ڴ���һЩ���õ� f-prefix �������� f.yw, f.cu
# 03-postfilter�ܶ�����޷���Yu-TF����ɣ�ǿ�ҽ�����211-Server����ɱ�����

# ����cube�����桢������ ----
ld(f.cube.ret)
# ����������
f.cube.wret <- f.cube.ret[, ":="(year = year(date), week = week(date))
    ][order(cube.symbol, year, week, -date)
    ][, .SD[1], keyby = .(cube.symbol, year, week)
    ][, ":="(wret = growth(value) * 100), keyby = cube.symbol
    ][!is.na(wret)]
# ���������1%����trim
f.cube.wret <- f.cube.wret[wret %between% c(quantile(wret, 0.01), quantile(wret, 0.99))]
sv(f.cube.wret)

# ����������
f.cube.mret <- f.cube.ret[, ":="(year = year(date), month = month(date))
    ][order(cube.symbol, year, month, -date)
    ][, .SD[1], keyby = .(cube.symbol, year, month)
    ][, ":="(mret = growth(value) * 100), keyby = cube.symbol
    ][!is.na(mret)]
# ���������1%������β
f.cube.mret <- f.cube.mret[mret %between% c(quantile(mret, 0.01), quantile(mret, 0.99))]
sv(f.cube.mret)

# ����year-week��date֮��Ķ�Ӧ�� f.yw�����ڻ�ͼ ----
ld(f.cube.ret)
f.yw <- f.cube.ret[, .(date = unique(date))][order(date)][, ":="(year = year(date), week = week(date))
    ][order(year, week, -date)][, .SD[1], keyby = .(year, week)]
sv(f.yw)

# ����year-week��cube.ret֮��Ķ�Ӧ�� f.cyw----
ld(f.cube.wret)
f.cyw <- f.cube.wret[, .(cube.symbol, year, week)] %>% unique() %>% setorder(cube.symbol, year, week)
sv(f.cyw)

# ����cube��owner֮��Ĺ�ϵ ----
ld(f.cube.info)
f.cu <- f.cube.info[, .(cube.symbol, user.id = owner.id)] %>% unique()
sv(f.cu)