# Import
# 03-PeterLung��ʹ�õ�����ԭʼ�ļ������� 01-Thesis�ġ�r-�����ߡ�f-������������----
ld(f.cube.info)
ld(f.cube.rb)
ld(f.cube.ret)
ld(f.cubelife)
ld(f.user.nwk)
ld(r.user.cmt)
ld(f.user.stock)
ld(f.user.info)

ld(f.ywd)
SDATE <- as.Date("2016-07-01")

# �Բ��֣�����ȫ�������ݼ����н�һ����ϴ��������С�p-��ǰ׺
# p.cube.rb ----
# ɾ��������1��life >= 60 d��2��2016-07-01֮����Ϊֻ���ڴ�֮�����ʵ������
cid.90d <- f.cubelife[life >= 90, unique(cube.symbol)]
cid.60d <- f.cubelife[life >= 60, unique(cube.symbol)]
cid.30d <- f.cubelife[life >= 30, unique(cube.symbol)]
p.cube.rb <- f.cube.rb[cube.symbol %in% cid.60d
    ][, .(cube.symbol, cube.type, stock.symbol, date, pre.weight = as.numeric(prev.weight.adjusted), target.weight, datetime)
    ][, ":="(date = as.Date(datetime))
    ][date >= SDATE
    ][is.na(pre.weight), pre.weight := 0
    ][, ":="(amt = target.weight - pre.weight)
    ][order(cube.symbol, date)]
sv(p.cube.rb)

# p.cube.ret ----
p.cube.ret <- f.cube.ret[cube.symbol %in% cid.60d
    ][, .(n = .N, value, label, cube.type), keyby = .(cube.symbol, date)
    ][n >= 2, ":="(value = NA) # ���ͬһ����������¼�������޷���֤��һ����¼�ǶԵģ����԰�������ļ�¼ȫ��ɾ������ǰһ���locf
    ][, ":="(value = na.locf(value, na.rm = F)), keyby = .(cube.symbol)
    ][!is.na(value)
    ][, ":="(n = .N), by = .(rleid(value))
    ][n <= 20 # ����и��������20�������յ�value��û�б䶯������Ϊ��������������30��ĵ�һ��֮������ݶ��޳�
    ] %>% unique(by = c("cube.symbol", "date"))
sv(p.cube.ret)


# p.cube.wret ----
# ɾ��������1��2016-07-01֮��; 2��1%����winsorize
p.cube.wret <- copy(f.cube.wret)[, ":="(wret = winsorize(wret, probs = c(0.01, 0.99)))
    ][f.ywd, on = .(year, week), nomatch = 0
    ][date >= SDATE]
sv(p.cube.wret)

# Market (daily/weekly) return / risk ----
ld(f.dmkt) # f.dmkt ���� trd.cndalym���������ۺ�A���봴ҵ���г��������桢�ɽ���
# p.dmkt���ۺ��г���ͳ��
n <- 60 # risk ����60�չ���
p.dmkt <- f.dmkt[, lapply((n + 1):.N, function(i) {
    dret <- dret.mkt[(i - n):i];
    skew <- skewness(dret);
    kurt <- kurtosis(dret);
    rv <- sd(dret);
    list(date = date[i], dret.mkt = dret.mkt[i], damt.mkt = damt.mkt[i], skew = skew, kurt = kurt, rv = rv)
}) %>% rbindlist()]
sv(p.dmkt)

## p.wmkt���ۺ��г���ͳ��
#ld(p.dmkt)
#p.wmkt <- p.dmkt[, .(wret.mkt = prod(1 + dret.mkt / 100) - 1,
    #wamt.mkt = sum(damt.mkt),
    #skew = skewness(dret.mkt),
    #kurt = kurtosis(dret.mkt),
    #rv = sd(dret.mkt)),
    #keyby = .(year = year(date), week = week(date))
    #][f.ywd, on = .(year, week), nomatch = 0]

p.wmkt <- p.dmkt[, .(wret.mkt = prod(1 + dret.mkt / 100) - 1,
    wamt.mkt = sum(damt.mkt),
    skew = mean(skew),
    kurt = mean(kurt),
    rv = mean(rv)),
    keyby = .(year = year(date), week = week(date))
    ][f.ywd, on = .(year, week), nomatch = 0]
sv(p.wmkt)

# N (network ������) ----
# ʹ��ÿ���ۼƵ�cube.n��proxy����Ϊcube�н���ʱ�䡪��deprecated������Ϊ����cubeֻ��಻����
#ld(f.cube.info)
#p.nwk.size <- f.cube.info[, .(cube.symbol, year = year(create.date), week = week(create.date))
    #][, .(new.cube.n = uniqueN(cube.symbol), new.sp.n = uniqueN(cube.symbol[str_sub(cube.symbol, 1, 2) == "SP"])), keyby = .(year, week)
    #][, ":="(cube.n = cumsum(new.cube.n), sp.n = cumsum(new.sp.n))
    #][year <= 2018]
#sv(p.nwk.size)

# ʹ��f.cubelife��proxy
ld(f.cubelife)
itvl <- data.table(date = seq(as.Date("2014-01-01"), as.Date("2018-01-01"), by = "day"))[, ":="(year = year(date), week = week(date))
    ][, .(start = min(date), end = max(date)), keyby = .(year, week)]
setkey(itvl, start, end)
setkey(f.cubelife, start, end)

olap <- foverlaps(itvl, f.cubelife, type = "any", which = T, nomatch = 0)

p.nwk.size <- olap[, {cube.symbol <- f.cubelife$cube.symbol[yid];
    cube.n <- length(cube.symbol);
    sp.n <- sum(str_sub(cube.symbol, 1, 2) == "SP");
    year <- itvl$year[.BY[[1]]];
    week <- itvl$week[.BY[[1]]];
    list(year = year, week = week, cube.n = cube.n, sp.n = sp.n)},
    key = .(xid)
    ][, ":="(xid = NULL)]

sv(p.nwk.size)
rm(olap, itvl)

# cmt.n �������� ----
ld(r.user.cmt) # ��ʱ 3.5 ����
ld(f.cu)
ld(SDATE)
ld(f.ywd)
# p.cmt.n: ÿ��uid����ͳ�ơ�msg.n: ȫ��������msg.n.user���û��������ų����Ҹոա���
p.cmt.n <- r.user.cmt[date >= SDATE, .(msg.n = .N, msg.n.user = .N - sum(str_sub(text, 1, 3) == "�Ҹո�")), keyby = .(user.id, year = year(date), week = week(date))]
sv(p.cmt.n)

# p.nwk.cmt.n: ����network�ķ���ͳ��
ld(f.sp.owner)
ld(p.nwk.size)
p.nwk.cmt.n <- p.cmt.n[, ":="(is.sp = ifelse(user.id %in% f.sp.owner$user.id, T, F))
    ][, .(sp.cmt.n = sum(msg.n[is.sp == T]), sp.cmt.n.user = sum(msg.n.user[is.sp == T]), cmt.n = sum(msg.n)), keyby = .(year, week)
    ][p.nwk.size, on = .(year, week), nomatch = 0]
sv(p.nwk.cmt.n)

p.nwk.cmt.n[f.ywd, on = .(year, week), nomatch = 0] %>% 
    ggplot(aes(x = date, y = sp.cmt.n.user)) +
    geom_line()