# Postfilter �����ɵ����ݼ��еĴ��С�f-����˵�����Ը�������Ŀ���ã��еĴ��С�t-���������ڡ�f-�������Ͻ�һ��filter�õ��ģ�ֻ��01-Thesis�Լ���
# ���� f.userlife ----
# userlife����Ϊ�����Ȱ�һ��user���е�cube���ܵ�һ�飬Ȼ����startΪ����cube������ʱ�䣬endΪ����cube�����һ�ʽ���ʱ�䣨��Ҫ���cubelife��
ld(f.cubelife)
ld(f.cube.info)
f.userlife <- f.cubelife[f.cube.info[, .(cube.symbol, user.id = owner.id)], nomatch = 0
    ][, .(start = min(start), end = max(end)), keyby = .(user.id)
    ][, ":="(life = as.integer(end - start))]
sv(f.userlife)

# ����cube�����桢������ ----
# ����cube.ret̫��ֻ�ֿܷ���㡣���Ƚ�����symbolͨ��ntile�ֳ�5�ݣ�Ȼ���ʹ��forѭ����ÿһ�ݽ���ȡÿ�����һ��Ĺ۲⣬�������ݼ�week
# ��������������ֿ����㣬�����ڴ治��
ld(f.cube.ret)
# ����������
f.cube.wret <- f.cube.ret[, ":="(year = year(date), week = week(date))
    ][order(cube.symbol, year, week, -date)
    ][, .SD[1], keyby = .(cube.symbol, year, week)
    ][, ":="(wret = growth(value) * 100), keyby = cube.symbol
    ][, ":="(label = NULL, date = NULL)
    ] %>% na.omit()
sv(f.cube.wret)
# ���������1%����trim
t.cube.wret <- copy(f.cube.wret)[, ":="(wret = winsorize(wret, probs = c(0.01, 0.99)))]
sv(t.cube.wret)

# ����������
f.cube.mret <- f.cube.ret[, ":="(year = year(date), month = month(date))][, tail(.SD, 1), keyby = .(cube.symbol, year, month)][, ":="(mret = growth(value) * 100), keyby = cube.symbol][, ":="(label = NULL, date = NULL)] %>% na.omit()
# ���������1%������β
f.cube.mret <- f.cube.mret[mret %between% c(quantile(mret, 0.01), quantile(mret, 0.99))]
sv(f.cube.mret)

# f.user.nwk ----
# ɸѡ������follow�������ʵ�̣�Ȼ���������½����ۼӡ���ÿһ�й۲ⶼ������ǰ��/��������������Լ��ۼ�follow�����
# cube.type == ZHCN�Ȱ�����ϣ��ְ���ʵ��
ld(f.user.stock)
sadd <- function(x) {
    if (length(x) == 1) {
        x
    } else {
        as.list(Reduce(union, x, accumulate = T))
    }
}
ld(f.userlife)
CJ <- f.userlife[, .(date = seq(start, end, by = "day")), keyby = .(user.id)
    ][, .(user.id, year = year(date), week = week(date))
    ] %>% unique() # Ϊ����һ��ʱ���ֵ
system.time({
f.user.nwk <- f.user.stock[cube.type %in% c('ZHCN')
    ][, ":="(year = year(create.date), week = week(create.date))
    ][, .(follow.cube = list(stock.symbol)), keyby = .(user.id, year, week)
    ][CJ, on = .(user.id, year, week), nomatch = NA
    ][order(user.id, year, week)
    ][, ":="(nbr = sadd(follow.cube)), keyby = .(user.id)]
    }) # 

#full.yw <- copy(f.user.stock)[, .(user.id, year = year(create.date), week = week(create.date))][order(user.id, year, week)] %>% unique()
sv(f.user.nwk)



# ��ǩ����sp ----
ld(f.cube.ret)
f.iposp <- f.cube.ret[(label != '') & cube.type == 'SP', unique(cube.symbol)]
sv(f.iposp)


# ����year-week��date֮��Ķ�Ӧ�� f.yw�����ڻ�ͼ ----
ld(f.cube.ret)
f.ywd <- f.cube.ret[, .(date = unique(date))][order(date)][, ":="(year = year(date), week = week(date))][, tail(.SD, 1), keyby = .(year, week)]
sv(f.ywd)

# ����year-week��cube.ret֮��Ķ�Ӧ�� f.cyw----
ld(f.cube.wret)
f.cyw <- f.cube.wret[, .(cube.symbol, year, week)] %>% unique() %>% setorder(cube.symbol, year, week)
sv(f.cyw)

# ����cube��owner֮��Ĺ�ϵ ----
ld(f.cube.info)
f.cu <- f.cube.info[, .(cube.type = str_sub(cube.symbol, 1, 2), cube.symbol, user.id = owner.id)]
sv(f.cu)

f.sp.owner <- f.cu[cube.type == "SP", .(user.id, cube.symbol)
    ][order(user.id, -cube.symbol)][, .SD[1], keyby = .(user.id)]
sv(f.sp.owner)
