# ���ű����д�������� TF ������
# ����Ҫ��˳��ִ�У����ܵ���������

# ����cid�����ڶ�cube������޳� ----
ld(r.cube.info, T)
# 0. cube.symbol���ظ�����Ϊ����������ץȡ�ļ�¼����ѡ��lastcrawl���
cube.info <- r.cube.info[order(cube.symbol, - lastcrawl)][, .SD[1], keyby = .(cube.symbol)]
rm(r.cube.info)

# 1. r.cube.info: market = 'cn'
cid.cn <- cube.info[market == 'cn', unique(cube.symbol)]
sv(cid.cn)

# 2. exists in cube.rb
# r.cube.rb�Ѿ�����ȥ�ش���������Ҫ��ȥ�ء�������Ϣ���£�
#> uniqueN(r.cube.rb, by = c("cube.symbol"))
#[1] 1116086
#> uniqueN(r.cube.rb, by = c("cube.symbol", "stock.id", "datetime"))
#[1] 40188907
#> uniqueN(r.cube.rb, by = c("id"))
#[1] 39358674
#> uniqueN(r.cube.rb)
#[1] 40213456 == nrow(r.cube.rb)
ld(r.cube.rb)
cid.rb <- r.cube.rb[, unique(cube.symbol)]
sv(cid.rb)

# 3. exists in cube.ret
# r.cube.ret����ȥ�ش���������Ҫ��ȥ�ء�������Ϣ���£�
#> uniqueN(r.cube.ret, by = c("cube.symbol", "date"))
#[1] 468832185
#> uniqueN(r.cube.ret)
#[1] 468837556 == nrow(r.cube.ret)
ld(r.cube.ret) # 6 min
cid.ret <- r.cube.ret[, unique(cube.symbol)]
sv(cid.ret)

# 4. abnormal return
# ��ֵ��͵�1%�Լ���ֵ����150���޳�
cid.abret <- cube.info[net.value %between% c(quantile(net.value, 0.01), 150), unique(cube.symbol)]
sv(cid.abret)

# 5. ʹ���������ɵ�cid.abret, cid.cn, cid.rb, cid.ret�������յ�cid
cid <- intersect(cid.abret, cid.cn) %>% intersect(cid.rb) %>% intersect(cid.ret)
sv(cid)


# ����uid�����ڶ�user������޳� ----
# 1. ֻ��cid��owner�ſ��ܳ�Ϊuid
uid.cidowner <- cube.info[cube.symbol %in% cid, unique(owner.id)]
sv(uid.cidowner)

# 2. exists in r.user.info
# r.user.info�е�user.id���ظ���ѡ��lastcrawl����Ǹ�
ld(r.user.info)
user.info <- r.user.info[order(user.id, - lastcrawl)][, .SD[1], keyby = .(user.id)]
uid.userinfo <- user.info[, unique(user.id)]
sv(uid.userinfo)
rm(r.user.info)

# 3. exists in r.user.stock
# r.user.stock��Ҫȥ�أ��������� ��user.id, code, createAt������������ͬ������£�ѡ��lastcrawl���
# ע�⣬stockName�ǲ�׼�ģ���Ϊ��˾���ܸ�����������ͬһ�ҹ�˾��stockName��ͬ��code��ͬ����������Ϣ���£�
# uniqueN(r.user.stock, by = c("user.id", "code", "createAt")) # 28103735
ld(r.user.stock)
user.stock <- r.user.stock[order(user.id, code, createAt, - lastcrawl)][, .SD[1], keyby = .(user.id, code, createAt)]
rm(r.user.stock)
uid.userstock <- user.stock[, unique(user.id)]
sv(uid.userstock)

# ʹ���������ɵ�uid.cidowner, uid.userinfo�������յ�uid
uid <- intersect(uid.cidowner, uid.userinfo) %>% intersect(uid.userstock)
sv(uid)


# ʹ��cid��uid���� f-prefix ----
# ����cube.info
# ����fans.count��1
f.cube.info <- cube.info[cube.symbol %in% cid][, ":="(fans.count = (fans.count - 1))]
sv(f.cube.info)
rm(cube.info, r.cube.info, f.cube.info)

# ����f.cube.rb
f.cube.rb <- r.cube.rb[cube.symbol %in% cid][, ":="(prev.weight.adjusted = as.numeric(prev.weight.adjusted))]
rm(r.cube.rb)
# target.weight��prev.weightֻ����NA������[0,110]֮�����
# ��ʱ��֪ʲôԭ��prev.weight������΢����100����ʱ�������
f.cube.rb <- f.cube.rb[(is.na(target.weight) | target.weight %between% c(0, 110)) & (is.na(prev.weight.adjusted) | prev.weight.adjusted %between% c(0, 110))]
sv(f.cube.rb)
rm(f.cube.rb)

# ����user.info
f.user.info <- user.info[user.id %in% uid]
sv(f.user.info)
rm(user.info, f.user.info)

# ����user.stock
f.user.stock <- user.stock[user.id %in% uid]
f.user.stock <- f.user.stock[, .(user.id, stock.symbol = code, cube.type = exchange, create.date = as.Date(as.POSIXct(createAt / 1000, origin = "1970-01-01")), buy.price = buyPrice, sell.price = sellPrice, is.notice = isNotice, target.percent = targetPercent)]
sv(f.user.stock)
rm(user.stock, f.user.stock)

# ����f.cube.ret 
f.cube.ret <- r.cube.ret[cube.symbol %in% cid]
rm(r.cube.ret)
# ���� quit / re-enter, ��f.cube.ret���н�һ���޳�
# life����Ϊ��һ�������һ��֮���ʱ��, f.cubelife����ÿ����ϵ�����ʱ���Լ�����
# life���ٴ���1����ʱ��603174��cube
ld(f.cube.rb)
f.cubelife <- f.cube.rb[order(cube.symbol, datetime)][, .(start = as.Date(datetime[1]), end = as.Date(datetime[.N]), trade.n = .N), keyby = .(cube.symbol)][, ":="(life = as.integer(end - start))][life >= 1]
cid.1day <- unique(f.cubelife$cube.symbol)
cid <- intersect(cid, cid.1day)
sv(f.cubelife)
sv(cid.1day)
sv(cid)
# ֻ������һ�������һ�ʽ���֮��ľ�ֵ��¼������84042179��
f.cube.ret <- f.cube.ret[f.cubelife[, .(cube.symbol, start, end)], on = .(cube.symbol), nomatch = 0][between(date, start, end)][, ":="(start = NULL, end = NULL)]
sv(f.cube.ret)

# �� life>=1 �������Ӧ���� f.cube.info, f.cube.rb ----
ld(f.cube.info)
ld(f.cube.rb)
ld(f.cube.ret)
f.cube.info <- f.cube.info[cube.symbol %in% cid.1day]
f.cube.rb <- f.cube.rb[cube.symbol %in% cid.1day]
sv(f.cube.info)
sv(f.cube.rb)

