# summary table ----
ld(f.user.info)
ld(f.cyw)
ld(f.cube.rb)
# trading frequency
f.cube.rb[, .(trd.n = .N), keyby = .(cube.symbol, year = year(datetime), week = week(datetime))
    ][f.cyw, on = .(cube.symbol, year, week)
    ][is.na(trd.n), trd.n := 0
    ][, .(mean = mean(trd.n), median = median(trd.n), sd = sd(trd.n), max = max(trd.n), min = min(trd.n)), keyby = .(cube.type = str_sub(cube.symbol, 1, 2))]
f.cube.rb[, .(trd.n = .N), keyby = .(cube.symbol, year = year(datetime), week = week(datetime))
    ][f.cyw, on = .(cube.symbol, year, week)
    ][is.na(trd.n), trd.n := 0
    ][, .(mean = mean(trd.n), median = median(trd.n), sd = sd(trd.n), max = max(trd.n), min = min(trd.n))]

# fans / follow / cmt
ld(f.user.info)
f <- f.user.info[f.cu, on = .(user.id)][, .(user.id, cube.symbol, fans.n = fans.count, follow.n = follow.count, stock.n = stock.count, cmt.n = status.count, cube.type = str_sub(cube.symbol, 1, 2))
    ][order(user.id, cube.type)
    ][, .SD[1], keyby = .(user.id)]
# fans
f[, .(mean = mean(fans.n, na.rm = T), median = median(fans.n, na.rm = T), sd = sd(fans.n, na.rm = T), max = max(fans.n, na.rm = T), min = min(fans.n, na.rm = T)), keyby = .(cube.type)]
f[, .(mean = mean(fans.n, na.rm = T), median = median(fans.n, na.rm = T), sd = sd(fans.n, na.rm = T), max = max(fans.n, na.rm = T), min = min(fans.n, na.rm = T)), keyby = .(cube.type)]
# follow
f[, .(mean = mean(follow.n, na.rm = T), median = median(follow.n, na.rm = T), sd = sd(follow.n, na.rm = T), max = max(follow.n, na.rm = T), min = min(follow.n, na.rm = T)), keyby = .(cube.type)]
f[, .(mean = mean(follow.n, na.rm = T), median = median(follow.n, na.rm = T), sd = sd(follow.n, na.rm = T), max = max(follow.n, na.rm = T), min = min(follow.n, na.rm = T))]
# follow
f[, .(mean = mean(follow.n, na.rm = T), median = median(follow.n, na.rm = T), sd = sd(follow.n, na.rm = T), max = max(follow.n, na.rm = T), min = min(follow.n, na.rm = T)), keyby = .(cube.type)]
f[, .(mean = mean(follow.n, na.rm = T), median = median(follow.n, na.rm = T), sd = sd(follow.n, na.rm = T), max = max(follow.n, na.rm = T), min = min(follow.n, na.rm = T))]
# cmt
f[, .(mean = mean(cmt.n, na.rm = T), median = median(cmt.n, na.rm = T), sd = sd(cmt.n, na.rm = T), max = max(cmt.n, na.rm = T), min = min(cmt.n, na.rm = T)), keyby = .(cube.type)]
f[, .(mean = mean(cmt.n, na.rm = T), median = median(cmt.n, na.rm = T), sd = sd(cmt.n, na.rm = T), max = max(cmt.n, na.rm = T), min = min(cmt.n, na.rm = T))]

# average trading frequency ----
ld(f.cube.rb)
ld(f.yw)
plot <- f.cube.rb[, .(tf = (.N / uniqueN(cube.symbol))), keyby = .(cube.type, year = year(datetime), week = week(datetime))][f.yw, on = .(year, week), nomatch = 0
    ][cube.type == "SP", type := "Signal Follower"
    ][cube.type == "ZH", type := "Signal Provider"]
plot[!(cube.type == "SP" & date <= "2016-07-01")] %>%
    ggplot(aes(x = date, y = tf)) +
    facet_grid(. ~ type, scales = "fixed") + 
    theme_bw() +
    geom_line(size = 0.75) +
    geom_point(size = 2.25) +
    geom_line(stat = "smooth", size = 0.65, linetype = "dashed", method = "lm", se = F, alpha = 0.6, color = "red") +
    xlab("") +
    ylab("Average trades per user")
ggsave("trading frequency.jpg")


# average stock.n ----
plot <- f.cube.rb[, .(stock.n = uniqueN(stock.symbol) / uniqueN(cube.symbol)), keyby = .(cube.type, year = year(datetime), week = week(datetime))
    ][f.yw, on = .(year, week), nomatch = 0
    ][cube.type == "SP", type := "Signal Follower"
    ][cube.type == "ZH", type := "Signal Provider"]
plot[!(cube.type == "SP" & date <= "2016-07-01") & stock.n %between% c(0, 1.5)] %>%
    ggplot(aes(x = date, y = stock.n)) +
    facet_grid(. ~ type, scales = "fixed") +
    theme_bw() +
    geom_line(size = 0.75) +
    geom_point(size = 2.25) +
    geom_line(stat = "smooth", size = 0.65, linetype = "dashed", method = "lm", se = F, alpha = 0.6, color = "red") +
    xlab("") +
    scale_x_date(limits = c(as.Date("2015-01-01"), as.Date("2017-09-20"))) +
    ylab("Stock broadness") +
    #scale_y_continuous(limits = c(0, 0.35))
ggsave("stock broadness.jpg")


# average ind.n ----
ld(f.stk.info)
plot <- f.cube.rb[, .(cube.type, cube.symbol, datetime, stock.symbol = str_sub(stock.symbol, 3, 8))
    ][f.stk.info[, .(stock.symbol = sprintf("%06d", Stkcd), ind.symbol = ind, ind.name = Nnindnme)], on = .(stock.symbol), nomatch = 0
    ][, .(ind.n = uniqueN(ind.name) / uniqueN(cube.symbol) * 1000), keyby = .(cube.type, year = year(datetime), week = week(datetime))
    ][f.yw, on = .(year, week), nomatch = 0
    ][cube.type == "SP", type := "Signal Follower"
    ][cube.type == "ZH", type := "Signal Provider"]

plot[!(cube.type == "SP" & date <= "2016-07-01") & ind.n %between% c(0, 100)] %>%
    ggplot(aes(x = date, y = ind.n)) +
    facet_grid(. ~ type, scales = "fixed") +
    theme_bw() +
    geom_line(size = 0.75) +
    geom_point(size = 2.25) +
    geom_line(stat = "smooth", size = 0.65, linetype = "dashed", method = "lm", se = F, alpha = 0.6, color = "red") +
    xlab("") +
    scale_x_date(limits = c(as.Date("2015-01-01"), as.Date("2017-09-20"))) +
    ylab("Industry broadness")
ggsave("Industry broadness.jpg")


# average cmt.n ----
ld(cmt.n)
ld(f.yw)
ld(f.cu)
#plot <- cmt.n[f.cu, on = .(user.id)]
    #[, .(cmt.n = sum(cmt.n) / uniqueN(user.id), cmt.reply.n = sum(cmt.reply.n) / uniqueN(user.id)), keyby = .(cube.type, year, week)][f.yw, on = .(year, week), nomatch = 0
    #][cube.type == "SP", type := "Signal Follower"
    #][cube.type == "ZH", type := "Signal Provider"]
#plot %>%
    #ggplot(aes(x = date, y = cmt.n)) +
    #theme_bw() +
    #geom_line(size = 0.75) +
    #geom_point(size = 2.25) +
    #geom_line(stat = "smooth", size = 0.65, linetype = "dashed", method = "lm", se = F, alpha = 0.6, color = "red") +
    #xlab("") +
    #scale_x_date(limits = c(as.Date("2015-01-01"), as.Date("2017-09-20"))) +
    #ylab("Comment number")
#ggsave("Comment number.jpg")

# return s.d. ----
ld(f.cube.wret)
ld(f.cube.rb)
ld(f.yw)
trade.week <- f.cube.rb[, .(flag = 1), keyby = .(cube.symbol, year = year(datetime), week = week(datetime))][, flag := NULL]
plot <- f.cube.wret[trade.week, on = .(cube.symbol, year, week), nomatch = 0
    ][, .(ret.sd = sd(wret)), keyby = .(cube.type, year, week)
    ][f.yw, on = .(year, week)
    ][cube.type == "SP", type := "Signal Follower"
    ][cube.type == "ZH", type := "Signal Provider"
    ][!is.na(type)]
plot %>%
    ggplot(aes(x = date, y = ret.sd)) +
    facet_grid(. ~ type, scales = "fixed") +
    theme_bw() +
    geom_line(size = 0.75) +
    geom_point(size = 2.25) +
    geom_line(stat = "smooth", size = 0.65, linetype = "dashed", method = "lm", se = F, alpha = 0.6, color = "red") +
    xlab("") +
    scale_x_date(limits = c(as.Date("2015-01-01"), as.Date("2017-09-20"))) +
    ylab("Return Standard Deviation")
ggsave("Return standard deviation.jpg")

