# ��user.follow���ݼ�����graph ----
# ԭ�򣬳���centrality��������data.table������igraph������Ч�ʷǳ���
# Ҫ��֤from��to������uid
library(igraph)
ld(uid)
if (!exists("f.user.follow")) ld(f.user.follow)
gdt <- copy(f.user.follow)[, ":="(follow = lapply(follow, as.character))][, .(to = unlist(follow)), keyby = .(from = user.id)][to %in% uid] %>% unique()
f.g <- graph_from_data_frame(gdt)
sv(f.g)
ecount(f.g) # 12157839
vcount(f.g) # 384353 ��Щ����Ȼ��follow����followȴ��û�н�������ϵ��ˣ���vcount��uid������(390078)Ҫ��һЩ

# ����centrality -----
system.time({
cen <- page_rank(g, directed = T)$vector
cen <- data.table(user.id = V(g)$name, cen = cen)

}) # 2 min
