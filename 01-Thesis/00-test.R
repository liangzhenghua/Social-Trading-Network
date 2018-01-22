library(mongolite)
conn <- mongo(collection = 'xq_cube_rb_updt', db = 'xueqiutest', url = "mongodb://192.168.1.54:27017")

# bach size = 1e7
system.time({
    iter <- conn$iterate(query = '{}', field = '{"_id":0, "holdings":0, "error_message":0, "error_status":0, "created_at":0, "updated_at":0, "prev_bebalancing_id":0, "new_buy_count":0, "diff":0, "exe_strategy":0}')
    r.cube.rb <- data.table()
    while (!is.null(res <- iter$batch(size = 1e7))) {
        chunk <- rbindlist(res, use.names = T, fill = T)
        r.cube.rb <- rbindlist(list(r.cube.ret, chunk), fill = T, use.names = T)
    }
})



#----------------
conn <- mongo(collection = 'xq_cube_rb_updt', db = 'xueqiutest', url = "mongodb://192.168.1.54:27017")
iter <- conn$iterate(query = '{}', field = '{"_id":0, "holdings":0, "error_message":0, "error_status":0, "created_at":0, "updated_at":0, "prev_bebalancing_id":0, "new_buy_count":0, "diff":0, "exe_strategy":0}')
res <- iter$batch(size = 1e3)
iter <- iter[1]

#res <- iter.cube$batch(size = 1000)
#cube <- rbindlist(res, fill = T, use.names = T, idcol = "rid")

res <- iter$batch(size = 1e7)
cb <- rbindlist(res[, .(category, status, cube_symbol, cube_type)], fill = T, use.names = T, idcol = "rid")


chunk$rebalancing_histories[1] %>% rbindlist()

z <- chunk[, .(x = rbindlist(rebalancing_histories)), keyby = .(rid)]







txt <- '{
    "id":1,
    "reply":{
        "num":2,
        "reply_content":[{
            "author":"Bill",
            "content":"abc"
        }, {
            "author":"Jack",
            "content":null
        }
        ]
    }
}'
json <- fromJSON(txt)

conn <- mongo(collection = 'test', db = 'xueqiutest', url = "mongodb://192.168.1.54:27017")
conn$insert(json)










