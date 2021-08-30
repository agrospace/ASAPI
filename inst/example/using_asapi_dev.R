library(ASAPI)
library(jsonlite)
library(dplyr)

url = "http://localhost:9090"
url = "https://api.agrospace.cl"


email = "tomasacuna@agrospace.cl"
password = "as1234"


client = "dga"
farm = "riohuemules"
api_key = "as098as"
index = "SCL"

client = "squella"
farm = "elcardal"

client = "clientexample"
farm = "farm2example"
api_key = "as098as"

auth = asapi_auth(email = email,password = password,url = url)
# auth = asapi_auth(email = "",password = password,url = url)

client = auth$client
api_key = auth$api_key
user=auth$user

asapi_user_get(client,email,user,api_key,url,dash_param = FALSE)
asapi_client_get(client,email,api_key,dash_param = FALSE)

farm = "piloto"
farm = "ALL"
shp = asapi_farm_get(client,farm,email,api_key,dash_param = FALSE)
plot(shp$shp)

tableid = "levelzero"

sensor = "S2SR"
index = "ALL"

date_start="2021-01-01"
date_end=Sys.Date()

df = asapi_table(client,farm,tableid,sensor,
                 index,date_start,date_end,email,api_key,
                 url,dash_param = FALSE)

df_cover = asapi_table_cover(client,farm,tableid,sensor,
                 index,date_start,date_end,email,api_key,url,
                 dash_param = FALSE)

# date=df$date[1]
indexselected = unique(df$index)[2]
date = df %>% filter(index==paste(indexselected)) %>%
  select(date) %>% slice(1) %>%  pull()

rst1 = asapi_image(client,farm,sensor,index,date,email,api_key,url,
                   dash_param = FALSE)
raster::plot(rst1$rst)

date2=df$date[nrow(df)]
rst2 = asapi_image(client,farm,sensor,index,date2,email,api_key,url,
                   dash_param = FALSE)
raster::plot(rst2$rst)


### RGB
index = "RGB"
df = asapi_table(client,farm,tableid,sensor,
                 index,date_start,date_end,email,api_key,url,
                 dash_param = FALSE)
date=df$date[7]

start_time <- Sys.time()
rst = asapi_image(client,farm,sensor,index,date,email,api_key,url)
end_time <- Sys.time()
end_time - start_time

raster::plotRGB(rst$rst)



features = asapi_features_get("ETR",farm_type = "cultivo",api_key = "as098as")

features = asapi_features_get("SELI",
                              "cultivo", "a",url=url)
