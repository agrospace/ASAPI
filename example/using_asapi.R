library(ASAPI)
library(jsonlite)

url = "http://localhost:9090"
url = "http://api.agrospace.cl"
url = "http://apidev.agrospace.cl"

email = "tomasacuna@agrospace.cl"
password = "****"

email = "user.example@agrospace.cl"
password = "contra1234"
# client = "squella"
# farm = "elcardal"

auth = asapi_auth(email = email,password = password,url = url)

client = auth$client
api_key = auth$api_key
user = auth$user

asapi_user_get(client,email,user,api_key,url)

asapi_client_get(client,email,api_key,url)

#farm = "piloto"
farm="farm1example"
asapi_farm_get(client,farm,email,api_key,url)

tableid = "levelzero"
sensor = "S2SR"
index = "NDVI"
date_start="2020-11-29"
date_end="2020-12-04"
# date_start="2020-10-01"
# date_end="2020-12-01"

df = asapi_table(client,farm,tableid,sensor,
                 index,date_start,date_end,email,api_key,url)
# df$date
# date="2019-10-21"

date = df$date[1]
rst = asapi_image(client,farm,sensor,index,date,email,api_key,url)
raster::plot(rst)
