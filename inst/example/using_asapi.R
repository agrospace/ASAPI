library(ASAPI)
library(jsonlite)
library(dplyr)

url = "https://api.agrospace.cl"

#     Login    #
#--------------#
email = "user.example@agrospace.cl"
password = "contra1234"
auth = asapi_auth(email=email, password=password, url=url)



client = auth$client
api_key = auth$api_key
user = auth$user


# Get client information #
#------------------------#
asapi_client_get(client, email, api_key, url)

asapi_list(client, "farm", api_key)
asapi_list(client, "user", api_key)

# Get user information #
#----------------------#
asapi_user_get(client, email, user, api_key, url)


# Get farm information #
#----------------------#
farm = "farm1example"
asapi_farm_get(client, farm, email, api_key, url)
asapi_sensor_get(client,email,farm,api_key)

# Get bigquery information #
#--------------------------#
tableid = "levelzero"
sensor = "S2SR"
index = "NDVI"
date_start="2020-11-29"
date_end="2021-02-10"
table = asapi_table(client, farm, tableid, sensor, index,
                    date_start, date_end, email, api_key, url)
table


asapi_index_get(client, email, farm,sensor, api_key)


# ALL case
sensor = "ALL"
index = "ALL"
table = asapi_table(client, farm, tableid, sensor, index,
                    date_start, date_end, email, api_key, url)
table

table_stats = asapi_table_stats(client, farm, datasetid,tableid, email, api_key, url)


#  Get Raster  #
#--------------#
(table_S2SR = table %>% filter(sensor == "S2SR") %>% slice(1))
date = table_S2SR$date
sensor = "S2SR"
index = table_S2SR$index
rst = asapi_image(client, farm, sensor, index, date, email,
                  api_key, url)
raster::plot(rst$rst)

rst = asapi_tiles(client, farm, sensor, index, date, email,
                  api_key, url)
raster::plot(rst$rst)


