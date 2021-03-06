library(ASAPI)
library(jsonlite)

url = "https://api.agrospace.cl"

#     Login    #
#--------------#
email = "user.example@agrospace.cl"
password = "CONTRA111"
auth = asapi_auth(email=email, password=password, url=url)



client = auth$client
api_key = auth$api_key
user = auth$user


# Get client information #
#------------------------#
asapi_client_get(client, email, api_key, url)


# Get user information #
#----------------------#
asapi_user_get(client, email, user, api_key, url)


# Get farm information #
#----------------------#
farm = "farm1example"
asapi_farm_get(client, farm, email, api_key, url)


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

# ALL case
sensor = "ALL"
index = "ALL"
table = asapi_table(client, farm, tableid, sensor, index,
                    date_start, date_end, email, api_key, url)
table

#  Get Raster  #
#--------------#
date = table$date[1]
rst = asapi_image(client, farm, sensor, index, date, email,
                  api_key, url)
raster::plot(rst)
