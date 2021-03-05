#=====================================#
#     CAPACITACION LIBRERIA ASAPI     #
#             17 - 02 - 2021          #
#=====================================#
remotes::install_github('agrospace/ASAPI')
library(ASAPI)
library(jsonlite)
url = "https://api.agrospace.cl"

#### FUNCIONES ASAPI ####
# asapi_client_get(): Información mongo del client
# asapi_user_get(): Información mongo del user
# asapi_farm_get(): Información mongo de la farm
# asapi_list(): Listado de farm/user del client
# asapi_auth(): Autentificación del user
# asapi_features(): Información mongo de features

# asapi_table(): Descarga datos BQ con mean & sd
# asapi_table_cover(): Descarga datos BQ con nubosidad
# asapi_sensor_get(): Listado de sensor segun client+farm
# asapi_index_get(): Listado de index segun client+farm

# asapi_image(): Descarga raster

#### EJEMPLOS ####
#     auth    #
email = "user.example@agrospace.cl"
password = "contra1234"
(auth = asapi_auth(email=email, password=password, url=url))

client = auth$client
api_key = auth$api_key
user = auth$user

#     list    #
(list = asapi_list(client = client, type = "farm", api_key = api_key, url = url))
farm = list[[1]]

#     info mongo    #
asapi_client_get(client = client, email = email, api_key = api_key, url = url)
asapi_user_get(client = client, email = email, user = user, api_key = api_key, url = url)
asapi_farm_get(client = client, email = email, farm = farm, api_key = api_key, url = url)

asapi_features_get(index = "BIOMASS", farm_type = "pradera", api_key = api_key, url = url)



#     tablas BQ    #
tableid = "levelzero"
sensor = "S2SR"         #(ALL)
index = "NDVI"          #(ALL)
date_start="2020-11-29" #(ALL)
date_end="2021-02-10"   #(ALL)

asapi_table(client = client, farm = farm, tableid = tableid,
            sensor = sensor, index = index,
            date_start = date_start, date_end = date_end,
            email = email, api_key = api_key, url = url)

asapi_table_cover(client = client, farm = farm, tableid = tableid,
                  sensor = sensor, index = index,
                  date_start = date_start, date_end = date_end,
                  email = email, api_key = api_key, url = url)


asapi_sensor_get(client = client, email = email, farm = farm, api_key = api_key, url = url)
asapi_index_get(client = client, email = email, farm = farm, sensor=sensor, api_key = api_key, url = url)



#     raster    #
date = "2021-02-07"
(rst = asapi_image(client = client, farm = farm, sensor = sensor, index = index, date = date, email = email, api_key = api_key, url = url))
raster::plot(rst$rst)


#     RGB    #
date = "2020-11-29"
rst = asapi_image(client = client, farm = farm, sensor = sensor, index = "RGB", date = date, email = email, api_key = api_key, url = url)
raster::plotRGB(rst$rst)
