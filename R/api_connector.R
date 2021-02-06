#====================================#
# FUNCIONES PARA VISUALIZAR Fx's API #
#====================================#

#### API KEY ####
#' The API Constructor Function
#'
#' Generical function to add api key token for each call
#' @param api_key API key return by asapi_auth() function.
#' @param param_query List of key parameters.
#' @keywords constructor
#' @export
#' @examples
#' asapi_construct()
asapi_construct = function(param_query = NULL, api_key=NULL){

  api_key = Sys.getenv("api_key")

  if(is.null(api_key) || length(api_key)==0 ) {
    "Please set api_key with  .Renviron file or Sys.setenv('api_key'='token')"
  }

  if(is.null(param_query) || length(param_query)==0 ) {"Please fill query_params"}

  param_query = paste0(param_query,"?api_key=",api_key)
  return(param_query)
}


#### JSON editor####
#' JSON editor form
#'
#' Generical function to translate json to data.frame
#' @param api_key API key return by asapi_auth() function.
#' @param res Respone from api.agrospace.cl
#' @keywords response
#' @export
#' @examples
#' asapi_json()
asapi_json = function(res = NULL){
  if(is.null(res) || length(res)==0 ) {"Please resolve res content"}
  response = jsonlite::fromJSON(jsonlite::toJSON(httr::content(res),auto_unbox = TRUE))
  return(response)
}

#### URL Composer ####
#' URL endpoint
#'
#' Generical function to translate json to data.frame
#' @param url URL for dev purpose
#' @param endpoint Endpoint
#' @export
#' @examples
#' asapi_url()
asapi_url = function(url="http://api.agrospace.cl",endpoint){
  url = paste0(url,endpoint)
  return(url)
}

#### AUTH ####
#' The Authentication Function
#'
#' This function retrieve your AgroSpace API KEY
#' @param email Email of custome user
#' @param password Password of register user in https://api.agrospace.cl
#' @param url URL for dev purpose
#' @keywords API_KEY, api_key
#' @export
#' @examples
#' asapi_auth(email="userexample@agrospace.cl", password="contra1234")
#'
asapi_auth = function(email=NULL, password=NULL, url="http://api.agrospace.cl"){
  res = httr::GET(url = asapi_url(url = url,endpoint = '/auth'),
                  query =  list(email = email,password = password))
  res = jsonlite::fromJSON(jsonlite::toJSON(httr::content(res),auto_unbox = TRUE))
  return(res)
}

#### USERS ####
#' The Users GET Function
#'
#' This function allows you to GET User information with the API and retrieve your API KEY
#' @param client Client name
#' @param email email of user
#' @param user user name
#' @param api_key Api Key obtain from /auth
#' @param url URL for dev purpose
#' @keywords API_KEY, api_key
#' @export
#' @examples
#' asapi_user_get(client="clientexample", email="user.example@agrospace.cl", user="userexample", api_key="APIKEYEXAMPLE")
asapi_user_get = function(client, email, user, api_key, url="http://api.agrospace.cl"){
  param_query = list(client = client,
                     email = email,
                     user = user,
                     api_key=api_key)

  res = httr::GET(url = asapi_url(url = url,endpoint = '/user'),
                  query = param_query)

  if(res$status_code==200){
    res = asapi_json(res)
    res$counted_calls = do.call(rbind.data.frame, res$counted_calls)
  }else{
    res = httr::content(res)
  }

  return(res)
}

#### CLIENT ####
#' The Client GET Function
#'
#' This function allows you to GET Client information with the API and retrieve your API KEY
#' @param client Client name
#' @param email email of user
#' @param api_key Api Key obtain from /auth
#' @param url URL for dev purpose
#' @keywords api_key
#' @export
#' @examples
#' asapi_client_get(client="clientexample", email="user.example@agrospace.cl", api_key="APIKEYEXAMPLE")
asapi_client_get = function(client, email, api_key, url="http://api.agrospace.cl"){
  param_query = list(client = client,email = email,api_key=api_key)

  res = httr::GET(url = asapi_url(url = url,endpoint = '/client'),
                  query = param_query)

  if(res$status_code==200){
    res = asapi_json(res)

  }else{
    res = httr::content(res)
  }
  return(res)
}


#### FARMS ####
#' The Farms GET Function
#'
#' This function allows you to GET Farms information with the API and retrieve your API KEY
#' @param client Client name
#' @param farm Farm name to query
#' @param email email of user
#' @param api_key Api Key obtain from /auth
#' @param url URL for dev purpose
#' @keywords Farms
#' @export
#' @examples
#' asapi_farm_get(client="clientexample", email="user.example@agrospace.cl", farm="farm1example", api_key="APIKEYEXAMPLE")
asapi_farm_get = function(client, farm, email, api_key, url="http://api.agrospace.cl"){
  param_query = list(client = client, farm = farm,email = email,api_key = api_key)

  res = httr::GET(url = asapi_url(url = url,endpoint = '/farm'),
                  query = param_query)

  if(param_query$farm==""){
    res = httr::content(res)
  }else{
    res = asapi_json(res)
  }

  vector_farm = res$location$vector[[1]]

  return(list(response=res,shp = sf::st_read(vector_farm)))
}

#' The Farms GET Function
#'
#' This function allows you to GET Farms information with the API and retrieve your API KEY
#' @param client Client name
#' @param farm Farm name to query
#' @param email email of user
#' @param api_key Api Key obtain from /auth
#' @param url URL for dev purpose
#' @keywords Farms
#' @export
#' @examples
#' asapi_farm_put(client="clientexample", email="user.example@agrospace.cl", farm="farm1example", api_key="APIKEYEXAMPLE")
asapi_farm_put = function(param_query,  url="https://api.agrospace.cl"){
  # param_query = list(client = client, farm = farm,email = email,api_key = api_key)

  res = httr::PUT(url = asapi_url(url = url,endpoint = '/farm'),
                  query = param_query)

  if(param_query$farm==""){
    res = httr::content(res)
  }else{
    res = asapi_json(res)
  }

  vector_farm = res$location$vector[[1]]

  return(list(response=res,shp = sf::st_read(vector_farm)))
}


#### TABLE ####
#' The Table GET Function
#'
#' This function allows you to GET Client Farms  information with the API
#' @param client Client name
#' @param farm Farm name to query
#' @param tableid The mean values for the field (levelzero) or for each paddock (levelone)
#' @param sensor Sensor
#' @param index Index
#' @param date_start date from, YYYY-MM-DD format
#' @param date_end date to, YYYY-MM-DD format
#' @param email email of user
#' @param api_key Api Key obtain from /auth
#' @param url URL for dev purpose
#' @keywords API_KEY, api_key
#' @export
#' @examples
#' asapi_table(client="clientexample", farm="farm1example", tableid="levelzero", sensor="S2SR", index="NDVI", date_start="2020-11-29", date_end="2020-12-04", email="user.example@agrospace.cl", api_key="APIKEYEXAMPLE", url="https://api.agrospace.cl")
asapi_table = function(client, farm, tableid, sensor,
                       index, date_start, date_end, email, api_key, url="http://api.agrospace.cl"){
  param_query = list(client = client, farm = farm,
                     tableid = tableid,sensor = sensor,index = index,
                     date_start = date_start, date_end = date_end,
                     email = email,
                     api_key =api_key)

  res = httr::GET(url = asapi_url(url = url,endpoint = '/table'),
                  query = param_query)
  if(res$status_code==200){
    res = do.call(cbind.data.frame, asapi_json(res))
  }else{
    message("something wrong with the request")
    res = httr::content(res)
  }

  return(res)
}

#### TABLE COVER ####
#' The Table Cover GET Function
#'
#' This function allows you to GET Cover information with the API
#' @param client Client name
#' @param farm Farm name to query
#' @param tableid The mean values for the field (levelzero) or for each paddock (levelone)
#' @param sensor Sensor
#' @param index Index
#' @param date_start date from, YYYY-MM-DD format
#' @param date_end date to, YYYY-MM-DD format
#' @param email email of user
#' @param api_key Api Key obtain from /auth
#' @param url URL for dev purpose
#' @keywords API_KEY, api_key
#' @export
#' @examples
#' asapi_table_cover(client="clientexample", farm="farm1example", tableid="levelzero", sensor="S2SR", index="NDVI", date_start="2020-12-04", date_end="2020-12-19", email="user.example@agrospace.cl", api_key="APIKEYEXAMPLE", url="https://api.agrospace.cl")
asapi_table_cover = function(client, farm, tableid, sensor,
                             index, date_start, date_end, email, api_key, url="http://api.agrospace.cl"){
  param_query = list(client=client, farm=farm,
                     tableid=tableid, sensor=sensor, index=index,
                     date_start=date_start, date_end=date_end,
                     email=email, api_key=api_key)

  res = httr::GET(url = asapi_url(url = url,endpoint = '/table_cover'),
                  query = param_query)
  if(res$status_code==200){
    res = do.call(cbind.data.frame, asapi_json(res))
  }else{
    message("something wrong with the request")
    res = httr::content(res)
  }
  return(res)
}


#### RASTER ####

#' The image GET Function
#'
#' This function allows you to GET Client - Farm raster information with the API
#' @param client Client name
#' @param farm Farm name to query
#' @param sensor Sensor
#' @param index Index
#' @param date imagen date
#' @param email email of user
#' @param api_key Api Key obtain from /auth
#' @param url URL for dev purpose
#' @keywords API_KEY, api_key
#' @export
#' @examples
#' asapi_image()
asapi_image = function(client,farm,sensor,index,date,email,api_key, url="http://api.agrospace.cl"){
  param_query = list(client = client, farm = farm,sensor = sensor,
                     index = index,date = date,email = email,api_key =  api_key)

  res = httr::GET(url = asapi_url(url = url,endpoint = '/image'),
                  query = param_query)

  res_file = httr::content(res)

  if(index == "RGB"){

    rst = raster::brick(paste0('/vsicurl/',res_file))
    rst[rst>10000] <- NA
    rst[rst<0] <- NA
  }else{
    rst = raster::raster(paste0('/vsicurl/',res_file))
    rst[rst==9999] <- NA
    rst[rst==-9999] <- NA
    names(rst) = paste0(index, "_", date)
    if(index == "BIOMASS"){rst[rst == 0] <- NA}
  }

  return(rst)
}

#### SENSOR ####
#' The Sensor GET Function
#'
#' This function allows you to GET Sensor information of client
#' @param client Client name
#' @param farm farm name
#' @param email email of user
#' @param api_key Api Key obtain from /auth
#' @param url URL for dev purpose
#' @keywords API_KEY, api_key
#' @export
#' @examples
#' asapi_sensor_get(client="clientexample", email="user.example@agrospace.cl", farm="farm1example", api_key="APIKEYEXAMPLE")
asapi_sensor_get = function(client, email, farm, api_key, url="http://api.agrospace.cl"){
  param_query = list(client = client,
                     email = email,
                     farm = farm,
                     api_key=api_key)
  #browser()
  res = httr::GET(url = asapi_url(url = url,endpoint = '/info_sensor'),
                  query = param_query)

  if(res$status_code==200){
    res = asapi_json(res)
  }else{
    res = httr::content(res)
  }
  return(res)
}

#### INDEX ####
#' The Index GET Function
#'
#' This function allows you to GET Index information of client
#' @param client Client name
#' @param farm farm name
#' @param email email of
#' @param sensor sensor
#' @param api_key Api Key obtain from /auth
#' @param url URL for dev purpose
#' @keywords API_KEY, api_key
#' @export
#' @examples
#' asapi_index_get(client="clientexample", email="user.example@agrospace.cl", farm="farm1example", sensor="S2SR", api_key="APIKEYEXAMPLE")
asapi_index_get = function(client, email, farm, sensor, api_key, url="http://api.agrospace.cl"){
  param_query = list(client = client,
                     email = email,
                     farm = farm,
                     sensor = sensor,
                     api_key=api_key)
  res = httr::GET(url = asapi_url(url = url,endpoint = '/info_index'),
                  query = param_query)

  res = httr::content(res)
  return(res)
}

#### FEATURES ####
#' The Features GET Function
#'
#' This function allows you to GET Features information like Index, Labels, Class, Pallete, others.
#' Agrospace members only, with apikey master.
#' @param index index
#' @param farm_type farm type
#' @param api_key Api Key obtain from /auth
#' @param url URL for dev purpose
#' @keywords API_KEY, api_key
#' @export
#' @examples
#' asapi_features_get(index="BIOMASS", farm_type="esmeralda", api_key="APIKEYEXAMPLE")
asapi_features_get = function(index, farm_type, api_key, url="http://api.agrospace.cl"){
  param_query = list(index = index,
                     farm_type = farm_type,
                     api_key=api_key)
  res = httr::GET(url = asapi_url(url = url,endpoint = '/feature'),
                  query = param_query)
  res = do.call(cbind.data.frame, fromJSON(toJSON(httr::content(res))))
  return(res)
}

#### LIST USER/FARM ####
#' The List GET Function
#'
#' This function allows you to GET list of farms or users of a client.
#' Agrospace members only, with apikey master.
#' @param client Client name
#' @param type list type: farm or user
#' @param api_key Api Key obtain from /auth
#' @param url URL for dev purpose
#' @keywords API_KEY, api_key
#' @export
#' @examples
#' asapi_list(client="clientexample", type="farm", api_key="APIKEYEXAMPLE")
asapi_list = function(client, type, api_key, url="http://api.agrospace.cl"){
  param_query = list(client = client,
                     type = type,
                     api_key = api_key)
  res = httr::GET(url = asapi_url(url = url,endpoint = '/list'),
                  query = param_query)
  res = httr::content(res)
  return(res)
}
