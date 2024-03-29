#====================================#
# FUNCIONES PARA VISUALIZAR Fx's API #
#====================================#

#### API KEY ####

#### JSON editor####
#' JSON editor form
#'
#' Generical function to translate json to data.frame
#' @param res Respone from api.agrospace.cl
#' @return A JSON formatted objected
#' @export
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
asapi_url = function(url="https://api.agrospace.cl",endpoint){
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
#' @export
asapi_auth = function(email=NULL, password=NULL, url="https://api.agrospace.cl"){
  res = httr::GET(url = asapi_url(url = url,endpoint = '/auth'),
                  query =  list(email = email,password = password))
  if(res$status_code==200){
    res = jsonlite::fromJSON(jsonlite::toJSON(httr::content(res),auto_unbox = TRUE))
  }else{
    res = httr::content(res,as = "text", encoding = "UTF-8")
    message(res)
  }

  return(res)
}

#' The New ApiKey Function
#'
#' This function Reset your AgroSpace API Key, entering your session.
#' @param email Email of custome user
#' @param password Password of register user in https://api.agrospace.cl
#' @param url URL for dev purpose
#' @export
asapi_new_apikey = function(email=NULL, password=NULL, url="https://api.agrospace.cl"){
  res = httr::POST(url = asapi_url(url = url,endpoint = '/new_apikey'),
                  query =  list(email = email,password = password))

  if(res$status_code==200){
    res = jsonlite::fromJSON(jsonlite::toJSON(httr::content(res),auto_unbox = TRUE))
  }else{
    res = httr::content(res,as = "text", encoding = "UTF-8")
    message(res)
  }

  return(res)
}

#' The New Password Function
#'
#' This function Reset your user Password, entering your session.
#' @param email Email of custome user
#' @param old_password Password of register user in https://api.agrospace.cl
#' @param new_password New Password user
#' @param url URL for dev purpose
#' @export
asapi_new_pass = function(email=NULL, old_password=NULL, new_password=NULL, url="https://api.agrospace.cl"){
  res = httr::POST(url = asapi_url(url = url,endpoint = '/new_password'),
                   query =  list(email=email, old_password=old_password, new_password=new_password))

  if(res$status_code==201){
    res = jsonlite::fromJSON(jsonlite::toJSON(httr::content(res),auto_unbox = TRUE))
  }else{
    res = httr::content(res,as = "text", encoding = "UTF-8")
    message(res)
  }

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
#' @param dash_param AgroSpace internal use parameter. Default value FALSE
#' @param url URL for dev purpose
#' @export
asapi_user_get = function(client, email, user, api_key, url="https://api.agrospace.cl", dash_param=FALSE){
  param_query = list(client = client,
                     email = email,
                     user = user,
                     dash_param = dash_param,
                     api_key=api_key)

  res = httr::GET(url = asapi_url(url = url,endpoint = '/user'),
                  query = param_query)

  if(res$status_code==200){
    res = asapi_json(res)
  }else{
    res = httr::content(res,as = "text", encoding = "UTF-8")
    message(res)
  }

  return(res)
}


#' The Users POST Function
#'
#' This function allows you to POST New User with the API and retrieve your API KEY
#' @param client Client name
#' @param user_name The new user name
#' @param email_user Email of the new user
#' @param password password of the new user
#' @param rol rol of the new user (Optional)
#' @param email email of user
#' @param api_key Api Key obtain from /auth
#' @param url URL for dev purpose
#' @export
asapi_user_post = function(client, user_name, email_user, password, rol, email, api_key, url="https://api.agrospace.cl"){
  param_query = list(client=client,
                     user_name=user_name,
                     email_user=email_user,
                     password=password,
                     rol=rol,
                     email=email,
                     api_key=api_key)

  res = httr::POST(url = asapi_url(url = url,endpoint = '/user'),
                   query = param_query)

  if(res$status_code==201){
    res = asapi_json(res)
  }else{
    res = httr::content(res,as = "text", encoding = "UTF-8")
    message(res)
  }

  return(res)
}


#' The Users PUT Function
#'
#' This function allows you to PUT User new information with the API and retrieve your API KEY
#' @param client Client name
#' @param email What is the email of the user to edit.
#' @param new_username New user name. If the value is 'NULL' it does not change.
#' @param new_email New email user. If the value is 'NULL' it does not change.
#' @param new_rol New user rol: admin or user. If the value is 'NULL' it does not change.
#' @param email_admin  Email of the user who is editing. It must be 'admin'.
#' @param api_key Api Key obtain from /auth
#' @param url URL for dev purpose
#' @export
asapi_user_put = function(client, email, new_username="NULL", new_email="NULL", new_rol="NULL", email_admin, api_key, url="https://api.agrospace.cl"){
  param_query = list(client=client, email=email, new_username=new_username,
                     new_email=new_email, new_rol=new_rol,
                     email_admin=email_admin, api_key=api_key)

  res = httr::PUT(url = asapi_url(url = url,endpoint = '/user'),
                  query = param_query)


  if(res$status_code==201){
    res = httr::content(res,as = "text", encoding = "UTF-8")
  }else{
    res = httr::content(res,as = "text", encoding = "UTF-8")
    message(res)
  }
  return(res)
}


#' The Users DELETE Function
#'
#' This function allows you to DELETE User information with the API and retrieve your API KEY
#' @param client Client name
#' @param email_deleted User's email to delete
#' @param email email of user admin
#' @param api_key Api Key obtain from /auth
#' @param url URL for dev purpose
#' @export
asapi_user_delete = function(client, email_deleted, email, api_key, url="https://api.agrospace.cl"){
  param_query = list(client = client,
                     email_deleted = email_deleted,
                     email = email,
                     api_key=api_key)

  res = httr::DELETE(url = asapi_url(url = url,endpoint = '/user'),
                     query = param_query)
  if(res$status_code==204){
    res = paste0("Email user: ", email_deleted, " removed")
  }else{
    res = httr::content(res,as = "text", encoding = "UTF-8")
    message(res)
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
#' @param dash_param AgroSpace internal use parameter. Default value FALSE
#' @param url URL for dev purpose
#' @export
asapi_client_get = function(client, email, api_key, url="https://api.agrospace.cl", dash_param=FALSE){
  param_query = list(client=client, email=email, api_key=api_key, dash_param=dash_param)

  res = httr::GET(url = asapi_url(url = url,endpoint = '/client'),
                  query = param_query)

  if(res$status_code==200){
    res = asapi_json(res)

  }else{
    res = httr::content(res,as = "text", encoding = "UTF-8")
    message(res)
  }
  return(res)
}

#' The Client POST Function
#'
#' This function allows you to POST New Client with the API and retrieve your API KEY
#' @param client_name  Name of de new client.
#' @param email Email of the contact of the new client.
#' @param contact Contact name.
#' @param api_key AgroSpace API Key.
#' @param url URL for dev purpose
#' @export
asapi_client_post = function(client_name, email, contact, api_key, url="https://api.agrospace.cl"){
  param_query = list(client_name=client_name,
                     email=email,
                     contact=contact,
                     api_key=api_key)

  res = httr::POST(url = asapi_url(url = url,endpoint = '/client'),
                   query = param_query)

  if(res$status_code==201){
    res = httr::content(res,as = "text", encoding = "UTF-8")
  }else{
    res = httr::content(res,as = "text", encoding = "UTF-8")
    message(res)
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
#' @param dash_param AgroSpace internal use parameter. Default value FALSE
#' @param url URL for dev purpose
#' @export
asapi_farm_get = function(client, farm, email, api_key, url="https://api.agrospace.cl", dash_param=FALSE){
  param_query = list(client=client, farm=farm, email=email, api_key=api_key, dash_param=dash_param)

  res = httr::GET(url = asapi_url(url = url,endpoint = '/farm'),
                  query = param_query)

  if(res$status_code==200){
    if(param_query$farm=="ALL"){
      res = httr::content(res)
      return(list(response = res))
    }else{
      res = asapi_json(res)
      vector_farm = res$location$vector[[1]]
      # TODO remove dependnecy of st_read from this packages
      return(list(response=res,shp = sf::st_read(vector_farm,quiet=TRUE)))
    }
  }else{
    res = httr::content(res,as = "text", encoding = "UTF-8")
    message(res)
  }
  return(res)
}

#' The Farms POST Function
#'
#' This function allows you to POST new Farm with the API and retrieve your API KEY
#' @param client Client name
#' @param farm_name Farm name to be set
#' @param email email of user
#' @param geojson geojson apram
#' @param api_key Api Key obtain from /auth
#' @param url URL for dev purpose
#' @export
asapi_farm_post = function(client, farm_name, geojson="NULL", email, api_key, url="https://api.agrospace.cl"){
  param_query = list(client=client, farm_name=farm_name, geojson=geojson, email=email, api_key=api_key)

  res = httr::POST(url = asapi_url(url = url,endpoint = '/farm'),
                  query = param_query)

  if(res$status_code==201){
    res = httr::content(res,as = "text", encoding = "UTF-8")
  }else{
    res = httr::content(res,as = "text", encoding = "UTF-8")
    message(res)
  }
  return(res)
}

#' The Farms PUT Function
#'
#' This function allows you to PUT farm new information with the API and retrieve your API KEY
#' @param client Client name
#' @param farm Farm name to query
#' @param new_type New farm type. If the value is 'NULL' it does not change.
#' @param new_region New farm region. If the value is 'NULL' it does not change.
#' @param new_commune New farm Commune. If the value is 'NULL' it does not change.
#' @param new_geojson New farm geojson. If the value is 'NULL' it does not change.
#' @param email_admin  Email of the user who is editing. It must be 'admin'.
#' @param api_key Api Key obtain from /auth
#' @param url URL for dev purpose
#' @export
asapi_farm_put = function(client, farm, new_type="NULL", new_region="NULL", new_commune="NULL", new_geojson="NULL", email_admin, api_key, url="https://api.agrospace.cl"){
  param_query = list(client=client, farm=farm, new_type=new_type, new_region=new_region,
                     new_commune=new_commune, new_geojson=new_geojson, email_admin=email_admin, api_key=api_key)
  res = httr::PUT(url = asapi_url(url = url,endpoint = '/farm'),
                  query = param_query)

  if(res$status_code==201){
    res = httr::content(res,as = "text", encoding = "UTF-8")
  }else{
    res = httr::content(res,as = "text", encoding = "UTF-8")
    message(res)
  }
  return(res)
}

#' The Farms DELETE Function
#'
#' This function allows you to DELETE Farms information with the API and retrieve your API KEY
#' @param client Client name
#' @param farm Farm name to query
#' @param email email of user
#' @param api_key Api Key obtain from /auth
#' @param url URL for dev purpose
#' @export
asapi_farm_delete = function(client, farm, email, api_key, url="https://api.agrospace.cl"){
  param_query = list(client=client, farm=farm, email=email, api_key=api_key)


  res = httr::DELETE(url = asapi_url(url = url,endpoint = '/farm'),
                   query = param_query)

  if(res$status_code==204){
    res = paste0("Farm: ", farm, " removed")
  }else{
    res = httr::content(res,as = "text", encoding = "UTF-8")
    message(res)
  }
  return(res)
}

#### TABLES ####
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
#' @param dash_param AgroSpace internal use parameter. Default value FALSE
#' @param url URL for dev purpose
#' @export
asapi_table = function(client, farm, tableid, sensor, index, date_start, date_end,
                       email, api_key, url="https://api.agrospace.cl", dash_param=FALSE){
  param_query = list(client=client, farm=farm, tableid=tableid, sensor=sensor,
                     index=index, date_start=date_start, date_end=date_end,
                     email=email, api_key=api_key, dash_param=dash_param)

  res = httr::GET(url = asapi_url(url = url,endpoint = '/table'),
                  query = param_query)

  if(res$status_code==200){
    res = do.call(cbind.data.frame, asapi_json(res))
  }else{
    res = httr::content(res,as = "text", encoding = "UTF-8")
    message(res)
  }

  return(res)
}

#' The Table GET Function
#'
#' This function allows you to GET Client Farms  information with the API
#' @param client Client name
#' @param farm Farm name to query
#' @param datasetid dataset prod or dev
#' @param tableid The mean values for the field (levelzero) or for each paddock (levelone)
#' @param month Boolean vaule TRUE or FALSE to retrieve monthly stats (TRUE)
#' @param email email of user
#' @param api_key Api Key obtain from /auth
#' @param dash_param AgroSpace internal use parameter. Default value FALSE
#' @param url URL for dev purpose
#' @export
asapi_table_stats = function(client, farm, datasetid, tableid, month, email,
                             api_key, url="https://api.agrospace.cl", dash_param=FALSE){
  param_query = list(client=client, farm=farm, datasetid=datasetid,tableid=tableid,
                     month=month,email=email, api_key=api_key, dash_param=dash_param)

  res = httr::GET(url = asapi_url(url = url,endpoint = '/table_stats'),
                  query = param_query)

  if(res$status_code==200){
    res = do.call(cbind.data.frame, asapi_json(res))
  }else{
    res = httr::content(res,as = "text", encoding = "UTF-8")
    message(res)
  }

  return(res)
}

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
#' @param dash_param AgroSpace internal use parameter. Default value FALSE
#' @param url URL for dev purpose
#' @export
asapi_table_cover = function(client, farm, tableid, sensor, index, date_start, date_end,
                             email, api_key, url="https://api.agrospace.cl", dash_param=FALSE){
  param_query = list(client=client, farm=farm,
                     tableid=tableid, sensor=sensor, index=index,
                     date_start=date_start, date_end=date_end,
                     email=email, api_key=api_key, dash_param=dash_param)

  res = httr::GET(url = asapi_url(url = url,endpoint = '/table_cover'),
                  query = param_query)
  if(res$status_code==200){
    res = do.call(cbind.data.frame, asapi_json(res))
  }else{

    res = httr::content(res,as = "text", encoding = "UTF-8")
    message(res)
  }
  return(res)
}


#### SENSOR ####
#' The Sensor GET Function
#'
#' This function allows you to GET Sensor information of client
#' @param client Client name
#' @param farm farm name
#' @param email email of user
#' @param api_key Api Key obtain from /auth
#' @param dash_param AgroSpace internal use parameter. Default value FALSE
#' @param url URL for dev purpose
#' @export
asapi_sensor_get = function(client, email, farm, api_key, url="https://api.agrospace.cl", dash_param=FALSE){
  param_query = list(client = client,
                     email = email,
                     farm = farm,
                     api_key=api_key,
                     dash_param=dash_param)

  res = httr::GET(url = asapi_url(url = url,endpoint = '/available_sensor'),
                  query = param_query)

  if(res$status_code==200){
    res = asapi_json(res)
  }else{
    res = httr::content(res,as = "text", encoding = "UTF-8")
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
#' @param dash_param AgroSpace internal use parameter. Default value FALSE
#' @param url URL for dev purpose
#' @export
asapi_index_get = function(client, email, farm, sensor, api_key, url="https://api.agrospace.cl", dash_param=FALSE){
  param_query = list(client = client,
                     email = email,
                     farm = farm,
                     sensor = sensor,
                     api_key=api_key,
                     dash_param=dash_param)
  res = httr::GET(url = asapi_url(url = url,endpoint = '/available_index'),
                  query = param_query)

  res = asapi_json(res)
  return(res)
}


#### RASTER - IMAGE ####

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
#' @param dash_param AgroSpace internal use parameter. Default value FALSE
#' @param url URL for dev purpose
#' @export
asapi_image = function(client, farm, sensor, index, date, email, api_key, url="https://api.agrospace.cl", dash_param=FALSE){
  param_query = list(client=client, farm=farm, sensor=sensor, index=index,
                     date=date, email=email, api_key=api_key, dash_param=dash_param)

  res = httr::GET(url = asapi_url(url = url,endpoint = '/image'),
                  query = param_query)
  if(res$status_code==200){
    res_file = asapi_json(res)
    if(index == "RGB"){
      rst = raster::brick(paste0('/vsicurl/',res_file$link))
      rst[rst>10000] <- NA
      rst[rst<0] <- NA
      names(rst) = paste0(index,1:3, "_", date)
    }else{
      rst = raster::raster(paste0('/vsicurl/',res_file$link))
      rst[rst==9999] <- NA
      rst[rst==-9999] <- NA
      names(rst) = paste0(index, "_", date)
      if(index == "BIOMASS"){rst[rst == 0] <- NA}
    }

    res_file$rst = rst
    return(res_file)

  }else{
    res = httr::content(res,as = "text", encoding = "UTF-8")
    message(res)
    return(res)
  }
}


#' The image GET Function
#'
#' This function brings a tiles to render directly in leaflet or QGIS
#' @param client Client name
#' @param farm Farm name to query
#' @param sensor Sensor
#' @param index Index
#' @param date imagen date: prod or dev
#' @param datasetid Dataset prod or dev
#' @param email email of user
#' @param api_key Api Key obtain from /auth
#' @param dash_param AgroSpace internal use parameter. Default value FALSE
#' @param url URL for dev purpose
#' @export
asapi_tiles = function(client, farm, sensor, index, date,datasetid, email, api_key, url="https://api.agrospace.cl", dash_param=FALSE){

   param_query = list(client=client, farm=farm, sensor=sensor, index=index,
                     date=date, datasetid=datasetid,email=email, api_key=api_key, dash_param=dash_param)

  res = httr::GET(url = asapi_url(url = url,endpoint = '/tiles'),
                  query = param_query)
  if(res$status_code==200){
    res = asapi_json(res)
    return(res)
  }else{
    message(httr::content(res,as = "text", encoding = "UTF-8"))
  }
}

#' The time series image GET Function
#'
#' This function allows you to GET the time series (ts) of raster images.
#' @param client Client name.
#' @param farm Farm name to query.
#' @param tableid The mean values for the field (levelzero) or for each paddock (levelone)
#' @param sensor Sensor.
#' @param index Index.
#' @param date_start date from, YYYY-MM-DD format.
#' @param date_end date to, YYYY-MM-DD format.
#' @param path Address where the raster images will be saved. Default "getwd()".
#' @param email email of user.
#' @param api_key Api Key obtain from /auth.
#' @param dash_param AgroSpace internal use parameter. Default value FALSE.
#' @param url URL for dev purpose
#' @export
asapi_ts_image = function(client, farm, tableid, sensor, index, date_start, date_end, path=getwd(),
                          email, api_key, url="https://api.agrospace.cl", dash_param=FALSE){
  Tabla = asapi_table(client = client, farm = farm, tableid = tableid, sensor = sensor,
                      index = index, date_start = date_start, date_end = date_end,
                      email = email, api_key = api_key)

  if(!is.character(Tabla)){
    dates = unique(Tabla$date)
    for(date in dates){
      raster = asapi_image(client=client, farm=farm, sensor=sensor, index=index,
                           date=date, email=email, api_key=api_key)
      raster::writeRaster(raster$rst, filename=paste0(path,"/", sensor,"_",index, "_", date,".tif"), format="GTiff", overwrite=F)
    }
    return("Download successful!")
  }else{
    return(Tabla)
  }
}



#### TERRAMODELS ####
#' The Terramodels POST Function
#'
#' This function set new task for downloading image indexes. This may take a some minutes.
#' @param client Client name
#' @param farm Farm name to query
#' @param sensor Sensor
#' @param index Index
#' @param date_start date from, YYYY-MM-DD format
#' @param date_end date to, YYYY-MM-DD format
#' @param type_export Type of export like UPGCLOUD or TOGCLOUD.
#' @param email email of user
#' @param api_key Api Key obtain from /auth
#' @param dash_param AgroSpace internal use parameter. Default value FALSE
#' @param url URL for dev purpose
#' @export
asapi_terramodels_post = function(client, farm, sensor, index, date_start, date_end, type_export,
                       email, api_key, url="https://api.agrospace.cl", dash_param=FALSE){
  param_query = list(client=client, farm=farm, sensor=sensor, index=index,
                     date_start=date_start, date_end=date_end, type_export=type_export,
                     email=email, api_key=api_key, dash_param=dash_param)

  res = httr::POST(url = asapi_url(url = url,endpoint = '/terramodels'),
                  query = param_query)

  if(res$status_code==200){
    res = do.call(cbind.data.frame, asapi_json(res))
    res = res$msg
  }else{
    res = httr::content(res,as = "text", encoding = "UTF-8")
    message(res)
  }
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
#' @export
asapi_features_get = function(index, farm_type, api_key, url="https://api.agrospace.cl"){
  param_query = list(index = index,
                     farm_type = farm_type,
                     api_key=api_key)
  res = httr::GET(url = asapi_url(url = url,endpoint = '/features'),
                  query = param_query)

  if(res$status_code==200){
    res = asapi_json(res)
  }else{
    res = httr::content(res,as = "text", encoding = "UTF-8")
    message(res)
  }

  return(res)
}

#' The Features POST Function
#'
#' This function Update feature info from googledrive to mongoDB.
#' Agrospace members only, with apikey master.
#' @param api_key Api Key obtain from /auth
#' @param url URL for dev purpose
#' @export
asapi_features_post = function(api_key, url="https://api.agrospace.cl"){
  res = httr::POST(url = asapi_url(url = url,endpoint = '/features'),
                  query = list(api_key=api_key))
  if(res$status_code=="201"){
    res = asapi_json(res)
  }else{
    message(res)
    res = asapi_json(res)
  }
  return(res)
}


#### OTHERS ENDPOINTS ####
#' The List GET Function
#'
#' This function allows you to GET list of farms or users of a client.
#' Agrospace members only, with apikey master.
#' @param client Client name
#' @param type list type: farm or user
#' @param api_key Api Key obtain from /auth
#' @param url URL for dev purpose
#' @export
asapi_list = function(client, type, api_key, url="https://api.agrospace.cl"){
  param_query = list(client = client,
                     type = type,
                     api_key = api_key)
  res = httr::GET(url = asapi_url(url = url,endpoint = '/list'),
                  query = param_query)
  res = asapi_json(res)

  return(res)
}

#' The Counted Calls GET Function
#'
#' This function get list of counted calls for a user.
#' Agrospace members only, with apikey master.
#' @param client Client name.
#' @param email Email of user.
#' @param api_key Api Key obtain from /auth.
#' @param url URL for dev purpose.
#' @export
asapi_counted = function(client, email, api_key, url="https://api.agrospace.cl"){
  param_query = list(client = client,
                     email = email,
                     api_key = api_key)
  res = httr::GET(url = asapi_url(url = url,endpoint = '/counted_calls'),
                  query = param_query)
  if(res$status_code==200){
    res = asapi_json(res)
    res$endpoint_calls = do.call(rbind.data.frame, res$endpoint_calls)
    res$month_calls = as.data.frame(res$month_calls)
    colnames(res$month_calls) = rownames(res$endpoint_calls)
  }else{
    res = httr::content(res, as = "text", encoding = "UTF-8")
    message(res)
  }
  return(res)
}


#### LOGO & LAYOUT ####

#' The Logo image GET Function
#'
#' This function returns the url of a client's logo with the API
#' @param client Client name
#' @param url URL for dev purpose
#' @export
asapi_logo = function(client, url="https://api.agrospace.cl"){
  res = httr::GET(url = asapi_url(url = url,endpoint = '/logo'),
                  query = list(client=client))
  if(res$status_code=="200"){
    res = asapi_json(res)
  }else{
    message(res)
    res = suppressMessages((asapi_json(res)))
  }
  return(res)
}

#' The Logo image POST Function
#'
#' This function upload a client's logo with the API
#' @param client Client name.
#' @param path Address of the new logo.
#' @param email email of user
#' @param api_key Api Key obtain from /auth
#' @param url URL for dev purpose
#' @export
asapi_logo_post = function(client, path, email, api_key, url="https://api.agrospace.cl"){
  res = httr::POST(url = asapi_url(url = url,endpoint = '/logo'),
                  query = list(client=client, path=path, email=email, api_key=api_key))
  if(res$status_code=="201"){
    res = asapi_json(res)
  }else{
    message(res)
    res = asapi_json(res)
  }
  return(res)
}

#' The Layout image GET Function
#'
#' This function returns the url of a farm's layout with the API
#' @param client Client name
#' @param farm Client name
#' @param url URL for dev purpose
#' @export
asapi_layout = function(client, farm, url="https://api.agrospace.cl"){
  res = httr::GET(url = asapi_url(url = url,endpoint = '/layout'),
                  query = list(client=client, farm=farm))
  if(res$status_code=="200"){
    res = asapi_json(res)
  }else{
    message(res)
    res = asapi_json(res)
  }
  return(res)
}

#' The Layout image POST Function
#'
#' This function upload a farms's layout with the API
#' @param client Client name.
#' @param farm Farm name.
#' @param path Address of the new logo.
#' @param email email of user
#' @param api_key Api Key obtain from /auth
#' @param url URL for dev purpose
#' @export
asapi_layout_post = function(client, path, farm, email, api_key, url="https://api.agrospace.cl"){
  res = httr::POST(url = asapi_url(url = url,endpoint = '/layout'),
                   query = list(client=client, farm=farm, path=path, email=email, api_key=api_key))
  if(res$status_code=="201"){
    res = asapi_json(res)
  }else{
    message(res)
    res = asapi_json(res)
  }
  return(res)
}

#### PLANES ####
#' The plan GET Function
#'
#' This function returns information on plans, with the API.
#' @param plan Name of the plan. You can use 'ALL' to call all available plans.
#' @param url URL for dev purpose
#' @export
asapi_planes_get = function(plan, url="https://api.agrospace.cl"){
  res = httr::GET(url = asapi_url(url = url,endpoint = '/planes'),
                  query = list(plan=plan))
  if(res$status_code=="200"){
    res = asapi_json(res)
  }else{
    message(res)
    res = asapi_json(res)
  }
  return(res)
}
