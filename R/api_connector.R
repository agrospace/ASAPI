#====================================#
# FUNCIONES PARA VISUALIZAR Fx's API #
#====================================#

#### API KEY ####

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
#' @keywords API_KEY, api_key
#' @export
#' @examples
#' asapi_auth(email="userexample@agrospace.cl", password="contra1234")
#'
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
#' @keywords API_KEY, api_key
#' @export
#' @examples
#' asapi_new_apikey(email="user.example@agrospace.cl", password="contra1234")
#'
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
#' @keywords API_KEY, api_key
#' @export
#' @examples
#' asapi_new_pass(email="user.example@agrospace.cl", old_password="contra1234", new_password="CONTRA111")
#'
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
#' @param url URL for dev purpose
#' @keywords API_KEY, api_key
#' @export
#' @examples
#' asapi_user_get(client="clientexample", email="user.example@agrospace.cl", user="userexample", api_key="APIKEYEXAMPLE")
asapi_user_get = function(client, email, user, api_key, url="https://api.agrospace.cl"){
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
#' @keywords API_KEY, api_key
#' @export
#' @examples
#' asapi_user_post(client="clientexample", user_name="New User Example", email_user="newuser.example@agrospace.cl", password="Contra123", rol="user", email="user.example@agrospace.cl", api_key="APIKEYEXAMPLE")
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
#' @keywords API_KEY, api_key
#' @export
#' @examples
#' asapi_user_put(client="clientexample", email="user.example@agrospace.cl", new_username="EDIT New User", email_admin="user.example@agrospace.cl", api_key="APIKEYEXAMPLE")
asapi_user_put = function(client, email, new_username="NULL", new_email="NULL", new_rol="NULL", email_admin, api_key, url="https://api.agrospace.cl"){
  param_query = list(client=client, email=email, new_username=new_username,
                     new_email=new_email, new_rol=new_rol,
                     email_admin=email_admin, api_key=api_key)

  res = httr::PUT(url = asapi_url(url = url,endpoint = '/user'),
                  query = param_query)


  if(res$status_code==201){
    res = httr::content(res)
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
#' @keywords API_KEY, api_key
#' @export
#' @examples
#' asapi_user_delete(client="clientexample", email_deleted="newuser.example@agrospace.cl", email="user.example@agrospace.cl", api_key="APIKEYEXAMPLE")
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
#' @param url URL for dev purpose
#' @keywords api_key
#' @export
#' @examples
#' asapi_client_get(client="clientexample", email="user.example@agrospace.cl", api_key="APIKEYEXAMPLE")
asapi_client_get = function(client, email, api_key, url="https://api.agrospace.cl"){
  param_query = list(client = client,email = email,api_key=api_key)

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
asapi_farm_get = function(client, farm, email, api_key, url="https://api.agrospace.cl"){
  param_query = list(client = client, farm = farm,email = email,api_key = api_key)

  res = httr::GET(url = asapi_url(url = url,endpoint = '/farm'),
                  query = param_query)

  if(res$status_code==200){
    if(param_query$farm=="ALL"){
      res = httr::content(res)
      return(list(response = res))
    }else{
      res = asapi_json(res)
      vector_farm = res$location$vector[[1]]
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
#' @param farm Farm name to query
#' @param email email of user
#' @param api_key Api Key obtain from /auth
#' @param url URL for dev purpose
#' @keywords Farms
#' @export
#' @examples
#' asapi_farm_post(client="clientexample", farm_name="farm.example", email="user.example@agrospace.cl", api_key="APIKEYEXAMPLE")
asapi_farm_post = function(client, farm_name, geojson="NULL", email, api_key, url="https://api.agrospace.cl"){
  param_query = list(client=client, farm_name=farm_name, geojson=geojson, email=email, api_key=api_key)

  res = httr::POST(url = asapi_url(url = url,endpoint = '/farm'),
                  query = param_query)

  if(res$status_code==201){
    res = httr::content(res)
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
#' @keywords Farms
#' @export
#' @examples
#' asapi_farm_put(client="clientexample", farm="farm2example", new_region="Region de los Lagos", email_admin="user.example@agrospace.cl", api_key="APIKEYEXAMPLE")
asapi_farm_put = function(client, farm, new_type="NULL", new_region="NULL", new_commune="NULL", new_geojson="NULL", email_admin, api_key, url="https://api.agrospace.cl"){
  param_query = list(client=client, farm=farm, new_type=new_type, new_region=new_region,
                     new_commune=new_commune, new_geojson=new_geojson, email_admin=email_admin, api_key=api_key)
  res = httr::PUT(url = asapi_url(url = url,endpoint = '/farm'),
                  query = param_query)

  if(res$status_code==201){
    res = httr::content(res)
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
#' @keywords Farms
#' @export
#' @examples
#' asapi_farm_delete(client="clientexample", farm="farmexample", email="user.example@agrospace.cl", api_key="APIKEYEXAMPLE")
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
#' @param url URL for dev purpose
#' @keywords API_KEY, api_key
#' @export
#' @examples
#' asapi_table(client="clientexample", farm="farm1example", tableid="levelzero", sensor="S2SR", index="NDVI", date_start="2019-12-05", date_end="2021-02-07", email="user.example@agrospace.cl", api_key="APIKEYEXAMPLE", url="https://api.agrospace.cl")
asapi_table = function(client, farm, tableid, sensor, index,
                       date_start, date_end, email, api_key, url="https://api.agrospace.cl"){
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
#' @param url URL for dev purpose
#' @keywords API_KEY, api_key
#' @export
#' @examples
#' asapi_table_cover(client="clientexample", farm="farm1example", tableid="levelzero", sensor="S2SR", index="NDVI", date_start="2020-12-04", date_end="2020-12-19", email="user.example@agrospace.cl", api_key="APIKEYEXAMPLE", url="https://api.agrospace.cl")
asapi_table_cover = function(client, farm, tableid, sensor, index,
                             date_start, date_end, email, api_key, url="https://api.agrospace.cl"){
  param_query = list(client=client, farm=farm,
                     tableid=tableid, sensor=sensor, index=index,
                     date_start=date_start, date_end=date_end,
                     email=email, api_key=api_key)

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
#' @param url URL for dev purpose
#' @keywords API_KEY, api_key
#' @export
#' @examples
#' asapi_sensor_get(client="clientexample", email="user.example@agrospace.cl", farm="farm1example", api_key="APIKEYEXAMPLE")
asapi_sensor_get = function(client, email, farm, api_key, url="https://api.agrospace.cl"){
  param_query = list(client = client,
                     email = email,
                     farm = farm,
                     api_key=api_key)

  res = httr::GET(url = asapi_url(url = url,endpoint = '/available_sensor'),
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
asapi_index_get = function(client, email, farm, sensor, api_key, url="https://api.agrospace.cl"){
  param_query = list(client = client,
                     email = email,
                     farm = farm,
                     sensor = sensor,
                     api_key=api_key)
  res = httr::GET(url = asapi_url(url = url,endpoint = '/available_index'),
                  query = param_query)

  res = httr::content(res)
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
#' asapi_image(client="clientexample", farm="farm1example", sensor="S2SR", index="NDVI", date="2021-02-07", email="user.example@agrospace.cl", api_key="APIKEYEXAMPLE")
asapi_image = function(client,farm,sensor,index,date,email,api_key, url="https://api.agrospace.cl"){
  param_query = list(client = client, farm = farm,sensor = sensor,
                     index = index,date = date,email = email,api_key =  api_key)

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
asapi_features_get = function(index, farm_type, api_key, url="https://api.agrospace.cl"){
  param_query = list(index = index,
                     farm_type = farm_type,
                     api_key=api_key)
  res = httr::GET(url = asapi_url(url = url,endpoint = '/features'),
                  query = param_query)
  res = asapi_json(res)

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
#' @keywords API_KEY, api_key
#' @export
#' @examples
#' asapi_list(client="clientexample", type="farm", api_key="APIKEYEXAMPLE")
asapi_list = function(client, type, api_key, url="https://api.agrospace.cl"){
  param_query = list(client = client,
                     type = type,
                     api_key = api_key)
  res = httr::GET(url = asapi_url(url = url,endpoint = '/list'),
                  query = param_query)
  res = httr::content(res)
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
#' @keywords API_KEY, api_key
#' @export
#' @examples
#' asapi_counted(client="clientexample", email="user.example@agrospace.cl", api_key="APIKEYEXAMPLE")
asapi_counted = function(client, email, api_key, url="https://api.agrospace.cl"){
  param_query = list(client = client,
                     email = email,
                     api_key = api_key)
  res = httr::GET(url = asapi_url(url = url,endpoint = '/counted_calls'),
                  query = param_query)
  if(res$status_code==200){
    res = httr::content(res)
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
#' @keywords API_KEY, api_key
#' @export
#' @examples
#' asapi_logo(client="clientexample")
asapi_logo = function(client, url="https://api.agrospace.cl"){
  res = httr::GET(url = asapi_url(url = url,endpoint = '/logo'),
                  query = list(client=client))
  if(res$status_code=="200"){
    res = asapi_json(res)
  }else{
    message(res)
    res = asapi_json(res)
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
#' @keywords API_KEY, api_key
#' @export
#' @examples
#' asapi_logo_post(client="clientexample", path="https://pbs.twimg.com/profile_images/1063094754228011008/-AN6PKnB.jpg", email="user.example@agrospace.cl", api_key="APIKEYEXAMPLE")
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
#' @keywords API_KEY, api_key
#' @export
#' @examples
#' asapi_layout(client="clientexample", farm="farm1example")
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
#' @keywords API_KEY, api_key
#' @export
#' @examples
#' asapi_layout_post(client="clientexample", farm="farm1example", path="", email="user.example@agrospace.cl", api_key="APIKEYEXAMPLE")
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
