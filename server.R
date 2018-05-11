##Interactive webportal for international wildlife trade evidence map

##server

##Main server.R for beta tool
source("sources.R")
source("server_explore.R")

shinyServer(function(input,output,session){
  env_serv=environment()
  elink(env_serv)
  eintout(env_serv)

})