
# Function from afscdata, courtesy B Williams
connect <- function(db = "akfin") {
  
  # check if database name is stored in keyring, if not request user/pwd
  if(!(db %in% keyring::key_list()[,1])) {
    user <- getPass::getPass(paste("Enter", db, "username: "))
    pwd <- getPass::getPass(paste("Enter", db, "password: "))
  } else {
    user <- keyring::key_list(db)$username
    pwd <-  keyring::key_get(db, keyring::key_list(db)$username)
  }
  # connect to server
  DBI::dbConnect ( odbc::odbc(),
                   db,
                   uid = user,
                   pwd = pwd )
}