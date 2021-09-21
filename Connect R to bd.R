##---------------------Required packages-------------------------
library(dplyr)
library(stringr)
library(DBI)
Sys.setenv(Java_HOME = "C:/Users/hendricks.tenin/Desktop/jre1.8.0_181/")
library(RJDBC)
library(rJava)
install.packages("rJava")




##--------------------------------Loaded Data--------------------------------------------



jdbcDriver =JDBC("oracle.jdbc.OracleDriver",classPath="C:/Users/hendricks.tenin/Desktop/Data Science/JDBC/ojdbc6.jar")

con =dbConnect(jdbcDriver, "jdbc:oracle:thin:@//192.6.250.116:1521/milci", "HTENIN", "MERCITENIN")


dbColumnInfo(dbSendQuery(con, "SELECT * from v_ch_affaire"))

