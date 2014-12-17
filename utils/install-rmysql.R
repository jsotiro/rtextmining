Sys.setenv(PKG_CPPFLAGS = "-I/usr/local/mysql/include")
Sys.setenv(PKG_LIBS = "-L/usr/local/mysql/lib -lmysqlclient")
file.symlink("/usr/local/mysql/lib/libmysqlclient.18.dylib","/Library/Frameworks/R.framework/Resources/lib")
install.packages("RMySQL", type = "source")
