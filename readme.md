# Follow me to create a web-tool by shiny 

### Publications

Please cite our latest paper when using our TFmapper

- https://www.ncbi.nlm.nih.gov/pmc/articles/PMC6216026/
- http://www.ijbs.com/v14p1724.htm
- http://www.tfmapper.org/

### Contact

Jianming Zeng (PHD student in university of Macau) : jmzeng1314@163.com 

## Follow me 

### step1: create tables and database in MYSQL

First, make sure that the mysql client and server were installed successfully in your OS , and please remember the **password** for root user( the default user in mysql). 

Then you can log in by  `mysql -u root -p  `  

useful link ( once you forget the password ):

- https://www.variphy.com/kb/mac-os-x-reset-mysql-root-password

- https://stackoverflow.com/questions/6474775/setting-the-mysql-root-user-password-on-os-x

```mysql
show databases; 
create database tfmapperdb;
show databases;
CREATE USER tfmapperuser IDENTIFIED BY 'tfmapper_@Abc';
  GRANT ALL PRIVILEGES ON tfmapperdb.* TO 'tfmapperuser'@'%' IDENTIFIED BY 'tfmapper_@Abc';
FLUSH PRIVILEGES;
```

Now, you just need to use the tfmapperdb and tfmapperuser.



### step2:





### Papers citing [TFmapper](www.tfmapper.org/)

