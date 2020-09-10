


###########################
#### Table Creation #######
###########################

library("RMySQL")

#Function to open the sql connection

open_mysql<- function(db_name){
  db<- dbConnect(MySQL(), user="", password='', dbname= db_name, 
                 host='')
  return(db)
}

#Creates the MySql database
db<- open_mysql('')

#############################
#Creates the Data Type Table
#############################


#Drops table if it is already in the database
dbGetQuery(db, "drop table DataType")

#Creates the DataType Table
dbGetQuery(db, "create table DataType(DTypeID int PRIMARY KEY, DType varchar(40))")


#Function for writing the table, includes the append = T
table_writing<- function(db_con,table_name,info_keys){
  dbWriteTable(db_con, name=table_name, value=info_keys, overwrite=F, append=T, row.names=F)
}

#Inserts the data into the table from the data prep area
#Also provides the exact same colnames
colnames(ath_exp_prime)<-c("DTypeID","DType")
table_writing(db, "DataType", ath_exp_prime)

#Queries and saves the data_type table
data_type<- dbGetQuery(db,"select * from DataType")


#Creates the Data table - necessary to make it Data1 for 
# the PHP website - fewer errors are thrown
#Drops the table if it is in the database
dbGetQuery(db, "drop table Data1")

#Creates the Data1 table
dbGetQuery(db, "create table Data1(DataID int PRIMARY KEY, 
           DTypeID_DataType int,
           DataName varchar(40),
           FOREIGN KEY(DTypeID_DataType) references DataType(DTypeID))")

#Inserts the data into the table from the data prep area
#Also provides the exact same colnames
colnames(All_Ids_Prime2)<-c("DataID","DTypeID_DataType","DataName")
table_writing(db,"Data1",All_Ids_Prime2)

#Queries and saves the data1 table table
filled_data_table2<-dbGetQuery(db, "select * from Data1")


#Drops Dattributetype table if it already exists
dbGetQuery(db, "drop table DAttributeType")

# Creates attribute type table
dbGetQuery(db, "create table DAttributeType(
           DATypeID int PRIMARY KEY,
           DAType varchar(40))")

#Inserts the data into the table from the data prep area
#Also provides the exact same colnames
colnames(att_prime)<-c("DATypeID","DAType")
table_writing(db, "DAttributeType", att_prime)


#Queries and saves the DAttributeType table
attribute_type<- dbGetQuery(db, "select * from DAttributeType")
write.csv(attribute_type,"DAttributeType.csv")

#Creating Attribute table and drops the Dattribute table if it already exists
dbGetQuery(db,"drop table DAttribute")
dbGetQuery(db, "create table DAttribute(
           DAttributeID int PRIMARY KEY,
           DATypeID_DAttributeType int,
           DataID_Data int,
           DAValue varchar(40),
           FOREIGN KEY(DATypeID_DAttributeType) 
           references DAttributeType(DATypeID),
           FOREIGN KEY(DataID_Data)
           references Data1(DataID))")

#Inserts the data into the table from the data prep area
#Also provides the exact same colnames
#Need to makes sure it is a data frame and not data table
test.all4.prime<-as.data.frame(test.all4.prime)
colnames(test.all4.prime)<-c("DAttributeID","DATypeID_DAttributeType",
                             "DataID_Data","DAValue")

table_writing(db, "DAttribute", test.all4.prime)

#Queries and saves the DAttribute table 
Dattribute<- dbGetQuery(db, "select * from DAttribute")

#Creating RelationshipType and dropping it if it already exists
dbGetQuery(db, "drop table RelationshipType")
dbGetQuery(db, "create table RelationshipType(
           RTypeID int PRIMARY KEY,
           RType varchar(40))")

#Inserts the data into the table from the data prep area
#Also provides the exact same colnames
colnames(relationships.prime)<-c("RTypeID","RType")
table_writing(db, "RelationshipType", relationships.prime)

#Queries and saves the DAttribute table 
Relationtype<- dbGetQuery(db, "select * from RelationshipType")

#Creating RelationshipTable and dropping it if it already exists
dbGetQuery(db, "drop table Relationship")
dbGetQuery(db, "create table Relationship(
           RelationshipID int PRIMARY KEY,
           DataID_Data1 int,
           DataID_Data2 int,
           RTypeID_RelationshipType int,
           Foreign Key(DataID_Data1)
           references Data1(DataID),
           Foreign Key(DataID_Data2) 
           references Data1(DataID),
           Foreign Key(RTypeID_RelationshipType)
           references RelationshipType(RTypeID))")

#Inserts the data into the table from the data prep area
#Also provides the exact same colnames
colnames(Relationshiptable)<-c("RelationshipID","DataID_Data1","DataID_Data2",
                               "RTypeID_RelationshipType")
table_writing(db, "Relationship", Relationshiptable)

#Queries and saves the Relationship table
Relationshiptable_Filled<-dbGetQuery(db,"select * from Relationship")

#Creating RelationshipValues Table and drops it if it already exists
dbGetQuery(db, "drop table RelationshipValue")
dbGetQuery(db, "create table RelationshipValue(
           RValueID int,
           RelationshipID_Relationship int,
           RValue float,
           Foreign Key(RelationshipID_Relationship)
           references Relationship(RelationshipID))")

#Inserts the data into the table from the data prep area
#Also provides the exact same colnames
colnames(relationship_values)<-c("RValueID","RelationshipID_Relationship","RValue")
table_writing(db,"RelationshipValue",relationship_values)

#Queries and saves the Relationship table
relationshipvalues_filled<-dbGetQuery(db, "select * from RelationshipValue")
