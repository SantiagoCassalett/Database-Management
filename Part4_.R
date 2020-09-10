

#Function that requires the input of the output files name - builds the gff from the database

gff_builder<- function(output_file_name){

#####################
#### LIBRARY ########
#####################

library(RMySQL)

####################################
##### Tair10 Database Loading ######
####################################

#Loading the database
db<- dbConnect(MySQL(), user="",password='', dbname="tair10", host='')


#Selects all of the tables from the Tair10 Database
typelist<-dbGetQuery(db, "select * from typelist")
attribute_table<-dbGetQuery(db, "select * from attribute")
feature<- dbGetQuery(db, "select * from feature")
attribute_list<-dbGetQuery(db, "select * from attributelist")
location_list<-dbGetQuery(db, "select * from locationlist")
name<- dbGetQuery(db, "select * from name")


#Selects the chr1 from the attributes table
chr1<-dbGetQuery(db, "select attribute_value from attribute where attribute_value = \"chr1\" ")

############################
### TAIR10 GFF BUILDING ####
############################

#Following codes creates the first and second columns in the GFF file
#the seqid and the source columns

#Length of the feature
all_num<-c(1:length(feature$id))

#replicates the chr1 attribute for the length of the data tables
all_num2<-as.data.frame(rep(chr1$attribute_value,length(all_num)))

#Creates the seqid column
first_df<-data.frame(all_num2)

#Replicates TAIR10 for the length of the data tables
all_num3<-as.data.frame(rep("TAIR10", length(all_num)))

#Combines the seqid column and the source column
first_df<-data.frame(first_df,all_num3)

#The code below creates the third column in the gff file - the feature

typeid<-as.data.frame(feature$typeid)

#Converts the typeid from the feature table into factors
fact.typeid<-factor(typeid$`feature$typeid`)

#Renames the levels of factors based on the names in the typelist table
levels(fact.typeid)<-typelist$tag
fact.typeid_tag<-as.data.frame(as.character(fact.typeid))

#Turns the factors back into characters
type.list<-as.character(fact.typeid_tag$`as.character(fact.typeid)`)

#Splits the character so that the TAIR10 portion can be removed
type.list2<-strsplit(type.list,":")
type.list3<-as.data.frame(unlist(type.list2))

#Leaves on the type withouth the TAIR10 portion
type.list4<-as.data.frame(type.list3[type.list3$`unlist(type.list2)`!="TAIR10",])

#Combines the first three columns of the GFF file df
second_df<-data.frame(first_df, test4)

#Combines the start and end locations to the GFF file df
third_df<-data.frame(second_df, feature$start,feature$end)

#Creates the score column and combines the first six columns
all_num4<-as.data.frame(rep(".", length(all_num)))
fourth_df<- data.frame(third_df, all_num4)

#The code below creates the strand column and adds it to the df
strand<-as.data.frame(feature$strand)

#Turns the strand into a factor
fact.strand<-factor(strand$`feature$strand`)

#Replaces the levels with the characters -,.,+
levels(fact.strand)<-c("-",".","+")
fact.strand2<-fact.strand
fact.strand2<-as.data.frame(fact.strand2)

#Adds the strand column into the GFF file df
fifth_df<-data.frame(fourth_df,fact.strand2$fact.strand2)

#Creates the frame column and adds it to the df as the characters "."
sixth_df<- data.frame(fifth_df,all_num4)

#The code below creates the feature column for the GFF file df

#Is only the numbers associated with the feature ids - row numbers
feature_num<-as.data.frame(feature$id)

#the associated ids from the name table
name_id<-name$id

#Checks which the name ids are present in the feature row numbers 
#returns a list of Trues and Falses 
falses<-as.data.frame(feature_num$`feature$id` %in% name_id)

falses_feature<-data.frame(falses,feature_num$`feature$id`)

falses2<-falses_feature$feature_num..feature.id...in..name_id

#Turns the logical into characters
falses2<-as.character(falses2)

#Replaces the "Trues" with the names associated in the names table
replaced_false<-ifelse(falses2=="TRUE",name$name,".")

#Adds the Feature column to the Gff file df
seventh_df<- data.frame(sixth_df,replaced_false)

colnames(seventh_df)<- c("Sequence","Source","Feature","Start","End","Score","Strand","Frame",
                         "Attributes")

#The final TAIR10.gff data.frame and writes it to a csv file
TAIR10.gff<-seventh_df
write.csv(TAIR10.gff,output_file_name,row.names = F)
}

#Running the final function
gff_builder("TAIR10_GFF.csv")
