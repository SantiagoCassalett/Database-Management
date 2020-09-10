#Script for preping the data in order for it to be inserted in the database

##################
#### LIBRARY #####
##################

library(data.table)

#############################
##### DataType Prep ########
#############################

#Function for opening the files with header = F and turns any blanks into NAs
open_file<- function(file_txt){
  data_values<- read.delim(file_txt,na.strings=c("","NA"),header=F)
  return(data_values)
}

#Uses the open_file function to open this file
ath<-open_file("AthBiomart.txt")

#Cant use the open_file function because of the need to sep =""
exp<- read.delim("expvalues.txt",sep = "", header=T, row.names = NULL)

#Changes the colnames to those below because header=F places control1 in row 1
colnames(exp)<- c("Exp_ID", "Control1", "Control2", "Control3", 
                  "Treatment1", "Treatment2", "Treatment3")

# ensures that the strings are not treated as factors for later on
colexp<-as.data.frame(colnames(exp), stringsAsFactors = F)



#Function to select the headers as name and transpose them
select_header_as_name<- function(data_name){
  header_name<-cbind(data_name[1,])
  
  #Removes any NAs that may be in the row
  header_name<-header_name[,colSums(is.na(header_name))==0]
  
  #Transposes the row into a column
  header_name<-t(header_name)
  header_name<-as.data.frame(header_name)
  return(header_name)
}

#Seperates the headers from the two dataframes
#Ath and exp headers
ath_headers<-select_header_as_name(ath)

#These are the headers for the DataType table - removes all the other headers
ath_headers<- ath_headers[-c(3,4,7),]
ath_headers<-as.data.frame(ath_headers)

exp_headers<- select_header_as_name(colexp)


#headers combined together
#Necessary to rbind these dataframes

names(ath_headers)<-names(exp_headers)

ath_exp<-rbind(ath_headers, exp_headers)


#Creating the primary keys  - function that takes the length of the input
# and turns it into a number row
creating_prim_key<- function(info){
  numbers<- c(1:length(info[,1]))
  num_info<- cbind(numbers,info)
  return(num_info)
}

# Contains the primary key for the headers and the headers combined
#Necessary for the Data Type Table
ath_exp_prime<- creating_prim_key(ath_exp)

###########################
#### Data Table Prep ######
###########################

#Data table contains the values or names from the headers

exp_ID<- as.data.frame(unique(as.character(exp$Exp_ID)))
gene_stable_id<- as.data.frame(unique(as.character(ath$V1[-1])))
transcipt_id<- as.data.frame(unique(as.character(ath$V2[-1])))
Affy_id<- as.data.frame(unique(as.character(ath$V5[-1])))
go_accession<- as.data.frame(unique(as.character(ath$V6[-1])))

#Creating Primary Keys for each of the headers using the creating_prim_key function
exp_id_prim<-creating_prim_key(exp_ID)
gene_stable_prime<-creating_prim_key(gene_stable_id)
transcript_prime<-creating_prim_key(transcipt_id)
affy_prime<-creating_prim_key(Affy_id)
go_accession<-creating_prim_key(go_accession)

#Creating dataframes that place the number from the foreign key with the
#actual values - the primary key from the datatype table
exp_id_prim2<-cbind(rep(5,length(exp_id_prim$numbers)),exp_id_prim)

#Function that does the above
rep_prime<- function(number,prime_number,prime){
  variable<-cbind(rep(as.numeric(number),length(prime_number)),prime)
}

#runs the rep_prime function that combines the foreign key with each data value
gene_stable_prime2<- rep_prime(1, gene_stable_prime$numbers, gene_stable_prime)
transcript_prime2<- rep_prime(2, transcript_prime$numbers, transcript_prime)
affy_prime2<- rep_prime(3, affy_prime$numbers, affy_prime)
go_accession2<-rep_prime(4,go_accession$numbers,go_accession)

#Have to do the names<-names for each prime individually - wont allow rbind without
#test is actually each prime added on to the next

names(gene_stable_prime2)<-names(transcript_prime2)
test<-rbind(gene_stable_prime2,transcript_prime2)
names(test)<-names(affy_prime2)
test<-rbind(test,affy_prime2)
names(test)<-names(go_accession2)
test<-rbind(test, go_accession2)
names(test)<-names(exp_id_prim2)
All_Ids_Prime<-rbind(test,exp_id_prim2)

#removes the "numbers" column which was the primary key for each data value
All_Ids_Prime2<-All_Ids_Prime[,-2]

#Actual values used to fill the data table
# Is the primary key for the entire data table and the data values themselves
All_Ids_Prime2<-creating_prim_key(All_Ids_Prime2)


#####################################
#### Attribute Type Data Prep #######
#####################################

#Ath attribute headers 
ath_att<-select_header_as_name(ath)

#only those headers that are Attributes
ath_att<- ath_att[-c(1,2,5,6),]
ath_att<-as.data.frame(ath_att)

#Combines the attribute types with the primary keys
att_prime<-creating_prim_key(ath_att)

##########################################
### Relationship Type Table Data Prep ####
##########################################

#Creates the relationship type
relationships<- as.data.frame(c("Transcript2Gene","Goterm2Gene","AffyID2Gene",
                                "Experiment2AffyID"))

#Combines the primary keys with the relationship types
relationships.prime<-creating_prim_key(relationships)


########################
#Relationship Table Prep
########################

#Below are many different sections - each with a header for
#each part of the relationship table

### ! IMPORTANT NOTE ! ####
#In order to run this section it is necessary to first build and
#insert the values into the Data Table

#Filled_data_table2 is the output from select * from DataTable
filled_data_table2.repeat<-filled_data_table2

#Turns the output into a data table for easier parsing
filled_data_table2.repeat<-as.data.table(filled_data_table2.repeat)

#selects the gene name, stable id name, go name and affy name
# using the data type foreign key - will be used further down
Gene.name<-filled_data_table2.repeat[DTypeID_DataType=="1"]
stable.name<-filled_data_table2.repeat[DTypeID_DataType=="2"]
go.name<-filled_data_table2.repeat[DTypeID_DataType=="4"]
affy.name<-filled_data_table2.repeat[DTypeID_DataType=="3"]


# Need all to not be unique for matching
exp_ID.nouni<- as.data.frame(as.character(exp$Exp_ID))
gene_stable_id.nouni<- as.data.frame(as.character(ath$V1[-1]))
transcipt_id.nouni<- as.data.frame(as.character(ath$V2[-1]))
Affy_id.nouni<- as.data.frame(as.character(ath$V5[-1]))
go_accession.nouni<- as.data.frame(as.character(ath$V6[-1]))

##################################################
#For the first relationship Type - transcript2gene
##################################################

# Binds together the gene_stable id and the transcript ID
test.bind<-as.data.frame(cbind(gene_stable_id.nouni,transcipt_id.nouni))
test.bind<-as.data.table(test.bind)

#After binding - uniques them
test.bind2<-unique(test.bind)

#stable.name$DataID is the number for the transcript ID
test.bind2<-cbind(test.bind2,stable.name$DataID)

length(test.bind2$V2)

# Length of test.bind2 - will provide the foreign key from the data table
test.nums<-c(1:35386)

#Binding the data values with the primary key
test.bind2<-cbind(test.nums, test.bind2)

#Removes the extra columns
test.bind2<-as.data.frame(test.bind2[,-2])
test.bind2[,-2]

# The rep_prime function is used to create the foreign key addition
# The foreign key is from the relationship type table
test.bind3<-rep_prime(1,test.nums,test.bind2)

# How to get the factors to order correctly
actual_numbers<-test.bind3$`as.character(ath$V1[-1])`
actual_numbers

#Changes the levels to the number 1:27416 which is the foreign key from the data table
levels(actual_numbers)<-c(1:27416)

#The final numbers associated with data values that will be used in the Relationship table
actual<-factor(actual_numbers,levels=unique(as.numeric(actual_numbers)))
as.numeric(actual)

#Binds together the primary key, the foreign key and the actual values
test.bind4<-cbind(test.bind3,as.numeric(actual))

#removes extra columns
test.bind4<-test.bind4[,-3]

#Provides the correct names for the relationship values
colnames(Transcript2Gene)<-c("Primary_Key","Transcript_name","Gene_name","Relationship_type")

#Final transcript2gene columns
Transcript2Gene<-as.data.frame(cbind(test.bind4$Primary_Key,test.bind4$Stable_name,test.bind4$Gene_name,
                                     test.bind4$Relationship_type))

###########################################
#For the second relationship type - go2gene
###########################################


#The go accession numbers and the gene_stable ids
go_accession.nouni<- as.data.frame(as.character(ath$V6[-1]))
gene_stable_id.nouni<- as.data.frame(as.character(ath$V1[-1]))

#Binds together the two different data types
go2gene.bind<-as.data.frame(cbind(gene_stable_id.nouni,go_accession.nouni))
go2gene.bind<-as.data.table(go2gene.bind)

#Uniques the data table
go2gene.bind2<-unique(go2gene.bind)

#matching the repeat go-terms with their number
go.num<-go2gene.bind2$`as.character(ath$V6[-1])`

#Changing the levels in order the values
levels(go.num)<-c(1:6147)

#Combining the ordered factors with the correct numbers
go.actual<-factor(go.num,levels=unique(as.numeric(go.num)))

#This is necessary in order to them provide the go.actual with the correct
# foreign keys from the data table
see<-as.numeric(go.actual)
see2<-as.character(see)
see2<-factor(see)
levels(see2)<-c(84339:90485)

#binding the new values back into the combined data table
go2gene.bind2<-cbind(go2gene.bind2,see2)

#matching the names with the go terms and their number
actual_numbers<-go2gene.bind2$`as.character(ath$V1[-1])`
actual_numbers

#Again this is necessary in order to order the data values
levels(actual_numbers)<-c(1:27416)
actual<-factor(actual_numbers,levels=unique(as.numeric(actual_numbers)))
as.numeric(actual)
as.character(see2)

#binding the new values
go2gene.bind2<-cbind(go2gene.bind2,as.numeric(actual))
go2gene.bind3<-as.data.frame(cbind(as.character(see2),go2gene.bind2$V2))

#providing the correct column names for the relationship
colnames(go2gene.bind3)<-c("Go_name","Gene_names")
length(go2gene.bind3$Go_name)

#Test.nums is the correct foreign key from the data table
test.nums<-c(35387:192421)
length(test.nums)

# Finally binding the foreign key from the relationship type table with
# the go terms and the gene stable ids
go2gene.bind3<-as.data.frame(cbind(test.nums,go2gene.bind3))
go2gene.bind4<-rep_prime(2,test.nums,go2gene.bind3)
go2gene.bind4<-data.frame(go2gene.bind4$test.nums,go2gene.bind4$Go_name,
                          go2gene.bind4$Gene_names,go2gene.bind4$`rep(as.numeric(number), length(prime_number))`)
colnames(go2gene.bind4)<-c("Primary_Key","Go_names","Gene_names","Relationship_Type")

###################
#Affy ID 2 Gene
###################

#Provides the affy ids and the gene stable ids
Affy_id.nouni<- as.data.frame(as.character(ath$V5[-1]))
gene_stable_id.nouni<- as.data.frame(as.character(ath$V1[-1]))

#Similar as above - binds together the two data types together
aff2gene<-as.data.frame(cbind(Affy_id.nouni,gene_stable_id.nouni))
aff2gene<-as.data.table(aff2gene)

#Uniques them
aff2gene2<-unique(aff2gene)

#Affy part 
#matching the names with the affy terms and their number
affy<-aff2gene2$`as.character(ath$V5[-1])`
affy
affy_num<-as.data.frame(affy.name$DataID)

#Same as above for ordering the values
levels(affy)<-c(1:21535)
affy.actual<-factor(affy,levels=unique(as.numeric(affy)))
as.numeric(affy.actual)

#Provides the correct foreign key from the data table
levels(affy.actual)<-c(62803:84338)
affy.actual
aff2gene3<-data.frame(affy.actual,aff2gene2)

#Genes part
#The same as above for matching the terms and the numbers
actual_numbers<-aff2gene3$as.character.ath.V1..1..
actual_numbers
levels(actual_numbers)<-c(1:27416)
actual<-factor(actual_numbers,levels=unique(as.numeric(actual_numbers)))
as.numeric(actual)


# Combines together the affy terms and the gene stable ids
aff2gene3<-data.frame(aff2gene3$affy.actual,as.numeric(actual))

#Provides the correct names for the columns
colnames(aff2gene3)<-c("Affy_Id","Gene_names")
length(aff2gene3$Affy_Id)

#This is the primary key for the relationship
test.nums<-c(192422:220380)

aff2gene4<-data.frame(test.nums,aff2gene3)

#Provides the foreign key from the relationship type table
aff2gene5<-rep_prime(3,test.nums,aff2gene4)

#Final affy2gene stable data frame
aff2gene5<-data.frame(test.nums,aff2gene5$Affy_Id,aff2gene5$Gene_names,
                      aff2gene5$`rep(as.numeric(number), length(prime_number))`)
colnames(aff2gene5)<-c("Primary Key","Affy_Id","Gene_Names","Relationship Type")

#####################
#Experiment2Affy
######################

#Provides the experiment IDs and the Affy IDs
exp_ID.nouni<- as.data.frame(as.character(exp$Exp_ID))
Affy_id.nouni<- as.data.frame(as.character(ath$V5[-1]))

affy.name<-filled_data_table2.repeat[DTypeID_DataType=="3"]
exp.id<-filled_data_table2.repeat[DTypeID_DataType=="5"]

#Sorting the affy IDS but keeping the number associated with them from the datatable
affy.name_name<-as.character(affy.name$DataName)
affy.name_num<-affy.name$DataID

sort.affy<-as.data.frame(sort(as.character(affy.name_name)))

affy_and_num<-as.data.frame(cbind(affy.name_name,affy.name_num))

#This code sorts the affy ids because they do not match 1-1 with the experiment ids
sort.affy<-affy_and_num[order(affy_and_num$affy.name_name,affy_and_num$affy.name_num),]

#omits any nas from teh affy data 
sort.affy2<-na.omit(sort.affy)


#Again provides the affy information
aff.test<-as.data.frame(as.character(ath$V5[-1]))

#Turns the dataframe into characters
order.affy<-as.character(aff.test$`as.character(ath$V5[-1])`)

#Orders the experiment ids using levels
order.exp<-levels(exp.test$`as.character(exp$Exp_ID)`)

#Coverts both to lists for the comparison further down
order.affy<-as.list(order.affy)
order.exp<-as.list(order.exp)
order.affy

#Finds all the records that are not in the expids and gives a column of falses
order.exp %in% order.affy
into<-as.data.frame(order.exp %in% order.affy)

#Converts back into data frame
order.exp<-as.data.frame(order.exp)

#Binds the falses, expid number and the actual ids
into2<-cbind(exp.id$DataID,into,order.exp)

#removes all the records taht are not in affy ids
into3<-into2[into2$`order.exp %in% order.affy`!="FALSE",]


#The primary key numbers
test.nums<-c(220381:241915)

#First combination of all the information
exp2affy<-data.frame(test.nums,into3$`exp.id$DataID`,sort.affy2$affy.name_num)

#The relationshiptype number foreign key
exp2affy2<-rep_prime(4,test.nums,exp2affy)

#The final dataframe for the exp2affy values
exp2affy3<-data.frame(exp2affy2$test.nums,exp2affy2$into3..exp.id.DataID.,
                      exp2affy2$sort.affy2.affy.name_num,
                      exp2affy2$`rep(as.numeric(number), length(prime_number))`)
colnames(exp2affy3)<-c("Prime Key", "Exp_Name", "Affy_Id", "Relationship_Type" )

#############################
#### Relationship Table #####
#############################

#Binding together all of the relationship dataframes that
#were made above

names(Transcript2Gene)<-names(go2gene.bind4)
test<-rbind(Transcript2Gene,go2gene.bind4)

names(test)<-names(aff2gene5)
test<-rbind(test,aff2gene5)

names(test)<-names(exp2affy3)

#Final relationship table that contains all the values
Relationshiptable<-rbind(test,exp2affy3)

#####################################
##### Relationship Values Table #####
#####################################

#into2 is from exp2affy

#removes the values that are not also found in the affy ids
control1_bound<-data.frame(into2, as.character(exp$Control1),
                           as.character(exp$Control2),
                           as.character(exp$Control3),
                           as.character(exp$Treatment1),
                           as.character(exp$Treatment2), 
                           as.character(exp$Treatment3))

control1_bound2<-control1_bound[control1_bound$order.exp..in..order.affy!="FALSE",]

#repeates the exp_ids 6 times to account for each experiment
exp.id.rep<-as.data.frame(rep(control1_bound2$exp.id.DataID,6))

#Combines together all the different values from the experiments
exp_together<-c(as.character(control1_bound2$as.character.exp.Control1.),
                as.character(control1_bound2$as.character.exp.Control2.),
                as.character(control1_bound2$as.character.exp.Control3.),
                as.character(control1_bound2$as.character.exp.Treatment1.),
                as.character(control1_bound2$as.character.exp.Treatment2.),
                as.character(control1_bound2$as.character.exp.Treatment3.))
exp_together<-as.data.frame(exp_together)


#Combines together the exp ids and all of the values from the experiments
exp_id_values<-data.frame(exp.id.rep$`rep(control1_bound2$exp.id.DataID, 6)`,
                          exp_together$exp_together)

#turns relationshiptable into a data.table in order to extract the prime key
Relationshiptable_Filled2<-as.data.table(Relationshiptable_Filled)
control1<-Relationshiptable_Filled2[RTypeID_RelationshipType=="4"]
control1<-as.data.frame(control1$RelationshipID)

#Repeats the prime key now as a foreign key 6 times
control2<-as.data.frame(rep(control1$`control1$RelationshipID`,6))

#combines together the foreign key with the values
exp_id_values2<-data.frame(control2,exp_id_values$exp_together.exp_together)

length(exp_id_values2$rep.control1..control1.RelationshipID...6.)
exp.nums<-c(1:129210)

#Final relationship values object
relationship_values<-data.frame(exp.nums,exp_id_values2)