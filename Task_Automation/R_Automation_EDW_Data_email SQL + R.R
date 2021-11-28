library(dplyr)
library(tidyr)
library(janitor)
library(openxlsx)
library(RODBC)
############################### PICK A DATE  ####################################
refDate <- as.Date("2021-11-22") ### Valid business date##




###############SET UP#########################################################
connString <- paste0("driver={SQL Server};", 
                     "server=DESKTOP-DMKI1PA\SQLEXPRESS;",
                     "database=AdventureWorksDW2019;",
                     "trusted_connection=yes;")


userName <- Sys.getenv("USERNAME")

MyFolder <- paste0("C:/Users/", userName,
                    "/Documents/Adam's File/",
                    format(refDate, "%Y/"), format(refDate, "%B/"))


emailsTo <- "adam26199@gmail.com"
emailsCc <- "Samxxx@gmail.com"

#################### GET THE DATA #######################################

SalesQuery <- paste0("SELECT *
  FROM [AdventureWorksDW2019].[dbo].[FactInternetSales] ",
                   "WHERE Order_Date = '", refDate, "'")


ProductQuery <- "SELECT * FROM [AdventureWorksDW2019].[dbo].[DimProduct]"

productcategoryquery <- sqlQuery(con, "SELECT ProductSubCategoryKey , [EnglishProductSubcategoryName]
  from [AdventureWorksDW2019].dbo.DimProductSubCategory",  stringsAsFactors = F)


con <- odbcDriverConnect(connection = connString)
SalesDATA <- sqlQuery(con, SalesQuery,
                   stringsAsFactors = F) %>%
  select(ProductKey
         ,OrderDateKey
         ,DueDateKey
         ,ShipDateKey
         ,CustomerKey) %>%
  filter(CustomerKey %in% c("0104B",
                     "0104C")) ##Specific CUstomers needed ##


Products <- sqlQuery(con, ProductQuery,
                    stringsAsFactors = F)
ProductsCategory <- sqlQuery(con, productcategoryquery,
                     stringsAsFactors = F)




newproducts <- products %>% left_join(ProductsCategory, by = "ProductSubCategoryKey") %>% 
  select(product_key, englishproductname, EnglishProductSubcategoryName, Color, size, productline, englishdescription )


SalesDATA <- SalesDATA %>% left_join(newproducts, by = "[ProductKey]") %>% 
  select(product_key, englishproductname, EnglishProductSubcategoryName, Color, size, productline, englishdescription 
         ,OrderDateKey
         ,DueDateKey
         ,ShipDateKey
         ,CustomerKey,
         Salesamount) %>% rename(sales_quantity = Salesamount)


 wb <- createWorkbook()
 addWorksheet(wb, "emaildataset")
 writeData(wb, "emaildataset", SalesDATA)
 saveWorkbook(wb, paste0(ProductFolder, "dataset - ", format(refDate, "%b"), " " ,format(refDate, "%Y.xlsx")), overwrite = T)

################# SEND EMAIL #########################
 if(readline(paste0("Hello there, Would you like to send the email with the file to ", emailsTo, " (Yes/No)? If Yes, (Type Y)")) == "Y") {
   source("sendRMail.R")
   
   TestLink <- paste0(MyFolder, "dataset - ", format(refDate, "%b"), " " ,format(refDate, "%Y.xlsx"))
   
   mailHTMLbody = paste0("<br>To whom it concerns, 

                         <br><br>Please see attached the dataset from the data warehouse for the month.
                         </b> 
<br><br>if you have any questions, please contact adam@g...
 
                      <br><br>Regards.",
                         "<br>",
                         "<b>",
                         "Project - Sales Team")
   
   RPrjFolder <- paste0(getwd(), "/")
   file <-  paste0(ProductFolder, "dataset - ", format(refDate, "%b"), " " ,format(refDate, "%Y.xlsx"))
   mailSent <- sendRMail(rmTo = emailsTo,
                       rmCc = emailsCc,
                         rmSubject = paste("EDW Data for -",
                                           refDate),
                       rmAttachmentPath = Link,
                                        
                         rmBody = mailHTMLbody,
                         rmDisc = F)
   
   print(mailSent)
   
  
 }
 

 
