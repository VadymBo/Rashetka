fsalary<-function(Product=0, Qualification=0)
{
timeAll=22*12
  #DONT FORGET TO CHECK WD
  setwd("C:\\Users\\Vadim\\Documents\\rashetka")
  products<-read.csv("Products.csv")
  qualif<-read.csv("Qualifications.csv")
  profes<-read.csv("Professions.csv")
  prodQualif<-read.csv("ProdQualif.csv")
  
  
  ## time for making all amount of each product
  eachProdTime=data.frame("id"=products$id,"allTime"=products$amount*products$time*timeAll)
  
  # data.frame to show about ALL products info
  productInfo<-data.frame(products)
  productInfo$amount<-productInfo$amount*timeAll
  productInfo$progTime<-eachProdTime$allTime
  
  
  ## salary for making each product
  salaryProd<-c()
  for (i in 1:nrow(prodQualif))
  {
    salaryProd[i]<-eachProdTime[prodQualif$product[i],"allTime"]*qualif[prodQualif$qualif.id[i],"tariff"]  
  }
  names(salaryProd)=prodQualif$product
  
  ## salary per qualifications
  salary<-c()
  # time for each qualification ID
  qualifTime<-c()
  for (i in 1:nrow(qualif))
  {
    salary[i]=sum(salaryProd[which(qualif$id[i]==prodQualif$qualif.id)])
    qualifTime[i] = sum(eachProdTime$allTime[which(qualif$id[i]==prodQualif$qualif.id)])
  }
  
  names(qualifTime)=qualif$id
  names(salary)=qualif$id
  
  
  # making data.frame with information describing ALL about prfessions and qualifications
  
  salaryInfo<-as.data.frame(qualif$id)
  names(salaryInfo)<-"qualif.id"
  salaryInfo$qualif<-qualif$qualification
    
  for ( i in 1:nrow(qualif))
  {
    salaryInfo$profes[i]<- as.character(profes$description[qualif$profes[i]])
    salaryInfo$tariff[i]<-as.character(qualif$tariff[qualif$qualification[i]])  
  }
  salaryInfo$salary<-salary
qualif
  # number of employees for each qualification ID
  employeeNum<-qualifTime/timeAll
  
  

# Show about products by its' ID
if (Product>0 )
{
  print("Product is: ")
  names(productInfo)<-c("id",  "amount","time per each", "time for program")
  print(productInfo[which(productInfo$id==Product),])
  
  x<-salaryInfo[which(salaryInfo$qualif.id==prodQualif$qualif.id[which(productInfo$id==Product)]),]
  # Changing value of salary per profession to value of salary PER PRODUCT
  x$salary<- salaryProd[Product]
  names(x)<- c("qualif.id", "qualification", "profession", "tariff", "salary for product")
  print(" Profession for this product :")
  print(x)
  
}

# SHow about profession by its' ID
if( Qualification>0)
{
  print("Qualifications is :")
print(salaryInfo[which(salaryInfo$qualif.id==Qualification),])
  print("Products, which are produced by this profession")
  
  names(productInfo)<-c("id",  "amount","time per each", "time for program")
print(productInfo[which(prodQualif$qualif.id==Qualification),])

}





if (Product==0 & Qualification==0 )
{
  ## MAKING INFO TO SHOW ON SCREEN
  cat ("salary for ", timeAll, " working days \n")
  
  ## Info about production to show with correct names
  namedProductInfo<-productInfo
  names(namedProductInfo)<-c("code",  "amount","time per each", "time for program")
  
  print("Information about production")
  print(namedProductInfo)
  
  ## Info about salary to show, sorted by profession
  sortedSalaryInfo<-salaryInfo[order(salaryInfo$profes),]
  print("Information about salary for qualifications")
  print(sortedSalaryInfo[c("profes", "qualif","tariff", "salary")])
  
}
}

fsalary(Product=2)