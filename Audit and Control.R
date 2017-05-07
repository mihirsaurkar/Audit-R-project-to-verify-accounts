#--------------------------------DATA SETS---------------------------------------------------------

pro_arCollections = read.csv("C:/AnC_Project/arCollections.csv")
pro_arConfirmations = read.csv("C:/AnC_Project/arConfirmations.csv")
pro_custCredit = read.csv("C:/AnC_Project/custCredit.csv")
pro_empReimbursements = read.csv("C:/AnC_Project/empReimbursements.csv")
pro_sales = read.csv("C:/AnC_Project/sales.csv")
pro_purchases = read.csv("C:/AnC_Project/purchases.csv")
pro_inventoryCounts = read.csv("C:/AnC_Project/inventoryCounts.csv")
pro_inventoryPerpetual = read.csv("C:/AnC_Project/inventoryPerpetual.csv")

#----------------------------------DATA SETS------------------------------------------------------



#-----------------------INSTALL FOLLOWING PACKAGES -----------------------------------------------
install.packages("benford.analysis")
install.packages("dplyr")
install.packages("pps")
install.packages("pwr")

#---------------------------------START - Preparing Data Set---------------------------------------
importAccounts = function() {
  library(readxl, readr)
  path = "/Users/mihirsingh/Documents/IDS 523/Final Project/dataset" ## folder for files downloaded from UIC Blackboard
  files = c("arCollections.csv", "arConfirmations.csv", 
            "custCredit.csv", "empReimbursements.csv", "sales.csv",
            "purchases.csv", "inventoryCounts.csv", "inventoryPerpetual.csv")
  dataFrameList = list()
  for(i in 1:length(files)){
    dataFrameName = strsplit(files[i], ".", fixed = TRUE)[[1]][1]
    fileType = strsplit(files[i], ".", fixed = TRUE)[[1]][2]
    if(fileType == "xlsx") {
      dataFrame = read_excel(paste(path, files[i], sep = "/"))
    } else {
      dataFrame = read.csv(paste(path, files[i], sep = "/"))
    }
    namedFrame = assign(dataFrameName, dataFrame)
    dataFrameList[[dataFrameName]] = namedFrame
  }
  return(dataFrameList)
}

convertAccounts = function(accounts) {
  library(stringr)
  library(lubridate)
  for(i in 1:length(accounts)) {
    for (n in 1:length(accounts[[i]])) {
      dataFrame = accounts[[i]]
      if(str_detect(names(dataFrame[n]), "date") | str_detect(names(dataFrame[n]), "dateColl")){
        if(is.factor(dataFrame[[n]])){
          accounts[[i]][[n]] = ymd(as.character(dataFrame[[n]]))
        }
      }
      else if(str_detect(names(dataFrame[n]), "sku") | str_detect(names(dataFrame[n]), "invoice")
              | str_detect(names(dataFrame[n]), ".no") | str_detect(names(dataFrame[n]), "customer")){
        accounts[[i]][[n]] = as.character(dataFrame[[n]])
      }
      else if (str_detect(names(dataFrame[n]), "cashtrue")) {
        accounts[[i]][[n]] = as.logical(dataFrame[[n]])
      }
      else if(str_detect(names(dataFrame[n]), "Amount"))
      {
        accounts[[i]][[n]] = as.numeric(sub("\\$","", dataFrame[[n]]))
      }
    }
  }
  return(accounts)
}


createDailySales = function(accounts) {
  totalSales = accounts$sales
  totalSales$amt = totalSales$qty * totalSales$unitprice
  dailySales = aggregate(amt~date,totalSales,sum)
  accounts[["dailySales"]] = dailySales
  return(accounts)
}

createCostofGoodsSold = function(accounts){
  costOfGoodsSold = merge(accounts$sales, accounts$inventoryPerpetual, by="sku", all.x=T)
  costOfGoodsSold$COGS = costOfGoodsSold$unitcost * costOfGoodsSold$qty
  accounts[["costOfGoodsSold"]] = costOfGoodsSold
  return(accounts)
}


createDailyPurchases = function(accounts) {
  totalSales = accounts$purchases
  totalSales$amt = totalSales$quantity * totalSales$unitcost
  dailySales = aggregate(amt~date,totalSales,sum)
  accounts[["dailyPurchases"]] = dailySales
  return(accounts)
}
createDailyCollections= function(accounts) {
  totalSales = accounts$arCollections
  dailySales = aggregate(amt.received~dateColl,totalSales,sum)
  accounts[["dailyCollected"]] = dailySales
  return(accounts)
}



createUnpaidAccountsReceivable = function(accounts) {
  splitSalesbyTransaction = split(accounts$sales, accounts$sales$cashtrue)
  credit = splitSalesbyTransaction[["FALSE"]]
  allCreditAccounts = merge(credit, accounts$arCollections, by="invoice", all.x = T)
  allCreditAccounts$notCollected = is.na(allCreditAccounts$amt.received)
  allCreditAccountsbyCollection = split(allCreditAccounts, allCreditAccounts$notCollected)
  unpaidAccountsReceivable = allCreditAccountsbyCollection[["TRUE"]]
  accounts[["unpaidAccountsReceivable"]] = unpaidAccountsReceivable
  return(accounts)
}

createAllowanceForDoubtfulAccounts = function(accounts) {
  x = accounts$unpaidAccountsReceivable
  endDateVector = rep(ymd("2016/12/31"), length(accounts$unpaidAccountsReceivable$invoice))
  x$endDate = endDateVector
  x$daysSincePurchase = x$endDate - x$date
  x$interval = findInterval(x$daysSincePurchase, c(90, 180))
  accounts[["doubtfulAccounts"]] = x
  return(accounts)
}

createOutofStock = function(accounts){
  salesBySKU = aggregate(qty~sku, accounts$sales,sum)
  purchasesBySKU = aggregate(quantity~sku,accounts$purchases,sum)
  purcahsesSalesBySKU = merge(salesBySKU, purchasesBySKU, by="sku")
  purchasesSalesInventoryBySKU = merge(purcahsesSalesBySKU, accounts$inventoryPerpetual, by="sku")
  purchasesSalesInventoryBySKU$turnover = (purchasesSalesInventoryBySKU$qtypurchasesSalesInventoryBySKU$
                                             quantity)/purchasesSalesInventoryBySKU$endstock
  turnover = data.frame(purchasesSalesInventoryBySKU$sku,purchasesSalesInventoryBySKU$turnover)
  colnames(turnover)=c("sku","times")
  accounts[["turnover"]] = turnover
  return(accounts)
}


createAccountsByYear = function(accounts, year) {
  for(i in 1:length(accounts)) {
    for (n in 1:length(accounts[[i]])) {
      dataFrame = accounts[[i]]
      dateColumnExists = FALSE
      if(str_detect(names(dataFrame[n]), "date")){
        dateColumn = n
        dateColumnExists = TRUE
        break()
      }
    }
    if(dateColumnExists == TRUE) {
      accounts[[i]]$year = year(accounts[[i]][[dateColumn]])
      dataFramebyYear = split(accounts[[i]], accounts[[i]][["year"]])
      accounts[[i]] = dataFramebyYear[[year]]
    }
  }
  return(accounts)
}



accounts = importAccounts()
accounts = convertAccounts(accounts)
accounts2016 = createAccountsByYear(accounts, year = "2016")
accounts2016 = createCostofGoodsSold(accounts2016)
accounts2016 = createUnpaidAccountsReceivable(accounts2016)
accounts2016 = createAllowanceForDoubtfulAccounts(accounts2016)


#---------------------------------END - Preparing Data Set---------------------------------------








#---------------------------------START - Test of Internal Controls-------------------------------

#Question 1
findCreditNegatives = function(accounts) {
  library(plyr, dplyr)
  #Prepare Sales table
  sales = split(accounts$sales, accounts$sales$cashtrue)[["FALSE"]]
  sales = subset(sales, select = c(date, cust.no, total))
  names(sales)[names(sales) == "total"] = "trans"
  sales$trans = sales$trans*-1
  #Prepare Collections table
  #collections = merge(accounts$sales, accounts$arCollections, by = "invoice", all.x = T)
  collections = na.omit(accounts$arCollections)
  collections = subset(collections, select = c(dateColl, cust.no, amt.received))
  names(collections)[names(collections) == "dateColl"] = "date"
  names(collections)[names(collections) == "amt.received"] = "trans"
  #TransactionsTable
  transTable = rbind(sales, collections)
  transTable = arrange(transTable, date)
  #Create TransByCustomer
  transByCustomer = split(transTable, transTable$cust.no)
  #Loop through customers
  badCreditAccount = data.frame()
  for(i in 1:length(transByCustomer)) {
    customer = transByCustomer[[i]]
    customerNumber = transByCustomer[[i]][1,]$cust.no
    customer$subTotal = accounts$custCredit[as.numeric(customerNumber),]$limit
    #loop through customer
    for(n in 1:length(customer$subTotal)) {
      if(n != 1) {
        customer[n,]$subTotal = customer[n - 1,]$subTotal + customer[n,]$trans
        if(sign(customer[n,]$subTotal) == -1) {
          badCreditAccount = rbind(badCreditAccount, customer[n,])
          break
        }
      }
    }
  }
  accounts[["overlimitCreditApprovals"]] = badCreditAccount
  return(accounts)
}
accounts2016 = findCreditNegatives(accounts2016)
View(accounts2016$overlimitCreditApprovals)



#Question 2
#Question - 2a
findDuplicates = function(dataframe, column) {
  dataframe$test = as.numeric(dataframe[[column]])
  dataframe$dup = duplicated(dataframe$test)
  x = split(dataframe, dataframe$dup)
  y = x[["TRUE"]]
  print(y)
  print ("Duplicates (head)")
  head(y)
}

findDuplicates(dataframe = accounts2016$sales, column = "invoice")



#Question - 2b
findMissingEntries =function(max,set) {
  good = 1:max
  test = as.numeric(set)
  missing = setdiff(good, set)
  print(missing)
  print ("Missing (head)")
  head(missing)
  print("Number of missing entries")
  print(length(missing))
}

findMissingEntries(max = length(accounts2016$sales$invoice), set = accounts2016$sales$invoice)


#Question - 2c
findSalesNotIn2016 = function(accounts) {
  x = accounts$sales
  x$year = year(accounts$sales$date)
  y = split(x, x$year)
  View(y)
  z = rbind(y[["2015"]], y[["2017"]])
  print("Sales not in 2016")
  print(z)
  print ("Sales not in 2016 (head)")
  head(z)
  print("The sum is")
  print(sum(z$total)) #total value of sale other than 2016
}

findSalesNotIn2016(accounts)


#---------------------------------END - Test of Internal Controls-------------------------------











#---------------------------------START - Re-Compute the trial balance--------------------------
#Question 1
#Question 1a

accountTotals = function(accounts) {
  #Sales
  print("Sales")
  totalSales = sum(accounts$sales$total)
  print(totalSales)
  
  #Sales Returns
  print("Sales Returns")
  x = aggregate((returns)*unitprice ~ sku, accounts2016$inventoryPerpetual, sum)
  print(sum(x$`(returns) * unitprice`))
  
  #Cost of Goods Sold
  print("COGS")
  totalCOGS = sum(accounts$costOfGoodsSold$COGS)
  print(totalCOGS)
  
  #Accounts Receivable
  print("Accounts Receivable")
  totalAR = sum(accounts2016$unpaidAccountsReceivable$total)
  print(sum(totalAR))
  
  #Collections
  print("Collections")
  totalCollections = sum(accounts$arCollections$amt.received)
  print(sum(totalCollections))
  
  
  #Perpetual Inventory system balances for Inventory at 1/1/2016 and 12/31/2016
  #Inventory on Hand
  print("Inventory Qty")
  print(sum(accounts$inventoryPerpetual$endstock))
  print("Inventory @ Cost")
  endInventoryValue = sum(accounts$inventoryPerpetual$unitcost*accounts$inventoryPerpetual$endstock)
  print(endInventoryValue)
  
  
  #Purchases
  print("Total Purchases")
  purchasesTotal = sum(accounts$purchases$unitcost * accounts$purchases$quantity)
  print(purchasesTotal)
  
  
  #Employee Reimbursements
  print("Employee Reimbursements")
  empReimbursementsTotal = sum(accounts$empReimbursements$Amount)
  print(empReimbursementsTotal)
}

accountTotals(accounts2016)

#Question 1b
summarizeAccount = function(accounts) {
  for(i in 1:length(accounts)){
    print(names(accounts[i]))
    print(summary(accounts[[i]]))
  }
}
summarizeAccount(accounts2016)


#Question 1c
#Incomplete -----SUMMARIZE ACCOUNTS




#Question2
#Question 2a and b
accounts2016 = createDailySales(accounts2016)
summary(accounts2016$dailySales)
accounts2016 = createDailyPurchases(accounts2016)
summary(accounts2016$dailyPurchases)
accounts2016 = createDailyCollections(accounts2016)
summary(accounts2016$dailyCollected)


#Question2c

#Answer
#Yes, because we have created the accounts2016 and filtered out the dates that lie in the fiscal year 2016.

#Question2d

#Answer
#Then we would have to filter out the 2016 transactions first and then recompute the balances.


#Question2e

#Answer
#No, because we have filtered out any transactions from years other than the audit year 2016



#---------------------------------END - Re-Compute the trial balance--------------------------










#---------------------------------START - Employee Expenditures Audit--------------------------

#Question 1
empPurchasesExceedingLimit = function(accounts) {
  library(BenfordTests)
  #Employee Reimbursements
  print("Employee Totals Exceeding 50000")
  x = aggregate(Amount ~ Employee.No, accounts$empReimbursements, sum)
  View(x)
  print(x)
}



#Question 2     #Question 3      #Question 4  

benfordAnalysis = function(accounts)
{
  library(benford.analysis)
  library(dplyr)
  AmountAnalysis = benford(accounts$empReimbursements$Amount,number.of.digits = 1,sign = "both")
  AmountSuspects = getSuspects(AmountAnalysis, accounts$empReimbursements, how.many=2)
  print("Amount Suspects")
  print(AmountSuspects)
  print("Count - Amount Suspects")
  print(nrow(AmountSuspects))
  print("ChiSquare Analysis - Amount")
  print(chisq(AmountAnalysis))
  plot(AmountAnalysis, main = "Benford Analysis on Amount")
  
  ReceiptAnalysis <- benford(accounts$empReimbursements$Receipt.No,number.of.digits = 1)
  ReceiptSuspects = getSuspects(ReceiptAnalysis, accounts$empReimbursements, how.many=2)
  print("Amount Suspects")
  print(ReceiptSuspects)
  print("Count - Amount Suspects")
  print(nrow(ReceiptSuspects))
  print("ChiSquare Analysis - Amount")
  print(chisq(ReceiptAnalysis))
  plot(ReceiptAnalysis)
  
  EmployeeAnalysis <- benford(accounts$empReimbursements$Employee.No,number.of.digits = 1,sign = "both")
  EmployeeSuspects = getSuspects(EmployeeAnalysis, accounts$empReimbursements, how.many=2)
  print("Amount Suspects")
  print(EmployeeSuspects)
  print("Count - Amount Suspects")
  print(nrow(EmployeeSuspects))
  print("ChiSquare Analysis - Amount")
  print(chisq(EmployeeAnalysis))
  plot(EmployeeAnalysis)
  
  
  AmountGreater500 = accounts$empReimbursements%>%filter(Amount>500)
  AmountGreater500Analysis = benford(AmountGreater500$Amount,number.of.digits = 1, sign = "both")
  AmountGreater500Suspects = getSuspects(AmountGreater500Analysis, accounts$empReimbursements, how.many=2)
  print("Amount Suspects")
  print(AmountGreater500Suspects)
  print("Count - Amount Suspects")
  print(nrow(AmountGreater500Suspects))
  print("ChiSquare Analysis - Amount")
  print(chisq(AmountGreater500Analysis))
  plot(AmountGreater500Analysis, main="Benford Analysis on Amount Greater than 500")
  
  AmountLess500 = accounts$empReimbursements%>%filter(Amount<=500)
  AmountLess500Analysis = benford(AmountLess500$Amount,number.of.digits = 1, sign = "both")
  AmountLess500Suspects = getSuspects(AmountLess500Analysis, accounts$empReimbursements, how.many=2)
  print("Amount Suspects")
  print(AmountLess500Suspects)
  print("Count - Amount Suspects")
  print(nrow(AmountLess500Suspects))
  print("ChiSquare Analysis - Amount")
  print(chisq(AmountLess500Analysis))
  plot(AmountLess500Analysis, main="Benford Analysis on Amount Greater than 500")
}

benfordAnalysis(accounts)


#---------------------------------END - Employee Expenditures Audit--------------------------







#---------------------------------START - Accounts Receivable Audit--------------------------

#Question - 1
#Unpaid Accounts Receivable
print("Accounts Receivable")
print(sum(accounts2016$unpaidAccountsReceivable$total))

#Question2
#Allowance for Doubtful Accounts
print("Allowance for Doubtful Accounts")
accounts2016 = createAllowanceForDoubtfulAccounts(accounts2016)
doubtfulTotals = aggregate(total~interval, accounts2016$doubtfulAccounts, sum)
print(0.3*doubtfulTotals$total[2] + 0.5*doubtfulTotals$total[3])


#Question3

accounts2016 = findCreditNegatives(accounts2016)
View(accounts2016$overlimitCreditApprovals)




#Question4
#SAme to Transaction Cut-off test
findSalesNotIn2016(accounts)


#Question 5
accounts2016 = findCreditNegatives(accounts2016)
View(accounts2016$overlimitCreditApprovals)


#Question 6

AuditSampler = function(accounts, samplesize)
{
  library("pps")
  ARjoin = merge(accounts$arConfirmations,accounts$arCollections, by="invoice")
  ARjoin$proportion <- ARjoin$amt.received.y/sum(ARjoin$amt.received.y)
  sample = ARjoin[ppss(ARjoin$proportion,samplesize),]
  
  return(sample)
}


verifyAR = function(accounts, sampleSize){
  verifyCollections = AuditSampler(accounts, sampleSize)
  
  verifyCollections$diffAmts = verifyCollections$amt.received.x - verifyCollections$amt.received.y
  
  accounts[["verifyCollections"]] = verifyCollections
  verifyCollections[verifyCollections$diffAmts != 0 , ]
  print("Tolerable limit")
  print(0.1)
  print("The total error value($ amt) is")
  print(sum(abs(verifyCollections$diffAmts))/sum(verifyCollections$amt.received.x))
  return(accounts)
  
}

accounts2016 = verifyAR(accounts2016, 620) # Sample size calculated by power analysis with d-10%
nrow(accounts2016$verifyCollections[accounts2016$verifyCollections$diffAmts != 0 , ])
View(accounts2016$verifyCollections)

#Question 7
#It can clearly seen above that with different sample tests, the error value is always within tolerable limit
#Thus it can be said that the value is fairly stated.


#---------------------------------END - Accounts Receivable Audit--------------------------






#---------------------------------START - Inventory Audit----------------------------------

#Question 1
#Cost of Goods Sold
print("COGS")
totalCOGS = sum(accounts2016$costOfGoodsSold$COGS)
print(totalCOGS)


#Question 2
summary(1 - (accounts2016$costOfGoodsSold$COGS/(accounts2016$costOfGoodsSold$qty *
accounts2016$costOfGoodsSold$unitprice.x)))


#Question 3
findOutOfStockDemand = function(accounts) {
  library(plyr)
  #prepare tables
  sales = subset(accounts$sales, select = c(sku, date, qty))
  sales$qty = sales$qty*-1
  purchases = accounts$purchases
  purchases$qty = purchases$quantity
  purchases = subset(purchases, select = c(sku, date, qty))
  inventoryTrans = rbind(sales, purchases)
  inventoryTrans = arrange(inventoryTrans, date)
  #Create dataframe by sku
  inventoryTransBySku = split(inventoryTrans, inventoryTrans$sku)
  stockOutSkus = list()
  for(i in 1:length(inventoryTransBySku)) {
    sku = inventoryTransBySku[[i]]
    skuNumber = as.numeric(sku[1,]$sku)
    sku$onHand = accounts$inventoryPerpetual[skuNumber,]$beginstock
    for(n in 1:length(sku$qty)) {
      if(n == 1) {
        sku[n,]$onHand = sku[n,]$onHand + sku[n,]$qty
      }
      else {
        sku[n,]$onHand = sku[n-1,]$onHand + sku[n,]$qty
      }
    }
    if(sum(sku$onHand < 0) > 0) {
      stockOutSkus[[length(stockOutSkus) + 1]] = skuNumber
    }
    inventoryTransBySku[[i]] = sku
  }
  stockOutTrans = data.frame()
  for(i in 1:length(stockOutSkus)){
    skuNumber = stockOutSkus[[i]]
    sku = inventoryTransBySku[[as.character(skuNumber)]]
    times = which(diff(sign(sku$onHand)) > 0)
    for(n in 1:length(times)) {
      stockOutTrans = rbind(stockOutTrans, sku[times[n],])
    }
  }
  accounts[["stockOutTrans"]] = stockOutTrans
  return(accounts)
}
accounts2016 = findOutOfStockDemand(accounts2016)
cat(length(na.omit(accounts2016$stockOutTrans$sku)),"stockout events in 2016\n")
View(na.omit(accounts2016$stockOutTrans))
nrow(accounts2016$stockOutTrans)
which(duplicated(accounts2016$stockOutTrans))



#Question 4
AuditSamplerInventory = function(accounts, samplesize)
{
  #library(pps)
  #library(pwr)
  inventorysample = accounts$inventoryCounts[sample(accounts$inventoryCounts$sku,samplesize),]
  #mus = mean(accounts$inventoryCounts$amt.received)
  #print(t.test(sample$amt.received,alternative = "greater", mu = mus , conf.level=.95))
  return(inventorysample)
}

verifyInventory = function(accounts, sampleSize){
  collectionsSample = AuditSamplerInventory(accounts, sampleSize)
  verifyCollections = merge(collectionsSample,accounts$inventoryPerpetual, by="sku")
  verifyCollections$diffAmts = verifyCollections$endstock.x - verifyCollections$endstock.y
  verifyCollections$totalError = verifyCollections$diffAmts * verifyCollections$unitcost
  #View(verifyCollections)
  inventoryError = verifyCollections[verifyCollections$diffAmts != 0 , ]
  accounts[["verifyCollectionsInventory"]] = inventoryError
  print("The value of physical inventory")
  print(sum(verifyCollections$totalError))
  return(accounts)
}

accounts = verifyInventory(accounts, 250)
length(accounts$verifyCollectionsInventory[accounts$verifyCollectionsInventory$diffAmts != 0 , ])
#It can be concluded that the error is not material and thus it can be said that the values have been fairly stated

#Question 5
correctedInventory = function(accounts)
{
  mergedInventory = merge(accounts$inventoryCounts, accounts$inventoryPerpetual, by="sku")
  inventoryPerpetualCorrection = accounts$inventoryPerpetual
  inventoryPerpetualCorrection$endstock = accounts$inventoryCounts$endstock
  accounts[["correctedInventory"]] = inventoryPerpetualCorrection
  return(accounts)
}

accounts = correctedInventory(accounts)
sum(accounts$correctedInventory$unitcost*accounts$correctedInventory$endstock)



#Question 6

AgeInventory = function(accounts)
{
  library(dplyr)
  soldstock = accounts$sales%>%group_by(sku)%>%mutate(qtysold=cumsum(qty))%>%filter(row_number()==n())%>%arrange(sku)
  totalpurchasebysku = accounts$purchase%>%group_by(sku)%>%mutate(qtypurchased=cumsum(quantity))%>%filter(row_number()==n())%>%arrange(sku)
  merged = merge(soldstock,totalpurchasebysku,by="sku",all.x = T)
  merged = merge(merged,accounts$inventoryPerpetual,by="sku")
  calculation = merged%>%mutate(qtyleft=endstock)%>%mutate(qtyoldduration=qtyleft/quantity)%>%arrange(sku)
  
  calculation$qtyleft = ifelse(calculation$qtyleft < 0,0,calculation$qtyleft)
  
  calculation$lessThan60 = ifelse(calculation$qtyoldduration<=2,calculation$qtyleft,2*calculation$quantity)
  calculation$lessThan180 = ifelse(calculation$qtyleft-calculation$lessThan60<=0,0,ifelse(calculation$qtyoldduration<=6,calculation$qtyleft-calculation$lessThan60,4*calculation$quantity))
  calculation$lessThan365 = ifelse(calculation$qtyleft-calculation$lessThan180-calculation$lessThan60<=0,0,ifelse(calculation$qtyoldduration<=12,calculation$qtyleft-calculation$lessThan180-calculation$lessThan60,6*calculation$quantity))
  calculation$greaterThan365 = ifelse(calculation$qtyleft-calculation$lessThan365-calculation$lessThan180-calculation$lessThan60<=0,0,calculation$qtyleft-calculation$lessThan365-calculation$lessThan180-calculation$lessThan60)
  calculation$lessThan60value = calculation$lessThan60*calculation$unitcost.x
  calculation$lessThan180value = calculation$lessThan180*calculation$unitcost.x*0.5
  calculation$lessThan365value = calculation$lessThan365*calculation$unitcost.x*0
  calculation$greaterThan365value = calculation$greaterThan365*calculation$unitcost.x*0
  
  sumLessThan60 = sum(calculation$lessThan60value)
  sumLessThan180 = sum(calculation$lessThan180value)
  sumLessThan365 = sum(calculation$lessThan365value)
  sumGreaterThan365 = sum(calculation$greaterThan365value)
  
  totalValue = sumGreaterThan365+sumLessThan365+sumLessThan180+sumLessThan60
  totalQuantity = sum(calculation$lessThan60)+ sum(calculation$lessThan180)+ sum(calculation$lessThan365) +sum(calculation$greaterThan365)
  print("Quantity proportion of less than 60")
  print(sum(calculation$lessThan60)/totalQuantity)
  
  print("Quantity proportion of less than 180")
  print(sum(calculation$lessThan180)/totalQuantity)
  
  print("Quantity proportion of less than 365")
  print(sum(calculation$lessThan365)/totalQuantity)
  
  print("Quantity proportion of greater than 365")
  print(sum(calculation$greaterThan365)/totalQuantity)
  
  print("Total and Percent for Less than 60 days")
  print(sumLessThan60)
  print((sumLessThan60/totalValue)*100)
  
  print("Total and Percent for Less than 180 days")
  print(sumLessThan180)
  print((sumLessThan180/totalValue)*100)
  
  print("Total and Percent for Less than 365 days")
  print(sumLessThan365)
  print((sumLessThan365/totalValue)*100)
  
  print("Total and Percent for Greater than 365 days")
  print(sumGreaterThan365)
  print(sumGreaterThan365/totalValue)
  
  print("total write down value")
  print(sum(calculation$greaterThan365*calculation$unitcost.x))
  
  print("Number of parts written down")
  print(nrow(calculation[calculation$greaterThan365 != 0,]))
  
  accounts[["sales_Purchase"]] = merged
  accounts[["agedInventory"]] = calculation
  return(accounts)
}

accounts2016 = AgeInventory(accounts2016)
View(accounts2016$agedInventory)


#Question 7
calculateLowTurnOver = function(accounts)
{
soldstock = accounts$sales%>%group_by(sku)%>%mutate(qtysold=cumsum(qty))%>%filter(row_number()==n())%>%arrange(sku)  
q7 = merge(soldstock,accounts$inventoryCounts,by="sku")
turnOver = q7%>%mutate(invturn=(qtysold/endstock))%>%arrange(invturn)
print("Turn Over less than 10 times")
print(nrow(turnOver[turnOver$invturn < 10,]))
}

calculateLowTurnOver(accounts)

#Question 8

performMarketTest = function(accounts)
{
  newInventory = accounts$inventoryPerpetual[,c(2,3,4)]   #sku,unitprice and unitcost
  newInventory = merge(accounts$inventoryCounts,newInventory,by="sku")
  newInventory = newInventory%>%mutate(differ=endstock*(unitprice-unitcost))
  print("Lower of Cost / Market Test")
  print(nrow(newInventory[newInventory$differ < 0,]))
}

performMarketTest(accounts)


#Question 9     #Question 10

realizableValues = function(accounts) {
Realizable_Sales <- accounts$sales%>%group_by(sku)%>%mutate(qtysold=cumsum(qty))%>%filter(row_number()==n())%>%arrange(as.numeric(sku))

Realizable_purchases <- accounts$purchases%>%group_by(sku)%>%mutate(qtypurchased=cumsum(quantity))%>%filter(row_number()==n())%>%arrange(as.numeric(sku))
Realizable_cost <- merge(Realizable_purchases,Realizable_Sales, by = "sku")

Realizable_cost$relizable_value <- (Realizable_cost$unitprice -Realizable_cost$unitcost)* Realizable_cost$qtysold
Realizable_cost$Commision_realizable_value <- ((Realizable_cost$unitprice -Realizable_cost$unitcost*1.1)* Realizable_cost$qtysold)
Realizable_cost$relizable_value_lt_cost <- (Realizable_cost$unitcost*Realizable_cost$qtysold) -  Realizable_cost$relizable_value
Realizable_cost$Realizable_value_lt <- (Realizable_cost$unitcost*1.1* Realizable_cost$qtysold) - Realizable_cost$Commision_realizable_value 


print("Count of SKUs having net realizable sales value less than cost")
print(nrow(Realizable_cost[Realizable_cost$relizable_value_lt_cost>0,]))

print("Count of SKUs having net realizable sales value less 110% of cost")
print(nrow(Realizable_cost[Realizable_cost$Realizable_value_lt>0,]))
accounts[["RealizableSet"]] = Realizable_cost
return(accounts)
}

accounts2016 = realizableValues(accounts2016)


#Question 11

calculateDefectiveRate = function(accounts, percent)
{
  totalpurchasebysku = accounts$purchase%>%group_by(sku)%>%mutate(qtypurchased=cumsum(quantity))%>%filter(row_number()==n())%>%arrange(sku)
  defectiveRate = merge(totalpurchasebysku,accounts$inventoryCounts,by="sku")
  defectiveRate=defectiveRate%>%mutate(calc=(defective/qtypurchased)*100)%>%arrange(sku)
  print(paste0("Defective Rate Greater than percent ",percent))
  print(length(defectiveRate$calc[defectiveRate$calc>percent]))
}

calculateDefectiveRate(accounts2016, 20)

#Question 12

calculateSalesReturnRate = function(accounts, percent)
{
  soldstock = accounts$sales%>%group_by(sku)%>%mutate(qtysold=cumsum(qty))%>%filter(row_number()==n())%>%arrange(sku)
  salesReturnRate = merge(soldstock,accounts$inventoryCounts,by="sku")
  salesReturnRate <- salesReturnRate%>%mutate(calculatedReturnRate=(returns/qtysold)*100)%>%arrange(sku)
  print(paste0("Sales Return Rate Greater than percent ",percent))
  print(length(salesReturnRate$calculatedReturnRate[salesReturnRate$calculatedReturnRate>percent]))
}

calculateSalesReturnRate(accounts2016, 2)


#---------------------------------END - Inventory Audit----------------------------------




