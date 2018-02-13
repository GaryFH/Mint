---
title: "Mint Data Processing"
author: "GaryFH"
date: "February 8, 2018"
output: html_document
editor_options: 
  chunk_output_type: console
---




###Setup environment


```r
suppressWarnings(library(dplyr))
```


###Look at income and try to reconcile - note that $5,000 of interest income was cashed and not part of Mint income.


```r
d1<-tbl_df(read.csv("transactions02082018.csv"))
d1$Date<- as.Date(d1$Date,format="%m/%d/%Y")
d1<-filter(d1,Date>=as.Date("2017-01-01")&Date<=as.Date("2017-12-31"))

d2<-filter(d1,Transaction.Type=="credit")

d222<-filter(d2,Category=="appraisal income")
print(paste("Total Interest Income = ","$",sum(d222$Amount),sep=""))
```

```
## [1] "Total Interest Income = $95787"
```

```r
d223<-filter(d2,Category=="Interest Income")
print(paste("Total Interest Income = ","$",(sum(d223$Amount)+5000),sep=""))
```

```
## [1] "Total Interest Income = $13202.77"
```

```r
d2b<-group_by(d2,Category)
print(summarise(d2b,sum(Amount)),n=nrow(d2b))
```

```
## # A tibble: 13 x 2
##    Category            `sum(Amount)`
##    <fctr>                      <dbl>
##  1 appraisal income          95787  
##  2 Auto Payment                 14.5
##  3 Clothing                    199  
##  4 Credit Card Payment       83924  
##  5 Groceries                    51.5
##  6 Home Improvement             19.3
##  7 Income                      112  
##  8 Interest Income            8203  
##  9 Office Supplies             145  
## 10 Reimbursement             10000  
## 11 Service Fee                   0  
## 12 Shopping                    984  
## 13 Transfer-hm equity        80000
```

```r
print(summarise(group_by(d2,Description),sum(Amount)),n=nrow(d2))
```

```
## # A tibble: 29 x 2
##    Description                            `sum(Amount)`
##    <fctr>                                         <dbl>
##  1 Amazon                                        177   
##  2 Annual Membership Fee                           0   
##  3 Bkofamerica ATM Deposit                     15450   
##  4 Churchill Hardw Camerson                       12.9 
##  5 Corelogic Des Lc                             2215   
##  6 Costco                                         51.5 
##  7 Costco Whse Folsom                            112   
##  8 Counter                                     22104   
##  9 Douglas Feed Ranch                             31.1 
## 10 Dvs Proc Des                                 3305   
## 11 Eddie Bauer                                    43.4 
## 12 Hobby Lobby                                    13.9 
## 13 Home Depot                                      6.48
## 14 Home Goods                                    102   
## 15 Interest Earned                                 2.77
## 16 Land's End                                    543   
## 17 Mercury                                        14.5 
## 18 Michaels                                       61.8 
## 19 Murcor Des Trade                            33483   
## 20 Nordstrom                                     199   
## 21 Nvs Pmd                                      1650   
## 22 Nvs Pmd Des                                 35780   
## 23 Online Banking Advance                      80000   
## 24 Online Payment                              55390   
## 25 Payment                                        18.0 
## 26 Sam's Club                                    145   
## 27 Thank You                                   19211   
## 28 Transfer from Adv Tiered Interest Chkg       9273   
## 29 Wal-Mart                                       42.8
```

```r
#View(filter(d2b,Category=="Interest Income"))
#View(filter(d2b,Category=="appraisal income"))
#View(filter(d2b,Category=="Reimbursement"))
#View(filter(d2b,Description=="Thank You"))
```


###Overview of all Categories


```r
d11<-group_by(d1,Category)
print(summarise(d11,sum(Amount)),n=nrow(d11))
```

```
## # A tibble: 59 x 2
##    Category               `sum(Amount)`
##    <fctr>                         <dbl>
##  1 Advertising                    59.0 
##  2 Alcohol & Bars                  9.20
##  3 appraisal income            95787   
##  4 Auto Insurance               4391   
##  5 Auto Payment                   58.0 
##  6 bus phone/internet           3242   
##  7 Business Meals               3424   
##  8 Business Services             635   
##  9 Business Travel              2082   
## 10 Charity                     13000   
## 11 Clothing                      746   
## 12 Credit Card Payment         86777   
## 13 Credit Card Payments        46774   
## 14 Dentist                      1218   
## 15 Doctor                       1465   
## 16 Driver cost                  9158   
## 17 Electric Bill                2272   
## 18 Electronics & Software         99.0 
## 19 Fast Food                      34.2 
## 20 Federal Tax                 11378   
## 21 Furnishings                   250   
## 22 Gas & Fuel                   2839   
## 23 Gift                          149   
## 24 Gmail Bill                     15.3 
## 25 Groceries                    7085   
## 26 Gym                          1275   
## 27 Health Insurance            10064   
## 28 HealthSavingsAccount         7750   
## 29 HOA Dues                      500   
## 30 Hobbies                       105   
## 31 Home Improvement            17679   
## 32 Hotel                         102   
## 33 Income                        112   
## 34 Interest Income              8203   
## 35 IRA                         13000   
## 36 Legal                         412   
## 37 Life Insurance               2652   
## 38 Mortgage & Rent             12719   
## 39 Movies & DVDs                 115   
## 40 Music                         157   
## 41 Office Supplies              6810   
## 42 Parking                         5.00
## 43 Pet Food & Supplies            79.8 
## 44 Printing                      115   
## 45 Propane Gas                  1349   
## 46 Property Tax                 5740   
## 47 Reimbursement               10000   
## 48 Restaurants                   100   
## 49 Service & Parts               964   
## 50 Service Fee                     0   
## 51 Shipping                       27.4 
## 52 Shopping                     8848   
## 53 State Tax                    2011   
## 54 Taxes                        2754   
## 55 Temp business loan          60000   
## 56 Transfer-hm equity          80000   
## 57 Trash Service                 357   
## 58 Tuition                        59.0 
## 59 Utilities                     150
```


###Total Health Insurance


```r
d4<-filter(d1,Category=="Health Insurance")
d4<-select(d4,Date,Description,Amount)
d44<-d4
d45<-mutate(d44,z="`")
d45<-select(d45,Date,Description,z,Amount)
d45$Amount<- sprintf("$ %7.2f", d45$Amount)
print(d45,n=nrow(d45))
```

```
## # A tibble: 41 x 4
##    Date       Description       z     Amount   
##    <date>     <fctr>            <chr> <chr>    
##  1 2017-12-21 Kaiser Permanente `     $  540.86
##  2 2017-12-07 Kaiser Permanente `     $   13.40
##  3 2017-12-07 Kaiser Permanente `     $    7.85
##  4 2017-12-06 Kaiser Permanente `     $   15.00
##  5 2017-12-04 Check 1580        `     $  281.88
##  6 2017-12-04 Kaiser Permanente `     $   35.00
##  7 2017-12-02 Kaiser Permanente `     $   71.67
##  8 2017-11-08 Kaiser Permanente `     $  136.46
##  9 2017-11-06 Check 1577        `     $  281.88
## 10 2017-10-12 Kaiser Permanente `     $    9.10
## 11 2017-10-10 Check 1575        `     $ 1127.52
## 12 2017-10-01 Kaiser Permanente `     $   13.40
## 13 2017-08-30 Kaiser Permanente `     $   79.52
## 14 2017-08-30 Kaiser Permanente `     $   13.40
## 15 2017-08-15 Rli Uw Serv       `     $  950.00
## 16 2017-07-31 Kaiser Permanente `     $   13.40
## 17 2017-07-26 Kaiser Permanente `     $  180.00
## 18 2017-07-26 Kaiser Permanente `     $  300.00
## 19 2017-07-17 Kaiser Permanente `     $  563.76
## 20 2017-07-07 Kaiser Permanente `     $   13.40
## 21 2017-06-27 Kaiser Permanente `     $  563.76
## 22 2017-06-06 Kaiser Permanente `     $   13.40
## 23 2017-06-06 Kaiser Permanente `     $   35.00
## 24 2017-06-02 Kaiser Permanente `     $  281.88
## 25 2017-05-29 Kaiser Permanente `     $   71.67
## 26 2017-05-23 Kaiser Permanente `     $   35.00
## 27 2017-05-22 Kaiser Permanente `     $   35.00
## 28 2017-05-19 Kaiser Permanente `     $   35.00
## 29 2017-05-16 Kaiser Permanente `     $   46.23
## 30 2017-05-01 Check 1565        `     $  563.76
## 31 2017-04-20 Kaiser Permanente `     $   13.40
## 32 2017-03-29 Kaiser Permanente `     $  428.40
## 33 2017-02-23 Check 1552        `     $  281.88
## 34 2017-02-10 Kaiser Permanente `     $   13.40
## 35 2017-02-10 Kaiser Permanente `     $  343.00
## 36 2017-02-08 Kaiser Permanente `     $  678.28
## 37 2017-02-08 Kaiser Permanente `     $  623.60
## 38 2017-01-10 Kaiser Permanente `     $   37.80
## 39 2017-01-06 Kaiser Permanente `     $   14.48
## 40 2017-01-04 Kaiser Permanente `     $  623.60
## 41 2017-01-04 Kaiser Permanente `     $  678.28
```

```r
print(paste("Total paid to Health Insurance = $",sum(d4$Amount),sep = ""))
```

```
## [1] "Total paid to Health Insurance = $10064.32"
```

###Health Savings Account


```r
dd4<-filter(d1,Category=="HealthSavingsAccount")
dd4<-select(dd4,Date,Original.Description,Amount)
dd44<-dd4
dd45<-mutate(dd44,z="`")
dd45<-select(dd45,Date,Original.Description,z,Amount)
dd45$Amount<- sprintf("$ %7.2f", dd45$Amount)
print(dd45,n=nrow(dd45))
```

```
## # A tibble: 1 x 4
##   Date       Original.Description                            z     Amount 
##   <date>     <fctr>                                          <chr> <chr>  
## 1 2017-12-14 BANK OF AMERICA DES:PLAN CONTR ID:RTLBOA000118~ `     $ 7750~
```

```r
print(paste("Total paid to Health Savings Account = $",sum(dd4$Amount),sep = ""))
```

```
## [1] "Total paid to Health Savings Account = $7750"
```


###Total Business Meals


```r
d3<-filter(d1,Category=="Business Meals")
d3<-select(d3,Date,Original.Description,Amount)
d33<-d3
d33<-mutate(d33,z="`")
d33<-select(d33,Date,Original.Description,z,Amount)
d33$Amount<- sprintf("$ %7.2f", d33$Amount)
print(d33,n=nrow(d33))
```

```
## # A tibble: 136 x 4
##     Date       Original.Description                     z     Amount   
##     <date>     <fctr>                                   <chr> <chr>    
##   1 2017-12-29 DENNY'S #7661 18007336 CAMERON PARK CA   `     $   19.45
##   2 2017-12-26 LAZY DOG RESTAURANT 23 FOLSOM CA         `     $   45.57
##   3 2017-12-20 ADALBERTO'S MEXICAN FOOD SACRAMENTO CA   `     $   12.29
##   4 2017-12-19 SELLANDS EDH EL DORADO HILCA             `     $   39.50
##   5 2017-12-18 PEDRICK PRODUCE DIXON CA                 `     $   56.14
##   6 2017-12-15 VELVET GRILL & CREAMERY GALT CA          `     $   37.61
##   7 2017-12-12 WALLYS PIZZA BAR CAMERON PARK CA         `     $   32.25
##   8 2017-12-11 CARL'S JR 7058 CAMERON PARK CA           `     $   16.77
##   9 2017-11-28 TORTILLA FLATS CANTINA PLACERVILLE CA    `     $  107.14
##  10 2017-11-21 ADALBERTO'S MEXICAN FOOD SACRAMENTO CA   `     $   16.94
##  11 2017-11-20 CARL'S JR 7058 CAMERON PARK CA           `     $   18.30
##  12 2017-11-19 QUE VIVA CAMERON PARK CA                 `     $   33.36
##  13 2017-11-15 SUBWAY 00034744 CAMERON PARK CA          `     $   16.81
##  14 2017-11-14 CARL'S JR 7058 CAMERON PARK CA           `     $   18.01
##  15 2017-11-13 CARL'S JR 7058 CAMERON PARK CA           `     $   15.98
##  16 2017-11-10 APPLEBEES CAME18218255 CAMERON PARK CA   `     $   27.45
##  17 2017-11-09 QUE VIVA CAMERON PARK CA                 `     $   24.76
##  18 2017-11-06 SUBWAY 00034744 CAMERON PARK CA          `     $   15.86
##  19 2017-11-02 CARL'S JR 7058 CAMERON PARK CA           `     $   15.98
##  20 2017-10-31 JACK IN THE BOX 3439 EL DORADO HILCA     `     $   13.92
##  21 2017-10-28 PEDRICK PRODUCE DIXON CA                 `     $   23.53
##  22 2017-10-21 SUBWAY 00034744 CAMERON PARK CA          `     $   15.00
##  23 2017-10-19 JACK IN THE BOX 3439 EL DORADO HILCA     `     $   11.78
##  24 2017-10-17 ADALBERTO'S MEXICAN FOOD SACRAMENTO CA   `     $    8.34
##  25 2017-10-17 QUE VIVA CAMERON PARK CA                 `     $   21.96
##  26 2017-10-17 QUE VIVA CAMERON PARK CA                 `     $   26.96
##  27 2017-10-14 CASCADA PLACERVILLE CA                   `     $   44.60
##  28 2017-10-13 SUBWAY 00034744 CAMERON PARK CA          `     $   20.03
##  29 2017-10-13 JACK'S URBAN EATS FOLSOM CA              `     $   41.59
##  30 2017-10-04 SUBWAY 00034744 CAMERON PARK CA          `     $   17.88
##  31 2017-10-04 QUE VIVA CAMERON PARK CA                 `     $   29.85
##  32 2017-10-01 QUE VIVA CAMERON PARK CA                 `     $   37.24
##  33 2017-09-30 FAT'S ASIA BISTRO FOLSOM CA              `     $   28.94
##  34 2017-09-28 SUBWAY 00034744 CAMERON PARK CA          `     $   17.88
##  35 2017-09-18 ADALBERTO'S MEXICAN FOOD SACRAMENTO CA   `     $    8.34
##  36 2017-09-15 ADALBERTO'S MEXICAN FOOD SACRAMENTO CA   `     $   11.85
##  37 2017-09-13 CASCADA PLACERVILLE CA                   `     $  113.55
##  38 2017-09-11 CARL'S JR 7058 CAMERON PARK CA           `     $   18.20
##  39 2017-09-08 QUE VIVA CAMERON PARK CA                 `     $   31.70
##  40 2017-09-06 SUBWAY 00034744 CAMERON PARK CA          `     $   15.89
##  41 2017-09-05 CALIFORNIA KITCHEN PLACERVILLE CA        `     $   33.00
##  42 2017-08-28 WALLYS PIZZA BAR CAMERON PARK CA         `     $   32.87
##  43 2017-08-22 CARL'S JR 7058 CAMERON PARK CA           `     $   15.40
##  44 2017-08-19 CARL'S JR 7058 CAMERON PARK CA           `     $   10.96
##  45 2017-08-17 JACK IN THE BOX 3439 EL DORADO HILCA     `     $   13.37
##  46 2017-08-15 PEDRICK PRODUCE DIXON CA                 `     $   22.72
##  47 2017-08-11 SUBWAY 00034744 CAMERON PARK CA          `     $   17.79
##  48 2017-08-08 CARL'S JR 7058 CAMERON PARK CA           `     $   19.71
##  49 2017-08-07 CARL'S JR 7058 CAMERON PARK CA           `     $   17.74
##  50 2017-08-05 ADALBERTO'S MEXICAN FOOD SACRAMENTO CA   `     $    8.06
##  51 2017-08-03 SUBWAY 00034744 CAMERON PARK CA          `     $   17.79
##  52 2017-08-01 SOURDOUGH & CO EL DORADO HILCA           `     $   19.20
##  53 2017-07-29 CARL'S JR 7058 CAMERON PARK CA           `     $   17.74
##  54 2017-07-26 SOURDOUGH & CO EL DORADO HILCA           `     $   22.15
##  55 2017-07-21 CARL'S JR 7058 CAMERON PARK CA           `     $   14.93
##  56 2017-07-20 DENNY'S #7661 CAMERON PARK CA            `     $   26.19
##  57 2017-07-18 QUE VIVA MEXICAN RESTAURACAMERON PARK CA `     $   35.72
##  58 2017-07-17 CARL'S JR 7058 CAMERON PARK CA           `     $   17.55
##  59 2017-07-15 CARL'S JR 7058 CAMERON PARK CA           `     $   16.57
##  60 2017-07-14 CARL'S JR 7058 CAMERON PARK CA           `     $   16.86
##  61 2017-07-13 VELVET GRILL & CREAMERY GALT CA          `     $   38.89
##  62 2017-07-11 WALLYS PIZZA BAR CAMERON PARK CA         `     $   29.11
##  63 2017-07-10 CARL'S JR 7058 CAMERON PARK CA           `     $   17.74
##  64 2017-07-07 CARL'S JR 7058 CAMERON PARK CA           `     $   14.77
##  65 2017-07-05 SUBWAY 00034744 CAMERON PARK CA          `     $   16.50
##  66 2017-07-03 JACK IN THE BOX 4367 GALT CA             `     $   12.20
##  67 2017-06-30 CASCADA PLACERVILLE CA                   `     $   37.65
##  68 2017-06-29 CARL'S JR 7058 CAMERON PARK CA           `     $   15.98
##  69 2017-06-27 CARL'S JR 7058 CAMERON PARK CA           `     $   15.98
##  70 2017-06-26 CARL'S JR 7058 CAMERON PARK CA           `     $   15.98
##  71 2017-06-24 DENNY'S #7661 CAMERON PARK CA            `     $   28.57
##  72 2017-06-22 CARL'S JR 7058 CAMERON PARK CA           `     $   17.16
##  73 2017-06-20 JACK IN THE BOX 3439 EL DORADO HILCA     `     $    8.88
##  74 2017-06-19 JACK IN THE BOX #0541 PLACERVILLE CA     `     $   10.27
##  75 2017-06-17 DENNY'S #7661 CAMERON PARK CA            `     $   26.36
##  76 2017-06-16 JACK IN THE BOX 3439 EL DORADO HILCA     `     $    9.20
##  77 2017-06-15 PEDRICK PRODUCE DIXON CA                 `     $   40.46
##  78 2017-06-14 JACK IN THE BOX #3439 EL DORADO HILCA    `     $   10.27
##  79 2017-06-14 CASCADA PLACERVILLE CA                   `     $  145.25
##  80 2017-06-12 CARL'S JR 7058 CAMERON PARK CA           `     $   17.45
##  81 2017-06-10 CARL'S JR 7058 CAMERON PARK CA           `     $   17.16
##  82 2017-06-09 JACK IN THE BOX #3439 EL DORADO HILCA    `     $   10.81
##  83 2017-06-08 ROUND TABLE PIZZA 6 CAMERON PARK CA      `     $   29.76
##  84 2017-06-06 JACK IN THE BOX #3439 EL DORADO HILCA    `     $   10.27
##  85 2017-05-25 JACK IN THE BOX #3439 EL DORADO HILCA    `     $   10.06
##  86 2017-05-20 JACK IN THE BOX #3439 EL DORADO HILCA    `     $   10.06
##  87 2017-05-18 BURGER KING #11982 ROSEVILLE CA          `     $    2.46
##  88 2017-05-13 JACK IN THE BOX #3439 EL DORADO HILCA    `     $   11.13
##  89 2017-05-11 JACK IN THE BOX #3439 EL DORADO HILCA    `     $   15.85
##  90 2017-05-09 BURGER KING #11982 ROSEVILLE CA          `     $    2.46
##  91 2017-05-08 QUE VIVA MEXICAN RESTAURACAMERON PARK CA `     $   25.86
##  92 2017-05-06 JACK IN THE BOX #3439 EL DORADO HILCA    `     $   10.60
##  93 2017-04-21 FOUR SISTERS CAFE ROSEVILLE CA           `     $   25.20
##  94 2017-04-20 JAMIE'S BAR AND GRILL SACRAMENTO CA      `     $   83.00
##  95 2017-04-13 QUE VIVA MEXICAN RESTAURACAMERON PARK CA `     $   32.55
##  96 2017-04-10 DENNY'S #7661 CAMERON PARK CA            `     $   80.00
##  97 2017-04-08 JACK IN THE BOX #3439 EL DORADO HILCA    `     $   14.02
##  98 2017-04-06 QUE VIVA MEXICAN RESTAURACAMERON PARK CA `     $   24.17
##  99 2017-04-05 JACK IN THE BOX #3442 FOLSOM CA          `     $    8.82
## 100 2017-03-29 CASCADA PLACERVILLE CA                   `     $  119.10
## 101 2017-03-28 JACK IN THE BOX #3439 EL DORADO HILCA    `     $   15.10
## 102 2017-03-25 SUBWAY 00034744 CAMERON PARK CA          `     $   16.09
## 103 2017-03-15 CARL'S JR 7058 CAMERON PARK CA           `     $    7.02
## 104 2017-03-15 CARL'S JR 7058 CAMERON PARK CA           `     $   17.74
## 105 2017-03-14 PEDRICK PRODUCE DIXON CA                 `     $   17.37
## 106 2017-03-09 CARL'S JR 7058 CAMERON PARK CA           `     $   15.81
## 107 2017-03-04 CARL'S JR 7058 CAMERON PARK CA           `     $   17.74
## 108 2017-03-02 QUE VIVA MEXICAN RESTAURACAMERON PARK CA `     $   31.16
## 109 2017-02-28 WALLYS PIZZA BAR CAMERON PARK CA         `     $   26.44
## 110 2017-02-27 SUBWAY 00034744 CAMERON PARK CA          `     $   16.52
## 111 2017-02-27 SUBWAY 00034744 CAMERON PARK CA          `     $    7.78
## 112 2017-02-27 PEDRICK PRODUCE DIXON CA                 `     $   26.71
## 113 2017-02-25 CARL'S JR 7058 CAMERON PARK CA           `     $   15.84
## 114 2017-02-22 JACK IN THE BOX #3439 EL DORADO HILCA    `     $   17.44
## 115 2017-02-18 CALIFORNIA KITCHEN PLACERVILLE CA        `     $   27.66
## 116 2017-02-16 SUBWAY 00034744 CAMERON PARK CA          `     $   15.55
## 117 2017-02-14 WALLYS PIZZA BAR CAMERON PARK CA         `     $   27.96
## 118 2017-02-13 CARL'S JR 7058 CAMERON PARK CA           `     $   16.87
## 119 2017-02-11 ADALBERTO'S MEXICAN FOOD SACRAMENTO CA   `     $   18.19
## 120 2017-02-09 SUBWAY 00034744 CAMERON PARK CA          `     $   11.80
## 121 2017-02-09 CARL'S JR 7058 CAMERON PARK CA           `     $    6.93
## 122 2017-02-08 CARL'S JR 7058 CAMERON PARK CA           `     $   13.88
## 123 2017-02-04 CARL'S JR 7058 CAMERON PARK CA           `     $   18.68
## 124 2017-02-02 CARL'S JR 7058 CAMERON PARK CA           `     $   17.16
## 125 2017-02-01 WALLYS PIZZA BAR CAMERON PARK CA         `     $   26.44
## 126 2017-01-28 CARL'S JR 7058 CAMERON PARK CA           `     $   17.16
## 127 2017-01-26 SMF SQUEEZE INN TERM A SACRAMENTO CA     `     $    9.49
## 128 2017-01-21 IN-N-OUT BURGER #139 PLACERVILLE CA      `     $    4.96
## 129 2017-01-20 PEDRICK PRODUCE DIXON CA                 `     $   57.59
## 130 2017-01-20 SUBWAY 00034744 CAMERON PARK CA          `     $    7.78
## 131 2017-01-18 JACK IN THE BOX #3439 EL DORADO HILCA    `     $   12.53
## 132 2017-01-17 CASCADA PLACERVILLE CA                   `     $  132.85
## 133 2017-01-16 SUBWAY 00034744 CAMERON PARK CA          `     $   20.75
## 134 2017-01-12 QUE VIVA MEXICAN RESTAURACAMERON PARK CA `     $   39.14
## 135 2017-01-06 WALLYS PIZZA BAR CAMERON PARK CA         `     $   26.49
## 136 2017-01-05 CALIFORNIA KITCHEN PLACERVILLE CA        `     $   47.83
```

```r
print(paste("Total Business Meals = $",sum(d3$Amount),sep = ""))
```

```
## [1] "Total Business Meals = $3423.98"
```



###Business phone/internet


```r
dd5<-filter(d1,Category=="bus phone/internet")
dd5<-select(dd5,Date,Original.Description,Amount)
dd55<-dd5
dd55<-mutate(dd55,z="`")
dd55<-select(dd55,Date,Original.Description,z,Amount)
dd55$Amount<- sprintf("$ %7.2f", dd55$Amount)
print(dd55,n=nrow(dd55))
```

```
## # A tibble: 22 x 4
##    Date       Original.Description                           z     Amount 
##    <date>     <fctr>                                         <chr> <chr>  
##  1 2017-11-28 AT&T MOBILITY Bill Payment                     `     $  194~
##  2 2017-11-08 ATT DES:Payment ID:XXXXX7011EPAYK INDN:sherry~ `     $   86~
##  3 2017-10-30 AT&T Mobility Bill Payment                     `     $  189~
##  4 2017-10-10 ATT DES:Payment ID:XXXXX9011EPAYG INDN:sherry~ `     $   86~
##  5 2017-09-15 AT&T Mobility Bill Payment                     `     $  201~
##  6 2017-09-11 ATT DES:Payment ID:XXXXX2011EPAYC INDN:sherry~ `     $   83~
##  7 2017-08-29 AT&T MOBILITY Bill Payment                     `     $  183~
##  8 2017-08-08 ATT DES:Payment ID:XXXXX7011EPAYW INDN:sherry~ `     $   91~
##  9 2017-07-13 AT&T Mobility Bill Payment                     `     $  183~
## 10 2017-07-10 ATT DES:Payment ID:XXXXX0011EPAYS INDN:sherry~ `     $   91~
## 11 2017-06-26 AT&T MOBILITY Bill Payment                     `     $  188~
## 12 2017-06-08 ATT DES:Payment ID:XXXXX6011EPAYN INDN:sherry~ `     $   91~
## 13 2017-06-02 AT&T Mobility Bill Payment                     `     $  183~
## 14 2017-05-09 ATT DES:Payment ID:XXXXX4011EPAYI INDN:sherry~ `     $   91~
## 15 2017-04-20 AT&T Mobility Bill Payment                     `     $  188~
## 16 2017-04-10 ATT DES:Payment ID:XXXXX4011EPAYE INDN:sherry~ `     $   91~
## 17 2017-03-28 AT&T MOBILITY Bill Payment                     `     $  183~
## 18 2017-03-13 ATT DES:Payment ID:XXXXX1012EPAYD INDN:sherry~ `     $   91~
## 19 2017-02-24 AT&T MOBILITY Bill Payment                     `     $  183~
## 20 2017-02-08 ATT DES:Payment ID:XXXXX8002EPAYS INDN:sherry~ `     $   90~
## 21 2017-01-09 ATT DES:Payment ID:XXXXX5012EPAYS INDN:sherry~ `     $   90~
## 22 2017-01-06 AT&T*BILL PAYMENT WWW.ATT.COM TX               `     $  376~
```

```r
print(paste("Total paid to Business Phone/Internet = $",sum(dd5$Amount),sep = ""))
```

```
## [1] "Total paid to Business Phone/Internet = $3241.91"
```


###Business Travel


```r
dd6<-filter(d1,Category=="Business Travel")
dd6<-select(dd6,Date,Original.Description,Amount)
dd66<-dd6
dd66<-mutate(dd66,z="`")
dd66<-select(dd66,Date,Original.Description,z,Amount)
dd66$Amount<- sprintf("$ %7.2f", dd66$Amount)
print(dd66,n=nrow(dd66))
```

```
## # A tibble: 16 x 4
##    Date       Original.Description                           z     Amount 
##    <date>     <fctr>                                         <chr> <chr>  
##  1 2017-10-13 CHECKCARD 1012 AA INFLIGHT VISA FACET PHOENIX~ `     $    1~
##  2 2017-10-12 AMERICAN AIR0010265351813FORT WORTH TX         `     $   25~
##  3 2017-10-05 AMERICAN AIR0010264751280FORT WORTH TX         `     $   25~
##  4 2017-10-04 AMERICAN AIR0010659284841FORT WORTH TX         `     $   13~
##  5 2017-09-14 AMERICAN AIR0018654873036FORT WORTH TX         `     $  351~
##  6 2017-09-04 DELTA AIR Baggage Fee SALT LAKE CTYUT          `     $   25~
##  7 2017-08-31 DELTA AIR Baggage Fee SACRAMENTO CA            `     $   25~
##  8 2017-07-17 JETAIR SHINGLE SPRINCA                         `     $  357~
##  9 2017-07-10 DELTA AIR 0068636969895BELLEVUE WA             `     $  176~
## 10 2017-05-02 DELTA AIR 0068618080762BELLEVUE WA             `     $  576~
## 11 2017-03-13 DELTA AIR Baggage Fee SALT LAKE CTYUT          `     $   25~
## 12 2017-03-08 DELTA AIR Baggage Fee SACRAMENTO CA            `     $   25~
## 13 2017-02-15 DELTA AIR 0067947592249BELLEVUE WA             `     $  156~
## 14 2017-01-28 THRIFTY CAR RENTAL SALT LAKE CTYUT             `     $   77~
## 15 2017-01-28 SMF PARKING SACRAMENTO CA                      `     $   24~
## 16 2017-01-28 HOLIDAY INN EXPRESS & SU AMERICAN FORKUT       `     $  197~
```

```r
print(paste("Total paid to Business Travel = $",sum(dd6$Amount),sep = ""))
```

```
## [1] "Total paid to Business Travel = $2081.92"
```


###Office Supplies


```r
d8<-filter(d1,Category=="Office Supplies")
d8<-select(d8,Date,Original.Description,Amount)
d88<-d8
d88<-mutate(d88,z="`")
d88<-select(d88,Date,Original.Description,z,Amount)
d88$Amount<- sprintf("$ %7.2f", d88$Amount)
print(d88,n=nrow(d88))
```

```
## # A tibble: 90 x 4
##    Date       Original.Description                    z     Amount   
##    <date>     <fctr>                                  <chr> <chr>    
##  1 2017-12-29 BEST BUY 00008458 FOLSOM CA             `     $   59.25
##  2 2017-12-23 RSTUDIO, INC. RSTUDIO.COM MA            `     $    9.00
##  3 2017-12-22 SAMSCLUB #6620 FOLSOM CA                `     $   68.64
##  4 2017-12-22 A LA MODE, INC 800-252-6633 FL          `     $ 1499.00
##  5 2017-12-21 AMAZON MKTPLACE PMTS AMZN.COM/BILLWA    `     $   27.86
##  6 2017-12-20 AMAZON MKTPLACE PMTS AMZN.COM/BILLWA    `     $   43.98
##  7 2017-12-20 AMAZON MKTPLACE PMTS AMZN.COM/BILLWA    `     $   19.07
##  8 2017-12-18 SAMSCLUB.COM 8887467726 AR              `     $  160.85
##  9 2017-12-18 AMAZON MKTPLACE PMTS AMZN.COM/BILLWA    `     $   18.00
## 10 2017-12-16 AMAZON MKTPLACE PMTS AMZN.COM/BILLWA    `     $   32.12
## 11 2017-12-15 AMAZON MKTPLACE PMTS AMZN.COM/BILLWA    `     $   49.37
## 12 2017-12-11 SAMSCLUB #6620 FOLSOM CA                `     $  137.71
## 13 2017-11-30 SAMSCLUB #6620 FOLSOM CA                `     $   52.40
## 14 2017-11-30 APPRAISAL SCOPE INC 800-4347260 OK      `     $   21.00
## 15 2017-11-24 AMAZON MKTPLACE PMTS AMZN.COM/BILLWA    `     $   21.99
## 16 2017-11-23 RSTUDIO, INC. RSTUDIO.COM MA            `     $    9.00
## 17 2017-11-18 AMAZON MKTPLACE PMTS AMZN.COM/BILLWA    `     $   81.05
## 18 2017-11-18 AMAZON MKTPLACE PMTS AMZN.COM/BILLWA    `     $    8.13
## 19 2017-11-15 AMAZON MKTPLACE PMTS AMZN.COM/BILLWA    `     $   10.49
## 20 2017-11-14 AMAZON MKTPLACE PMTS AMZN.COM/BILLWA    `     $   55.81
## 21 2017-11-13 AMAZON MKTPLACE PMTS AMZN.COM/BILLWA    `     $   23.98
## 22 2017-11-12 AMAZON MKTPLACE PMTS AMZN.COM/BILLWA    `     $   43.98
## 23 2017-11-08 AMAZON MKTPLACE PMTS AMZN.COM/BILLWA    `     $   55.99
## 24 2017-11-02 AMAZON MKTPLACE PMTS AMZN.COM/BILLWA    `     $  109.97
## 25 2017-10-09 AMAZON MKTPLACE PMTS AMZN.COM/BILLWA    `     $   54.57
## 26 2017-10-07 AMAZON MKTPLACE PMTS AMZN.COM/BILLWA    `     $   16.99
## 27 2017-09-16 AMAZON MKTPLACE PMTS AMZN.COM/BILLWA    `     $   13.95
## 28 2017-09-15 SAMSCLUB #6620 FOLSOM CA                `     $  101.20
## 29 2017-09-15 AMAZON MKTPLACE PMTS AMZN.COM/BILLWA    `     $   15.05
## 30 2017-09-15 AMAZON MKTPLACE PMTS AMZN.COM/BILLWA    `     $   31.21
## 31 2017-09-15 AMAZON MKTPLACE PMTS AMZN.COM/BILLWA    `     $    7.99
## 32 2017-08-30 AMAZON MKTPLACE PMTS AMZN.COM/BILLWA    `     $  148.99
## 33 2017-08-25 AMAZON MKTPLACE PMTS AMZN.COM/BILLWA    `     $   38.56
## 34 2017-08-25 AMAZON MKTPLACE PMTS AMZN.COM/BILLWA    `     $   15.90
## 35 2017-08-25 AMAZON MKTPLACE PMTS AMZN.COM/BILLWA    `     $   21.00
## 36 2017-08-24 AMAZON MKTPLACE PMTS AMZN.COM/BILLWA    `     $    9.96
## 37 2017-08-20 AMAZON MKTPLACE PMTS AMZN.COM/BILLWA    `     $   29.99
## 38 2017-08-18 AMAZON MKTPLACE PMTS AMZN.COM/BILLWA    `     $   28.99
## 39 2017-08-18 AMAZON MKTPLACE PMTS AMZN.COM/BILLWA    `     $    8.99
## 40 2017-07-20 SAMS CLUB #6620 FOLSOM CA               `     $   83.71
## 41 2017-07-14 SAMS CLUB - #6620 FOLSOM CA             `     $  189.98
## 42 2017-07-14 SAMS CLUB - #6620 FOLSOM CA             `     $   56.98
## 43 2017-07-12 AMAZON MKTPLACE PMTS AMZN.COM/BILLWA    `     $  197.61
## 44 2017-07-05 AMAZON MKTPLACE PMTS AMZN.COM/BILLWA    `     $   69.43
## 45 2017-07-03 SAMSCLUB #6620 FOLSOM CA                `     $  148.54
## 46 2017-07-01 AMAZON MKTPLACE PMTS AMZN.COM/BILLWA    `     $   12.75
## 47 2017-06-25 AMAZON MKTPLACE PMTS AMZN.COM/BILLWA    `     $   63.92
## 48 2017-06-17 SAMS CLUB #6620 FOLSOM CA               `     $   46.79
## 49 2017-06-17 SAMSCLUB #6620 FOLSOM CA                `     $   26.88
## 50 2017-06-13 SAMSCLUB #6620 FOLSOM CA                `     $   15.96
## 51 2017-06-13 SAMS CLUB #6620 FOLSOM CA               `     $   81.92
## 52 2017-06-09 SAMS CLUB #6620 FOLSOM CA               `     $   61.91
## 53 2017-06-09 AMAZON MKTPLACE PMTS AMZN.COM/BILLWA    `     $   64.70
## 54 2017-06-07 SAMSCLUB #6620 FOLSOM CA                `     $   27.91
## 55 2017-06-07 SAMS CLUB #6620 FOLSOM CA               `     $   45.00
## 56 2017-06-06 AMAZON MKTPLACE PMTS AMZN.COM/BILLWA    `     $   29.80
## 57 2017-06-06 AMAZON MKTPLACE PMTS AMZN.COM/BILLWA    `     $   41.99
## 58 2017-06-05 AMAZON MKTPLACE PMTS AMZN.COM/BILLWA    `     $   45.06
## 59 2017-06-04 AMAZON MKTPLACE PMTS AMZN.COM/BILLWA    `     $   52.95
## 60 2017-06-03 AMAZON MKTPLACE PMTS AMZN.COM/BILLWA    `     $   39.99
## 61 2017-06-02 AMAZON MKTPLACE PMTS AMZN.COM/BILLWA    `     $   28.69
## 62 2017-05-31 AMAZON MKTPLACE PMTS AMZN.COM/BILLWA    `     $  359.96
## 63 2017-05-30 SAMS CLUB - #6620 FOLSOM CA             `     $   16.08
## 64 2017-05-23 SAMS CLUB - #6620 FOLSOM CA             `     $   50.26
## 65 2017-05-19 SAMS CLUB - #6620 FOLSOM CA             `     $   45.73
## 66 2017-05-19 AMAZON MKTPLACE PMTS AMZN.COM/BILLWA    `     $   45.59
## 67 2017-05-17 SAMS CLUB - #6620 FOLSOM CA             `     $    9.96
## 68 2017-05-17 SAMS CLUB - #6620 FOLSOM CA             `     $   47.32
## 69 2017-04-26 AMAZON MKTPLACE PMTS AMZN.COM/BILLWA    `     $   32.88
## 70 2017-04-21 SAMS CLUB - #6620 FOLSOM CA             `     $    8.39
## 71 2017-04-21 SAMS CLUB - #6620 FOLSOM CA             `     $   87.80
## 72 2017-04-13 AMAZON MKTPLACE PMTS AMZN.COM/BILLWA    `     $   18.46
## 73 2017-04-11 SAMS CLUB - #6620 FOLSOM CA             `     $   60.37
## 74 2017-04-11 SAMS CLUB - #6620 FOLSOM CA             `     $   87.60
## 75 2017-04-08 IKEA WEST SACRAMENTO WEST SACRAMENCA    `     $   28.06
## 76 2017-03-15 AMAZON MKTPLACE PMTS AMZN.COM/BILLWA    `     $   52.97
## 77 2017-03-08 SAMS CLUB - #6620 FOLSOM CA             `     $   91.54
## 78 2017-03-06 SAMSCLUB.COM 888-746-7726 AR            `     $   24.51
## 79 2017-03-03 AMAZON MKTPLACE PMTS AMZN.COM/BILLWA    `     $  245.96
## 80 2017-03-03 AMAZON MKTPLACE PMTS AMZN.COM/BILLWA    `     $   54.49
## 81 2017-02-28 SAMS CLUB - #6620 FOLSOM CA             `     $  115.40
## 82 2017-02-23 AMAZON MKTPLACE PMTS AMZN.COM/BILLWA    `     $   59.98
## 83 2017-02-22 AMAZON MKTPLACE PMTS AMZN.COM/BILLWA    `     $   18.51
## 84 2017-02-15 AMAZON MKTPLACE PMTS AMZN.COM/BILLWA    `     $    9.42
## 85 2017-02-14 CARTRIDGE WORLD # EL DORADO HILCA       `     $  179.54
## 86 2017-02-10 SAMS CLUB - #6620 FOLSOM CA             `     $  123.49
## 87 2017-02-04 AMAZON MKTPLACE PMTS AMZN.COM/BILLWA    `     $    8.07
## 88 2017-02-04 AMAZON MKTPLACE PMTS AMZN.COM/BILLWA    `     $  160.41
## 89 2017-01-26 AMAZON MKTPLACE PMTS AMZN.COM/BILLWA    `     $  102.27
## 90 2017-01-03 PAYPAL *ACCESSDISPLAYGR 516-678-7772 NY `     $   70.84
```

```r
print(paste("Total paid to Office Supplies = $",sum(d8$Amount),sep = ""))
```

```
## [1] "Total paid to Office Supplies = $6810.31"
```



###Charity


```r
d6<-filter(d1,Category=="Charity")
d6<-select(d6,Date,Description,Amount)
d66<-d6
d66<-mutate(d66,z="`")
d66<-select(d66,Date,Description,z,Amount)
d66$Amount<- sprintf("$ %9.2f", d66$Amount)
print(d66,n=nrow(d66))
```

```
## # A tibble: 1 x 4
##   Date       Description z     Amount     
##   <date>     <fctr>      <chr> <chr>      
## 1 2017-09-14 Lds Church  `     $  13000.00
```

```r
print(paste("Total paid to Charity = $",sum(d6$Amount),sep = ""))
```

```
## [1] "Total paid to Charity = $13000"
```


###Doctor&Dentist


```r
d7<-filter(d1,Category=="Doctor" | Category=="Dentist")
d7<-select(d7,Date,Category,Description,Amount)
d77<-d7
d77<-mutate(d77,z="`")
d77<-select(d77,Date,Category,Description,z,Amount)
d77$Amount<- sprintf("$ %7.2f", d77$Amount)
print(d77,n=nrow(d77))
```

```
## # A tibble: 9 x 5
##   Date       Category Description          z     Amount   
##   <date>     <fctr>   <fctr>               <chr> <chr>    
## 1 2017-10-02 Doctor   Eye Exam             `     $  101.96
## 2 2017-10-02 Doctor   Lenscrafters         `     $ 1210.62
## 3 2017-09-26 Doctor   Eye Exam             `     $  101.96
## 4 2017-06-05 Dentist  Pleasant Valley Apts `     $  540.00
## 5 2017-05-08 Dentist  Pleasant Valley Apts `     $  141.00
## 6 2017-05-01 Dentist  Forrest R Boozer DDS `     $  149.00
## 7 2017-04-24 Dentist  Pleasant Valley Apts `     $  239.00
## 8 2017-04-14 Dentist  Forrest R Boozer DDS `     $  149.00
## 9 2017-03-24 Doctor   In Kime Performance  `     $   50.00
```

```r
print(paste("Total paid to Doctor and Dentist = $",sum(d7$Amount),sep = ""))
```

```
## [1] "Total paid to Doctor and Dentist = $2682.54"
```



###Driver Cost


```r
d8<-filter(d1,Category=="Driver cost")
d8<-select(d8,Date,Original.Description,Amount)
d88<-d8
d88<-mutate(d88,z="`")
d88<-select(d88,Date,Original.Description,z,Amount)
d88$Amount<- sprintf("$ %7.2f", d88$Amount)
print(d88,n=nrow(d88))
```

```
## # A tibble: 6 x 4
##   Date       Original.Description z     Amount   
##   <date>     <fctr>               <chr> <chr>    
## 1 2017-12-29 Check 1583           `     $ 2000.00
## 2 2017-12-29 Check 1582           `     $ 4558.00
## 3 2017-12-20 Check 1581           `     $  250.00
## 4 2017-07-07 Check 1571           `     $  750.00
## 5 2017-05-01 Check 1566           `     $  750.00
## 6 2017-02-10 Check 1551           `     $  850.00
```

```r
print(paste("Total paid to Driver costs = $",sum(d8$Amount),sep = ""))
```

```
## [1] "Total paid to Driver costs = $9158"
```


###Estimated Federal & State tax payments


```r
dd7<-filter(d1,Category=="Federal Tax" | Category=="State Tax")
dd7<-select(dd7,Date,Category,Original.Description,Amount,Notes)
dd77<-dd7
dd77<-mutate(dd77,z="`")
dd77<-select(dd77,Date,Category,Original.Description,z,Amount,Notes)
dd77$Amount<- sprintf("$ %7.2f", dd77$Amount)
print(dd77,n=nrow(dd77))
```

```
## # A tibble: 9 x 6
##   Date       Category    Original.Description z     Amount    Notes 
##   <date>     <fctr>      <fctr>               <chr> <chr>     <fctr>
## 1 2017-09-25 Federal Tax Check 1574           `     $ 2500.00 ""    
## 2 2017-09-22 State Tax   Check 1573           `     $  300.00 ""    
## 3 2017-06-22 State Tax   Check 1569           `     $  300.00 ""    
## 4 2017-06-21 Federal Tax Check 1570           `     $ 2500.00 ""    
## 5 2017-06-19 State Tax   Check 1568           `     $  143.35 ""    
## 6 2017-04-24 State Tax   Check 1564           `     $  949.00 ""    
## 7 2017-04-24 State Tax   Check 1563           `     $  319.00 ""    
## 8 2017-04-17 Federal Tax Check 1562           `     $ 2378.00 ""    
## 9 2017-04-17 Federal Tax Check 1561           `     $ 4000.00 ""
```

###Property and other taxes


```r
d8<-filter(d1,Category=="Taxes")
d8<-select(d8,Date,Original.Description,Amount)
d88<-d8
d88<-mutate(d88,z="`")
d88<-select(d88,Date,Original.Description,z,Amount)
d88$Amount<- sprintf("$ %7.2f", d88$Amount)
print(d88,n=nrow(d88))
```

```
## # A tibble: 2 x 4
##   Date       Original.Description                     z     Amount   
##   <date>     <fctr>                                   <chr> <chr>    
## 1 2017-12-09 EL DORADO CNTY TRES TAX 530-621-5815 CA  `     $ 2691.93
## 2 2017-12-09 CONVENIENCE PAY SERVICES VERONICA.HUFFPA `     $   62.22
```

```r
print(paste("Total paid to property&other TAXES = $",sum(d8$Amount),sep = ""))
```

```
## [1] "Total paid to property&other TAXES = $2754.15"
```


###Business Services,Electronics & Software,Legal,Printing,Tuition


```r
d7<-filter(d1,Category=="Business Services" | Category=="Electronics & Software"| Category=="Legal"| Category=="Printing"| Category==" Tuition")
d7<-select(d7,Date,Category,Original.Description,Amount)
d77<-d7
d77<-mutate(d77,z="`")
d77<-select(d77,Date,Category,Original.Description,z,Amount)
d77$Amount<- sprintf("$ %7.2f", d77$Amount)
print(d77,n=nrow(d77))
```

```
## # A tibble: 28 x 5
##    Date       Category               Original.Description    z     Amount 
##    <date>     <fctr>                 <fctr>                  <chr> <chr>  
##  1 2017-12-19 Legal                  APPRAISAL SCOPE INC 80~ `     $   21~
##  2 2017-12-17 Legal                  COURSERA 650-265-7649 ~ `     $   49~
##  3 2017-12-14 Business Services      MetroList El Dorado Bi~ `     $  144~
##  4 2017-12-13 Legal                  APPRAISAL SCOPE INC 80~ `     $   21~
##  5 2017-11-22 Legal                  APPRAISAL SCOPE INC 80~ `     $   21~
##  6 2017-11-22 Legal                  APPRAISAL SCOPE INC 80~ `     $   21~
##  7 2017-11-21 Legal                  ASSURANT APPRAISALS LX~ `     $    6~
##  8 2017-11-17 Legal                  COURSERA 650-265-7649 ~ `     $   49~
##  9 2017-11-05 Business Services      SILICONDUST USA INC 92~ `     $   35~
## 10 2017-10-31 Legal                  ASSURANT APPRAISALS LX~ `     $    6~
## 11 2017-10-23 Legal                  RSTUDIO, INC. 84444812~ `     $    9~
## 12 2017-10-17 Legal                  ASSURANT APPRAISALS LX~ `     $    6~
## 13 2017-10-08 Electronics & Software Dropbox*3GGYWNMMSGQ2 d~ `     $   99~
## 14 2017-09-26 Legal                  ASSURANT APPRAISALS LX~ `     $    6~
## 15 2017-09-19 Business Services      MetroList El Dorado Bi~ `     $  144~
## 16 2017-09-12 Legal                  ASSURANT APPRAISALS LX~ `     $   12~
## 17 2017-07-07 Business Services      THE BEAD SHOPPE ROSEVI~ `     $   23~
## 18 2017-06-15 Business Services      MetroList El Dorado Bi~ `     $  144~
## 19 2017-05-23 Printing               LEANPUB 6042650857 CA   `     $   30~
## 20 2017-05-05 Legal                  COURSERA 650-265-7649 ~ `     $   49~
## 21 2017-04-13 Legal                  INTUIT *TURBOTAX 800-4~ `     $   59~
## 22 2017-04-13 Legal                  INTUIT *TURBOTAX 800-4~ `     $   24~
## 23 2017-03-27 Printing               LEANPUB 6049168017 CA   `     $   15~
## 24 2017-03-10 Business Services      MetroList El Dorado Bi~ `     $  144~
## 25 2017-03-05 Printing               COURSERA 650-265-7649 ~ `     $   49~
## 26 2017-02-24 Printing               UDEMY.COM UDEMY.COM CA  `     $   10~
## 27 2017-02-07 Printing               LEANPUB 6042650857 CA   `     $   10~
## 28 2017-02-04 Legal                  COURSERA 650-265-7649 ~ `     $   49~
```

```r
print(paste("Total paid to Various Business expenses = $",sum(d7$Amount),sep = ""))
```

```
## [1] "Total paid to Various Business expenses = $1261.85"
```








