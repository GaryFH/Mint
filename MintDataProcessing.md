### Setup environment

    suppressWarnings(library(dplyr))

### Look at income and try to reconcile - note that $5,000 of interest income was cashed and not part of Mint income.

    d1<-tbl_df(read.csv("transactions03062019.csv"))
    d1$Date<- as.Date(d1$Date,format="%m/%d/%Y")
    d1<-filter(d1,Date>=as.Date("2018-01-01")&Date<=as.Date("2018-12-31"))

    d2<-filter(d1,Transaction.Type=="credit")

    d222<-filter(d2,Category=="appraisal income")
    print(paste("Total Interest Income = ","$",sum(d222$Amount),sep=""))

    ## [1] "Total Interest Income = $59565"

    d223<-filter(d2,Category=="Interest Income")
    print(paste("Total Interest Income = ","$",(sum(d223$Amount)+5000),sep=""))

    ## [1] "Total Interest Income = $12403.09"

    d2b<-group_by(d2,Category)
    print(summarise(d2b,sum(Amount)),n=nrow(d2b))

    ## # A tibble: 13 x 2
    ##    Category            `sum(Amount)`
    ##    <fct>                       <dbl>
    ##  1 appraisal income          59565  
    ##  2 Credit Card Payment       59651. 
    ##  3 Groceries                    71.9
    ##  4 Health Insurance              8  
    ##  5 Income                       80.0
    ##  6 Interest Income            7403. 
    ##  7 Loan Repayment           327607. 
    ##  8 Office Supplies             164. 
    ##  9 Paycheck                   4390. 
    ## 10 Reimbursement             45000  
    ## 11 Shopping                    623. 
    ## 12 State Tax                   669  
    ## 13 Temp business loan        10000

    print(summarise(group_by(d2,Description),sum(Amount)),n=nrow(d2))

    ## # A tibble: 26 x 2
    ##    Description                            `sum(Amount)`
    ##    <fct>                                          <dbl>
    ##  1 Amazon                                        157.  
    ##  2 Bkofamerica ATM Deposit                     73123   
    ##  3 CA State Tax                                  669   
    ##  4 Capital One N                                5683.  
    ##  5 Corelogic Des Lc                             1000   
    ##  6 Costco                                         71.9 
    ##  7 Counter                                    329107.  
    ##  8 Deseret Book                                   31.9 
    ##  9 Floor Decor Rocklin                            60.2 
    ## 10 Gusto Des Ebv                                   0.01
    ## 11 Gusto Des Pay                                4390.  
    ## 12 Interest Earned                                 3.09
    ## 13 J. Jill                                        22.5 
    ## 14 Jo-Ann Stores                                  80.5 
    ## 15 Kaiser Permanente                               8   
    ## 16 Michaels                                       47.7 
    ## 17 Murcor Des Trade                            17177   
    ## 18 Murcor Trade Pay                              700   
    ## 19 Nordstrom                                     283.  
    ## 20 Nvs Pmd Des                                 18465   
    ## 21 Online Banking Advance                      10000   
    ## 22 Online Payment                              32781.  
    ## 23 Online Thank You                             9816.  
    ## 24 Returned Bill Viper                            80   
    ## 25 Sam's Club                                    164.  
    ## 26 Transfer from Adv Tiered Interest Chkg      11310.

    #View(filter(d2b,Category=="Interest Income"))
    #View(filter(d2b,Category=="appraisal income"))
    #View(filter(d2b,Category=="Reimbursement"))
    #View(filter(d2b,Description=="Thank You"))

### Overview of all Categories

    d11<-group_by(d1,Category)
    print(summarise(d11,sum(Amount)),n=nrow(d11))

    ## # A tibble: 56 x 2
    ##    Category             `sum(Amount)`
    ##    <fct>                        <dbl>
    ##  1 Advertising                   59  
    ##  2 Air Travel                    25  
    ##  3 appraisal income           59565  
    ##  4 ATM Fee                       94.6
    ##  5 Auto & Transport            1723. 
    ##  6 Auto Insurance              5048. 
    ##  7 bus phone/internet          3566. 
    ##  8 Business Meals              2196. 
    ##  9 Business Services           1559. 
    ## 10 Business Travel             1385. 
    ## 11 Charity                     8500  
    ## 12 Clothing                      61.2
    ## 13 Credit Card Payment        76355. 
    ## 14 Credit Card Payments       27481. 
    ## 15 Driver cost                 1000  
    ## 16 Electric Bill               2313. 
    ## 17 Entertainment                216  
    ## 18 Fast Food                     75.0
    ## 19 Federal Tax                 9825  
    ## 20 Finance Charge               132. 
    ## 21 Food & Dining                  9.5
    ## 22 Gas & Fuel                  2689. 
    ## 23 Gift                          35  
    ## 24 Gmail Bill                   120  
    ## 25 Groceries                   7025. 
    ## 26 Hair                          22.4
    ## 27 Health & Fitness              10  
    ## 28 Health Insurance            7974. 
    ## 29 HOA Dues                     500  
    ## 30 Home Improvement            5959. 
    ## 31 Hotel                         43.2
    ## 32 Income                        80.0
    ## 33 Interest Income             7403. 
    ## 34 IRA Contribution           15000  
    ## 35 Late Fee                      25  
    ## 36 Legal                        825. 
    ## 37 Life Insurance              2652  
    ## 38 Loan Repayment            327607. 
    ## 39 Mortgage & Rent            16155. 
    ## 40 Movies & DVDs                 72.3
    ## 41 Office Supplies             2970. 
    ## 42 Paycheck                    4390. 
    ## 43 Pet Food & Supplies           28.6
    ## 44 Pharmacy                     613. 
    ## 45 Propane Gas                  954. 
    ## 46 Property Tax                5431. 
    ## 47 Reimbursement              45000  
    ## 48 Restaurants                  821. 
    ## 49 Service & Parts               34.6
    ## 50 Shipping                      17.2
    ## 51 Shopping                    8701. 
    ## 52 State Tax                    669  
    ## 53 Temp business loan         30000  
    ## 54 Transfer                  298064. 
    ## 55 Trash Service                373. 
    ## 56 Utilities                    135

### Total Health Insurance - subtracted $2,000 from below to adjust for non insurance expenses

    d4<-filter(d1,Category=="Health Insurance")
    d4<-select(d4,Date,Description,Amount)
    d44<-d4
    d45<-mutate(d44,z="`")
    d45<-select(d45,Date,Description,z,Amount)
    d45$Amount<- sprintf("$ %7.2f", d45$Amount)
    print(d45,n=nrow(d45))

    ## # A tibble: 40 x 4
    ##    Date       Description            z     Amount   
    ##    <date>     <fct>                  <chr> <chr>    
    ##  1 2018-12-11 Aetna                  `     $   31.60
    ##  2 2018-12-03 Health Net             `     $  236.00
    ##  3 2018-11-27 Check 1606             `     $ 1002.58
    ##  4 2018-11-20 Check 1607             `     $  118.00
    ##  5 2018-11-14 Aetna                  `     $   31.60
    ##  6 2018-11-13 Kaiser Permanente      `     $   55.00
    ##  7 2018-10-11 Aetna                  `     $   31.60
    ##  8 2018-10-02 Check 1586             `     $  540.86
    ##  9 2018-09-28 Kaiser Permanente      `     $   11.67
    ## 10 2018-09-26 Kaiser Permanente      `     $  275.14
    ## 11 2018-09-21 Cms Medicare Insurance `     $  402.00
    ## 12 2018-08-30 Check 1586             `     $  540.86
    ## 13 2018-08-16 Kaiser Permanente      `     $   20.86
    ## 14 2018-07-31 Kaiser Permanente      `     $ 1081.72
    ## 15 2018-07-24 Dan Services           `     $   75.00
    ## 16 2018-07-24 Dan Services           `     $   75.00
    ## 17 2018-07-20 Kaiser Permanente      `     $   11.67
    ## 18 2018-07-17 Kaiser Permanente      `     $   30.20
    ## 19 2018-06-23 Kaiser Permanente      `     $   90.00
    ## 20 2018-06-18 Kaiser Permanente      `     $    8.00
    ## 21 2018-06-12 Kaiser Permanente      `     $   56.00
    ## 22 2018-05-29 Check 1597             `     $ 1081.72
    ## 23 2018-04-24 Kaiser Permanente      `     $   12.56
    ## 24 2018-04-20 Kaiser Permanente      `     $   26.67
    ## 25 2018-04-17 Kaiser Permanente      `     $   25.00
    ## 26 2018-04-17 Kaiser Permanente      `     $  173.69
    ## 27 2018-04-09 Check 1586             `     $  540.86
    ## 28 2018-04-03 Kaiser Permanente      `     $   12.56
    ## 29 2018-03-14 Kaiser Permanente      `     $   17.11
    ## 30 2018-03-14 Kaiser Permanente      `     $   55.00
    ## 31 2018-03-14 Kaiser Permanente      `     $   12.56
    ## 32 2018-03-12 Kaiser Permanente      `     $   60.00
    ## 33 2018-03-06 Kaiser Permanente      `     $   55.00
    ## 34 2018-02-27 Kaiser Permanente      `     $   35.00
    ## 35 2018-02-05 Kaiser Permanente      `     $   12.56
    ## 36 2018-02-02 Kaiser Permanente      `     $    8.69
    ## 37 2018-01-30 Check 1586             `     $  540.86
    ## 38 2018-01-23 Kaiser Permanente      `     $    9.54
    ## 39 2018-01-12 Kaiser Permanente      `     $   28.90
    ## 40 2018-01-12 Check 1586             `     $  540.86

    print(paste("Total paid to Health Insurance = $",sum(d4$Amount),sep = ""))

    ## [1] "Total paid to Health Insurance = $7974.5"

### Health Savings Account

    dd4<-filter(d1,Category=="HealthSavingsAccount")
    dd4<-select(dd4,Date,Original.Description,Amount)
    dd44<-dd4
    dd45<-mutate(dd44,z="`")
    dd45<-select(dd45,Date,Original.Description,z,Amount)
    dd45$Amount<- sprintf("$ %7.2f", dd45$Amount)
    print(dd45,n=nrow(dd45))

    ## # A tibble: 0 x 4
    ## # ... with 4 variables: Date <date>, Original.Description <fct>, z <chr>,
    ## #   Amount <chr>

    print(paste("Total paid to Health Savings Account = $",sum(dd4$Amount),sep = ""))

    ## [1] "Total paid to Health Savings Account = $0"

### Total Business Meals

    d3<-filter(d1,Category=="Business Meals")
    d3<-select(d3,Date,Original.Description,Amount)
    d33<-d3
    d33<-mutate(d33,z="`")
    d33<-select(d33,Date,Original.Description,z,Amount)
    d33$Amount<- sprintf("$ %7.2f", d33$Amount)
    print(d33,n=nrow(d33))

    ## # A tibble: 102 x 4
    ##     Date       Original.Description                     z     Amount   
    ##     <date>     <fct>                                    <chr> <chr>    
    ##   1 2018-12-31 CARL'S JR 7058 CAMERON PARK CA           `     $    9.64
    ##   2 2018-12-19 FAT'S ASIA BISTRO FOLSOM CA              `     $   28.94
    ##   3 2018-12-17 ADALBERTO'S MEXICAN FOOD SACRAMENTO CA   `     $   10.28
    ##   4 2018-12-11 CARL'S JR 7058 CAMERON PARK CA           `     $    6.44
    ##   5 2018-12-10 WALLYS PIZZA BAR - CAMEROCAMERON PARK CA `     $   32.26
    ##   6 2018-12-06 CARL'S JR 7058 CAMERON PARK CA           `     $    6.44
    ##   7 2018-12-04 JACK IN THE BOX 0541 PLACERVILLE CA      `     $   12.84
    ##   8 2018-12-03 CARL'S JR 7058 CAMERON PARK CA           `     $    6.44
    ##   9 2018-11-30 CARL'S JR 7058 CAMERON PARK CA           `     $    6.44
    ##  10 2018-11-23 CARL'S JR 7058 CAMERON PARK CA           `     $    9.64
    ##  11 2018-11-14 CARL'S JR 7058 CAMERON PARK CA           `     $    6.44
    ##  12 2018-11-13 CARL'S JR 7058 CAMERON PARK CA           `     $    6.51
    ##  13 2018-11-11 BRAMBLE BERRY 360-734-8278 WA            `     $   28.60
    ##  14 2018-11-07 CARL'S JR 7058 CAMERON PARK CA           `     $    6.44
    ##  15 2018-11-06 CARL'S JR 7058 CAMERON PARK CA           `     $    6.95
    ##  16 2018-11-05 CARL'S JR 7058 CAMERON PARK CA           `     $    9.64
    ##  17 2018-11-02 IN N OUT BURGER 139 PLACERVILLE CA       `     $   22.07
    ##  18 2018-10-29 CARL'S JR 7058 CAMERON PARK CA           `     $    6.44
    ##  19 2018-10-22 PANERA BREAD #202210 FOLSOM CA           `     $   24.12
    ##  20 2018-10-19 JACK'S URBAN EATS FOLSOM CA              `     $   26.34
    ##  21 2018-10-17 CARL'S JR 7058 CAMERON PARK CA           `     $    9.64
    ##  22 2018-10-13 CARL'S JR 7054 PLACERVILLE CA            `     $    6.50
    ##  23 2018-10-12 CARL'S JR 7058 CAMERON PARK CA           `     $    6.44
    ##  24 2018-10-06 SUBWAY 00322750 ELK GROVE CA             `     $   19.59
    ##  25 2018-10-05 SQ *PERRONS Q PLACERVILLE CA             `     $   35.50
    ##  26 2018-10-02 THE INDEPENDENT RESTAU PLACERVILLE CA    `     $  223.54
    ##  27 2018-10-02 JACK IN THE BOX 3439 EL DORADO HILCA     `     $    9.31
    ##  28 2018-10-01 SUBWAY 00034744 CAMERON PARK CA          `     $   18.10
    ##  29 2018-09-28 CARL'S JR 7058 CAMERON PARK CA           `     $    9.64
    ##  30 2018-09-26 CARL'S JR 7058 CAMERON PARK CA           `     $    9.32
    ##  31 2018-09-25 BRAMBLE BERRY 360-734-8278 WA            `     $   38.65
    ##  32 2018-09-25 MONTEREY BAY SPICE COMPA 831-426-2808 CA `     $   26.05
    ##  33 2018-09-25 APPLEBEES CAME18218255 CAMERON PARK CA   `     $   55.91
    ##  34 2018-09-24 CARL'S JR 7058 CAMERON PARK CA           `     $    9.32
    ##  35 2018-09-22 CARL'S JR 7058 CAMERON PARK CA           `     $    5.57
    ##  36 2018-09-20 CARL'S JR 7058 CAMERON PARK CA           `     $   20.88
    ##  37 2018-09-19 CARL'S JR 7058 CAMERON PARK CA           `     $    6.44
    ##  38 2018-09-11 QUE VIVA MEXICAN RESTAURACAMERON PARK CA `     $   28.08
    ##  39 2018-08-27 CARL'S JR 7058 CAMERON PARK CA           `     $    6.44
    ##  40 2018-08-24 CARL'S JR 7058 CAMERON PARK CA           `     $    9.20
    ##  41 2018-08-20 CARL'S JR 7058 CAMERON PARK CA           `     $    8.52
    ##  42 2018-08-20 LAZY DOG RESTAURANT 23 FOLSOM CA         `     $  146.72
    ##  43 2018-08-14 JACK IN THE BOX 0541 PLACERVILLE CA      `     $    7.27
    ##  44 2018-08-13 CARL'S JR 7058 CAMERON PARK CA           `     $   10.90
    ##  45 2018-08-06 CARL'S JR 7058 CAMERON PARK CA           `     $    6.44
    ##  46 2018-07-30 WALLYS PIZZA BAR CAMERON PARK CA         `     $   26.96
    ##  47 2018-07-28 JACK IN THE BOX 3439 EL DORADO HILCA     `     $   14.12
    ##  48 2018-07-24 CARL'S JR 7058 CAMERON PARK CA           `     $    9.64
    ##  49 2018-07-24 QUE VIVA MEXICAN RESTAURACAMERON PARK CA `     $   28.02
    ##  50 2018-07-19 CARL'S JR 7058 CAMERON PARK CA           `     $   12.42
    ##  51 2018-07-14 CARL'S JR 7058 CAMERON PARK CA           `     $    6.44
    ##  52 2018-07-11 CARL'S JR 7058 CAMERON PARK CA           `     $    6.44
    ##  53 2018-07-10 QUE VIVA MEXICAN RESTAURACAMERON PARK CA `     $   31.76
    ##  54 2018-07-10 CARL'S JR 7058 CAMERON PARK CA           `     $    6.44
    ##  55 2018-07-06 SUBWAY 00468983 LOST HILLS CA            `     $   12.96
    ##  56 2018-07-05 CARL'S JR 7058 CAMERON PARK CA           `     $    6.44
    ##  57 2018-06-29 ADALBERTO'S MEXICAN FOOD SACRAMENTO CA   `     $   13.10
    ##  58 2018-06-25 BURGER KING #18516 WINTERS CA            `     $    7.81
    ##  59 2018-06-21 CARL'S JR 7058 CAMERON PARK CA           `     $    6.44
    ##  60 2018-06-20 CARL'S JR 7058 CAMERON PARK CA           `     $    6.44
    ##  61 2018-06-13 APPLEBEES CAME18218255 CAMERON PARK CA   `     $   31.65
    ##  62 2018-06-08 JACK IN THE BOX 3435 ROCKLIN CA          `     $   10.06
    ##  63 2018-06-08 THE INDEPENDENT RESTAU PLACERVILLE CA    `     $  207.12
    ##  64 2018-05-25 WALLYS PIZZA BAR CAMERON PARK CA         `     $   48.20
    ##  65 2018-05-22 CARL'S JR 7058 CAMERON PARK CA           `     $    6.44
    ##  66 2018-05-03 CARL'S JR 7058 CAMERON PARK CA           `     $    6.44
    ##  67 2018-04-27 DENNY'S #7661 18007336 CAMERON PARK CA   `     $   22.05
    ##  68 2018-04-24 CARL'S JR 7058 CAMERON PARK CA           `     $    6.44
    ##  69 2018-04-23 CARL'S JR 7058 CAMERON PARK CA           `     $    6.44
    ##  70 2018-04-14 CARL'S JR 7058 CAMERON PARK CA           `     $    8.76
    ##  71 2018-04-13 CARL'S JR 7058 CAMERON PARK CA           `     $    7.51
    ##  72 2018-04-12 CARL'S JR 7058 CAMERON PARK CA           `     $   18.13
    ##  73 2018-04-11 SUBWAY 00034744 CAMERON PARK CA          `     $   15.99
    ##  74 2018-04-04 CARL'S JR 7058 CAMERON PARK CA           `     $    4.49
    ##  75 2018-03-31 CARL'S JR 7058 CAMERON PARK CA           `     $    6.44
    ##  76 2018-03-29 CARL'S JR 7058 CAMERON PARK CA           `     $    6.44
    ##  77 2018-03-24 CARL'S JR 7058 CAMERON PARK CA           `     $    6.44
    ##  78 2018-03-21 SUBWAY 00034744 CAMERON PARK CA          `     $   15.00
    ##  79 2018-03-06 SUBWAY 00034744 CAMERON PARK CA          `     $   15.43
    ##  80 2018-03-02 CARL'S JR 7058 CAMERON PARK CA           `     $   15.23
    ##  81 2018-02-27 PANERA BREAD #202210 FOLSOM CA           `     $   16.90
    ##  82 2018-02-27 SUBWAY 00034744 CAMERON PARK CA          `     $   15.43
    ##  83 2018-02-24 CARL'S JR 7058 CAMERON PARK CA           `     $   22.49
    ##  84 2018-02-22 CARL'S JR 7058 CAMERON PARK CA           `     $    6.95
    ##  85 2018-02-19 CARL'S JR 7058 CAMERON PARK CA           `     $    5.78
    ##  86 2018-02-07 CARL'S JR 7058 CAMERON PARK CA           `     $   12.52
    ##  87 2018-02-05 CARL'S JR 7058 CAMERON PARK CA           `     $   11.75
    ##  88 2018-02-02 SUBWAY 00034744 CAMERON PARK CA          `     $   17.99
    ##  89 2018-01-30 JACK IN THE BOX 3442 FOLSOM CA           `     $   13.66
    ##  90 2018-01-29 CARL'S JR 7058 CAMERON PARK CA           `     $    7.27
    ##  91 2018-01-27 SUBWAY 00034744 CAMERON PARK CA          `     $   15.49
    ##  92 2018-01-25 CARL'S JR 7058 CAMERON PARK CA           `     $   16.29
    ##  93 2018-01-24 PANERA BREAD #202217 RANCHO CORDOVCA     `     $   28.86
    ##  94 2018-01-24 QUE VIVA MEXICAN RESTAURACAMERON PARK CA `     $   40.06
    ##  95 2018-01-23 PIZZA BENE PLACERVILLE CA                `     $  169.10
    ##  96 2018-01-20 VELVET GRILL & CREAMERY GALT CA          `     $   40.52
    ##  97 2018-01-19 CARL'S JR 7058 CAMERON PARK CA           `     $    7.30
    ##  98 2018-01-17 SUBWAY 00034744 CAMERON PARK CA          `     $   16.50
    ##  99 2018-01-09 CARL'S JR 7058 CAMERON PARK CA           `     $   14.36
    ## 100 2018-01-08 CARL'S JR 7058 CAMERON PARK CA           `     $   18.13
    ## 101 2018-01-05 SUBWAY 00034744 CAMERON PARK CA          `     $   15.49
    ## 102 2018-01-04 CARL'S JR 7058 CAMERON PARK CA           `     $   16.29

    print(paste("Total Business Meals = $",sum(d3$Amount),sep = ""))

    ## [1] "Total Business Meals = $2196.14"

### Business phone/internet

    dd5<-filter(d1,Category=="bus phone/internet")
    dd5<-select(dd5,Date,Original.Description,Amount)
    dd55<-dd5
    dd55<-mutate(dd55,z="`")
    dd55<-select(dd55,Date,Original.Description,z,Amount)
    dd55$Amount<- sprintf("$ %7.2f", dd55$Amount)
    print(dd55,n=nrow(dd55))

    ## # A tibble: 26 x 4
    ##    Date       Original.Description                            z     Amount 
    ##    <date>     <fct>                                           <chr> <chr>  
    ##  1 2018-12-21 AT&T Mobility Bill Payment                      `     $  197~
    ##  2 2018-12-19 T-MOBILE STORE # 933C FOLSOM CA                 `     $   80~
    ##  3 2018-12-19 T-MOBILE STORE # 933C FOLSOM CA                 `     $   20~
    ##  4 2018-12-19 T-MOBILE STORE # 933C FOLSOM CA                 `     $  144~
    ##  5 2018-12-10 ATT DES:Payment ID:XXXXX6011EPAYP INDN:sherry ~ `     $   92~
    ##  6 2018-11-30 AT&T Mobility Bill Payment                      `     $  197~
    ##  7 2018-11-09 ATT DES:Payment ID:XXXXX7011EPAYL INDN:sherry ~ `     $   92~
    ##  8 2018-10-30 AT&T MOBILITY Bill Payment                      `     $  192~
    ##  9 2018-10-10 ATT DES:Payment ID:XXXXX9012EPAYH INDN:sherry ~ `     $   91~
    ## 10 2018-09-21 AT&T Mobility Bill Payment                      `     $  191~
    ## 11 2018-09-10 ATT DES:Payment ID:XXXXX7011EPAYC INDN:sherry ~ `     $   91~
    ## 12 2018-08-20 AT&T MOBILITY Bill Payment                      `     $  197~
    ## 13 2018-08-08 ATT DES:Payment ID:XXXXX8011EPAYW INDN:sherry ~ `     $   91~
    ## 14 2018-07-31 AT&T MOBILITY Bill Payment                      `     $  191~
    ## 15 2018-07-10 ATT DES:Payment ID:XXXXX6011EPAYS INDN:sherry ~ `     $   88~
    ## 16 2018-06-19 AT&T MOBILITY Bill Payment                      `     $  190~
    ## 17 2018-06-07 ATT DES:Payment ID:XXXXX2001EPAYG INDN:sherry ~ `     $   88~
    ## 18 2018-04-26 AT&T MOBILITY Bill Payment                      `     $  195~
    ## 19 2018-04-09 ATT DES:Payment ID:XXXXX4011EPAYE INDN:sherry ~ `     $   89~
    ## 20 2018-03-27 AT&T MOBILITY Bill Payment                      `     $  194~
    ## 21 2018-03-13 AT&T MOBILITY Bill Payment                      `     $  194~
    ## 22 2018-03-12 ATT DES:Payment ID:XXXXX3011EPAYB INDN:sherry ~ `     $   88~
    ## 23 2018-02-07 ATT DES:Payment ID:XXXXX8011EPAYW INDN:sherry ~ `     $   86~
    ## 24 2018-01-30 AT&T MOBILITY Bill Payment                      `     $  194~
    ## 25 2018-01-08 ATT DES:Payment ID:XXXXX3011EPAYR INDN:sherry ~ `     $   86~
    ## 26 2018-01-03 AT&T Mobility Bill Payment                      `     $  194~

    print(paste("Total paid to Business Phone/Internet = $",sum(dd5$Amount),sep = ""))

    ## [1] "Total paid to Business Phone/Internet = $3565.52"

### Business Travel

    dd6<-filter(d1,Category=="Business Travel")
    dd6<-select(dd6,Date,Original.Description,Amount)
    dd66<-dd6
    dd66<-mutate(dd66,z="`")
    dd66<-select(dd66,Date,Original.Description,z,Amount)
    dd66$Amount<- sprintf("$ %7.2f", dd66$Amount)
    print(dd66,n=nrow(dd66))

    ## # A tibble: 7 x 4
    ##   Date       Original.Description                   z     Amount   
    ##   <date>     <fct>                                  <chr> <chr>    
    ## 1 2018-11-03 HYATT REGENCY SAN FRAN A BURLINGAME CA `     $  721.39
    ## 2 2018-11-02 HYATT REGENCY SF ARP F&B BURLINGAME CA `     $   81.68
    ## 3 2018-09-05 PMT*SAC CO AIRPORT PARKNGSACRAMENTO CA `     $   60.00
    ## 4 2018-09-04 DELTA AIR 0062160434458DALLAS TX       `     $  202.00
    ## 5 2018-07-27 DELTA AIR 0067167625947BELLEVUE WA     `     $  240.40
    ## 6 2018-04-07 PMT*SAC CO AIRPORT PARKNGSACRAMENTO CA `     $   30.00
    ## 7 2018-03-14 PMT*SAC CO AIRPORT PARKNGSACRAMENTO CA `     $   50.00

    print(paste("Total paid to Business Travel = $",sum(dd6$Amount),sep = ""))

    ## [1] "Total paid to Business Travel = $1385.47"

### Office Supplies - note that $1350 of the total below was for liability insurance.

    d8<-filter(d1,Category=="Office Supplies")
    d8<-select(d8,Date,Original.Description,Amount)
    d88<-d8
    d88<-mutate(d88,z="`")
    d88<-select(d88,Date,Original.Description,z,Amount)
    d88$Amount<- sprintf("$ %7.2f", d88$Amount)
    print(d88,n=nrow(d88))

    ## # A tibble: 37 x 4
    ##    Date       Original.Description                 z     Amount   
    ##    <date>     <fct>                                <chr> <chr>    
    ##  1 2018-12-28 BEST BUY 00003491 ELK GROVE CA       `     $   26.92
    ##  2 2018-12-12 A LA MODE, INC 800-252-6633 FL       `     $  632.83
    ##  3 2018-12-10 CARTRIDGE WORLD #0796 916-9397384 CA `     $  134.34
    ##  4 2018-10-27 SAMSCLUB #6620 FOLSOM CA             `     $   44.28
    ##  5 2018-10-22 SAMSCLUB #6620 FOLSOM CA             `     $   38.88
    ##  6 2018-10-20 SAMSCLUB #6620 FOLSOM CA             `     $   70.80
    ##  7 2018-10-20 SAMS CLUB #6620 FOLSOM CA            `     $  119.07
    ##  8 2018-10-20 SAMS CLUB #6620 FOLSOM CA            `     $   11.01
    ##  9 2018-10-04 SAMSCLUB #6620 FOLSOM CA             `     $  152.41
    ## 10 2018-09-15 SAMS CLUB #6620 FOLSOM CA            `     $  109.11
    ## 11 2018-06-23 SAMSCLUB #6620 FOLSOM CA             `     $   32.30
    ## 12 2018-06-23 SAMS CLUB #6620 FOLSOM CA            `     $   14.52
    ## 13 2018-06-22 SAMSCLUB #6620 FOLSOM CA             `     $  101.11
    ## 14 2018-06-12 SAMSCLUB #6620 FOLSOM CA             `     $   87.72
    ## 15 2018-06-12 SAMSCLUB #6620 FOLSOM CA             `     $   69.66
    ## 16 2018-05-30 SAMS CLUB #6620 FOLSOM CA            `     $  177.75
    ## 17 2018-05-26 SAMSCLUB #6620 FOLSOM CA             `     $   21.54
    ## 18 2018-05-26 SAMSCLUB #6620 FOLSOM CA             `     $  162.49
    ## 19 2018-04-27 CARTRIDGE WORLD # EL DORADO HILCA    `     $  223.90
    ## 20 2018-04-03 SAMSCLUB #6620 FOLSOM CA             `     $   12.91
    ## 21 2018-03-22 SAMS CLUB #6620 FOLSOM CA            `     $    4.45
    ## 22 2018-03-14 SAMS CLUB #6620 FOLSOM CA            `     $   27.40
    ## 23 2018-03-07 SAMS CLUB #6620 FOLSOM CA            `     $  103.89
    ## 24 2018-02-27 SAMSCLUB #6620 FOLSOM CA             `     $   58.35
    ## 25 2018-02-20 AMAZON MKTPLACE PMTS AMZN.COM/BILLWA `     $   27.76
    ## 26 2018-02-14 SAMSCLUB #6620 FOLSOM CA             `     $   35.43
    ## 27 2018-02-14 SAMS CLUB #6620 FOLSOM CA            `     $   41.41
    ## 28 2018-02-10 SAMSCLUB #6620 FOLSOM CA             `     $   59.80
    ## 29 2018-02-09 SAMS CLUB #6620 FOLSOM CA            `     $    3.96
    ## 30 2018-02-09 SAMSCLUB #6620 FOLSOM CA             `     $   45.66
    ## 31 2018-01-23 RSTUDIO, INC. RSTUDIO.COM MA         `     $    9.00
    ## 32 2018-01-22 AMAZON MKTPLACE PMTS AMZN.COM/BILLWA `     $    9.99
    ## 33 2018-01-12 SAMS CLUB #6620 FOLSOM CA            `     $   56.19
    ## 34 2018-01-02 SAMSCLUB #6620 FOLSOM CA             `     $   61.85
    ## 35 2018-01-02 SAMSCLUB #6620 FOLSOM CA             `     $  144.94
    ## 36 2018-01-02 SAMS CLUB #6620 FOLSOM CA            `     $   31.06
    ## 37 2018-01-02 SAMSCLUB #6620 FOLSOM CA             `     $    5.75

    print(paste("Total paid to Office Supplies = $",sum(d8$Amount),sep = ""))

    ## [1] "Total paid to Office Supplies = $2970.44"

### Charity

    d6<-filter(d1,Category=="Charity")
    d6<-select(d6,Date,Description,Amount)
    d66<-d6
    d66<-mutate(d66,z="`")
    d66<-select(d66,Date,Description,z,Amount)
    d66$Amount<- sprintf("$ %9.2f", d66$Amount)
    print(d66,n=nrow(d66))

    ## # A tibble: 1 x 4
    ##   Date       Description z     Amount     
    ##   <date>     <fct>       <chr> <chr>      
    ## 1 2018-12-07 LDS Church  `     $   8500.00

    print(paste("Total paid to Charity = $",sum(d6$Amount),sep = ""))

    ## [1] "Total paid to Charity = $8500"

### Doctor&Dentist

    d7<-filter(d1,Category=="Doctor" | Category=="Dentist")
    d7<-select(d7,Date,Category,Description,Amount)
    d77<-d7
    d77<-mutate(d77,z="`")
    d77<-select(d77,Date,Category,Description,z,Amount)
    d77$Amount<- sprintf("$ %7.2f", d77$Amount)
    print(d77,n=nrow(d77))

    ## # A tibble: 0 x 5
    ## # ... with 5 variables: Date <date>, Category <fct>, Description <fct>,
    ## #   z <chr>, Amount <chr>

    print(paste("Total paid to Doctor and Dentist = $",sum(d7$Amount),sep = ""))

    ## [1] "Total paid to Doctor and Dentist = $0"

### Driver Cost

    d8<-filter(d1,Category=="Driver cost")
    d8<-select(d8,Date,Original.Description,Amount)
    d88<-d8
    d88<-mutate(d88,z="`")
    d88<-select(d88,Date,Original.Description,z,Amount)
    d88$Amount<- sprintf("$ %7.2f", d88$Amount)
    print(d88,n=nrow(d88))

    ## # A tibble: 1 x 4
    ##   Date       Original.Description z     Amount   
    ##   <date>     <fct>                <chr> <chr>    
    ## 1 2018-01-05 Check 1584           `     $ 1000.00

    print(paste("Total paid to Driver costs = $",sum(d8$Amount),sep = ""))

    ## [1] "Total paid to Driver costs = $1000"

### Estimated Federal & State tax payments

    dd7<-filter(d1,Category=="Federal Tax" | Category=="State Tax")
    dd7<-select(dd7,Date,Category,Original.Description,Amount,Notes)
    dd77<-dd7
    dd77<-mutate(dd77,z="`")
    dd77<-select(dd77,Date,Category,Original.Description,z,Amount,Notes)
    dd77$Amount<- sprintf("$ %7.2f", dd77$Amount)
    print(dd77,n=nrow(dd77))

    ## # A tibble: 5 x 6
    ##   Date       Category  Original.Description              z     Amount Notes
    ##   <date>     <fct>     <fct>                             <chr> <chr>  <fct>
    ## 1 2018-09-18 Federal ~ Check 1603                        `     $ 250~ ""   
    ## 2 2018-06-20 Federal ~ Check 1598                        `     $ 250~ ""   
    ## 3 2018-04-19 State Tax FRANCHISE TAX BD DES:CASTTAXRFD ~ `     $  66~ ""   
    ## 4 2018-04-17 Federal ~ Check 1594                        `     $ 282~ ""   
    ## 5 2018-04-17 Federal ~ Check 1595                        `     $ 200~ ""

### Property and other taxes

    d8<-filter(d1,Category=="Taxes")
    d8<-select(d8,Date,Original.Description,Amount)
    d88<-d8
    d88<-mutate(d88,z="`")
    d88<-select(d88,Date,Original.Description,z,Amount)
    d88$Amount<- sprintf("$ %7.2f", d88$Amount)
    print(d88,n=nrow(d88))

    ## # A tibble: 0 x 4
    ## # ... with 4 variables: Date <date>, Original.Description <fct>, z <chr>,
    ## #   Amount <chr>

    print(paste("Total paid to property&other TAXES = $",sum(d8$Amount),sep = ""))

    ## [1] "Total paid to property&other TAXES = $0"

### Business Services,Electronics & Software,Legal,Printing,Tuition

    d7<-filter(d1,Category=="Business Services" | Category=="Electronics & Software"| Category=="Legal"| Category=="Printing"| Category==" Tuition")
    d7<-select(d7,Date,Category,Description,Amount)
    d77<-d7
    d77<-mutate(d77,z="`")
    d77<-select(d77,Date,Category,Description,Amount)
    d77$Amount<- sprintf("$ %7.2f", d77$Amount)
    print(d77,n=nrow(d77))

    ## # A tibble: 58 x 4
    ##    Date       Category          Description                Amount   
    ##    <date>     <fct>             <fct>                      <chr>    
    ##  1 2018-12-23 Business Services Rstudio Rstudio.com Ma     $    9.00
    ##  2 2018-12-22 Business Services Columbia Candy Kitchen Inc $   77.45
    ##  3 2018-12-19 Business Services Appraisal Scope Ok         $   21.00
    ##  4 2018-12-12 Business Services Appraisal Scope Ok         $   21.00
    ##  5 2018-12-07 Business Services Metrolist                  $  144.17
    ##  6 2018-12-01 Business Services Gsuite Hulbert Cc          $   20.00
    ##  7 2018-11-23 Business Services Rstudio Rstudio.com Ma     $    9.00
    ##  8 2018-11-05 Business Services Silicondust USA Inc        $   35.00
    ##  9 2018-11-02 Business Services Gsuite Hulbert Cc          $   20.00
    ## 10 2018-10-31 Legal             Udemy                      $   17.00
    ## 11 2018-10-23 Business Services Rstudio Rstudio.com Ma     $    9.00
    ## 12 2018-10-12 Legal             Udemy                      $   19.98
    ## 13 2018-10-09 Business Services Assurant Appraisals Lx     $    6.40
    ## 14 2018-10-07 Business Services Dropbox                    $   99.00
    ## 15 2018-10-02 Business Services Assurant Appraisals Lx     $    6.40
    ## 16 2018-10-01 Business Services Gsuite Hulbert Cc          $   20.00
    ## 17 2018-09-25 Legal             Udemy                      $   13.99
    ## 18 2018-09-23 Business Services Rstudio Rstudio.com Ma     $    9.00
    ## 19 2018-09-18 Business Services Assurant Appraisals Lx     $   12.80
    ## 20 2018-09-14 Business Services Metrolist                  $  144.17
    ## 21 2018-09-11 Business Services Assurant Appraisals Lx     $    6.40
    ## 22 2018-09-04 Business Services Assurant Appraisals Lx     $    6.40
    ## 23 2018-09-01 Business Services Gsuite Hulbert Cc          $   20.00
    ## 24 2018-08-23 Business Services Rstudio Rstudio.com Ma     $    9.00
    ## 25 2018-08-21 Business Services Assurant Appraisals Lx     $    6.40
    ## 26 2018-08-18 Business Services Appraisal Scope Ok         $   21.00
    ## 27 2018-08-18 Business Services Appraisal Scope Ok         $   21.00
    ## 28 2018-08-16 Business Services Appraisal Scope Ok         $   21.00
    ## 29 2018-08-02 Business Services Appraisal Scope Ok         $   21.00
    ## 30 2018-08-02 Business Services Appraisal Scope Ok         $   21.00
    ## 31 2018-08-01 Business Services Google                     $   20.00
    ## 32 2018-07-23 Business Services Rstudio Rstudio.com Ma     $    9.00
    ## 33 2018-07-10 Business Services Appraisal Scope Ok         $   21.00
    ## 34 2018-07-06 Business Services Appraisal Scope Ok         $   21.00
    ## 35 2018-06-23 Business Services Rstudio Rstudio.com Ma     $    9.00
    ## 36 2018-06-14 Business Services Appraisal Scope Ok         $   21.00
    ## 37 2018-06-13 Business Services Metrolist                  $  144.17
    ## 38 2018-06-07 Business Services Cityofsac Parknggarage     $    4.50
    ## 39 2018-06-07 Business Services Cityofsac Ips Pkgmeter     $    5.25
    ## 40 2018-06-07 Business Services Appraisal Scope Ok         $    6.00
    ## 41 2018-06-04 Business Services Appraisal Scope Ok         $   21.00
    ## 42 2018-05-23 Business Services Rstudio Rstudio.com Ma     $    9.00
    ## 43 2018-05-04 Business Services Appraisal Scope Ok         $   21.00
    ## 44 2018-04-27 Legal             Bureau Real Estate         $  640.00
    ## 45 2018-04-24 Business Services Appraisal Scope Ok         $   21.00
    ## 46 2018-04-23 Business Services Rstudio Rstudio.com Ma     $    9.00
    ## 47 2018-04-19 Business Services Appraisal Scope Ok         $   21.00
    ## 48 2018-04-17 Business Services Coursera                   $   49.00
    ## 49 2018-04-12 Legal             Intuit TurboTax            $   84.98
    ## 50 2018-04-10 Business Services Coursera                   $   49.00
    ## 51 2018-04-06 Business Services Mercury                    $   14.50
    ## 52 2018-03-23 Business Services Rstudio Rstudio.com Ma     $    9.00
    ## 53 2018-03-17 Business Services Coursera                   $   49.00
    ## 54 2018-03-15 Business Services Metrolist                  $  144.17
    ## 55 2018-02-23 Business Services Rstudio Rstudio.com Ma     $    9.00
    ## 56 2018-02-17 Business Services Coursera                   $   49.00
    ## 57 2018-01-17 Legal             Coursera                   $   49.00
    ## 58 2018-01-16 Business Services Assurant Appraisals Lx     $    6.40

    print(paste("Total paid to Various Business expenses = $",sum(d7$Amount),sep = ""))

    ## [1] "Total paid to Various Business expenses = $2383.53"
