##### Script with password
options(java.parameters = "-Xmx18048m")
  start.time <- Sys.time()
# Set-up database connection
  # http://wush.ghost.io/rjdbc-sqlserver/
  # http://www.oracle.com/technetwork/java/javase/downloads/jdk8-downloads-2133151.html
  # install.packages(c('rJava', 'RJDBC'), type = 'source')  
  # http://stackoverflow.com/questions/30738974/rjava-load-error-in-rstudio-r-after-upgrading-to-osx-yosemite
  library(DBI)
  library(rJava)
  library(RJDBC)
  library(lubridate)
  #library(xlsxjars)
  #library(xlsx)
  #library(openxlsx)
  
  setwd("~/")
  getwd();wd <- getwd();print(paste0("Current working dir: ", wd))
  
  # 2 day off problem, see below links
  # https://blogs.msdn.microsoft.com/jdbcteam/2012/01/20/hotfix-available-for-date-issue-when-using-jre-1-7/
  # http://stackoverflow.com/questions/11296606/dates-consistently-two-days-off
  findDriver <- paste(wd,"/Library/Mobile Documents/com~apple~CloudDocs/GitHub/R/JDBC-Driver/sqljdbc_6.0/enu/jre8/sqljdbc42.jar", sep = "")
  
  #grep("Google Drive",wd,perl = TRUE, value = FALSE)
  
  #findDriver <- "~/Desktop/sqljdbc_6.0/enu/jre8/sqljdbc42.jar"
  print(findDriver)
  # https://msdn.microsoft.com/en-us/library/ms378428(v=sql.105).aspx
  driver <- JDBC("com.microsoft.sqlserver.jdbc.SQLServerDriver", findDriver)
               # "~/Desktop/sqljdbc4.jar")
               # "/Google Drive/AFT/R/JDBC-Driver/sqljdbc_3.0/cht/sqljdbc4.jar") 
  # Open debug mode
  #.jclassLoader()$setDebug(1L)
  #J("java.lang.System")$getProperty("java.version")
  ###################################################################################################################################
  # If counter Java error like this, JavaVM: requested Java version ((null)) not available. Using Java at "" instead, go to this link 
  # http://stackoverflow.com/questions/35179151/cannot-load-r-xlsx-package-on-mac-os-10-11
  # http://www.oracle.com/technetwork/java/javase/downloads/jre8-downloads-2133155.html Download Java runtime SE
  # https://oliverdowling.com.au/2015/10/09/oracles-jre-8-on-mac-os-x-el-capitan/ this works
  
  ###################################################################################################################################
  
  # https://www.r-bloggers.com/connecting-to-sql-server-from-r-using-rjdbc/
  # http://quantlego.com/howto/import-data-from-sql-server-into-r-rjdbc/
  con <- RJDBC::dbConnect(driver, 
                          paste0(
                          "jdbc:sqlserver://10.250.4.224;database=UgsReport;"), 
                          "dauser", 
                          "p@ss123$$",
                          "ApplicationIntent=ReadOnly")
  #con <- RJDBC::dbConnect(driver,paste0("jdbc:sqlserver://10.250.4.104;user=seanhuang;password=p@ss123$$;database=UgsReport;"))
  
  # Check database 
    dbListTables(con)
  # Find tables with Dail as the Start
    dbListTables(con, "Dail%")
    dbListFields(con,"DailyBrandCurrencyPlayerAggregates")
  # dbListTables(con, "t%")
  # Select 10 rows
  # a <- data.frame()
  # a <- dbGetQuery(con, "select top 10 * from DailyBrandCurrencyPlayerAggregates")

### TSQL Queries(MS SQL Server)

  # Auto
    #curentDay  <- day(Sys.Date())
    #lastWeek    <- curentDay - 7

  # Customized Export File Name
  # Manuel
    yearVar    <- 2017
    monthVar   <- 9
    startVar   <- 08
    endVar     <- 14
    folder     <- "Week1"
    tempFile   <- "~/Desktop/tem.xlsx"
    pathSunbet <- "/Sunbet-csv-"
    pathGD     <- "/GD-"
    pathGG     <- "/GG-"
    pathLax    <- "/Lax-"
    pathRT     <- "/RT-"
    pathEA     <- "/EA-"
    customizedDate <- function(month,start,end){
      for(i in c(1:9)) {
        if (monthVar == i) {
          assign(paste("monthVar"),
                 paste("0",monthVar,sep = ""),
                 envir = parent.env(environment())
          )
          break
        }
        else {
          assign(paste("monthVar"),paste(monthVar))
        }
      }
      for(i in c(1:9)) {
        if(startVar == i){
          assign(paste("startVar"),
                 paste("0",startVar,sep = ""),
                 envir = parent.env(environment())
          )
          break
        }
        else {
          assign(paste("startVar"),paste(startVar))
        }
      }
      for(i in c(1:9)) {
        if(endVar == i){
          assign(paste("endVar"),
                 paste("0",endVar,sep = ""),
                 envir = parent.env(environment())
          )
          break
        }
        else {
          assign(paste("endVar"),paste(endVar))
        }
      }
    }
    customizedDate(month,start,end)
    customizedExportFileName <- function(month,start,end){
      
      for(i in c(1:9)) {
        if (month == i) {
          assign(paste("outcome1"),paste("0",monthVar,sep = ""))
          break
        }
        else {
          assign(paste("outcome1"),paste(monthVar))
        }
      }
      for(i in c(1:9)) {
        if(start == i){
          assign(paste("outcome2"),paste("0",start,sep = ""))
          break
        }
        else {
          assign(paste("outcome2"),paste(start))
        }
      }
      for(i in c(1:9)) {
        if(end == i){
          assign(paste("outcome3"),paste("0",end,sep = ""))
          break
        }
        else {
          assign(paste("outcome3"),paste(end))
        }
      }
      print(#assign(
        #paste("exportFileName"),
        paste("~/Desktop/UGS-",outcome1,outcome2,"-",outcome1,outcome3,".xlsx", sep = "")#,
        #envir = parent.env(environment())
      )#)
    }
    exportFile <- customizedExportFileName(monthVar,startVar,endVar)
                  # Result will be something like this
                  # "~/Desktop/UGS-0201-0207.xlsx"
                  # paste0("~/Desktop/","0",monthVar,startVar,"-","0",monthVar,endVar,".xlsx",sep="")
    
  
    queryOut <- function() {
      assign(
        paste("command.daily"),
        # Original: data-accuracy-report-player-game.sql
        paste0(
          "
          DECLARE @year                   INT = ", yearVar,"
          DECLARE @month                  INT = ", monthVar,"
          DECLARE @startday               INT = ", startVar,"
          DECLARE @endday                 INT = ", endVar,"
          DECLARE @Brand                  NVARCHAR (64) 
          DECLARE @username               NVARCHAR (64)
          DECLARE @GameProvider           NVARCHAR (64)
          DECLARE @GameName               NVARCHAR (64) 
          
          SELECT
            'Day'                           = d.RoundClosedDate,
            'Reseller'                      = d.ResellerCode,
            'Licensee'                      = d.LicenseeCode,
            'Brand'                         = d.BrandCode,
            'Currency'                      = d.CurrencyCode,
            'Game Provider'                 = d.GameProviderName,
            'Game'                          = d.GameName,
            'Player'                        = d.Username,
            'Player ID'                     = d.PlayerID,
            'Rounds'                        = SUM(d.Rounds),
            'Bet Amount'                    = -SUM(d.AdjustedRisk),
            'Turnover '                     = -SUM(d.Turnover),
            'Valid Bet'                     = -SUM(d.ValidBet),
            'Company Win Loss'              = -SUM(d.Hold),
            'Jackpot Contribution'          =  SUM(d.JackpotContribution),
            'Company Win Loss No Jp'        = (-SUM(d.Hold)) - SUM(d.JackpotContribution),  

            CASE
                WHEN SUM(Turnover) <> 0
                THEN (SUM(Hold) / SUM(Turnover))*100
                ELSE 0
                END AS 'Company Win/Loss %',

            CASE 
                WHEN d.platformtype = 0 THEN CAST('Desktop' AS NVARCHAR(64))
                WHEN d.platformtype = 1 THEN CAST('Mobile' AS NVARCHAR(64))
                ELSE CAST(d.PlatformType AS NVARCHAR(64))
                END AS 'Platform'
          
          FROM dbo.DailyBrandCurrencyPlayerAggregates d
          
          WHERE 1 = 1 
          
          AND (@year         IS NULL OR YEAR(d.RoundClosedDate) = @year)
          AND (@month        IS NULL OR MONTH(d.RoundClosedDate) = @month)
          AND (@startday     IS NULL OR DAY(d.RoundClosedDate) >= @startday)
          AND (@endday       IS NULL OR DAY(d.RoundClosedDate) <= @endday)
          AND (@Brand        IS NULL OR d.brandcode = @Brand)
          AND (@username     IS NULL OR d.Username = @username)
          AND (@GameProvider IS NULL OR d.GameProviderName = @GameProvider)
          AND (@GameName     IS NULL OR d.GameName = @GameName)
          AND d.playertype = '1'
          
          GROUP BY
            d.ResellerCode,
            d.RoundClosedDate,
            d.CurrencyCode,
            d.LicenseeCode, 
            d.BrandCode, 
            d.GameProvidername, 
            d.gamename, 
            d.username, 
            d.PlayerId, 
            d.platformtype
          
          ORDER BY  
            d.RoundClosedDate,
            d.CurrencyCode,
            d.LicenseeCode, 
            d.BrandCode, 
            d.PlatformType
          "
          ),
        envir = parent.env(environment())
      )
      assign(
        paste("command.monthly"),
        # Original: data-accuracy-report-player-game-aggregate.sql
        paste0(
          "
          DECLARE @year                   INT = ",yearVar,"
          DECLARE @month                  INT = ",monthVar,"	
          DECLARE @startday               INT = ",startVar,"
          DECLARE @endday                 INT = ",endVar,"
          DECLARE @Brand                  NVARCHAR (64)
          DECLARE @username               NVARCHAR (64)
          DECLARE @GameProvider           NVARCHAR (64)
          DECLARE @Game                   NVARCHAR (64)
          
          SELECT
          'Year'                          = YEAR(d.RoundClosedDate),
          'Month'                         = MONTH(d.RoundClosedDate),
          'Reseller'                      = d.ResellerCode,
          'Licensee'                      = d.LicenseeCode,
          'Brand'                         = d.BrandCode,
          'Currency'                      = d.CurrencyCode,
          'Game Provider'                 = d.GameProviderName,
          'Game'                          = d.GameName,
          'Player'                        = d.Username,
          'Player ID'                     = d.PlayerID,
          'Rounds'                        = SUM(d.Rounds),
          'Bet Amount'                    = -SUM(d.AdjustedRisk),
          'Turnover '                     = -SUM(d.Turnover),
          'Valid Bet'                     = -SUM(d.ValidBet),
          'Company Win/Loss'              = -SUM(d.Hold),
          'Jackpot Contribution'          =  SUM(d.JackpotContribution),
          'Company Win Loss No Jp'        = (-SUM(d.Hold)) - SUM(d.JackpotContribution),  

          CASE
              WHEN SUM(Turnover) <> 0
              THEN (SUM(Hold) / SUM(Turnover))*100
              ELSE 0
              END AS 'Company Win/Loss %',
          
          CASE 
              WHEN d.platformtype = 0 THEN CAST('Desktop' AS NVARCHAR(64))
              WHEN d.platformtype = 1 THEN CAST('Mobile' AS NVARCHAR(64))
              ELSE CAST(d.PlatformType AS NVARCHAR(64))
              END AS 'Platform'
          
          FROM dbo.DailyBrandCurrencyPlayerAggregates d
          
          WHERE 1 = 1 
          
          AND (@year         IS NULL OR YEAR(d.RoundClosedDate) = @year)
          AND (@month        IS NULL OR MONTH(d.RoundClosedDate) = @month)
          AND (@startday     IS NULL OR DAY(d.RoundClosedDate) >= @startday)
          AND (@endday       IS NULL OR DAY(d.RoundClosedDate) <= @endday)
          AND (@Brand        IS NULL OR d.BrandCode = @Brand)
          AND (@username     IS NULL OR d.Username = @username)
          AND (@GameProvider IS NULL OR d.GameProviderName = @GameProvider)
          AND (@Game         IS NULL OR d.GameName = @Game)
          AND d.PlayerType = '1'
          
          GROUP BY 
            YEAR(d.RoundClosedDate), 
            MONTH(d.RoundClosedDate),
            d.ResellerCode,
            d.LicenseeCode,
            d.BrandCode, 
            d.CurrencyCode, 
            d.username, 
            d.playerid, 
            d.gameproviderName, 
            d.gamename, 
            d.PlatformType 
          
          ORDER BY 
            d.LicenseeCode,
            d.BrandCode, 
            d.CurrencyCode, 
            d.username, 
            d.playerid, 
            d.gameproviderName, 
            d.gamename, 
            d.PlatformType
          "),
        envir = parent.env(environment())
      )
    }
    queryOut()
    
    # Query Data from database
      daily     <- dbGetQuery(con, command.daily)
      aggregate <- dbGetQuery(con, command.monthly)

### Save data into spreadsheets

  ## Write different data.frame into Multiple spreadsheets in a workbook  
       list_of_datasets <- list("DataSheet1.Daily" = daily, "DataSheet2.Aggre" = aggregate) 
       openxlsx::write.xlsx(list_of_datasets, file = tempFile)
      # write.csv is only for one sheet 
      # write.csv(daily, file = "test.csv", row.names=FALSE)
      # write.csv(daily, file= "~/Desktop/daily.csv", row.names=FALSE)
      # write.csv(aggregate, file="~/Desktop/aggregate.csv", row.names=FALSE )
      
  ## Import two sheets
      sheet1.daily     <- openxlsx::read.xlsx(tempFile, sheet="DataSheet1.Daily")
      sheet2.aggregate <- openxlsx::read.xlsx(tempFile, sheet = "DataSheet2.Aggre")
      #sheet1.daily <- read.csv(file="~/Desktop/daily.csv", header=TRUE, sep=",")
      #sheet2.aggregate <- read.csv(file="~/Desktop/aggregate.csv", header=TRUE, sep=",")
      
  ## Rename the column 
      dailyColNameOld <- colnames(sheet1.daily)
      aggreColNameOld <- colnames(sheet2.aggregate)
      
      colnames(sheet1.daily) <- 
                      c("Date","Reseller","Licensee","Brand","Currency","GameProvider","Game",
                        "Username","UserID","Rounds","BetAmount","Turnover","ValidBet","Winloss","JackpotContribution","Winloss No JP","Winloss %","Platform")
      colnames(sheet2.aggregate) <- 
                        c("Year","Month","Reseller","Licensee","Brand","Currency","GameProvider","Game",
                          "Username","UserID","Rounds","BetAmount","Turnover","ValidBet","Winloss","JackpotContribution","Winloss No JP","Winloss%","Platform")

      list_of_datasets <- list(
        "DataSheet1.Daily" = sheet1.daily, 
        "DataSheet2.Aggre" = sheet2.aggregate)

      # Export weekly and monthly data
      openxlsx::write.xlsx(list_of_datasets, file = exportFile)
      system("rm ~/Desktop/tem.xlsx")

        
## Data Comparison 
  curOrder <- c("RMB","THB","VND","IDR","MYR","EUR","GBP","USD")
  # UGS
    # Reading Data
    data.ugs       <- openxlsx::read.xlsx(exportFile, sheet = "DataSheet1.Daily")
    # Currency
    unique(data.ugs$Currency)
    cur.ugs  <- unique(data.ugs$Currency)[order(match(unique(data.ugs$Currency),curOrder))]
    # Game Provider
    ugs.GP   <- unique(data.ugs$GameProvider)
    # Counter
    counter = 0
    # Create empty dataframe
    result.ugs <- data.frame()
    # Function
    for(c in cur.ugs) {
      for(n in ugs.GP) {
        counter = counter + 1
        
        print(c)
        print(n)
        BetAmount <- 
          formattable::formattable(
            sum(
              data.ugs[data.ugs$Currency == c & data.ugs$GameProvider == n,]$BetAmount)
            , digits=2,format="f")
iris %>% filter(Species == 'virginica') %>%
        tmp_result.ugs <- list(c, n, data.ugs %>% filter(Currency == c, GameProvider == n) %>% select('BetAmount') %>% round(sum(.),2),
            data.ugs %>% filter(Currency == c, GameProvider == n) %>% select('Winloss') %>% round(sum(.),2))
        if(sum(tmp_result.ugs[,3:4]) > 0) {result.ugs <- rbind(result.ugs, result.ugs_a)}
          
        data.ugs %>% filter(Currency == c, GameProvider == n) %>% 
                              summarise()
        Winloss   <- formattable::formattable(sum(data.ugs[data.ugs$Currency == c & data.ugs$GameProvider == n,]$Winloss), digits=2,format="f")
        if (BetAmount != 0 & Winloss != 0) {
          result.ugs[counter,1] <- c
          result.ugs[counter,2] <- n
          result.ugs[counter,3] <- format(abs(BetAmount), digits =2)
          result.ugs[counter,4] <- format(abs(Winloss),  digits =2)
        }
      } 
    }
    ## Get Sum-up data
   
    colnames(result.ugs) <- c("Currency","Game Providers","UGS-BetAmount","UGS-Winloss")
    result.ugs <- na.omit(result.ugs)
    rownames(result.ugs) <- seq(1:length(rownames(result.ugs)))
    #ugs.B2C    <- as.data.frame(result.ugs[result.ugs$`Game Providers`=="Genesis",])
    #ugs.B2B    <- as.data.frame(result.ugs[result.ugs$`Game Providers`!="Genesis",]) 
  
  # Laxino
    glob.laxino    <- Sys.glob(paste("~/Desktop/",folder,pathLax,monthVar,startVar,"-",monthVar,endVar,".csv",sep = ""))
    if(length(glob.laxino) > 0) {
      # Reading Data  
      data.laxino    <- read.csv(file = glob.laxino, stringsAsFactors = FALSE)
      # take out comma, then change to number
      data.laxino$Bet_Amount    <- as.numeric(gsub(",","",data.laxino$Bet_Amount))
      data.laxino$Member_Result <- gsub("[\\(|]","-",data.laxino$Member_Result)
      data.laxino$Member_Result <- as.numeric(gsub("[\\)|,]","",data.laxino$Member_Result))
      
      # Currency
      unique(data.laxino$Currency)
      cur.laxino <- unique(data.laxino$Currency)[order(match(unique(data.laxino$Currency),curOrder))]
      # Counter
      counter = 0
      # Create empty dataframe 
      result.laxino  <- data.frame() 
      # Function
      for(c in cur.laxino) {
        counter = counter + 1
        print(c)
        # Bet_Amount includes test players
        BetAmount <- formattable::formattable(sum(data.laxino[data.laxino$Currency == c,]$Bet_Amount),digits=2,format="f")
        Winloss  <- formattable::formattable(sum(data.laxino[data.laxino$Currency == c,]$Member_Result),digits=2,format="f")
        
        if (BetAmount != 0 & Winloss != 0) {
          result.laxino[counter,1] <- c
          result.laxino[counter,2] <- format(abs(BetAmount), digits =2)
          result.laxino[counter,3] <- format(abs(Winloss), digits =2)
        }
        rowG <- as.numeric(rownames(result.ugs[result.ugs$`Game Providers` == "Laxino" & result.ugs$Currency == c,]))
        print(rowG)
        result.ugs[rowG,5] <- format(abs(BetAmount), digits =2)
        result.ugs[rowG,6] <- format(abs(Winloss), digits =2)
        result.ugs[rowG,7] <- formattable::formattable(as.numeric(result.ugs[rowG,3]),digits=2,format='f') - 
                              formattable::formattable(as.numeric(result.ugs[rowG,5]),digits=2,format='f')
        result.ugs[rowG,8] <- formattable::formattable(as.numeric(result.ugs[rowG,4]),digits=2,format='f') - 
                              formattable::formattable(as.numeric(result.ugs[rowG,6]),digits=2,format='f')
      }
      colnames(result.laxino) <- c("Currency","BetAmount","Winloss")
    } else {
      print("No file named laxino-")
    }
  # GoldDelux
    glob.goldDeluxe <- Sys.glob(paste("~/Desktop/",folder,pathGD,monthVar,startVar,"-",monthVar,endVar,".csv",sep = ""))
    if(length(glob.goldDeluxe) > 0) {
      # Reading Data
      data.goldDeluxe <- read.csv(file = glob.goldDeluxe, stringsAsFactors = FALSE)
      data.goldDeluxe[data.goldDeluxe$Currency == "CNY",]$Currency <- "RMB"
      # Currency
      unique(data.goldDeluxe$Currency)
      cur.goldDeluxe <- unique(data.goldDeluxe$Currency)[order(match(unique(data.goldDeluxe$Currency),curOrder))]
      # Counter
      counter = 0
      # Create empty dataframe 
      result.goldDeluxe  <- data.frame()
      # Function
      for(c in cur.goldDeluxe) {
        counter = counter + 1
        print(c)
        
        BetAmount <- formattable::formattable(sum(data.goldDeluxe[data.goldDeluxe$Currency == c,]$Bet.Amt),digits=2,format="f")
        Winloss   <- formattable::formattable(sum(data.goldDeluxe[data.goldDeluxe$Currency == c,]$Player.WinLoss),digits=2,format="f")
        
        if (BetAmount != 0 & Winloss != 0) {
          result.goldDeluxe[counter,1] <- c
          result.goldDeluxe[counter,2] <- format(abs(BetAmount), digits =2)
          result.goldDeluxe[counter,3] <- format(abs(Winloss), digits =2)
        }
        rowG <- as.numeric(rownames(result.ugs[result.ugs$`Game Providers` == "GoldDeluxe" & result.ugs$Currency == c,]))
        result.ugs[rowG,5] <- format(abs(BetAmount), digits =2)
        result.ugs[rowG,6] <- format(abs(Winloss), digits =2)
        result.ugs[rowG,7] <- formattable::formattable(as.numeric(result.ugs[rowG,3]),digits=2,format='f') - 
                              formattable::formattable(as.numeric(result.ugs[rowG,5]),digits=2,format='f')
        result.ugs[rowG,8] <- formattable::formattable(as.numeric(result.ugs[rowG,4]),digits=2,format='f') - 
                              formattable::formattable(as.numeric(result.ugs[rowG,6]),digits=2,format='f')
      }
      colnames(result.goldDeluxe) <- c("Currency","BetAmount","Winloss")
    } else {
      print("No file named goldDeluxe-")
    }    
  # GG
    glob.gg        <- Sys.glob(paste("~/Desktop/",folder,pathGG,monthVar,startVar,"-",monthVar,endVar,".csv",sep = ""))
    if (length(glob.gg) > 0) {
      # Reading Data  
      system(paste("cp ", glob.gg, " ~/Desktop",sep = ""))
      glob.gg.desk   <- Sys.glob(paste("~/Desktop",pathGG,monthVar,startVar,"-",monthVar,endVar,".csv",sep = ""))
      system(paste("sed -i '' 1d ", glob.gg.desk))
      
      data.gg        <- read.csv(file = glob.gg.desk, stringsAsFactors = FALSE)
      data.gg[data.gg$Currency == "CNY",]$Currency <- "RMB"
      # Currency
      unique(data.gg$Currency)
      cur.gg <- unique(data.gg$Currency)[order(match(unique(data.gg$Currency),curOrder))]
      # Counter
      counter = 0
      # Create empty dataframe 
      result.gg  <- data.frame()
      # Function
      for(c in cur.gg) {
        counter = counter + 1
        print(c)
        
        BetAmount <- formattable::formattable(sum(data.gg[data.gg$Currency == c,]$Bet.Amount),digits=2,format="f")
        Winloss   <- formattable::formattable(sum(data.gg[data.gg$Currency == c,]$Player.Win.Loss),digits=2,format="f")
        
        if (BetAmount != 0 & Winloss != 0) {
          result.gg[counter,1] <- c
          result.gg[counter,2] <- format(abs(BetAmount), digits =2)
          result.gg[counter,3] <- format(abs(Winloss), digits =2)
        }
        rowG <- as.numeric(rownames(result.ugs[result.ugs$`Game Providers` == "GG" & result.ugs$Currency == c,]))
        result.ugs[rowG,5] <- format(abs(BetAmount), digits =2)
        result.ugs[rowG,6] <- format(abs(Winloss), digits =2)
        result.ugs[rowG,7] <- formattable::formattable(as.numeric(result.ugs[rowG,3]),digits=2,format='f') - 
                              formattable::formattable(as.numeric(result.ugs[rowG,5]),digits=2,format='f')
        result.ugs[rowG,8] <- formattable::formattable(as.numeric(result.ugs[rowG,4]),digits=2,format='f') - 
                              formattable::formattable(as.numeric(result.ugs[rowG,6]),digits=2,format='f')
      }
      colnames(result.gg) <- c("Currency","BetAmount","Winloss")
      system(paste("rm ", glob.gg.desk))
    } else {
      print("No file named fishing-")  
    }
  # Red Tiger
    glob.RT        <- Sys.glob(paste("~/Desktop/",folder,pathRT,monthVar,startVar,"-",monthVar,endVar,".csv",sep = ""))
    if(length(glob.RT) > 0) {
      # Reading Data
      data.RT    <- read.csv(file = glob.RT, stringsAsFactors = FALSE)
      # Currency
      unique(data.RT$Currency)
      cur.RT <- unique(data.RT$Currency)[order(match(unique(data.RT$Currency),curOrder))]
      # Counter
      counter = 0
      # Create empty dataframe 
      result.RT  <- data.frame()
      # Function
      for(c in cur.RT) {
        counter = counter + 1
        print(c)
        
        BetAmount    <- formattable::formattable(sum(data.RT[data.RT$Currency == c,]$Turnover),digits=2,format="f")
        Winloss      <- formattable::formattable(sum(data.RT[data.RT$Currency == c,]$WinLoss),digits=2,format="f")
        JackpotWins  <- formattable::formattable(sum(data.RT[data.RT$Currency == c,]$JackpotWins),digits=2,format="f")
        
        if (BetAmount != 0 & Winloss != 0) {
          result.RT[counter,1] <- c
          result.RT[counter,2] <- format(abs(BetAmount), digits =2)
          result.RT[counter,3] <- format(abs(Winloss) + abs(JackpotWins), digits =2)
        }
        
        rowG <- as.numeric(rownames(result.ugs[result.ugs$`Game Providers` == "Red Tiger" & result.ugs$Currency == c,]))
        result.ugs[rowG,5] <- format(abs(BetAmount), digits =2)
        result.ugs[rowG,6] <- format(abs(Winloss) + abs(JackpotWins), digits =2) 
        result.ugs[rowG,7] <- formattable::formattable(as.numeric(result.ugs[rowG,3]),digits=2,format='f') - 
                              formattable::formattable(as.numeric(result.ugs[rowG,5]),digits=2,format='f')
        result.ugs[rowG,8] <- formattable::formattable(as.numeric(result.ugs[rowG,4]),digits=2,format='f') - 
                              formattable::formattable(as.numeric(result.ugs[rowG,6]),digits=2,format='f')
      }
      colnames(result.RT) <- c("Currency","Turnover","Winloss")
    } else {
      print("No file named RT-")
    }
  # EA 
    glob.ea        <- Sys.glob(paste("~/Desktop/",folder,pathEA,monthVar,startVar,"-",monthVar,endVar,".csv",sep = ""))
    if(length(glob.ea) > 0) {
      # Reading Data
      data.ea      <- read.csv(file = glob.ea, stringsAsFactors = FALSE)
      data.ea[data.ea$Currency == "CNY",]$Currency <- "RMB"
      # Currency
      unique(data.ea$Currency)
      cur.EA <- unique(data.ea$Currency)[order(match(unique(data.ea$Currency),curOrder))]  
      # Counter
      counter = 0
      # Create empty dataframe 
      result.EA  <- data.frame()
      # Function
      for(c in cur.EA) {
        counter = counter + 1
        print(c)
        
        Turnover   <- formattable::formattable(sum(as.numeric(gsub(",","",data.ea[data.ea$Currency == c,]$Bet.Amt.))),digits=2,format="f")
        Winloss    <- formattable::formattable(sum(as.numeric(gsub(",","",data.ea[data.ea$Currency == c,]$Player.Win.Loss))),digits=2,format="f")
        
        if (BetAmount != 0 & Winloss != 0) {
          result.EA[counter,1] <- c
          result.EA[counter,2] <- format(abs(BetAmount), digits =2)
          result.EA[counter,3] <- format(abs(Winloss), digits =2)
        }
        rowG <- as.numeric(rownames(result.ugs[result.ugs$`Game Providers` == "EA - N2Live" & result.ugs$Currency == c,]))
        result.ugs[rowG,5] <- format(abs(BetAmount), digits =2)
        result.ugs[rowG,6] <- format(abs(Winloss) , digits =2) 
        result.ugs[rowG,7] <- formattable::formattable(as.numeric(result.ugs[rowG,3]),digits=2,format='f') - 
                              formattable::formattable(as.numeric(result.ugs[rowG,5]),digits=2,format='f')
        result.ugs[rowG,8] <- formattable::formattable(as.numeric(result.ugs[rowG,4]),digits=2,format='f') - 
                              formattable::formattable(as.numeric(result.ugs[rowG,6]),digits=2,format='f')
      }
      colnames(result.EA) <- c("Currency","BetAmount","Winloss")
    } else {
      print("No file named EA-")
    }
  # Sunbet
    glob.sunbet    <- Sys.glob(paste("~/Desktop/",folder,pathSunbet,monthVar,startVar,"-",monthVar,endVar,".csv",sep = ""))
    if (length(glob.sunbet) > 0) {
      # Reading Data
        data.sunbet    <- read.csv(file = glob.sunbet,  encoding="UTF-8" ,stringsAsFactors = FALSE)
      # Currency
        unique(data.sunbet$Currency)
        cur.sunbet     <- unique(data.sunbet$Currency)[order(match(unique(data.sunbet$Currency),curOrder))]
      # Counter
        counter = 0
      # Create empty dataframe 
        result.sunbet  <- data.frame()
      # Function
        for(c in cur.sunbet) {
          counter = counter + 1
          print(c)
          BetAmount <- formattable::formattable(sum(as.numeric(gsub(",","",data.sunbet[data.sunbet$Currency == c,]$Bet.Amount))),digits=2,format="f")
          Winloss  <- formattable::formattable(sum(as.numeric(gsub(",","",data.sunbet[data.sunbet$Currency == c,]$WinLoss))),digits=2,format="f")
          
          if (BetAmount != 0 & Winloss != 0) {
            result.sunbet[counter,1] <- c
            result.sunbet[counter,2] <- format(abs(BetAmount), digits =2)
            result.sunbet[counter,3] <- format(abs(Winloss), digits =2)
          }
          rowG <- as.numeric(rownames(result.ugs[result.ugs$`Game Providers` == "Sunbet" & result.ugs$Currency == c,]))
          result.ugs[rowG,5] <- format(abs(BetAmount), digits =2)
          result.ugs[rowG,6] <- format(abs(Winloss), digits =2)
          result.ugs[rowG,7] <- formattable::formattable(as.numeric(result.ugs[rowG,3]),digits=2,format='f') - 
                                formattable::formattable(as.numeric(result.ugs[rowG,5]),digits=2,format='f')
          result.ugs[rowG,8] <- formattable::formattable(as.numeric(result.ugs[rowG,4]),digits=2,format='f') - 
                                formattable::formattable(as.numeric(result.ugs[rowG,6]),digits=2,format='f')
        }
        colnames(result.sunbet) <- c("Currency","BetAmount","Winloss")
    } else {
      print("No file named sunbet-new-")
    }
    
        
#setwd("~/Desktop/")
#write.csv(result.ugs, file = "MyData.csv", row.names = F)


end.time <- Sys.time()
time.taken <- end.time - start.time
time.taken