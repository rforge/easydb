qStr <- sprintf("DAT_TIM_SEC < %d", as.POSIXct("2011-12-31"))
myDb[ "MISCFORMAT", sRow = list( "SQL" = qStr ) ] 

qStr <- sprintf("DAT_TIM_SEC >= %d", as.POSIXct("2011-12-31"))
myDb[ "MISCFORMAT", sRow = list( "SQL" = qStr ), formatCol = list( 
    "DAT_TIM_SEC" = formatDT, "DAT_DAY" = formatD, 
    "TEST_BOOL" = as.logical ) ] 

