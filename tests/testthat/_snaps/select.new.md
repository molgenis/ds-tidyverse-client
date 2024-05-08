# Work out what is happening 

    Code
      tryCatch(ds.colnames("mtcars"), error = function(cond) {
        print(datashield.errors())
      })
    Output
      $sim1
       [1] "mpg"  "cyl"  "disp" "hp"   "drat" "wt"   "qsec" "vs"   "am"   "gear"
      [11] "carb"
      
      $sim2
       [1] "mpg"  "cyl"  "disp" "hp"   "drat" "wt"   "qsec" "vs"   "am"   "gear"
      [11] "carb"
      
      $sim3
       [1] "mpg"  "cyl"  "disp" "hp"   "drat" "wt"   "qsec" "vs"   "am"   "gear"
      [11] "carb"
      

