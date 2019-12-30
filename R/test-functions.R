# Testing age_cat_fun() below lower bound
# age_cat_fun(11)
# expected output: NA

# Testing age_cat_fun() within bounds
# age_cat_fun(33)
# expected output: 6

# Testing age_cat_fun() above bounds
# age_cat_fun(96)
# expected output: 16

# Testing bmi_fun() when one or more parameters is NA
# bmi_fun(NA, 50)
# expected output: NA
# bmi_fun(1.7, NA)
# expected output: NA
# bmi_fun(NA, NA)
# expected output: NA

# Testing bmi_fun() when both parameters are not NA
# bmi_fun(1.7, 60)
# expected output: 20.76125

# Testing pct_time_fun() when age is out of range
# pct_time_fun(-1, 2, 1)
# expected output: NA

# Testing pct_time_fun() when immigrant status is out of range
# pct_time_fun(20, 3, 1)
# expected output: NA

# Testing pct_time_fun() when time in Canada is out of range
# pct_time_fun(20, 2, 3)
# expected output: NA

# Testing pct_time_fun() when all parameters are in range
# pct_time_fun(20, 2, 1)
# expected output: 0.225

# Testing resp_condition_fun1() when age is out of range
# resp_condition_fun1(-1, 1)
# expected output: NA

# Testing resp_condition_fun1() when COPD/Emphs is out of range
# resp_condition_fun1(2, 0)
# expected output: NA

# Testing resp_condition_fun1() when both parameters are in range
# resp_condition_fun1(40, 1)
# expected output: 1

# Testing resp_condition_fun2() when age is out of range
# resp_condition_fun2(-1, 1, 1, 1)
# expected output: NA

# Testing resp_condition_fun2() when emphysema is out of range
# resp_condition_fun2(40, 3, 1, 1)
# expected output: NA

# Testing resp_condition_fun2() when COPD is out of range
# resp_condition_fun2(40, 1, 3, 1)
# expected output: NA

# Testing resp_condition_fun2() when chronic bronchitis is out of range
# resp_condition_fun2(40, 1, 1, 3)
# expected output: NA

# Testing resp_condition_fun2() when all parameters are in range
# resp_condition_fun2(40, 1, 1, 1)
# expected output: 1

# Testing resp_condition_fun3() when age is out of range
# resp_condition_fun3(-1, 1, 1)
# expected output: NA

# Testing resp_condition_fun3() when COPD/Emphys is out of range
# resp_condition_fun3(35, 4, 1)
# expected output: NA

# Testing resp_condition_fun3() when bronchitis is out of range
# resp_condition_fun3(35, 1, 4)
# expected output: NA

# Testing resp_condition_fun3() when all parameters are in range
# resp_condition_fun3(35, 1, 1)
# expected output: 1

# Testing pack_years_fun() when SMKDSTY is out of range
# pack_years_fun(10, 40, 6, 6, 22, 96, 12, 996, 996, 96, 96, 1)
# expected output: NA

# Testing pack_years_fun() when DHHGAGE_cont is out of range
# pack_years_fun(1, -1, 6, 6, 22, 96, 12, 996, 996, 96, 96, 1)
# expected output: NA

# Testing pack_years_fun() when SMKG203_cont is out of range
# pack_years_fun(1, 40, 6, 6, 100, 96, 12, 996, 996, 96, 96, 1)
# expected output: NA

# Testing pack_years_fun() when all parameters in range
# pack_years_fun(1, 40, 6, 6, 22, 96, 12, 996, 996, 96, 96, 1)
# expected output: 10.8
