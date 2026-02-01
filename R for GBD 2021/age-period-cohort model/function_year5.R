#### 该功能合并不同年份数据，5年为一单位
#### 参考代码：https://blog.csdn.net/NickyCat/article/details/118636690?ops_request_misc=%257B%2522request%255Fid%2522%253A%2522164585075016781683955469%2522%252C%2522scm%2522%253A%252220140713.130102334..%2522%257D&request_id=164585075016781683955469&biz_id=0&utm_medium=distribute.pc_search_result.none-task-blog-2~all~baidu_landing_v2~default-2-118636690.pc_search_result_control_group&utm_term=GBD%E6%95%B0%E6%8D%AE%E5%BA%93&spm=1018.2226.3001.4187
#### 做部分调整
function_year5 <- function(table_name, start_year, end_year, current_year){
  
  remain <- current_year - floor((current_year - start_year)/5) * 5 
  year_names <- NULL
  for (i in start_year:end_year) {
    if((i - current_year)/5 - floor((i - current_year)/5) == 0){
      if(i == remain){
        temp <- paste(start_year, i, sep = '-')
        year_names <- append(year_names, temp)
      }
      else{
        temp <- paste(i-4, i, sep = '-')
        year_names <- append(year_names, temp)
      }
    }
  }
  
  table_name <- as.data.frame(table_name)
  new_years <- seq(start_year,end_year,1)
  new_table <- as.data.frame(matrix(data = rep(0, length(year_names)*nrow(table_name)), ncol = length(year_names), nrow = nrow(table_name)))  %>% as.data.frame()
  colnames(new_table) <- year_names
  
  j = 1
  for (i in 1:(end_year - start_year + 1)){
    if((new_years[i] - current_year)/5 - floor((new_years[i] - current_year)/5) != 0){
      new_table[, year_names[j]] <- new_table[,year_names[j]] + table_name[,as.character(new_years[i])]
    }
    else{
      if(j == 1){
        new_table[,year_names[j]] <- (new_table[,year_names[j]] + table_name[,as.character(new_years[i])]) / (remain - start_year + 1)
      }
      else{
        new_table[,year_names[j]] <- (new_table[,year_names[j]] + table_name[,as.character(new_years[i])]) / 5
      }
      j = j + 1
    }
  }
  return(new_table)
}