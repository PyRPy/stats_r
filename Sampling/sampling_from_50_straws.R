
# Sampling from 50 Straws -------------------------------------------------
# This sampling method is for 'draw 6 x yao' to determine a 'gua' use 
# 'yin' or 'yang' to represent changes;

# 'yin' in yiching is like 0  while 'yang' is 1, so the gua is like a 
# six digits like 011011

# The sampling method is farily a 'complicated' process, comprising 3 steps

draw_yao <- function(straw_start) {
  
  straw_half_left <- sample(straw_start, 
                            floor(length(straw_start)/2.0) + sample(1:4, 1))
  straw_half_right <- setdiff(straw_start, straw_half_left)
  
 
  straw_half_left_lessone <- sample(straw_half_left, length(straw_half_left)-1)
  
  group_four = 4
  draw_remainder = length(straw_half_left_lessone) %% group_four
  
  if (draw_remainder != 0 ) {
    
    straw_half_left_lessone2 <- sample(straw_half_left_lessone, length(straw_half_left_lessone) - draw_remainder)
    
  } else {
    straw_half_left_lessone2 <- sample(straw_half_left_lessone, length(straw_half_left_lessone) - group_four)
  }
  
  
  draw_remainder = length(straw_half_right) %% group_four
  
  if (draw_remainder != 0 ) {
    
    straw_half_right2 <- sample(straw_half_right, length(straw_half_right) - draw_remainder)
    
  } else {
    straw_half_right2 <- sample(straw_half_right, length(straw_half_right) - group_four)
  }
  
  return (union(straw_half_left_lessone2, straw_half_right2))
  
}

yao_number <- function(straw_start1) {
  group_four = 4
  yao1 = draw_yao(straw_start1)
  yao2 = draw_yao(yao1)
  yao3 = draw_yao(yao2)
  
  return (length(yao3) / group_four)
  
}


gua <- c()

for (i in (1:6)) {
  
  straw_ID <- c(1:50)
  size_temp <- length(straw_ID)-1
  straw_start1 <- sample(straw_ID, 49)
  yao_temp <- yao_number(straw_start1)
  gua <- c(gua, yao_temp)
  cat("yao", i,  " ",  yao_temp, "\n")
  
}

gua
