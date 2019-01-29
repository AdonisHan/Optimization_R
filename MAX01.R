### R-Fairies ######
### Optimization ###
### Linear Programming

## EX 1
## 설명 : 페어리 제조업체에서 x1,x2 2개 형태의 구조물을 제조하여 판매하려고 한다. 
# 구조물을 제조하여 판매할 경우, 구조물 x1은 4만원, 구조물 x2는 3만원의 이익을 얻을 것으로 추정된다. 
# 구조물의 제조과정에서 A,B,C 3종류의 자원이 소요되며, x1,x2 구조물별로 소요되는 A,B,C 자원의 수량과 각 
# 자원별 최대 사용 가능량은 다음의 표에서 보는바와 같다. 즉, 구조물 x1은 A자원 2단위 소모, B자원 3단위 생산,
# C자원 2단위 소모하며, 구조물 x2는 A자원 3단위, B자원 2단위, C자원 1단위를 소모한다. 

# 소요자원	구조물 X1	구조물 X2	사용가능 자원량
# A         	2	      3	              6
# B	          -3	    2	              2
# C	          2	      1	              4
# 이익	      4	      3	

### Question : 이 제조업체가 최대이익을 얻기 위한 구조물 생산계획을 어떻게 수립해야 할까? ###

# 최대화 문제 Max Z 방법	
# (1) 결정변수 :x1,x2 의 생산량 	
# 목적함수 max Z : 4x1 + 3x2	
# 제약조건 	
# 2x1 + 3x2 <= 6	자원A
# -3x1 + 2x2 <= 2	자원B
# 2x1 + x2 <=4	자원C
# x1,x2 >= 0 비음조건	

library(lpSolve)
# 목적함수 Z 공헌계수
f <- c(4,3)
# 제약조건의 기술계수
a = matrix(c(2,-3,2,3,2,1), nr=3)
#       [,1] [,2]
# [1,]    2    3
# [2,]   -3    2
# [3,]    2    1
d = c("<=", "<=", "<=")
#사용가능 자원량
r = c(6,2,4)

### Insight ###

#(1) Optimal Solution
lp("max",f,a,d,r)
# Success: the objective function is 9 

#결정 변수값 : 최적 생산계획은 구조물 x1 = 1.5단위, x2  = 1 단위를 생산하는 것이고 이때 얻을 수 
# 있는 최대이익은 9단위로 분석된다.
lp("max",f,a,d,r)$solution
# [1] 1.5 1.0

#(2) Sensitivity Analysis 
lp("max",f,a,d,r,compute.sens = TRUE)$sens.coef.from
# [1] 2 2
lp("max",f,a,d,r,compute.sens = TRUE)$sens.coef.to
# [1] 6 6

# 결정변수 x1의 기저변수(Basic Variable)로서 기능하기 위한 생산계획 유지범위는 2~6 (최적값 x1 = 1.5)
# 결정변수 x2의 기저변수(Basic Variable)로서 기능하기 위한 생산계획 유지범위는 2~6 (최적값 x1 = 1.0)

#(3) Duel Problem 
lp("max",f,a,d,r,compute.sens = TRUE)$duals
# [1] 0.5 0.0 1.5 0.0 0.0
# x1,x2의 shadow price 는A,B,C = 0.5 / 0.0 / 1.5 (결과의 앞 3개)
# x1,x2의 shadow cost 는 0.0 / 0.0 (결과의 뒤 2개)
# 결론 - 이에 따라 자원 C의 가치가 가장 높다. 자원C를 1단위 증가하면 이익이 1.5단위 증가함을 의미한다.
# 시장가치가 이보다 낮으면 자원 C를 충당하여 이익을 증대하게 된다.
# x1,x2의 그림자 비용은 Zero로서 결정변수 x1,x2 모두 이익 증대에 기여함을 나타낸다.

## 더 나아가기
# (4) Dual + Sensitivity
lp("max",f,a,d,r,compute.sens = TRUE)$duals.from
lp("max",f,a,d,r,compute.sens = TRUE)$duals.to
# [1]  4.000000e+00 -1.000000e+30  2.615385e+00 -1.000000e+30 -1.000000e+30
# [1]  8.571429e+00  1.000000e+30  6.000000e+00  1.000000e+30  1.000000e+30
# x1,x2의 shadow price 범위 A,B,C 는 4~8.57 / -∞  ~ +∞  / 2.61~6
# x1,x2의 shadow cost 는  -∞  ~ +∞/ -∞  ~ +∞..