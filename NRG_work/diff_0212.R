# multiple proportional z test

# GENDER/AGE, there is no diff
p1 <- c(81, 84, 84, 83, 81, 78)
p2 <- c(167, 167, 167, 167, 167, 167)

prop.test(p1, p2, conf.level = 0.95)


# REGION, only Delhi and Mumbai has no diff
p1 <- c(78, 81, 84, 84)
p2 <- c(155, 345, 400, 100)

prop.test(p1, p2, conf.level = 0.95)


prop.test(c(81,84), c(345, 400), conf.level = 0.95)


#===============
prop.test(c(140,125), c(500, 500), conf.level = 0.95)

prop.test(c(252,225), c(900,900), conf.level = 0.95)

a=1750
prop.test(c(a*0.28, a*0.25), c(a,a), conf.level = 0.95)

# multiple proportional z test

# GENDER/AGE, there is no diff
p1 <- c(402,472,402)
p2 <- c(1004,1004,1004)

prop.test(p1, p2, conf.level = 0.95)

prop.test(c(402,472), c(1004,1004), conf.level = 0.95)

#====
prop.test(c(40, 35), c(85, 121), conf.level = 0.95)

prop.test(c(24, 18), c(167, 176), conf.level = 0.95)
