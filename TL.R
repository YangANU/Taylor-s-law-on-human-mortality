########
# Japan
########

TL_female = TL_male = TL_total = TL_female_2 = TL_male_2 = TL_total_2 = vector("numeric", 48)
for(ik in 1:48)
{
		TL_female[ik] = coef(lm(log(apply(get(state[ik])$rate$female, 1, var), base=10)~log(apply(get(state[ik])$rate$female, 1, mean), base=10)))[2]
		TL_male[ik] = coef(lm(log(apply(get(state[ik])$rate$male, 1, var), base=10)~log(apply(get(state[ik])$rate$male, 1, mean), base=10)))[2]
		TL_total[ik] = coef(lm(log(apply(get(state[ik])$rate$total, 1, var), base=10)~log(apply(get(state[ik])$rate$total, 1, mean), base=10)))[2]
		
		TL_female_2[ik] = coef(lm(log(apply(get(state[ik])$rate$female, 2, var), base=10)~log(apply(get(state[ik])$rate$female, 2, mean), base=10)))[2]
		TL_male_2[ik] = coef(lm(log(apply(get(state[ik])$rate$male, 2, var), base=10)~log(apply(get(state[ik])$rate$male, 2, mean), base=10)))[2]
		TL_total_2[ik] = coef(lm(log(apply(get(state[ik])$rate$total, 2, var), base=10)~log(apply(get(state[ik])$rate$total, 2, mean), base=10)))[2]
}	

range(TL_female)
range(TL_male)
range(TL_total)

range(TL_female_2)
range(TL_male_2)
range(TL_total_2)

