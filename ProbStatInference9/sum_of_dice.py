# simulate a dice
import random
import matplotlib.pyplot as plt 

dice = []
for i in range(1000):
    dice1 = random.randint(1, 6)
    dice2 = random.randint(1, 6)
    sum = dice1 + dice2
    dice.append(sum)
print(dice)

plt.hist(dice)
plt.show()
