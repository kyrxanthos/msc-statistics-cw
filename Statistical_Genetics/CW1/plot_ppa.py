import numpy as np
import matplotlib.pyplot as plt
pis = np.linspace(0, 1, 100)


def ppa(bf, pi):
    return(bf / (bf - 1 + 1/pi))
    
plt.figure(figsize=(10,5))
for bf in [1, 0.3, 0.1, 0.03, 0.01]:
    plt.plot(pis, ppa(bf, pis), label = 'BF : {}'.format(bf))

plt.xlabel('$\pi$')
plt.ylabel('PPA')
plt.legend()
plt.savefig('ppa_plot.pdf')
plt.show()
