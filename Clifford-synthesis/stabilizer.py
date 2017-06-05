import numpy as np
import scipy as sp
import scipy.linalg


class Circuit:
	# note when manipulating everything is transposed becuase it 
	# is more natural to manipulate matrices in row fashion 
	# rather than column fashion
	def __init__(self, n):
		self.n = n
		# don't worry about transposing here - they are just 
		i = np.array([[1, 0], [0, 1]])
		self.circuit = reduce(np.kron, [i]*(n-1), i)
		
	def printFinalCircuit(self):
		# transpose circuit to bring back to original column
		# format
		print np.transpose(self.circuit)

	# Applying S to qubit k
	# n = number of qubits
	# circuit = matrix representation of the circuit
	def s(self, k):
		# S(k) = row(k-1) += row(n+k-1) % 2
		self.circuit[k-1] += self.circuit[self.n+k-1]
		return self

	def h(self, k):
		# H(k) = exchange row(k-1) and row(n+k-1)
		temp = self.circuit[k-1].copy()
		self.circuit[k-1] = self.circuit[self.n+k-1]
		self.circuit[self.n+k-1] = temp
		return self

	# cnot with control k and target j
	def cnot(self, k, j):
		self.circuit[j-1] += self.circuit[k-1]
		self.circuit[self.n+k-1] += self.circuit[self.n+j-1]
		return self

	def equivalent(circuit1, circuit2):
		return np.array_equal(circuit1.circuit, circuit2.circuit)

if __name__ == "__main__":
	c = Circuit(2)
	c.printFinalCircuit()
	c.s(2)
	c.printFinalCircuit()
	c.h(1)
	c.printFinalCircuit()
	c.cnot(1, 2)
	c.printFinalCircuit() 

	circuit1 = Circuit(2).h(1).h(2).cnot(1, 2).h(1).h(2)
	circuit2 = Circuit(2).cnot(2, 1)
	print Circuit.equivalent(circuit1, circuit2)