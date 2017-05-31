import numpy as np
import scipy as sp
import scipy.linalg

def Normalize(state):
	return state / sp.linalg.norm(state)

def Tensor(*args):
	result = np.array([1.0])
	for op in args:
		result = np.kron(result, op)
	return result

# single qubit definitions
zero = np.array([1.0, 0.0], dtype=complex)
one = np.array([0.0, 1.0], dtype=complex)
plus = Normalize(zero + one)
minus = Normalize(zero - one)

# Gate set
i = np.array([[1, 0], [0, 1]])
x = np.array([[0, 1], [1, 0]])
y = np.array([[0, 0 - 1j], [0 + 1j, 0]], dtype=complex)
z = np.array([[1, 0], [0, -1]])
hadamard = (1.0 / np.sqrt(2)) * np.array([[1, 1], [1, -1]])
phase = np.array([[1, 0], [0, 0 + 1j]], dtype=complex)
cnot = np.array([[1, 0, 0, 0],
				[0, 1, 0, 0],
				[0, 0, 0, 1],
				[0, 0, 1, 0]])

# Convenience gates
revcnot = np.array([[1, 0, 0, 0], 
				[0, 0, 0, 1],
				[0, 0, 1, 0],
				[0, 1, 0, 0]])

def H(q):
	return np.dot(hadamard, q)

def apply(gate, q):
	return np.dot(gate, q)


if __name__ == "__main__":
	print zero
	print x
	print i
	print H(zero)
	print np.dot(y, zero)
	q = Tensor(zero, one)
	print apply(cnot, q)
	print(np.kron(x, i))
	print(np.kron(i,x))

	original_gates = [np.kron(hadamard, i), np.kron(i, hadamard), cnot, np.kron(hadamard, i),  np.kron(i, hadamard)]
	original = reduce(np.dot, original_gates, np.kron(i, i))
	optimized = revcnot
	print "original"
	print original
	print "optimized"
	print optimized
	print np.kron(i, i)

