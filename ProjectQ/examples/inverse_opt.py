from projectq import MainEngine
from projectq.ops import H, CNOT, Measure
from projectq.backends import CircuitDrawer, Simulator
from projectq.cengines import LocalOptimizer

# Figure 5b from QProject Paper

# drawing_engine = CircuitDrawer()
# eng = MainEngine(drawing_engine)
eng = MainEngine(Simulator(), engine_list=[LocalOptimizer()])

q = eng.allocate_qureg(3)

H | q[0]
H | q[1]
H | q[2]
H | q[0]
CNOT | (q[1], q[0])
H | q[0]
H | q[1]
H | q[0]
CNOT | (q[2], q[0])
H | q[0]
H | q[2]

# measure
Measure | q[0]
Measure | q[1]
Measure | q[2]

eng.flush()
# print the result:
print("Measured: {}".format(int(q[0])))
print("Measured: {}".format(int(q[1])))
print("Measured: {}".format(int(q[2])))

# print circuit
#print(drawing_engine.get_latex())

