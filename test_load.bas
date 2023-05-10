#include "nn_class.bas"
dim shared as NNetwork brain
brain.Load("best.nnb")
brain.Activation(nRELU)

screenres 512,512,32


dim as Neuron ptr ou
for y as long = 0 to 511
for x as long = 0 to 511

	brain.Clear()
		brain.NR[0].signal = (x-255)/255
		brain.NR[1].signal = (y-255)/255
	brain.Tick()
	ou = brain.Out()
	
	dim as long v  = ou[0].signal*255
	if v<0 then v=0
	if v>255 then v=0
	pset(x, y), rgb(v,v,v)
next
next

sleep 
