#include "nn_class.bas"

dim shared as NNetwork brain

brain.Load("mem.nnb")
brain.Activation(nRELU)

screenres 512,512,32


dim as single px, py, in1,in2, in3
dim as Neuron ptr ou
for y as long = 0 to 255
for x as long = 0 to 255
	
	
	px = x/255
	py = y/255
	in1 = sqr(px*px + py*py) 
	in2 = sqr((1-px)^2 + (0-py)^2) 
	in3 = sqr( (px)^2 + (1-py)^2  )

	brain.Clear()
		brain.NR[0].signal = in1
		brain.NR[1].signal = in2
		brain.NR[2].signal = in3
	brain.Tick()	
	ou = brain.Out()
	
	px = ou[0].signal * 255
	py = ou[1].signal * 255
	
	pset(128+px,128+py)
	
next
next

line(128,128)-step(255,255),,B

sleep 