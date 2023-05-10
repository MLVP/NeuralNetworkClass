#include "nn_class.bas"

sub DrawNetwork(b as NNetwork)
	dim as long x , y
	dim as long i
	
	line(0,0)-(640,480), &H443366, BF
	
	' set position
	for l as long = 0 to b.layers-1
		for n as long = 0 to b.layer[l] - 1		
			i = b.layer_i[l]+n
			b.NR[i].xp = 32 + l* 64
			b.NR[i].yp = 32+ n*32
		next
	next
	
	' draw layers
	for l as long = 0 to b.layers-1
		i = b.layer_i[l]+0
		draw string (b.NR[i].xp-8, b.NR[i].yp-26), str(b.layer[l]), &HFF0000
	next
	
	' draw links
	for n as long = 0 to b.NRc-1
		for l as long = 0 to b.NR[n].nc - 1
			i = b.NR[n].L[l]
			'dim as long v = l*255 / b.NR[n].nc
			line(b.NR[n].xp, b.NR[n].yp) - (b.NR[i].xp, b.NR[i].yp),  &H888888
		next		
		'sleep 
	next

	' Draw Neurons
	for l as long = 0 to b.layers-1
		for n as long = 0 to b.layer[l] - 1
			i = b.layer_i[l]+n
			
			draw string (b.NR[i].xp-15, b.NR[i].yp-15), str(i), &HFFFF00
			if b.bias=1 and n = b.layer[l] - 1	then
				circle(b.NR[i].xp, b.NR[i].yp), 15, &H0000FF
			else
				circle(b.NR[i].xp, b.NR[i].yp), 15, &HFFFFFF
			end if
		next
	next
	
end sub



dim  as NNetwork brain



dim as long bias = 1
dim as long layer(), layers
layers= 3 : redim layer(layers-1)
layer(0)=bias + 2
layer(1)=bias + 3
layer(2)=bias + 2
brain.Create(layer(), layers, bias,  0,0)
brain.Randomize(-0.8, 0.8)
brain.Activation(nRELU)



screenres 640,480,32

DrawNetwork(Brain)
sleep 
brain.AddNeuron(0)
DrawNetwork(Brain)
sleep 
brain.AddNeuron(1)
DrawNetwork(Brain)
sleep


brain.SimplifyWeights(2)
DrawNetwork(Brain)
sleep 









