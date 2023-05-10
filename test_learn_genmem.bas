#include "nn_class.bas"

type AI_obj
	as NNetwork nn
	as single derr
end type

dim shared as AI_obj bot()
dim shared as long   botc

dim shared as single ai_min_derr

sub nn_init()
	'initRND()
	
	dim as long layers
	dim layer() as long
	dim as long bias = 1
	layers=5
	redim layer(layers-1)
	layer(0)= bias + 3
	layer(1)= bias + 6
	layer(2)= bias + 6
	layer(3)= bias + 4
	layer(4)= bias + 2
	
	botc = 200
	redim bot(botc-1)
	for n as long = 0 to botc-1
		bot(n).nn.Create( layer(), layers, bias, 0, 0)
		bot(n).nn.Randomize(-0.5, 0.5, 0.1)
		bot(n).nn.Activation(nRELU)
	next
end sub


sub nn_sort()
	for n as long = 0 to botc-1
		for m as long = n+1 to botc-1
			if bot(m).derr < bot(n).derr then
				NNetwork_Swap(bot(m).nn, bot(n).nn)
				swap bot(n).derr, bot(m).derr
			end if
		next
	next
	ai_min_derr= bot(0).derr
end sub
sub nn_select()
	dim as long keep = botc*0.2, i1, i2
	for n as long = keep to botc-1
		i1= int(rnd*keep)
		i2= int(rnd*keep)
		bot(n).nn.Crossing(bot(i1).nn, bot(i2).nn, 0.03, 0.01)
	next
end sub


dim as single avgerr = 0

screenres 512,512,32

nn_init()
dim as long gen = 0
do
	dim as single in1, in2, in3, oux, ouy
	
	oux = rnd
	ouy = rnd
	in1 = sqr(oux*oux + ouy*ouy)
	in2 = sqr( (1-oux)^2 + (0-ouy)^2  )
	in3 = sqr( (oux)^2 + (1-ouy)^2  )
	dim as Neuron ptr ou
	for n as long = 0 to botc-1
		
		bot(n).nn.Clear()
			bot(n).nn.NR[0].signal = in1
			bot(n).nn.NR[1].signal = in2
			bot(n).nn.NR[2].signal = in3
		bot(n).nn.Tick()	
		ou = bot(n).nn.Out()
		
		bot(n).derr = abs(ou[0].signal - oux) + abs(ou[1].signal - ouy)
	next
	
	nn_sort()
	nn_select()
	
	if gen mod 100 = 0 then 
		avgerr = avgerr*0.99 + bot(0).derr*0.01
		locate 1,1
		? avgerr & "         "
	end if
	
	if multikey(1) then bot(0).nn.Save("mem.nnb")
	gen+=1
loop








