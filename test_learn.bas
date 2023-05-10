#include "nn_class.bas"


dim shared as NNetwork brain

sub createLayers()
	dim as long bias = 1
	dim as long layer(), layers
	layers= 5 : redim layer(layers-1)
	layer(0)=bias + 2
	layer(1)=bias + 4
	layer(2)=bias + 4
	layer(3)=bias + 4
	layer(4)=bias + 1
	brain.Create(layer(), layers, bias,  0,0)
	brain.Randomize(-0.8, 0.8)
	brain.Activation(nRELU)
end sub
sub init_nn()
	createLayers()
	Brain.ClearDelta()	
	Brain.ClearDeltaSumm()
end sub
function botEvo(gen as long,lrn_rate as single) as single
	dim as single avg_diff=0
	dim as long j=0, max_j= 4
	dim as long MAX_SAMPLES=20000
	dim as single lr = lrn_rate
	dim as long p
	
	dim as single res ,dx,dy
	
	for k as long = 0 to MAX_SAMPLES-1
		if j=0 then brain.ClearDelta()

		dx = rnd*2-1
		dy = rnd*2-1
		res = sqr(dx*dx+dy*dy)
		
		Brain.Clear()
			brain.NR[0].signal = dx
			brain.NR[1].signal = dy
		Brain.Tick()
		Brain.BackPropogation( @res, 1 )		
		dim as Neuron ptr ou = Brain.Out()

		avg_diff += abs(ou[0].signal - res)

		j+=1
		if j=max_j or k=MAX_SAMPLES-1 then
			Brain.GradientDescent(j, lr, 0.01)
			'Brain.LimitWeights(brain, 0,  3, -2, 2)
			j=0
		end if
	next

	return avg_diff/MAX_SAMPLES
end function





sub main()
	randomize timer
	init_nn()
	
	screenres 1024,512,32
		
	dim as double fps, fpt_tm=timer
	dim as long cfps
	dim as long curSample=0
	dim as single loss_res, lrn_rate= 0.01
	dim as long gen, less_lrn=0
	do : gen += 1
		
		loss_res = botEvo(gen, lrn_rate)
		'? gen & " " & loss_res
		pset(gen \ 5, 512-loss_res*100)

		locate 1,1
		? gen & " | " &  lrn_rate & " " & "  fps: " & fps
		? loss_res
		? 

		if multikey(1) then Brain.Save("best.nnb")
		if multikey(&h02) then lrn_rate=0.0005
		if multikey(&h03) then lrn_rate=0.0001
		if multikey(&h04) then lrn_rate=0.00005
		if multikey(&h05) then lrn_rate=0.00001
		if multikey(&h06) then lrn_rate=0.000005
		'if multikey(&h39) then curSample = int(rnd*samplec)

		cfps+=1
		if cfps=10 then
			fps = cfps/ (timer-fpt_tm)
			fpt_tm=timer
			cfps=0
		end if
	loop

	sleep 
end sub

main()

