#include once "randomizer_class.bas"
#include once "crt.bi"
#include once "crt/math.bi"

Enum NeuronActivation
	SAME = 0
	RELU
	lRELU
	nRELU
	TANHENT
	SIGMOID
end enum

Type Neuron
    signal As Single 			' signal
	
	tmp_signal as single
	mem as single 				' % of saved signal
	
    W As Single PTR =0 			' Вес синопса
    L As Long PTR =0 			' Ссылка на нейрон
	delta as single PTR=0 		' ошибка связи
	delta_speed as single PTR=0	' ошибка связи
	
	Er as single 				' ошибка нейрона
	Gr as single 				' градиент ошибки
	
	nc as long 
	as long xp, yp
End Type

Type NNetwork
    NR As Neuron PTR=0			' Array of neurons
    NRc As Long					' Count of neurons
	layer as long ptr=0			' Array of count of neurons for each layer
	layers as long				' Count of layers
	layer_i as long ptr=0		' index/starting neuron for each layer
	
	bias as long				' Bias used
	memory as long				' Memory used

	thdW as single ptr = 0
	layer_wc as long ptr = 0
	
	as single gradW


	as long ptr layer_a =0		' Functuion for each layer
	as integer ptr ptr		acs_fnc_ptr = 0
	as integer ptr ptr		acs_fncder_ptr = 0
	
	declare sub 		Create(layer() as long, layers as long, bias as long=0, mem as long=0, nolinks as long = 0)
	declare sub 		Activation(act as NeuronActivation, sel_layer as long = -1)
	declare sub 		Destroy()
	declare sub 		Clear(clearmem as long=0)
	declare sub 		ClearDelta()
	declare sub 		ClearDeltaSumm()
	declare sub 		Tick(start_layer as long = 0)
	declare sub 		threadPrepareData()
	declare sub 		TickMT(threads as long=2)
	declare sub 		Randomize(v1 as single, v2 as single, memr as single=0)
	declare sub 		LimitWeights(l1 as long, l2 as long, v1 as single=-2, v2 as single=2)
	declare sub 		LimitDeltaSpeed(l1 as long, l2 as long, v1 as single=-2, v2 as single=2)
	declare sub 		Copy(dst as NNetwork)
	declare sub 		AvgWith(src as NNetwork)
	declare sub 		Crossing(byref b1 as NNetwork, byref  b2 as NNetwork, chance as single=0.03, mutaion as double=0.0)
	declare sub 		Round(offset as single = 0.00001)
	declare sub 		Mutate(mutaion as single=0.0)
	declare sub			RemoveUnused()
	declare sub			Conv2D(n_layer as long, sw as long, sh as long, svec_size as long, dw as long, dh as long, dvec_size as long, rad as single = 2)
	declare sub 		KeepOnlyWeights(layer as long=0, max as long=1)
	declare sub 		OptimizeWeights(layer_idx as long=0, prec as single = 0.5)
	declare sub 		Interpolate(byref b1 as NNetwork, byref b2 as NNetwork, k as single)
	declare sub  		PassErrorFrom( src as NNetwork, Weights as single ptr )
	declare sub 		BackPropogation(targets as single ptr, norm_err as long = 0, layers_back as long = 999, no_gradient as long =0, include_layer0 as long=0, noerr as long = 0, gradientW as single = 1)
	declare sub 		GradientDescent(learn_rate as single=0.1, inertia_k as single=0, layers_back as long = 999)
	declare function 	Out(out_layer as long = -1) as Neuron ptr
	declare sub  		Save(filename as string)
	declare function 	Load(filename as string) as long
	declare function 	WeightsCount() as long


	declare function 	AddNeuron(sel_layer as long, rnd_weights as single = 0.001) as long
	
	declare function  	CountWeights() as long	
End Type



type nnmaskfnc as function(x as single) as single

CONST as long nnetwork_version = 4
CONST as long nnetwork_min_version = 3

dim shared as Randomizer nn_rnd = 1048576


function nns_same(x as single) as single
	return x
end function
function nns_same_der(x as single) as single
	return 1
end function

Function nns_ReLU(x As single) As single
    If x > 0 Then
        Return x
    Else
        Return 0
    End If
End Function
Function nns_ReLU_der(x As single) As single
    If x > 0 Then
        Return 1
    Else
        Return 0
    End If
End Function

function nns_lReLU(x as single) as single
    if x >= 0 then
        return x
    else
        return 0.01 * x
    end if
end function
function nns_lReLU_der(x as single) as single
    if x >= 0 then
        return 1
    else
        return 0.01
    end if
end function

function nns_nReLU(x as single) as single
	if x>1 then return 1+(x-1)*0.5
	if x<-1 then return -1 + (x--1)*0.5
	return x
end function
function nns_nReLU_der(x as single) as single
	if x<-1 then return 0.5
	if x>1 then return 0.5
	return 1
end function

function nns_tanh(x As Single ) As Single
	return  (exp(x) - exp(-x)) / (exp(x)+exp(-x))
end function
function nns_tanh_der(x As Single ) As Single
	return 1-x*x
end function

function nns_sigmoid(x As Single ) As Single
	return (1.0 / (1.0 + exp(-x)))
end function
function nns_sigmoid_der(x As Single ) As Single
	return x*(1-x)
end function


sub NNetwork_Swap(b1 as NNetwork, b2 as NNetwork)
	swap b1.NR, b2.NR
	swap b1.NRc, b2.NRc
end sub
function NNetwork_Compare(b1 as NNetwork, b2 as NNetwork) as long
	if b1.NRc<>b2.NRc then return 1
	for j as long = 0 to b1.NRc-1
		if b1.NR[j].nc <>  b2.NR[j].nc then return 2
		for k as long = 0 to b1.NR[j].nc-1
			if b1.NR[j].L[k] <> b2.NR[j].L[k] then return 3
			if b1.NR[j].W[k] <> b2.NR[j].W[k] then return 4
		next 
	next
	return 0
end function

sub NNetwork.Activation(act as NeuronActivation, sel_layer as long = -1)

	if sel_layer=-1 then 
		if this.layer_a then deallocate(this.layer_a)
		if this.acs_fnc_ptr then deallocate(this.acs_fnc_ptr)
		if this.acs_fncder_ptr then deallocate(this.acs_fncder_ptr)
		
		this.layer_a = callocate( this.layers, sizeof(long) )
		this.acs_fnc_ptr = callocate( this.layers, sizeof(integer) )
		this.acs_fncder_ptr = callocate( this.layers, sizeof(integer) )
	end if
	
	for n as long = 0 to this.layers - 1
		if sel_layer=-1 or sel_layer=n then 
			this.layer_a[n] = act
			select case act
			case SAME
				this.acs_fnc_ptr[n] = cast(integer ptr, @nns_same)
				this.acs_fncder_ptr[n] = cast(integer ptr, @nns_same_der)
			case RELU
				this.acs_fnc_ptr[n] = cast(integer ptr, @nns_ReLU)
				this.acs_fncder_ptr[n] = cast(integer ptr, @nns_ReLU_der)
			case lRELU
				this.acs_fnc_ptr[n] = cast(integer ptr, @nns_lReLU)
				this.acs_fncder_ptr[n] = cast(integer ptr, @nns_lReLU_der)
			case nRELU
				this.acs_fnc_ptr[n] = cast(integer ptr, @nns_nReLU)
				this.acs_fncder_ptr[n] = cast(integer ptr, @nns_nReLU_der)
			case TANHENT
				this.acs_fnc_ptr[n] = cast(integer ptr, @nns_tanh)
				this.acs_fncder_ptr[n] = cast(integer ptr, @nns_tanh_der)
			case SIGMOID
				this.acs_fnc_ptr[n] = cast(integer ptr, @nns_sigmoid)
				this.acs_fncder_ptr[n] = cast(integer ptr, @nns_sigmoid_der)
			end select
		end if
	next
end sub

sub NNetwork.Interpolate(byref b1 as NNetwork, byref b2 as NNetwork, k as single)
	for n as long =0 to this.NRc-1
		for m as long =0 to this.NR[n].nc-1
			this.NR[n].W[m] = b1.NR[n].W[m] + (b2.NR[n].W[m] - b1.NR[n].W[m])*k			
		next
	next 
end sub

sub NNetwork.Mutate(mutaion as single=0.0)
	for n as long =0 to this.NRc-1
		for m as long =0 to this.NR[n].nc-1
			this.NR[n].W[m] += (rnd-0.5)*mutaion
'			if this.NR[n].W[m]<-2 then this.NR[n].W[m]=-2
'			if this.NR[n].W[m]>2 then this.NR[n].W[m]=2
		next
		if this.memory then
			this.NR[n].mem += (rnd-0.5)*mutaion
			if this.NR[n].mem <0 then this.NR[n].mem =0
			if this.NR[n].mem >0.999 then this.NR[n].mem =0.999
		end if
	next 
end sub


sub NNetwork.Round(offset as single = 0.00001)
	for n as long =0 to this.NRc-1
		for m as long =0 to this.NR[n].nc-1
			if this.NR[n].W[m]<offset then this.NR[n].W[m]=0		
		next
		if this.memory then
			if this.NR[n].mem<offset then this.NR[n].mem=0			
		end if
	next 
end sub

sub NNetwork.Crossing(byref b1 as NNetwork, byref  b2 as NNetwork, chance as single=0.03, mutaion as double=0.0)
	for n as long =0 to this.NRc-1
		for m as long =0 to this.NR[n].nc-1
			if nn_rnd.Value<0.5 then
				this.NR[n].W[m] = b1.NR[n].W[m]
			else
				this.NR[n].W[m] = b2.NR[n].W[m]
			end if
			if nn_rnd.Value<chance then
				this.NR[n].W[m] = cdbl(this.NR[n].W[m]) + (nn_rnd.Value-0.5)*mutaion
			end if
		next
		if this.memory then
			if nn_rnd.Value<0.5 then
				this.NR[n].mem = b1.NR[n].mem
			else
				this.NR[n].mem = b2.NR[n].mem
			end if
			if nn_rnd.Value<chance then
				this.NR[n].mem += (nn_rnd.Value-0.5)*mutaion
			end if
		end if
	next 
end sub



sub NNetwork.RemoveUnused()
	for n as long = 0 to this.NRc - 1
		dim as long i=0
		for m as long = 0 to this.NR[n].nc - 1
			if this.NR[n].L[m]<>-1 then
				this.NR[n].L[i] = this.NR[n].L[m]
				this.NR[n].W[i] = this.NR[n].W[m]
				i+=1
			end if
		next
		this.NR[n].nc = i
	next
end sub


sub NNetwork.Conv2D(n_layer as long, sW as long, sH as long, svec_size as long, dW as long, dH as long, dvec_size as long, rad as single = 2)

	dim as single kx=sW/dW, ky=sH/dH, dx,dy
	dim as long i = this.layer_i[n_layer]
	for y as long = 0 to sH-1
		for x as long = 0 to (sW*svec_size)-1
			for m as long = 0 to this.NR[i].nc - 1
				dx = ((m\dvec_size) mod dW) * kx
				dy = ((m\dvec_size) \ dW) * ky
				if sqr( ((x\svec_size)-dx)^2 + (y-dy)^2 ) > rad then
					this.NR[i].L[m] = -1 ' remove link
				end if
			next
			
			i+=1
		next
	next

end sub


sub NNetwork.KeepOnlyWeights(layer_idx as long, lmax as long)
	dim as long i = this.layer_i[layer_idx], c = iif( this.bias, this.layer[layer_idx]-1, this.layer[layer_idx])

	for n as long = i to i+c - 1
		
		for l as long = 0 to lmax - 1 'this.NR[i].nc - 1
			dim as single maxw=0
			dim as long maxi=0
			for m as long = 0 to this.NR[n].nc - 1
				if abs(this.NR[n].W[m])>maxw then 
					maxw = abs(this.NR[n].W[m])
					maxi=m
				end if				
			next
			
			swap this.NR[n].W[l], this.NR[n].W[maxi]
			swap this.NR[n].L[l] , this.NR[n].L[maxi]
		next
		this.NR[n].nc = lmax
		
		'? this.NR[i].nc 
	next
end sub

sub NNetwork.OptimizeWeights(layer_idx as long=0, prec as single = 0.5)
	dim as long i = this.layer_i[layer_idx], c = iif( this.bias, this.layer[layer_idx]-1, this.layer[layer_idx])
	dim as long lmax
	
	for n as long = i to i+c - 1
		
		lmax = prec*this.NR[n].nc
		if lmax<1 then lmax=1
		if lmax>this.NR[n].nc-1 then lmax=this.NR[n].nc-1
		
		for l as long = 0 to lmax - 1 'this.NR[i].nc - 1
			dim as single maxw=0
			dim as long maxi=0
			for m as long = 0 to this.NR[n].nc - 1
				if abs(this.NR[n].W[m])>maxw then 
					maxw = abs(this.NR[n].W[m])
					maxi=m
				end if				
			next
			
			swap this.NR[n].W[l], this.NR[n].W[maxi]
			swap this.NR[n].L[l] , this.NR[n].L[maxi]
		next
		this.NR[n].nc = lmax
		
		'? this.NR[i].nc 
	next
end sub


function NNetwork.WeightsCount() as long 
	dim as long c=0
	for n as long = 0 to this.NRc-1
		c += this.NR[n].nc
	next
	return c
end function


type calc_thead_t
	as any ptr thd

	as long n1, n2, exist
	as single ptr wcl
end type


Sub tdr_CalcOut(ByVal ud As Any PTR)
	dim as calc_thead_t ptr 	ctt = cast(calc_thead_t ptr , ud)
	for n as long = ctt->n1 to ctt->n2
		ctt->wcl[n*4+2] = ctt->wcl[n*4+0] * ctt->wcl[n*4+1]
	next
end sub

sub NNetwork.threadPrepareData()

	this.layer_wc = allocate( this.layers * 4)
	dim as long maxw=0
	for l as long =0 to this.layers-2
		dim as long wcnt=0, i, w, oi
		i = this.layer_i[l]
		for n as long = i to i+this.layer[l] -1
			wcnt += this.NR[n].nc
		next
		this.layer_wc[l]=wcnt
		if wcnt>maxw then maxw = wcnt 
	next
	
	
	this.thdW = allocate(maxw*4*sizeof(single))
	
end sub

Sub NNetwork.TickMT(threads as long=2)

	dim as calc_thead_t 	td()

	for l as long =0 to this.layers-2
		dim as long i, w, oi
		i = this.layer_i[l]

		w = 0
		for n as long = i to i+this.layer[l] -1
			for m as long = 0 to this.NR[n].nc - 1
				this.thdW[w+0] = this.NR[n].signal
				this.thdW[w+1] = this.NR[n].W[m]
				this.thdW[w+2] = 0
				w+=4
			next
		next
		
		redim td(threads-1)
		dim as long ll = ( this.layer_wc[l] -1)/threads, i1=0, i2
		if ll<1 then ll=1
		for t as long = 0 to threads-1
			i2 = int(i1+ll-1)
			if t=threads-1 then i2 = this.layer_wc[l]-1
			if i2>this.layer_wc[l]-1 then i2=this.layer_wc[l]-1
			td(t).n1 = i1
			td(t).n2 = i2
			td(t).wcl = this.thdW
			td(t).exist = 1
			i1= i2+1
			if i1>this.layer_wc[l]-1 then exit for
		next
		for t as long = 0 to threads-1
			if td(t).exist then td(t).thd = threadcreate(@tdr_CalcOut, @td(t))
		next
		for t as long = 0 to threads-1
			if td(t).exist then threadwait(td(t).thd)
		next
		
		i = 0
		for n as long = this.layer_i[l] to this.layer_i[l]+this.layer[l] -1
			' суммируем сигналы текущего слоя
			' в каждый элементследущего слоя
			for m as long = 0 to this.NR[n].nc-1
				oi = this.NR[n].L[m]
				this.NR[oi].signal += this.thdW[i+2]
				i+=4
			next
		next
		
		
		dim as long k = this.layer_i[l+1]
		for n as long = 0 to this.layer[l+1] - iif(this.bias,2,1)
			this.NR[k].signal = cast(nnmaskfnc, this.acs_fnc_ptr[l+1])(this.NR[k].signal)
			k+=1
		next
		

	next
end sub

Sub NNetwork.Tick(start_layer as long = 0)
	dim as long i = 0, oi, k
	dim as single t
	
	dim as nnmaskfnc acs_fnc
	dim as single  sgl
	dim as long ptr ll
	dim as single ptr ww
	if this.memory then 
		for l as long = start_layer to this.layers-2
			i = this.layer_i[l]
			for n as long = 0 to this.layer[l] -1
				sgl = this.NR[i].signal
				ll = this.NR[i].L
				ww = this.NR[i].W
				for m as long = 0 to this.NR[i].nc-1
					this.NR[*ll].tmp_signal += sgl * (*ww)
					ll+=1
					ww+=1
				next
				i +=1
			next
			k = this.layer_i[l+1]
			acs_fnc =  cast(nnmaskfnc, this.acs_fnc_ptr[l+1])
			dim as single ns
			for n as long = 0 to this.layer[l+1] - iif(this.bias,2,1)
				'this.NR[k].signal = this.NR[k].signal*this.NR[k].mem + acs_fnc(this.NR[k].tmp_signal)*(1-this.NR[k].mem)
				ns = acs_fnc(this.NR[k].tmp_signal)
				this.NR[k].signal = ns + (this.NR[k].signal - ns) * this.NR[k].mem
				k+=1
			next
		next
		
	else
		for l as long = start_layer to this.layers-2
			i = this.layer_i[l]
			for n as long = 0 to this.layer[l] -1
				sgl = this.NR[i].signal
				ll = this.NR[i].L
				ww = this.NR[i].W
				for m as long = 0 to this.NR[i].nc-1
					this.NR[*ll].signal += sgl * (*ww)
					ll+=1
					ww+=1
				next
				i +=1
			next
			acs_fnc =  cast(nnmaskfnc, this.acs_fnc_ptr[l+1])
			dim as long k = this.layer_i[l+1]
			for n as long = 0 to this.layer[l+1] - iif(this.bias,2,1)
				this.NR[k].signal = acs_fnc(this.NR[k].signal)
				k+=1
			next
		next
		
'		for l as long =0 to this.layers-2
'			for n as long = 0 to this.layer[l] -1
'				for m as long = 0 to this.NR[i].nc-1
'					oi = this.NR[i].L[m]
'					this.NR[oi].signal += this.NR[i].signal * this.NR[i].W[m]
'				next
'				i +=1
'			next
'			k = this.layer_i[l+1]
'			for n as long = 0 to this.layer[l+1] - iif(this.bias,2,1)
'				this.NR[k].signal = cast(nnmaskfnc, this.acs_fnc_ptr[l+1])(this.NR[k].signal)
'				k+=1
'			next
'		next
		
	end if
	
End Sub

sub NNetwork.Clear(clearmem as long=0)
	dim i as long=this.layer_i[0]
	for n as long =0 to this.layers-1
		if this.memory then 
			for m as long=0 to this.layer[n] -1
				this.NR[i].tmp_signal = 0
				if clearmem then this.NR[i].signal = 0
				i+=1
			next
		else
			for m as long=0 to this.layer[n] -1
				this.NR[i].signal = 0
				i+=1
			next
		end if
		if this.bias then this.NR[i-1].signal = 1
	next
end sub
sub NNetwork.ClearDelta()
	this.gradW = 0
	for n as long =0 to this.NRc - 1
		for m as long = 0 to this.NR[n].nc-1
			this.NR[n].delta[m] = 0
		next
	next
end sub
sub NNetwork.ClearDeltaSumm()
	for n as long =0 to this.NRc - 1
		for m as long = 0 to this.NR[n].nc-1
			this.NR[n].delta_speed[m] = 0
		next
	next
end sub

function NNetwork.Out(out_layer as long = -1) as Neuron ptr
	if out_layer=-1 then 
		return @this.NR[this.layer_i[this.layers-1]]
	else
		return @this.NR[this.layer_i[out_layer]]
	end if
end function

sub NNetwork.Copy(dst as NNetwork)
	for n as long =0 to dst.NRc-1
		memcpy dst.NR[n].W, this.NR[n].W, dst.NR[n].nc*sizeof(single)
		dst.NR[n].mem = this.NR[n].mem
	next
end sub

sub NNetwork.AvgWith(src as NNetwork)
	for n as long =0 to this.NRc-1
		for m as long = 0 to this.NR[n].nc - 1
			this.NR[n].W[m] = (this.NR[n].W[m] + src.NR[n].W[m])*0.5		
		next
	next
end sub



sub NNetwork.Randomize(v1 as single, v2 as single, memr as single=0)
	for j as long = 0 to this.NRc-1
		for k as long = 0 to this.NR[j].nc-1
			this.NR[j].W[k]  =  v1 + rnd*(v2-v1)
		next 
	next
	if this.memory then 
		for j as long = 0 to this.NRc-1
			this.NR[j].mem = rnd*memr
		next
	end if
end sub

sub NNetwork.LimitWeights(l1 as long, l2 as long, v1 as single=-2, v2 as single=2)
	dim as long n
	for l as long = l1 to l2
		for n as long = this.layer_i[l] to this.layer_i[l]+this.layer[l]-1
			for m as long = 0 to this.NR[n].nc-1
				if this.NR[n].W[m]<v1 then this.NR[n].W[m]=v1
				if this.NR[n].W[m]>v2 then this.NR[n].W[m]=v2
			next		
		next	
	next
end sub

sub NNetwork.LimitDeltaSpeed(l1 as long, l2 as long, v1 as single=-2, v2 as single=2)
	dim as long n
	for l as long = l1 to l2
		for n as long = this.layer_i[l] to this.layer_i[l]+this.layer[l]-1
			for m as long = 0 to this.NR[n].nc-1
				if this.NR[n].delta_speed[m]<v1 then this.NR[n].delta_speed[m]=v1
				if this.NR[n].delta_speed[m]>v2 then this.NR[n].delta_speed[m]=v2
			next		
		next	
	next
end sub


sub NNetwork.Destroy()
	for n as long =0 to this.NRc-1
		for m as long =0 to this.NR[n].nc-1
			if this.NR[n].W then 			deallocate this.NR[n].W
			if this.NR[n].L then 			deallocate this.NR[n].L
			if this.NR[n].delta then 		deallocate this.NR[n].delta
			if this.NR[n].delta_speed then 	deallocate this.NR[n].delta_speed
			if this.NR then 				deallocate this.NR
		next
	next 
end sub

sub NNetwork.Create(nnlayer() as long, nnlayers as long, bias as long=0, mem as long=0, nolinks as long = 0)
	this.layers = nnlayers
	this.layer = allocate(sizeof(long)*nnlayers)
	this.layer_i = allocate(sizeof(long)*nnlayers)
	
	dim as long 	c=0
	for n as long =0 to nnlayers-1
		this.layer[n] = nnlayer(n)
		this.layer_i[n] = c
		c += nnlayer(n)
	next
	
	dim as long p=0,pn,i=0
    this.NRc = c
    this.NR = callocate(this.NRc, sizeof(Neuron))
    this.bias = bias
	this.memory = mem
	for n as long = 0 to nnlayers-1
		if nolinks =0 then
			i = this.layer_i[n]
			for m as long =0 to nnlayer(n)-1
				
				if n< nnlayers-1 then
					' links to the next layer
					if this.bias then
						this.NR[i].nc = nnlayer(n+1)-1
					else
						this.NR[i].nc = nnlayer(n+1)
					end if
					this.NR[i].L = Callocate(this.NR[i].nc, sizeof(Long))
					this.NR[i].w = Callocate(this.NR[i].nc, sizeof(Single))
					this.NR[i].delta = Callocate(this.NR[i].nc, sizeof(Single))
					this.NR[i].delta_speed = Callocate(this.NR[i].nc, sizeof(Single))
					
					pn = this.layer_i[n+1]
					if this.bias then

						for k as long = 0 to this.NR[i].nc - 1
							this.NR[i].L[k] = pn+k
						next
					else

						for k as long = 0 to this.NR[i].nc - 1
							this.NR[i].L[k] = pn+k
						next
					end if
				else
					this.NR[i].nc =0
				end if
				i+=1
			next
		end if
	next
end sub

sub NNetwork.PassErrorFrom( src as NNetwork, weights as single ptr)
	' weights count should be = to Neurons
	' this.layer[this.layers-1] = src.layer[0]
	
	dim as long nc = this.layer[this.layers-1]
	if this.bias then nc -= 1

	' calc err in last layer
	dim as long i = this.layer_i[this.layers-1]
	for n as long = 0 to nc-1
		
		' integral of error
		this.NR[i].Er = weights[n] * src.NR[src.layer_i[0]+n].Er
		i+=1
	next
	
'	' normalazing
'	dim as double hh,df
'	i = this.layer_i[this.layers-1]
'	for n as long = 0 to nc-1
'		df += this.NR[i].Er*this.NR[i].Er
'		i+=1
'	next
'	hh = sqr(df)
	
'	if hh>1 then 
'	i = this.layer_i[this.layers-1]
'	for n as long = 0 to nc-1
'		this.NR[i].Er /= hh
'		'? this.NR[i].Er
'		i+=1
'	next
'	end if
	
end sub

sub NNetwork.BackPropogation(targets as single ptr, norm_err as long = 0, layers_back as long = 999, no_gradient as long =0, include_layer0 as long=0, noerr as long = 0, gradientW as single = 1)
	dim as single a = 0.1
	dim as long lnkc, i, l
	lnkc = this.layer[this.layers-1]
	if this.bias then lnkc -= 1
	
	this.gradW += gradientW
	
	if noerr=0 then 
		i = this.layer_i[this.layers-1]
		for n as long = 0 to lnkc-1
			this.NR[i].Er = targets[n] - this.NR[i].signal
			i+=1
		next
	end if
	
	dim as long lb = 0
	for k as long = this.layers-2 to 0 step -1
		if no_gradient=0 then 
			' calc gradient 
			lnkc = this.layer[k+1]
			if this.bias then lnkc -= 1
			for n as long = 0 to lnkc-1
				i = this.layer_i[k+1]+n
				'this.NR[i].Gr = this.NR[i].Er * NNActInv(this.NR[i].signal)
				this.NR[i].Gr = this.NR[i].Er * cast(nnmaskfnc, this.acs_fncder_ptr[k+1])(this.NR[i].signal) * gradientW
			next
			
			' weights deltas
			lnkc = this.layer[k]
			for n as long = 0 to lnkc-1
				i = this.layer_i[k]+n
				for m as long = 0 to this.NR[i].nc-1
					l = this.NR[i].L[m]
					this.NR[i].delta[m] += this.NR[l].Gr * this.NR[i].signal
				next
			next
		end if
		
		' ???? Is it needed for the first layer?
		lnkc = this.layer[k]
		if k>0 or include_layer0=1 then 
			if this.bias then lnkc -= 1
			for n as long = 0 to lnkc-1
				i = this.layer_i[k]+n
				
				' integral of error
				this.NR[i].Er = 0
				for m as long = 0 to this.NR[i].nc-1
					l = this.NR[i].L[m]
					this.NR[i].Er += this.NR[i].W[m] * this.NR[l].Er
				next
				' do we need to avg this?
				if norm_err then this.NR[i].Er /= this.NR[i].nc
			next
		end if
		
		lb+=1
		if lb>=layers_back then exit for
	next

end sub

sub NNetwork.GradientDescent(learn_rate as single=0.1, inertia_k as single=0, layers_back as long = 999)
	if this.gradW = 0 then return 
	dim as double ik = learn_rate / this.gradW
	dim as long start_l
	start_l = this.layers - 1 - layers_back
	if start_l<0 then start_l=0
	for l as long = start_l to this.layers - 1
		for n as long = this.layer_i[l] to this.layer_i[l]+this.layer[l]-1
			for m as long = 0 to this.NR[n].nc-1
				this.NR[n].delta_speed[m] = this.NR[n].delta_speed[m]*inertia_k  + this.NR[n].delta[m]*ik
				this.NR[n].W[m] += this.NR[n].delta_speed[m]
				'if abs(this.NR[n].delta[m])>0.2 then this.NR[n].delta[m] = 0.2*sgn(this.NR[n].delta[m])
				'this.NR[n].W[m] += this.NR[n].delta[m]*ik
			next		
		next
	next
end sub


sub NNetwork.Save(filename as string)
	dim as string tp = "NNET"
	dim as long ver=nnetwork_version
	dim as long ff=freefile
	if len(dir(filename)) then kill(filename)
	open filename for binary as #ff
		put #ff,1, tp
		put #ff, , ver
		put #ff, , this.bias
		put #ff, , this.memory
		
		' layers
		put #ff, , this.layers
		for n as long = 0 to this.layers -1
			put #ff,,this.layer[n]
		next
		
		' neurons
		put #ff, , this.NRc
		for n as long = 0 to this.NRc-1
			put #ff, , this.NR[n].nc
			for m as long = 0 to this.NR[n].nc-1
				put #ff, ,this.NR[n].L[m]
				put #ff, ,this.NR[n].W[m]
			next
		next
		if this.memory then
			for n as long = 0 to this.NRc-1
				put #ff, , this.NR[n].mem
			next	
		end if
		
		for n as long = 0 to this.layers - 1
			put #ff, , layer_a[n]
		next
		
	close #ff
end sub

function NNetwork.Load(filename as string) as long 
	dim as string tp= space(4)
	dim as long vers
	
	if len(dir(filename))=0 then return 1
	
	dim as long ff=freefile
	open filename for binary as #ff
		get #ff,1, tp
		if tp<>"NNET" then close #ff: return 2
		get #ff, , vers
		if vers<nnetwork_min_version then close #ff: return 3
		
		get #ff, , this.bias
		get #ff, , this.memory
		
		' layers
		get #ff, , this.layers
		if this.layer then deallocate this.layer
		if this.layer_i then deallocate this.layer_i
		this.layer = allocate(sizeof(long)*this.layers)
		this.layer_i = allocate(sizeof(long)*this.layers)
		dim as long p = 0
		for n as long = 0 to this.layers -1
			get #ff, ,this.layer[n]
			this.layer_i[n] = p
			p += this.layer[n]
		next

		' Neurons
		get #ff, , this.NRc
		if this.NR then deallocate(this.NR)
		this.NR = callocate(this.NRc, sizeof(Neuron))
		'clear *cast(ubyte ptr, this.NR), 0, sizeof(Neuron)*this.NRc
		
		for n as long = 0 to this.NRc-1
			get #ff, , this.NR[n].nc

			if this.NR[n].L then deallocate(this.NR[n].L)
			if this.NR[n].W then deallocate(this.NR[n].W)
			if this.NR[n].delta then deallocate(this.NR[n].delta)
			if this.NR[n].delta_speed then deallocate(this.NR[n].delta_speed)
			
			this.NR[n].L = Callocate(this.NR[n].nc, sizeof(Long))
			this.NR[n].w = Callocate(this.NR[n].nc, sizeof(Single))			
			this.NR[n].delta = Callocate(this.NR[n].nc, sizeof(Single))		
			this.NR[n].delta_speed = Callocate(this.NR[n].nc, sizeof(Single))	
			
			for m as long = 0 to this.NR[n].nc-1
				get #ff, ,this.NR[n].L[m]
				get #ff, ,this.NR[n].W[m]
			next
		next
		if this.memory then
			for n as long = 0 to this.NRc-1
				get #ff, , this.NR[n].mem
			next			
		end if
		
		if vers>=nnetwork_min_version then
			if this.layer_a then deallocate(this.layer_a)
			if this.acs_fnc_ptr then deallocate(this.acs_fnc_ptr)
			if this.acs_fncder_ptr then deallocate(this.acs_fncder_ptr)
			this.layer_a = callocate( this.layers, sizeof(long) )
			this.acs_fnc_ptr = callocate( this.layers, sizeof(integer) )
			this.acs_fncder_ptr = callocate( this.layers, sizeof(integer) )			
			for n as long = 0 to this.layers -1
				get #ff, , this.layer_a[n]
				this.Activation( this.layer_a[n], n)
			next
		end if
		
	close #ff

	return 0
end function


function NNetwork.AddNeuron(sel_layer as long, rnd_weights as single = 0.001) as long
	
	this.NRc += 1
	this.NR = reallocate( this.NR, this.NRc * sizeof(Neuron) )
	
	dim as long ni = this.layer_i[sel_layer] + this.layer[sel_layer] - this.bias
	' shift neurons
	for n as long = this.NRc-1 to ni+1 step -1
		this.NR[n] = this.NR[n-1]		
	next
	for l as long = sel_layer+1 to this.layers-1
		this.layer_i[l] += 1
	next
	this.NR[ni].nc =0
	' +1 to this layer
	this.layer[sel_layer] += 1

	' +1 to all links ahead
	for n as long = this.layer_i[sel_layer] to this.NRc-1
		for l as long = 0 to this.NR[n].nc-1
			this.NR[n].L[l] += 1
		next
	next
	
	' Creating links from this neuron
	if sel_layer+1 < this.layers then
		this.NR[ni].nc = this.layer[sel_layer+1]-this.bias
		this.NR[ni].L = Callocate(this.NR[ni].nc, sizeof(Long))
		this.NR[ni].w = Callocate(this.NR[ni].nc, sizeof(Single))
		this.NR[ni].delta = Callocate(this.NR[ni].nc, sizeof(Single))
		this.NR[ni].delta_speed = Callocate(this.NR[ni].nc, sizeof(Single))
		for k as long = 0 to this.NR[ni].nc - 1
			this.NR[ni].L[k] = this.layer_i[sel_layer+1] +k
			'this.NR[ni].W[k] = rnd*rnd_weights
		next
	end if
	
	' links form prev layer
	if sel_layer then 
		for n as long = this.layer_i[sel_layer-1] to this.layer_i[sel_layer-1]+this.layer[sel_layer-1] - 1
			this.NR[n].nc+=1
			
			this.NR[n].L = reallocate(this.NR[n].L, this.NR[n].nc*sizeof(Long))
			this.NR[n].w = reallocate(this.NR[n].w, this.NR[n].nc*sizeof(Single))
			this.NR[n].delta = reallocate(this.NR[n].delta, this.NR[n].nc*sizeof(Single))
			this.NR[n].delta_speed = reallocate(this.NR[n].delta_speed, this.NR[n].nc*sizeof(Single))
			
			this.NR[n].L[this.NR[n].nc-1] = ni
			this.NR[n].W[this.NR[n].nc-1] = rnd*rnd_weights
			this.NR[n].delta[this.NR[n].nc-1] = 0
			this.NR[n].delta_speed[this.NR[n].nc-1] = 0
		next
	end if
	
	
	return(ni)
end function






function NNetwork.CountWeights() as long
	dim as long c=0
	for n as long = 0 to this.NRc-1
		c += this.NR[n].nc
	next
	return c
end function

