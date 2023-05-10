#include once "randomizer_class.bas"
#include once "crt.bi"
#include once "crt/math.bi"

Enum NeuronActivation
	SAME
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

	as integer ptr		acs_fnc_ptr
	as integer ptr		acs_fncder_ptr
	
	declare sub 		Create(layer() as long, layers as long, bias as long=0, mem as long=0, nolinks as long = 0)
	declare sub 		Activation(act as NeuronActivation)
	declare sub 		Destroy()
	declare sub 		Clear()
	declare sub 		ClearDelta()
	declare sub 		ClearDeltaSumm()
	declare sub 		Tick()
	declare sub 		Randomize(v1 as single, v2 as single, memr as single=0)
	declare sub 		LimitWeights(l1 as long, l2 as long, v1 as single=-2, v2 as single=2)
	declare sub 		Copy( dst as NNetwork)
	declare sub 		Crossing(byref b1 as NNetwork, byref  b2 as NNetwork, chance as single=0.03, mutaion as double=0.0)
	declare sub 		Round(offset as single = 0.00001)
	declare sub 		Mutate(mutaion as single=0.0)
	declare sub 		Interpolate(byref b1 as NNetwork, byref b2 as NNetwork, k as single)
	declare sub 		BackPropogation(targets as single ptr, norm_err as long = 0, layers_back as long = 999)
	declare sub 		GradientDescent(sample_count as long=1, learn_rate as single=0.1, inertia_k as single=0, layers_back as long = 999)
	declare function 	Out() as Neuron ptr
	declare sub  		Save(filename as string)
	declare function 	Load(filename as string) as long



	declare function 	AddNeuron(sel_layer as long, rnd_weights as single = 0.001) as long
	
	declare sub  		SimplifyWeights(k as single=0.1)
	declare function  	CountWeights() as long	
End Type



type nnmaskfnc as function(x as single) as single

CONST as long nnetwork_version = 3

dim shared as Randomizer grnd = 1048576


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
    if x < 0 then
        return 0.01 * x
    else
        return x
    end if
end function
function nns_lReLU_der(x as single) as single
    if x < 0 then
        return 0.01
    else
        return 1
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

sub NNetwork.Activation(act as NeuronActivation)
	select case act
	case SAME
		this.acs_fnc_ptr = cast(integer ptr, @nns_same)
		this.acs_fncder_ptr = cast(integer ptr, @nns_same_der)
	case RELU
		this.acs_fnc_ptr = cast(integer ptr, @nns_ReLU)
		this.acs_fncder_ptr = cast(integer ptr, @nns_ReLU_der)
	case lRELU
		this.acs_fnc_ptr = cast(integer ptr, @nns_lReLU)
		this.acs_fncder_ptr = cast(integer ptr, @nns_lReLU_der)
	case nRELU
		this.acs_fnc_ptr = cast(integer ptr, @nns_nReLU)
		this.acs_fncder_ptr = cast(integer ptr, @nns_nReLU_der)
	case TANHENT
		this.acs_fnc_ptr = cast(integer ptr, @nns_tanh)
		this.acs_fncder_ptr = cast(integer ptr, @nns_tanh_der)
	case SIGMOID
		this.acs_fnc_ptr = cast(integer ptr, @nns_sigmoid)
		this.acs_fncder_ptr = cast(integer ptr, @nns_sigmoid_der)
	end select
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
		next
		if this.memory then
			this.NR[n].mem += (rnd-0.5)*mutaion
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
			if grnd.Value<0.5 then
				this.NR[n].W[m] = b1.NR[n].W[m]
			else
				this.NR[n].W[m] = b2.NR[n].W[m]
			end if
			if grnd.Value<chance then
				this.NR[n].W[m] = cdbl(this.NR[n].W[m]) + (grnd.Value-0.5)*mutaion
			end if
		next
		if this.memory then
			if grnd.Value<0.5 then
				this.NR[n].mem = b1.NR[n].mem
			else
				this.NR[n].mem = b2.NR[n].mem
			end if
			if grnd.Value<chance then
				this.NR[n].mem += (grnd.Value-0.5)*mutaion
			end if
		end if
	next 
end sub

Sub NNetwork.Tick()
	dim as long i = 0, oi, k
	dim as single t
	
	if this.memory then 
		for l as long =0 to this.layers-2
			for n as long = 0 to this.layer[l] -1
				for m as long = 0 to this.NR[i].nc-1
					oi = this.NR[i].L[m]
					this.NR[oi].tmp_signal += this.NR[i].signal * this.NR[i].W[m]
				next
				i +=1
			next
			k = this.layer_i[l+1]
			for n as long = 0 to this.layer[l+1] - iif(this.bias,2,1)
				'this.NR[k].signal = this.NR[k].signal*this.NR[k].mem + NNAct(this.NR[k].tmp_signal)*(1-this.NR[k].mem)
				this.NR[k].signal = this.NR[k].signal*this.NR[k].mem + cast(nnmaskfnc, this.acs_fnc_ptr)(this.NR[k].tmp_signal)*(1-this.NR[k].mem)
				k+=1
			next
		next
	else
		for l as long =0 to this.layers-2
			for n as long = 0 to this.layer[l] -1
				for m as long = 0 to this.NR[i].nc-1
					oi = this.NR[i].L[m]
					this.NR[oi].signal += this.NR[i].signal * this.NR[i].W[m]
				next
				i +=1
			next
			k = this.layer_i[l+1]
			for n as long = 0 to this.layer[l+1] - iif(this.bias,2,1)
				'this.NR[k].signal = NNAct(this.NR[k].signal)
				this.NR[k].signal = cast(nnmaskfnc, this.acs_fnc_ptr)(this.NR[k].signal)
				k+=1
			next
		next
	end if
	
End Sub

sub NNetwork.Clear()
	dim i as long=this.layer_i[0]
	for n as long =0 to this.layers-1
		if this.memory then 
			for m as long=0 to this.layer[n] -1
				this.NR[i].tmp_signal = 0
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

function NNetwork.Out() as Neuron ptr
	return @this.NR[this.layer_i[this.layers-1]]
end function

sub NNetwork.Copy(dst as NNetwork)
	for n as long =0 to dst.NRc-1
		memcpy dst.NR[n].W, this.NR[n].W, dst.NR[n].nc*sizeof(single)
		dst.NR[n].mem = this.NR[n].mem
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


sub NNetwork.BackPropogation(targets as single ptr, norm_err as long = 0, layers_back as long = 999)
	dim as single a = 0.1
	dim as long lnkc, i, l
	lnkc = this.layer[this.layers-1]
	if this.bias then lnkc -= 1
	i = this.layer_i[this.layers-1]
	for n as long = 0 to lnkc-1
		this.NR[i].Er = targets[n] - this.NR[i].signal
		i+=1
	next
	dim as long lb = 0
	for k as long = this.layers-2 to 0 step -1
		' calc gradient 
		lnkc = this.layer[k+1]
		if this.bias then lnkc -= 1
		for n as long = 0 to lnkc-1
			i = this.layer_i[k+1]+n
			'this.NR[i].Gr = this.NR[i].Er * NNActInv(this.NR[i].signal)
			this.NR[i].Gr = this.NR[i].Er * cast(nnmaskfnc, this.acs_fncder_ptr)(this.NR[i].signal)
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
		
		' ???? Is it needed for the first layer?
		if k>0 then 
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

sub NNetwork.GradientDescent(sample_count as long=1, learn_rate as single=0.1, inertia_k as single=0, layers_back as long = 999)
	dim as single ik, k
	ik = learn_rate / sample_count
	dim as long start_l
	start_l = this.layers - 1 - layers_back
	if start_l<0 then start_l=0
	for l as long = start_l to this.layers - 1
		for n as long = this.layer_i[l] to this.layer_i[l]+this.layer[l]-1
			for m as long = 0 to this.NR[n].nc-1
				this.NR[n].delta_speed[m] = this.NR[n].delta_speed[m]*inertia_k  + this.NR[n].delta[m]*ik
				this.NR[n].W[m] += this.NR[n].delta_speed[m]
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
		if vers<>nnetwork_version then close #ff: return 3
		
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




sub	NNetwork.SimplifyWeights(k as single=0.1)
	dim as long ptr NrnInCnt = Callocate( this.NRc,  4)
	for n as long = 0 to this.NRc-1
		for l as long = 0 to this.NR[n].nc -1
			NrnInCnt[this.NR[n].L[l]] += 1
		next
	next

	for n as long = 0 to this.NRc-1
		
		dim as double avg=0
		for l as long = 0 to this.NR[n].nc -1
			avg += abs(this.NR[n].W[l])
		next
		avg /= this.NR[n].nc
		avg *= k
		
		dim as long i = 0
		for l as long = 0 to this.NR[n].nc -1
			if abs(this.NR[n].W[l]) > avg or i<1 or NrnInCnt[this.NR[n].L[i]]<2 then
				' keep this
				this.NR[n].W[i] = this.NR[n].W[l]
				this.NR[n].L[i] = this.NR[n].L[l]
				i += 1
			else
				NrnInCnt[this.NR[n].L[i]] -= 1
			end if
		next
		this.NR[n].nc = i
		
	next
	
	deallocate(NrnInCnt)
end sub



function NNetwork.CountWeights() as long
	dim as long c=0
	for n as long = 0 to this.NRc-1
		c += this.NR[n].nc
	next
	return c
end function
