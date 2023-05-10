

type Randomizer
	as double ptr rndval=0
	as long index
	as long maxindex
	
	declare sub init(count as long)
	declare sub seed(v as long)
	declare function Value() as double
	
	Declare Destructor()
	Declare Constructor()
	Declare Constructor(count as long)
end type


Destructor Randomizer()
	if this.rndval then deallocate(this.rndval)
	this.index = 0
	this.maxindex = 0
end destructor

Constructor Randomizer()
	randomize 0
	this.maxindex = 4096
	this.index = 0
	this.rndval = Callocate( this.maxindex, sizeof(double) )
	for n as long = 0 to this.maxindex-1
		this.rndval[n] = rnd
	next
end constructor
Constructor Randomizer(count as long)
	randomize 0
	this.maxindex = count
	this.index = 0
	this.rndval = Callocate( this.maxindex, sizeof(double) )
	for n as long = 0 to this.maxindex-1
		this.rndval[n] = rnd
	next
end constructor



sub Randomizer.init(count as long)
	randomize 0
	this.maxindex = count
	this.index = 0
	this.rndval = Callocate( this.maxindex, sizeof(double) )
	for n as long = 0 to count-1
		this.rndval[n] = rnd
	next
end sub

sub Randomizer.seed(i as long)
	this.index = i mod this.maxindex
end sub

function Randomizer.value() as double
	this.index+=1
	if this.index=this.maxindex then this.index=0
	'? "i " & this.index
	return this.rndval[this.index]
end function

''var	r = Randomizer(1)
''dim as Randomizer r = Randomizer(3)
'dim as Randomizer r

'for n as long = 0 to 9
'	? r.Value
'next

'sleep 
