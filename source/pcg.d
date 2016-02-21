
/++
 + A D port of the PCG pseudorandom number generator at http://pcg-random.org
 +
 + Most of the functionality is tested against the PCG C++ implementation to ensure
 + that the output matches. That is, the D generators should produce the same output as the
 + C++ ones. The exception is default-initialized, unseeded generators, due to D not allowing
 + a default constructor on structs.
 +
 + 64-bit generators are not supported yet, as they require the (currently unsupported)
 + 128-bit integer types that D has not yet implemented.
 +
 + Unique generators are also not supported. Seed with std.random.unpredictableSeed instead.
++/
module pcg;

import std.traits;
import std.typetuple;
version(unittest) {
	import std.range;
	import std.algorithm;
}

private {
	// Type to use for bit counts.
	alias bits_t = size_t;
	
	
	// Generator constants. Depends on the integer type. 
	template default_multiplier(T) { static assert(false); }
	template default_increment(T) { static assert(false); }

	enum ubyte  default_multiplier(T : ubyte)  = 141U;
	enum ushort default_multiplier(T : ushort) = 12829U;
	enum uint   default_multiplier(T : uint)   = 747796405U;
	enum ulong  default_multiplier(T : ulong)  = 6364136223846793005UL;

	enum ubyte  default_increment(T : ubyte)  = 77U;
	enum ushort default_increment(T : ushort) = 47989U;
	enum uint   default_increment(T : uint)   = 2891336453U;
	enum ulong  default_increment(T : ulong)  = 1442695040888963407UL;
	
	unittest {
		static assert(default_multiplier!ubyte == 141);
		static assert(default_multiplier!ushort == 12829);
		static assert(!__traits(compiles, default_multiplier!string));
	}
	
	template mcg_multiplier(T) { static assert(false); }
	template mcg_unmultiplier(T) { static assert(false); }
	
	enum ubyte  mcg_multiplier(T : ubyte)  = 217U;
	enum ushort mcg_multiplier(T : ushort) = 62169U;
	enum uint   mcg_multiplier(T : uint)   = 277803737U;
	enum ulong  mcg_multiplier(T : ulong)  = 12605985483714917081UL;
	
	enum ubyte  mcg_unmultiplier(T : ubyte)  = 105U;
	enum ushort mcg_unmultiplier(T : ushort) = 28009U;
	enum uint   mcg_unmultiplier(T : uint)   = 2897767785U;
	enum ulong  mcg_unmultiplier(T : ulong)  = 15009553638781119849UL;
	
	template HalfSize(T) { static assert(false, "no half size for type "~T.stringof); }
	alias HalfSize(T : ushort) = ubyte;
	alias HalfSize(T : uint) = ushort;
	alias HalfSize(T : ulong) = uint;
	
	T rotr(T)(T val, bits_t shift)
	if(isUnsigned!T) {
		// TODO: implement in ASM
		enum bits = T.sizeof*8;
		enum mask = bits-1;
		
		return cast(T)(val >> shift) | cast(T)(val << ((-shift) & mask));
	}
	
	unittest {
		assert(rotr!ubyte(1<<1, 1) == 1);
		assert(rotr!ubyte(1<<5, 2) == 1<<3);
		assert(rotr!ubyte(1, 3) == 1<<5);
		
		assert(rotr!ubyte(3<<6, 2) == 3<<4);
		assert(rotr!ubyte(0xF0, 4) == 0x0F);
	}
}

/+ ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
 + Generator variations.
 +
 + Unique is not supported; using `this` as an lvalue in D is deprecated.
 + Use std.random.unpredictableSeed instead.
++/

/++
 + MCG Generator.
 + 
 + No multi-stream support. Slightly faster, but smaller period.
++/
mixin template MCG(StateType) {
	enum VariationName = "MCG";
	enum ulong Period_pow2 = StateType.sizeof*8 - 2;
	enum ulong MaxStream = 0;
	enum stream = 0;
	private enum StateType increment = 0;
}

/++
 + OneSeq Generator.
 +
 + No multi-stream support.
++/
mixin template OneSeq(StateType) {
	enum VariationName = "OneSeq";
	enum ulong Period_pow2 = StateType.sizeof*8;
	enum ulong MaxStream = 0;
	enum stream = 0;
	private enum StateType increment = default_increment!StateType;
}

/++
 + SetSeq Generator.
 +
 + Has multi-stream support.
++/
mixin template SetSeq(StateType) {
	enum VariationName = "SetSeq";
	enum ulong Period_pow2 = StateType.sizeof*8;
	enum ulong MaxStream = StateType.max >> 1;
	
	private StateType stream_inc = default_increment!StateType;
	
	private StateType increment() const pure nothrow @nogc @property {
		return stream_inc;
	}
	StateType stream(StateType seq) pure nothrow @nogc @property {
		return stream_inc = cast(StateType) (seq << 1) | 1;
	}
	StateType stream() const pure nothrow @nogc @property {
		return stream_inc >> 1;
	}
}

unittest {
	void CheckVariation(alias Variation, T)() {
		static struct Foo {
			mixin Variation!(T);
		}
	}
	
	CheckVariation!(MCG, ubyte);
	CheckVariation!(MCG, ushort);
	CheckVariation!(MCG, uint);
	CheckVariation!(MCG, ulong);
	
	CheckVariation!(OneSeq, ubyte);
	CheckVariation!(OneSeq, ushort);
	CheckVariation!(OneSeq, uint);
	CheckVariation!(OneSeq, ulong);
	
	CheckVariation!(SetSeq, ubyte);
	CheckVariation!(SetSeq, ushort);
	CheckVariation!(SetSeq, uint);
	CheckVariation!(SetSeq, ulong);
}

/++
 + The main PCG engine.
 +
 + It takes several configurable parameters.
++/
struct PCGEngine(
	ResultType,
	StateType,
	alias OutputFunc,
	bool OutputPrevious=true,
	alias StreamTypeMixin=OneSeq,
	StateType Multiplier=default_multiplier!StateType
) {
	static assert(isUnsigned!ResultType, "ResultType must be an unsigned integer, not "~ResultType.stringof);
	static assert(isUnsigned!StateType, "StateType must be an unsigned integer, not "~StateType.stringof);
	static assert(is(typeof(OutputFunc!(StateType, ResultType))), "Invalid OutputFunc used with PCGEngine");
	
	mixin StreamTypeMixin!StateType;
	
public:
	enum isUniformRandom = true;
	enum min = ResultType.min;
	enum max = ResultType.max;
	enum empty = false;
	
	/++
	 + Creates and seeds a new RNG.
	++/
	this(StateType seed) pure {
		this.seed(seed);
	}
	
	static if(MaxStream > 0) {
		/++
		 + Creates and seeds a new RNG, at the specified stream.
		 + Only available if using the SetSeq variation.
		++/
		this(StateType seed, StateType stream) pure {
			this.seed(seed, stream);
		}
	}
	
	///
	ResultType front() @property const pure nothrow @nogc {
		return _front;
	}
	
	///
	void popFront() pure nothrow @nogc {
		_front = OutputFunc!(StateType, ResultType)(base_generate());
	}
	
	///
	inout(typeof(this)) save() inout pure nothrow @nogc {
		return this;
	}
	
	private void advance(StateType amount) {
		StateType accMult = 1;
		StateType accPlus = 0;
		auto curMult = Multiplier;
		auto curPlus = increment;
		
		while(amount > 0) {
			if(amount & 1) {
				accMult *= curMult;
				accPlus = accPlus*curMult + curPlus;
			}
			curPlus = (curMult+1)*curPlus;
			curMult *= curMult;
			amount >>= 1;
		}
		state = accMult * state + accPlus;
	}
	
	/// Advances the RNG faster than calling popFrontN multiple times.
	void popFrontN(StateType amount) {
		if(amount == 0)
			return;
		advance(amount-1);
		popFront();
	}
	
	/// Seeds the RNG.
	void seed(StateType seed) pure nothrow @nogc {
		static if(VariationName == "MCG")
			state = seed | 3;
		else
			state = bump(seed + increment);
		this.popFront();
	}
	
	static if(MaxStream > 0) {
		/++
		 + Seeds the RNG and sets the stream.
		 + Only available if using the SetSeq variation.
		++/
		void seed(StateType seed, StateType stream) {
			this.stream = stream;
			this.seed(seed);
		}
	}
	
private:
	StateType state = cast(StateType) 0xcafef00dd15ea5e5UL;
	ResultType _front;
	
	StateType bump(StateType state) const pure nothrow @nogc {
		return state * Multiplier + increment;
	}
	
	StateType base_generate() pure nothrow @nogc {
		static if(OutputPrevious) {
			StateType oldState = state;
			state = bump(state);
			return oldState;
		} else {
			return state = bump(state);
		}
	}
}

/++
 + Pre-defined PCG generators, matching the original source code's definitions.
 + 
 + PCG32 support multiple streams, PCG32OneSeq and PCG32Fast do not. PCG32Fast is slightly faster, but has a shorter period.
++/
alias PCG32 = PCGEngine!(uint, ulong, xsh_rr, true, SetSeq);
/// ditto
alias PCG32OneSeq = PCGEngine!(uint, ulong, xsh_rr, true, OneSeq);
/// ditto
alias PCG32Fast = PCGEngine!(uint, ulong, xsh_rs, true, MCG);

unittest {
	import std.random;
	static assert(isUniformRNG!(PCG32, uint));
	static assert(isSeedable!(PCG32, ulong));
}
unittest {
	auto gen = PCG32(12345);
	immutable testVector = [1411482639,3165192603,3360792183,2433038347,628889468,3778631550,2430531221,2504758782,1116223725,3013600377,];
	assert(gen.take(testVector.length).equal(testVector));
}
unittest {
	auto gen = PCG32(12345, 678);
	immutable testVector = [3778649606,938063991,2368796250,1689035216,3455663708,3248344731,1831696548,608339070,2791141168,1236841111,];
	assert(gen.take(testVector.length).equal(testVector));
}
unittest {
	auto gen = PCG32OneSeq(12345);
	immutable testVector = [1411482639,3165192603,3360792183,2433038347,628889468,3778631550,2430531221,2504758782,1116223725,3013600377,];
	assert(gen.take(testVector.length).equal(testVector));
}
unittest {
	auto gen = PCG32Fast(12345);
	immutable testVector = [0,360236480,3576984158,3399133164,3953016716,1075407150,134550159,1121230050,697817023,1550232051,];
	assert(gen.take(testVector.length).equal(testVector));
}

/+ ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
 + Output functions.
 +
 + Each function has a unit test that compares it to some hardcoded randomly-generated
 + values from the C++ versions of the function, to ensure that they work the same way.
++/

private template BitInfo(StateType, ResultType) {
	static assert(isUnsigned!StateType, "Invalid state type");
	static assert(isUnsigned!ResultType, "Invalid result type");
	static assert(StateType.sizeof >= ResultType.sizeof, "Cannot have a state type larger than the result type.");
	
	enum stateBits = StateType.sizeof * 8;
	enum resultBits = ResultType.sizeof * 8;
	enum spareBits = stateBits - resultBits;
}

/++
 + Output function XSH RS -- high xorshift, followed by a random shift
 +
 + Fast. A good performer.
 +/
ResultType xsh_rs(StateType, ResultType)(StateType state) {
	alias bi = BitInfo!(StateType, ResultType);
	enum opBits = bi.spareBits-5 >= 64 ? 5
	            : bi.spareBits-4 >= 32 ? 4
	            : bi.spareBits-3 >= 16 ? 3
	            : bi.spareBits-2 >= 4  ? 2
	            : bi.spareBits-1 >= 1  ? 1
	            :                        0;
	enum mask = (1 << opBits) - 1;
	enum maxRandShift = mask;
	enum topSpare = opBits;
	enum bottomSpare = bi.spareBits - topSpare;
	enum xShift = topSpare + (bi.resultBits + maxRandShift) / 2;
	
	bits_t rshift = opBits ? (state >> (bi.stateBits - opBits)) & mask : 0;
	state ^= state >> xShift;
	return cast(ResultType)(state >> (bottomSpare - maxRandShift + rshift));
}

unittest {
	auto v = xsh_rs!(ulong, uint)(1234UL);
	static assert(is(typeof(v) == uint));
	static assert(!__traits(compiles, xsh_rs!(uint, ulong)(123)));
	
	immutable testVectors = [
		[15067669103579037956UL,296871741UL],
		[10585216671734060556UL,3113159775UL],
		[14465915293962989487UL,2350131006UL],
		[12560359203105191607UL,3387675550UL],
		[16381757648301003838UL,448631329UL],
		[18405880340653979308UL,4218850008UL],
		[15777501264337822631UL,2941200983UL],
		[12749535132123028338UL,502140052UL],
		[15452512430144842739UL,1730516846UL],
		[15214490214938895492UL,843814672UL],
	];
	foreach(vec; testVectors) {
		assert(xsh_rs!(ulong, uint)(vec[0]) == vec[1]);
	}
}

/++
 + Output function XSH RR -- high xorshift, followed by a random rotate
 +
 + Fast.  A good performer.  Slightly better statistically than XSH RS.
++/
ResultType xsh_rr(StateType, ResultType)(StateType state) {
	alias bi = BitInfo!(StateType, ResultType);
	enum wantedOpBits = bi.resultBits >= 128 ? 7
	                  : bi.resultBits >=  64 ? 6
	                  : bi.resultBits >=  32 ? 5
	                  : bi.resultBits >=  16 ? 4
	                  :                        3;
	enum opBits = bi.spareBits > wantedOpBits ? wantedOpBits : bi.spareBits;
	enum amplifier = wantedOpBits - opBits;
	enum mask = (1 << opBits) - 1;
	enum topSpare = opBits;
	enum bottomSpare = bi.spareBits - topSpare;
	enum xShift = (topSpare + bi.resultBits) / 2;
	
	bits_t rot = opBits ? cast(bits_t)(state >> (bi.stateBits - opBits)) & mask : 0;
	bits_t ampRot = cast(bits_t)(rot << amplifier) & mask;
	state ^= state >> xShift;
	return rotr(cast(ResultType)(state >> bottomSpare), ampRot);
}

unittest {
	auto v = xsh_rr!(ulong, uint)(1234UL);
	static assert(is(typeof(v) == uint));
	static assert(!__traits(compiles, xsh_rr!(uint, ulong)(123)));
	
	immutable testVectors = [
		[15067669103579037956UL,3645606856UL],
		[10585216671734060556UL,3592116016UL],
		[14465915293962989487UL,389417868UL],
		[12560359203105191607UL,2008424015UL],
		[16381757648301003838UL,2937207046UL],
		[18405880340653979308UL,3686489923UL],
		[15777501264337822631UL,3541945963UL],
		[12749535132123028338UL,2941149303UL],
		[15452512430144842739UL,2474483187UL],
		[15214490214938895492UL,611820953UL],
	];
	foreach(vec; testVectors) {
		assert(xsh_rr!(ulong, uint)(vec[0]) == vec[1]);
	}
}

/++
 + Output function RXS -- random xorshift
 +/
ResultType rxs(StateType, ResultType)(StateType state) {
	alias bi = BitInfo!(StateType, ResultType);
	enum shift = bi.stateBits - bi.resultBits;
	enum extraShift = (bi.resultBits - shift) / 2;
	bits_t rshift = shift > 64+8 ? (state >> (bi.stateBits - 6)) & 63
	              : shift > 32+4 ? (state >> (bi.stateBits - 5)) & 31
	              : shift > 16+2 ? (state >> (bi.stateBits - 4)) & 15
	              : shift >  8+1 ? (state >> (bi.stateBits - 3)) & 7
	              : shift >  4+1 ? (state >> (bi.stateBits - 2)) & 3
	              : shift >  2+1 ? (state >> (bi.stateBits - 1)) & 1
	              :                0;
	state ^= state >> (shift + extraShift - rshift);
	return cast(ResultType) (state >> rshift);
}

unittest {
	auto v = rxs!(ulong, uint)(1234UL);
	static assert(is(typeof(v) == uint));
	static assert(!__traits(compiles, rxs!(uint, ulong)(123)));
	
	immutable testVectors = [
		[15067669103579037956UL,950461087UL],
		[10585216671734060556UL,2945752657UL],
		[14465915293962989487UL,3721378190UL],
		[12560359203105191607UL,2805810440UL],
		[16381757648301003838UL,611454941UL],
		[18405880340653979308UL,1512285355UL],
		[15777501264337822631UL,1417801921UL],
		[12749535132123028338UL,3826008940UL],
		[15452512430144842739UL,118172308UL],
		[15214490214938895492UL,46684810UL],
	];
	foreach(vec; testVectors) {
		assert(rxs!(ulong, uint)(vec[0]) == vec[1]);
	}
}

/++
 + Outpur function RXS M XS -- random xorshift, mcg multiply, fixed xorshift
 +
 + The most statistically powerful generator, but all those steps
 + make it slower than some of the others.  We give it the rottenest jobs.
 +
 + Because it's usually used in contexts where the state type and the
 + result type are the same, it is a permutation and is thus invertable.
 + We thus provide a function to invert it.  This function is used to
 + for the "inside out" generator used by the extended generator.
 +/
ResultType rxs_m_xs(StateType, ResultType)(StateType state) {
	alias bi = BitInfo!(StateType, ResultType);
	enum opBits = bi.resultBits >= 128 ? 6
	            : bi.resultBits >=  64 ? 5
	            : bi.resultBits >=  32 ? 4
	            : bi.resultBits >=  16 ? 3
	            :                        2;
	enum shift = bi.stateBits - bi.resultBits;
	enum mask = cast(bits_t) (1 << opBits) - 1;
	
	bits_t rshift = opBits ? cast(bits_t)(state >> (bi.stateBits - opBits)) & mask : 0;
	state ^= state >> (opBits + rshift);
	state *= mcg_multiplier!StateType;
	ResultType result = cast(ResultType)(state >> shift);
	result ^= cast(ResultType)(result >> ((2*bi.resultBits+2)/3));
	return result;
}

unittest {
	auto v = rxs_m_xs!(ulong, uint)(1234UL);
	static assert(is(typeof(v) == uint));
	static assert(!__traits(compiles, rxs_m_xs!(uint, ulong)(123)));
	
	immutable testVectors = [
		[15067669103579037956UL,3319069165UL],
		[10585216671734060556UL,357161392UL],
		[14465915293962989487UL,614575156UL],
		[12560359203105191607UL,934180987UL],
		[16381757648301003838UL,490692769UL],
		[18405880340653979308UL,4215630753UL],
		[15777501264337822631UL,575635089UL],
		[12749535132123028338UL,1920410296UL],
		[15452512430144842739UL,2192536769UL],
		[15214490214938895492UL,2986597130UL],
	];
	foreach(vec; testVectors) {
		assert(rxs_m_xs!(ulong, uint)(vec[0]) == vec[1]);
	}
}

/++
 + Output function RXS M -- random xorshift, mcg multiply
++/
ResultType rxs_m(StateType, ResultType)(StateType state) {
	alias bi = BitInfo!(StateType, ResultType);
	enum opBits = bi.resultBits >= 128 ? 6
	            : bi.resultBits >=  64 ? 5
	            : bi.resultBits >=  32 ? 4
	            : bi.resultBits >=  16 ? 3
	            :                        2;
	enum shift = bi.stateBits - bi.resultBits;
	enum mask = (1 << opBits) - 1;
	bits_t rShift = opBits ? (state >> (bi.stateBits - opBits)) & mask : 0;
	state ^= state >> (opBits + rShift);
	state *= mcg_multiplier!StateType;
	return state >> shift;
}

unittest {
	auto v = rxs_m!(ulong, uint)(1234UL);
	static assert(is(typeof(v) == uint));
	static assert(!__traits(compiles, rxs_m!(uint, ulong)(123)));
	
	immutable testVectors = [
		[15067669103579037956UL,3319069434UL],
		[10585216671734060556UL,357161445UL],
		[14465915293962989487UL,614575270UL],
		[12560359203105191607UL,934181029UL],
		[16381757648301003838UL,490692821UL],
		[18405880340653979308UL,4215629900UL],
		[15777501264337822631UL,575634968UL],
		[12749535132123028338UL,1920410481UL],
		[15452512430144842739UL,2192537291UL],
		[15214490214938895492UL,2986596802UL],
	];
	foreach(vec; testVectors) {
		assert(rxs_m!(ulong, uint)(vec[0]) == vec[1]);
	}
}

/++
 + Output function XSL RR -- fixed xorshift (to low bits), random rotate
 +
 + Useful for 128-bit types that are split across two CPU registers.
++/
ResultType xsl_rr(StateType, ResultType)(StateType state) {
	alias bi = BitInfo!(StateType, ResultType);
	enum spareBits = bi.stateBits - bi.resultBits;
	enum wantedOpBits = bi.resultBits >= 128 ? 7
	                  : bi.resultBits >=  64 ? 6
	                  : bi.resultBits >=  32 ? 5
	                  : bi.resultBits >=  16 ? 4
	                  :                        3;
	enum opBits = spareBits > wantedOpBits ? wantedOpBits : spareBits;
	enum amplifier = wantedOpBits - opBits;
	enum mask = (1 << opBits) - 1;
	enum topSpare = spareBits;
	enum bottomSpare = spareBits - topSpare;
	enum xShift = (topSpare + bi.resultBits) / 2;
	
	bits_t rot = opBits ? cast(bits_t)(state >> (bi.stateBits - opBits)) & mask : 0;
	bits_t ampRot = (rot << amplifier) & mask;
	state ^= state >> xShift;
	return rotr(cast(ResultType)(state >> bottomSpare), ampRot);
}

unittest {
	auto v = xsl_rr!(ulong, uint)(1234UL);
	static assert(is(typeof(v) == uint));
	static assert(!__traits(compiles, xsl_rr!(uint, ulong)(123)));
	
	immutable testVectors = [
		[15067669103579037956UL,3146190043UL],
		[10585216671734060556UL,2585632233UL],
		[14465915293962989487UL,2790752147UL],
		[12560359203105191607UL,1599378225UL],
		[16381757648301003838UL,3442106232UL],
		[18405880340653979308UL,2295384084UL],
		[15777501264337822631UL,1520586031UL],
		[12749535132123028338UL,2225559205UL],
		[15452512430144842739UL,424441662UL],
		[15214490214938895492UL,3432492627UL],
	];
	foreach(vec; testVectors) {
		assert(xsl_rr!(ulong, uint)(vec[0]) == vec[1]);
	}
}

// todo: xsl_rr_rr_mixin, xsh_mixin, xsl_mixin

