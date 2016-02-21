D-PCG
=====

The [PCG psuedorandom number generator](http://www.pcg-random.org) ported to D.

Based off of the complete C++ library, though D-isms are used where practical and some features are missing.

The RNGs provided fulfill the contracts specified by `std.random`; that is, `std.random.isUniformRNG!PCG32 == true`, and thus the random number functions in `std.random` can (and should) be used.

Example usage:

```d
import std.stdio;
import std.random;
import pcg;

void main() {
	auto rng = PCG32(unpredictableSeed);
	foreach(i; 1..20) {
		writeln("Random number: ", uniform(1, 6, rng));
	}
}
```

For the complete API, generate documentation using `dub build -d doc`
