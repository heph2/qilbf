# qilbf

qilbf is a simple brainfuck compiler written in F#, for learning pourposes.
It outputs QBE intermediate language and then can be compiled to assembly (aarch64 and amd64).

Actually compile something using .NET is beyond my capacity.. 
So you've to try qilbf using `dotnet run`

Usage:

	$ dotnet run examples/mandlebrot.bf > mandlebrot.ssa
	$ qbe mandlebrot.ssa > mandlebrot.s
	$ cc -o mandlebrot mandlebrot.s && ./mandlebrot

