all: bench

bench: finger_radix.o bench.c gperf_lookup.c
	$(CC) -O2 bench.c $< -o $@

finger_radix.o: finger_radix.c
	$(CC) -c finger_radix.c

finger_radix.c: input
	../dist/build/hctrie/hctrie --prefix finger --headers '<stdint.h>' input

gperf_lookup.c: input
	gperf -L ANSI-C -GCD input > gperf_lookup.c

clean:
	$(RM) bench
	$(RM) finger*
	$(RM) gperf_lookup.c
	$(RM) *.o
