all: bench

bench: finger_radix.o bench.c gperf_lookup.c
	$(CC) -O2 bench.c $< -o $@

finger_radix.o: finger_radix.c
	$(CC) -c $<

finger_radix.c: input
	../dist/build/hctrie/hctrie --prefix finger --headers '<stdint.h>' $<

gperf_lookup.c: input_gperf
	gperf -L ANSI-C -GCD $< > $@

#input_gperf: input_gperf.header input
#	cat input_gperf.header input > input_gperf

clean:
	$(RM) bench
	$(RM) finger*
	$(RM) gperf_lookup.c
	$(RM) *.o
	$(RM) input_grepf
