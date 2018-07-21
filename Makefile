
all:
	stack build
	stack build --test

clean:
	rm -f *.o *.hi
