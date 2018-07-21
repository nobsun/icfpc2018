
all:
	stack build
	stack build --test

clean:
	stack clean
	rm -f *.o *.hi
