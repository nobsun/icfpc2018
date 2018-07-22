
all:
	stack build -j4
	stack build -j4 --test

clean:
	stack clean
	rm -f *.o *.hi
