
all:
	stack build -j4

all-check:
	stack build -j4 --test

install:
	make all all-check
	install -m 755 .stack-work/install/x86_64-linux/lts-12.1/8.4.3/bin/naive-traces ./

install-g:
	make install
	mkdir -p log
	mkdir -p bin
	install -m 755 .stack-work/install/x86_64-linux/lts-12.1/8.4.3/bin/submit-gateway ./bin/

clean:
	stack clean
	rm -f *.o *.hi
