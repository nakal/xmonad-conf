
all:
	for hsf in *.hs; do ghc -O -i.. --make $$hsf; done

clean:
	find .. -type f -name "*.hi" -exec rm -f {} \;
	find .. -type f -name "*.o" -exec rm -f {} \;
