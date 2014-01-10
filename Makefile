run:
	ghc --make Bench.hs -O2 -fforce-recomp && ./Bench -o report.html
