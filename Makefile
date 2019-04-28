golorp: *.pl *.txt
	pl -s save.pl

distrib:
	rm -rf golorp-0.0.1
	mkdir golorp-0.0.1
	cp COPYING README Makefile *.txt *.pl golorp golorp-0.0.1
	tar -czvf golorp-0.0.1.tgz golorp-0.0.1
