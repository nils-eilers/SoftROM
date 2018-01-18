softrom.prg: softrom.asm
	bsa $<

clean:
	rm -f softrom.prg softrom.lst
