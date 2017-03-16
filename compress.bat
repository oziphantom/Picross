64tass -a -o s.prg -l strings.lab string.asm 
64tass -a -o picross.prg --dump-labels -l picross.tass picross.asm
cscript D:\pathstuff\dumpToVice.vbs picross.tass picross.vice
exomizer sfx sys -n picross.prg -o picross_ex.prg
c1541 -attach picross.d64 -format PICROSS,OX
c1541 -attach picross.d64 -write picross_ex.prg picross -write s.prg s 
