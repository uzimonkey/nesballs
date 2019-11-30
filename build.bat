ca65 -g balls.asm -t nes
@if errorlevel 1 exit
ld65 balls.o -o balls.nes -t nes --dbgfile balls.dbg
