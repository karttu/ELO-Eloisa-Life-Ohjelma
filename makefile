MODEL = s
CC = tcc
ASM = tasm
LINK = tlink
LDFLAGS = -v -M -m$(MODEL)
CFLAGS =  -DMSDOS -m$(MODEL) -O2
ASMFLAGS = /ml

.c.obj:
  $(CC) -c $(CFLAGS)  $<

.asm.obj:
  $(ASM) $(ASMFLAGS) $<

EXE_dependencies = elo.obj pupdate.obj puprest.obj supdate.obj loadfile.obj \
bittable.obj getcpu.obj

elo.exe : $(EXE_dependencies)
  $(CC) -e$< $(LDFLAGS) $(EXE_dependencies) graphics.lib

pupdate.obj: pupdate.asm pupdate.h pupmacro.h
puprest.obj: puprest.asm pupdate.h pupmacro.h
supdate.obj: supdate.asm
getcpu.obj:  getcpu.asm
loadfile.obj: loadfile.c elo.h
bittable.obj: bittable.c
elo.obj:      elo.c elo.h
