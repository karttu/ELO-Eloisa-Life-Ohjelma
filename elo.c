/*
 * ELO.C   - Electronic Life Orchestra, Eloisa Life Ohjelma
 *            Coded by Antti Karttunen (karttu@mits.mdata.fi)
 *
 *
 */

#include <signal.h>
#include <ctype.h>
#include <time.h>
#include <stdio.h>

#ifdef MSDOS
#include <conio.h>
#include <dos.h>
#include <graphics.h>
#else
#include <curses.h>
#endif

#include "elo.h"

#define BANNER\
"ELO - (Electronic Life Orchestra) V3.0 by A. Karttunen (karttu@mits.mdata.fi)"

#define LIFE 0
#define FRED 1

#define CTRL(C) ((C)-64) /* E.g. CTRL('A') is 1. */
#define DEL 127 /* in ascii. */

#define getlow(X)  ((unsigned int) ((unsigned long int) (X)))
#define gethigh(X) ((unsigned int) (((unsigned long int) (X)) >> 16))

#define poweroftwo(x)  (1 << (x)) /* Returns 2^x (one shifted left x times) */

#define uli(X) ((unsigned long int) (X))

#define swap_uints(X,Y) { unsigned int _kumiankkablues_;\
_kumiankkablues_ = (X); (X) = (Y); (Y) = _kumiankkablues_; }

#define swap_luints(X,Y) { unsigned long int _kumipetosoul_;\
_kumipetosoul_ = (X); (X) = (Y); (Y) = _kumipetosoul_; }


/* Number of cycles before stop, 0 if not used. */
#define DEFAULT_REPSTOP 0
/* If -S option is specified without number, then run always at least
    four cycles of fredkin algorithm when life world becomes stagnant: */
#define DEFAULT_STAGNANT 4

/* For -Z option: */
#define DEFAULT_SLEEPCYCLE 32

/* Not needed, curses' LINES & COLS are used instead.
#define DEFAULT_MAXROW  23
#define DEFAULT_MAXCOL  79
 */

#ifdef __BORLANDC__

/* We want larger stack than 80h ! */
extern unsigned _stklen = 1000;

#endif


CELL **tables[2]; /* Now using pointers instead of fixed arrays. */

int
        minrow = 1,
        mincol = 1,
        maxrow = 0,
        maxcol = 0;

unsigned int
        minextcol = 0,    /* These are for pupdate functions' */
        maxextcol = 0,    /*  internal use, */
        minextrow = 0,    /*  minrowext, mincolext, maxrowext and maxcolext */
        maxextrow = 0,    /*  are computed from these. */

        maxcolext = 0,    /* These must be signed ! */
        mincolext = 0,    /* (Contains inclusive extents as byte columns) */
        maxrowext = 0,
        minrowext = 0,
        oldmaxcolext = 0,
        oldmincolext = 0,
        oldmaxrowext = 0,
        oldminrowext = 0,
        statline = 0,
        draw = 0,
        pixelmode = 0,
/* Contains algorithm currently in use, i.e. LIFE (0) or FRED (1). */
        algorithm = 0,
        fredkin_mode = 0,
        icnt   = 0, /* counter which is used when stagnant_option is nonzero */
        stagnant_option = 0,
        sleep_option  = 0,
        sleep_cycle   = 0,
        sleep_subcycle = 0,
        random_option = 0,
        topology = 4, /* Default is normal toroid. */
        noinput = 0, /* If 1 then getcharacter is not called between cycles */
        pause_mode = 0,
        poll_mode = 0,
        repstop = DEFAULT_REPSTOP,
        ctrl_c_pressed = 0,
        redraw_needed = 0,
	exit_character = 0;

/* When this is 1 then all the cells are printed, regardless whether
    they are changed or not: */
unsigned char print_all = 1; /* In first generation all are printed. */
/* print_all is compared to this, and if it is found different, then
   *initfuns[algorithm] is called to do some self-modifications, har har har.
   This is used only with pixelmode. This value is guaranteed to
   be different first time from 0 and 1: */
#define ALGORITHM_CHANGED 2
unsigned char old_print_all = ALGORITHM_CHANGED;

/* When this is nonzero we don't stop even if anychanges would be zero
    in the end of the main loop: */
unsigned char dontcare_about_anychanges = 0;

unsigned char force_loadmsg = 0;

unsigned char extent_checking = 0;

/* If set to non-zero, then we are running in NEC PC98 or clone: */
unsigned char pc98 = 0;

/* This variable contains nonzero after update if there's any changes
    from the generation computed two cycles before, i.e. it can
     be used to detect when world has stagnated to stable or
      period two oscillation.
 */
unsigned long int anychanges = 0, colchanges = 0;

unsigned long int
        alive = 0,          /* total count of alive cells. */
        born = 0,           /* number of births this cycle */
        died = 0,           /* number of deaths this cycle */
        old_alive = 0,      /* old value of alive. */
        old_born = 0,       /* old value of born. */
        old_died = 0,       /* old value of died. */
        cycle = 0,          /* current cycle # */
        old_cycle = -1,     /* old value of cycle. */
        total = 0,          /* total # of changes */
        size  = 0,          /* rows * columns */
        saveseed = 0;       /* if -s option used, contains the seed used. */

#ifndef MSDOS
WINDOW  *mns,                           /* Main Screen */
        *info;                          /* Bottom line */
#endif

char *progname; /* Added by AK. */
char errmsg[MAXMSG+3]         = { '\0' };
char loadmsg[PATNAMESIZ+43]   = { '\0' };

FILE *popcnt_fp=NULL; /* File Pointer for population count. */

#define DEFAULT_DEADCHAR ' '
#define DEFAULT_FREDCHAR "O" /* This must be string ! */
#define DEFAULT_LIFECHAR '*'

char fredchar = 'O';
char lifechar = DEFAULT_LIFECHAR;

char cell_chars[3] = { DEFAULT_DEADCHAR, DEFAULT_LIFECHAR, '\0' };


#define _past(y,x)    (*((*((*(tables+0))+(y)))+(x)))
#define _present(y,x) (*((*((*(tables+1))+(y)))+(x)))

/* No need to copy anything from present to past, it's just enough to
    swap two pointers! */
#define _swap_tables(X,Y) { void *tmp; tmp = X; X = Y; Y = tmp; }

#define _swap_btables(X,Y) { unsigned char far *tmp; tmp= X; X = Y; Y = tmp; }

#define alivep(C) (C) /* The cell is alive if it's non-zero. */


#ifdef MSDOS

int
        cellbwidth = 2,      /* 4 if using dwords in 386. */
        i386code = 0,        /* 1 if using dwords in 386. */
        silimit   = 0,       /* This is for the pixel update functions. */
        orgsilimit = 0,      /* For cgapworld and wrap funcs in puprest.asm */
        collimit  = 0,
        vcollimit = 0,
        ydispl    = 0,       /* 0 if no statline, 1 with charmode, etc. */
        bywidth   = 0,       /* Also this one. */
        bytewidth = 0,       /* This is maxcol/8. bywidth is this plus 2. */
        cga1kludge = 0,
        cga2kludge = 0,
        orgcga1kludge = 0,   /* For the functions in puprest.asm */
        orgcga2kludge = 0,
        p_column  = 1,       /* Memory locations for temporary */
        di_save   = 0;       /*  use in pupdate functions. */

unsigned int
        clearword = 0x0720,  /* Attribute 7 (normal) & blank. */
        vidcols   = 80,
        vidlim    = 4000,    /* vidstart + (vidwidth_ * vidlines_) */
        vidlines  = 25,
        vidseg    = 0xB800,  /* 0xA000 for PC98's */
        vidstart  = 0,
        vid2start = 0,
        orgvid2start = 0,    /* For the functions in puprest.asm */
        vidwidth  = 160; /* 2 * vidcols, because those attribute bytes */

unsigned long int c46,s46; /* For temporary use in pupdate functions. */


unsigned int getcpu(); /* Get processor's type (20, 86, 186, 286 or 386) */

/* Functions for computing cells character by character: */
void slife(),svlife(),svfred();

/* PUPFUN definitions & arrays: */

void
   pl0000000(),
   pf0000000(),
   pl1000000(),
   pf1000000(),
   pl0100000(),
   pf0100000(),
   pl1100000(),
   pf1100000(),
   pl0010000(),
   pf0010000(),
   pl1010000(),
   pf1010000(),
   pl0110000(),
   pf0110000(),
   pl1110000(),
   pf1110000();

void
   ipl0000000(),
   ipf0000000(),
   ipl1000000(),
   ipf1000000(),
   ipl0100000(),
   ipf0100000(),
   ipl1100000(),
   ipf1100000(),
   ipl0010000(),
   ipf0010000(),
   ipl1010000(),
   ipf1010000(),
   ipl0110000(),
   ipf0110000(),
   ipl1110000(),
   ipf1110000();

extern int
   _pl0000000[],
   _pf0000000[],
   _pl1000000[],
   _pf1000000[],
   _pl0100000[],
   _pf0100000[],
   _pl1100000[],
   _pf1100000[],
   _pl0010000[],
   _pf0010000[],
   _pl1010000[],
   _pf1010000[],
   _pl0110000[],
   _pf0110000[],
   _pl1110000[],
   _pf1110000[];

PFV pupfuns[] =
 {
   pl0000000,
   pf0000000,
   pl1000000,
   pf1000000,
   pl0100000,
   pf0100000,
   pl1100000,
   pf1100000,
   pl0010000,
   pf0010000,
   pl1010000,
   pf1010000,
   pl0110000,
   pf0110000,
   pl1110000,
   pf1110000
 };

PFV ipupfuns[] =
 {
   ipl0000000,
   ipf0000000,
   ipl1000000,
   ipf1000000,
   ipl0100000,
   ipf0100000,
   ipl1100000,
   ipf1100000,
   ipl0010000,
   ipf0010000,
   ipl1010000,
   ipf1010000,
   ipl0110000,
   ipf0110000,
   ipl1110000,
   ipf1110000
 };

unsigned int *pup_ptrs[] =
 {
   _pl0000000,
   _pf0000000,
   _pl1000000,
   _pf1000000,
   _pl0100000,
   _pf0100000,
   _pl1100000,
   _pf1100000,
   _pl0010000,
   _pf0010000,
   _pl1010000,
   _pf1010000,
   _pl0110000,
   _pf0110000,
   _pl1110000,
   _pf1110000
 };


#ifdef OLD_SHIT

/* Functions for computing cells with "parallel" pixel method: */
void p16life(),p16fred(),pa16life(),pa16fred();
/* Corresponding "initialization" functions: */
void ip16life(),ip16fred(),ipa16life(),ipa16fred();

/* Functions for computing cells with "parallel" pixel method: */
void p32life(),p32fred(),pa32life(),pa32fred();
/* Corresponding "initialization" functions: */
void ip32life(),ip32fred(),ipa32life(),ipa32fred();

/* Vectors of pointers to indexes for the pixel update functions: */
unsigned int _p16life[],_p16fred[],_pa16life[],_pa16fred[];
unsigned int _p32life[],_p32fred[],_pa32life[],_pa32fred[];

#endif

unsigned int *src_ipointers=NULL,*dst_ipointers=NULL;

PFV updatefuns[2] = { NULLFP, NULLFP };
PFV initfuns[2]   = { NULLFP, NULLFP };
unsigned int *ipointers[2] = { NULL, NULL };

unsigned char far *btables[2];

CELL p_getcell(table,y,x)
unsigned int table;
/* register */ unsigned int y,x;
{
    x--; /* First real column is column 1, so decrement x. */
    return(((*(btables[table]+1+(y*bywidth)+(x>>3))) >> ((~x)&7)) & 1);
}

/* That +1+ is for skipping the wrap byte. */

CELL p_setcell(table,y,x,c)
unsigned int table;
/* register */ unsigned int y,x;
CELL c;
{
    x--; /* First real column is column 1, so decrement x. */
    if(c) { *(btables[table]+1+(y*bywidth)+(x>>3)) |=  (1 << ((~x)&7)); }
    else  { *(btables[table]+1+(y*bywidth)+(x>>3)) &= ~(1 << ((~x)&7)); }
    return(c);
}

#define past(y,x)    (pixelmode ? p_getcell(0,y,x) : _past(y,x))
#define present(y,x) (pixelmode ? p_getcell(1,y,x) : _present(y,x))

#define setpast(y,x,c)\
 (pixelmode ? p_setcell(0,y,x,c) : (_past(y,x) = (c)))
#define setpresent(y,x,c)\
 (pixelmode ? p_setcell(1,y,x,c) : (_present(y,x) = (c)))


swap_tables()
{
    if(pixelmode) { _swap_btables(btables[0],btables[1]); }
    else { _swap_tables(tables[0],tables[1]); }
}

#define NO_KEY       -1

#else

#define NO_KEY       ERR

#define past(y,x)    _past(y,x)
#define present(y,x) _present(y,x)

#define setpast(y,x,c)    (past(y,x) = (c))
#define setpresent(y,x,c) (present(y,x) = (c))

#define swap_tables() _swap_tables(table[0],table[1])

#endif



/*      present[MAXROW+1][MAXCOL+1],       screen 1 cycle ago */
/*      past[MAXROW+1][MAXCOL+1],          screen this cycle */

#ifdef MSDOS


int getcharacter()
{
#ifdef  AZTEC
    return(scr_getc());
#else
    return(getch()); /* Turbo-C function. */
#endif
}


int pollcharacter()
{
    int c;

#ifdef  AZTEC
    if((c = scr_poll()) != -1)
     {
       return(scr_getc());
     }
#else
    if((c = kbhit()) != 0) /* For Turbo-C. */
     {
       return(getch());
     }
    else { c = NO_KEY; }
#endif

    return(c);
}


putcelltoscreen(y,x,cell)
int y,x,cell;
{
    int plot98cell(),cgaplotcell();
    if(pixelmode)
     {
       (pc98 ? plot98cell : cgaplotcell)(((y-1)+ydispl),(x-1),cell);
     }
    else { cputvid(((y-1)+ydispl),(x-1),cell_chars[cell]); }
}

movecursor(y,x)
int y,x;
{
    if(!pixelmode)
     {
#ifdef  AZTEC
       scr_curs((y+ydispl-1),x-1);
#else
       gotoxy(x,(y+ydispl)); /* Turbo-C function. */
#endif
     }
}

#define refresh_screen()

clear_screen()
{
    unsigned int savevidseg;

    savevidseg = vidseg;

    if(pc98 || !pixelmode)
     {
       if(pixelmode) { vidseg = 0xA000; } /* Address for text screen (PC98) */
       fillmem(0,vidseg,clearword,4000);
     }
    else /* if(!pc98 && pixelmode) */
     {
       gotoxy(1,1); /* Cursor home. Turbo-C function. */
    /* printf("\33[;H"); Ansi code for Home cursor. Commented out. */
     }

    /* Hack for PC98 series machines (Epson etc.): */
    if(pc98)
     { /* Clear attributes also: */
       fillmem(0,(vidseg+0x0200),0xFFE1,4000);
     }

    vidseg = savevidseg;
}

#else

int getcharacter()
{
    if(poll_mode) { nodelay(mns,0); poll_mode = 0; }
    return(wgetch(mns));
}

int pollcharacter()
{
    if(!poll_mode) { nodelay(mns,1); poll_mode = 1; }
    return(wgetch(mns));
}
              

#define putcelltoscreen(Y,X,CELL)\
 { mvwaddch(mns,(Y-1),(X-1),cell_chars[CELL]); }

#define movecursor(Y,X) { wmove(mns,(Y-1),(X-1)); wrefresh(mns); }

#define refresh_screen() wrefresh(mns)

#define clear_screen()

#endif



int addcell(x,y)
long int x,y;
{
    int xx,yy;

    /* If coordinates don't fit to world: */
    if((y < ((long)minrow)) || (y > ((long)maxrow)) ||
       (x < ((long)mincol)) || (x > ((long)maxcol)))
     {
       sprintf(errmsg,"point <x=%ld,y=%ld> is out of limits!",x,y);
       return(0);
     }

    xx = getlow(x);
    yy = getlow(y);

    setpresent(yy,xx,1);
    putcelltoscreen(yy,xx,1);
    refresh_screen();
    alive++; born++;
    return(1);
}


/* Return hundredths of seconds since beginning of this year: */
unsigned long int hsecs_of_year()
{
        struct tm *p;
        register unsigned long int z;
        unsigned long int z2;

#ifdef  AZTEC /* At least in MS-DOS! */
        struct tm buf;

/* dostime gives hundredths of seconds, but gmtime doesn't ! (with Aztec-C) */
        dostime(p = &buf);
#else

        time_t clock;

        time(&clock);
        p = gmtime(&clock);
#endif

        /* Do this step by step so that Aztec-C can't fuck it up: */
        z   = uli(86400);
        z  *= p->tm_yday;
        z2  = uli(3600);
        z2 *= p->tm_hour;
        z  += z2;
        z  += (p->tm_min * 60);
        z  += (p->tm_sec);
        z  *= 100;
#ifdef MSDOS
#ifdef AZTEC
/* There's no hundredth of seconds available in Unix (neither with Borland) */
        z  += p->tm_hsec;
#endif
#endif
        return(z);
}


char *hsecs2secs(hsecs)
unsigned long int hsecs; /* Hundredths of seconds */
{
        register unsigned int len;
        static char resbuf[21];

        sprintf(resbuf,"%lu",hsecs);

        len = strlen(resbuf);

        if(len == 1) /* If just one digit */
         {
           resbuf[4] = '\0';
           resbuf[3] = resbuf[0];
           resbuf[2] = '0';
           resbuf[1] = '.';
           resbuf[0] = '0';
         }
        else if(len == 2) /* If two digits */
         {
           resbuf[4] = '\0';
           resbuf[3] = resbuf[1];
           resbuf[2] = resbuf[0];
           resbuf[1] = '.';
           resbuf[0] = '0';
         }
        else /* Three or more digits */
         {
           resbuf[len+1] = '\0'; /* Put ending zero */
           resbuf[len]   = resbuf[len-1]; /* Move hundredths one */
           resbuf[len-1] = resbuf[len-2]; /*      place forward. */
           resbuf[len-2] = '.'; /* So that decimal point can be put here */
         }

        return(resbuf);
}


/* Convert to percents a ratio x/y */
/* Result is of the form: ddd.dd e.g. it is 6 characters long */
char *ito2perc(z,x,y)
char *z;
unsigned long int x,y;
{
/*      unsigned long int tmp; */
        double tmp;

        /* We don't want any division by zero errors
           (Or worse: halting of the whole PuuCee !) */
        if(!y)
/*       { strcpy(z,"000.00"); } */
         { strcpy(z,"000.0000"); }
        else
         {
/* Perkeleen Aztec-C fucks this !!!
           sprintf(z,"%05lu",((((uli(x) * uli(100000))/y)+5)/10)); */
/* Fuck, with table size of 128000 procedure below goes above 2^32.
           tmp = x;
           tmp *= uli(100000);
           tmp /= y;
           tmp += 5;
           tmp /= 10;
           sprintf(z,"%05lu",tmp);
           z[6] = '\0';
           z[5] = z[4];
           z[4] = z[3];
           z[3] = '.';  */ /* And put decimal point between. */
           /* Argh, do it with doubles: */
           tmp = x;
           tmp *= 100.0;
           sprintf(z,"%08.4f",(tmp/((double) y)));
         }

        return(z);
}



/* If stat is zero, then clear the graphics from the screen. If nonzero,
    then leave the hires-stuff into screen:
 */
clean_up(stat)
int stat;
{
#ifdef MSDOS
    /* If pixelmode on, then exit from hires mode: */
    if(pixelmode) { exitgraf(stat); }
#else
    move(23, 0);  /* go to bottom of screen */
    refresh();    /* update cursor */
    endwin();     /* Clean up curses */
#endif
}

/* Not needed ?
void cleanup()
{
    clean_up(0);
    exit(1);
}
 */

void redraw_screen()
{
#ifndef MSDOS
    signal(SIGQUIT, redraw_screen);
#endif
    redraw_needed = 1;
}

void brkhandler()
{
    signal(SIGINT, brkhandler);
    ctrl_c_pressed = 1;
}

/* This function by AK. Reserves space for the cell tables. */
int initcelltables()
{
    char *calloc();
    int y,z;
    CELL **p;

#ifdef MSDOS
    void far *farcalloc(); /* Turbo-C function. */
    unsigned long int size_in_bytes;

    if(pixelmode)
     {
       if(!i386code)
        { /* Allocate btables from the same segment. */
          size_in_bytes = (uli(bywidth) * ((2*maxrow)+3));

          /* farcalloc seems to put some eight byte header into the beginning
              of allocated memory block: (so 65536-8 = 65528) */
          if(size_in_bytes > 65528L) { goto ertzu; }

          if(!(btables[0] = (unsigned char far *)
                    farcalloc(size_in_bytes,uli(sizeof(unsigned char)))))
           { goto ertzu; }
/* We save bywidth bytes when we use the same row both for the bottom wrap row
    of btable[0] and the top wrap row of btable[1]. Of course we should
    remember to call wrapping functions always before the updating
    function. */
          btables[1] = (btables[0] + ((maxrow+1)*bywidth));
          /* Check that segments are same in both (should be!): */
          if(FP_SEG(btables[0]) != FP_SEG(btables[1])) { goto ertzu; }
        }
       else /* We run 386 code. */
        { /* Allocate btables from separate segments, each max 65528. bytes */
          size_in_bytes = (uli(bywidth) * (maxrow+2));

          /* farcalloc seems to put some eight byte header into the beginning
              of allocated memory block: (so 65536-8 = 65528) */
          if(size_in_bytes > 65528L) { goto ertzu; }

          for(z = 0; z < 2; z++)
           {
             if(!(btables[z] = (unsigned char far *)
                       farcalloc(size_in_bytes,uli(sizeof(unsigned char)))))
              { goto ertzu; }
           }
          /* Check that offsets are same in both (should be!): */
          if(FP_OFF(btables[0]) != FP_OFF(btables[1])) { goto ertzu; }
        }
     }
    else
     {
#endif

       for(z = 0; z < 2; z++)
        {
          if(!((*(tables+z)) = p = (CELL **) calloc(maxrow+3,sizeof(CELL *))))
           { goto ertzu; }
          for(y = 1; y <= maxrow; y++)
           {
             if(!((*(p+y)) = (CELL *) calloc(maxcol+2,sizeof(CELL))))
              { goto ertzu; }
           }

 /* If plain world, then allocate one row of dead cells for top & bottom: */
          if(!topology)
           { /* We can use the same memory area for both rows: */
             *(p+0) = *(p+maxrow+1) = ((CELL *) calloc(maxcol+2,sizeof(CELL)));
           }
          else /* It's toroid, let's do some pointer copying. */
           {
/* Set the first pointer, before the first real line, to point the last
    real line of the cells: */
             *(p+0)        = *(p+maxrow);
/* And vice versa, i.e. the last pointer to point the topmost line of
    the cells: */
             *(p+maxrow+1) = *(p+1);
           }

/* Terminate with NULL, because assembly version of update needs it: */
          *(p+maxrow+2) = NULL;
        } /* for(z = 0; z < 2; z++) */

#ifdef MSDOS
     }
#endif

    return(1);

ertzu:

    clean_up(1); /* First exit from curses. */
    fputs(progname,stderr);
    fputs(": Memory exhausted when tried to allocate space for cell tables!",
      stderr);
    fprintf(stderr," <%u,%u>\n",maxrow,maxcol);
#ifdef MSDOS
    if(pixelmode)
     {
       if(i386code)
        {
          if(FP_OFF(btables[0]) != FP_OFF(btables[1]))
           {
             fprintf(stderr,
"Fatal error: offsets differ: btables[0]=%08lX   btables[1]=%08lX\n",
                  btables[0],btables[1]);
           }
        }
       else
        {
          if(FP_SEG(btables[0]) != FP_SEG(btables[1]))
           {
             fprintf(stderr,
"Fatal error: segments differ: btables[0]=%08lX   btables[1]=%08lX\n",
                  btables[0],btables[1]);
           }
        }
       fprintf(stderr,
        "size_in_bytes is too big: %lu (> 65528)\n",
          size_in_bytes);
     }
#endif
    exit(1);
}

#ifdef MSDOS

#ifdef AZTEC

/* Well maybe macros would be better, but I don't use these anyway... */

initgraf()
{
    mode('h');  /* For Aztec-C ! */
    color('w');
}

exitgraf(stat)
int stat;
{
    if(!stat) { mode(2); }
}

#else


/* For Turbo-C: */

initgraf()
{
     int graphdriver=CGA;
     int graphmode=CGAHI;
     int errorcode;

     if(pc98)
      {
        init98graf();
	return;
      }

/* Some auto-detection code, not used now:
     if (registerbgidriver(EGAVGA_driver) < 0) exit(1);
     if (registerbgidriver(Herc_driver) < 0) exit(1);
     if (registerbgidriver(CGA_driver) < 0) exit(1);
 */

     initgraph(&graphdriver,&graphmode,"");

     if((errorcode = graphresult()) != grOk)
      {
        fprintf(stderr,
"\n%s: graphics error %d: %s\n",progname,errorcode,grapherrormsg(errorcode));
        exit(1);
      }

/*
     MAXCOL=getmaxx();
     MAXROW=getmaxy();
 */
}


exitgraf(stat)
int stat;
{
     if(pc98) { exit98graf(stat); }
     else     { if(!stat) { closegraph(); } }
}


/* These functions stolen (possibly modified) from the JINTORI game
   (which was coded by ???.??? for NEC PC-9801 machines.)
 */
plot98cell(y, x, cell)
unsigned int  y, x, cell;
{
    unsigned char far *addr;
    unsigned char mask;

    /* If out of limits: */
    if((x >= vidcols) || (y >= vidlines)) { return; }

    /* vidwidth should be 80: */
    addr = MK_FP(vidseg,((vidwidth*y) + (x>>3)));
    mask = (1 << (7-(x%8)));
    if (cell) { *addr |= mask; }
    else      { *addr &= ~mask; }

}


cls98(bgd_color)
unsigned int bgd_color;
{
/* Nice but so ineffective:
    int  i;
    unsigned char far *blue  = (unsigned char far *)0xa8000000L;
    unsigned char far *red   = (unsigned char far *)0xb0000000L;
    unsigned char far *green = (unsigned char far *)0xb8000000L;

    clrscr();
    for(i=0; i < 0x7f00; i++)
     {
        *blue++ = 0;
        *red++ = 0;
        *green++ = 0;
     }
 */
/* Use our own assembly routine fillmem: */

    fillmem(0,0xA800,((bgd_color&1) ? 0xFFFF : 0),0x7F00); /* Blue. */
    fillmem(0,0xB000,((bgd_color&2) ? 0xFFFF : 0),0x7F00); /* Red. */
    fillmem(0,0xB800,((bgd_color&4) ? 0xFFFF : 0),0x7F00); /* Green. */
/* I'm not sure whether this is intensity or something. Anyway, commented out:
    fillmem(0,0xE000,((bgd_color&8) ? 0xFFFF : 0),0x7F00);
 */
}


init98graf()
{
    char *getenv(),*s;
    union REGS  reg;

    reg.h.ah = 0x42;
    reg.h.ch = 0xc0;
    int86(0x18, &reg, &reg);
    reg.h.ah = 0x40;
    int86(0x18, &reg, &reg);

    /* This should be equal to call: textcursor(NODISP_CURSOR);
       (We don't use that call because it's not included in libraries
       of western versions of Borland Turbo C.)
     */
    reg.h.al = 0x00;
    reg.h.ah = 0x12;
    int86(0x18, &reg, &reg);

    s = getenv("BCOLOR");
    cls98(s ? atoi(s) : 0);
}

exit98graf(stat)
int stat;
{
    union REGS  reg;

    if(!stat) { cls98(0); }

    /* This should be equal to call: textcursor(DISP_CURSOR); */
    reg.h.al = 0x00;
    reg.h.ah = 0x11;
    int86(0x18, &reg, &reg);
}


#endif

#endif



/*
 * initialize - init windows, variables, and signals
 */

initialize()
{
#ifdef MSDOS
        if(statline)
         { /* One character, height 8 pixels with CGA hires mode.
	      And 16 pixels in PC98: */
           if(pixelmode) { ydispl = (8<<pc98); }
           else { ydispl = 1; }
         }
        else { ydispl = 0; }
         
        if(!maxcol) { maxcol = vidcols; }
        if(!maxrow)
         {
           maxrow = (vidlines - ydispl);
/* More than this won't fit into single 64K block: (see initcelltables()) */
           if(!i386code && (maxcol >= 640) && (maxrow > 398))
            { maxrow = 398; }
         }

        if(pixelmode)
         {
/* Set vidwidth to be width of one video line in bytes: */
           vidwidth = 80;
/* Check that maxcol is divisible by 16 if cellbwidth is 2, and with 32
    if cellbwidth is 4: */
           if(maxcol & ((cellbwidth<<3)-1))
            {
              fprintf(stderr,
"\n%s: Sorry, maxcol must be divisible by %u when using -X option!\n",
               progname,(cellbwidth<<3));
              exit(1);
            }

           p_column  = 1;
         /* Width in bytes, (i.e. divide maxcol by eight) plus 2: */
           bytewidth = (maxcol >> 3);
           bywidth   = (bytewidth + 2);

           init_extents();

           oldmaxcolext = maxcolext;
           oldmaxrowext = maxrowext;
           oldmincolext = mincolext;
           oldminrowext = minrowext;

           /* Pointing to first byte of the bottom wrap row: */
           orgsilimit = silimit = ((maxrow + 1) * bywidth);
           vidlim = (((vidlines-ydispl)+1) * bywidth);

           collimit  = (p_column + maxcolext); /* I.e. = (bywidth-1) */
           vcollimit = (p_column + vidwidth);

           if(pc98) /* Straightforward video memory. */
	    {
	      cga1kludge = cga2kludge = (vidwidth - bywidth);
              vid2start  = (((ydispl)*vidwidth)-(bywidth+1));
	    }
	   else /* CGA's braindamaged system. */
	    {
              /* These are hairy correction constants added in turns to
                 index which keeps the SI index to cell tables aligned also
                 to CGA's video memory in 640x200 pixel mode: */
              cga1kludge = (8192  - bywidth);
              cga2kludge = (-8112 - bywidth);
              vid2start  = (((ydispl>>1)*vidwidth)-(bywidth+1));
              if(ydispl&1) /* If ydispl is odd... */
               {
                 vid2start += 8192; /* Then add 8192 to vid2start. */
                 swap_uints(cga1kludge,cga2kludge); /* And swap these. */
               } /* (I really hope that this works! It's not tested yet...) */
	    }
         } /* if(pixelmode) */
        else
         {
           vid2start = vidstart + (ydispl * vidwidth);
           vidlim = (vidstart + (vidwidth * vidlines));
         }

        orgvid2start = vid2start;
        orgcga1kludge = cga1kludge;
        orgcga2kludge = cga2kludge;

        saveseed = setseed(saveseed ? saveseed
                                    : ((unsigned long) getlow(time(NULL))));
#else
        saveseed = setseed(saveseed ? saveseed : getpid());
        /* Was: srand(getpid()); */

        initscr();                      /* init curses */
/* Set these to be size of screen minus 1, if not overridden with -c and -r
    options in main: */
        if(!maxcol) { maxcol = (COLS - 1); }
/* Now maxrow is set to full screen size, unless -p option (print statistics)
    is specified. In that case one line is left for the status line: */
        if(!maxrow) { maxrow = (LINES - !!statline); }
#endif

        if(!maxcol || !maxrow) /* Check that these are not zero. */
         {
           clean_up(1);
           fprintf(stderr,
"\n%s: Sorry, world must be larger than %ux%u !\n",
               progname,maxcol,maxrow);
           exit(1);
         }

        size = maxcol;
        size *= maxrow;
        initcelltables();
        select_overlay();
        choose_algorithm(fredkin_mode);
        signal(SIGINT,  brkhandler);   /* catch ^C */
        signal(SIGTERM, ((PFV) exit)); /* exit on kill -15 */

#ifdef MSDOS
        /* Set hires graphics mode on: */
        if(pixelmode) { initgraf(); }
        /* If PC98's screen: */
        if(pc98) { clearword = 0x0020; }
#else
        signal(SIGQUIT, redraw_screen);
        /* new window starting from row 0 if no status line, and from row 1
            if user wants status line with -p option: */
        mns = newwin(maxrow, maxcol, !!statline, 0);
        scrollok(mns, FALSE);

        /* Set input mode. No echo and doesn't wait for CR. */
        noecho(); cbreak();

        if(statline)
         {
           info = newwin(1, COLS, 0, 0);
           scrollok(info, FALSE);
         }
        wclear(mns);
#endif
        clear_screen();
}


init_extents()
{
    maxcolext = bytewidth;
    maxrowext = maxrow;
    mincolext = mincol;
    minrowext = minrow;
}

swap_extents()
{
    swap_uints(maxcolext,oldmaxcolext);
    swap_uints(maxrowext,oldmaxrowext);
    swap_uints(mincolext,oldmincolext);
    swap_uints(minrowext,oldminrowext);
}


make_random_pattern()
{
    register int j,k;

    sprintf(loadmsg," Seed: %lu, initial density: 1/%u",
              saveseed,(random_option+1));
    force_loadmsg = 1;

    for(j = 1; j <= maxrow; j++)
     {
       for(k = 1; k <= maxcol; k++)
        {
          if(!intran(random_option+1))
           {
             setpresent(j,k,1);
             alive++; born++;
             putcelltoscreen(j,k,1);
             refresh_screen();
           }
        }
     }
}


/*
 * makscr - make your own screen using arrow keys and space bar
 */

makscr()
{
    int curx, cury;             /* current point on screen */
    char c;                     /* input char */

#ifndef MSDOS
    if(statline)
     {
       wclear(info);
       wmove(info, 0, 0);
       wprintw(info,
"Use any keys to move, * to put, space to erase, . to toggle, ^D to start"
              );
       wrefresh(info);
     }
#endif

/*  if(filename) { print(); } */

    curx = cury = 1;
    movecursor(cury,curx);
    for(;;)
     {
       c = getcharacter();
       if(c == '\004') { break; }
       else if((c == '.') || (c == ' ') || (c == '*') ||
               (c == cell_chars[0]) || (c == cell_chars[1]))
        {
          if(alivep(present(cury,curx)) && (c != '*') && (c != cell_chars[1]))
           {
             setpresent(cury,curx,0);
             died++;
             alive--;
             putcelltoscreen(cury,curx,0);
           } /* Else it was dead previously: */
          else if((c != ' ') && (c != cell_chars[0]))
           {
             setpresent(cury,curx,1);
             born++;
             alive++;
             putcelltoscreen(cury,curx,1);
           }

          if(c != '.') { curx++; }
        }
       else if((c == '\n') || (c == '\r')) /* enter pressed. */
        {
          curx = 1; /* Go to beginning of ... */
          cury++;   /* ... the next line.     */
        }
       else if(c == '\033')
        {
read_again:
          switch(getcharacter())
           {
             case '[': case 'O': { goto read_again; }
             case 'A':
                --cury; break;
             case 'B':
                ++cury; break;
             case 'C':
                ++curx; break;
             case 'D':
                --curx; break;
             default:
                break;
           }
        }
       else
        {
          switch(c)
           {
             case CTRL('H'): case DEL:
             case 'H':
             case 'h':
                --curx; break;
/*           case CTRL('J'):  Actually, this is checked elsewhere ('\n') */
             case 'J':
             case 'j':
                ++cury; break;
/*           case CTRL('K'):  not so good idea... */
             case 'K':
             case 'k':
                --cury; break;
/*           case CTRL('L'): */
             case 'L':
             case 'l':
                ++curx; break;
             default:
                break;
           }
        }

       if(cury > maxrow) cury = 1;
       if(cury < 1)      cury = maxrow;
       if(curx > maxcol) curx = 1;
       if(curx < 1)      curx = maxcol;
       if((c == CTRL('H')) || (c == DEL))
        {
          if(alivep(present(cury,curx)))
           {
             setpresent(cury,curx,0);
             died++;
             alive--;
             putcelltoscreen(cury,curx,0);
           }
        }
       movecursor(cury,curx);
     } /* for(;;) */
#ifndef MSDOS
    if(statline) { wclear(info); }
#endif
}



#ifndef MSDOS

/* Update rules:  2 or 3 adjacent alive --- stay alive
 *                3 adjacent alive -- dead to live
 *                all else die or stay dead
 */


/* C & Curses version of life algorithm.
   I used algorithm like this in my General Automation Life, written
   in assembly language about 1987.
 */
clife()
{
/*  c is Centre or Cell itself, nw_w_sw contains sum of its 'western'
    (= to the left) neighbours, n_s the neighbours up and down, and
    ne_se neighbours diagonally to right, and e contains cell one right
    from c.
 */
    register CELL c,nw_w_sw,n_s,ne_se,e,old_c;
    register int col,row;

    for(row = 1; row <= maxrow; row++)
     { /* Initialize these for the first cell of each row: */
       n_s   = (past(row-1,0)+past(row+1,0));
       c     = past(row,0);
       ne_se = (past(row-1,1) + past(row+1,1));
       e     = past(row,1);
       for(col = 1; col <= maxcol; col++)
        {
          nw_w_sw = (n_s + c);
          n_s = ne_se;
          c = e;
          ne_se  = (past(row-1,col+1) + past(row+1,col+1)); /* Only three */
          e      = past(row,col+1);  /* ..accesses to the table in loop. */
/* Cell is born or remains alive, only if sum of its eight neighbours
    ored with cell itself is 3, otherwise it's dead: */
          old_c = present(row,col); /* Get the cell two cycles before. */
          if(nw_w_sw = setpresent(row,col,(((nw_w_sw+n_s+ne_se+e)|c) == 3)))
           {
             alive++;
             /* If it was dead previously, then update born count, and
                 print the cell. However, if print_all flag is nonzero,
                 then it's printed anyway.
              */
             if((!c && ++born) || print_all) { putcelltoscreen(row,col,1); }
           }
          else /* it's going to be dead cell. */
           {
             /* If it was alive previously, then update died count, and
                 print the cell. However, if print_all flag is nonzero,
                 then it's printed anyway.
              */
             if((c && ++died) || print_all)  { putcelltoscreen(row,col,0); }
           }
          anychanges += (nw_w_sw ^ old_c);
        }
     }

    refresh_screen();
}


/* C & Curses version of Fredkin's algorithm.
   Modified from clife().
 */
cfred()
{
/*  c is Centre or Cell itself, nw_w_sw contains sum of its 'western'
    (= to the left) neighbours, and
    ne_se neighbours diagonally to right, and e contains cell one right
    from c.
 */
    register CELL c,nw_w_sw,ne_se,e,old_c;
    register int col,row;

    for(row = 1; row <= maxrow; row++)
     { /* Initialize these for the first cell of each row: */
       c     = past(row,0);
       ne_se = (past(row-1,1) ^ past(row+1,1));
       e     = past(row,1);
       for(col = 1; col <= maxcol; col++)
        {
          nw_w_sw = (c ^ ne_se);
          c = e;
          ne_se  = (past(row-1,col+1) ^ past(row+1,col+1)); /* Only three */
          e      = past(row,col+1);  /* ..accesses to the table in loop. */
/* Cell is born or remains alive, only if sum of its four orthogonal
    neighbours is odd, otherwise it will be dead: */
          old_c = present(row,col); /* Get the cell two cycles before. */
          if(nw_w_sw = setpresent(row,col,(nw_w_sw ^ e)))
           {
             alive++;
             /* If it was dead previously, then update born count, and
                 print the cell. However, if print_all flag is nonzero,
                 then it's printed anyway.
              */
             if((!c && ++born) || print_all) { putcelltoscreen(row,col,1); }
           }
          else /* it's going to be dead cell. */
           {
             /* If it was alive previously, then update died count, and
                 print the cell. However, if print_all flag is nonzero,
                 then it's printed anyway.
              */
             if((c && ++died) || print_all)  { putcelltoscreen(row,col,0); }
           }
          anychanges += (nw_w_sw ^ old_c);
        }
     }

    refresh_screen();
}



wrap_edges() /* This wraps left and right edges together. */
{
    register int y;

/* Copy the rightmost cells to column 0 of each row, and leftmost cells
    to column (maxcol+1). If appropriate topology is specified, then do it
    in twisted way, to generate the twisted world:
   Modify this!
 */
    for(y = 1; y <= maxrow; y++)
     {
       setpast(y,0,past((((topology%3)==2) ? ((maxrow+1)-y) : y),maxcol));
       setpast(y,(maxcol+1),past((((topology%3)==2) ? ((maxrow+1)-y) : y),1));
     }
}

#else /* If MSDOS defined. */

wrap_edges()
{
    if(!pixelmode) { (((topology%3)==2) ? wrap2edges() : wrap1edges()); }
    else
     {
       switch(topology%3)
        {
          case 0: { break; } /* Do nothing, because of cut edges. */
          case 1: { pwrap1lr(); break; }
          case 2: { pwrap2lr(); break; }
        }
       switch(topology/3)
        {
          case 0: { break; } /* Do nothing, because of cut edges. */
          case 1: { pwrap1tb(); break; }
          case 2: { pwrap2tb(); break; }
        }
     }
}

char *vidlineptr;

copylinetovid(s)
char *s;
{
    vidlineptr = s;
    aux_copylinetovid();
}

#endif


select_overlay()
{
#ifdef MSDOS

    if(pixelmode)
     {
#ifdef AZTEC
       char overlayname[14];

/* Default overlay name. Uses 16-bit 8086 code, no extent checking,
    no counting of alive cells, no checking of video limits, no nothing: */
       strcpy(overlayname,"p0000000");

       if(i386code)        { overlayname[1] = '1'; }
       if(extent_checking) { overlayname[2] = '1'; }
       /* If counting of alive cells is needed: */
       if(statline)        { overlayname[3] = '1'; }

       if((maxcol > vidcols) || (maxrow > (vidlines - ydispl)))
        {
          /* overlayname[4] = '1'; */
          clean_up(1);
          fprintf(stderr,
"\n%s: Sorry, computing worlds larger than screen is not yet implemented\n",
               progname);
          fprintf(stderr,"with the -X option!\n");
          exit(1);
        }

       /* Load the module in, and call ovmain in it, which in turn sets
           updatefuns, initfuns and ipointers to contain appropriate
           function and array addresses: */
       ovloader(overlayname);
#else /* For Turbo-C! */
       unsigned int index;

       index = 0;

       if(i386code)        { index |= 1; }
       if(extent_checking) { index |= 2; }
       /* If counting of alive cells is needed: */
       if(statline)        { index |= 4; }

/* Now it is implemented, so this is commented out:
       if((maxcol > vidcols) || (maxrow > (vidlines - ydispl)))
        {
          clean_up(1);
          fprintf(stderr,
"\n%s: Sorry, computing worlds larger than screen is not yet implemented\n",
               progname);
          fprintf(stderr,"with the -X option!\n");
          exit(1);
        }
 */

       index <<= 1; /* Multiply index by 2. */
       updatefuns[0] = pupfuns[index];
       updatefuns[1] = pupfuns[index+1];
       initfuns[0]   = ipupfuns[index];
       initfuns[1]   = ipupfuns[index+1];
       ipointers[0]  = pup_ptrs[index];
       ipointers[1]  = pup_ptrs[index+1];

#endif

     }
    else /* if(!pixelmode) */
     {
    /* If the table is bigger than screen, then we need vidlim checking: */
       if((maxcol > vidcols) || (maxrow > (vidlines - ydispl)))
        {
          updatefuns[0] = svlife;
          updatefuns[1] = svfred;
        }
       else
        { /* No vidlim checking is needed. */
          updatefuns[0] = slife;
          updatefuns[1] = svfred;
        }
     }
#else
    /* For the curses version: */
    updatefuns[0] = clife;
    updatefuns[1] = cfred;
#endif

}


choose_algorithm(newalg)
int newalg;
{
    cell_chars[1] = ((newalg == FRED) ? fredchar : lifechar);

/* If not in pixelmode, and not calling this first time, and characters
    used to output the fredkin and life cells are different, and algorithm
    has been changed, then set print_all flag to be nonzero, so that all
    the cells will be output in the next generation cycle, regardless
    whether they have changed or not:
 */
    if(algorithm != newalg) /* Algorithm has changed ? */
     {
       if(pixelmode)
        { /* Set this to be different from 0 and 1, so *initfuns[algorithm]
             is forced to call before *updatefuns[algorithm]:
              (Actually, I don't know whether this is necessary. Probably.) */
          old_print_all = ALGORITHM_CHANGED;
/* If changing algorithm, and extent checking used, then compute the next
    generation from the max extents:
    (Because what is stable in Life, is not stable in Fredkin.)
 */
          if(extent_checking) { init_extents(); }
          print_all = 1;  /* Force to print all & do other hamy things. */
        }
       else if(/* update_fun && ??? */ (fredchar != lifechar))
        {
          old_print_all = ALGORITHM_CHANGED;
          print_all = 1;
/*        fill_past_world(2);  Currently not needed. */
        }
     }

    algorithm = newalg; /* Save this to global variable. */

#ifdef MSDOS

    if(pixelmode)
     {
       dst_ipointers = ipointers[algorithm];

       /* If this is called first time, then assign dst_ipointers also
           to src_ipointers, and call fillind to fill the indexes: */
       if(!src_ipointers)
        {
          fillind(src_ipointers = dst_ipointers);
        }
     }

#endif

}


generate()
{
    swap_tables(); /* Then swap present <-> past */
    if(topology)  /* Wrap the edges of past. */
     { wrap_edges(); }
    update();      /* And compute the new world to present. */
    cycle++;
}

update()
{
    unsigned int pettulimppu; /* Only for internal use... */
    unsigned int trow,trow2; /* Tentative row (or column). */

    old_cycle = cycle;
    old_alive = alive;
    old_born  = born;
    old_died  = died;
    died = born = 0;
    if(!extent_checking) { alive = 0; }
    anychanges = 0;
#ifdef MSDOS
    if(pixelmode)
     {
       if(extent_checking)
        { /* Then set the various limits variables for the pupdate function: */
          silimit   = ((maxrowext + 1) * bywidth);
          /* Set p_column first to point to 0th byte (left edge wrap byte)
              of the line one upward from the first actual line to be
              computed: */
          p_column  = ((minrowext - 1) * bywidth);
          /* Set collimit to contain exclusive limit. (maxcolext contains
              inclusive limit, that's why +1) */
          collimit  = (p_column + maxcolext)+1;
          vcollimit = (p_column + vidwidth)+1; /* +1 is for wrap column. */
          p_column += mincolext; /* Add starting column to p_column. */

          pettulimppu = (minrowext-1);

          if(pc98)
	   {
             vid2start  = orgvid2start + (pettulimppu * cga1kludge);
	   }
          else
	   {
             cga1kludge = orgcga1kludge;
             cga2kludge = orgcga2kludge;
/* Get the value computed in initialize(), and subtract something from it,
    depending on the value of minrowext: */
             vid2start  = orgvid2start + ((pettulimppu>>1) * ((40-bywidth)*2));

             /* I guess this won't work if ydispl is odd! */
             if(pettulimppu & 1) /* If pettulimppu is odd... */
              {
                vid2start += cga1kludge; /* Then add cga1kludge to vid2start */
                swap_uints(cga1kludge,cga2kludge); /* And swap these. */
              } /* (I really hope that this works!) */
           }
          maxextcol = maxextrow = minextcol = 0;
          minextrow = 65535; /* Maximum 16-bit unsigned value. */
        }
/* If no extent checking used, then start always from the upper lefthand
    corner: */
       else { p_column = 1; }

       /* swapind should be called between every generation if 386 code
          not in use. And even with 386 code always when algorithm has
          changed: */
       if(!i386code || (old_print_all == ALGORITHM_CHANGED))
        { swapind(src_ipointers,dst_ipointers); }
       src_ipointers = dst_ipointers;
       if(print_all != old_print_all)
        { /* If this has changed, then do some self-modifications... */
          ((*initfuns[algorithm])(print_all));
        }
     }
#endif
    ((*updatefuns[algorithm])());
    old_print_all = print_all;
    /* Set this zero, so that it remains zero, unless it's again
         turned on by choose_algorithm or by somebody else: */
    print_all = 0;

#ifdef MSDOS
    if(pixelmode)
     {
       if(extent_checking)
        {
          alive += born;
          alive -= died;

          /* First save these: */
          oldmaxcolext = maxcolext;
          oldmincolext = mincolext;
          oldmaxrowext = maxrowext;
          oldminrowext = minrowext;

          /* Compute extents variables, from the internal variables
              computed by pupdate function: */
          maxcolext = /* (maxextcol ? */ (maxextcol % bywidth)     /* : 0) */;
          mincolext = /* (minextcol ? */ (minextcol % bywidth)     /* : 0) */;
          maxrowext = /* (maxextrow ? */ ((maxextrow / bywidth)+1) /* : 0) */;
          minrowext = /* (minextrow ? */ ((minextrow / bywidth)-1) /* : 0) */;

          /* Check whether the extents go outside of the world,
              and if they go, and world is toroid, then set the
              corresponding opposite extents to start from the
              opposite edge:
              If twisted edges are used in future, then this code
              needs some revising!!!
           */
          pettulimppu = ((bytewidth+1)-cellbwidth);
          trow=0;
          if(maxcolext > bytewidth)
           { /* If left & right edges connected: */
             if(topology%3) { mincolext = 1; trow = 1; }
             maxcolext = pettulimppu;
           }
          if((mincolext < 1) || (mincolext > pettulimppu))
           { /* If left & right edges connected: */
             if(topology%3) { maxcolext = pettulimppu; trow = 1; }
             mincolext = 1;
           }

          /* If something on the edge and we have twisted connection: */
          if(trow && ((topology%3) == 2))
           {
             trow  = ((maxrow-maxrowext)+1);
             trow2 = ((maxrow-minrowext)+1);
             if(trow < minrowext)  { minrowext = trow; }
             if(trow2 > maxrowext) { maxrowext = trow2; }
           }

          trow=0;
          if(maxrowext >= maxrow)
           { /* If upper & lower edges connected: */
             if(topology/3) { minrowext = minrow; trow = 1; }
             maxrowext = maxrow;
           }
          if(minrowext <= minrow)
           { /* If upper & lower edges connected: */
             if(topology/3) { maxrowext = maxrow; trow = 1; }
             minrowext = minrow;
           }

          /* If something on the edge and we have twisted connection: */
          if(trow && ((topology/3) == 2))
           {
             trow  = ((pettulimppu-maxcolext)+1);
             trow2 = ((pettulimppu-mincolext)+1);
             if(trow  < mincolext) { mincolext = trow; }
             if(trow2 > maxcolext) { maxcolext = trow2; }
           }
        }
     }
#endif
}



print_stat()
{
    register int j,k;
    char statbuf[PATNAMESIZ+81],tmpbuf[15];

    if(!statline) { return; }

    sprintf(statbuf,
"%-10ld %10lu (%s%%)",
       cycle, alive, ito2perc(tmpbuf,alive,size));

    if(force_loadmsg)
     {
       int i;
       strcat(statbuf,loadmsg);
       i = strlen(statbuf);
       if(i > 79) { statbuf[79] = '\0'; } /* Cut if too long. */
       else /* Fill the rest with blanks. */
        {
          while(i < 79) { statbuf[i++] = ' '; }
          statbuf[i] = '\0';
        }
     }
    else if(extent_checking)
     {
       register char *p;
       register int i;

       sprintf((statbuf + strlen(statbuf))," +%-7ld -%-7ld  %4d %4d %4d %4d",
                 born,died,mincolext,maxcolext,minrowext,maxrowext);
       /* Fill the rest with blanks: */
       i = strlen(statbuf);
       if(i > 79) { statbuf[79] = '\0'; } /* Cut if too long. */
       else
        {
          while(i < 79) { statbuf[i++] = ' '; }
          statbuf[i] = '\0';
        }
     }

    force_loadmsg = 0;

#ifdef MSDOS
    if(pc98 || !pixelmode)
     {
       unsigned int savevidseg;

       savevidseg = vidseg;
       if(pixelmode) { vidseg = 0xA000; } /* Text screen in PC98's */
       copylinetovid(statbuf);
       vidseg = savevidseg;
     }
    else
     {
       fputs(statbuf,stdout);
       putchar('\r');
     }

#else
    wmove(info, 0, 0);
    waddstr(info,statbuf);
    wclrtoeol(info);
#endif

    /* If population count file specified, and we are not loading file
        anymore: */
    if(popcnt_fp && (!*loadmsg || random_option))
     { /* Then print that stuff also to that file: */
       fprintf(popcnt_fp,"%s\n",statbuf);
     }

#ifndef MSDOS
    wrefresh(info);
#endif

}


/*
 * print_world - prints the whole world, regardless of changes.
 */
print_world()
{
    register int j,k;

#ifdef MSDOS
    if(pixelmode) { cgapworld(btables[1]); return; }
#endif

    for(j = 1; j <= maxrow; j++)
     {
       for(k = 1; k <= maxcol; k++)
        {
          putcelltoscreen(j,k,alivep(present(j,k)));
        }
     }

    refresh_screen();
}

/* Fill every cell (except wrap cells) of past table with fill_value: */
/* Currently commented out, because noone uses this:
fill_past_world(fill_value)
CELL fill_value;
{
    register int j,k;

    for(j = 1; j <= maxrow; j++)
     {
       for(k = 1; k <= maxcol; k++)
        {
          setpast(j,k,fill_value);
        }
     }
}
*/


/* is  C  '0' - '7' ? */
#define isoctdigit(C) (((C) & ~7) == 060) /* 060 = 0x30 = 48. = '0' */

#define hexdigtoi(C) (isdigit(C) ? ((C) - '0') : (toupper(C) - ('A' - 10)))

unsigned int hextoi();

char *parse_char(res_ptr,string)
unsigned int *res_ptr;
unsigned char *string;
{

        if(*string != '\\')
         {
           *res_ptr = *string;
           return((char *) string+1);
         }

/* New escapes added 23.AUG.1991 \a for bell, and \v for vertical tab
    as specified in ANSI C standard. Also now recognizes hexadecimal
    character constants beginning with \x Note that \e for escape
    doesn't belong to standard.
 */
        switch(*++string)
         {
           case 'a': { *res_ptr = '\7'; break; }    /* BEL audible alert */
           case 'b': { *res_ptr = '\b'; break; }    /* BS  backspace */
           case 'e': { *res_ptr = '\033'; break; }  /* ESC escape */
           case 'f': { *res_ptr = '\f'; break; }    /* FF  form feed */
           case 'n': { *res_ptr = '\n'; break; }    /* NL (LF) newline */
           case 'r': { *res_ptr = '\r'; break; }    /* CR  carriage return */
           case 't': { *res_ptr = '\t'; break; }    /* HT  horizontal tab */
           case 'v': { *res_ptr = '\013'; break; }  /* VT  vertical tab */
           case 'x': /* There's a hexadecimal char constant \xhh */
           case 'X':
            {
              *res_ptr = hextoi(&string,++string);
              return((char *) string);
/*            break; */
            }
           case '^': /* AK's special control-character escapes */
/* E.g. \^A or \^a is CTRL-A (= \1)  and \^? is 63-64 = -1 = 255 */
            {
              unsigned char veba;

              veba = ((unsigned char) toupper(*++string));
              *res_ptr = ((unsigned char) (veba - ((unsigned char) 64)));
              break;
            }
           default:
            {
              unsigned int i,z;

/* This commented out: (i.e. if string ends to \ then nothing is
    printed for that:
              if(!*string)
               { *res_ptr = '\0'; return(string); }
   And replaced by this at 15.08.1991:
 */
              if(!*string || (*string > 126))
               { *res_ptr = *(string-1); return((char *) string); }
 
              if(!isoctdigit(*string)) { *res_ptr = *string; break; }

              z = 0; i = 3;
              while(isoctdigit(*string) && (i--))
               {
                  z = ((z << 3) + (*string++ - '0'));
               }
              *res_ptr = z;
              return((char *) string);
            }
         }
        return((char *) string+1);
}


unsigned int hextoi(rest_ptr,string)
unsigned char **rest_ptr;
register unsigned char *string;
{
        register unsigned int x;

        x = 0;
        while(isascii(*string) && isxdigit(*string))
         {
           x = ((x << 4) + hexdigtoi(*string));
           string++;
         }
        *rest_ptr = string;
        return(x);
}

/* Currently not needed, commented out:

unsigned int octoi(rest_ptr,string)
unsigned char **rest_ptr;
register unsigned char *string;
{
        register unsigned int o;

        o = 0;
        while(isoctdigit(*string))
         {
           o = ((o << 3) + (*string++ - '0'));
         }
        *rest_ptr = string;
        return(o);
}

*/

#define isnumchar(C) (isdigit(C) || ((C) == '-') || ((C) == '+'))


/* Aaaargh! This function has grown hopelessly kludgous... I hope that
    it works...
 */
int getnumarg(ptr_to_s,p_argv,defval)
char **ptr_to_s;
char ***p_argv;
int defval;
{
    char *s;
    char optletter;
    char *force_next_argv = " ";

    s = *ptr_to_s;
    optletter = *s;

    if(!*++s) /* If no next character. */
     { /* Force to break from inner loop in option handling routine: */
       *ptr_to_s = force_next_argv;
       /* Then take the numeric argument from the next cmd line arg: */
       if(!(s = *((*p_argv)+1)) || ((*s == '-') && !isdigit(*(s+1)))
              || !isnumchar(*s))
        {
no_numarg:
          /* If no next argument, or there is option. */
          if(defval) { return(defval); }
          else { goto ertzu; }
        }
       else { ++*p_argv; }
     }

    if((*s != '-') && (*s != '+'))
     {
       if(!isdigit(*s)) { goto no_numarg; }
     }
    else if(!isdigit(*(s+1)))
     {
ertzu:
       fprintf(stderr,"%s: Numeric argument for the option -%c is %s",
         progname,optletter,(s ? "invalid: " : "missing."));
       if(s) { fputs(s,stderr); }
       fprintf(stderr,"\n");
       exit(1);
     }

    /* Force to break from inner loop in option handling routine: */    
    *ptr_to_s = force_next_argv;

    /* Is it hex number? */
    if((*s == '0') && (tolower(*(s+1)) == 'x'))
     {
       int z;

       if(!isxdigit(*(s+2))) { goto ertzu; }
       z = hextoi(&s,s+2);
       /* If there was left something after hex number: */
       if(*s) { goto ertzu; }
       return(z);
     }

    return(atoi(s));
}


char *getstrarg(s,p_argv,defval)
char *s;
char ***p_argv;
char *defval;
{
    char optletter;

    optletter = *s;

    if(!*++s) /* If no next character. */
     { /* Then take the string argument from the next cmd line arg: */
       if(!(s = *((*p_argv)+1)) || (*s == '-'))
        { /* If no next argument, or there is option. */
          if(defval) { s = defval; }
          else
           {
             fprintf(stderr,
                      "%s: string argument for the option -%c is missing!\n",
                       progname,optletter);
             exit(1);
           }
        }
       else { ++*p_argv; }
     }

    return(s);
}


/* These functions are adapted from functions rand & srand from the
    page 46 of K/R second edition:
 */
static unsigned long int _seed_num_ = 1;

/* Return random integer from range 0 - n-1, and from 0 to 65535 if n is zero:
 */
int intran(n)
unsigned int n;
{
    _seed_num_ = ((_seed_num_ * 1103515245) + 12345);

/*
 Original formula was like this, returning pseudo-random integer on 0...32767
    return (unsigned int)(_seed_num_/65536) % 32768;
 I hope that this works:
 */
    /* If argument is 0 then result is from range 0-65535: */
    /* Otherwise take modulo with argument: */
    return(!n ? gethigh(_seed_num_): (gethigh(_seed_num_) % n));
                    
}

unsigned long int setseed(seed)
unsigned long int seed;
{
    return(_seed_num_ = seed);
}


usage()
{
    printf("%s\n",BANNER);
    printf(
"usage: %s [-options] [pattern1] [pattern2] [...]\n",progname);
    printf(
"Options are:\n");
    printf(
"-A char  -D char  specifies characters used to print alive and/or dead\n");
    printf(
"         cells. By default these are '%c' and '%c'. E.g. -AO -D.\n",
           DEFAULT_LIFECHAR,DEFAULT_DEADCHAR);
    printf(
"         The \\ooo notation of the C language can be used for the special chars\n"
           );
    printf(
"         where ooo indicate octal digits. (Works also with the -F option).\n"
           );
    printf(
"-c cols  -r rows  overrides the default values for the columns and rows.\n");
/*
    printf(
"         In Unix these are the size of window, and in MSDOS 80 & 25.\n");
 */
    printf(
"-F [c]   Use Fredkin's automaton instead of Life, and if the optional character\n");
    printf(
"         is specified, then set the alive character for Fredkin cells to be\n"
           );
    printf(
"         that. However, if -S option is also used, then -F option only sets\n"
           );
    printf(
"         character for Fredkin cells, but program still starts in Life mode.\n"           );
    printf(
"-S [num] Specify that if Life process becomes stagnant (i.e. contains only\n"
           );
    printf(
"         stables and period two oscillators) then num generations of Fredkin\n"           );
    printf(
"         automaton is run with them, before changing back to Life again.\n"
           );
    printf(
"         If num is not given, the default value %u is used.\n",
          DEFAULT_STAGNANT);
    printf(
"-Z [num] Sleep one second after every num generations. -ZZ num = Sleep two\n");
    printf(
"         seconds, -ZZZ num = three seconds, etc. If num is omitted, the\n");
    printf(
"         default value %u is used. This is handy option to use with -XF\n",
          DEFAULT_SLEEPCYCLE);
    printf(
"-R num   Compute only num generations, and terminate after that.\n");
    printf(
"-P [num] Set pause mode on after computing num generations. Without num\n");
    printf(
"         starts the process with pause on. Can be toggled with p.\n");
    printf("More?>");
    if(tolower(getchar()) == 'q') { exit(1); }
    /* else { printf("\n"); } */
    printf(
"-N       Use the plain no-toroid world, i.e. edges are not wrapped together.\n");
    printf(
"         (use this with -F when you want the patterns to reflect from edges)\n");
#ifdef MSDOS
    printf(
"-X       Use pixels in hires graphics mode for showing the cells, instead of\n");
    printf(
"         the characters, allowing much larger world (at least 640x200).\n");
    printf(
"         Uses more efficient algorithm for computing the cells.\n");
    printf(
"-X16     Force to compute only 16 cells at a time if running on 386 or 486,\n");
    printf(
"         instead of default 32 on those processors.\n");
#endif
    printf(
"-n       Don't check for input. Maybe runs little faster, maybe not.\n");
    printf(
"-t num   Choose topology. Enter without argument to see the avail. topologies.\n");
    printf(
"-p       Show status line on the top of screen, containing the generation and\n");
    printf(
"         the population count, and the percentage of the whole area.\n");
    printf(
"-pp[file] Output that same status info to stdout or file (if specified).\n");
    printf(
"-d       Lets the user draw the initial pattern. Use blanks, asterisks\n");
    printf(
"         and newlines to draw the pattern, cursor keys or hjkl to move\n");
    printf(
"         around, and . to toggle alive/dead, and CTRL-D to complete.\n");
    printf(
"-s[seed] Makes a random initial pattern, using seed if that is given.\n");
    printf(
"         With -s the initial density is 1/2, with -ss it's 1/3, -sss 1/4, etc.\n");
    printf("More?>");
    if(tolower(getchar()) == 'q') { exit(1); }
    /* else { printf("\n"); } */
    printf(
"  Patterns are specified in the format: filename[:pattern_name][parameters]\n");
    printf(
"where optional parameters are letters with integer values, separated by commas.\n");
    printf(
"They are: X&Y = x&y of origo, x&y = x&y of offset, r = rotation, f = flip,\n");
    printf(
"d = delay. E.g. ss:glider,X20,Y10,r1,f-1 loads a glider from the file ss.lif\n"
           );
    printf(
"to the point <20,10> flipped by its X-axis and rotated once clockwise.\n"); 
    printf(
"  Note that you must specify at least one pattern or option -d or -s to start\n"
           );
    printf(
"the life process. -d and patterns work also together, which allows you to edit\n");
    printf(
"the pattern loaded from file before starting it.\n");
    printf(
"  Keys active when running the process (unless the -n option is used):\n");
    printf(
"CTRL-L       Redraw the screen.\n");
    printf(
"-            Show previous/next generation.\n");
    printf(
"digits 0-9   Run with Fredkin's algorithm for the next 2^digit generations\n"
           );
    printf(
"             without pause, then change back to Life if originally in the\n");
    printf(
"             Life mode. Can be used to make copies of the patterns.\n");
    printf(
"f            Change to Fredkin's algorithm indefinitely.\n");
    printf(
"l            Change to Life algorithm indefinitely.\n");
    printf(
"p            Toggle between pause & non-pause mode.\n");
    printf(
"q or x       Quit.\n");
    printf(
"Any other key in pause mode: Run one generation.\n");

    exit(1);
}

list_topologies()
{
    int i;

    static char *descriptions[] = { "edges cut         ",
                                    "connected straight",
                                    "connected twisted " };

    printf(
"The following topologies are available:\n");
    printf(
"Nr. left & right          upper & lower\n");
    for(i=0; i < 9; i++)
     {
       printf("%d   %s    %s",i,descriptions[i%3],descriptions[i/3]);
       if(i==0)      { printf("   Cut Euclidean plane\n"); }
       else if(i==4) { printf("   Normal toroid (default)\n"); }
       else          { printf("\n"); }
     }
}

/*
 * main - main procedure
 */

#define MAXPATNAMES 256 /* Should be enough! */

main(argc,argv)
int argc;
char **argv;
{
    char *getenv();
    int imulihemuli=1,patcnt=0,i;
    unsigned long int t1,t2,timediff,stagnations=0;
    double totcells;
    char *s;
    char *patnames[MAXPATNAMES+2];
    
#ifdef MSDOS
    progname = "ELO"; /* Equine Life Onager. */
                      /* Equulean Life Omniprocessor. */

    if((s = getenv("PC98")))     { pc98 = 1; vidseg = 0xa000; }

    if((s = getenv("VIDCOLS")))  { vidcols = atoi(s); }

    if((s = getenv("VIDLINES"))) { vidlines = atoi(s); }

    if((s = getenv("VIDSEG")))
     {
       vidseg = hextoi(&s,s);
       if(*s || !vidseg)
        {
          fprintf(stderr,
"\nEnvironment variable VIDSEG is invalid: %s\n",s);
          exit(1);
        }
     }
#else
    progname = *argv; /* First element in vector. Works in Unix. */
#endif

       while((s = *++argv))
        {
          if(*s == '-') /* If option ??? */
           {
             if(!*(s+1)) { goto ertzu; } /* If nothing following the hyphen. */
             while(*++s)
              {
                switch(*s)
                 { /* Override the default output characters: */
                   case 'A': case 'D':
                    {
                      unsigned int result;
                      
                      parse_char(&result,getstrarg(s,&argv,NULL));
                      if(*s == 'A') { lifechar = result; }
                      else { cell_chars[0] = result; }
                      goto bigloop;
                    }
                   case 'c':
                      maxcol = getnumarg(&s,&argv,0);
                      break;
                   case 'r':
                      maxrow = getnumarg(&s,&argv,0);
                      break;
                   case 'C':
                      dontcare_about_anychanges = 2;
                      break;
                   case 'R':
                      repstop = getnumarg(&s,&argv,0);
                      break;
                   case 'f':
                      s = getstrarg(s,&argv,NULL);
                      goto handle_patnames;
                   case 'F':
                    {
                      unsigned int result;

                      if(!stagnant_option) { fredkin_mode = 1; }

                      parse_char(&result,getstrarg(s,&argv,DEFAULT_FREDCHAR));
                      fredchar = result;
                      goto bigloop;
                    }
                   case 'd':
                      ++draw;
                      break;
                   case 'p':
                    {
                      if(++statline > 1)
                       {
                         s = getstrarg(s,&argv,"stdout");
                         if(!strcmp(s,"stdout")) { popcnt_fp = stdout; }
                         else if(!(popcnt_fp = fopen(s,"w")))
                          {
                            fprintf(stderr,
                              "\n%s: can't open file %s for writing!\n",
                               progname,s);
                            exit(1);
                          }
                         else { goto bigloop; }
                       }
                      break;
                    }
                   case 'P':
                      pause_mode = (getnumarg(&s,&argv,-1)+2);
                      break;
                   case 's':
                      if((saveseed = getnumarg(&s,&argv,1)) == 1)
                       { saveseed = 0; }
                      ++random_option;
                      break;
                   case 'S':
                      stagnant_option = getnumarg(&s,&argv,DEFAULT_STAGNANT);
                      break;
                   case 'Z':
                      sleep_subcycle = sleep_cycle =
                           getnumarg(&s,&argv,DEFAULT_SLEEPCYCLE);
                      ++sleep_option;
                      break;
                   case 't':
                      if((topology = getnumarg(&s,&argv,9)) > 8)
                       { list_topologies(); exit(1); }
                      break;
                   case 'i':
                      --imulihemuli;
                      break;
                   case 'n':
                      ++noinput;
                      break;
                   case 'N':
                      topology = 0;
                      break;
#ifdef MSDOS
                   case 'v':
                      vidseg = getnumarg(&s,&argv,0);
                      break;
                   case 'V':
                      vidlines = getnumarg(&s,&argv,0);
                      break;
                   case 'x':
                      ++extent_checking;
                      break;
                   case 'X':
                      ++pixelmode;
                      if(!getenv("VIDCOLS"))  { vidcols  = 640; }
                      if(!getenv("VIDLINES"))
                       { vidlines = (pc98 ? 400 : 200); }
		      /* If running in PC98 and user didn't specified VIDSEG
		         explicitly, then use value for green:
			 (that looks best on the LCD-display of my Epson.) */
		      if(pc98 && !getenv("VIDSEG")) { vidseg = 0xB800; }
                      if((getnumarg(&s,&argv,1) != 16) && (getcpu() >= 386))
                       {
                         /* If user didn't specify -X16 and CPU is 80386
                             or 486, then use dword instructions: */
                         i386code = 1;
                         cellbwidth = 4;
                       }
                      break;
#endif
                   default:
                    {
ertzu:
                      fprintf(stderr,
"\n%s: don't know what to do with %s ! Enter %s without arguments for help.\n",
                               progname,s,progname);
                      exit(1);
                    }
                 } /* switch(*s) */
              } /* while(*++s) */
           } /* if(*s == '-') */
          else
           {
handle_patnames:
             if(patcnt >= MAXPATNAMES)
              {
                fprintf(stderr,
"\n%s: Can't specify more than %u patterns for loading in command line!\n",
                      progname,MAXPATNAMES);
                exit(1);
              }
             patnames[patcnt++] = s;
           }
bigloop: ;
        } /* while(s = *++argv) */

        if(!draw && !patcnt && !random_option)
         { usage(); }

/*
        if(draw) { printf("User-built screen\n"); }
        if(statline)  { printf("Print statistics\n");  }
 */
        initialize();

        /* If one or more patterns were specified: */
        if(patcnt)
         { /* Then first add them all to load request queue: */
           for(i=0; i < patcnt; )
            {
              addloadpat(patnames[i++]);
            }
           loadfiles(); /* And then load them all. */
         }
        
        if(random_option) { make_random_pattern(); }

        if(draw) { makscr(); }

#ifndef MSDOS

        intrflush(mns,imulihemuli);
        if(!noinput)
         { poll_mode = (pause_mode != 1); nodelay(mns,poll_mode); }

        if(statline)
         {
           wclear(info);
           wmove(info, 0, 0);
           wprintw(info,BANNER);
           wrefresh(info);
         }
#endif


        t1 = hsecs_of_year();

        for(;;)
         {
           print_stat(); /* Info for previous generation */
           /* Decrement pause_mode if it's over 1, and let it be if it's
               1 or 0. Then check whether it's 1, and noinput option was
               specified. If that was case, then turn noinput mode off,
               allowing input again, because now it's time to check the
               input: */
           if(((pause_mode > 1) && (--pause_mode == 1)) || (pause_mode == 1))
            {
              if(noinput) { noinput = 0; }
            }
           if(redraw_needed) { goto redraw_it; }
           if(!noinput)
            {
              int c;

luuppi:
              c = NO_KEY;

              c = ((pause_mode == 1) ? getcharacter() : pollcharacter());
              if(redraw_needed) { goto redraw_it; }
              switch(tolower(c))
               {
                 case CTRL('L'):
                  {
redraw_it:
                    redraw_needed = 0;
#ifndef MSDOS
                 /* The next call to wrefresh will clear the screen completely
                    and redraw the entire screen from scratch: */
                    clearok(mns,TRUE);
                    if(statline) { clearok(info,TRUE); }
#endif
/*
                    if((pause_mode == 1))
                     {
 */
                       print_all = 1;
                       print_stat();
                       print_world();
                       goto luuppi;
/*                   } */
                    break;
                  }
                 case '-': /* Show previous generation on the screen. */
                  {
                    print_all = 1; /* Just to be sure... */
/* If we continue computing from the previous generation, then anychanges
    comes zero, so we must ignore it: */
                    dontcare_about_anychanges = 1;
                    swap_luints(cycle,old_cycle);
                    swap_luints(alive,old_alive);
                    swap_luints(born,old_born);
                    swap_luints(died,old_died);
                    swap_tables();
#ifdef MSDOS
                    if(pixelmode)
                     {
                       if(!i386code) { swapind(src_ipointers,dst_ipointers); }
                       src_ipointers = dst_ipointers;
                       if(extent_checking) { swap_extents(); }
                     }
#endif
                    print_stat();
                    print_world();
                    if((pause_mode == 1)) { goto luuppi; }
                    break;
                  }
/* Change algorithm to Fredkin's self-replicating cellular automaton,
    for 2^N next generations: */
                 case '0':
                 case '1':
                 case '2':
                 case '3':
                 case '4':
                 case '5':
                 case '6':
                 case '7':
                 case '8':
                 case '9':
                    if(algorithm == LIFE)
                     {
                       icnt = poweroftwo((c - '0')); /* And fall thru... */
                     }
                    if(pause_mode == 1)
                     { pause_mode = poweroftwo((c - '0'))+1; }
                 case 'f': /* Change to Fredkin indefinitely. */
                    if(algorithm != FRED) { choose_algorithm(FRED); }
                    break;
                 case 'l': /* And back to life: */
                  {
                    if(algorithm != LIFE) { choose_algorithm(LIFE); }
                    break;
                  }
                 case 'p': /* Toggle pause mode. */
                  {
                    pause_mode = !(pause_mode == 1);
                    break;
                  }
                 case 'q': case 'x': /* q as Quit. x as eXit. */
                  {
		    exit_character = c;
                    goto out_of_loop;
                  }
               }
            }

           generate();

           if(ctrl_c_pressed) { break; } /* Out of loop. */

           if(sleep_cycle && !--sleep_subcycle)
            {
              sleep(sleep_option);
              sleep_subcycle = sleep_cycle;
            }

           if(icnt && !--icnt) { choose_algorithm(LIFE); }

           /* If no changes between the generation just computed, and
              the generation computed two cycles before: */
           if((!anychanges && !dontcare_about_anychanges) ||
           /* Or extent checking in use, and everything is stable: */
                (extent_checking && !maxextcol))
            {
              /* If no stagnant option specified, or if this happens
                  when we are running Fredkin's algorithm (in that
                  case all the cells are probably dead), then
                  quit: */
              if(!stagnant_option || (algorithm == FRED)) { break; }
              icnt = stagnant_option;
              stagnations++;
              choose_algorithm(FRED);
            }

           if(dontcare_about_anychanges == 1)
            { dontcare_about_anychanges = 0; }

           if(repstop && (cycle == repstop)) { break; }
         }
out_of_loop:

        t2 = hsecs_of_year();


/* Unnecessary, and popcnt_fp can also be stdout, so not recommended:
        if(popcnt_fp) { fclose(popcnt_fp); } */

#ifdef MSDOS
        /* Wait user to press any key before hires screen is cleared. */
        if(pixelmode && !repstop) { putchar('!'); getchar(); }
#endif
        /* If user exits with 'x' then the screen is not cleared: */
        clean_up(tolower(exit_character) == 'x');

        totcells = size;
        totcells *= cycle;

        printf("\n");

/* Bullshit code, commented out:
        if(extent_checking)
         {
           printf("Final configuration: ");
           printf(maxextcol ? "period 2 oscillation.\n" : "stable.\n");
         }
 */

        if(random_option)
         {
           printf("Seed used: %lu, initial density: 1/%u\n",
                    saveseed,(random_option+1));
         }

        printf(
"Computed %lu cycles (= %.0f cells) in %s seconds.\n",
                      cycle,totcells,hsecs2secs(timediff = (t2-t1)));
        if(timediff && cycle) /* This test for divide by 0 error. */
         {
           printf(
"One cycle in %f seconds, %.2f cyc/sec. %.0f Effective CUPS\n",
                  ((((double) timediff)/cycle)/100),
                  ((((double) cycle)/timediff)*100),
                  ((((double) totcells)/timediff)*100)
               );
         }

        printf("Computed %u cell%s at a time.\n",
           (!pixelmode ? 1 : ((cellbwidth == 2) ? 16 : 32)),
           (pixelmode ? "s" : ""));

        if(stagnations)
         {
           printf("%lu stagnations.\n",stagnations);
         }

#ifdef MSDOS
       {
         unsigned int cpu;

         cpu = getcpu();

         if(cpu == 20) { printf("CPU is NEC V20/30.\n"); }
         else { printf("CPU is 80%u or similar.\n",cpu); }
       }
#endif
          
}

