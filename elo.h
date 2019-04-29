
/* One byte is enough for each cell. (Even one bit would suffice!) */
typedef unsigned char CELL;

/* Pointer to Function returning void:
 *  (see K&R pages 114-116,141,195 and two last lines of page 209) */
typedef void (*PFV)();

#define NULLFP ((PFV) 0)


#ifdef AZTEC
#define strchr  index
#define strrchr rindex
#endif

char *strchr(),*strrchr();
unsigned long int setseed();

extern int
	minrow,
	mincol,
        maxrow,
        maxcol;
extern unsigned int
	minextcol,
	maxextcol,
	minextrow,
	minextcol,
	maxcolext,
	mincolext,
	maxrowext,
	minrowext,
	oldmaxcolext,
	oldmincolext,
	oldmaxrowext,
	oldminrowext,
	statline,
	draw,
        pixelmode,
	algorithm,
	fredkin_mode,
	icnt,
	stagnant_option,
	random_option,
        topology,
        noinput,
        pause_mode,
	poll_mode,
        repstop,
	ctrl_c_pressed,
        redraw_needed;

extern unsigned char print_all,old_print_all;
extern unsigned char dontcare_about_anychanges;
extern unsigned char force_loadmsg;
extern unsigned char extent_checking;
extern unsigned long int anychanges,colchanges;
extern unsigned long int
                  alive,          /* total count of alive cells. */
                  old_alive,      /* old value of alive. */
	          died,
                  born,
                  cycle,          /* current cycle # */
                  old_cycle,      /* old value of cycle. */
                  total,          /* total # of changes */
                  size,           /* rows * columns */
                  saveseed;

extern char *progname; /* Added by AK. */
extern char errmsg[],loadmsg[];

extern PFV updatefuns[];
extern PFV initfuns[];
extern unsigned int *ipointers[];

#define MAXMSG 165

#define PATNAMESIZ 201 /* It's an arbitrary limit, but probably enough */

