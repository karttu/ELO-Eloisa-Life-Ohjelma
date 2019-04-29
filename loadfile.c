/*
 * XLife Copyright 1989 Jon Bennett jb7m+@andrew.cmu.edu, jcrb@cs.cmu.edu
 *
 * Permission to use, copy, modify, distribute, and sell this software and its
 * documentation for any purpose is hereby granted without fee, provided that
 * the above copyright notice appear in all copies and that both that
 * copyright notice and this permission notice appear in supporting
 * documentation, and that the name of the copyright holders not be used in
 * advertising or publicity pertaining to distribution of the software without
 * specific, written prior permission.  The copyright holders make no
 * representations about the suitability of this software for any purpose.  It
 * is provided "as is" without express or implied warranty.
 *
 * THE COPYRIGHT HOLDERS DISCLAIM ALL WARRANTIES WITH REGARD TO THIS SOFTWARE,
 * INCLUDING ALL IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS, IN NO
 * EVENT SHALL THE COPYRIGHT HOLDERS BE LIABLE FOR ANY SPECIAL, INDIRECT OR
 * CONSEQUENTIAL DAMAGES OR ANY DAMAGES WHATSOEVER RESULTING FROM LOSS OF USE,
 * DATA OR PROFITS, WHETHER IN AN ACTION OF CONTRACT, NEGLIGENCE OR OTHER
 * TORTIOUS ACTION, ARISING OUT OF OR IN CONNECTION WITH THE USE OR
 * PERFORMANCE OF THIS SOFTWARE.
 */

/* 
Enhancements to #I format added by Paul Callahan (callahan@cs.jhu.edu).
New format is backward compatible with old format.
*/

/*
#include "defs.h"
#include "data.h"
#include "cellbox.h"
 */
#include "elo.h"
#include <stdio.h>
/* #include <pwd.h> */
#include <ctype.h>


/* Definition for load queue to handle delayed loading of included files. 
   Added by Paul Callahan (callahan@cs.jhu.edu)
   (Moved here from defs.h of Xlife by AK)
 */


typedef struct lq
 {
   long loadtime;
   char patname[PATNAMESIZ+1];
   int relpath;
   unsigned long hotx,hoty;
   int rxx,rxy,ryx,ryy;
   struct lq *next;
 } LoadReq;

/* definitions for geometric transformations */ 

/* Get around "%" bogosity, and compute mod *correctly*. */
#define mod(m,n) (((m)>=0)?((m)%(n)):((n)-1-(-(m)+(n)-1)%(n)))

/* compute coordinate transform for rotations */
/* yes, macros are equivalent, but this makes the meaning clearer (maybe) */
#define tx(x,y,rxx,rxy) ((rxx)*(x)+(rxy)*(y))
#define ty(x,y,ryx,ryy) ((ryx)*(x)+(ryy)*(y))

LoadReq *loadqueue = NULL;
 
/* matrix for rotations */
static int rotxx[]={1, 0,-1, 0}; 
static int rotxy[]={0,-1, 0, 1}; 
static int rotyx[]={0, 1, 0,-1}; 
static int rotyy[]={1, 0,-1, 0}; 

char oldpatfile[PATNAMESIZ] = { '\0' }; /* These are used for separate... */
static char old_patfile[BUFSIZ+1] = { '\0' }; /* ...purposes. */
static FILE *oldloadfl = NULL;


/* Get pointer to that colon separating filename and pattern name: (by AK) */
char *getcolonptr(patname)
char *patname;
{
    char *ptr;

    /* If the rightmost colon is second character in patname, then it's
        probably the colon separating drive-id and the rest of path
        in MS-DOS (e.g. like C:\patterns\pattern3.lif), so return NULL
        in that case. (So please don't create pattern files with
        one character names, and with subpatterns!!!)
     */
    if((ptr=strrchr(patname,':')) && ((ptr-patname)==1))
     { return(NULL); }
    else /* Return pointer to that colon or NULL if not found at all. */
     { return(ptr); }
}


/* Separates path from rest of pattern's name.  */
char *seppatdir(filename,filefield) 
char *filename,*filefield;
{
   char *ffptr,*colptr;

   /* Make sure "/" in pattern block name isn't confusing by temporarily
      eliminating ":" suffix */
   if(colptr=getcolonptr(filename)) { (*colptr)='\0'; }

   ffptr=strrchr(filename,'/');
   if(ffptr)
    {
      strcpy(filefield,ffptr+1);
      *(ffptr+1)='\0';
    }
   else
    {
      strcpy(filefield,filename);
      *(filename)='\0';
    } 

   /* restore ":" suffix */
   if(colptr) { (*colptr)=':'; }
}


char *addlifeext(buf)
char *buf;
{
    char *p;
 /* If there's a period, then don't append anything, except
    if the last period of filename is part of . or .. directory reference:
  */
    if((p = strrchr(buf,'.')) && (((p-buf)>1) || (*buf != '.')))
     {
       return("");
     }
    return(".lif");
}



addloadpat(patname)
char *patname;
{
    unsigned long atol();
    register char *p;
    unsigned long hotx,hoty,loadtime;
    int xoff,yoff,rotate,flip;
    int rxx, rxy, ryx, ryy;
    int i,relpathoffset = 0;
    char tmpbuf[PATNAMESIZ];

    /* Find the approximate centre point of world: */
    hotx = (maxcol+1)/2;
    hoty = (maxrow+1)/2;

    rxx = 1;
    rxy = 0;
    ryx = 0;
    ryy = 1;

    /* Default values for these: */
    xoff = yoff = rotate = loadtime = 0;
    flip = 1;

    strcpy(tmpbuf,patname); /* Make intact copy of patname. */

    p = patname;
    if(*p == ',')
     {
       fprintf(stderr,
"\n%s: Missing pattern name:\n%s\n",progname,tmpbuf);
       goto ertzu;
     }
    while(p = strchr(p,',')) /* If (one more) comma found... */
     {
       *p++ = '\0'; /* Overwrite the comma. */
       switch(*p)
        {
          case 'd': { loadtime = atol(++p); break; }
          case 'f':
           {
             flip     = atoi(++p);
             if((flip!=1) && (flip!= -1))
              {
                clean_up(1);
                fprintf(stderr,
"\n%s: invalid value for flip specified with pattern: (must be -1 or 1)\n%s\n",
                    progname,tmpbuf);
                goto ertzu;
              }
             break;
           }
          case 'r': { rotate   = atoi(++p); break; }
          case 'x': { xoff     = atoi(++p); break; }
          case 'y': { yoff     = atoi(++p); break; }
          case 'X': { hotx     = atol(++p); break; }
          case 'Y': { hoty     = atol(++p); break; }
          default:
           {
             clean_up(1);
             fprintf(stderr,
"\n%s: %s include option after the pattern name:\n%s\n",
               progname,(*p ? "invalid" : "missing"),tmpbuf);
ertzu:
             if((i=(p-patname)) < 79)
              {
                for(p=tmpbuf; i--;)
                 {
                   *p++ = ' ';
                 }
                *p++ = '^';
                *p = '\0';
                fprintf(stderr,"%s\n",tmpbuf);
              }
             exit(1);
           }
        } /* switch(*p) */
     }

/* if patname begins with ':' then assume it's in previously
    specified file:
 */
    if(*patname == ':')
     {
       if(!*oldpatfile)
        {
          clean_up(1);
          fprintf(stderr,
"\n%s: filename missing from pattern name:  %s\n",
            progname,patname);
          exit(1);
        }
       else
        {
          strcpy(tmpbuf,oldpatfile);
          strcat(tmpbuf,patname);
          strcpy(patname,tmpbuf);
        }
     }
    else
     {
       /* Copy this patname to oldpatfile: */
       strcpy(oldpatfile,patname);
       /* And cut it from colon, so now it contains only file path: */
       if(p = getcolonptr(oldpatfile)) { *p = '\0'; }

#ifdef DO_THIS_LATER
       /* if relative path given, assume directory of parent */
       if(!strchr("/~",incl[0]))
        {
          strcpy(pardir,patfile);
          seppatdir(pardir,tmpstring);
          relpathoffset=strlen(pardir);
          strcat(pardir,incl);
          strcpy(incl,pardir);
        }
#endif
     }

    add_loadreq(&loadqueue, loadtime, patname, relpathoffset, 
                  hotx + tx(xoff,yoff,rxx,rxy), 
                  hoty + ty(xoff,yoff,ryx,ryy),
                  rxx*rotxx[mod(rotate,4)]+rxy*rotyx[mod(rotate,4)],
                  flip*(rxx*rotxy[mod(rotate,4)]+rxy*rotyy[mod(rotate,4)]),
                  ryx*rotxx[mod(rotate,4)]+ryy*rotyx[mod(rotate,4)],
                  flip*(ryx*rotxy[mod(rotate,4)]+ryy*rotyy[mod(rotate,4)])
               );

}


loadfiles()
{
    do_loadreq(loadqueue);
    if(oldloadfl)
     { fclose(oldloadfl); oldloadfl = NULL; } /* Close the previous file. */
    *old_patfile = '\0';
    cycle = 0; /* Clear this before starting the process itself. */
}


#define divis(x,y) (((long)((x)/(y)))*(y)==(x)) 

do_loadreq(loadqueue)
LoadReq *loadqueue;
{
    int quitload=0;
    unsigned long generations=0;
    char thispat[PATNAMESIZ],badpat[PATNAMESIZ];

#ifdef XLIFE
    /* Save name of original file to be loaded (for load script) */
    if(strncmp(loadqueue->patname,DIR,strlen(DIR)))
     {
       strcpy(tentpat,loadqueue->patname);
     }
    else
     {
       strcpy(tentpat,loadqueue->patname+strlen(DIR));
     }

    /* start with empty population */
    head=NULL; 
    numboxes=numcells=0;

    swaphash();

    /* clear tentative hash table */
    initcells();   
#endif


    *badpat = '\0';

    while(loadqueue!=NULL)
     {
       strcpy(thispat,loadqueue->patname+loadqueue->relpath);
       sprintf(loadmsg," %s %ld",
                 thispat,loadqueue->loadtime);
       force_loadmsg = 1;
       while(loadqueue->loadtime > generations)
        {
#ifdef XLIFE
          if(divis(loadqueue->loadtime-generations,100))
           {
             sprintf(inpbuf,
                  "Generating: %ld steps left (type X to halt)",
                       loadqueue->loadtime-generations);
             XClearWindow(disp,inputw);
             XDrawString(disp, inputw, ntextgc,ICOORDS(0,0),
                            inpbuf, strlen(inpbuf));
           }
#endif
          print_stat(); /* Info for previous generation (if -p specified) */
          generate();
          generations++;
#ifdef XLIFE
          if(quitload=breakreq()) break;
#endif
        }

#ifdef XLIFE
       sprintf(inpbuf,
             "Loading %s (type X to halt)",thispat);
       XClearWindow(disp,inputw);
       XDrawString(disp, inputw, ntextgc,ICOORDS(0,0),
                         inpbuf, strlen(inpbuf));
#endif
       if(!do_loadfile(&loadqueue,generations))
        {
          strcpy(badpat,thispat);
        }

       /* Clear load info from statline: */
       *loadmsg = '\0';
       force_loadmsg = 1;

       /* If extent checking in use, and new pattern was loaded in, then
           compute the next generation from the maximum extents: */
       if(extent_checking) { init_extents(); }

#ifdef XLIFE
       if(quitload = (quitload || breakreq())) break;
#endif
     }

    /* echo name of last pattern that couldn't be read */
    if(*badpat)
     {
       clean_up(1);
       fprintf(stderr,"\n%s: can't load %s\n",progname,badpat);
       if(*errmsg)
        {
          fprintf(stderr,"Reason is: %s\n",errmsg);
        }
       exit(1);
     }
}


int add_loadreq(loadqueue,loadtime,patname,relpath,hotx,hoty,rxx,rxy,ryx,ryy)
LoadReq **loadqueue;
long loadtime;
char *patname;
int relpath;
unsigned long hotx,hoty;
int rxx,rxy,ryx,ryy;
{
    char *malloc();

    LoadReq *newreq;


  /* Find first entry where request can go while maintaining time order.
     A heap would have better theoretical time complexity, of course, but
     in practice, the list probably won't get too long.   

     After loop, we have a pointer to the pointer that must be modified
     (Could be pointer to list itself). */ 

    while((*loadqueue)!=NULL && ((*loadqueue)->loadtime < loadtime)) 
     { loadqueue= &((*loadqueue)->next); }

    /* Create new request block and load it up */
    if(!(newreq=(LoadReq *)malloc(sizeof(LoadReq))))
     {
       clean_up(1);
       fprintf(stderr,
"\nadd_loadreq: memory exhausted when tried to allocate new element to loadqueue!"
             );
       fprintf(stderr,"\npatname=%s\n",patname);
       exit(1);
     }
    newreq->loadtime=loadtime;

    /* Silently truncates long file names--should probably tell user */
    strncpy(newreq->patname,patname,PATNAMESIZ);
    newreq->relpath=relpath;
    newreq->hotx=hotx;
    newreq->hoty=hoty;
    newreq->rxx=rxx;
    newreq->rxy=rxy;
    newreq->ryx=ryx;
    newreq->ryy=ryy;

    /* Insert new request in queue */
    newreq->next=(*loadqueue);
    (*loadqueue)=newreq;

}


void parse_patname(patname,patfield)
char *patname,*patfield;
{
    char *pfptr;

/* Breaks "<filename>:<pattern>" into two strings. */

    pfptr=getcolonptr(patname);
    if(pfptr)
     {
       *pfptr = '\0';
       strcpy(patfield,pfptr+1);
     }
    else { *patfield = '\0'; }
}

int do_loadfile(loadqueue,generations)
LoadReq **loadqueue;
unsigned long generations;
{
    char patname[PATNAMESIZ],patfield[PATNAMESIZ],tmpstring[PATNAMESIZ];
    unsigned long hotx,hoty;
    int relpath, rxx, rxy, ryx, ryy;
    FILE *loadfl;
    char patfile[BUFSIZ+1];
    LoadReq *tmpqptr;

    int x,y, linect = 0;
#define M_ABSOLUTE      0
#define M_RELATIVE      1
#define M_PICTURE       2
    int loadmode = M_PICTURE; /* Now default is #P format. */

/* Get load request */
    strcpy(patname,(*loadqueue)->patname);
    relpath=(*loadqueue)->relpath;
    hotx=(*loadqueue)->hotx;
    hoty=(*loadqueue)->hoty;
    rxx=(*loadqueue)->rxx;
    rxy=(*loadqueue)->rxy;
    ryx=(*loadqueue)->ryx;
    ryy=(*loadqueue)->ryy;

/* Delete request from queue */
    tmpqptr=(*loadqueue)->next; 
    free(*loadqueue);
    (*loadqueue)=tmpqptr;

    /* separate filename from pattern name */
    parse_patname(patname,patfield);

    /* add .lif to filename if needed */
    strcat(patname,addlifeext(patname));

    /* Copy the filename to patfile: */
    strcpy(patfile,patname);

    /* If the current pattern is in the same file as previous: (added by AK)*/
    if(!strcmp(patfile,old_patfile) && oldloadfl)
     {
       loadfl = oldloadfl; /* Then don't open it again, but instead */
       rewind(loadfl); /* rewind the still open filepointer to beginning */
     }
    else if((loadfl=fopen(patfile,"r")) == NULL)
     {
#ifdef DO_THIS_LATER
       /* look for included pattern in pattern directory 
          if not found in given directory */
       strcpy(patfile,DIR);
       strcat(patfile,patname+relpath);
       if((loadfl=fopen(patfile,"r")) == NULL)
        {
          /* if all else fails, try current directory */
          strcpy(patfile,patname+relpath);
          loadfl=fopen(patfile,"r");
        }
#endif
       if(oldloadfl) /* Close the previous file. */
        { fclose(oldloadfl); oldloadfl = NULL; }
       *old_patfile = '\0';
     }
    else
     { /* Opened succesfully a new (first or different from previous) file: */
       if(oldloadfl) { fclose(oldloadfl); } /* Close the previous file. */
       oldloadfl = loadfl;
       strcpy(old_patfile,patfile);
     }

    if(loadfl!=NULL)
     {
       char buf[BUFSIZ+1],patid[PATNAMESIZ];
       int xoff = 0, yoff = 0;
       int endpattern=0,found=0;

       /* If we are searching for a specific pattern in the file,
          then we skip lines till we find it.  This is a pretty tacky way
          to handle multiple pattern definitions in the same file,
          but files with #I format probably won't get big enough for
          anyone to notice.  Really, we should implement a dictionary to
          save the location of a pattern definition in a file the first time 
          we see it. 
        */
       if(*patfield)  /* Old code: if(patfield[0]!='\0') */
        { 
          while(!found && fgets(buf,BUFSIZ,loadfl)) /* != (char *)NULL) */
           { 
             if((*buf == '#') && (buf[1] == 'B'))
              {
                sscanf(buf+2," %s",patid); 
                found=!strcmp(patfield,patid);
              }
           }
          if(!found)
           {
             oldloadfl = NULL; fclose(loadfl);
             sprintf(errmsg,"couldn't find pattern %s from file %s",
                                patfield,patfile);
             return(0); 
           }
        }

       while(fgets(buf,BUFSIZ,loadfl) && !endpattern)
        {
          if(*buf == '#')
           {
             char incl[BUFSIZ+1];
             char pardir[PATNAMESIZ];
             int lx, ly, rotate, flip, relpathoffset=0;
             long loadtime;

             *incl = '\0';
             switch(buf[1])
              {
                case 'B':
                 {
                   /* Anything between #B and #E is ignored, since it
                      is a pattern definition to be included later.
                      #B and #E cannot be nested, so we just skip till
                      we pass #E */
                   while(fgets(buf,BUFSIZ,loadfl) /* != (char *)NULL */
                           && !(buf[0]=='#' && buf[1]=='E')) {}
                   break;
                 }
                case 'E':
                 { 
                   /* if we hit a "#E" at this point, then it must be
                      the one that closes the "#B <patfield>" that we
                      are reading (assuming a syntactically correct file) */
                   endpattern=1;
                   break;
                 }
#ifdef XLIFE
/* This line contains the internal name of the pattern */
                case 'N':
                 {
                   strcat(fname,buf+2);
                   fname[strlen(fname)-1]=0;
                   break;
                 }
#endif
                case 'A':
                 {
                   sprintf(errmsg,
"absolute format (#A) is not supported anymore! (in file %s)",
                            patfile);
                   return(0);
/* Old code:
                   loadmode = M_ABSOLUTE;
                   break;
 */
                 } 
                case 'R':
                 {
                   loadmode = M_RELATIVE;
                   xoff = yoff = 0;
                   sscanf(buf+2, " %d %d", &xoff, &yoff);
                   break;
                 }
                case 'P':
                 {
                   loadmode = M_PICTURE;
                   xoff = yoff = 0;
                   sscanf(buf+2, " %d %d", &xoff, &yoff); 
                   linect=0;
                   break;
                 }
                case 'I':
                 {
                   xoff = yoff = rotate = loadtime = 0;
                   flip = 1;
                   sscanf(buf+2, " %s %d %d %d %d %d",
                               incl, &xoff, &yoff, &rotate, &flip, &loadtime); 
                   /* Silently ignore invalid flips */
                   if ((flip!=1) && (flip!= -1)) flip=1;

                   /* if included pattern begins with ':' then assume
                       it's in current file */
                   if(*incl == ':')
                    {
                      strcpy(tmpstring,patfile);
                      strcat(tmpstring,incl);
                      strcpy(incl,tmpstring);
                    }
#ifdef DO_THIS_LATER
                   else
                    {
                      /* if relative path given, assume directory of parent */
                      if(!strchr("/~",incl[0]))
                       {
                         strcpy(pardir,patfile);
                         seppatdir(pardir,tmpstring);
                         relpathoffset=strlen(pardir);
                         strcat(pardir,incl);
                         strcpy(incl,pardir);
                       }
                    } 
#endif
                   add_loadreq(
                     loadqueue, generations+loadtime, incl, relpathoffset, 
                     hotx + tx(xoff,yoff,rxx,rxy),
                     hoty + ty(xoff,yoff,ryx,ryy),
                     rxx*rotxx[mod(rotate,4)]+rxy*rotyx[mod(rotate,4)],
                     flip*(rxx*rotxy[mod(rotate,4)]+rxy*rotyy[mod(rotate,4)]),
                     ryx*rotxx[mod(rotate,4)]+ryy*rotyx[mod(rotate,4)],
                     flip*(ryx*rotxy[mod(rotate,4)]+ryy*rotyy[mod(rotate,4)])
                    );
                   break;
                 }
#ifdef XLIFE
                case 'L':
                 {
                   (void) sscanf(buf+2, " %d %d %[^\n]", &lx, &ly, incl); 
                   XDrawString(disp, lifew, ntextgc,
                                RXPOS(xoff+lx, xpos), RYPOS(yoff+ly, ypos),
                                incl, strlen(incl));
                   break;
                 }
#endif
                default:
                 {
                   break;
                 }
              } /* switch(buf[1]) */
           } /* if(buf[0] == '#') */
          else if(loadmode == M_ABSOLUTE && sscanf(buf,"%d %d\n",&x,&y)==2)
           {
/*           if(!addcell(xpos+tx(x,y,rxx,rxy),ypos+ty(x,y,ryx,ryy))) */
             if(!addcell(hotx+tx(x,y,rxx,rxy),hoty+ty(x,y,ryx,ryy)))
              { return(0); }
           }
          else if(loadmode == M_RELATIVE && sscanf(buf,"%d %d\n",&x,&y)==2)
           {
             if(!addcell(hotx+tx(xoff+x,yoff+y,rxx,rxy), 
                         hoty+ty(xoff+x,yoff+y,ryx,ryy))
               ) { return(0); }
           }
          else /* loadmode == M_PICTURE */
           {
             int col;
             char *cp;

             for(cp=buf, col=0; *cp; cp++)
              {
                if(*cp == '*')
                 {
                   if(!addcell(hotx+tx(xoff+col, yoff+linect,rxx,rxy),
                               hoty+ty(xoff+col, yoff+linect,ryx,ryy))
                     ) { return(0); }
                   col++;
                 }
/* If tab then skip to the next column divisible by eight: */
                else if((++col & 7) && (*cp == '\t')) { --cp; }
              }
           }
          linect++;
        } /* while(fgets(buf,BUFSIZ,loadfl) && !endpattern) */
/*     fclose(loadfl); Commented out by AK. Now done elsewhere. */
       return(1);
     } /* if(loadfl!=NULL) */
    else
     {
       sprintf(errmsg,"couldn't open file %s for reading",patfile);
       return(0);
     }
}

/* Rest of stuff in file.c of Xlife not currently needed, deleted. */

