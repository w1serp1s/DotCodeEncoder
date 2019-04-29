/* ========================================================================== */
/***        "DotCode.c" - Printing sample DotCode symbols 06/20/08  (AL)    ***/
/***       v2.21 Added a "fast" switch to use bypass algo 9/5/2017 (jHe)    ***/
/* ========================================================================== */

#include <string.h>
#include <math.h>
#include <stdlib.h>
#include <stdio.h>

#include "DotEncod.h"

#define UCHAR unsigned char
#define TWIX(a,b,c) (((a)<=(c))&&((c)<=(b)))
#define DIGIT(c) TWIX('0','9',(c))

/* ======================================================================= */
/* ***********************      HELP SCREEN      ************************* */
/* ======================================================================= */

/*-------------------------------------------------------------------------*/
/*          "Usage()" lists the command line arguments and options.             */
/*-------------------------------------------------------------------------*/
void Usage(void)
{
    printf("\nCommand line: \"DotCode File [/x# /u# /h# /w# /q# /d# /l /s /p /f]\"\n");
    printf("where: \"File\" is the Input Message file name\n");
    printf("         [alternately, \"/abcde...\" loads Message from the Command line]\n");
    printf("         Note: \"#0\"-\"#3\" invoke <NUL> & FNC1-3 respectively, \"##\" encodes \"#\"\n");
    printf("           -except- \"/l\" invokes Literal encoding instead (no FNCx support!)\n");
    printf("       /x# specifies the X-dimension (1 to N pixels, default = 5)\n");
    printf("       /u# specifies a dot Undercut (0 to X-1 pixels, default = 0)\n");
    printf("       /h# specifies symbol Height, and/or...\n");
    printf("       /w# specifies symbol Width (BOTH specify a H/W ratio, default = 2/3)\n");
    // printf("         Note: Height & Width BOTH Negative defines size (possibly illegal!)\n");
    printf("       /q# specifies a quiet zone width (default is 3 dots)\n");
    // printf("       /d# specifies (1) round dots vs. (0) squares (default is round)\n");
    // printf("       /m# specifies symbol Mask 1-4 (default is Best Mask)\n");
    printf("       /s  Shows encoding details on the screen\n");
    printf("       /p  Plots the symbol on the screen\n");
    printf("       /f  Fast algo=stops at first mask passing the score threshold\n");
    printf("Output is \"DotCode.bmp\".  [Copyright 2016-2017 AIM TSC]");
}

/* ======================================================================= */
/* ***********************      OUTPUT UTILS      ************************ */
/* ======================================================================= */

/*-------------------------------------------------------------------------*/
/*      "BmpHeader()" outputs a poor-man's "Microsoft Bitmap (BMP)" header  */
/*-------------------------------------------------------------------------*/
void BmpHeader(int xdim, output *out, FILE *f, int qzwid)
{
    struct bmphdr {
        long fileLength, const0a, const3E, const28, width, height;
        short const1a, const1b;
        long const0b, const0c, const0d, const0e, const0f, const0g, const0h, constFFFFFF;
    } header = { 0,0,0x3e,0x28,0,0,1,1,0,0,0,0,0,0,0,0xffffff };

    header.width = (NCOL+(qzwid<<1)) * xdim;
    header.height = (NROW+(qzwid<<1)) * xdim;
    header.fileLength = 0x3e + header.height * (((header.width + 31) / 32) * 4);

    fprintf(f,"BM");
    fwrite(&header,sizeof(header),1,f);
}

/*-------------------------------------------------------------------------*/
/*      "NextBit(b,*q,f)" adds bit "b" to byte "q", outputting to file "f"  */
/*-------------------------------------------------------------------------*/
void NextBit (int obt, int *q, FILE *f)
{
    *q += *q + (1-obt);
    if (*q > 255) {
        fprintf(f,"%c",*q-256);
        *q = 1;
    }
}

/*-------------------------------------------------------------------------*/
/* BmpImage(xdim,ucut,out,dot,qzwid) creates a BMP file "dotcode.bmp" for  */
/* the matrix symbol whose bitmap is in "out" scaled by "xdim" & undercut  */
/* by "ucut".  "dot" true produces round dots, & "qzwid" adds a quiet zone */
/*-------------------------------------------------------------------------*/
#define BIT(x,y) (BMAP[(x>>3)+(y*((7+NCOL)>>3))]>>(7-(x&0x7)))%0x2
#define K (k%(NROW+1))

static void BmpImage (int xdim, int ucut, output *out, int dot, int qzwid)
{
    int i, j, k, l, start, nwords, obt, ebit, sbit, sebit, xdis, ydis, mask, q = 1;
    FILE *ofile;

    /*** First open the file and include the BMP header, ***/
    ofile = fopen("DotCode.bmp","wb");
    BmpHeader(xdim,out,ofile,qzwid);
    /*** then point "start" at the start of one row at a time ***/
    nwords = (7+NCOL)/8;

    for (i=qzwid*xdim*(((((NCOL+(qzwid<<1))*xdim)+31)>>5)<<2); i>0; i--) fprintf(ofile,"%c",255);   // Bottom quiet zone

    for (i=NROW-1; i>=0; i--) {        // Bottom row first!!
        start = i * nwords;
        /*** ... and output this row "xdim" times ***/
        for (j=0; j<xdim; j++) {

            for (k=qzwid*xdim; k; k--) NextBit(0,&q,ofile); // Left quiet zone

            ydis = (j<<1) - (xdim-ucut-1);
            if (ydis < 0) ydis = -ydis;
            for (k=0; k<NCOL; k++) {
                /*** fetching each module state in succession ***/
                obt = (BMAP[start + k/8]>>(7-(k%8)))%2;
                if (k < NCOL-1) ebit = (BMAP[start + (k+1)/8]>>(7-((k+1)%8)))%2;
                else ebit = 0;
                if (i > 0) {
                    sbit = (BMAP[start - nwords + k/8]>>(7-(k%8)))%2;
                    if (k < NCOL-1) sebit = (BMAP[start - nwords + (k+1)/8]>>(7-((k+1)%8)))%2;
                    else sebit = 0;
                }
                else sbit = sebit = 0;

                /*** ... and output it "xdim" times, ***/
                for (l=0; l<xdim; l++) {
                    xdis = (l<<1) - (xdim-ucut-1);
                    if (xdis < 0) xdis = -xdis;

                    if (obt) {
                        if (dot) {
                            if ((l >= xdim-ucut) || (j >= xdim-ucut)) obt = 0;
                        }
                        else {
                            /*** (This fancy conditional preserves the bullseye intact!) ***/
                            if (
                                ((l >= xdim-ucut)&&(!ebit))
                                ||((j >= xdim-ucut)&&(!sbit))
                                ||((l >= xdim-ucut)&&(j >= xdim-ucut)&&(!sebit))
                            ) obt = 0;
                        }
                    }

                    mask = ((dot)&&((xdis+ydis) > (xdim-ucut)*4/3))? 0:obt;

                    NextBit(mask,&q,ofile);
                }
            }

            for (k=qzwid*xdim; k; k--) NextBit(0,&q,ofile); // Right quiet zone

            /*** finally padding each row's final byte with "0"s ***/
            while (q != 1) NextBit(0,&q,ofile);
            /*** & finally padding each row to a multiple of 4 bytes! ***/
            k = ((NCOL+(qzwid<<1))*xdim + 7)/8;
            k %= 4;
            if (k) {
                for (; k<4; k++) fprintf(ofile,"%c",0);
            }
        }

    }

    for (i=qzwid*xdim*(((((NCOL+(qzwid<<1))*xdim)+31)>>5)<<2); i>0; i--) fprintf(ofile,"%c",255);   // Top quiet zone

    fclose(ofile);
}

/*-------------------------------------------------------------------------*/
/*          "PlotSymbol()" displays the bitmap for a DotCode symbol        */
/*          The "plot" is truncated at column 80 if the code is wider      */
/*-------------------------------------------------------------------------*/
static void PlotSymbol (output *out)
{
    int i, j, k, c, b;
    for (i=k=0; i<NROW; i++) {
        for (j=0; j<NCOL; j++) {
            if (!(j%8)) {
                c = BMAP[k++];
                b = 128;
            }
            if (j < 80) {
                printf((c&b) ? "O" : " ");
                b >>= 1;
            }
        }
        if (j < 80) printf("|\n");
    }
    for (j=0; j<NCOL; j++)
        printf("+");
    printf("+\n");
}

/* ======================================================================== */
/* ***********************          MAIN          ************************* */
/* ======================================================================== */

int main (int argc, char *argv[])
{
    int i, ucut, xdim, hgt, wid, dots, lit, msk, qz, show, plot, fast, ok;
    UCHAR fname[250];

    // Default all of the local and input parameters:
    ucut = show = plot = hgt = wid = lit = fast = 0;
    xdim = 5;
    qz = 3;
    msk = -1;
    dots = ok = 1;

    // Then parse the command line for all arguments:
    if (argc > 1) strcpy(fname,argv[1]);
    else {
        printf("\nFileName is Required!\n");
        ok = 0;
    }
    for (i=2; (ok) && (i < argc) && (strchr("-/",argv[i][0]) != 0); i++) {
        switch (argv[i][1]) {
            case 'X':
            case 'x':
                xdim = atoi(argv[i]+2);
                break;
            case 'U':
            case 'u':
                ucut = atoi(argv[i]+2);
                break;
            case 'H':
            case 'h':
                hgt = atoi(argv[i]+2);
                break;
            case 'W':
            case 'w':
                wid = atoi(argv[i]+2);
                break;
            case 'M':
            case 'm':
                msk = atoi(argv[i]+2);
                break;
            case 'Q':
            case 'q':
                qz = atoi(argv[i]+2);
                break;
            case 'D':
            case 'd':
                dots = atoi(argv[i]+2);
                break;
            case 'L':
            case 'l':
                lit = 1;
                break;
            case 'S':
            case 's':
                show = 1;
                break;
            case 'P':
            case 'p':
                plot = 1;
                break;
            case 'f':
            case 'F':
                fast = 1;
                break;
            default:
                printf("\nUnrecognized Argument!\n");
                ok = 0;
                break;
        }
    }

    // ...& now check that the arguments are all valid:
    if (ok) {
        if (xdim < 1) {
            printf("\nX-dimension too Low!\n");
            ok = 0;
        }
        if ((ucut < 0)||(ucut >= xdim)) {
            printf("\nUnachieveable Undercut!\n");
            ok = 0;
        }
        if ((!wid)&&(hgt)&&(hgt < 5)) {
            printf("\nSymbol Height too Low!\n");
            ok = 0;
        }
        if ((!hgt)&&(wid)&&(wid < 7)) {
            printf("\nSymbol Height too Low!\n");
            ok = 0;
        }
        if (!TWIX(-1,7,msk)) {
            printf("\nIllegal Mask Value!\n");
            ok = 0;
        }
    }

    if (ok) {
        // OK so far?... then accept the data message:
        inputs in;
        output OUT, *out = &OUT;
        UCHAR msg[4001];
        if ((*fname == '-')||(*fname == '/')) strcpy(msg,fname+1);
        else {
            FILE *f = fopen(fname,"rb");
            if (f) {
                i = fread(msg,sizeof(char),4000,f);
                fclose(f);
                msg[i] = 0;
            }
            else {
                printf("\nCan't Open \"%s\"\n",fname);
                ok = 0;
            }
        }

        if (ok) {
            // & load up the "inputs" structure
            in.msg = msg;
            in.msglen = strlen(msg);
            in.hgt = hgt;
            in.wid = wid;

            if (show) {
                printf("Input Data: ");
                for (i=0; i<in.msglen; i++) printf(" %d",msg[i]);
                printf("\n");
            }

            // & if so, go find out how big this symbol must be
            i = DotCodeEncode(&in,&OUT,lit,msk,0,0, 1);

            if (i >= 0) {
                BMAP = (UCHAR*)malloc(sizeof(UCHAR) * i);
                if (BMAP) {
                    DotCodeEncode(&in,&OUT,lit,msk,1,show, fast);

                    if (plot) PlotSymbol(out);

                    BmpImage(xdim,ucut,out,dots,qz);

                    free (BMAP);

                    if (show || plot)
                        getchar();
                }
            }
            else {
                printf("\nEncoding failure! - Check input parameters\n");
                ok = 0;
            }
        }
    }

    if (ok) return 1;
    else {
        Usage();
        return 0;
    }
}
