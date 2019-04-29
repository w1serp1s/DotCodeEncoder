/* ======================================================================= */
/**  "DotEncod.c" - DotCode (rev 2.23) Encoding Module 04/05/19  (jHe)    **/
/* ======================================================================= */
/* Copyright (c) 2016-2019 AIM Technical Symbology Committee (AIM TSC).    */
/* AIM TSC grants anyone free use of the code and assumes no liability.    */
/* This software is based on contribution from Honeywell, and the original */
/* copyright notice is seen below.                                         */
/* ======================================================================= */
/* This DotCode encoding source code is Copyrighted 2013 by Honeywell      */
/* Imaging & Mobility, Skaneateles Falls, NY. However, it is intended for  */
/* public use and may be distributed and used freely. This software is be- */
/* lieved to be error-free but Honeywell assumes no liability for its use. */
/* ======================================================================= */

// Rev 1.20 -- initial public release (6/20/08, released 8/5/08)
// Rev 1.21 -- fixed bugs in Macro header encoding & Symbol Separation (10/7/08)
// Rev 1.22 -- updated padding character(s) to value 106 as specified; also
//                  updated Macro(xx) expansion & FNC2 operations per changes to
//                  specification (10/26/08)
// Rev 2.00 -- fixed two bugs, in Score() and in call to AddPads() (search for "REV 2.00 FIX" in 5 places)
// Rev 2.10 -- fixed bugs in rsencode() to properly handle large symbols (nc >= GF)  (marked by "LARGE FIX")
// Rev 2.20 -- subtract a penalty score for empty rows/columns from total code score for each mask, where
//                  the penalty is Sum(n * N), where N is the number of positions in a row/column, and n is
//                  the number of consecutive empty rows/columns (2/24/2016)
//             commit to the first mask that scores above a threshold (2/24/2016)
// Rev 2.21 -- "fast" parameter of the encoder controls whether to use the bypass algo above (9/5/2017)
// Rev 2.22 -- reverse the mask trial order; fill in the last 6 dots if no mask passes the threshold
//                  Also tries to use the fill-in corner dots method after each regular mask for the fast
//                  bypass method (10/12/2017)
// Rev 2.23 -- Removed conditions for calling RowPenalty() and ColPenalty in ScoreArray(). Also
//                  changed some integral variable and function return types to long if they may not fit
//                  in a 16-bit integer (4/5/2019)

// DotCodeEncode() normally works on an input character string with the
//  following substitutions:
//      "#0" stands for <NUL>
//      "#1" stands for FNC1
//      "#2" stands for FNC2  (& "#2" followed by 6 digits encodes an ECI)
//      "#3" stands for FNC3, and
//      "##" for the "#" character
// NOTES:
//      "#" followed by anything else is invalid, & DotCodeEncode() returns -1.
//      BUT... setting argument "literal" non-zero bypasses these "#-" translations,
//       & all input message strings are valid but FNCx characters can't be encoded

#include <math.h>
#include <stdlib.h>
#include <stdio.h>
#include <limits.h>

#include "DotEncod.h"

#define BOOL char
#define UCHAR unsigned char
#define TRUE 1
#define FALSE 0

#define GF 113      /* Size of the Galois field */
#define PM 3        /* Prime Modulus for the Galois field */

/*****  GLOBAL VARIABLES    *****/
int wd[5000];           /* array of Codewords (data plus checks) in order */
int lg[GF], alg[GF];    /* arrays for log and antilog values */
int neras;          /* the # of Erasures &...   */
int ocp;            /* the total # of Erasures plus Errors found */
int uec;

/* ======================================================================= */
/* ************************      R-S ENCODING     ************************ */
/* ======================================================================= */
/*-------------------------------------------------------------------------*/
/*  "rsencode(nd,nc)" adds "nc" R-S check words to "nd" data words in wd[]  */
/*-------------------------------------------------------------------------*/
void rsencode (int nd, int nc)
{
    int i, j, k, nw, start, step;
    int root[GF], c[GF];

    nw = nd+nc;
    step = (nw+GF-2)/(GF-1);
    for (start=0; start<step; start++) {
        int ND = (nd-start+step-1)/step, NW = (nw-start+step-1)/step, NC = NW-ND;

        /* First time through, begin by generating "NC+1" roots (antilogs):   */
        if (!start) {   // LARGE FIX
            root[0] = 1;
            for (i=1; i<=(NC+1); i++) root[i] = (PM * root[i-1]) % GF;
        }

        /* Compute also the generator polynomial "c" of order "NC": */
        for (i=1; i<=NC; i++) c[i] = 0;
        c[0] = 1;
        for (i=1; i<=NC; i++) {
            for (j=NC; j>=1; j--) {
                c[j] = (GF + c[j] - (root[i] * c[j-1]) % GF) % GF;
            }
        }

        // Finally compute the corresponding checkword values into wd[], starting at wd[start] & stepping by step
        for (i=ND; i<NW; i++) wd[start+i*step] = 0;
        for (i=0; i<ND; i++) {
            k = (wd[start+i*step] + wd[start+ND*step]) % GF;
            for (j=0; j<NC-1; j++) {
                wd[start+(ND+j)*step] = (GF - ((c[j+1] * k) % GF) + wd[start+(ND+j+1)*step]) % GF;
            }
            wd[start+(ND+NC-1)*step] = (GF - ((c[NC] * k) % GF)) % GF;
        }
        for (i=ND; i<NW; i++) wd[start+i*step] = (GF - wd[start+i*step]) % GF;

    }
}

/* ======================================================================= */
/* *********************      MESSAGE ENCODING      ********************** */
/* ======================================================================= */
/*****  MORE GLOBAL VARIABLES   *****/
UCHAR *cw;
char PastFirstDatum, InsideMacro;   // some status flags
int Base103[6], bincnt; // accomodates Binary Mode compaction

#define FNC1 256
#define FNC2 257
#define FNC3 258
#define END  259

#define EOT 04
#define LF  10
#define CR  13
#define FS  28
#define GS  29
#define RS  30
#define US  31

#define TWIX(a,b,c) (((a)<=(c))&&((c)<=(b)))
#define DIGIT(c) TWIX('0','9',(c))

#define STORE(a) *(cw++) = (a)
#define STOREDATUM(a) { STORE(a); PastFirstDatum = 1; }

int nDigits (int *c)
{
    int *last = c;
    while (DIGIT(*last)) last++;
    return (last-c);
}
void StoreC (int *c)
{
    int v = (*c-'0') * 10 + (*(c+1)-'0');
    STOREDATUM(v);
}

void BinShift (int c)
{
    if (c < 160) {
        STORE(110);
        STOREDATUM(c-64);
    }
    else {
        STORE(111);
        STOREDATUM(c-160);
    }
}

#define SHIFT(v,m,n) { STORE(v); backto = mode; mode = m; nshift = n; repeat = TRUE; }
#define LATCH(v,m) { STORE(v); mode = m; repeat = TRUE; }
#define CODE_SET_A 0
#define CODE_SET_B 1
#define CODE_SET_C 2
#define BINARY_MODE 3

#define FNCx(c) TWIX(FNC1,FNC3,c)
BOOL DatumA (int c)
{
    return ((TWIX(0,95,c)||(FNCx(c)))? TRUE:FALSE);
}
BOOL DatumB (int c)
{
    return ((TWIX(32,127,c)||((PastFirstDatum)&&((c==9)||TWIX(28,30,c)))||(FNCx(c)))? TRUE:FALSE);
}
BOOL CrLf (int *c)
{
    return (((*c==CR)&&(*(c+1)==LF))? TRUE:FALSE);
}
BOOL DigitPair (int *c)
{
    return (((DIGIT(*c))&&(DIGIT(*(c+1))))? TRUE:FALSE);
}
BOOL SeventeenTen (int *c)
{
    return (((nDigits(c)>=10)&&(*c=='1')&&(*(c+1)=='7')&&(*(c+8)=='1')&&(*(c+9)=='0'))? TRUE:FALSE);
}
BOOL Binary (int c)
{
    return ((TWIX(128,255,c))? TRUE:FALSE);
}
BOOL ECI (int *c, long *v)
{
    if ((*c == FNC2)&&(nDigits(c+1) >= 6)) {
        int n;
        for (n=6,*v=0; n; n--) *v = *v * 10 + (*(++c)-'0');
        return (TRUE);
    }
    return (FALSE);
}

int StoreFNC2 (int *c, int *nshift)
{
    long j;
    STORE(108);
    if (ECI(c,&j)) {
        if (j < 40) {
            STORE(j);
            if (*nshift) (*nshift)--;
        }
        else {
            j -= 40;
            STORE((j/12769)+40);
            STORE((j/113)%113);
            STORE(j%113);
            if (*nshift) *nshift -= 3;
        }
        return (7);
    }
    else return (1);
}

int AheadC (int *c)
{
    int n = 0, x;
    do {
        x = n;
        if (SeventeenTen(c)) {
            c += 10;
            n += 4;
            continue;
        }
        if (DigitPair(c)) {
            c += 2;
            n++;
            continue;
        }
        if (FNCx(*c)) {
            c++;
            n++;
            continue;
        }
    }
    while (n > x);
    return (n);
}
int TryC (int *c)
{
    if (DIGIT(*c)) {
        int n = AheadC(c);
        if (n > AheadC(c+1)) return (n);
    }
    return (0);
}

int AheadA (int *c)
{
    int n = 0, x;
    long j;
    do {
        x = n;
        if (TryC(c) >= 2) continue;
        if (ECI(c,&j)) {
            c += 7;
            n += ((j <= 49)? 2:4);
            continue;
        }
        if (DatumA(*c)) {
            c++;
            n++;
            continue;
        }
    }
    while (n > x);
    return (n);
}
int AheadB (int *c)
{
    int n = 0, x;
    long j;
    do {
        x = n;
        if (TryC(c) >= 2) continue;
        if (ECI(c,&j)) {
            c += 7;
            n += ((j <= 49)? 2:4);
            continue;
        }
        if (CrLf(c)) {
            c += 2;
            n++;
            continue;
        }
        if (DatumB(*c)) {
            c++;
            n++;
            continue;
        }
    }
    while (n > x);
    return (n);
}

// routines for filling and then outputting Binary mode characters
static void BinFinish (void)
{
    int *wd;
    if (bincnt) {
        for (wd=Base103+5-bincnt; wd<=Base103+5; wd++) STORE(*wd);
        PastFirstDatum = 1;
    }
    memset(Base103,0,sizeof(int)*6);
    bincnt = 0;
}
static void BinAdd (int c)
{
    int *wd;
    for (wd=Base103+5; wd>=Base103; wd--) {
        *wd = *wd * 259 + c;
        c = *wd/103;
        *wd %= 103;
    }
    if ((++bincnt) >= 5) {
        bincnt = 5;
        BinFinish();
    }
}

/*-------------------------------------------------------------------------*/
/*  "FindDatawords(*msg,msglen,*cw)" encodes a'la Code 128                      */
/*-------------------------------------------------------------------------*/
int FindDataWords (UCHAR *msg, int msglen, UCHAR *CW, int literal)
{
    int *Msg = (int*)malloc(sizeof(int) * (msglen + 8));    // extra, for END and then some look-aheads...
    int mode = 2;
    cw = CW;
    if (Msg) {
        int i, *M = Msg, *Mend = M + msglen + 8;
        UCHAR *m = msg;
        BOOL ok = TRUE;
        while ((msglen--)&&(ok)) {
            UCHAR c = *(m++);
            if ((c != '#')||(literal)) *(M++) = c;
            else {
                if (msglen--) switch (*(m++)) {
                        case '#':
                            *(M++) = '#';
                            break;    // '#'
                        case '0':
                            *(M++) = 0;
                            break;  // <NUL>
                        case '1':
                            *(M++) = FNC1;
                            break;
                        case '2':
                            *(M++) = FNC2;
                            break;
                        case '3':
                            *(M++) = FNC3;
                            break;
                        default:
                            ok = FALSE;
                    }
                else ok = FALSE;
            }
        }
        while (M < Mend) *(M++) = END;

        if (ok) {
            int j, repeat, nshift, backto;
            long v;
            nshift = backto = bincnt = 0;
            BinFinish();
            PastFirstDatum = InsideMacro = FALSE;
            for (M=Msg; *M<END;) {
                do {
                    repeat = FALSE;
                    if ((InsideMacro == 1)&&(*M == RS)&&(*(M+1) == EOT)&&((*(M+2) == FNC3)||(*(M+2) == END))) {
                        M += 2;
                        InsideMacro = FALSE;
                    }
                    else if ((InsideMacro == 2)&&(*M == EOT)&&((*(M+1) == FNC3)||(*(M+1) == END))) {
                        M++;
                        InsideMacro = FALSE;
                    }
                    if (*M >= END) break;
                    switch (mode) {

                        case CODE_SET_A:
                            /* Check Code Set C */
                            if ((i = TryC(M)) >= 2) {
                                if (i <= 4) SHIFT(101+i,CODE_SET_C,i) else LATCH(106,CODE_SET_C);
                                break;
                            }
                            /* Try Codeset A */         if TWIX(0,95,*M) {
                                STOREDATUM((*(M++)+64)%96);
                                break;
                            }
                            if (*M == FNC1) {
                                STORE(107);
                                M++;
                                break;
                            }
                            if (*M == FNC2) {
                                M += StoreFNC2(M,&nshift);
                                break;
                            }
                            if (*M == FNC3) {
                                STORE(109);
                                M++;
                                if (PastFirstDatum) mode = CODE_SET_C;
                                break;
                            }
                            /* is it Binary? */         if (*M > 127) {
                                if (DatumA(*(M+1))) BinShift(*(M++));
                                else LATCH(112,BINARY_MODE);
                                break;
                            }
                            /* else Codeset B */            if ((i = AheadB(M)) <= 6) SHIFT(95+i,CODE_SET_B,i) else LATCH(102,CODE_SET_B);
                            break;

                        case CODE_SET_B:
                            /* Check Code Set C */
                            if ((i = TryC(M)) >= 2) {
                                if (i <= 4) SHIFT(101+i,CODE_SET_C,i) else LATCH(106,CODE_SET_C);
                                break;
                            }
                            /* Try Codeset B */         if TWIX(32,127,*M) {
                                STOREDATUM(*(M++)-32);
                                break;
                            }
                            if ((*M == 13)&&(*(M+1) == 10)) {
                                STOREDATUM(96);
                                M += 2;
                                break;
                            }
                            if (PastFirstDatum) {
                                if (*M == 9) {
                                    STOREDATUM(97);
                                    M++;
                                    break;
                                }
                                if (TWIX(28,30,*M)) {
                                    STOREDATUM(98 + *(M++)-28);
                                    break;
                                }
                            }
                            if (*M == FNC1) {
                                STORE(107);
                                M++;
                                break;
                            }
                            if (*M == FNC2) {
                                M += StoreFNC2(M,&nshift);
                                break;
                            }
                            if (*M == FNC3) {
                                STORE(109);
                                M++;
                                if (PastFirstDatum) mode = CODE_SET_C;
                                break;
                            }
                            /* Is it Binary? */         if (*M > 127) {
                                if (DatumB(*(M+1))) BinShift(*(M++));
                                else LATCH(112,BINARY_MODE);
                                break;
                            }
                            /* else Codeset A */            if ((i = AheadA(M)) == 1) SHIFT(101,CODE_SET_A,1) else LATCH(102,CODE_SET_A);
                            break;

                        case CODE_SET_C:
                        default:
                            // in first data position, check for a Macro
                            if ((!PastFirstDatum)&&(*M == '[')&&(*(M+1) == ')')&&(*(M+2) == '>')&&(*(M+3) == RS)
                                    &&(DigitPair(M+4))) {   // Got the Start of a Macro
                                int *m = M+7;
                                while (((*m)!=FNC3)&&((*m)!=END)) m++;
                                if (*(m-1) == EOT) {    // ... and the ending too!
                                    LATCH(106,CODE_SET_B);
                                    i = (*(M+4)-'0')*10 + *(M+5)-'0';
                                    if ((*(M+6) == GS)&&(*(m-2) == RS)) {
                                        switch (i) {
                                            case 05:
                                                STOREDATUM(97);
                                                break;
                                            case 06:
                                                STOREDATUM(98);
                                                break;
                                            case 12:
                                                STOREDATUM(99);
                                                break;
                                            default:
                                                break;
                                        }
                                        if (PastFirstDatum) {
                                            InsideMacro = 1;
                                            M += 7;
                                        }
                                    }
                                    if (!PastFirstDatum) {
                                        STOREDATUM(100);
                                        STORE(i);
                                        InsideMacro = 2;
                                        M += 6;
                                    }
                                }
                                if (InsideMacro) break;
                            }
                            // otherwise... always continue in C if at all possible
                            if (nDigits(M) >= 2) {
                                if (SeventeenTen(M)) {
                                    STOREDATUM(100);
                                    StoreC(M+2);
                                    StoreC(M+4);
                                    StoreC(M+6);
                                    M += 10;
                                }
                                else {
                                    StoreC(M);
                                    M += 2;
                                }
                                break;
                            }
                            if (*M == FNC1) {
                                STORE(107);
                                M++;
                                break;
                            }
                            if (*M == FNC2) {
                                M += StoreFNC2(M,&nshift);
                                break;
                            }
                            if (*M == FNC3) {
                                STORE(109);
                                M++;
                                break;
                            }
                            /* Check for Binary */      if (*M > 127) {
                                if (DigitPair(M+1)) BinShift(*(M++));
                                else LATCH(112,BINARY_MODE);
                                break;
                            }
                            /* else to A or B */        if ((i = AheadA(M)) > (j = AheadB(M))) {
                                LATCH(101,CODE_SET_A);    // to Codeset A
                            }
                            else {
                                if (j <= 4) SHIFT(101+j,CODE_SET_B,j) else LATCH(106,CODE_SET_B);    // to Codeset B
                            }
                            break;

                        case BINARY_MODE:
                            /* Check Code Set C */
                            if ((i = TryC(M)) >= 2) {   // if "favorable",
                                BinFinish();
                                if (i <= 7) SHIFT(101+i,CODE_SET_C,i) else LATCH(111,CODE_SET_C);
                                break;
                            }
                            /* Try Binary */                if ((ECI(M,&v))&&((Binary(*(M+7)))||(*(M+7) == END))) { // an ECI?...
                                if (v < 256) {
                                    BinAdd(256);
                                    BinAdd(v);
                                }
                                else if (v < 65563) {
                                    BinAdd(257);
                                    BinAdd(v>>8);
                                    BinAdd(v&0xff);
                                }
                                else {
                                    BinAdd(258);
                                    BinAdd(v>>16);
                                    BinAdd((v>>8)&0xff);
                                    BinAdd(v&0xff);
                                }
                                M += 7;
                                break;
                            }
                            // or a candidate for continuing Binary mode...
                            if ((!(FNCx(*M)))&&(((Binary(*M))||(Binary(*(M+1)))||(Binary(*(M+2)))||(Binary(*(M+3))))
                                                ||((ECI(M+1,&v))&&(Binary(*(M+8)))))) {
                                BinAdd(*(M++));
                                break;
                            }
                            /* else Terminate */            BinFinish();
                            if (*M != END) {
                                /* a symbol separator? */       if (*M == FNC3) {
                                    LATCH(112,CODE_SET_C);
                                    break;
                                }
                                /* else A or B */                   if (AheadA(M) > AheadB(M)) LATCH(109,CODE_SET_A) else LATCH(110,CODE_SET_B);
                                break;
                            }
                            break;

                    }
                }
                while (repeat);
                if (nshift) {
                    nshift--;
                    if (!nshift) mode = backto;
                }
            }
            if (mode == BINARY_MODE) BinFinish();
        }
        free(Msg);
    }
    *cw = mode; // store final "mode" for possible padding
    return (cw - CW);
}

static void AddPads (UCHAR *CW, int nd, int n)
{
    if (*(CW+nd) == 3) {
        STORE(109);
        n--;
    }
    while (n--) STORE(106);
}

/* ======================================================================== */
/* ***********************          FILL          ************************* */
/* ======================================================================== */

// The 9-bit character patterns for the values 0 thru 102, plus a filler!
static const int CharPats[113] = { // the 5-of-9 patterns with maximum transitions
    0x155,0x0ab,0x0ad,0x0b5,0x0d5,0x156,0x15a,0x16a,0x1aa,0x0ae,
    0x0b6,0x0ba,0x0d6,0x0da,0x0ea,0x12b,0x12d,0x135,0x14b,0x14d,
    0x153,0x159,0x165,0x169,0x195,0x1a5,0x1a9,0x057,0x05b,0x05d,
    0x06b,0x06d,0x075,0x097,0x09b,0x09d,0x0a7,0x0b3,0x0b9,0x0cb,
    0x0cd,0x0d3,0x0d9,0x0e5,0x0e9,0x12e,0x136,0x13a,0x14e,0x15c,
    0x166,0x16c,0x172,0x174,0x196,0x19a,0x1a6,0x1ac,0x1b2,0x1b4,
    0x1ca,0x1d2,0x1d4,0x05e,0x06e,0x076,0x07a,0x09e,0x0bc,0x0ce,
    0x0dc,0x0e6,0x0ec,0x0f2,0x0f4,0x117,0x11b,0x11d,0x127,0x133,
    0x139,0x147,0x163,0x171,0x18b,0x18d,0x193,0x199,0x1a3,0x1b1,
    0x1c5,0x1c9,0x1d1,0x02f,0x037,0x03b,0x03d,0x04f,0x067,0x073,
    0x079,0x08f,0x0c7,0x0e3,0x0f1,0x11e,0x13c,0x178,0x18e,0x19c,
    0x1b8,0x1c6,0x1cc,
};

static void SetBit (output *out, int x, int y)
{
    unsigned char msk = 0x80 >> (x&7), *byte = BMAP + y * ((NCOL+7)>>3) + (x >> 3);
    *byte |= msk;
}

static void NextDot (output *out, const int **wd, int *nw, int x, int y, int *pat, int *msk)
{
    if (*pat & *msk) SetBit(out,x,y);
    *msk >>= 1;
    if (!(*msk)) {
        *msk = 0x100;
        *pat = CharPats[*(++(*wd))];
        (*nw)--;
        if (*nw <= 0) *pat = 0x1ff;
    }
}

static void LightAllCorners(output *out)
{
    if (NROW & 1) { // Odd symbol height
        SetBit(out,NCOL-2,0);
        SetBit(out,NCOL-2,NROW-1);
        SetBit(out,NCOL-1,1);
        SetBit(out,NCOL-1,NROW-2);
        SetBit(out,0,0);
        SetBit(out,0,NROW-1);
    }
    else {      // Even symbol height
        SetBit(out,NCOL-1,NROW-2);
        SetBit(out,0,NROW-2);
        SetBit(out,NCOL-2,NROW-1);
        SetBit(out,1,NROW-1);
        SetBit(out,NCOL-1,0);
        SetBit(out,0,0);
    }
}

static void FillDotArray (output *out, const int *wd, int nw)
{
    int x, y, pat = *wd, msk = 0x02;
    memset(BMAP,0,sizeof(UCHAR) * NROW * ((NCOL+7)>>3));
    if (NROW & 1) { // Odd symbol height
        x = 0;
        y = NROW-1;
        do {
            if ((((y>0)&&(y<NROW-1))||((x>0)&&(x<NCOL-2)))&&(((y>1)&&(y<NROW-2))||(x<NCOL-1))) {
                NextDot(out,&wd,&nw,x,y,&pat,&msk);
            }
            x += 2;
            if (x >= NCOL) x = (--y) & 1;
        }
        while (y >= 0);
        NextDot(out,&wd,&nw,NCOL-2,0,&pat,&msk);
        NextDot(out,&wd,&nw,NCOL-2,NROW-1,&pat,&msk);
        NextDot(out,&wd,&nw,NCOL-1,1,&pat,&msk);
        NextDot(out,&wd,&nw,NCOL-1,NROW-2,&pat,&msk);
        NextDot(out,&wd,&nw,0,0,&pat,&msk);
        NextDot(out,&wd,&nw,0,NROW-1,&pat,&msk);
    }
    else {      // Even symbol height
        x = y = 0;
        do {
            if ((((x>0)&&(x<NCOL-1))||((y>0)&&(y<NROW-2)))&&(((x>1)&&(x<NCOL-2))||(y<NROW-1))) {
                NextDot(out,&wd,&nw,x,y,&pat,&msk);
            }
            y += 2;
            if (y >= NROW) y = (++x) & 1;
        }
        while (x < NCOL);
        NextDot(out,&wd,&nw,NCOL-1,NROW-2,&pat,&msk);
        NextDot(out,&wd,&nw,0,NROW-2,&pat,&msk);
        NextDot(out,&wd,&nw,NCOL-2,NROW-1,&pat,&msk);
        NextDot(out,&wd,&nw,1,NROW-1,&pat,&msk);
        NextDot(out,&wd,&nw,NCOL-1,0,&pat,&msk);
        NextDot(out,&wd,&nw,0,0,&pat,&msk);
    }
}

int Printed (unsigned char *Dots, int Hgt, int Wid, int x, int y)
{
    if ((x >= 0)&&(x < Wid)&&(y >= 0)&&(y < Hgt)) {
        unsigned char mask = 0x80 >> (x&7);
        unsigned char *byte = Dots + y * ((Wid+7)>>3) + (x>>3);
        if (*byte & mask) return (1);
    }
    return (0);
}

int ClrCol (unsigned char *Dots, int Hgt, int Wid, int x)
{
    int y;
    for (y=x&1; y<Hgt; y+=2) if (Printed(Dots,Hgt,Wid,x,y)) return FALSE;
    return TRUE;
}
int ClrRow (unsigned char *Dots, int Hgt, int Wid, int y)
{
    int x;
    for (x=y&1; x<Wid; x+=2) if (Printed(Dots,Hgt,Wid,x,y)) return FALSE;
    return TRUE;
}

// calc penalty for empty interior columns
int ColPenalty (unsigned char *Dots, int Hgt, int Wid)
{
    int x, penalty = 0, penalty_local = 0;
    for (x=1; x<Wid-1; x++) {
        if (ClrCol(Dots,Hgt,Wid,x)) {
            if (penalty_local == 0) penalty_local = Hgt;
            else penalty_local *= Hgt;
        }
        else {
            if (penalty_local) {
                penalty += penalty_local;
                penalty_local = 0;
            }
        }
    }
    return penalty + penalty_local;
}
// calc penalty for empty interior rows
int RowPenalty (unsigned char *Dots, int Hgt, int Wid)
{
    int y, penalty = 0, penalty_local = 0;
    for (y=1; y<Hgt-1; y++) {
        if (ClrRow(Dots,Hgt,Wid,y)) {
            if (penalty_local == 0) penalty_local = Wid;
            else penalty_local *= Wid;
        }
        else {
            if (penalty_local) {
                penalty += penalty_local;
                penalty_local = 0;
            }
        }
    }
    return penalty + penalty_local;
}

long ScoreArray (unsigned char *Dots, int Hgt, int Wid)
{
    int x, y, worstedge, first, last, sum;
    long penalty;

    // first, guard against "pathelogical" gaps in the array
    // subtract a penalty score for empty rows/columns from total code score for each mask,
    // where the penalty is Sum(N ^ n), where N is the number of positions in a column/row,
    // and n is the number of consecutive empty rows/columns (jHe, 2/24/2016)
    penalty = RowPenalty(Dots, Hgt, Wid) + ColPenalty(Dots, Hgt, Wid);

    // across the top edge, count printed dots and measure their extent
    for (x=sum=0,first=last=-1; x<Wid; x+=2)    // REV 2.00 FIX
        if (Printed(Dots,Hgt,Wid,x,0)) {
            if (first<0) first = x;
            last = x;
            sum++;
        }
    if (sum == 0) penalty += 100000L;   // guard against empty edge
    worstedge = sum + last-first;
    worstedge *= Hgt;

    // across the bottom edge, ditto; REV 2.00 FIX
    for (x=Wid&1,sum=0,first=last=-1; x<Wid; x+=2) 
        if (Printed(Dots,Hgt,Wid,x,Hgt-1)) {
            if (first<0) first = x;
            last = x;
            sum++;
        }
    if (sum == 0) penalty += 200000L;   // guard against empty edge
    sum += last-first;
    sum *= Hgt;
    if (sum < worstedge) worstedge = sum;

    // down the left edge, ditto; REV 2.00 FIX
    for (y=sum=0,first=last=-1; y<Hgt; y+=2) 
        if (Printed(Dots,Hgt,Wid,0,y)) {
            if (first<0) first = y;
            last = y;
            sum++;
        }
    if (sum == 0) penalty += 400000L;   // guard against empty edge
    sum += last-first;
    sum *= Wid;
    if (sum < worstedge) worstedge = sum;

    // down the right edge, ditto; REV 2.00 FIX
    for (y=Hgt&1,sum=0,first=last=-1; y<Hgt; y+=2) 
        if (Printed(Dots,Hgt,Wid,Wid-1,y)) {
            if (first<0) first = y;
            last = y;
            sum++;
        }
    if (sum == 0) penalty += 800000L;   // guard against empty edge
    sum += last-first;
    sum *= Wid;
    if (sum < worstedge) worstedge = sum;

    // throughout the array, count the # of unprinted 5-somes (cross patterns)
    // plus the # of printed dots surrounded by 8 unprinted neighbors
    for (y=0,sum=0; y<Hgt; y++) {
        for (x=y&1; x<Wid; x+=2) {
            if ((!Printed(Dots,Hgt,Wid,x-1,y-1)) && (!Printed(Dots,Hgt,Wid,x+1,y-1))
                    && (!Printed(Dots,Hgt,Wid,x-1,y+1)) &&(!Printed(Dots,Hgt,Wid,x+1,y+1))
                    && ((!Printed(Dots,Hgt,Wid,x,y)) || ((!Printed(Dots,Hgt,Wid,x-2,y))
                            && (!Printed(Dots,Hgt,Wid,x,y-2)) && (!Printed(Dots,Hgt,Wid,x+2,y))
                            && (!Printed(Dots,Hgt,Wid,x,y+2)))
                       )
               ) sum++;
        }
    }

    return (worstedge - sum*sum - penalty);
}

/* ======================================================================== */
/* *************          GENERAL DOTCODE ENCODING          *************** */
/* ======================================================================== */

const int mask[4] = { 0, 3, 7, 17 };

int DotCodeEncode (inputs *in, output *out, int literal, int topmsk, int fill, int show, int fast)
{
    // First, if not "literal", check that all #-sequences terminate legally
    UCHAR *CW;
    int i, nBytes = 0;
    if (!literal) {
        for (i=in->msglen,CW=in->msg; i>0; i--,CW++) {
            if (*CW == '#') {
                i--;
                CW++;
                if ((!i)||((*CW != '#')&&((*CW < '0')||(*CW >'3')))) return (-1);
            }
        }
    }
    CW = (UCHAR*)malloc(sizeof(UCHAR) * (LEN<<4) + 4);
    if (CW) {
        int i, nd, nc, nw, minArea, hgt, wid;
        UCHAR *cw = CW;
        // First perform the Data Encoding
        nd = FindDataWords(MSG,LEN,cw,literal);
        nc = (nd>>1) + 3;
        nw = nd + nc;
        minArea = (2 + 9 * nw) << 1;

        if (show) {
            printf("Message Chars: ");
            for (i=0; i<nd; i++) printf(" %d",CW[i]);
            printf("\n");
            printf("  %d data + %d checks => Minimum # dots = %d\n",nd, nc, minArea>>1);
        }

        // Then find the symbol's size
        if (HGT || WID) {
            hgt = HGT;
            wid = WID;
        }
        else {
            hgt = 2;
            wid = 3;
        }
        if (hgt) {
            if (!wid) {
                NROW = hgt;
                NCOL = (minArea + NROW-1) / NROW;
                if (!((NCOL^NROW)&0x1)) NCOL++;
                while (NCOL < 7) NCOL += 2; // making sure NCOL >= 7 - padding will take care of the rest
            }
            else {
                if (hgt * wid < 0) return (-1);
                else if (hgt < 0) { // negative hgt & wid specifies symbol size!
                    if ((hgt + wid) & 1) {
                        NROW = -hgt;
                        NCOL = -wid;
                    }
                    else return (-1);
                }
                else {
                    float height = sqrt(minArea * hgt / wid), width = sqrt(minArea * wid / hgt);
                    NROW = (int)height;
                    NCOL = (int)width;
                    if ((NROW^NCOL) & 0x1) {    // already Odd vs. Even
                        if ((NROW*NCOL) < minArea) {
                            NROW++;
                            NCOL++;
                        }
                    }
                    else {      // either both Odd or both Even!
                        if ((height * NCOL) < (width * NROW)) {
                            NCOL++;
                            if ((NROW*NCOL) < minArea) {
                                NCOL--;
                                NROW++;
                            }
                            if ((NROW*NCOL) < minArea) NCOL += 2;
                        }
                        else {
                            NROW++;
                            if ((NROW*NCOL) < minArea) {
                                NROW--;
                                NCOL++;
                            }
                            if ((NROW*NCOL) < minArea) NROW += 2;
                        }
                    }
                    while ((NROW < 7)||(NCOL < 7)) {
                        NROW++;    // making sure NCOL & NROW both >= 7 - padding will take care of the rest
                        NCOL++;
                    }
                }
            }
        }
        else {
            NCOL = wid;
            NROW = (minArea + NCOL-1) / NCOL;
            if (!((NCOL^NROW)&0x1)) NROW++;
            while (NROW < 7) NROW += 2; // making sure NROW >= 7 - padding will take care of the rest
        }
        if (show) printf("Symbol Size (HxW): %d x %d => ",NROW,NCOL);
        nBytes = NROW * ((NCOL+7)>>3);

        if ((nw * 9 + 2) > ((NROW * NCOL)>>1)) return (-1);  // in case hgt & wid are specified (both negative) but too small

        if (fill) {
            int NDOTS, ND, NC, NW, msk;
            long score, topscore;
            NDOTS = (NROW * NCOL)>>1;
            NW = (NDOTS - 2) / 9;
            if ((NW % 3) == 2) NW--;
            NC = (NW / 3) + 2;
            ND = NW - NC;
            if (show) printf("Total # dots = %d\n",NDOTS);
            if (ND > nd) AddPads(cw,nd,ND-nd); // REV 2.00 FIX

            if (!TWIX(0,7,topmsk)) {
                int threshold = (out->rows*out->cols)>>1;
                topscore = LONG_MIN;
                for (msk=3; msk>=0; msk--) {
                    wd[0] = msk;
                    for (i=0; i<ND; i++) wd[i+1] = (CW[i] + i*mask[msk])%GF;
                    rsencode(ND+1,NC);
                    FillDotArray(out,wd,NW+1);

                    score = ScoreArray(out->bitmap,out->rows,out->cols);
                    if (score > topscore) {
                        topscore = score;
                        topmsk = msk;

                        // if topscore now exceeds 1/2 Height x Width, this mask is Acceptable!
                        if (fast) {
                            if (topscore > threshold)
                                break;
                        }
                    }
                    if (fast) {
                        LightAllCorners(out);
                        score = ScoreArray(out->bitmap,out->rows,out->cols);
                        if (score > topscore) {
                            topscore = score;
                            topmsk = msk + 4;

                            // if topscore now exceeds 1/2 Height x Width, this mask is Acceptable!
                            if (topscore > threshold)
                                break;
                        }
                    }
                } // for loop over masks

                if (!fast && topscore <= threshold) {
                    for (msk=3; msk>=0; msk--) {
                        wd[0] = msk;
                        for (i=0; i<ND; i++) wd[i+1] = (CW[i] + i*mask[msk])%GF;
                        rsencode(ND+1,NC);
                        FillDotArray(out,wd,NW+1);
                        LightAllCorners(out);

                        score = ScoreArray(out->bitmap,out->rows,out->cols);
                        if (score > topscore) {
                            topscore = score;
                            topmsk = msk + 4;
                        }
                    }
                }
            }

            wd[0] = topmsk % 4;
            for (i=0; i<ND; i++) wd[i+1] = (CW[i] + i*mask[topmsk % 4])%GF;
            rsencode(ND+1,NC);
            FillDotArray(out,wd,NW+1);
            if (topmsk >= 4)
                LightAllCorners(out);
            if (show) {
                printf("\nFull Char Sequence: ");
                for (i=0; i<ND+1; i++) printf(" %d",wd[i]);
                printf(" |");
                for (; i<NW+1; i++) printf(" %d",wd[i]);
                printf("\nSelected Mask: %d  =>  Score = %ld\n",topmsk,ScoreArray(out->bitmap,out->rows,out->cols));
            }
        }
        free(CW);
    }
    return (nBytes);
}
