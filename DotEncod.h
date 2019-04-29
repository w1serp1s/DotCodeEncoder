/* ======================================================================= */
/**	 "DotEncod.h" -- DotCode Encoding Module headers 10/21/08  (AL)	  **/
/* ======================================================================= */

#if defined(__cplusplus)
extern "C" {
#endif

/*-------------------------------------------------------------------------*/
/**************   PRIMARY DATA & PARAMETER INPUT STRUCTURE   ***************/
/*-------------------------------------------------------------------------*/
typedef struct {
	unsigned char *msg;	// a pointer to the Input Message
	int msglen;				// .. & the # of bytes it contains
	int hgt, wid; 			// dot array Height, Width, or Aspect Ratio
} inputs;
//  NOTE: the rules for specifying symbol Height & Width are as follows:
//		"hgt"		"wid"		------------------- Meaning ------------------------
//		 >0		  0		symbol Height = "hgt" (must be >= 5)
//		  0		 >0		symbol Width = "wid" (must be >= 5)
//		 >0		 >0		symbol Height/Width approximates "hgt"/"wid"
//		  0		  0		symbol Height/Width approximates 2/3 (default)
//		 <0		 <0		symbol Height = -"hgt" & Width = -"wid"
//						(this final case might prove too small or illegal!)

/*-------------------------------------------------------------------------*/
/**************   PRIMARY SYMBOL PATTERN OUTPUT STRUCTURE   ****************/
/*-------------------------------------------------------------------------*/
typedef struct {
	unsigned char *bitmap; /* a pointer to the Output Bitmap	*/
	int cols;				/* # of bits per row	*/
	int rows;				/* # of rows in the pattern	*/
} output;
// NOTE: each row of the bitmap is "(cols+7)/8" bytes padded with trailing
//			"0"s, thus the size of "bitmap" in bytes is "(cols+7)/8 * rows"

/*-------------------------------------------------------------------------*/
/*****************   PROTOTYPE OF THE ENCODING FUNCTION    *****************/
/*-------------------------------------------------------------------------*/
int DotCodeEncode (inputs *in, output *out, int literal, int topmsk, int fill, int show, int fast);
// Notes:
//		"literal" is nornally 0, but when non-zero causes the input message to
//					be encoded literally, not interpreting "#x" sequences & thus
//					incapable of encoding the FNCx characters
//		"topmsk" is normally -1, but values 0 to 3 -dictate- the symbols mask
//					(generally for illustrative purposes only)
//		"fill" determines if the symbol shall be filled or just "sized"
//		"show" determines if symbol encoding details shall be output
//					(generally for dignostic purposes only)
//      "fast" allows short-circuiting if score is high enough
//		DotCodeEncode() returns the size of the symbol bitmap in chars

/*-------------------------------------------------------------------------*/
/*********   HANDY MACROS REFERRING TO INPUT & OUTPUT VARIABLES    *********/
/*-------------------------------------------------------------------------*/
#define MSG	 (in->msg)
#define LEN	 (in->msglen)
#define HGT  (in->hgt)
#define WID	 (in->wid)

#define BMAP (out->bitmap)
#define NROW (out->rows)
#define NCOL (out->cols)

#if defined(__cplusplus)
}
#endif
