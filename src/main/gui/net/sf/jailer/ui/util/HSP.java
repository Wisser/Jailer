//package net.sf.jailer.ui.util;
//
////  public domain function by Darel Rex Finley, 2006
////
////  This function expects the passed-in values to be on a scale
////  of 0 to 1, and uses that same scale for the return values.
////
////  See description/examples at alienryderflex.com/hsp.html
//public class HSP {
//	#define  Pr  .299
//	#define  Pg  .587
//	#define  Pb  .114
//
//
//
//
//	void RGBtoHSP(
//	double  R, double  G, double  B,
//	double *H, double *S, double *P) {
//
//	  //  Calculate the Perceived brightness.
//	  *P=sqrt(R*R*Pr+G*G*Pg+B*B*Pb);
//
//	  //  Calculate the Hue and Saturation.  (This part works
//	  //  the same way as in the HSV/B and HSL systems???.)
//	  if      (R==G && R==B) {
//	    *H=0.; *S=0.; return; }
//	  if      (R>=G && R>=B) {   //  R is largest
//	    if    (B>=G) {
//	      *H=6./6.-1./6.*(B-G)/(R-G); *S=1.-G/R; }
//	    else         {
//	      *H=0./6.+1./6.*(G-B)/(R-B); *S=1.-B/R; }}
//	  else if (G>=R && G>=B) {   //  G is largest
//	    if    (R>=B) {
//	      *H=2./6.-1./6.*(R-B)/(G-B); *S=1.-B/G; }
//	    else         {
//	      *H=2./6.+1./6.*(B-R)/(G-R); *S=1.-R/G; }}
//	  else                   {   //  B is largest
//	    if    (G>=R) {
//	      *H=4./6.-1./6.*(G-R)/(B-R); *S=1.-R/B; }
//	    else         {
//	      *H=4./6.+1./6.*(R-G)/(B-G); *S=1.-G/B; }}}
//
//
//
//	//  public domain function by Darel Rex Finley, 2006
//	//
//	//  This function expects the passed-in values to be on a scale
//	//  of 0 to 1, and uses that same scale for the return values.
//	//
//	//  Note that some combinations of HSP, even if in the scale
//	//  0-1, may return RGB values that exceed a value of 1.  For
//	//  example, if you pass in the HSP color 0,1,1, the result
//	//  will be the RGB color 2.037,0,0.
//	//
//	//  See description/examples at alienryderflex.com/hsp.html
//
//	void HSPtoRGB(
//	double  H, double  S, double  P,
//	double *R, double *G, double *B) {
//
//	  double  part, minOverMax=1.-S ;
//
//	  if (minOverMax>0.) {
//	    if      ( H<1./6.) {   //  R>G>B
//	      H= 6.*( H-0./6.); part=1.+H*(1./minOverMax-1.);
//	      *B=P/sqrt(Pr/minOverMax/minOverMax+Pg*part*part+Pb);
//	      *R=(*B)/minOverMax; *G=(*B)+H*((*R)-(*B)); }
//	    else if ( H<2./6.) {   //  G>R>B
//	      H= 6.*(-H+2./6.); part=1.+H*(1./minOverMax-1.);
//	      *B=P/sqrt(Pg/minOverMax/minOverMax+Pr*part*part+Pb);
//	      *G=(*B)/minOverMax; *R=(*B)+H*((*G)-(*B)); }
//	    else if ( H<3./6.) {   //  G>B>R
//	      H= 6.*( H-2./6.); part=1.+H*(1./minOverMax-1.);
//	      *R=P/sqrt(Pg/minOverMax/minOverMax+Pb*part*part+Pr);
//	      *G=(*R)/minOverMax; *B=(*R)+H*((*G)-(*R)); }
//	    else if ( H<4./6.) {   //  B>G>R
//	      H= 6.*(-H+4./6.); part=1.+H*(1./minOverMax-1.);
//	      *R=P/sqrt(Pb/minOverMax/minOverMax+Pg*part*part+Pr);
//	      *B=(*R)/minOverMax; *G=(*R)+H*((*B)-(*R)); }
//	    else if ( H<5./6.) {   //  B>R>G
//	      H= 6.*( H-4./6.); part=1.+H*(1./minOverMax-1.);
//	      *G=P/sqrt(Pb/minOverMax/minOverMax+Pr*part*part+Pg);
//	      *B=(*G)/minOverMax; *R=(*G)+H*((*B)-(*G)); }
//	    else               {   //  R>B>G
//	      H= 6.*(-H+6./6.); part=1.+H*(1./minOverMax-1.);
//	      *G=P/sqrt(Pr/minOverMax/minOverMax+Pb*part*part+Pg);
//	      *R=(*G)/minOverMax; *B=(*G)+H*((*R)-(*G)); }}
//	  else {
//	    if      ( H<1./6.) {   //  R>G>B
//	      H= 6.*( H-0./6.); *R=sqrt(P*P/(Pr+Pg*H*H)); *G=(*R)*H; *B=0.; }
//	    else if ( H<2./6.) {   //  G>R>B
//	      H= 6.*(-H+2./6.); *G=sqrt(P*P/(Pg+Pr*H*H)); *R=(*G)*H; *B=0.; }
//	    else if ( H<3./6.) {   //  G>B>R
//	      H= 6.*( H-2./6.); *G=sqrt(P*P/(Pg+Pb*H*H)); *B=(*G)*H; *R=0.; }
//	    else if ( H<4./6.) {   //  B>G>R
//	      H= 6.*(-H+4./6.); *B=sqrt(P*P/(Pb+Pg*H*H)); *G=(*B)*H; *R=0.; }
//	    else if ( H<5./6.) {   //  B>R>G
//	      H= 6.*( H-4./6.); *B=sqrt(P*P/(Pb+Pr*H*H)); *R=(*B)*H; *G=0.; }
//	    else               {   //  R>B>G
//	      H= 6.*(-H+6./6.); *R=sqrt(P*P/(Pr+Pb*H*H)); *B=(*R)*H; *G=0.; }}}
//
//
//}
//
//
//
//
////Perceived brightness to Red ratio.
//private const double Pr = .299;
////Perceived brightness to Green ratio.
//private const double Pg = .587;
////Perceived brightness to Blue ratio.
//private const double Pb = .114;
//#endregion
//
////Expected ranges: Hue = 0-359... Other values = 0-1
//public static ColorRGB ToRGB(double hue, double saturation, double perceivedBrightness, double alpha) {
//  //Check values within expected range
//  hue = hue < 0 ? 0 : hue > 359 ? 359 : hue;
//  saturation = saturation < 0 ? 0 : saturation > 1 ? 1 : saturation;
//  perceivedBrightness = perceivedBrightness < 0 ? 0 : perceivedBrightness > 1 ? 1 : perceivedBrightness;
//  alpha = alpha < 0 ? 0 : alpha > 1 ? 1 : alpha;
//  //Conversion
//  var minOverMax = 1 - saturation;
//  double r, g, b;
//  if (minOverMax > 0) {
//      double part;
//      if (hue < 0.166666666666667D) { //R>G>B
//          hue = 6 * (hue - 0); part = 1 + hue * (1 / minOverMax - 1);
//          b = perceivedBrightness / Math.Sqrt(Pr / minOverMax / minOverMax + Pg * part * part + Pb);
//          r = b / minOverMax; g = b + hue * (r - b);
//      }
//      else if (hue < 0.333333333333333D) { //G>R>B
//          hue = 6 * (-hue + 0.333333333333333D); part = 1 + hue * (1 / minOverMax - 1);
//          b = perceivedBrightness / Math.Sqrt(Pg / minOverMax / minOverMax + Pr * part * part + Pb);
//          g = b / minOverMax; r = b + hue * (g - b);
//      }
//      else if (hue < 0.5D) {   //  G>B>R
//          hue = 6 * (hue - 0.333333333333333D); part = 1 + hue * (1 / minOverMax - 1);
//          r = perceivedBrightness / Math.Sqrt(Pg / minOverMax / minOverMax + Pb * part * part + Pr);
//          g = r / minOverMax; b = r + hue * (g - r);
//      }
//      else if (hue < 0.666666666666667D) { //B>G>R
//          hue = 6 * (-hue + 0.666666666666667D); part = 1 + hue * (1 / minOverMax - 1);
//          r = perceivedBrightness / Math.Sqrt(Pb / minOverMax / minOverMax + Pg * part * part + Pr);
//          b = r / minOverMax; g = r + hue * (b - r);
//      }
//      else if (hue < 0.833333333333333D) { //B>R>G
//          hue = 6 * (hue - 0.666666666666667D); part = 1 + hue * (1 / minOverMax - 1);
//          g = perceivedBrightness / Math.Sqrt(Pb / minOverMax / minOverMax + Pr * part * part + Pg);
//          b = g / minOverMax; r = g + hue * (b - g);
//      }
//      else { //R>B>G
//          hue = 6 * (-hue + 1D); part = 1 + hue * (1 / minOverMax - 1);
//          g = perceivedBrightness / Math.Sqrt(Pr / minOverMax / minOverMax + Pb * part * part + Pg);
//          r = g / minOverMax; b = g + hue * (r - g);
//      }
//  }
//  else {
//      if (hue < 0.166666666666667D) { //R>G>B
//          hue = 6 * (hue - 0D); r = Math.Sqrt(perceivedBrightness * perceivedBrightness / (Pr + Pg * hue * hue)); g = r * hue; b = 0;
//      }
//      else if (hue < 0.333333333333333D) { //G>R>B
//          hue = 6 * (-hue + 0.333333333333333D); g = Math.Sqrt(perceivedBrightness * perceivedBrightness / (Pg + Pr * hue * hue)); r = g * hue; b = 0;
//      }
//      else if (hue < 0.5D) { //G>B>R
//          hue = 6 * (hue - 0.333333333333333D); g = Math.Sqrt(perceivedBrightness * perceivedBrightness / (Pg + Pb * hue * hue)); b = g * hue; r = 0;
//      }
//      else if (hue < 0.666666666666667D) { //B>G>R
//          hue = 6 * (-hue + 0.666666666666667D); b = Math.Sqrt(perceivedBrightness * perceivedBrightness / (Pb + Pg * hue * hue)); g = b * hue; r = 0;
//      }
//      else if (hue < 0.833333333333333D) { //B>R>G
//          hue = 6 * (hue - 0.666666666666667D); b = Math.Sqrt(perceivedBrightness * perceivedBrightness / (Pb + Pr * hue * hue)); r = b * hue; g = 0;
//      }
//      else { //R>B>G
//          hue = 6 * (-hue + 1D); r = Math.Sqrt(perceivedBrightness * perceivedBrightness / (Pr + Pb * hue * hue)); b = r * hue; g = 0;
//      }
//  }
//  return new ColorRGB(r, g, b, alpha);
//}
//
////Expected ranges: 0-1 on all components
//public static ColorHSP FromRGB(double red, double green, double blue, double alpha) {
//  //Guarantee RGB values are in the correct ranges
//  red = red < 0 ? 0 : red > 1 ? 1 : red;
//  green = green < 0 ? 0 : green > 1 ? 1 : green;
//  blue = blue < 0 ? 0 : blue > 1 ? 1 : blue;
//  alpha = alpha < 0 ? 0 : alpha > 1 ? 1 : alpha;
//  //Prepare & cache values for conversion
//  var max = MathExtensions.Max(red, green, blue);
//  var min = MathExtensions.Min(red, green, blue);
//  var delta = max - min;
//  double h, s, p = Math.Sqrt(0.299 * red + 0.587 * green + 0.114 * blue);
//  //Conversion
//  if (delta.Equals(0)) h = 0;
//  else if (max.Equals(red)) {
//      h = (green - blue) / delta % 6;
//  }
//  else if (max.Equals(green)) h = (blue - red) / delta + 2;
//  else h = (red - green) / delta + 4;
//  h *= 60;
//  if (h < 0) h += 360;
//  if (p.Equals(0))
//      s = 0;
//  else
//      s = delta / p;
//  //Result
//  return new ColorHSP(h, s, p, alpha);
//}