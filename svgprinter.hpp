/******************************************************************************
 * GRIDGEN: Grid Generating Compiler
 * By: Andy Stone (aistone@gmail.com)
 * (C) Copyright 2011 Colorado State University
 *****************************************************************************/
/** \addtogroup IO
 *  @{
 */

#ifndef SVGPRINTER_HPP_
#define SVGPRINTER_HPP_

#include <iostream>
using namespace std;

/**
 * Initialization function for the svgprinter module.
 */
void initializeModule_svgprinter();

/**
 * This printer is used to output simple shapes and text into an SVG file.
 * A list of valid colors for stroke and fill properties is available
 * at: <http://www.december.com/html/spec/colorsvg.html>
 */
class SVGPrinter {
  public:
    // =======================
    // - [Construction] -
    // =======================
    /** @name Construction */
    ////@{
        /** Create a new SVG printer that outputs to the specified stream */
        SVGPrinter(ostream &out);
    ///@}

    // =======================
    // - [Printing] -
    // =======================
    /** @name Printing */
    ////@{
        /**
         * Print the SVG header to the stream. Note: This must be called
         * before executing any other print functions.
         **/
        void printHeader();

        /**
         * Print the SVG footer to the stream.  Note: This must be called
         * after printing the contents of the SVG object.
         **/
        void printFooter();

        /** Print a circle **/
        void printCircle(int x, int y, int r, string stroke, string fill);

        /** Print a rectangle **/
        void printRectangle(int x, int y, int w, int h,
                            string stroke, string fill);

        /** Print text centered around a specified point **/
        void printCenteredText(int x, int y, string text, string fontsize="");
    ///@}
    
  private:
    ostream &mOut;
};

#endif
/** @}*/
