#include "svgprinter.hpp"

void initializeModule_svgprinter() {
}

SVGPrinter::SVGPrinter(ostream &out) :
    mOut(out)
{
}

void SVGPrinter::printHeader() {
    mOut << "<?xml version=\"1.0\" standalone=\"no\"?>" << endl;
    mOut << "<!DOCTYPE svg PUBLIC \"-//W3C//DTD SVG 1.1//EN\"" << endl;
    mOut << "\"http://www.w3.org/Graphics/SVG/1.1/DTD/svg11.dtd\">" << endl;
    mOut << "" << endl;
    mOut << "<svg xmlns=\"http://www.w3.org/2000/svg\" version=\"1.1\">" << endl;
}

void SVGPrinter::printFooter() {
   mOut << "</svg>" << endl;
}

void SVGPrinter::printCircle(int x, int y, int r, string stroke, string fill) {
    mOut << "<circle cx=\"" << x << "\" cy=\"" << y << "\" r=\"" << r << "\" "
         << "stroke=\"" << stroke << "\" fill=\"" << fill << "\" />" << endl;
}

void SVGPrinter::printRectangle(
    int x, int y, int w, int h, string stroke, string fill)
{
    mOut << "<rect "
         << "x=\"" << x << "\" "
         << "y=\"" << y << "\" "
         << "width=\"" << w << "\" "
         << "height=\"" << h << "\" "
         << "fill=\"" << fill << "\" "
         << "stroke=\"" << stroke << "\" />" << endl;
}

void SVGPrinter::printCenteredText(int x, int y, string text, string fontSize) {
    mOut << "<text x=\"" << x << "\" y=\"" << y
         << "\" alignment-baseline=\"central\" "
         << "text-anchor=\"middle\"";
    if(fontSize != "") {
        mOut << " font-size = \"" << fontSize << "\">" << endl;
    } else {
        mOut << ">" << endl;
    }
    mOut << text << endl;
    mOut << "</text>" << endl;
}
