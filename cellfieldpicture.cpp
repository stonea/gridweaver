#include "cellfieldpicture.hpp"

#include "svgprinter.hpp"
#include <fstream>
using namespace std;

void initializeModule_cellfieldpicture() {
}

CellFieldPicture::CellFieldPicture(int width, int height) {
    mWidth = width;
    mHeight = height;

    mLabel = new string*[mHeight];
    mStroke = new string*[mHeight];
    mFill = new string*[mHeight];

    for(int i = 0; i < mHeight; i++) {
        mLabel[i] = new string[mWidth];
        mStroke[i] = new string[mWidth];
        mFill[i] = new string[mWidth];

        for(int j = 0; j < mWidth; j++) {
            mStroke[i][j] = "black";
            mFill[i][j] = "white";
            mLabel[i][j]="";
        }
    }

    mBoxX1 = NO_BOX;
    mBoxY1 = NO_BOX;
    mBoxX2 = NO_BOX;
    mBoxY2 = NO_BOX;

    // Calculate radius and spacing based on width and height
    mSpacing = 640 / max(height, width);
    mRadius  = mSpacing / 2 - mSpacing / 10;
}

void CellFieldPicture::saveToFile(string filename) {
    ofstream file(filename.c_str());
    SVGPrinter svg(file);
    
    svg.printHeader();

    // place a box behind cells if specified
    if(mBoxX1 != NO_BOX) {
        int x = (mBoxX1) * mSpacing + mSpacing/2;
        int y = (mHeight - mBoxY2) * mSpacing - mSpacing/2;
        int w = (mBoxX2 - mBoxX1 + 1) * mSpacing;
        int h = (mBoxY2 - mBoxY1 + 1) * mSpacing;

        svg.printRectangle(x, y, w, h, "darkgrey", "lightgrey");
    }
 
    // output the field of cells
    for(int i = 0; i < mHeight; i++) {
        for(int j = 0; j < mWidth; j++) {
            int x = (j+1) * mSpacing;
            int y = (mHeight - i) * mSpacing;
            
            svg.printCircle(x, y, mRadius, mStroke[j][i], mFill[j][i]);
            svg.printCenteredText(x, y, mLabel[j][i], "16");
        }
    }
     
    svg.printFooter();
}

void CellFieldPicture::setLabel(int x, int y, string label) {
    mLabel[x][y] = label;
}

void CellFieldPicture::setStroke(int x, int y, string stroke) {
    mStroke[x][y] = stroke;
}

void CellFieldPicture::setFill(int x, int y, string fill) {
    mFill[x][y] = fill;
}

void CellFieldPicture::setBox(int x1, int y1, int x2, int y2) {
    mBoxX1 = x1; mBoxY1 = y1; mBoxX2 = x2; mBoxY2 = y2;
}

string CellFieldPicture::getLabel(int x, int y) {
   return(mLabel[x][y]);
}

string CellFieldPicture::getStroke(int x, int y) {
   return(mStroke[x][y]);
}

string CellFieldPicture::getFill(int x, int y) {
   return(mFill[x][y]);
}

