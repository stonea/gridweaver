/******************************************************************************
 * GRIDGEN: Grid Generating Compiler
 * By: Andy Stone (aistone@gmail.com)
 * (C) Copyright 2011 Colorado State University
 *****************************************************************************/
#include "grid.hpp"
#include "binIO.hpp"
#include "utils.hpp"
#include "environment.hpp"
//#include <stdlib.h>
//#include <math.h>
using namespace std;

void initializeModule_grid() {
}


int x, y;
Subgrid *sg;

void GlobalCoordinate::print(std::ostream &out) const {
}

void GlobalCoordinate::printSimp(std::ostream &out) const {
    out << "(" << sg->getID() << ", <" << x << ", " << y << ">)";
}

bool GlobalCoordinate::operator<(const struct GlobalCoordinate& rhs) const {
    return (sg->getID()  < rhs.sg->getID()) ||
           (sg->getID() == rhs.sg->getID() && x < rhs.x) ||
           (sg->getID() == rhs.sg->getID() && x == rhs.x && y < rhs.y);
}


Region::Region() :
    mLowX(0), mLowY(0), mHighX(0), mHighY(0),
    mOrientation(BL),
    mEmpty(true)
{ }

Region::Region(int x1, int y1, int x2, int y2) {
    reshape(x1, y1, x2, y2);
}

void Region::print(ostream &out) const {
    printObj_start(out, "Region", "");
    printObj_property(out, "lowX", mLowX);
    printObj_property(out, "lowY", mLowY);
    printObj_property(out, "highX", mHighX);
    printObj_property(out, "highY", mHighY);
    printObj_property(out, "empty", mEmpty);
    printObj_property(out, "orientation", mOrientation);
    printObj_end(out);
}

void Region::printSimp(ostream &out) const {
    out << "(" << mLowX << "," << mLowY << " - "
               << mHighX << "," << mHighY << "  ";

    switch(mOrientation) {
        case BL: out << "BL)"; break;
        case BR: out << "BR)"; break;
        case TL: out << "TL)"; break;
        case TR: out << "TR)"; break;
    }
}

void Region::output(ostream &out) const {
    BinIO::out(out, mLowX);
    BinIO::out(out, mLowY);
    BinIO::out(out, mHighX);
    BinIO::out(out, mHighY);
    BinIO::out(out, mEmpty);
    BinIO::out(out, (int)mOrientation);
}

void Region::input(istream &in) {
    int iOrientation;

    BinIO::in(in, mLowX);
    BinIO::in(in, mLowY);
    BinIO::in(in, mHighX);
    BinIO::in(in, mHighY);
    BinIO::in(in, mEmpty);
    BinIO::in(in, iOrientation);
    mOrientation = (Orientation)iOrientation;
}

int Region::keyX() const {
         if(mOrientation == BL) { return mLowX;  }
    else if(mOrientation == BR) { return mHighX; }
    else if(mOrientation == TL) { return mLowX;  }
    else if(mOrientation == TR) { return mHighX; }
}

int Region::keyY() const {
         if(mOrientation == BL) { return mLowY;  }
    else if(mOrientation == BR) { return mLowY;  }
    else if(mOrientation == TL) { return mHighY; }
    else if(mOrientation == TR) { return mHighY; }
}

bool Region::contains(int x, int y) const {
    if(mEmpty) { return false; }
    return(x >= lowX() && x <= highX() && y >= lowY() && y <= highY());
}

bool Region::isHorizFlipRelativeTo(const Region &rhs) const {
    // If this region has a left to right orientation check if rhs
    // has a right to left orientation
    if(mOrientation == BL || mOrientation == TL) {
        return(rhs.orientation() == BR || rhs.orientation() == TR);
    }
    // Otherwise this region has a right to left orientation so check if
    // rhs has a left to right orientation
    return(rhs.orientation() == BL || rhs.orientation() == TL);
}

bool Region::isVertFlipRelativeTo(const Region &rhs) const {
    // If this region has a bottom to top orientation check if rhs
    // has a top to bottom orientation
    if(mOrientation == BL || mOrientation == BR) {
        return(rhs.orientation() == TL || rhs.orientation() == TR);
    }
    // Otherwise this region has a top to bottom orientation so check if
    // rhs has a bottom to top orientation
    return(rhs.orientation() == BL || rhs.orientation() == BR);
}

bool Region::is90degFlip(const Region &rhs) const {
    return ((highX() - lowX()) == (rhs.highY() - rhs.lowY()));
}

void Region::reorient(Region tgt, int &x, int &y, bool accountForSelf,
                      bool flip) const
{
    int m11;  int m12; int m21;  int m22;
    int n11;  int n12; int n21;  int n22;
    int r11;  int r12; int r21;  int r22;

    if(accountForSelf) {
        switch(orientation()) {
            case BL:
                m11 =  1;    m12 =  0;
                m21 =  0;    m22 =  1;
                break;

            case BR:
                m11 = -1;    m12 =  0;
                m21 =  0;    m22 =  1;
                break;

            case TL:
                m11 =  1;    m12 =  0;
                m21 =  0;    m22 = -1;
                break;

            case TR:
                m11 = -1;    m12 =  0;
                m21 =  0;    m22 = -1;
                break;
        }
    } else {
        m11 =  1;    m12 =  0;
        m21 =  0;    m22 =  1;
    }

    switch(tgt.orientation()) {
        case BL:
            n11 =  1;    n12 =  0;
            n21 =  0;    n22 =  1;
            break;

        case BR:
            n11 = -1;    n12 =  0;
            n21 =  0;    n22 =  1;
            break;

        case TL:
            n11 =  1;    n12 =  0;
            n21 =  0;    n22 = -1;
            break;

        case TR:
            n11 = -1;    n12 =  0;
            n21 =  0;    m22 = -1;
            break;
    }

    if(flip) {
        r11 = (m11 * n12) + (m12 * n22);    r12 = (m11 * n11) + (m12 * n21);
        r21 = (m21 * n12) + (m22 * n22);    r22 = (m21 * n11) + (m22 * n21);
    } else {
        r11 = (m11 * n11) + (m12 * n21);    r12 = (m11 * n12) + (m12 * n22);
        r21 = (m21 * n11) + (m22 * n21);    r22 = (m21 * n12) + (m22 * n22);
    }

    int oldX = x;
    int oldY = y;

    x = r11 * oldX + r12 * oldY;
    y = r21 * oldX + r22 * oldY;
}

Region Region::intersect(const Region &rhs) const {
    int x1, x2, y1, y2;
    Region res;
    
    x1 = MAX(lowX(),  rhs.lowX());
    y1 = MAX(lowY(),  rhs.lowY());
    x2 = MIN(highX(), rhs.highX());
    y2 = MIN(highY(), rhs.highY());

    // If there's an overlap return the overlapping region otherwise return an
    // empty rectangle
    if(x1 <= x2 && y1 <= y2) {
        res.reshape(x1, y1, x2, y2);
    }

    return res;
}

Region Region::analogousRegion(const Region &reg, const Region &tgt) const {
    Region res;
    int x1, y1, x2, y2;

    // If the embedded region is empty or doesn't intersect with the source
    // region return an empty region
    if(isEmpty() || intersect(reg).isEmpty()) { return res; }

    x1 = reg.mLowX  - mLowX;        x2 = reg.mHighX - mLowX;
    y1 = reg.mLowY  - mLowY;        y2 = reg.mHighY - mLowY;

    //cout << "x1/y1: " << x1 << " " << y1 << "\t"
    //     << "x2/y2: " << x2 << " " << y2 << endl;

    if(is90degFlip(tgt)) {
        reorient(tgt, x1,y1, true, true);
        reorient(tgt, x2,y2, true, true);
    }
    else {
        reorient(tgt, x1,y1);
        reorient(tgt, x2,y2);
    }

    //cout << "Reoriented: " << endl;
    //cout << "x1/y1: " << x1 << " " << y1 << "\t"
    //     << "x2/y2: " << x2 << " " << y2 << endl;

    x1 += tgt.keyX();   x2 += tgt.keyX();
    y1 += tgt.keyY();   y2 += tgt.keyY();

//    reorient();

    // Otherwise determine the analogy based on the targets orientation
//    if(! tgt.isHorizFlipRelativeTo(reg)) {
//        x1 = tgt.mLowX + (mLowX  - reg.mLowX);
//        x2 = tgt.mLowX + (mHighX - reg.mLowX);
//    } else {
//        x1 = tgt.mHighX - (mLowX  - reg.mLowX);
//        x2 = tgt.mHighX - (mHighX - reg.mLowX);
//    }

//    if(! tgt.isVertFlipRelativeTo(reg)) {
//        y1 = tgt.mLowY + (mLowY  - reg.mLowY);
//        y2 = tgt.mLowY + (mHighY - reg.mLowY);
//    } else {
//        y1 = tgt.mHighY - (mLowY  - reg.mLowY);
//        y2 = tgt.mHighY - (mHighY - reg.mLowY);
//    }

    res.reshape(x1, y1, x2, y2);

    return res;
}

void Region::cutAnalogously(const Region &rhs, const Region &tgt,
                            Region &resInSrc, Region &resInTgt) const
{
    int x1, y1, x2, y2;

    // If the embedded region is empty or doesn't intersect with the source
    // region return an empty region
    if(isEmpty() || rhs.isEmpty() || intersect(rhs).isEmpty()) {
        resInSrc.clear();
        resInTgt.clear();
        return;
    }

    // Do the cut on the source side
    resInSrc = intersect(rhs);

    // Do cut on the target side
    //x1 = (MAX(lowX(),  rhs.lowX())  - lowX());
    //x2 = (MIN(highX(), rhs.highX()) - lowX());
    //y1 = (MAX(lowY(),  rhs.lowY())  - lowY());
    //y2 = (MIN(highY(), rhs.highY()) - lowY());

    //x1 = resInSrc.lowX()  - keyX();
    //y1 = resInSrc.lowY()  - keyY();
    //x2 = resInSrc.highX() - keyX();
    //y2 = resInSrc.highY() - keyY();

    x1 = resInSrc.lowX()  - lowX();
    y1 = resInSrc.lowY()  - lowY();
    x2 = resInSrc.highX() - lowX();
    y2 = resInSrc.highY() - lowY();

    if(is90degFlip(tgt)) {
        reorient(tgt, x1,y1, false, true);
        reorient(tgt, x2,y2, false, true);
    }
    else {
        reorient(tgt, x1,y1, false);
        reorient(tgt, x2,y2, false);
    }
    x1 += tgt.keyX();   x2 += tgt.keyX();
    y1 += tgt.keyY();   y2 += tgt.keyY();

    resInTgt.reshape(x1, y1, x2, y2);

    // Keep orientation of what you're cutting from
    resInSrc.mOrientation = mOrientation;
    resInTgt.mOrientation = tgt.mOrientation;

    // Do the cut on the target side; determine the analogy based on the targets
    // orientation
    //if(! isHorizFlipRelativeTo(rhs)) {
    //    x1 = tgt.mLowX + (MAX(lowX(),  rhs.lowX())  - lowX());
    //    x2 = tgt.mLowX + (MIN(highX(), rhs.highX()) - lowX());
    //} else {
    //    x1 = tgt.mHighX - (MAX(lowX(),  rhs.lowX())  - lowX());
    //    x2 = tgt.mHighX - (MIN(highX(), rhs.highX()) - lowX());
    //}

    //if(! isVertFlipRelativeTo(rhs)) {
    //    y1 = tgt.mLowY + (MAX(lowY(),  rhs.lowY())  - lowY());
    //    y2 = tgt.mLowY + (MIN(highY(), rhs.highY()) - lowY());
    //} else {
    //    y1 = tgt.mHighY - (MAX(lowY(),  rhs.lowY())  - lowY());
    //    y2 = tgt.mHighY - (MIN(highY(), rhs.highY()) - lowY());
    //}
}

void Region::flipHoriz() {
    switch(mOrientation) {
        case BL: mOrientation = BR; break;
        case BR: mOrientation = BL; break;
        case TL: mOrientation = TR; break;
        case TR: mOrientation = TL; break;
    }
}

void Region::flipVert() {
    switch(mOrientation) {
        case BL: mOrientation = TL; break;
        case BR: mOrientation = TR; break;
        case TL: mOrientation = BL; break;
        case TR: mOrientation = BR; break;
    }
}

void Region::reshape(int x1, int y1, int x2, int y2) {
    // Infer orientation from order of indices:

    // Bottom to top orientations
    if(x1 <= x2 && y1 <= y2) {
        mOrientation = BL;
        mLowX = x1; mHighX = x2; mLowY = y1; mHighY = y2;
    }
    else if(x2 <= x1 && y1 <= y2) {
        mOrientation = BR;
        mLowX = x2; mHighX = x1; mLowY = y1; mHighY = y2;
    }

    // Top to top orientations
    else if(x1 <= x2 && y2 <= y1) {
        mOrientation = TL;
        mLowX = x1; mHighX = x2; mLowY = y2; mHighY = y1;
    }
    else if(x2 <= x1 && y2 <= y1) {
        mOrientation = TR;
        mLowX = x2; mHighX = x1; mLowY = y2; mHighY = y1;
    }

    mEmpty = false;
}

void Region::expand(
    int deltaLowX, int deltaLowY, int deltaHighX, int deltaHighY)
{
    mLowX  += deltaLowX;
    mLowY  += deltaLowY;
    mHighX += deltaHighX;
    mHighY += deltaHighY;
}

void Region::expandToMultiple(int multipleX,  int multipleY) {
    mLowX  = ROUND_DOWN_BLK(mLowX,  multipleX);
    mLowY  = ROUND_DOWN_BLK(mLowY,  multipleY);
    mHighX =   ROUND_UP_BLK(mHighX, multipleX);
    mHighY =   ROUND_UP_BLK(mHighY, multipleY);
}

void Region::cut(const Region &rhs) {
    if(isEmpty() || rhs.isEmpty()) {
        mEmpty = true;
        return;
    }

    mLowX  = MAX(mLowX,  rhs.lowX());
    mLowY  = MAX(mLowY,  rhs.lowY());
    mHighX = MIN(mHighX, rhs.highX());
    mHighY = MIN(mHighY, rhs.highY());

    // Flip if necessary
    if(isHorizFlipRelativeTo(rhs)) { flipHoriz(); }
    if(isVertFlipRelativeTo(rhs))  { flipVert(); }
}

void Region::translate(int deltaX, int deltaY) {
    mLowX  += deltaX;
    mLowY  += deltaY;
    mHighX += deltaX;
    mHighY += deltaY;
}

void Region::clear() {
    mLowX = 0;
    mLowY = 0;
    mHighX = 0;
    mHighY = 0;
    mOrientation = BL;
    mEmpty = true;
}

Neighbor::Neighbor(const string &name) :
    mName(name) { }

void Neighbor::print(ostream &out) const {
    printObj_start(out, "Neighbor", mName);
    printObj_property(out, "x", mX);
    printObj_property(out, "y", mY);
    printObj_end(out);
}

void Neighbor::printSimp(ostream &out) const {
    out << mName;
}

void Neighbor::output(ostream &out) const {
    BinIO::out(out, mName);
    BinIO::out(out, mX);
    BinIO::out(out, mY);
}

void Neighbor::input(istream &in) {
    BinIO::in(in, mName);
    BinIO::in(in, mX);
    BinIO::in(in, mY);
}


Subgrid::Subgrid(const string &name, int sgid) :
    mName(name),
    mW(0),
    mH(0),
    mSGID(sgid)
    { }

void Subgrid::print(ostream &out) const {
    printObj_start(out, "Subgrid", mName);
    printObj_property(out, "w", mW);
    printObj_property(out, "h", mH);
    printObj_end(out);
}

void Subgrid::printSimp(ostream &out) const {
    out << mName;
}

void Subgrid::output(ostream &out) const {
    BinIO::out(out, mName);
    BinIO::out(out, mW);
    BinIO::out(out, mH);
}

void Subgrid::input(istream &in) {
    BinIO::in(in, mName);
    BinIO::in(in, mW);
    BinIO::in(in, mH);
}

Region Subgrid::topRegion() const {
    return Region(1, h(), w(), h());
}

Region Subgrid::bottomRegion() const {
    return Region(1, 1, w(), 1);
}

Region Subgrid::leftRegion() const {
    return Region(1, 1, 1, h());
}

Region Subgrid::rightRegion() const {
    return Region(w(), 1, w(), h());
}

Region Subgrid::topGhostRegion() const {
    return Region(1, h()+1, w(), h()+1);
}

Region Subgrid::bottomGhostRegion() const {
    return Region(1, 0, w(), 0);
}

Region Subgrid::leftGhostRegion() const {
    return Region(0, 1, 0, h());
}

Region Subgrid::rightGhostRegion() const {
    return Region(w()+1, 1, w()+1, h());
}

bool Subgrid::contains(int x, int y) {
    return (x >= 1 && y >= 1 && x <= mW && y <= mH);
}

Region Subgrid::region() const {
    return Region(1, 1, w(), h());
}


Grid::Grid(const string &name) :
    mName(name) { }

void Grid::print(ostream &out) const {
    printObj_start(out, "Grid", mName);
    printObj_property(out, "subgrids");
        printPtrs(out, mSubgrids.begin(), mSubgrids.end());
    printObj_property(out, "srcRegions");
        printObjs(out, mBorderSrcRegions.begin(), mBorderSrcRegions.end());
    printObj_property(out, "srcSubgrids");
        printPtrs(out, mBorderSrcSubgrids.begin(), mBorderSrcSubgrids.end());
    printObj_property(out, "tgtRegions");
        printObjs(out, mBorderTgtRegions.begin(), mBorderTgtRegions.end());
    printObj_property(out, "tgtSubgrids");
        printPtrs(out, mBorderTgtSubgrids.begin(), mBorderTgtSubgrids.end());
    printObj_property(out, "rotations");
        printVals(out, mBorderRotation.begin(), mBorderRotation.end());
    printObj_end(out);
}

void Grid::printSimp(ostream &out) const {
    out << mName;
}

void Grid::output(ostream &out) const {
    BinIO::out(out, mName);
    BinIO::outIdents(out, mSubgrids.begin(), mSubgrids.end());
    BinIO::out(out, mBorderSrcRegions.begin(), mBorderSrcRegions.end());
    BinIO::outIdents(out, mBorderSrcSubgrids.begin(),
                     mBorderSrcSubgrids.end());
    BinIO::out(out, mBorderTgtRegions.begin(), mBorderTgtRegions.end());
    BinIO::outIdents(out, mBorderTgtSubgrids.begin(),
                     mBorderTgtSubgrids.end());
    BinIO::out(out, mBorderRotation.begin(), mBorderRotation.end());
}

void Grid::input(istream &in) {
    int tmp;

    BinIO::in(in, mName);
    BinIO::inIdents(in, back_inserter(mSubgrids),
                    &Environment::getSubgrid);
    BinIO::in(in, back_inserter(mBorderSrcRegions), Region());
    BinIO::inIdents(in, back_inserter(mBorderSrcSubgrids),
                    &Environment::getSubgrid);
    BinIO::in(in, back_inserter(mBorderTgtRegions), Region());
    BinIO::inIdents(in, back_inserter(mBorderTgtSubgrids),
                    &Environment::getSubgrid);
    BinIO::in(in, back_inserter(mBorderRotation), tmp);
}

Subgrid *Grid::subgrid(int sgid) const {
    // Perform a bounds check
    if(sgid <= 0 || sgid > mSubgrids.size()) {
        error(ERR_SG__INVALID_SGID, str(sgid));
    }

    return mSubgrids[sgid-1];
}

bool Grid::containsSubgrid(Subgrid *sg) const {
    // Iterate through all subgrids and see if one matches sg
    for(vector<Subgrid*>::const_iterator i = mSubgrids.begin();
        i != mSubgrids.end(); i++)
    {
        if(*i == sg) {
            return true;
        }
    }
    return false;
}


GlobalCoordinate Grid::resolveBMap(const GlobalCoordinate &src) const {
    for(int i = 0; i < mBorderSrcRegions.size(); i++) {
        if(mBorderSrcSubgrids[i] != src.sg) continue;
        if(mBorderSrcRegions[i].contains(src.x, src.y)) {

            Region srcRegion(src.x,src.y,src.x,src.y);
            Region res = 
                mBorderSrcRegions[i].analogousRegion(
                    srcRegion, mBorderTgtRegions[i]);

            return GlobalCoordinate(
                mBorderTgtSubgrids[i],
                res.lowX(),
                res.lowY());
        }
    }

    return src;
}

void Grid::addSubgrid(Subgrid *sg) {
    mSubgrids.push_back(sg);
}

void Grid::addBorder(const Region &srcRegion, Subgrid *srcSG,
                     const Region &tgtRegion, Subgrid *tgtSG,
                     int rotation)
{
    // Check validity of parameters
    if(!containsSubgrid(srcSG) || !containsSubgrid(tgtSG)) {
        error(ERR_GRID__UNKNOWN_SUBGRID);
    }

    // Add the border
    mBorderSrcRegions.push_back(srcRegion);
    mBorderSrcSubgrids.push_back(srcSG);
    
    mBorderTgtRegions.push_back(tgtRegion);
    mBorderTgtSubgrids.push_back(tgtSG);

    mBorderRotation.push_back(rotation);
}

