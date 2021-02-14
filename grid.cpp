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
         if(mOrientation == BL) { return mLowX; }
    else if(mOrientation == BR) { return mLowX; }
    else if(mOrientation == TL) { return mLowX; }
    else if(mOrientation == TR) { return mLowX; }
}

int Region::keyY() const {
         if(mOrientation == BL) { return mLowY; }
    else if(mOrientation == BR) { return mLowY; }
    else if(mOrientation == TL) { return mHighY; }
    else if(mOrientation == TR) { return mHighY; }
}

bool Region::contains(int x, int y) const {
    if(mEmpty) { return false; }
    return(x >= lowX() && x <= highX() && y >= lowY() && y <= highY());
}

void Region::reshape(int x1, int y1, int x2, int y2) {
    // Infer orientation from order of indices
    if(x1 <= x2 && y1 <= y2) {
        mOrientation = BL;
        mLowX = x1; mLowY = y1; mHighX = x2; mHighY = y2;
    }
    else if(x2 <= x1 && y1 <= y2) {
        mOrientation = BR;
        mLowX = x2; mLowY = y1; mHighX = x1; mHighY = y2;
    }
    else if(x1 <= x2 && y2 <= y1) {
        mOrientation = TL;
        mLowX = x1; mLowY = y2; mHighX = x2; mHighY = y1;
    }
    else if(x2 <= x1 && y2 <= y1) {
        mOrientation = BL;
        mLowX = x2; mLowY = y2; mHighX = x1; mHighY = y1;
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
}

void Region::translate(int deltaX, int deltaY) {
    mLowX  += deltaX;
    mLowY  += deltaY;
    mHighX += deltaX;
    mHighY += deltaY;
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


Subgrid::Subgrid(const string &name) :
    mName(name),
    mW(0),
    mH(0) { }

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

void Grid::placeAdjacentLR(Subgrid *sgL, Subgrid *sgR) {
    // Check validity of parameters
    if(!containsSubgrid(sgL) || !containsSubgrid(sgR)) {
        error(ERR_GRID__UNKNOWN_SUBGRID);
    }

    addBorder(sgL->rightGhostRegion(), sgL, sgR->leftRegion(),  sgR, 0);
    addBorder(sgR->leftGhostRegion(),  sgR, sgL->rightRegion(), sgL, 0);
}

void Grid::placeAdjacentRL(Subgrid *sgR, Subgrid *sgL) {
    // Check validity of parameters
    if(!containsSubgrid(sgR) || !containsSubgrid(sgL)) {
        error(ERR_GRID__UNKNOWN_SUBGRID);
    }

    addBorder(sgL->rightGhostRegion(), sgL, sgR->leftRegion(),  sgR, 0);
    addBorder(sgR->leftGhostRegion(),  sgR, sgL->rightRegion(), sgL, 0);
}

void Grid::placeAdjacentTB(Subgrid *sgT, Subgrid *sgB) {
    // Check validity of parameters
    if(!containsSubgrid(sgT) || !containsSubgrid(sgB)) {
        error(ERR_GRID__UNKNOWN_SUBGRID);
    }

    addBorder(sgT->bottomGhostRegion(), sgT, sgB->topRegion(),    sgB, 0);
    addBorder(sgB->topGhostRegion(),    sgB, sgT->bottomRegion(), sgT, 0);
}

void Grid::placeAdjacentBT(Subgrid *sgB, Subgrid *sgT) {
    // Check validity of parameters
    if(!containsSubgrid(sgB) || !containsSubgrid(sgT)) {
        error(ERR_GRID__UNKNOWN_SUBGRID);
    }

    addBorder(sgT->bottomGhostRegion(), sgT, sgB->topRegion(),    sgB, 0);
    addBorder(sgB->topGhostRegion(),    sgB, sgT->bottomRegion(), sgT, 0);
}

void Grid::connectTtoB(Subgrid *sg1, Subgrid *sg2) {
    // Check validity of parameters
    if(!containsSubgrid(sg1) || !containsSubgrid(sg2)) {
        error(ERR_GRID__UNKNOWN_SUBGRID);
    }
    
    // connectTtoB is equivalent to placeAdjacentBT
    placeAdjacentBT(sg1, sg2);
}

void Grid::connectRtoL(Subgrid *sg1, Subgrid *sg2) {
    // Check validity of parameters
    if(!containsSubgrid(sg1) || !containsSubgrid(sg2)) {
        error(ERR_GRID__UNKNOWN_SUBGRID);
    }

    // connectRtoL is equivalent to placeAdjacentLR
    placeAdjacentLR(sg1, sg2);
}

void Grid::connectBtoT(Subgrid *sg1, Subgrid *sg2) {
    // Check validity of parameters
    if(!containsSubgrid(sg1) || !containsSubgrid(sg2)) {
        error(ERR_GRID__UNKNOWN_SUBGRID);
    }

    // connectBtoT is equivalent to placeAdjacentTB
    placeAdjacentTB(sg1, sg2);
}

void Grid::connectLtoR(Subgrid *sg, Subgrid *sg2) {
    // Check validity of parameters
    if(!containsSubgrid(sg) || !containsSubgrid(sg2)) {
        error(ERR_GRID__UNKNOWN_SUBGRID);
    }

    // connectLtoR is equivalent to placeAdjacentRL
    connectRtoL(sg, sg2);
}

void Grid::connectLtoT(Subgrid *sg, Subgrid *sgBL) {
    // Check validity of parameters
    if(!containsSubgrid(sg) || !containsSubgrid(sgBL)) {
        error(ERR_GRID__UNKNOWN_SUBGRID);
    }

    addBorder(sg->leftGhostRegion(),  sg,   sgBL->topRegion(),  sgBL,
              Environment::numNeighbors() / 4);
    addBorder(sgBL->topGhostRegion(), sgBL, sg->leftRegion(),   sg,
              Environment::numNeighbors() / 4);
}

void Grid::connectLtoB(Subgrid *sg, Subgrid *sgTL) {
    // Check validity of parameters
    if(!containsSubgrid(sg) || !containsSubgrid(sgTL)) {
        error(ERR_GRID__UNKNOWN_SUBGRID);
    }

    addBorder(sg->leftGhostRegion(),     sg,   sgTL->bottomRegion(), sgTL,
              Environment::numNeighbors() / 4);
    addBorder(sgTL->bottomGhostRegion(), sgTL, sg->leftRegion(),     sg,
              Environment::numNeighbors() / 4);
}

void Grid::connectRtoT(Subgrid *sg, Subgrid *sgBR) {
    // Check validity of parameters
    if(!containsSubgrid(sg) || !containsSubgrid(sgBR)) {
        error(ERR_GRID__UNKNOWN_SUBGRID);
    }

    addBorder(sg->rightGhostRegion(),  sg,  sgBR->topRegion(), sgBR,
              Environment::numNeighbors() / 4);
    addBorder(sgBR->topGhostRegion(), sgBR, sg->rightRegion(), sg,
              Environment::numNeighbors() / 4);
}

void Grid::connectRtoB(Subgrid *sg, Subgrid *sgTR) {
    // Check validity of parameters
    if(!containsSubgrid(sg) || !containsSubgrid(sgTR)) {
        error(ERR_GRID__UNKNOWN_SUBGRID);
    }

    addBorder(sg->rightGhostRegion(),    sg,   sgTR->bottomRegion(), sgTR,
              Environment::numNeighbors() / 4);
    addBorder(sgTR->bottomGhostRegion(), sgTR, sg->rightRegion(), sg,
              Environment::numNeighbors() / 4);
}

void Grid::connectTtoL(Subgrid *sg, Subgrid *sgTR) {
    // Check validity of parameters
    if(!containsSubgrid(sg) || !containsSubgrid(sgTR)) {
        error(ERR_GRID__UNKNOWN_SUBGRID);
    }

    addBorder(sg->topGhostRegion(),    sg,   sgTR->leftRegion(), sgTR,
              Environment::numNeighbors() / 4);
    addBorder(sgTR->leftGhostRegion(), sgTR, sg->topRegion(), sg,
              Environment::numNeighbors() / 4);
}

void Grid::connectTtoR(Subgrid *sg, Subgrid *sgTL) {
    // Check validity of parameters
    if(!containsSubgrid(sg) || !containsSubgrid(sgTL)) {
        error(ERR_GRID__UNKNOWN_SUBGRID);
    }

    addBorder(sg->topGhostRegion(),     sg,   sgTL->rightRegion(), sgTL,
              Environment::numNeighbors() / 4);
    addBorder(sgTL->rightGhostRegion(), sgTL, sg->topRegion(),     sg,
              Environment::numNeighbors() / 4);
}

void Grid::connectBtoL(Subgrid *sg, Subgrid *sgBR) {
    // Check validity of parameters
    if(!containsSubgrid(sg) || !containsSubgrid(sgBR)) {
        error(ERR_GRID__UNKNOWN_SUBGRID);
    }

    addBorder(sg->bottomGhostRegion(),   sg, sgBR->leftRegion(), sgBR,
              Environment::numNeighbors() / 4);
    addBorder(sgBR->leftGhostRegion(), sgBR, sg->bottomRegion(), sg,
              Environment::numNeighbors() / 4);
}

void Grid::connectBtoR(Subgrid *sg, Subgrid *sgBL) {
    // Check validity of parameters
    if(!containsSubgrid(sg) || !containsSubgrid(sgBL)) {
        error(ERR_GRID__UNKNOWN_SUBGRID);
    }

    addBorder(sg->bottomGhostRegion(),   sg, sgBL->rightRegion(), sgBL,
              Environment::numNeighbors() / 4);
    addBorder(sgBL->rightGhostRegion(), sgBL, sg->bottomRegion(), sg,
              Environment::numNeighbors() / 4);
}

void Grid::connectLtoL(Subgrid *sgT, Subgrid *sgB) {
    // Check validity of parameters
    if(!containsSubgrid(sgT) || !containsSubgrid(sgB)) {
        error(ERR_GRID__UNKNOWN_SUBGRID);
    }

    addBorder(sgT->leftGhostRegion(), sgT, sgB->leftRegion(), sgB,
              Environment::numNeighbors() / 2);
    addBorder(sgB->leftGhostRegion(), sgB, sgT->leftRegion(), sgT,
              Environment::numNeighbors() / 4);
}

void Grid::connectRtoR(Subgrid *sgT, Subgrid *sgB) {
    // Check validity of parameters
    if(!containsSubgrid(sgT) || !containsSubgrid(sgB)) {
        error(ERR_GRID__UNKNOWN_SUBGRID);
    }

    addBorder(sgT->rightGhostRegion(), sgT, sgB->rightRegion(), sgB,
              Environment::numNeighbors() / 2);
    addBorder(sgB->rightGhostRegion(), sgB, sgT->rightRegion(), sgT,
              Environment::numNeighbors() / 4);
}

void Grid::connectTtoT(Subgrid *sgL, Subgrid *sgR) {
    // Check validity of parameters
    if(!containsSubgrid(sgL) || !containsSubgrid(sgR)) {
        error(ERR_GRID__UNKNOWN_SUBGRID);
    }

    addBorder(sgL->topGhostRegion(), sgL, sgR->topRegion(), sgR,
              Environment::numNeighbors() / 2);
    addBorder(sgR->topGhostRegion(), sgR, sgL->topRegion(), sgL,
              Environment::numNeighbors() / 4);
}

void Grid::connectBtoB(Subgrid *sgL, Subgrid *sgR) {
    // Check validity of parameters
    if(!containsSubgrid(sgL) || !containsSubgrid(sgR)) {
        error(ERR_GRID__UNKNOWN_SUBGRID);
    }

    addBorder(sgL->bottomGhostRegion(), sgL, sgR->bottomRegion(), sgR,
              Environment::numNeighbors() / 2);
    addBorder(sgR->bottomGhostRegion(), sgR, sgL->bottomRegion(), sgL,
              Environment::numNeighbors() / 4);
}

void Grid::wrapLR(Subgrid *sg) {
    // Check validity of parameters
    if(!containsSubgrid(sg)) {
        error(ERR_GRID__UNKNOWN_SUBGRID);
    }

    addBorder(sg->leftGhostRegion(),  sg, sg->rightRegion(), sg,
              Environment::numNeighbors() / 2);
    addBorder(sg->rightGhostRegion(), sg, sg->leftRegion(), sg,
              Environment::numNeighbors() / 2);
}

void Grid::wrapTB(Subgrid *sg) {
    // Check validity of parameters
    if(!containsSubgrid(sg)) {
        error(ERR_GRID__UNKNOWN_SUBGRID);
    }

    addBorder(sg->topGhostRegion(),    sg, sg->bottomRegion(), sg,
              Environment::numNeighbors() / 2);
    addBorder(sg->bottomGhostRegion(), sg, sg->topRegion(),    sg,
              Environment::numNeighbors() / 2);
}

void Grid::mirrorB(Subgrid *sg) {
    // Check validity of parameters
    if(!containsSubgrid(sg)) {
        error(ERR_GRID__UNKNOWN_SUBGRID);
    }

    addBorder(sg->bottomGhostRegion(), sg, sg->bottomRegion(), sg,
              Environment::numNeighbors() / 2);
}

void Grid::mirrorL(Subgrid *sg) {
    // Check validity of parameters
    if(!containsSubgrid(sg)) {
        error(ERR_GRID__UNKNOWN_SUBGRID);
    }

    addBorder(sg->leftGhostRegion(), sg, sg->leftRegion(), sg,
              Environment::numNeighbors() / 2);
}

void Grid::mirrorR(Subgrid *sg) {
    // Check validity of parameters
    if(!containsSubgrid(sg)) {
        error(ERR_GRID__UNKNOWN_SUBGRID);
    }

    addBorder(sg->rightGhostRegion(), sg, sg->rightRegion(), sg,
              Environment::numNeighbors() / 2);
}

void Grid::mirrorT(Subgrid *sg) {
    // Check validity of parameters
    if(!containsSubgrid(sg)) {
        error(ERR_GRID__UNKNOWN_SUBGRID);
    }

    addBorder(sg->topGhostRegion(), sg, sg->topRegion(), sg,
              Environment::numNeighbors() / 2);
}

void Grid::foldT(Subgrid *sg) {
    // Check validity of parameters
    if(!containsSubgrid(sg)) {
        error(ERR_GRID__UNKNOWN_SUBGRID);
    }

    // Get each half of the top's ghost region
    Region ghost1 = Region(1, sg->h() + 1,
                           floor(sg->w() / 2.0), sg->h() + 1);
    Region ghost2 = Region(floor(sg->w() / 2.0) + 1, sg->h() + 1,
                           sg->w(), sg->h() + 1);
    
    // Get each half of the top region
    Region half1 = Region(floor(sg->w() / 2.0), sg->h(),
                          1, sg->h());
    Region half2 = Region(sg->w(), sg->h(),
                          floor(sg->w() / 2.0) + 1, sg->h());

    // Do borders for the fold
    addBorder(ghost1, sg, half2, sg, Environment::numNeighbors() / 2);
    addBorder(ghost2, sg, half1, sg, Environment::numNeighbors() / 2);
}

void Grid::foldB(Subgrid *sg) {
    // Check validity of parameters
    if(!containsSubgrid(sg)) {
        error(ERR_GRID__UNKNOWN_SUBGRID);
    }

    // Get each half of the bottom's ghost region
    Region ghost1 = Region(1, 0,
                           floor(sg->w() / 2.0), 0);
    Region ghost2 = Region(floor(sg->w() / 2.0) + 1, 0,
                           sg->w(), 0);
    
    // Get each half of the bottom region
    Region half1 = Region(floor(sg->w() / 2.0), 1, 1, 1);
    Region half2 = Region(sg->w(), 1, floor(sg->w() / 2.0) + 1, 1);

    // Do borders for the fold
    addBorder(ghost1, sg, half2, sg, Environment::numNeighbors() / 2);
    addBorder(ghost2, sg, half1, sg, Environment::numNeighbors() / 2);
}

void Grid::foldL(Subgrid *sg) {
    // Check validity of parameters
    if(!containsSubgrid(sg)) {
        error(ERR_GRID__UNKNOWN_SUBGRID);
    }

    // Get each half of the left's ghost region
    Region ghost1 = Region(0, 1,
                           0, floor(sg->w() / 2.0));
    Region ghost2 = Region(0, floor(sg->w() / 2.0) + 1,
                           0, sg->w());
    
    // Get each half of the left region
    Region half1 = Region(1, floor(sg->w() / 2.0),
                          1, 1);
    Region half2 = Region(1, sg->w(),
                          1, floor(sg->w() / 2.0) + 1);

    // Do borders for the fold
    addBorder(ghost1, sg, half2, sg, Environment::numNeighbors() / 2);
    addBorder(ghost2, sg, half1, sg, Environment::numNeighbors() / 2);
}

void Grid::foldR(Subgrid *sg) {
    // Check validity of parameters
    if(!containsSubgrid(sg)) {
        error(ERR_GRID__UNKNOWN_SUBGRID);
    }

    // Get each half of the right's ghost region
    Region ghost1 = Region(sg->w() + 1, 1,
                           sg->w() + 1, floor(sg->h() / 2.0));
    Region ghost2 = Region(sg->w() + 1, floor(sg->h() / 2.0) + 1,
                           sg->w() + 1, sg->h());
    
    // Get each half of the right region
    Region half1 = Region(sg->w(), floor(sg->h() / 2.0),
                          sg->w(), 1);
                          
    Region half2 = Region(sg->w(), sg->h(),
                          sg->w(), floor(sg->h() / 2.0) + 1);

    // Do borders for the fold
    addBorder(ghost1, sg, half2, sg, Environment::numNeighbors() / 2);
    addBorder(ghost2, sg, half1, sg, Environment::numNeighbors() / 2);
}

