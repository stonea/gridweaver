/******************************************************************************
 * GRIDGEN: Grid Generating Compiler
 * By: Andy Stone (aistone@gmail.com)
 * (C) Copyright 2011 Colorado State University
 *****************************************************************************/

#include "distribution.hpp"
#include "binIO.hpp"
#include "utils.hpp"
#include "vispage.hpp"
#include "error.hpp"
#include <assert.h>
#include <mpi.h>
using namespace std;

void initializeModule_distribution() {
}

Distribution::Distribution(const string &name) :
    mName(name),
    mGrid(NULL)
{
}

void Distribution::clear(int nProcs) {
    mSg2gbid.clear();
    mGbid2proc.resize(0);
    mGbid2lbid.resize(0);
    mLbid2gbid.resize(nProcs);
    mProc2nLbid.resize(nProcs);

    for(int i = 0; i < nProcs; i++) {
        mProc2nLbid[i] = 0;
    }
}

void Distribution::applyFillBlock(Grid *g, int nProcs, int blockH)
{
    // Copy parameters into object's state
    mGrid = g;
    mnProcs = nProcs;
    mBlkW = g->subgrid(1)->w();
    mBlkH = blockH;
    clear(mnProcs);
    
    // Iterate through each subgrid applying the distribution
    int blk = 0;
    for(int i = 1; i <= mGrid->numSubgrids(); i++) {
        blk = applyFillBlockToSG(mGrid->subgrid(i), blk+1);
    }
}

void Distribution::applyBlockFill(Grid *g, int nProcs, int blockW)
{
    // Copy parameters into object's state
    mGrid = g;
    mnProcs = nProcs;
    mBlkW = blockW;
    mBlkH = g->subgrid(1)->h();
    clear(mnProcs);
    
    // Iterate through each subgrid applying the distribution
    int blk = 0;
    for(int i = 1; i <= mGrid->numSubgrids(); i++) {
        blk = applyBlockFillToSG(mGrid->subgrid(i), blk+1);
    }
}

void Distribution::applyBlockCyclic(
    Grid *g, int nProcs, int blockW, int blockH)
{
    // Copy parameters into object's state
    mGrid = g;
    mnProcs = nProcs;
    mBlkW = blockW;
    mBlkH = blockH;
    clear(mnProcs);
    
    // Iterate through each subgrid applying the distribution
    int blk = 0;
    for(int i = 1; i <= mGrid->numSubgrids(); i++) {
        blk = applyBlockCyclicToSG(mGrid->subgrid(i), blk+1);
    }
}

void Distribution::applyBlankDist(Grid *g, int nProcs, int blockW, int blockH) {
    // Copy parameters into object's state
    mGrid = g;
    mnProcs = nProcs;
    mBlkW = blockW;
    mBlkH = blockH;
    clear(mnProcs);

    // Itialize sg2gbid 
    int blk = 0;
    for(int i = 1; i <= mGrid->numSubgrids(); i++) {
        Subgrid *sg = mGrid->subgrid(i);
        mSg2gbid.insert(make_pair(sg, blk+1));
        blk += numBlksInSg(sg);
    }

    // Allocate room for gbid2proc and gbid2lbid; initialize all values to -1
    mGbid2proc.resize(lastGbidInSg(mGrid->subgrid(mGrid->numSubgrids())));
    mGbid2lbid.resize(lastGbidInSg(mGrid->subgrid(mGrid->numSubgrids())));
    for(int i = 0; i < mGbid2proc.size(); i++) {
        mGbid2proc[i] = -1;
        mGbid2lbid[i] = -1;
    }
}

void Distribution::setProcForBlock(int gbid, int rank) {
    if(rank > mProc2nLbid.size() || rank < 0) {
        cout << "INVALID RANK" << rank << mProc2nLbid.size() << endl;
    }

    mGbid2proc[gbid-1] = rank;
    mLbid2gbid[rank].push_back(gbid);
    mProc2nLbid[rank]++;
    mGbid2lbid[gbid-1] = mLbid2gbid[rank].size();
}

void Distribution::print(ostream &out) const {
    printObj_start(out, "Distribution", mName);
    if(mGrid != NULL) {
        printObj_property(out, "grid", mGrid);
        printObj_property(out, "nProcs", mnProcs);
        printObj_property(out, "blkW", mBlkW);
        printObj_property(out, "blkH", mBlkH);
        printObj_property(out, "sg2gbid");
            printVals(out, mSg2gbid.begin(), mSg2gbid.end());
        printObj_property(out, "proc2nLbid");
            printVals(out, mProc2nLbid.begin(), mProc2nLbid.end());
        printObj_property(out, "gbid2proc");
            printVals(out, mGbid2proc.begin(), mGbid2proc.end());
        printObj_property(out, "gbid2lbid");
            printVals(out, mGbid2lbid.begin(), mGbid2lbid.end());
        for(int i=0; i < mnProcs; i++) {
            printObj_property(out, "lbid2gbid@" + str(i));
            printVals(out, mLbid2gbid[i].begin(), mLbid2gbid[i].end());
        }
    }
    printObj_end(out);
}

void Distribution::printSimp(ostream &out) const {
    out << mName;
}

void Distribution::visualize(const std::string &outDir) const {
    VisPage vispage;

    // Iterate through each subgrid and create a picture of the distribution
    // of that subgrid.
    for(int i = 1; i <= mGrid->numSubgrids(); i++) {
        Subgrid *sg = mGrid->subgrid(i);
        CellFieldPicture pic(sg->w(), sg->h());

        // Iterate through each block for the subgrid, create a seperate
        // visualization for each subgrid
        for(int gbid =  firstGbidInSg(sg);
                gbid <= lastGbidInSg(sg); gbid++)
        {
            // Iterate through each point in the block's region, paint
            // that region in the visualization of the subgrid
            Region r = gbidRegion(gbid);

            for(int y = r.lowY(); y <= r.highY(); y++) {
                for(int x = r.lowX(); x <= r.highX(); x++) {
                    pic.setFill(x-1, y-1, num2SVGcolor(gbidProc(gbid)));
                    pic.setLabel(x-1, y-1, str(gbidAtPos(sg, x, y)));
                }
            }
        }

        // Add the visualization to the page
        vispage.add(sg->getID(), pic);
    }
    
    // Output the visualizations
    vispage.save(outDir);
}

void Distribution::output(ostream &out) const {
    BinIO::out(out, mName);
    BinIO::outIdent(out, *mGrid);
    BinIO::out(out, mnProcs);
    BinIO::out(out, mBlkW);
    BinIO::out(out, mBlkH);
    BinIO::out(out, mSg2gbid.begin(), mSg2gbid.end(), &saveSg2gbidEntry);
    BinIO::out(out, mProc2nLbid.begin(), mProc2nLbid.end());
    BinIO::out(out, mGbid2proc.begin(), mGbid2proc.end());
    BinIO::out(out, mGbid2lbid.begin(), mGbid2lbid.end());
    BinIO::out(out, mLbid2gbid.begin(), mLbid2gbid.end(), &saveLbid2gbidEntry);
}

void Distribution::input(istream &in) {
    pair<Subgrid*, int> blankSgGbidEntry;
    vector<int> blankLbid2gbidEntry;
    int blankInt;

    BinIO::in(in, mName);
    BinIO::inIdent(in, &mGrid, &Environment::getGrid);
    BinIO::in(in, mnProcs);
    BinIO::in(in, mBlkW);
    BinIO::in(in, mBlkH);
    BinIO::in(in, inserter(mSg2gbid, mSg2gbid.begin()),
              &loadSg2gbidEntry, blankSgGbidEntry);
    BinIO::in(in, back_inserter(mProc2nLbid), blankInt);
    BinIO::in(in, back_inserter(mGbid2proc), blankInt);
    BinIO::in(in, back_inserter(mGbid2lbid), blankInt);
    BinIO::in(in, back_inserter(mLbid2gbid),
              &loadLbid2gbidEntry, blankLbid2gbidEntry);
}

int Distribution::firstGbidInSg(Subgrid *sg) const {
    typedef std::map<Subgrid*, int>::const_iterator  Iter;
    Iter i;

    i = mSg2gbid.find(sg);
    if(i == mSg2gbid.end()) { error(ERR_DIST__INVALID_SG); }

    return (*i).second;
}

int Distribution::numBlocks() const {
    return lastGbidInSg(mGrid->subgrid(mGrid->numSubgrids()));
}

int Distribution::numNodesForProc(int pid) const {
    int n = 0;

    // Iterate through blocks
    for(int gbid = 1; gbid <= numBlocks(); gbid++) {
        Subgrid *sg = gbidSubgrid(gbid);
        if(gbidProc(gbid) == pid) {
            n += MIN(mBlkW * mBlkH, sg->w() * sg->h());
        }
    }

    return n; 
}

int Distribution::gbid2SG(int gbid) const {
    for(int i = 1; i <= mGrid->numSubgrids(); i++) {
        Subgrid *sg = mGrid->subgrid(i);
        if(gbid >= firstGbidInSg(sg) &&
           gbid <= lastGbidInSg(sg))
        {
            return i;
        }
    }
}

bool Distribution::isBorderBlock(int gbid) const {
    Subgrid *sg = gbidSubgrid(gbid);
    int nBlocksHoriz = blocksHorizInSg(sg);
    int nBlksInSG    = numBlksInSg(sg);
    int gbidAdjusted = gbid - firstGbidInSg(sg) + 1;

    return
      (gbidAdjusted <= nBlocksHoriz ||
       gbidAdjusted >= (nBlksInSG - nBlocksHoriz + 1) ||
       gbidAdjusted % nBlocksHoriz == 0 ||
       gbidAdjusted % nBlocksHoriz == 1);
}

bool Distribution::isInteriorBlock(int gbid) const {
    return !isBorderBlock(gbid);
}

Subgrid *Distribution::gbidSubgrid(int gbid) const {
    // Loop through each subgrid until we find the one that contains
    // the specified gbid
    for(int i = 1; i <= mGrid->numSubgrids(); i++) {
        Subgrid *sg = mGrid->subgrid(i);
        if(gbid >= firstGbidInSg(sg) && gbid <= lastGbidInSg(sg)) {
            return sg;
        }
    }

    return NULL; // TODO: Throw an error
}

int Distribution::lastGbidInSg(Subgrid *sg) const {
    return firstGbidInSg(sg) + numBlksInSg(sg) - 1;
}

int Distribution::numBlksInSg(Subgrid *sg) const {
    return
        ceil((double)sg->w() / (double)mBlkW) *
        ceil((double)sg->h() / (double)mBlkH);
}

int Distribution::blocksHorizInSg(Subgrid *sg) const {
    return ceil((double)sg->w() / (double)mBlkW);
}

int Distribution::blocksVertInSg(Subgrid *sg) const {
    return ceil((double)sg->h() / (double)mBlkH);
}

Region Distribution::gbidRegion(int gbid) const {
    Region result;
    Subgrid *sg = gbidSubgrid(gbid);

    // Adjust gbid so that it is normalized for the subgrid
    int sgbid = gbid - firstGbidInSg(sg) + 1;

    int blkY = (sgbid-1) / blocksHorizInSg(sg);
    int blkX = sgbid - (blkY * blocksHorizInSg(sg)) - 1;

    result.reshape(MIN( blkX    * mBlkW + 1, sg->w()),
                   MIN( blkY    * mBlkH + 1, sg->h()),
                   MIN((blkX+1) * mBlkW,     sg->w()),
                   MIN((blkY+1) * mBlkH,     sg->h()));

    return result;
}

int Distribution::gbidAtPos(Subgrid *sg, int x, int y) const {
    Region result;

    int blkX = (x-1) / mBlkW;
    int blkY = (y-1) / mBlkH;

    int gbid = firstGbidInSg(sg) + (blocksHorizInSg(sg) * blkY) + blkX;

    return gbid;
}

void Distribution::pos2BlockPos(
    int x, int y, Subgrid *sg, int &blkX, int &blkY, int &gbid)
{
    gbid = gbidAtPos(sg, x, y);
    blkX = x - gbidRegion(gbid).lowX() + 1;
    blkY = y - gbidRegion(gbid).lowY() + 1;
}

set<int> Distribution::gbidsIntersectingRegion(
    Subgrid *sg, const Region &rgn) const
{
    set<int> blocks;    // Result

    // Expand r so it fits along block boundries; cut r with the subgrid
    Region expandedRgn = rgn;
    expandedRgn.expandToMultiple(mBlkW, mBlkH);
    expandedRgn.cut(sg->region());

    // Point (blkX, blkY) to the (x,y) index of the bottom-left block in
    // rgn.
    int blkX = expandedRgn.lowX() / mBlkW;
    int blkY = expandedRgn.lowY() / mBlkH;

    // Iterate through each block in expandedRgn.  (x,y) is the coordinates of
    // the lower left corner (the key point) of each block.
    for(int y = expandedRgn.lowY(); y <= expandedRgn.highY(); y += mBlkH) {
        for(int x = expandedRgn.lowX(); x <= expandedRgn.highX(); x += mBlkW) {
            int gbid = gbidAtPos(sg, x, y);
            blocks.insert(gbid);
            gbid++;
        }
    }
    
    return blocks;
}

int Distribution::applyBlockCyclicToSG(
    Subgrid *sg, int startingBlk)
{
    /* A two-dimensional block cyclic distribution applies a cycle of
     * blocks horizontally and vertically through the grid.
     *
     * For example a subgrid might be broken into 6 block cycles on a
     * 2 by 3 arrangement:
     *
     *    |------ nBlkCycHoriz ----|
     *
     *    ,------------------------,   -----------
     *    |,------, ,------, ,----,|        |
     *    || blk  | | blk  | | bl ||        |
     *    || cycle| | cycle| | cye||         
     *    |`------' `------' `----'|   nBlkCycVert
     *    |,------, ,------, ,----,|
     *    || blk  | | blk  | | bl ||        |
     *    || cycle| | cycle| | cye||        |
     *    |`------' `------' `----'|        |
     *    `------------------------'   -----------
     *
     * Note that block along the top or right edges may be cropped.
     * An individual block cycle of 5 processors would look like
     * the following:
     *
     *    |--- nBlkHoriz ---|
     *
     *    ,-----------------,   ---------
     *    |,---, ,---, ,---,|       |
     *    || 4 | | 5 | | 1 ||
     *    |`---' `---' `---'|   nBlkVert
     *    |,---, ,---, ,---,|
     *    || 1 | | 2 | | 3 ||       |
     *    |`---' `---' `---'|       |
     *    `-----------------'   ---------
     *
     * Note that: blkCycW and blkCycH are the dimensions of a block cycle
     * in terms of number of points horizontally and vertically.
     */
    // Specify the starting block for the subgrid
    vector<int> lbid = vector<int>(mnProcs);  // stores lbid for each proc
    for(int i = 0; i < mnProcs; i++) { lbid[i] = mProc2nLbid[i] + 1; }
    mSg2gbid.insert(make_pair(sg, startingBlk));

    // Calculate the geometry of the distribution
    int nBlkHoriz = ceil(sqrt(mnProcs));
    int nBlkVert  = floor(sqrt(mnProcs));
    int blkCycW   = nBlkHoriz * mBlkW;
    int blkCycH   = nBlkVert  * mBlkH;
    int nBlkCycHoriz = ceil((double)sg->w() / (double)blkCycW);
    int nBlkCycVert  = ceil((double)sg->h() / (double)blkCycH);

    // Make room for the results
    mGbid2proc.resize(lastGbidInSg(sg));
    mGbid2lbid.resize(lastGbidInSg(sg));

    // In the following loop nest (blkCycX, blkCycY) is the position of a
    // block-cycle where (1,1) is the position of the lower-left block-cycle
    // and (2,1) is the position of the next block-cycle.
    //
    // (blkX, blkY) is the position of a block within a block cycle, where
    // (1,1) is the position of the lower-left block and (2,1) is the position
    // of the block to the right of that.
    for(int blkCycY = 1; blkCycY <= nBlkCycVert; blkCycY++) {
        for(int blkCycX = 1; blkCycX <= nBlkCycHoriz; blkCycX++) {
            // iterate through each block in the block-cycle.  As we're
            // iterating accumulate a processor ID count
            int procID = 0;
            for(int blkY = 1; blkY <= nBlkVert; blkY++) {
                for(int blkX = 1; blkX <= nBlkHoriz; blkX++) {
                    // Determine the (x,y) position in the subgrid where this
                    // block is located
                    int x = (blkCycX-1) * blkCycW + 1 +
                               (blkX-1) * mBlkW;
                    int y = (blkCycY-1) * blkCycH + 1 +
                               (blkY-1) * mBlkH;

                    int gbid = gbidAtPos(sg,x,y);

                    // If (x,y) is in the subgrid register this block in the
                    // distribution
                    if(sg->contains(x,y)) {
                        mGbid2proc[gbid-1] = procID;
                        mGbid2lbid[gbid-1] = lbid[procID];
                        mLbid2gbid[procID].push_back(gbid);
                        assert(mLbid2gbid[procID].size() == lbid[procID]);
                        lbid[procID]++;
                    }

                    // Increment procID, cycle back to 0 if we've exceeded
                    // the number of processors.
                    procID = (procID + 1) % mnProcs;
                }
            }
        }
    }

    // Update the list of how many LBID's exist per processor
    for(int i = 0; i < mnProcs; i++) { mProc2nLbid[i] = lbid[i] - 1; }

    return lastGbidInSg(sg);
}

int Distribution::applyFillBlockToSG(Subgrid *sg, int startingBlk)
{
    /* A two-dimensional fill block distribution:
     *
     *    ,------------------------,
     *    |,----------------------,|
     *    || blk                  ||
     *    ||                      ||
     *    |`----------------------'|
     *    |,----------------------,|
     *    || blk                  ||
     *    ||                      ||
     *    |`----------------------'|
     *    |,----------------------,|
     *    || blk                  ||
     *    ||                      ||
     *    |`----------------------'|
     *    `------------------------'
     */
    //Sg2gbid_t  mSg2gbid;                // Maps subgrids to their starting blk
    //std::vector<int>  mProc2nLbid;      // Keeps track of how many LBIDs are
                                        // allocated to each proc.
    //std::vector<int>  mGbid2proc;       // Maps global block ID's to processors
    //std::vector<int>  mGbid2lbid;       // Maps global block ID's to local ID's
    //std::vector<std::vector<int> > mLbid2gbid;  // Maps local block ID's to

    // Specify the starting block for the subgrid
    vector<int> lbid = vector<int>(mnProcs);  // stores lbid for each proc
    for(int i = 0; i < mnProcs; i++) { lbid[i] = mProc2nLbid[i] + 1; }
    mSg2gbid.insert(make_pair(sg, startingBlk));

    // Make room for the results
    mGbid2proc.resize(lastGbidInSg(sg));
    mGbid2lbid.resize(lastGbidInSg(sg));

    // If we have a single element block
    if(sg->w() == 1 || sg->h() == 1) {
        int gbid = gbidAtPos(sg,1,1);

        mGbid2proc[gbid-1] = 0;
        mGbid2lbid[gbid-1] = lbid[0];
        mLbid2gbid[0].push_back(gbid);
        assert(mLbid2gbid[0].size() == lbid[0]);
        lbid[0]++;
    } else {
        // Iterate through the vertical extent of the subgrid
        int gbid = startingBlk;
        int procID;
        if(startingBlk > 1) {
            procID = (gbidProc(startingBlk-1) + 1) % mnProcs;
        } else { procID = 0; }
        for(int y = 1; y < sg->h(); y += mBlkH) {
            gbid = gbidAtPos(sg,1,y);

            mGbid2proc[gbid-1] = procID;
            mGbid2lbid[gbid-1] = lbid[procID];
            mLbid2gbid[procID].push_back(gbid);
            assert(mLbid2gbid[procID].size() == lbid[procID]);
            lbid[procID]++;
            
            // Increment procID, cycle back to 0 if we've exceeded
            // the number of processors.
            procID = (procID + 1) % mnProcs;
        }
    }

    // Update the list of how many LBID's exist per processor
    for(int i = 0; i < mnProcs; i++) { mProc2nLbid[i] = lbid[i] - 1; }

    return lastGbidInSg(sg);
    //return 0;
}


int Distribution::applyBlockFillToSG(Subgrid *sg, int startingBlk)
{
    /* A two-dimensional block fill distribution:
     *
     *    ,---------------------,
     *    |.-----,.-----,.-----,|
     *    || blk || blk || blk ||
     *    ||     ||     ||     ||
     *    ||     ||     ||     ||
     *    ||     ||     ||     ||
     *    ||     ||     ||     ||
     *    ||     ||     ||     ||
     *    |`-----'`-----'`-----'|
     *    `---------------------'
     */
    //Sg2gbid_t  mSg2gbid;                // Maps subgrids to their starting blk
    //std::vector<int>  mProc2nLbid;      // Keeps track of how many LBIDs are
                                        // allocated to each proc.
    //std::vector<int>  mGbid2proc;       // Maps global block ID's to processors
    //std::vector<int>  mGbid2lbid;       // Maps global block ID's to local ID's
    //std::vector<std::vector<int> > mLbid2gbid;  // Maps local block ID's to

    // Specify the starting block for the subgrid
    vector<int> lbid = vector<int>(mnProcs);  // stores lbid for each proc
    for(int i = 0; i < mnProcs; i++) { lbid[i] = mProc2nLbid[i] + 1; }
    mSg2gbid.insert(make_pair(sg, startingBlk));

    // Make room for the results
    mGbid2proc.resize(lastGbidInSg(sg));
    mGbid2lbid.resize(lastGbidInSg(sg));

    // If we have a single element block
    if(sg->w() == 1 || sg->h() == 1) {
        int gbid = gbidAtPos(sg,1,1);

        mGbid2proc[gbid-1] = 0;
        mGbid2lbid[gbid-1] = lbid[0];
        mLbid2gbid[0].push_back(gbid);
        assert(mLbid2gbid[0].size() == lbid[0]);
        lbid[0]++;
    } else {
        // Iterate through the vertical extent of the subgrid
        int gbid = startingBlk;
        int procID;
        if(startingBlk > 1) {
            procID = (gbidProc(startingBlk-1) + 1) % mnProcs;
        } else { procID = 0; }
        for(int x = 1; x < sg->w(); x += mBlkW) {
            gbid = gbidAtPos(sg,x,1);

            mGbid2proc[gbid-1] = procID;
            mGbid2lbid[gbid-1] = lbid[procID];
            mLbid2gbid[procID].push_back(gbid);
            assert(mLbid2gbid[procID].size() == lbid[procID]);
            lbid[procID]++;
            
            // Increment procID, cycle back to 0 if we've exceeded
            // the number of processors.
            procID = (procID + 1) % mnProcs;
        }
    }

    // Update the list of how many LBID's exist per processor
    for(int i = 0; i < mnProcs; i++) { mProc2nLbid[i] = lbid[i] - 1; }

    return lastGbidInSg(sg);
    //return 0;
}


void Distribution::saveSg2gbidEntry(
    std::ostream &out,
    const std::pair<Subgrid*, int> &obj)
{
    BinIO::outIdent(out, *obj.first);
    BinIO::out(out, obj.second);
}

void Distribution::loadSg2gbidEntry(std::istream &in,
                                    std::pair<Subgrid*, int> &obj)
{
    BinIO::inIdent(in, &obj.first, &Environment::getSubgrid);
    BinIO::in(in, obj.second);
}

void Distribution::saveLbid2gbidEntry(std::ostream &out,
                                      const std::vector<int> &obj)
{
    BinIO::out(out, obj.begin(), obj.end());
}

void Distribution::loadLbid2gbidEntry(std::istream &in, std::vector<int> &obj)
{
    int blankInt;
    BinIO::in(in, back_inserter(obj), blankInt);
}

