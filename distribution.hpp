/******************************************************************************
 * GRIDGEN: Grid Generating Compiler
 * By: Andy Stone (aistone@gmail.com)
 * (C) Copyright 2011 Colorado State University
 *****************************************************************************/
/** \addtogroup Environmental
 *  @{
 */
#ifndef DISTRIBUTION_HPP_
#define DISTRIBUTION_HPP_

#include <iostream>
#include <string>
#include <vector>
#include <set>
#include <map>
#include "grid.hpp"
#include "iprintable.hpp"
#include "ivisualizable.hpp"
#include "iserializable.hpp"
#include "ienvironmental.hpp"

/**
 * Initialization function for the distribution module.
 */
void initializeModule_distribution();

/**
 * A distribution specifies how to partition a grid's index space up into
 * blocks.  Distributions may only be instantiated via an environment object.
 * Blocks are assigned unique global ID's (starting from 1) and local ID's
 * (also starting from 1).
 *
 * Data in a distribution that doesn't point to another environmental object
 * (for example a grid) is replicated across all MPI ranks.
 */
class Distribution : public IPrintable,
                     public IVisualizable,
                     public ISerializable,
                     public IEnvironmental
{
  public:
    friend class Environment;   // Must be a friend so that environment may
                                // construct objects of this type. 

    // =======================
    // - [Construction] -
    // =======================
    /** @name Construction */
    ///@{
        /**
         * Calculate a fill-block distribution for grid g where the height of
         * each block is blockH.  Assume there are nProcs processors.
         */
        void applyFillBlock(Grid *g, int nProcs, int blockH);

        /**
         * Calculate a block-fill distribution for grid g where the width of
         * each block is blockW.  Assume there are nProcs processors.
         */
        void applyBlockFill(Grid *g, int nProcs, int blockW);

        /**
         * Calculate a block cyclic distribution for grid g where blocks
         * are of size blockW by blockH.  Assume that there are nProcs
         * processors.
         */
        void applyBlockCyclic(Grid *g, int nProcs,
                              int blockW, int blockH);

        /**
         * Calculate a blocked horizontal distribution for grid g. Assume that
         * there are nProcs processors.
         */
        //void applyBlockHoriz(Grid *g, int nProcs);
    ///@}

    // =======================
    // - [Input and Output] -
    // =======================
    /** @name Input and Output */
    ///@{
        void print(std::ostream &out) const;
        void printSimp(std::ostream &out) const;
        void visualize(const std::string &outDir) const;
        void output(std::ostream &out) const;
        void input(std::istream &in);
    ///@}

    // =======================
    // - [Accessors] -
    // =======================
    /** @name Accessors */
    ///@{
        std::string getID() const { return mName; }
    
        /**
         * Return the grid associated with this distrbution.
         */
        Grid *grid() const { return mGrid; }
        
        /**
         * Return the number of processors this distribution assumes.
         */
        int numProcs() const { return mnProcs; }

        /**
         * Return the width of blocks.
         */
        int blockWidth() const { return mBlkW; }
        
        /**
         * Return the height of blocks.
         */
        int blockHeight() const { return mBlkH; }
        
        /**
         * Return the starting GBID for the sepcified subgrid.
         */
        int firstGbidInSg(Subgrid *sg) const;

        /**
         * Return the number of local blocks a proccess is responsible for.
         */
        int numLclBlocksForProc(int pid) const { return mProc2nLbid[pid]; }

        /**
         * Given a global block ID return the MPI rank of the processor
         * that owns that block.
         */
        int gbidProc(int gbid) const { return mGbid2proc[gbid-1]; }
        
        /**
         * Given a global block ID return its corresponding local block ID.
         */
        int gbid2lbid(int gbid) const { return mGbid2lbid[gbid-1]; }
        
        /**
         * Given a local block ID return its corresponding global block ID.
         */
        int lbid2gbid(int pid, int lbid) const {
            return mLbid2gbid[pid][lbid-1];
        }
    ///@}

    // =======================
    // - [Queries] -
    // =======================
    /** @name Queries */
    ///@{
        /**
         * Return the subgrid the specified block is in.
         */
        Subgrid *gbidSubgrid(int gbid) const;
    
        /**
         * Return the GBID of the last block in the specified subgrid.
         */
        int lastGbidInSg(Subgrid *sg) const;

        /**
         * Return the number of blocks contained by the specified subgrid.
         */
        int numBlksInSg(Subgrid *sg) const;

        /**
         * Return the number of blocks that horizontally span the specified
         * subgrid.
         */
        int blocksHorizInSg(Subgrid *sg) const;

        /**
         * Return the number of blocks that vertically span the specified
         * subgrid.
         */
        int blocksVertInSg(Subgrid *sg) const;

        /**
         * Return the region for a specified block.
         */
        Region gbidRegion(int gbid) const;

        /**
         * Return the gbid at a specified position.
         */
        int gbidAtPos(Subgrid *sg, int x, int y) const;

        /**
         * Return the gbid and block coordinates at a specific position
         */
        void pos2BlockPos(
            int x, int y, Subgrid *sg, int &blkX, int &blkY, int &gbid);

        /**
         * Return the set of gbids that intersect within a given region of a
         * subgrid.  Note this does not account for the subgrid's halo (no
         * blocks in the halo will be returned).
         */
        std::set<int> gbidsIntersectingRegion(
            Subgrid *sg, const Region &rgn) const;
    ///@}
        
  protected:
    // =======================
    // - [Construction] -
    // =======================
        /** Instantiate a new distribution object with the given name. */
        Distribution(const std::string &name);

        /**
         * Clear distribution data and allocate space for new data
         * when the distribution is to be applied to nProcs processors.
         */
        void clear(int nProcs);

  private:
    /**
     * Apply a fill block distribution to the specified subgrid starting
     * with the specfied starting block global ID.  Return the global ID
     * of the last block in the distribution.
     **/
    int applyFillBlockToSG(Subgrid *sg, int startingGBID);

    /**
     * Apply a block cyclic distribution to the specified subgrid starting
     * with the specfied starting block global ID.  Return the global ID
     * of the last block in the distribution.
     **/
    int applyBlockCyclicToSG(Subgrid *sg, int startingGBID);

  private:
    typedef std::map<Subgrid*, int> Sg2gbid_t;

    /** Given an entry in mSg2gbid serialize it to the stream. */
    static void saveSg2gbidEntry(std::ostream &out,
                                 const std::pair<Subgrid*, int> &obj);

    /** Load an entry of mSg2gbid from the stream. */
    static void loadSg2gbidEntry(std::istream &in,
                                 std::pair<Subgrid*, int> &obj);

    /** Given an entry in mLbid2gbid serialize it to the stream. */
    static void saveLbid2gbidEntry(std::ostream &out,
                                   const std::vector<int> &obj);

    /** Load an entry of mLbid2gbid from the stream. */
    static void loadLbid2gbidEntry(std::istream &in,
                                   std::vector<int> &obj);


    std::string mName;
    Grid  *mGrid;
    int   mnProcs;
    int   mBlkW, mBlkH;                 // Block size
    Sg2gbid_t  mSg2gbid;                // Maps subgrids to their starting blk
    std::vector<int>  mProc2nLbid;      // Keeps track of how many LBIDs are
                                        // allocated to each proc.
    std::vector<int>  mGbid2proc;       // Maps global block ID's to processors
    std::vector<int>  mGbid2lbid;       // Maps global block ID's to local ID's
    std::vector<std::vector<int> > mLbid2gbid;  // Maps local block ID's to
                                                // global block ID's
};

#endif
/** @}*/
