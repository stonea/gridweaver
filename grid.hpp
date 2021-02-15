/******************************************************************************
 * GRIDGEN: Grid Generating Compiler
 * By: Andy Stone (aistone@gmail.com)
 * (C) Copyright 2011 Colorado State University
 *****************************************************************************/
/** \addtogroup Environmental
 *  @{
 */
#ifndef GRID_HPP_
#define GRID_HPP_

#include <iostream>
#include <string>
#include <vector>
#include "error.hpp"
#include "iprintable.hpp"
#include "iserializable.hpp"
#include "ienvironmental.hpp"
//#include <set>
//#include <list>
//#include "cellFieldPicture.hpp"

class Subgrid;

/**
 * Initialization function for the grid module.
 */
void initializeModule_grid();

/**
 * An orientation corresponds to one of four corners in a region.
 */
enum Orientation {
    BL = 0, // bottom-left
    BR,     // bottom-right
    TL,     // top-left
    TR      // top-right
};

/** Represents a location in the global coordinate space */
struct GlobalCoordinate : public virtual IPrintable {
    Subgrid *sg;
    int x, y;

    GlobalCoordinate(Subgrid *_sg, int _x, int _y) :
        sg(_sg),
        x(_x),
        y(_y)
    { }
    virtual void print(std::ostream &out) const;
    virtual void printSimp(std::ostream &out) const;

    bool operator<(const struct GlobalCoordinate& rhs) const;
};

/**
 * @ingroup Environmental
 *
 * A region is a rectangular index space that has an orientation.
 * The index space may optionally be empty.
 */
class Region : public virtual IPrintable,
               public virtual ISerializable
{
  public:
    // =======================
    // - [Construction] -
    // =======================
    /** @name Construction */
    ///@{
        /** Construct an empty region */
        Region();

        /**
         * Construct a region that represents the set of indices
         * {(x, y) | lowx <= x <= highx and lowy <= y <= highy} where
         * lowx = min(x1, x2), highx = max(x1, x2), lowy = min(y1, y2), and
         * highy = min(y1, y2).  Infer the orientation of the rectangle based
         * on the relative orders of x1 and x2, and y1 and y2.
         */
        Region(int x1, int y1, int x2, int y2);
    ///@}

    // =======================
    // - [Input/Output] -
    // =======================
    /** @name Input/Output */
    ///@{
        virtual void print(std::ostream &out) const;
        virtual void printSimp(std::ostream &out) const;
        virtual void output(std::ostream &out) const;
        virtual void input(std::istream &in);
    ///@}
    
    // =======================
    // - [Accessors] -
    // =======================
    /** @name Accessors */
    ///@{
        /** Return the smallest X index in the region. */
        int lowX() const { return mLowX; }
        
        /** Return the smallest Y index in the region. */
        int lowY() const { return mLowY; }
        
        /** Return the highest X index in the region. */
        int highX() const { return mHighX; }
        
        /** Return the highest Y index in the region. */
        int highY() const { return mHighY; }
        
        /** Return the highest Y index in the region. */
        bool isEmpty() const { return mEmpty; }
        
        /** Return the highest Y index in the region. */
        Orientation orientation() const { return mOrientation; }
    ///@}
    
    // =======================
    // - [Queries] -
    // =======================
    /** @name Queries */
    ///@{
        /**
         * Return the key X location in the region.  The key point is determined
         * by the region's orientation
         */
        int keyX() const;

        /**
         * Return the key Y location in the region.  The key point is determined
         * by the region's orientation
         */
        int keyY() const;
    
        /** Return true if (x,y) is contained in this region */
        bool contains(int x, int y) const;

        /**
         * Return true if rhs is horizontally flipped relative to this
         * region.
         */
        bool isHorizFlipRelativeTo(const Region &rhs) const;

        /**
         * Return true if rhs is vertically flipped relative to this
         * region.
         */
        bool isVertFlipRelativeTo(const Region &rhs)  const;

        /**
         * Return true if the shape of rhs is a 90 degree flip of this
         * region.
         */
        bool is90degFlip(const Region &rhs) const;

        /**
         * Reorient the point to use the orientation of tgt
         */
        void reorient(Region tgt, int &x, int &y, bool accountForSelf=true, bool flip=false) const;

    ///@}

    // ================================
    // - [Arithmetic Operations] -
    // ================================
    /** @name Arithmetic Operations */
    ///@{

        /**
         * Determine the intersection of two regions.
         *
         * Example:
         */
        Region intersect(const Region &rhs) const;
        
        /**
         * Given a region embedded in this region determine the analogous region
         * embedded in the target.
         */
        Region analogousRegion(const Region &reg, const Region &tgt) const;

        /**
         * Find the intersection of this region and rhs, then determine the
         * analogous region of the cut in tgt.
         */
        void cutAnalogously(const Region &rhs, const Region &tgt,
                            Region &resInSrc, Region &resInTgt) const;

        /**
         * Flip the region horizontally.
         */
        void flipHoriz();

        /**
         * Flip the region vertically.
         */
        void flipVert();

        //Region operator+(Neighbor *neigh);
        //Region operator-(Neighbor *neigh);
        //Region intersect(Region rhs);
        //Region expand(vector<Neighbor*> neighs);
        //Region expandIntoMultiple(vector<int> sizes);
    ///@}
   
    // =======================
    // - [Mutators] -
    // =======================
    /** @name Mutators */
    ///@{
        /**
         * Reshape the region so that it represents the set of indices
         * {(x, y) | lowx <= x <= highx and lowy <= y <= highy} where
         * lowx = min(x1, x2), highx = max(x1, x2), lowy = min(y1, y2), and
         * highy = min(y1, y2).  Infer the orientation of the rectangle based
         * on the relative orders of x1 and x2, and y1 and y2.
         */
        void reshape(int x1, int y1, int x2, int y2);

        /**
         * Expand the region by a given amount in each dimension.
         */
        void expand(int deltaLowX,  int deltaLowY,
                    int deltaHighX, int deltaHighY);

        /**
         * Expand the region so that its x coordinate fall along a multiple
         * of a given value; do the same for the y coordinate.  You can think of
         * this as "snapping" the size of the region.
         */
        void expandToMultiple(int multipleX,  int multipleY);

        /**
         * Reshape this region to only include indices that intersect with rhs.
         */
        void cut(const Region &rhs);

        /**
         * Move the region to an offset location.
         */
        void translate(int deltaX, int deltaY);

        /**
         * Set region to be empty
         */
        void clear();
    ///@}

    //static void reorient(int &ptX, int &ptY, Orientation o1, Orientation o2);
    //static void reorient(int &ptX, int &ptY, Orientation o1);
    
  private:
    int  mLowX, mLowY, mHighX, mHighY;
    Orientation  mOrientation;
    bool  mEmpty;
};


/**
 * Two cells on a grid neighbor one another if the are adjacent to one another.
 * Neighbor objects are used to identify the relative positioning of the values
 * of two neighboring cells when stored using an array representation.
 * Stencil computations are also defined in terms of what neighbors they
 * access.
 *
 * Neighbor objects can be represented as a two-dimensional vector (x,y) where
 * x is the offset of the neighbor in the x-dimension of the array and y
 * is the offset of the neighbor in the y-dimension.
 */
class Neighbor : public virtual IPrintable,
                 public virtual ISerializable,
                 public virtual IEnvironmental
{
  public:
    friend class Environment;  // Must be a friend so that environment may
                               // construct objects of this type. 

    // =======================
    // - [Input and Output] -
    // =======================
    /** @name Input and Output */
    ///@{
        virtual void print(std::ostream &out) const;
        virtual void printSimp(std::ostream &out) const;
        virtual void output(std::ostream &out) const;
        virtual void input(std::istream &in);
    ///@}

    // =======================
    // - [Accessors] -
    // =======================
    /** @name Accessors */
    ///@{
        virtual std::string getID() const { return mName; }
        int x() const { return mX; }
        int y() const { return mY; }
    ///@}

    // =======================
    // - [Mutators] -
    // =======================
    /** @name Mutators */
    ///@{
        void setX(int x) { mX = x; }
        void setY(int y) { mY = y; }
        void set(int x, int y) { mX = x; mY = y; }
    ///@}

  protected:
    // =======================
    // - [Construction] -
    // =======================
        /** Instantiate a new neighbor object using the specified identifier. */
        Neighbor(const std::string &name);
    
  private:
    std::string  mName;
    int mX, mY;
};


/**
 * A subgrid is a two-dimensional index space.  Subgrids can be connected to
 * one another to form a grid.
 */
class Subgrid : public virtual IPrintable,
                public virtual ISerializable,
                public virtual IEnvironmental
{
  public:
    friend class Environment;  // Must be a friend so that environment may 
                               // construct objects of this type. 

    // =======================
    // - [Input/Output] -
    // =======================
    /** @name Input/Output */
    ///@{
        virtual void print(std::ostream &out) const;
        virtual void printSimp(std::ostream &out) const;
        virtual void output(std::ostream &out) const;
        virtual void input(std::istream &in);
    ///@}

    // =======================
    // - [Accessors] -
    // =======================
    /** @name Accessors */
    ///@{
        virtual std::string getID() const { return mName; }

        /** Return the width of the subgrid. */
        int w() const { return mW; }

        /** Return the height of the subgrid. */
        int h() const { return mH; }
    
        int sgid() const { return mSGID; }

        std::string symbolicWidth()  { return msW; }
        std::string symbolicHeight() { return msH; }

        /**
         * Return the region of the subgrid that includes the topmost elements
         */
        Region topRegion() const;

        /**
         * Return the region of the subgrid that includes the bottommost
         * elements
         */
        Region bottomRegion() const;
        
        /**
         * Return the region of the subgrid that includes the leftmost elements
         */
        Region leftRegion() const;

        /**
         * Return the region of the subgrid that includes the rightmost
         * elements
         */
        Region rightRegion() const;

        /**
         * Return the region of the subgrid that includes the topmost ghost
         * elements (this does not include the corners)
         */
        Region topGhostRegion() const;

        /**
         * Return the region of the subgrid that includes the bottommost ghost
         * elements (this does not include the corners)
         */
        Region bottomGhostRegion() const;

        /**
         * Return the region of the subgrid that includes the leftmost ghost
         * elements (this does not include the corners)
         */
        Region leftGhostRegion() const;

        /**
         * Return the region of the subgrid that includes the rightmost ghost
         * elements (this does not include the corners)
         */
        Region rightGhostRegion() const;
    ///@}

    // =======================
    // - [Queries] -
    // =======================
    /** @name Queries */
    ///@{
        /** Return true if position (x,y) is included in the subgrid */
        bool contains(int x, int y);

        /** Return a region that represents the index space of this subgrid */
        Region region() const;
    ///@}

    // =======================
    // - [Mutators] -
    // =======================
    /** @name Mutators */
    ///@{
        void setWidth(int w)  { mW = w; }
        void setHeight(int h) { mH = h; }
        void setExtents(int w, int h) { mW = w; mH = h; }
        void setSymbolicExtents(std::string w, std::string h) { msW = w; msH = h; }
    ///@}

  protected:
    // =======================
    // - [Construction] -
    // =======================
        /** Instantiate a new subgrid object using the specified identifier. */
        Subgrid(const std::string &name, const int sgid);

  private:
    std::string  mName;
    std::string msW, msH;
    int mW, mH, mSGID;
};


/**
 * A grid is a set of subgrids that connect to one another to form a unified
 * index space.
 */
class Grid : public virtual IPrintable,
             public virtual ISerializable,
             public virtual IEnvironmental
{
  public:
    friend class Environment;  // Must be a friend so that environment may
                               // construct objects of this type. 

    // =======================
    // - [Input and Output] -
    // =======================
    /** @name Input and Output */
    ///@{
        virtual void print(std::ostream &out) const;
        virtual void printSimp(std::ostream &out) const;
        virtual void output(std::ostream &out) const;
        virtual void input(std::istream &in);
    ///@}

    // =======================
    // - [Accessors] -
    // =======================
    /** @name Accessors */
    ///@{
        virtual std::string getID() const { return mName; }
        
        /**
         * Return the number of subgrids.  Subgrid ID's (SGIDs) range between
         * [1, numSubgrids()].
         */
        int numSubgrids() const { return mSubgrids.size(); }

        /**
         * Return the subgrid with the specified ID.  Note: ID's begin at 1.
         */
        Subgrid *subgrid(int sgid) const;

        /**
         * Return true if this grid contains the specified subgrid.
         */
        bool containsSubgrid(Subgrid *sg) const;

        /**
         * Return how many neighbors we have to pass through to rotate 0
         * degrees in this grid.
         */
        int orient0() const;

        /**
         * Return how many neighbors we have to pass through to rotate 45
         * degrees in this grid.
         */
        int orient45() const;

        /**
         * Return how many neighbors we have to pass through to rotate 90
         * degrees in this grid.
         */
        int orient90() const;
        
        /**
         * Return how many neighbors we have to pass through to rotate 135
         * degrees in this grid.
         */
        int orient135() const;

        /**
         * Return how many neighbors we have to pass through to rotate 180
         * degrees in this grid.
         */
        int orient180() const;

        /**
         * Return how many neighbors we have to pass through to rotate 225
         * degrees in this grid.
         */
        int orient225() const;

        /**
         * Return how many neighbors we have to pass through to rotate 270
         * degrees in this grid.
         */
        int orient270() const;

        /**
         * Return how many neighbors we have to pass through to rotate 315
         * degrees in this grid.
         */
        int orient315() const;

        /**
         * Return how many neighbors we have to pass through to rotate 360
         * degrees in this grid.
         */
        int orient360() const;

        /** 
         * If the specified coordinate lies in a border map translate it.
         */
        GlobalCoordinate resolveBMap(const GlobalCoordinate &src) const;
    ///@}
    
    // =======================
    // - [Mutators] -
    // =======================
    /** @name Mutators */
    ///@{
        /** Specify that subgrid sg should be connected to this grid */ 
        void addSubgrid(Subgrid *sg);

        /**
         * Add a connectivity relationship between two subgrid.
         * Note that srcRegion should be in the halo of srcSG, and tgtRegion
         * should not be in the halo of the target subgrid.
         **/
        void addBorder(const Region &srcRegion, Subgrid *srcSG,
                       const Region &tgtRegion, Subgrid *tgtSG, int rotation);
        
    ///@}

    std::string mName;
    std::vector<Subgrid*>  mSubgrids;
    std::vector<Region>    mBorderSrcRegions;
    std::vector<Subgrid*>  mBorderSrcSubgrids;
    std::vector<Region>    mBorderTgtRegions;
    std::vector<Subgrid*>  mBorderTgtSubgrids;
    std::vector<int>       mBorderRotation;

  protected:
    // =======================
    // - [Construction] -
    // =======================
    Grid(const std::string &name);
    
    // =======================
    // - [Constants] -
    // =======================
        //static const int BMAP_HDL__NO_MAP = -1; // Specifies a lack of a border
                                                // mapping.
};

/** @}*/
