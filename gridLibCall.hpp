/******************************************************************************
 * GRIDGEN: Grid Generating Compiler
 * By: Andy Stone (aistone@gmail.com)
 * (C) Copyright 2011 Colorado State University
 *****************************************************************************/
#ifndef GRIDLIBCALL_HPP_
#define GRIDLIBCALL_HPP_

#include <rose.h>
#include <iostream>
#include <vector>
#include <map>
#include "distribution.hpp"
#include "dataObject.hpp"
#include "grid.hpp"
#include "iprintable.hpp"

class GridLibCall;

/** Common GridLibCall types */
typedef std::vector<GridLibCall*> CallsVectorT;
typedef std::vector<GridLibCall*>::iterator CallsIteratorT;
typedef std::vector<GridLibCall*>::const_iterator CallsConstIteratorT;


enum ENVIRONMENT_TYPE_CODE {
    //ENVIRONMENT_STRING = 0,
    //ENVIRONMENT_FILE,
    
    ENVIRONMENT_NEIGHBOR = 0,
    ENVIRONMENT_SUB_GRID,
    ENVIRONMENT_GRID,
    ENVIRONMENT_DISTRIBUTION,
    ENVIRONMENT_SCHEDULE,
    ENVIRONMENT_DATA_OBJECT,

    ENVIRONMENT_UNTYPED
};

/** Types of GridLIB calls that need to be addressed by the code generator */
enum GridLibCallCode {
    CALL__SUBGRID_NEW,
    CALL__SUBGRID_WIDTH,
    CALL__SUBGRID_HEIGHT,

    CALL__GRID_NEW,
    CALL__GRID_ADDSUBGRID,
    CALL__GRID_ADDBORDER,
    CALL__GRID_PLACEADJACENTLR,
    CALL__GRID_PLACEADJACENTRL,
    CALL__GRID_PLACEADJACENTTB,
    CALL__GRID_PLACEADJACENTBT,
    CALL__GRID_CONNECTTTOB,
    CALL__GRID_CONNECTRTOL,
    CALL__GRID_CONNECTBTOT,
    CALL__GRID_CONNECTLTOR,
    CALL__GRID_CONNECTLTOT,
    CALL__GRID_CONNECTLTOB,
    CALL__GRID_CONNECTRTOT,
    CALL__GRID_CONNECTRTOB,
    CALL__GRID_CONNECTTTOL,
    CALL__GRID_CONNECTTTOR,
    CALL__GRID_CONNECTBTOL,
    CALL__GRID_CONNECTBTOR,
    CALL__GRID_WRAPLR,
    CALL__GRID_WRAPTB,
    CALL__GRID_PLACEADJACENTWITHOFFSETLR,
    CALL__GRID_PLACEADJACENTWITHOFFSETRL,
    CALL__GRID_PLACEADJACENTWITHOFFSETTB,
    CALL__GRID_PLACEADJACENTWITHOFFSETBT,
    CALL__GRID_MIRRORT,
    CALL__GRID_MIRRORB,
    CALL__GRID_MIRRORL,
    CALL__GRID_MIRRORR,
    CALL__GRID_FOLDT,
    CALL__GRID_FOLDB,
    CALL__GRID_FOLDL,
    CALL__GRID_FOLDR,
    CALL__GRID_NUMSUBGRIDS,
    CALL__GRID_GETSUBGRID,
    CALL__GRID_ORIENT0,
    CALL__GRID_ORIENT45,
    CALL__GRID_ORIENT90,
    CALL__GRID_ORIENT135,
    CALL__GRID_ORIENT180,
    CALL__GRID_ORIENT225,
    CALL__GRID_ORIENT270,
    CALL__GRID_ORIENT315,
    CALL__GRID_ORIENT360,

    CALL__DISTRIBUTION_NEW,
    CALL__DISTRIBUTION_APPLYFILLBLOCK,
    CALL__DISTRIBUTION_APPLYBLOCKFILL,
    CALL__DISTRIBUTION_APPLYBLOCKCYCLIC,
    CALL__DISTRIBUTION_VISUALIZE,

    CALL__ENVIRONMENT_PRINT,
    CALL__ENVIRONMENT_OUTPUT,
    CALL__ENVIRONMENT_INPUT,
    CALL__ENVIRONMENT_CLEAR,

    CALL__SCHEDULE_NEW,
    CALL__SCHEDULE_CALCULATE,
    CALL__SCHEDULE_TRANSFERTOFORTRAN,
    CALL__SCHEDULE_PRINTFORTRANVERSION,
    CALL__SCHEDULE_SENDREGIONSIZE,
    CALL__SCHEDULE_RECVREGIONSIZE,

    CALL__DATA_NEW,
    CALL__DATA_PRINT,
    CALL__DATA_PRINTFORPROCS,
    CALL__DATA_PRINTFORPROC,
    CALL__DATA_APPLY1,
    CALL__DATA_APPLY2,
    CALL__DATA_APPLY3,
    CALL__DATA_APPLY4,
    CALL__DATA_APPLY5,
    CALL__DATA_APPLY6,
    CALL__DATA_APPLY7,
    CALL__DATA_APPLY8,
    CALL__DATA_APPLY9,
    CALL__DATA_APPLY10,
    CALL__DATA_FORCEUPDATE,

    CALL__NUM_CALLS
};

/**
 * Maps GridLib call IDs to strings.
 */
extern const std::string CALL_IDENT[CALL__NUM_CALLS];


/**
 * Maps GridLib call codes to functions that can construct wrappers for
 * the specified call.
 **/
extern GridLibCall* (*CALL_CONSTRUCTOR[CALL__NUM_CALLS])(SgFunctionCallExp *node);


/* call classes */
class Call__subgrid_new;
class Call__subgrid_width;
class Call__subgrid_height;
class Call__grid_new;
class Call__grid_addSubgrid;
class Call__grid_addBorder;
class Call__grid_placeAdjacentLR;
class Call__grid_placeAdjacentRL;
class Call__grid_placeAdjacentTB;
class Call__grid_placeAdjacentBT;
class Call__grid_connectTtoB;
class Call__grid_connectRtoL;
class Call__grid_connectBtoT;
class Call__grid_connectLtoR;
class Call__grid_connectLtoT;
class Call__grid_connectLtoB;
class Call__grid_connectRtoT;
class Call__grid_connectRtoB;
class Call__grid_connectTtoL;
class Call__grid_connectTtoR;
class Call__grid_connectBtoL;
class Call__grid_connectBtoR;
class Call__grid_wrapLR;
class Call__grid_wrapTB;
class Call__grid_placeAdjacentWithOffsetLR;
class Call__grid_placeAdjacentWithOffsetRL;
class Call__grid_placeAdjacentWithOffsetTB;
class Call__grid_placeAdjacentWithOffsetBT;
class Call__grid_mirrorT;
class Call__grid_mirrorB;
class Call__grid_mirrorL;
class Call__grid_mirrorR;
class Call__grid_foldT;
class Call__grid_foldB;
class Call__grid_foldL;
class Call__grid_foldR;
class Call__grid_numSubgrids;
class Call__grid_getSubgrid;
class Call__grid_orient0;
class Call__grid_orient45;
class Call__grid_orient90;
class Call__grid_orient135;
class Call__grid_orient180;
class Call__grid_orient225;
class Call__grid_orient270;
class Call__grid_orient315;
class Call__grid_orient360;
class Call__distribution_new;
class Call__distribution_applyFillBlock;
class Call__distribution_applyBlockFill;
class Call__distribution_applyBlockCyclic;
class Call__distribution_visualize;
class Call__environment_print;
class Call__environment_output;
class Call__environment_input;
class Call__environment_clear;
class Call__schedule_new;
class Call__schedule_calculate;
class Call__schedule_transferToFortran;
class Call__schedule_printFortranVersion;
class Call__schedule_sendRegionSize;
class Call__schedule_recvRegionSize;
class Call__data_new;
class Call__data_print;
class Call__data_printForProcs;
class Call__data_printForProc;
class Call__data_apply1;
class Call__data_apply2;
class Call__data_apply3;
class Call__data_apply4;
class Call__data_apply5;
class Call__data_apply6;
class Call__data_apply7;
class Call__data_apply8;
class Call__data_apply9;
class Call__data_apply10;
class Call__data_forceUpdate;

/**
 * Abstract visitor to for visiting calls in a list.
 **/
class CallVisitor {
  public:
    virtual void accept(const CallsVectorT &v);

    virtual void visit__default(GridLibCall *call) { }

    virtual void visit__subgrid_new(Call__subgrid_new *call);
    virtual void visit__subgrid_width(Call__subgrid_width *call);
    virtual void visit__subgrid_height(Call__subgrid_height *call);
    virtual void visit__grid_new(Call__grid_new *call);
    virtual void visit__grid_addSubgrid(Call__grid_addSubgrid *call);
    virtual void visit__grid_addBorder(Call__grid_addBorder *call);
    virtual void visit__grid_placeAdjacentLR(Call__grid_placeAdjacentLR *call);
    virtual void visit__grid_placeAdjacentRL(Call__grid_placeAdjacentRL *call);
    virtual void visit__grid_placeAdjacentTB(Call__grid_placeAdjacentTB *call);
    virtual void visit__grid_placeAdjacentBT(Call__grid_placeAdjacentBT *call);
    virtual void visit__grid_connectTtoB(Call__grid_connectTtoB *call);
    virtual void visit__grid_connectRtoL(Call__grid_connectRtoL *call);
    virtual void visit__grid_connectBtoT(Call__grid_connectBtoT *call);
    virtual void visit__grid_connectLtoR(Call__grid_connectLtoR *call);
    virtual void visit__grid_connectLtoT(Call__grid_connectLtoT *call);
    virtual void visit__grid_connectLtoB(Call__grid_connectLtoB *call);
    virtual void visit__grid_connectRtoT(Call__grid_connectRtoT *call);
    virtual void visit__grid_connectRtoB(Call__grid_connectRtoB *call);
    virtual void visit__grid_connectTtoL(Call__grid_connectTtoL *call);
    virtual void visit__grid_connectTtoR(Call__grid_connectTtoR *call);
    virtual void visit__grid_connectBtoL(Call__grid_connectBtoL *call);
    virtual void visit__grid_connectBtoR(Call__grid_connectBtoR *call);
    virtual void visit__grid_wrapLR(Call__grid_wrapLR *call);
    virtual void visit__grid_wrapTB(Call__grid_wrapTB *call);
    virtual void visit__grid_placeAdjacentWithOffsetLR(Call__grid_placeAdjacentWithOffsetLR *call);
    virtual void visit__grid_placeAdjacentWithOffsetRL(Call__grid_placeAdjacentWithOffsetRL *call);
    virtual void visit__grid_placeAdjacentWithOffsetTB(Call__grid_placeAdjacentWithOffsetTB *call);
    virtual void visit__grid_placeAdjacentWithOffsetBT(Call__grid_placeAdjacentWithOffsetBT *call);
    virtual void visit__grid_mirrorT(Call__grid_mirrorT *call);
    virtual void visit__grid_mirrorB(Call__grid_mirrorB *call);
    virtual void visit__grid_mirrorL(Call__grid_mirrorL *call);
    virtual void visit__grid_mirrorR(Call__grid_mirrorR *call);
    virtual void visit__grid_foldT(Call__grid_foldT *call);
    virtual void visit__grid_foldB(Call__grid_foldB *call);
    virtual void visit__grid_foldL(Call__grid_foldL *call);
    virtual void visit__grid_foldR(Call__grid_foldR *call);
    virtual void visit__grid_numSubgrids(Call__grid_numSubgrids *call);
    virtual void visit__grid_getSubgrid(Call__grid_getSubgrid *call);
    virtual void visit__grid_orient0(Call__grid_orient0 *call);
    virtual void visit__grid_orient45(Call__grid_orient45 *call);
    virtual void visit__grid_orient90(Call__grid_orient90 *call);
    virtual void visit__grid_orient135(Call__grid_orient135 *call);
    virtual void visit__grid_orient180(Call__grid_orient180 *call);
    virtual void visit__grid_orient225(Call__grid_orient225 *call);
    virtual void visit__grid_orient270(Call__grid_orient270 *call);
    virtual void visit__grid_orient315(Call__grid_orient315 *call);
    virtual void visit__grid_orient360(Call__grid_orient360 *call);
    virtual void visit__distribution_new(Call__distribution_new *call);
    virtual void visit__distribution_applyFillBlock(Call__distribution_applyFillBlock *call);
    virtual void visit__distribution_applyBlockFill(Call__distribution_applyBlockFill *call);
    virtual void visit__distribution_applyBlockCyclic(Call__distribution_applyBlockCyclic *call);
    virtual void visit__distribution_visualize(Call__distribution_visualize *call);
    virtual void visit__environment_print(Call__environment_print *call);
    virtual void visit__environment_output(Call__environment_output *call);
    virtual void visit__environment_input(Call__environment_input *call);
    virtual void visit__environment_clear(Call__environment_clear *call);
    virtual void visit__schedule_new(Call__schedule_new *call);
    virtual void visit__schedule_calculate(Call__schedule_calculate *call);
    virtual void visit__schedule_transferToFortran(Call__schedule_transferToFortran *call);
    virtual void visit__schedule_printFortranVersion(Call__schedule_printFortranVersion *call);
    virtual void visit__schedule_sendRegionSize(Call__schedule_sendRegionSize *call);
    virtual void visit__schedule_recvRegionSize(Call__schedule_recvRegionSize *call);
    virtual void visit__data_new(Call__data_new *call);
    virtual void visit__data_print(Call__data_print *call);
    virtual void visit__data_printForProcs(Call__data_printForProcs *call);
    virtual void visit__data_printForProc(Call__data_printForProc *call);
    virtual void visit__data_apply1(Call__data_apply1 *call);
    virtual void visit__data_apply2(Call__data_apply2 *call);
    virtual void visit__data_apply3(Call__data_apply3 *call);
    virtual void visit__data_apply4(Call__data_apply4 *call);
    virtual void visit__data_apply5(Call__data_apply5 *call);
    virtual void visit__data_apply6(Call__data_apply6 *call);
    virtual void visit__data_apply7(Call__data_apply7 *call);
    virtual void visit__data_apply8(Call__data_apply8 *call);
    virtual void visit__data_apply9(Call__data_apply9 *call);
    virtual void visit__data_apply10(Call__data_apply10 *call);
    virtual void visit__data_forceUpdate(Call__data_forceUpdate *call);
};

/* Call construction functions */
GridLibCall* constructCall__subgrid_new(SgFunctionCallExp *node);
GridLibCall* constructCall__subgrid_width(SgFunctionCallExp *node);
GridLibCall* constructCall__subgrid_height(SgFunctionCallExp *node);
GridLibCall* constructCall__grid_new(SgFunctionCallExp *node);
GridLibCall* constructCall__grid_addSubgrid(SgFunctionCallExp *node);
GridLibCall* constructCall__grid_addBorder(SgFunctionCallExp *node);
GridLibCall* constructCall__grid_placeAdjacentLR(SgFunctionCallExp *node);
GridLibCall* constructCall__grid_placeAdjacentRL(SgFunctionCallExp *node);
GridLibCall* constructCall__grid_placeAdjacentTB(SgFunctionCallExp *node);
GridLibCall* constructCall__grid_placeAdjacentBT(SgFunctionCallExp *node);
GridLibCall* constructCall__grid_connectTtoB(SgFunctionCallExp *node);
GridLibCall* constructCall__grid_connectRtoL(SgFunctionCallExp *node);
GridLibCall* constructCall__grid_connectBtoT(SgFunctionCallExp *node);
GridLibCall* constructCall__grid_connectLtoR(SgFunctionCallExp *node);
GridLibCall* constructCall__grid_connectLtoT(SgFunctionCallExp *node);
GridLibCall* constructCall__grid_connectLtoB(SgFunctionCallExp *node);
GridLibCall* constructCall__grid_connectRtoT(SgFunctionCallExp *node);
GridLibCall* constructCall__grid_connectRtoB(SgFunctionCallExp *node);
GridLibCall* constructCall__grid_connectTtoL(SgFunctionCallExp *node);
GridLibCall* constructCall__grid_connectTtoR(SgFunctionCallExp *node);
GridLibCall* constructCall__grid_connectBtoL(SgFunctionCallExp *node);
GridLibCall* constructCall__grid_connectBtoR(SgFunctionCallExp *node);
GridLibCall* constructCall__grid_wrapLR(SgFunctionCallExp *node);
GridLibCall* constructCall__grid_wrapTB(SgFunctionCallExp *node);
GridLibCall* constructCall__grid_placeAdjacentWithOffsetLR(SgFunctionCallExp *node);
GridLibCall* constructCall__grid_placeAdjacentWithOffsetRL(SgFunctionCallExp *node);
GridLibCall* constructCall__grid_placeAdjacentWithOffsetTB(SgFunctionCallExp *node);
GridLibCall* constructCall__grid_placeAdjacentWithOffsetBT(SgFunctionCallExp *node);
GridLibCall* constructCall__grid_mirrorT(SgFunctionCallExp *node);
GridLibCall* constructCall__grid_mirrorB(SgFunctionCallExp *node);
GridLibCall* constructCall__grid_mirrorL(SgFunctionCallExp *node);
GridLibCall* constructCall__grid_mirrorR(SgFunctionCallExp *node);
GridLibCall* constructCall__grid_foldT(SgFunctionCallExp *node);
GridLibCall* constructCall__grid_foldB(SgFunctionCallExp *node);
GridLibCall* constructCall__grid_foldL(SgFunctionCallExp *node);
GridLibCall* constructCall__grid_foldR(SgFunctionCallExp *node);
GridLibCall* constructCall__grid_numSubgrids(SgFunctionCallExp *node);
GridLibCall* constructCall__grid_getSubgrid(SgFunctionCallExp *node);
GridLibCall* constructCall__grid_orient0(SgFunctionCallExp *node);
GridLibCall* constructCall__grid_orient45(SgFunctionCallExp *node);
GridLibCall* constructCall__grid_orient90(SgFunctionCallExp *node);
GridLibCall* constructCall__grid_orient135(SgFunctionCallExp *node);
GridLibCall* constructCall__grid_orient180(SgFunctionCallExp *node);
GridLibCall* constructCall__grid_orient225(SgFunctionCallExp *node);
GridLibCall* constructCall__grid_orient270(SgFunctionCallExp *node);
GridLibCall* constructCall__grid_orient315(SgFunctionCallExp *node);
GridLibCall* constructCall__grid_orient360(SgFunctionCallExp *node);
GridLibCall* constructCall__distribution_new(SgFunctionCallExp *node);
GridLibCall* constructCall__distribution_applyFillBlock(SgFunctionCallExp *node);
GridLibCall* constructCall__distribution_applyBlockFill(SgFunctionCallExp *node);
GridLibCall* constructCall__distribution_applyBlockCyclic(SgFunctionCallExp *node);
GridLibCall* constructCall__distribution_visualize(SgFunctionCallExp *node);
GridLibCall* constructCall__environment_print(SgFunctionCallExp *node);
GridLibCall* constructCall__environment_output(SgFunctionCallExp *node);
GridLibCall* constructCall__environment_input(SgFunctionCallExp *node);
GridLibCall* constructCall__environment_clear(SgFunctionCallExp *node);
GridLibCall* constructCall__schedule_new(SgFunctionCallExp *node);
GridLibCall* constructCall__schedule_calculate(SgFunctionCallExp *node);
GridLibCall* constructCall__schedule_transferToFortran(SgFunctionCallExp *node);
GridLibCall* constructCall__schedule_printFortranVersion(SgFunctionCallExp *node);
GridLibCall* constructCall__schedule_sendRegionSize(SgFunctionCallExp *node);
GridLibCall* constructCall__schedule_recvRegionSize(SgFunctionCallExp *node);
GridLibCall* constructCall__data_new(SgFunctionCallExp *node);
GridLibCall* constructCall__data_print(SgFunctionCallExp *node);
GridLibCall* constructCall__data_printForProcs(SgFunctionCallExp *node);
GridLibCall* constructCall__data_printForProc(SgFunctionCallExp *node);
GridLibCall* constructCall__data_apply1(SgFunctionCallExp *node);
GridLibCall* constructCall__data_apply2(SgFunctionCallExp *node);
GridLibCall* constructCall__data_apply3(SgFunctionCallExp *node);
GridLibCall* constructCall__data_apply4(SgFunctionCallExp *node);
GridLibCall* constructCall__data_apply5(SgFunctionCallExp *node);
GridLibCall* constructCall__data_apply6(SgFunctionCallExp *node);
GridLibCall* constructCall__data_apply7(SgFunctionCallExp *node);
GridLibCall* constructCall__data_apply8(SgFunctionCallExp *node);
GridLibCall* constructCall__data_apply9(SgFunctionCallExp *node);
GridLibCall* constructCall__data_apply10(SgFunctionCallExp *node);
GridLibCall* constructCall__data_forceUpdate(SgFunctionCallExp *node);






/** Represents a call to gridlib (associates a call code with a node) */
class GridLibCall : public IPrintable {
  public:
    /**
     * Construct a new call object given a calling code and a location
     * in the AST.
     **/
    GridLibCall(GridLibCallCode operation, SgFunctionCallExp *location) :
        mOperation(operation),
        mLocation(location)
    { }

    virtual void accept(CallVisitor &v) = 0; 

    virtual GridLibCallCode operation() const { return mOperation; }
    virtual SgFunctionCallExp* location() const { return mLocation; }

  private:
    GridLibCallCode mOperation;
    SgFunctionCallExp *mLocation;
};


class Call__subgrid_new : public virtual GridLibCall {
  private:
    std::string mLHS;
    std::string mEnvName;
    std::string mWidth;
    std::string mHeight;
 
  public:
    Call__subgrid_new(SgFunctionCallExp *location,
        std::string lhs,
        std::string envName,
        std::string width,
        std::string height
    )
        : GridLibCall(CALL__SUBGRID_NEW, location),
          mLHS(lhs),
          mEnvName(envName),
          mWidth(width),
          mHeight(height)
    { }

    virtual void print(std::ostream &out) const;

    virtual void printSimp(std::ostream &out) const {
        out << "<Call__subgrid_new>";
    }

    virtual void accept(CallVisitor &v) {
        v.visit__subgrid_new(this);
    }

    std::string lhs()     const { return mLHS;     }
    std::string envName() const { return mEnvName; }
    std::string width()   const { return mWidth;   }
    std::string height()  const { return mHeight;  }
};

class Call__subgrid_width : public virtual GridLibCall {
  public:
    Call__subgrid_width(SgFunctionCallExp *location)
        : GridLibCall(CALL__SUBGRID_WIDTH, location)
    { }

    virtual void print(std::ostream &out) const;

    virtual void printSimp(std::ostream &out) const {
        out << "<Call__subgrid_width>";
    }

    virtual void accept(CallVisitor &v) {
        v.visit__subgrid_width(this);
    }
};

class Call__subgrid_height : public virtual GridLibCall {
  public:
    Call__subgrid_height(SgFunctionCallExp *location)
        : GridLibCall(CALL__SUBGRID_HEIGHT, location)
    { }

    virtual void print(std::ostream &out) const;

    virtual void printSimp(std::ostream &out) const {
        out << "<Call__subgrid_height>";
    }

    virtual void accept(CallVisitor &v) {
        v.visit__subgrid_height(this);
    }
};

class Call__grid_new : public virtual GridLibCall {
  private:
    std::string mLHS;
    std::string mEnvName;

  public:
    Call__grid_new(SgFunctionCallExp *location,
        std::string lhs,
        std::string envName)
        :
          GridLibCall(CALL__GRID_NEW, location),
          mLHS(lhs),
          mEnvName(envName)
    { }

    virtual void print(std::ostream &out) const;

    virtual void printSimp(std::ostream &out) const {
        out << "<Call__grid_new>";
    }

    virtual void accept(CallVisitor &v) {
        v.visit__grid_new(this);
    }

    std::string lhs()     const { return mLHS; }
    std::string envName() const { return mEnvName; }
};

class Call__grid_addSubgrid : public virtual GridLibCall {
  private:
    std::string mGrid;
    std::string mSubgrid;

  public:
    Call__grid_addSubgrid(SgFunctionCallExp *location,
        std::string grid,
        std::string subgrid)
        :
          GridLibCall(CALL__GRID_ADDSUBGRID, location),
          mGrid(grid),
          mSubgrid(subgrid)
    { }

    virtual void print(std::ostream &out) const;

    virtual void printSimp(std::ostream &out) const {
        out << "<Call__grid_addSubgrid>";
    }

    virtual void accept(CallVisitor &v) {
        v.visit__grid_addSubgrid(this);
    }

    std::string grid()    { return mGrid;    }
    std::string subgrid() { return mSubgrid; }
};

class Call__grid_addBorder : public virtual GridLibCall {
  private:
    std::string mGrid;
    std::string mSrcX1;
    std::string mSrcY1;
    std::string mSrcX2;
    std::string mSrcY2;
    std::string mSrcSG;
    std::string mTgtX1;
    std::string mTgtY1;
    std::string mTgtX2;
    std::string mTgtY2;
    std::string mTgtSG;
    std::string mRotation;

  public:
    Call__grid_addBorder(SgFunctionCallExp *location,
        std::string grid,
        std::string srcX1,
        std::string srcY1,
        std::string srcX2,
        std::string srcY2,
        std::string srcSG,
        std::string tgtX1,
        std::string tgtY1,
        std::string tgtX2,      
        std::string tgtY2,      
        std::string tgtSG,
        std::string rotation)
        :
          GridLibCall(CALL__GRID_ADDBORDER, location),
          mGrid(grid),
          mSrcX1(srcX1),
          mSrcY1(srcY1),
          mSrcX2(srcX2),
          mSrcY2(srcY2),
          mSrcSG(srcSG),
          mTgtX1(tgtX1),
          mTgtY1(tgtY1),
          mTgtX2(tgtX2),
          mTgtY2(tgtY2),
          mTgtSG(tgtSG),
          mRotation(rotation)
    { }

    virtual void print(std::ostream &out) const;

    virtual void printSimp(std::ostream &out) const {
        out << "<Call__grid_addBorder>";
    }

    virtual void accept(CallVisitor &v) {
        v.visit__grid_addBorder(this);
    }

    std::string grid()     const { return mGrid;     }
    std::string srcX1()    const { return mSrcX1;    }
    std::string srcY1()    const { return mSrcY1;    }
    std::string srcX2()    const { return mSrcX2;    }
    std::string srcY2()    const { return mSrcY2;    }
    std::string srcSG()    const { return mSrcSG;    }
    std::string tgtX1()    const { return mTgtX1;    }
    std::string tgtY1()    const { return mTgtY1;    }
    std::string tgtX2()    const { return mTgtX2;    }
    std::string tgtY2()    const { return mTgtY2;    }
    std::string tgtSG()    const { return mTgtSG;    }
    std::string rotation() const { return mRotation; }
};

class Call__grid_placeAdjacentLR : public virtual GridLibCall {
  public:
    Call__grid_placeAdjacentLR(SgFunctionCallExp *location)
        : GridLibCall(CALL__GRID_PLACEADJACENTLR, location)
    { }

    virtual void print(std::ostream &out) const;

    virtual void printSimp(std::ostream &out) const {
        out << "<Call__grid_placeAdjacentLR>";
    }

    virtual void accept(CallVisitor &v) {
        v.visit__grid_placeAdjacentLR(this);
    }
};

class Call__grid_placeAdjacentRL : public virtual GridLibCall {
  public:
    Call__grid_placeAdjacentRL(SgFunctionCallExp *location)
        : GridLibCall(CALL__GRID_PLACEADJACENTRL, location)
    { }

    virtual void print(std::ostream &out) const;

    virtual void printSimp(std::ostream &out) const {
        out << "<Call__grid_placeAdjacentRL>";
    }

    virtual void accept(CallVisitor &v) {
        v.visit__grid_placeAdjacentRL(this);
    }
};

class Call__grid_placeAdjacentTB : public virtual GridLibCall {
  public:
    Call__grid_placeAdjacentTB(SgFunctionCallExp *location)
        : GridLibCall(CALL__GRID_PLACEADJACENTTB, location)
    { }

    virtual void print(std::ostream &out) const;

    virtual void printSimp(std::ostream &out) const {
        out << "<Call__grid_placeAdjacentTB>";
    }

    virtual void accept(CallVisitor &v) {
        v.visit__grid_placeAdjacentTB(this);
    }
};

class Call__grid_placeAdjacentBT : public virtual GridLibCall {
  public:
    Call__grid_placeAdjacentBT(SgFunctionCallExp *location)
        : GridLibCall(CALL__GRID_PLACEADJACENTBT, location)
    { }

    virtual void print(std::ostream &out) const;

    virtual void printSimp(std::ostream &out) const {
        out << "<Call__grid_placeAdjacentBT>";
    }

    virtual void accept(CallVisitor &v) {
        v.visit__grid_placeAdjacentBT(this);
    }
};

class Call__grid_connectTtoB : public virtual GridLibCall {
  public:
    Call__grid_connectTtoB(SgFunctionCallExp *location)
        : GridLibCall(CALL__GRID_CONNECTTTOB, location)
    { }

    virtual void print(std::ostream &out) const;

    virtual void printSimp(std::ostream &out) const {
        out << "<Call__grid_connectTtoB>";
    }

    virtual void accept(CallVisitor &v) {
        v.visit__grid_connectTtoB(this);
    }
};

class Call__grid_connectRtoL : public virtual GridLibCall {
  public:
    Call__grid_connectRtoL(SgFunctionCallExp *location)
        : GridLibCall(CALL__GRID_CONNECTRTOL, location)
    { }

    virtual void print(std::ostream &out) const;

    virtual void printSimp(std::ostream &out) const {
        out << "<Call__grid_connectRtoL>";
    }

    virtual void accept(CallVisitor &v) {
        v.visit__grid_connectRtoL(this);
    }
};

class Call__grid_connectBtoT : public virtual GridLibCall {
  public:
    Call__grid_connectBtoT(SgFunctionCallExp *location)
        : GridLibCall(CALL__GRID_CONNECTBTOT, location)
    { }

    virtual void print(std::ostream &out) const;

    virtual void printSimp(std::ostream &out) const {
        out << "<Call__grid_connectBtoT>";
    }

    virtual void accept(CallVisitor &v) {
        v.visit__grid_connectBtoT(this);
    }
};

class Call__grid_connectLtoR : public virtual GridLibCall {
  public:
    Call__grid_connectLtoR(SgFunctionCallExp *location)
        : GridLibCall(CALL__GRID_CONNECTLTOR, location)
    { }

    virtual void print(std::ostream &out) const;

    virtual void printSimp(std::ostream &out) const {
        out << "<Call__grid_connectLtoR>";
    }

    virtual void accept(CallVisitor &v) {
        v.visit__grid_connectLtoR(this);
    }
};

class Call__grid_connectLtoT : public virtual GridLibCall {
  public:
    Call__grid_connectLtoT(SgFunctionCallExp *location)
        : GridLibCall(CALL__GRID_CONNECTLTOT, location)
    { }

    virtual void print(std::ostream &out) const;

    virtual void printSimp(std::ostream &out) const {
        out << "<Call__grid_connectLtoT>";
    }

    virtual void accept(CallVisitor &v) {
        v.visit__grid_connectLtoT(this);
    }
};

class Call__grid_connectLtoB : public virtual GridLibCall {
  public:
    Call__grid_connectLtoB(SgFunctionCallExp *location)
        : GridLibCall(CALL__GRID_CONNECTLTOB, location)
    { }

    virtual void print(std::ostream &out) const;

    virtual void printSimp(std::ostream &out) const {
        out << "<Call__grid_connectLtoB>";
    }

    virtual void accept(CallVisitor &v) {
        v.visit__grid_connectLtoB(this);
    }
};

class Call__grid_connectRtoT : public virtual GridLibCall {
  public:
    Call__grid_connectRtoT(SgFunctionCallExp *location)
        : GridLibCall(CALL__GRID_CONNECTRTOT, location)
    { }

    virtual void print(std::ostream &out) const;

    virtual void printSimp(std::ostream &out) const {
        out << "<Call__grid_connectRtoT>";
    }

    virtual void accept(CallVisitor &v) {
        v.visit__grid_connectRtoT(this);
    }
};

class Call__grid_connectRtoB : public virtual GridLibCall {
  public:
    Call__grid_connectRtoB(SgFunctionCallExp *location)
        : GridLibCall(CALL__GRID_CONNECTRTOB, location)
    { }

    virtual void print(std::ostream &out) const;

    virtual void printSimp(std::ostream &out) const {
        out << "<Call__grid_connectRtoB>";
    }

    virtual void accept(CallVisitor &v) {
        v.visit__grid_connectRtoB(this);
    }
};

class Call__grid_connectTtoL : public virtual GridLibCall {
  public:
    Call__grid_connectTtoL(SgFunctionCallExp *location)
        : GridLibCall(CALL__GRID_CONNECTTTOL, location)
    { }

    virtual void print(std::ostream &out) const;

    virtual void printSimp(std::ostream &out) const {
        out << "<Call__grid_connectTtoL>";
    }

    virtual void accept(CallVisitor &v) {
        v.visit__grid_connectTtoL(this);
    }
};

class Call__grid_connectTtoR : public virtual GridLibCall {
  public:
    Call__grid_connectTtoR(SgFunctionCallExp *location)
        : GridLibCall(CALL__GRID_CONNECTTTOR, location)
    { }

    virtual void print(std::ostream &out) const;

    virtual void printSimp(std::ostream &out) const {
        out << "<Call__grid_connectTtoR>";
    }

    virtual void accept(CallVisitor &v) {
        v.visit__grid_connectTtoR(this);
    }
};

class Call__grid_connectBtoL : public virtual GridLibCall {
  public:
    Call__grid_connectBtoL(SgFunctionCallExp *location)
        : GridLibCall(CALL__GRID_CONNECTBTOL, location)
    { }

    virtual void print(std::ostream &out) const;

    virtual void printSimp(std::ostream &out) const {
        out << "<Call__grid_connectBtoL>";
    }

    virtual void accept(CallVisitor &v) {
        v.visit__grid_connectBtoL(this);
    }
};

class Call__grid_connectBtoR : public virtual GridLibCall {
  public:
    Call__grid_connectBtoR(SgFunctionCallExp *location)
        : GridLibCall(CALL__GRID_CONNECTBTOR, location)
    { }

    virtual void print(std::ostream &out) const;

    virtual void printSimp(std::ostream &out) const {
        out << "<Call__grid_connectBtoR>";
    }

    virtual void accept(CallVisitor &v) {
        v.visit__grid_connectBtoR(this);
    }
};

class Call__grid_wrapLR : public virtual GridLibCall {
  public:
    Call__grid_wrapLR(SgFunctionCallExp *location)
        : GridLibCall(CALL__GRID_WRAPLR, location)
    { }

    virtual void print(std::ostream &out) const;

    virtual void printSimp(std::ostream &out) const {
        out << "<Call__grid_wrapLR>";
    }

    virtual void accept(CallVisitor &v) {
        v.visit__grid_wrapLR(this);
    }
};

class Call__grid_wrapTB : public virtual GridLibCall {
  public:
    Call__grid_wrapTB(SgFunctionCallExp *location)
        : GridLibCall(CALL__GRID_WRAPTB, location)
    { }

    virtual void print(std::ostream &out) const;

    virtual void printSimp(std::ostream &out) const {
        out << "<Call__grid_wrapTB>";
    }

    virtual void accept(CallVisitor &v) {
        v.visit__grid_wrapTB(this);
    }
};

class Call__grid_placeAdjacentWithOffsetLR : public virtual GridLibCall {
  public:
    Call__grid_placeAdjacentWithOffsetLR(SgFunctionCallExp *location)
        : GridLibCall(CALL__GRID_PLACEADJACENTWITHOFFSETLR, location)
    { }

    virtual void print(std::ostream &out) const;

    virtual void printSimp(std::ostream &out) const {
        out << "<Call__grid_placeAdjacentWithOffsetLR>";
    }

    virtual void accept(CallVisitor &v) {
        v.visit__grid_placeAdjacentWithOffsetLR(this);
    }
};

class Call__grid_placeAdjacentWithOffsetRL : public virtual GridLibCall {
  public:
    Call__grid_placeAdjacentWithOffsetRL(SgFunctionCallExp *location)
        : GridLibCall(CALL__GRID_PLACEADJACENTWITHOFFSETRL, location)
    { }

    virtual void print(std::ostream &out) const;

    virtual void printSimp(std::ostream &out) const {
        out << "<Call__grid_placeAdjacentWithOffsetRL>";
    }

    virtual void accept(CallVisitor &v) {
        v.visit__grid_placeAdjacentWithOffsetRL(this);
    }
};

class Call__grid_placeAdjacentWithOffsetTB : public virtual GridLibCall {
  public:
    Call__grid_placeAdjacentWithOffsetTB(SgFunctionCallExp *location)
        : GridLibCall(CALL__GRID_PLACEADJACENTWITHOFFSETTB, location)
    { }

    virtual void print(std::ostream &out) const;

    virtual void printSimp(std::ostream &out) const {
        out << "<Call__grid_placeAdjacentWithOffsetTB>";
    }

    virtual void accept(CallVisitor &v) {
        v.visit__grid_placeAdjacentWithOffsetTB(this);
    }
};

class Call__grid_placeAdjacentWithOffsetBT : public virtual GridLibCall {
  public:
    Call__grid_placeAdjacentWithOffsetBT(SgFunctionCallExp *location)
        : GridLibCall(CALL__GRID_PLACEADJACENTWITHOFFSETBT, location)
    { }

    virtual void print(std::ostream &out) const;

    virtual void printSimp(std::ostream &out) const {
        out << "<Call__grid_placeAdjacentWithOffsetBT>";
    }

    virtual void accept(CallVisitor &v) {
        v.visit__grid_placeAdjacentWithOffsetBT(this);
    }
};

class Call__grid_mirrorT : public virtual GridLibCall {
  public:
    Call__grid_mirrorT(SgFunctionCallExp *location)
        : GridLibCall(CALL__GRID_MIRRORT, location)
    { }

    virtual void print(std::ostream &out) const;

    virtual void printSimp(std::ostream &out) const {
        out << "<Call__grid_mirrorT>";
    }

    virtual void accept(CallVisitor &v) {
        v.visit__grid_mirrorT(this);
    }
};

class Call__grid_mirrorB : public virtual GridLibCall {
  public:
    Call__grid_mirrorB(SgFunctionCallExp *location)
        : GridLibCall(CALL__GRID_MIRRORB, location)
    { }

    virtual void print(std::ostream &out) const;

    virtual void printSimp(std::ostream &out) const {
        out << "<Call__grid_mirrorB>";
    }

    virtual void accept(CallVisitor &v) {
        v.visit__grid_mirrorB(this);
    }
};

class Call__grid_mirrorL : public virtual GridLibCall {
  public:
    Call__grid_mirrorL(SgFunctionCallExp *location)
        : GridLibCall(CALL__GRID_MIRRORL, location)
    { }

    virtual void print(std::ostream &out) const;

    virtual void printSimp(std::ostream &out) const {
        out << "<Call__grid_mirrorL>";
    }

    virtual void accept(CallVisitor &v) {
        v.visit__grid_mirrorL(this);
    }
};

class Call__grid_mirrorR : public virtual GridLibCall {
  public:
    Call__grid_mirrorR(SgFunctionCallExp *location)
        : GridLibCall(CALL__GRID_MIRRORR, location)
    { }

    virtual void print(std::ostream &out) const;

    virtual void printSimp(std::ostream &out) const {
        out << "<Call__grid_mirrorR>";
    }

    virtual void accept(CallVisitor &v) {
        v.visit__grid_mirrorR(this);
    }
};

class Call__grid_foldT : public virtual GridLibCall {
  public:
    Call__grid_foldT(SgFunctionCallExp *location)
        : GridLibCall(CALL__GRID_FOLDT, location)
    { }

    virtual void print(std::ostream &out) const;

    virtual void printSimp(std::ostream &out) const {
        out << "<Call__grid_foldT>";
    }

    virtual void accept(CallVisitor &v) {
        v.visit__grid_foldT(this);
    }
};

class Call__grid_foldB : public virtual GridLibCall {
  public:
    Call__grid_foldB(SgFunctionCallExp *location)
        : GridLibCall(CALL__GRID_FOLDB, location)
    { }

    virtual void print(std::ostream &out) const;

    virtual void printSimp(std::ostream &out) const {
        out << "<Call__grid_foldB>";
    }

    virtual void accept(CallVisitor &v) {
        v.visit__grid_foldB(this);
    }
};

class Call__grid_foldL : public virtual GridLibCall {
  public:
    Call__grid_foldL(SgFunctionCallExp *location)
        : GridLibCall(CALL__GRID_FOLDL, location)
    { }

    virtual void print(std::ostream &out) const;

    virtual void printSimp(std::ostream &out) const {
        out << "<Call__grid_foldL>";
    }

    virtual void accept(CallVisitor &v) {
        v.visit__grid_foldL(this);
    }
};

class Call__grid_foldR : public virtual GridLibCall {
  public:
    Call__grid_foldR(SgFunctionCallExp *location)
        : GridLibCall(CALL__GRID_FOLDR, location)
    { }

    virtual void print(std::ostream &out) const;

    virtual void printSimp(std::ostream &out) const {
        out << "<Call__grid_foldR>";
    }

    virtual void accept(CallVisitor &v) {
        v.visit__grid_foldR(this);
    }
};

class Call__grid_numSubgrids : public virtual GridLibCall {
  public:
    Call__grid_numSubgrids(SgFunctionCallExp *location)
        : GridLibCall(CALL__GRID_NUMSUBGRIDS, location)
    { }

    virtual void print(std::ostream &out) const;

    virtual void printSimp(std::ostream &out) const {
        out << "<Call__grid_numSubgrids>";
    }

    virtual void accept(CallVisitor &v) {
        v.visit__grid_numSubgrids(this);
    }
};

class Call__grid_getSubgrid : public virtual GridLibCall {
  public:
    Call__grid_getSubgrid(SgFunctionCallExp *location)
        : GridLibCall(CALL__GRID_GETSUBGRID, location)
    { }

    virtual void print(std::ostream &out) const;

    virtual void printSimp(std::ostream &out) const {
        out << "<Call__grid_getSubgrid>";
    }

    virtual void accept(CallVisitor &v) {
        v.visit__grid_getSubgrid(this);
    }
};

class Call__grid_orient0 : public virtual GridLibCall {
  public:
    Call__grid_orient0(SgFunctionCallExp *location)
        : GridLibCall(CALL__GRID_ORIENT0, location)
    { }

    virtual void print(std::ostream &out) const;

    virtual void printSimp(std::ostream &out) const {
        out << "<Call__grid_orient0>";
    }

    virtual void accept(CallVisitor &v) {
        v.visit__grid_orient0(this);
    }
};

class Call__grid_orient45 : public virtual GridLibCall {
  public:
    Call__grid_orient45(SgFunctionCallExp *location)
        : GridLibCall(CALL__GRID_ORIENT45, location)
    { }

    virtual void print(std::ostream &out) const;

    virtual void printSimp(std::ostream &out) const {
        out << "<Call__grid_orient45>";
    }

    virtual void accept(CallVisitor &v) {
        v.visit__grid_orient45(this);
    }
};

class Call__grid_orient90 : public virtual GridLibCall {
  public:
    Call__grid_orient90(SgFunctionCallExp *location)
        : GridLibCall(CALL__GRID_ORIENT90, location)
    { }

    virtual void print(std::ostream &out) const;

    virtual void printSimp(std::ostream &out) const {
        out << "<Call__grid_orient90>";
    }

    virtual void accept(CallVisitor &v) {
        v.visit__grid_orient90(this);
    }
};

class Call__grid_orient135 : public virtual GridLibCall {
  public:
    Call__grid_orient135(SgFunctionCallExp *location)
        : GridLibCall(CALL__GRID_ORIENT135, location)
    { }

    virtual void print(std::ostream &out) const;

    virtual void printSimp(std::ostream &out) const {
        out << "<Call__grid_orient135>";
    }

    virtual void accept(CallVisitor &v) {
        v.visit__grid_orient135(this);
    }
};

class Call__grid_orient180 : public virtual GridLibCall {
  public:
    Call__grid_orient180(SgFunctionCallExp *location)
        : GridLibCall(CALL__GRID_ORIENT180, location)
    { }

    virtual void print(std::ostream &out) const;

    virtual void printSimp(std::ostream &out) const {
        out << "<Call__grid_orient180>";
    }

    virtual void accept(CallVisitor &v) {
        v.visit__grid_orient180(this);
    }
};

class Call__grid_orient225 : public virtual GridLibCall {
  public:
    Call__grid_orient225(SgFunctionCallExp *location)
        : GridLibCall(CALL__GRID_ORIENT225, location)
    { }

    virtual void print(std::ostream &out) const;

    virtual void printSimp(std::ostream &out) const {
        out << "<Call__grid_orient225>";
    }

    virtual void accept(CallVisitor &v) {
        v.visit__grid_orient225(this);
    }
};

class Call__grid_orient270 : public virtual GridLibCall {
  public:
    Call__grid_orient270(SgFunctionCallExp *location)
        : GridLibCall(CALL__GRID_ORIENT270, location)
    { }

    virtual void print(std::ostream &out) const;

    virtual void printSimp(std::ostream &out) const {
        out << "<Call__grid_orient270>";
    }

    virtual void accept(CallVisitor &v) {
        v.visit__grid_orient270(this);
    }
};

class Call__grid_orient315 : public virtual GridLibCall {
  public:
    Call__grid_orient315(SgFunctionCallExp *location)
        : GridLibCall(CALL__GRID_ORIENT315, location)
    { }

    virtual void print(std::ostream &out) const;

    virtual void printSimp(std::ostream &out) const {
        out << "<Call__grid_orient315>";
    }

    virtual void accept(CallVisitor &v) {
        v.visit__grid_orient315(this);
    }
};

class Call__grid_orient360 : public virtual GridLibCall {
  public:
    Call__grid_orient360(SgFunctionCallExp *location)
        : GridLibCall(CALL__GRID_ORIENT360, location)
    { }

    virtual void print(std::ostream &out) const;

    virtual void printSimp(std::ostream &out) const {
        out << "<Call__grid_orient360>";
    }

    virtual void accept(CallVisitor &v) {
        v.visit__grid_orient360(this);
    }
};

class Call__distribution_new : public virtual GridLibCall {
  private:
    std::string mLHS;
    std::string mEnvName;

  public:
    Call__distribution_new(SgFunctionCallExp *location,
        std::string lhs,
        std::string envName)
        :
          GridLibCall(CALL__DISTRIBUTION_NEW, location),
          mLHS(lhs),
          mEnvName(envName)
    { }

    virtual void print(std::ostream &out) const;

    virtual void printSimp(std::ostream &out) const {
        out << "<Call__distribution_new>";
    }

    virtual void accept(CallVisitor &v) {
        v.visit__distribution_new(this);
    }

    std::string lhs()     const { return mLHS;     }
    std::string envName() const { return mEnvName; }
};

class Call__distribution_applyFillBlock : public virtual GridLibCall {
  private:
    std::string mDist;
    std::string mGrid;
    std::string mBlockW;

  public:
    Call__distribution_applyFillBlock(SgFunctionCallExp *location,
        std::string dist,
        std::string grid,
        std::string blockW)
        :
          GridLibCall(CALL__DISTRIBUTION_APPLYFILLBLOCK, location),
          mDist(dist),
          mGrid(grid),
          mBlockW(blockW)
    { }

    virtual void print(std::ostream &out) const;

    virtual void printSimp(std::ostream &out) const {
        out << "<Call__distribution_applyFillBlock>";
    }

    virtual void accept(CallVisitor &v) {
        v.visit__distribution_applyFillBlock(this);
    }

    std::string dist()   const { return mDist;   }
    std::string grid()   const { return mGrid;   }
    std::string blockW() const { return mBlockW; }
};

class Call__distribution_applyBlockFill : public virtual GridLibCall {
  private:
    std::string mDist;
    std::string mGrid;
    std::string mBlockH;

  public:
    Call__distribution_applyBlockFill(SgFunctionCallExp *location,
        std::string dist,
        std::string grid,
        std::string blockH)
        :
          GridLibCall(CALL__DISTRIBUTION_APPLYBLOCKFILL, location),
          mDist(dist),
          mGrid(grid),
          mBlockH(blockH)
    { }

    virtual void print(std::ostream &out) const;

    virtual void printSimp(std::ostream &out) const {
        out << "<Call__distribution_applyBlockFill>";
    }

    virtual void accept(CallVisitor &v) {
        v.visit__distribution_applyBlockFill(this);
    }

    std::string dist()   const { return mDist;   }
    std::string grid()   const { return mGrid;   }
    std::string blockH() const { return mBlockH; }
};

class Call__distribution_applyBlockCyclic : public virtual GridLibCall {
  private:
    std::string mDist;
    std::string mGrid;
    std::string mBlkW;
    std::string mBlkH;

  public:
    Call__distribution_applyBlockCyclic(SgFunctionCallExp *location,
        std::string dist,
        std::string grid,
        std::string blkW,
        std::string blkH)
        :
          GridLibCall(CALL__DISTRIBUTION_APPLYBLOCKCYCLIC, location),
            mDist(dist),
            mGrid(grid),
            mBlkW(blkW),
            mBlkH(blkH)
    { }

    virtual void print(std::ostream &out) const;

    virtual void printSimp(std::ostream &out) const {
        out << "<Call__distribution_applyBlockCyclic>";
    }

    virtual void accept(CallVisitor &v) {
        v.visit__distribution_applyBlockCyclic(this);
    }

    std::string dist() const { return mDist; }
    std::string grid() const { return mGrid; }
    std::string blkW() const { return mBlkW; }
    std::string blkH() const { return mBlkH; }
};

class Call__distribution_visualize : public virtual GridLibCall {
  public:
    Call__distribution_visualize(SgFunctionCallExp *location)
        : GridLibCall(CALL__DISTRIBUTION_VISUALIZE, location)
    { }

    virtual void print(std::ostream &out) const;

    virtual void printSimp(std::ostream &out) const {
        out << "<Call__distribution_visualize>";
    }

    virtual void accept(CallVisitor &v) {
        v.visit__distribution_visualize(this);
    }
};

class Call__environment_print : public virtual GridLibCall {
  public:
    Call__environment_print(SgFunctionCallExp *location)
        : GridLibCall(CALL__ENVIRONMENT_PRINT, location)
    { }

    virtual void print(std::ostream &out) const;

    virtual void printSimp(std::ostream &out) const {
        out << "<Call__environment_print>";
    }

    virtual void accept(CallVisitor &v) {
        v.visit__environment_print(this);
    }
};

class Call__environment_output : public virtual GridLibCall {
  public:
    Call__environment_output(SgFunctionCallExp *location)
        : GridLibCall(CALL__ENVIRONMENT_OUTPUT, location)
    { }

    virtual void print(std::ostream &out) const;

    virtual void printSimp(std::ostream &out) const {
        out << "<Call__environment_output>";
    }

    virtual void accept(CallVisitor &v) {
        v.visit__environment_output(this);
    }
};

class Call__environment_input : public virtual GridLibCall {
  public:
    Call__environment_input(SgFunctionCallExp *location)
        : GridLibCall(CALL__ENVIRONMENT_INPUT, location)
    { }

    virtual void print(std::ostream &out) const;

    virtual void printSimp(std::ostream &out) const {
        out << "<Call__environment_input>";
    }

    virtual void accept(CallVisitor &v) {
        v.visit__environment_input(this);
    }
};

class Call__environment_clear : public virtual GridLibCall {
  public:
    Call__environment_clear(SgFunctionCallExp *location)
        : GridLibCall(CALL__ENVIRONMENT_CLEAR, location)
    { }

    virtual void print(std::ostream &out) const;

    virtual void printSimp(std::ostream &out) const {
        out << "<Call__environment_clear>";
    }

    virtual void accept(CallVisitor &v) {
        v.visit__environment_clear(this);
    }
};

class Call__schedule_new : public virtual GridLibCall {
  private:
    std::string mLHS;
    std::string mEnvName;

  public:
    Call__schedule_new(SgFunctionCallExp *location,
        std::string lhs,
        std::string envName)
        :
          GridLibCall(CALL__SCHEDULE_NEW, location),
          mLHS(lhs),
          mEnvName(envName)
    { }

    virtual void print(std::ostream &out) const;

    virtual void printSimp(std::ostream &out) const {
        out << "<Call__schedule_new>";
    }

    virtual void accept(CallVisitor &v) {
        v.visit__schedule_new(this);
    }

    std::string lhs()     const { return mLHS;     }
    std::string envName() const { return mEnvName; }
};

class Call__schedule_calculate : public virtual GridLibCall {
  private:
    std::string mSched;
    std::string mGrid;
    std::string mDist;

  public:
    //return new Call__schedule_calculate(node, sched, grid, dist);

    Call__schedule_calculate(SgFunctionCallExp *location,
        std::string sched,
        std::string grid,
        std::string dist)
        :
          GridLibCall(CALL__SCHEDULE_CALCULATE, location),
        mSched(sched),
        mGrid(grid),
        mDist(dist)
    { }

    virtual void print(std::ostream &out) const;

    virtual void printSimp(std::ostream &out) const {
        out << "<Call__schedule_calculate>";
    }

    virtual void accept(CallVisitor &v) {
        v.visit__schedule_calculate(this);
    }

    std::string sched() const { return mSched; }
    std::string grid()  const { return mGrid;  }
    std::string dist()  const { return mDist;  }
};

class Call__schedule_transferToFortran : public virtual GridLibCall {
  private:
    std::string mSched;

  public:
    Call__schedule_transferToFortran(SgFunctionCallExp *location,
        std::string sched)
        :
          GridLibCall(CALL__SCHEDULE_TRANSFERTOFORTRAN, location),
          mSched(sched)
    { }

    virtual void print(std::ostream &out) const;

    virtual void printSimp(std::ostream &out) const {
        out << "<Call__schedule_transferToFortran>";
    }

    virtual void accept(CallVisitor &v) {
        v.visit__schedule_transferToFortran(this);
    }

    std::string sched() const { return mSched; }
};

class Call__schedule_printFortranVersion : public virtual GridLibCall {
  public:
    Call__schedule_printFortranVersion(SgFunctionCallExp *location)
        : GridLibCall(CALL__SCHEDULE_PRINTFORTRANVERSION, location)
    { }

    virtual void print(std::ostream &out) const;

    virtual void printSimp(std::ostream &out) const {
        out << "<Call__schedule_printFortranVersion>";
    }

    virtual void accept(CallVisitor &v) {
        v.visit__schedule_printFortranVersion(this);
    }
};

class Call__schedule_sendRegionSize : public virtual GridLibCall {
  public:
    Call__schedule_sendRegionSize(SgFunctionCallExp *location)
        : GridLibCall(CALL__SCHEDULE_SENDREGIONSIZE, location)
    { }

    virtual void print(std::ostream &out) const;

    virtual void printSimp(std::ostream &out) const {
        out << "<Call__schedule_sendRegionSize>";
    }

    virtual void accept(CallVisitor &v) {
        v.visit__schedule_sendRegionSize(this);
    }
};

class Call__schedule_recvRegionSize : public virtual GridLibCall {
  public:
    Call__schedule_recvRegionSize(SgFunctionCallExp *location)
        : GridLibCall(CALL__SCHEDULE_RECVREGIONSIZE, location)
    { }

    virtual void print(std::ostream &out) const;

    virtual void printSimp(std::ostream &out) const {
        out << "<Call__schedule_recvRegionSize>";
    }

    virtual void accept(CallVisitor &v) {
        v.visit__schedule_recvRegionSize(this);
    }
};

class Call__data_new : public virtual GridLibCall {
  private:
    std::string mLHS;
    std::string mEnvName;

  public:
    Call__data_new(SgFunctionCallExp *location,
        std::string lhs,
        std::string envName)
        :
          GridLibCall(CALL__DATA_NEW, location),
          mLHS(lhs),
          mEnvName(envName)
    { }

    virtual void print(std::ostream &out) const;

    virtual void printSimp(std::ostream &out) const {
        out << "<Call__data_new>";
    }

    virtual void accept(CallVisitor &v) {
        v.visit__data_new(this);
    }

    std::string lhs() const     { return mLHS; }
    std::string envName() const { return mEnvName; }
};

class Call__data_print : public virtual GridLibCall {
  private:
    std::string mData;
    std::string mFileID;

  public:
    Call__data_print(SgFunctionCallExp *location,
        std::string sched, std::string fileID)
        :
          GridLibCall(CALL__DATA_PRINT, location),
          mData(sched),
          mFileID(fileID)
    { }

    virtual void print(std::ostream &out) const;

    virtual void printSimp(std::ostream &out) const {
        out << "<Call__data_print>";
    }

    virtual void accept(CallVisitor &v) {
        v.visit__data_print(this);
    }

    std::string data()  const { return mData;  }
    std::string fileID() const { return mFileID; }
};

class Call__data_printForProcs : public virtual GridLibCall {
  private:
    std::string mDataObj;
    std::string mFileID;

  public:
    Call__data_printForProcs(SgFunctionCallExp *location,
        std::string dataObj, std::string fileID)
        :
          GridLibCall(CALL__DATA_PRINTFORPROCS, location),
          mDataObj(dataObj),
          mFileID(fileID)
    { }

    virtual void print(std::ostream &out) const;

    virtual void printSimp(std::ostream &out) const {
        out << "<Call__data_printForProcs>";
    }

    virtual void accept(CallVisitor &v) {
        v.visit__data_printForProcs(this);
    }

    std::string dataObj() const { return mDataObj; }
    std::string fileID()  const { return mFileID;  }
};

class Call__data_printForProc : public virtual GridLibCall {
  public:
    Call__data_printForProc(SgFunctionCallExp *location)
        : GridLibCall(CALL__DATA_PRINTFORPROC, location)
    { }

    virtual void print(std::ostream &out) const;

    virtual void printSimp(std::ostream &out) const {
        out << "<Call__data_printForProc>";
    }

    virtual void accept(CallVisitor &v) {
        v.visit__data_printForProc(this);
    }
};

class Call__data_apply1 : public virtual GridLibCall {
  private:
    std::string mDataOut;
    std::string mDataIn1;
    std::string mStencilFunc;
    SgFunctionDefinition *mStencilFuncDef;

  public:
    Call__data_apply1(SgFunctionCallExp *location,
        std::string dataOut,
        std::string dataIn1,
        std::string stencilFunc,
        SgFunctionDefinition *stencilFuncDef)
        :
          GridLibCall(CALL__DATA_APPLY1, location),
          mDataOut(dataOut),
          mDataIn1(dataIn1),
          mStencilFunc(stencilFunc),
          mStencilFuncDef(stencilFuncDef)
    { }

    virtual void print(std::ostream &out) const;

    virtual void printSimp(std::ostream &out) const {
        out << "<Call__data_apply1>";
    }

    virtual void accept(CallVisitor &v) {
        v.visit__data_apply1(this);
    }

    std::string dataOut()     const { return mDataOut;     }
    std::string dataIn1()     const { return mDataIn1;     }
    std::string stencilFunc() const { return mStencilFunc; }
    SgFunctionDefinition* stencilFuncDef() const { return mStencilFuncDef; }
};

class Call__data_apply2 : public virtual GridLibCall {
  private:
    std::string mDataOut;
    std::string mDataIn1;
    std::string mDataIn2;
    std::string mStencilFunc;
    SgFunctionDefinition *mStencilFuncDef;

  public:
    Call__data_apply2(SgFunctionCallExp *location,
        std::string dataOut,
        std::string dataIn1,
        std::string dataIn2,
        std::string stencilFunc,
        SgFunctionDefinition *stencilFuncDef)
        :
          GridLibCall(CALL__DATA_APPLY2, location),
          mDataOut(dataOut),
          mDataIn1(dataIn1),
          mDataIn2(dataIn2),
          mStencilFunc(stencilFunc),
          mStencilFuncDef(stencilFuncDef)
    { }

    virtual void print(std::ostream &out) const;

    virtual void printSimp(std::ostream &out) const {
        out << "<Call__data_apply2>";
    }

    virtual void accept(CallVisitor &v) {
        v.visit__data_apply2(this);

    }
    std::string dataOut()     const { return mDataOut;     }
    std::string dataIn1()     const { return mDataIn1;     }
    std::string dataIn2()     const { return mDataIn2;     }
    std::string stencilFunc() const { return mStencilFunc; }
    SgFunctionDefinition* stencilFuncDef() const { return mStencilFuncDef; }
};

class Call__data_apply3 : public virtual GridLibCall {
  private:
    std::string mDataOut;
    std::string mDataIn1;
    std::string mDataIn2;
    std::string mDataIn3;
    std::string mStencilFunc;
    SgFunctionDefinition *mStencilFuncDef;

  public:
    Call__data_apply3(SgFunctionCallExp *location,
        std::string dataOut,
        std::string dataIn1,
        std::string dataIn2,
        std::string dataIn3,
        std::string stencilFunc,
        SgFunctionDefinition *stencilFuncDef)
        :
          GridLibCall(CALL__DATA_APPLY3, location),
          mDataOut(dataOut),
          mDataIn1(dataIn1),
          mDataIn2(dataIn2),
          mDataIn3(dataIn3),
          mStencilFunc(stencilFunc),
          mStencilFuncDef(stencilFuncDef)
    { }

    virtual void print(std::ostream &out) const;

    virtual void printSimp(std::ostream &out) const {
        out << "<Call__data_apply3>";
    }

    virtual void accept(CallVisitor &v) {
        v.visit__data_apply3(this);
    }

    std::string dataOut()     const { return mDataOut;     }
    std::string dataIn1()     const { return mDataIn1;     }
    std::string dataIn2()     const { return mDataIn2;     }
    std::string dataIn3()     const { return mDataIn3;     }
    std::string stencilFunc() const { return mStencilFunc; }
    SgFunctionDefinition* stencilFuncDef() const { return mStencilFuncDef; }
};

class Call__data_apply4 : public virtual GridLibCall {
  private:
    std::string mDataOut;
    std::string mDataIn1;
    std::string mDataIn2;
    std::string mDataIn3;
    std::string mDataIn4;
    std::string mStencilFunc;
    SgFunctionDefinition *mStencilFuncDef;

  public:
    Call__data_apply4(SgFunctionCallExp *location,
        std::string dataOut,
        std::string dataIn1,
        std::string dataIn2,
        std::string dataIn3,
        std::string dataIn4,
        std::string stencilFunc,
        SgFunctionDefinition *stencilFuncDef)
        :
          GridLibCall(CALL__DATA_APPLY4, location),
          mDataOut(dataOut),
          mDataIn1(dataIn1),
          mDataIn2(dataIn2),
          mDataIn3(dataIn3),
          mDataIn4(dataIn4),
          mStencilFunc(stencilFunc),
          mStencilFuncDef(stencilFuncDef)
    { }

    virtual void print(std::ostream &out) const;

    virtual void printSimp(std::ostream &out) const {
        out << "<Call__data_apply4>";
    }

    virtual void accept(CallVisitor &v) {
        v.visit__data_apply4(this);
    }

    std::string dataOut()     const { return mDataOut;     }
    std::string dataIn1()     const { return mDataIn1;     }
    std::string dataIn2()     const { return mDataIn2;     }
    std::string dataIn3()     const { return mDataIn3;     }
    std::string dataIn4()     const { return mDataIn4;     }
    std::string stencilFunc() const { return mStencilFunc; }
    SgFunctionDefinition* stencilFuncDef() const { return mStencilFuncDef; }
};

class Call__data_apply5 : public virtual GridLibCall {
  private:
    std::string mDataOut;
    std::string mDataIn1;
    std::string mDataIn2;
    std::string mDataIn3;
    std::string mDataIn4;
    std::string mDataIn5;
    std::string mStencilFunc;
    SgFunctionDefinition *mStencilFuncDef;

  public:
    Call__data_apply5(SgFunctionCallExp *location,
        std::string dataOut,
        std::string dataIn1,
        std::string dataIn2,
        std::string dataIn3,
        std::string dataIn4,
        std::string dataIn5,
        std::string stencilFunc,
        SgFunctionDefinition *stencilFuncDef)
        : GridLibCall(CALL__DATA_APPLY5, location),
          mDataOut(dataOut),
          mDataIn1(dataIn1),
          mDataIn2(dataIn2),
          mDataIn3(dataIn3),
          mDataIn4(dataIn4),
          mDataIn5(dataIn5),
          mStencilFunc(stencilFunc),
          mStencilFuncDef(stencilFuncDef)
    { }

    virtual void print(std::ostream &out) const;

    virtual void printSimp(std::ostream &out) const {
        out << "<Call__data_apply5>";
    }

    virtual void accept(CallVisitor &v) {
        v.visit__data_apply5(this);
    }

    std::string dataOut()     const { return mDataOut;     }
    std::string dataIn1()     const { return mDataIn1;     }
    std::string dataIn2()     const { return mDataIn2;     }
    std::string dataIn3()     const { return mDataIn3;     }
    std::string dataIn4()     const { return mDataIn4;     }
    std::string dataIn5()     const { return mDataIn5;     }
    std::string stencilFunc() const { return mStencilFunc; }
    SgFunctionDefinition* stencilFuncDef() const { return mStencilFuncDef; }
};

class Call__data_apply6 : public virtual GridLibCall {
  private:
    std::string mDataOut;
    std::string mDataIn1;
    std::string mDataIn2;
    std::string mDataIn3;
    std::string mDataIn4;
    std::string mDataIn5;
    std::string mDataIn6;
    std::string mStencilFunc;
    SgFunctionDefinition *mStencilFuncDef;

  public:
    Call__data_apply6(SgFunctionCallExp *location,
        std::string dataOut,
        std::string dataIn1,
        std::string dataIn2,
        std::string dataIn3,
        std::string dataIn4,
        std::string dataIn5,
        std::string dataIn6,
        std::string stencilFunc,
        SgFunctionDefinition *stencilFuncDef)
        :
          GridLibCall(CALL__DATA_APPLY6, location),
          mDataOut(dataOut),
          mDataIn1(dataIn1),
          mDataIn2(dataIn2),
          mDataIn3(dataIn3),
          mDataIn4(dataIn4),
          mDataIn5(dataIn5),
          mDataIn6(dataIn6),
          mStencilFunc(stencilFunc),
          mStencilFuncDef(stencilFuncDef)
    { }

    virtual void print(std::ostream &out) const;

    virtual void printSimp(std::ostream &out) const {
        out << "<Call__data_apply6>";
    }

    virtual void accept(CallVisitor &v) {
        v.visit__data_apply6(this);
    }

    std::string dataOut()     const { return mDataOut;     }
    std::string dataIn1()     const { return mDataIn1;     }
    std::string dataIn2()     const { return mDataIn2;     }
    std::string dataIn3()     const { return mDataIn3;     }
    std::string dataIn4()     const { return mDataIn4;     }
    std::string dataIn5()     const { return mDataIn5;     }
    std::string dataIn6()     const { return mDataIn6;     }
    std::string stencilFunc() const { return mStencilFunc; }
    SgFunctionDefinition* stencilFuncDef() const { return mStencilFuncDef; }
};

class Call__data_apply7 : public virtual GridLibCall {
  private:
    std::string mDataOut;
    std::string mDataIn1;
    std::string mDataIn2;
    std::string mDataIn3;
    std::string mDataIn4;
    std::string mDataIn5;
    std::string mDataIn6;
    std::string mDataIn7;
    std::string mStencilFunc;
    SgFunctionDefinition *mStencilFuncDef;

  public:
    Call__data_apply7(SgFunctionCallExp *location,
        std::string dataOut,
        std::string dataIn1,
        std::string dataIn2,
        std::string dataIn3,
        std::string dataIn4,
        std::string dataIn5,
        std::string dataIn6,
        std::string dataIn7,
        std::string stencilFunc,
        SgFunctionDefinition *stencilFuncDef)
        :
          GridLibCall(CALL__DATA_APPLY7, location),
          mDataOut(dataOut),
          mDataIn1(dataIn1),
          mDataIn2(dataIn2),
          mDataIn3(dataIn3),
          mDataIn4(dataIn4),
          mDataIn5(dataIn5),
          mDataIn6(dataIn6),
          mDataIn7(dataIn7),
          mStencilFunc(stencilFunc),
          mStencilFuncDef(stencilFuncDef)
    { }

    virtual void print(std::ostream &out) const;

    virtual void printSimp(std::ostream &out) const {
        out << "<Call__data_apply7>";
    }

    virtual void accept(CallVisitor &v) {
        v.visit__data_apply7(this);
    }

    std::string dataOut()     const { return mDataOut;     }
    std::string dataIn1()     const { return mDataIn1;     }
    std::string dataIn2()     const { return mDataIn2;     }
    std::string dataIn3()     const { return mDataIn3;     }
    std::string dataIn4()     const { return mDataIn4;     }
    std::string dataIn5()     const { return mDataIn5;     }
    std::string dataIn6()     const { return mDataIn6;     }
    std::string dataIn7()     const { return mDataIn7;     }
    std::string stencilFunc() const { return mStencilFunc; }
    SgFunctionDefinition* stencilFuncDef() const { return mStencilFuncDef; }
};

class Call__data_apply8 : public virtual GridLibCall {
  private:
    std::string mDataOut;
    std::string mDataIn1;
    std::string mDataIn2;
    std::string mDataIn3;
    std::string mDataIn4;
    std::string mDataIn5;
    std::string mDataIn6;
    std::string mDataIn7;
    std::string mDataIn8;
    std::string mStencilFunc;
    SgFunctionDefinition *mStencilFuncDef;

  public:
    Call__data_apply8(SgFunctionCallExp *location,
        std::string dataOut,
        std::string dataIn1,
        std::string dataIn2,
        std::string dataIn3,
        std::string dataIn4,
        std::string dataIn5,
        std::string dataIn6,
        std::string dataIn7,
        std::string dataIn8,
        std::string stencilFunc,
        SgFunctionDefinition *stencilFuncDef)
        :
          GridLibCall(CALL__DATA_APPLY8, location),
          mDataOut(dataOut),
          mDataIn1(dataIn1),
          mDataIn2(dataIn2),
          mDataIn3(dataIn3),
          mDataIn4(dataIn4),
          mDataIn5(dataIn5),
          mDataIn6(dataIn6),
          mDataIn7(dataIn7),
          mDataIn8(dataIn8),
          mStencilFunc(stencilFunc),
          mStencilFuncDef(stencilFuncDef)
    { }

    virtual void print(std::ostream &out) const;

    virtual void printSimp(std::ostream &out) const {
        out << "<Call__data_apply8>";
    }

    virtual void accept(CallVisitor &v) {
        v.visit__data_apply8(this);
    }
    std::string dataOut()     const { return mDataOut;     }
    std::string dataIn1()     const { return mDataIn1;     }
    std::string dataIn2()     const { return mDataIn2;     }
    std::string dataIn3()     const { return mDataIn3;     }
    std::string dataIn4()     const { return mDataIn4;     }
    std::string dataIn5()     const { return mDataIn5;     }
    std::string dataIn6()     const { return mDataIn6;     }
    std::string dataIn7()     const { return mDataIn7;     }
    std::string dataIn8()     const { return mDataIn8;     }
    std::string stencilFunc() const { return mStencilFunc; }
    SgFunctionDefinition* stencilFuncDef() const { return mStencilFuncDef; }
};

class Call__data_apply9 : public virtual GridLibCall {
  private:
    std::string mDataOut;
    std::string mDataIn1;
    std::string mDataIn2;
    std::string mDataIn3;
    std::string mDataIn4;
    std::string mDataIn5;
    std::string mDataIn6;
    std::string mDataIn7;
    std::string mDataIn8;
    std::string mDataIn9;
    std::string mStencilFunc;
    SgFunctionDefinition *mStencilFuncDef;

  public:
    Call__data_apply9(SgFunctionCallExp *location,
        std::string dataOut,
        std::string dataIn1,
        std::string dataIn2,
        std::string dataIn3,
        std::string dataIn4,
        std::string dataIn5,
        std::string dataIn6,
        std::string dataIn7,
        std::string dataIn8,
        std::string dataIn9,
        std::string stencilFunc,
        SgFunctionDefinition *stencilFuncDef)
        :
          GridLibCall(CALL__DATA_APPLY9, location),
          mDataOut(dataOut),
          mDataIn1(dataIn1),
          mDataIn2(dataIn2),
          mDataIn3(dataIn3),
          mDataIn4(dataIn4),
          mDataIn5(dataIn5),
          mDataIn6(dataIn6),
          mDataIn7(dataIn7),
          mDataIn8(dataIn8),
          mDataIn9(dataIn9),
          mStencilFunc(stencilFunc),
          mStencilFuncDef(stencilFuncDef)
    { }

    virtual void print(std::ostream &out) const;

    virtual void printSimp(std::ostream &out) const {
        out << "<Call__data_apply9>";
    }

    virtual void accept(CallVisitor &v) {
        v.visit__data_apply9(this);
    }

    std::string dataOut()     const { return mDataOut;     }
    std::string dataIn1()     const { return mDataIn1;     }
    std::string dataIn2()     const { return mDataIn2;     }
    std::string dataIn3()     const { return mDataIn3;     }
    std::string dataIn4()     const { return mDataIn4;     }
    std::string dataIn5()     const { return mDataIn5;     }
    std::string dataIn6()     const { return mDataIn6;     }
    std::string dataIn7()     const { return mDataIn7;     }
    std::string dataIn8()     const { return mDataIn8;     }
    std::string dataIn9()     const { return mDataIn9;     }
    std::string stencilFunc() const { return mStencilFunc; }
    SgFunctionDefinition* stencilFuncDef() const { return mStencilFuncDef; }
};

class Call__data_apply10 : public virtual GridLibCall {
  private:
    std::string mDataOut;
    std::string mDataIn1;
    std::string mDataIn2;
    std::string mDataIn3;
    std::string mDataIn4;
    std::string mDataIn5;
    std::string mDataIn6;
    std::string mDataIn7;
    std::string mDataIn8;
    std::string mDataIn9;
    std::string mDataIn10;
    std::string mStencilFunc;
    SgFunctionDefinition *mStencilFuncDef;

  public:
    Call__data_apply10(SgFunctionCallExp *location,
        std::string dataOut,
        std::string dataIn1,
        std::string dataIn2,
        std::string dataIn3,
        std::string dataIn4,
        std::string dataIn5,
        std::string dataIn6,
        std::string dataIn7,
        std::string dataIn8,
        std::string dataIn9,
        std::string dataIn10,
        std::string stencilFunc,
        SgFunctionDefinition *stencilFuncDef)
        :
          GridLibCall(CALL__DATA_APPLY10, location),
          mDataOut(dataOut),
          mDataIn1(dataIn1),
          mDataIn2(dataIn2),
          mDataIn3(dataIn3),
          mDataIn4(dataIn4),
          mDataIn5(dataIn5),
          mDataIn6(dataIn6),
          mDataIn7(dataIn7),
          mDataIn8(dataIn8),
          mDataIn9(dataIn9),
          mDataIn10(dataIn10),
          mStencilFunc(stencilFunc),
          mStencilFuncDef(stencilFuncDef)
    { }

    virtual void print(std::ostream &out) const;

    virtual void printSimp(std::ostream &out) const {
        out << "<Call__data_apply10>";
    }

    virtual void accept(CallVisitor &v) {
        v.visit__data_apply10(this);
    }

    std::string dataOut()     const { return mDataOut;     }
    std::string dataIn1()     const { return mDataIn1;     }
    std::string dataIn2()     const { return mDataIn2;     }
    std::string dataIn3()     const { return mDataIn3;     }
    std::string dataIn4()     const { return mDataIn4;     }
    std::string dataIn5()     const { return mDataIn5;     }
    std::string dataIn6()     const { return mDataIn6;     }
    std::string dataIn7()     const { return mDataIn7;     }
    std::string dataIn8()     const { return mDataIn8;     }
    std::string dataIn9()     const { return mDataIn9;     }
    std::string dataIn10()    const { return mDataIn10;    }
    std::string stencilFunc() const { return mStencilFunc; }
    SgFunctionDefinition* stencilFuncDef() const { return mStencilFuncDef; }
};

class Call__data_forceUpdate : public virtual GridLibCall {
  public:
    Call__data_forceUpdate(SgFunctionCallExp *location)
        : GridLibCall(CALL__DATA_FORCEUPDATE, location)
    { }

    virtual void print(std::ostream &out) const;

    virtual void printSimp(std::ostream &out) const {
        out << "<Call__data_forceUpdate>";
    }

    virtual void accept(CallVisitor &v) {
        v.visit__data_forceUpdate(this);
    }
};

#endif
