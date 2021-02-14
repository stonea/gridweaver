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
    ENVIRONMENT_STRING = 0,
    ENVIRONMENT_FILE,
    ENVIRONMENT_NEIGHBOR,
    ENVIRONMENT_SUB_GRID,
    ENVIRONMENT_GRID,
    ENVIRONMENT_DISTRIBUTION,
    ENVIRONMENT_DATA_OBJECT,
    ENVIRONMENT_SCHEDULE,

    ENVIRONMENT_UNTYPED
};

/** Types of GridLIB calls that need to be addressed by the code generator */
enum GridLibCallCode {
    CALL__STRING_NEW = 0,
    CALL__STRING_INIT,

    CALL__FILE_OPEN,

    CALL__SUBGRID_PRINT,
    CALL__SUBGRID_PRINTSIMP,
    CALL__SUBGRIDPTRVEC_PRINT,

    CALL__GRID_PRINT,
    CALL__GRID_PRINTSIMP,

    CALL__DISTRIBUTION_PRINT,
    CALL__DISTRIBUTION_PRINTSIMP,

    CALL__ENVIRONMENT_INPUT,

    CALL__SCHEDULE_NEWFROMFILE,
    CALL__SCHEDULE_INPUT,
    CALL__SCHEDULE_PRINT,
    CALL__SCHEDULE_PRINTSIMP,
  
    CALL__DATA_NEW,
    CALL__DATA_NEWMETADATACOPIED,
    CALL__DATA_PRINT,
    CALL__DATA_PRINTSIMP,
    CALL__DATA_PRINTFORPROC,

    CALL__DATA_INPUT,
    CALL__DATA_FORCEUPDATE,

    CALL__DATA_APPLY,
    CALL__DATA_ADD,
    CALL__DATA_SUB,
    CALL__DATA_MULT,
    CALL__DATA_POW,
    CALL__DATA_SUM,

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
class Call__string_new;
class Call__string_init;
class Call__file_open;
class Call__subgrid_print;
class Call__subgrid_printSimp;
class Call__subgridptrvec_print;
class Call__grid_print;
class Call__grid_printSimp;
class Call__distribution_print;
class Call__distribution_printSimp;
class Call__environment_input;
class Call__schedule_newFromFile;
class Call__schedule_input;
class Call__schedule_print;
class Call__schedule_printSimp;
class Call__data_new;
class Call__data_newMetaDataCopied;
class Call__data_print;
class Call__data_printSimp;
class Call__data_printForProc;
class Call__data_input;
class Call__data_forceUpdate;
class Call__data_apply;
class Call__data_add;
class Call__data_sub;
class Call__data_mult;
class Call__data_pow;
class Call__data_sum;

/**
 * Abstract visitor to for visiting calls in a list.
 **/
class CallVisitor {
  public:
    virtual void accept(const CallsVectorT &v);


    virtual void visit__default(GridLibCall *call) { }

    virtual void visit__string_new(Call__string_new *call);

    virtual void visit__string_init(Call__string_init *call);
    virtual void visit__file_open(Call__file_open *call);
    virtual void visit__subgrid_print(Call__subgrid_print *call);
    virtual void visit__subgrid_printSimp(Call__subgrid_printSimp *call);
    virtual void visit__subgridptrvec_print(Call__subgridptrvec_print *call);
    virtual void visit__grid_print(Call__grid_print *call);
    virtual void visit__grid_printSimp(Call__grid_printSimp *call);
    virtual void visit__environment_input(Call__environment_input *call);
    virtual void visit__distribution_print(Call__distribution_print *call);
    virtual void visit__distribution_printSimp(Call__distribution_printSimp *call);
    virtual void visit__schedule_newFromFile(Call__schedule_newFromFile *call);
    virtual void visit__schedule_input(Call__schedule_input *call);
    virtual void visit__schedule_print(Call__schedule_print *call);
    virtual void visit__schedule_printSimp(Call__schedule_printSimp *call);
    virtual void visit__data_new(Call__data_new *call);
    virtual void visit__data_newMetaDataCopied(Call__data_newMetaDataCopied *call);
    virtual void visit__data_print(Call__data_print *call);
    virtual void visit__data_printSimp(Call__data_printSimp *call);
    virtual void visit__data_printForProc(Call__data_printForProc *call);
    virtual void visit__data_input(Call__data_input *call);
    virtual void visit__data_forceUpdate(Call__data_forceUpdate *call);
    virtual void visit__data_apply(Call__data_apply *call);
    virtual void visit__data_add(Call__data_add *call);
    virtual void visit__data_sub(Call__data_sub *call);
    virtual void visit__data_mult(Call__data_mult *call);
    virtual void visit__data_pow(Call__data_pow *call);
    virtual void visit__data_sum(Call__data_sum *call);
};


/* Reference classes */
class EnvRef {
  public:
    EnvRef() {}
    EnvRef(SgNode *node);

    void setNode(SgNode *node);
    std::string ident() { return mIdent; }

    std::string  *isString();
    std::string  *isFile();
    Neighbor  *isNeighbor();
    SubGrid  *isSubGrid();
    Grid  *isGrid();
    Distribution  *isDistribution();
    DataObject  *isDataObject();
    Schedule *isSchedule();

  private:
    enum REFERENCE_TYPE {
        REFERENCE_TYPE_DIRECT,
        REFERENCE_TYPE_VIA_ENV
    };

    REFERENCE_TYPE mRefType;
    ENVIRONMENT_TYPE_CODE  mObjType;
    string mIdent;
    SgNode  *mNode;
};

/* Call construction functions */
GridLibCall* constructCall__string_new(SgFunctionCallExp *node);
GridLibCall* constructCall__string_init(SgFunctionCallExp *node);

GridLibCall* constructCall__file_open(SgFunctionCallExp *node);

GridLibCall* constructCall__subgrid_print(SgFunctionCallExp *node);
GridLibCall* constructCall__subgrid_printSimp(SgFunctionCallExp *node);
GridLibCall* constructCall__subgridptrvec_print(SgFunctionCallExp *node);

GridLibCall* constructCall__grid_print(SgFunctionCallExp *node);
GridLibCall* constructCall__grid_printSimp(SgFunctionCallExp *node);

GridLibCall* constructCall__distribution_print(SgFunctionCallExp *node);
GridLibCall* constructCall__distribution_printSimp(SgFunctionCallExp *node);

GridLibCall* constructCall__environment_input(SgFunctionCallExp *node);

GridLibCall* constructCall__schedule_newFromFile(SgFunctionCallExp *node);
GridLibCall* constructCall__schedule_input(SgFunctionCallExp *node);
GridLibCall* constructCall__schedule_print(SgFunctionCallExp *node);
GridLibCall* constructCall__schedule_printSimp(SgFunctionCallExp *node);

GridLibCall* constructCall__data_new(SgFunctionCallExp *node);
GridLibCall* constructCall__data_newMetaDataCopied(SgFunctionCallExp *node);
GridLibCall* constructCall__data_print(SgFunctionCallExp *node);
GridLibCall* constructCall__data_printSimp(SgFunctionCallExp *node);
GridLibCall* constructCall__data_printForProc(SgFunctionCallExp *node);

GridLibCall* constructCall__data_input(SgFunctionCallExp *node);
GridLibCall* constructCall__data_forceUpdate(SgFunctionCallExp *node);
GridLibCall* constructCall__data_apply(SgFunctionCallExp *node);
GridLibCall* constructCall__data_add(SgFunctionCallExp *node);
GridLibCall* constructCall__data_sub(SgFunctionCallExp *node);
GridLibCall* constructCall__data_mult(SgFunctionCallExp *node);
GridLibCall* constructCall__data_pow(SgFunctionCallExp *node);
GridLibCall* constructCall__data_sum(SgFunctionCallExp *node);

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


class Call__string_new : public virtual GridLibCall {
  public:
    Call__string_new(SgFunctionCallExp *location)
        : GridLibCall(CALL__STRING_NEW, location)
    { }

    virtual void print(std::ostream &out) const {
    }

    virtual void printSimp(std::ostream &out) const {
        out << "<Call__string_new>";
    }

    virtual void accept(CallVisitor &v) {
        v.visit__string_new(this);
    }
};


class Call__string_init : public virtual GridLibCall {
  public:
    Call__string_init(SgFunctionCallExp *location,
                      std::string ident, std::string value)
        : GridLibCall(CALL__STRING_INIT, location),
          mIdent(ident),
          mValue(value)
    { }

    virtual void print(std::ostream &out) const {
    }

    virtual void printSimp(std::ostream &out) const {
        out << "<Call__string_init>";
    }

    virtual void accept(CallVisitor &v) {
        v.visit__string_init(this);
    }

    const std::string &ident() { return mIdent; }
    const std::string &value() { return mValue; }

  private:
    std::string mIdent;
    std::string mValue;
};


class Call__file_open : public virtual GridLibCall {
  public:
    Call__file_open(SgFunctionCallExp *location,
                    std::string ident, std::string value)
        : GridLibCall(CALL__FILE_OPEN, location),
          mIdent(ident),
          mFilename(value)
    { }

    virtual void print(std::ostream &out) const {
    }

    virtual void printSimp(std::ostream &out) const {
        out << "<Call__file_open>";
    }

    virtual void accept(CallVisitor &v) {
        v.visit__file_open(this);
    }

    const std::string &ident() { return mIdent; }
    const std::string &filename() { return mFilename; }

  private:
    std::string mIdent;
    std::string mFilename;
};


class Call__subgrid_print : public virtual GridLibCall {
  public:
    Call__subgrid_print(SgFunctionCallExp *location)
        : GridLibCall(CALL__SUBGRID_PRINT, location)
    { }

    virtual void print(std::ostream &out) const {
    }

    virtual void printSimp(std::ostream &out) const {
        out << "<Call__subgrid_print>";
    }

    virtual void accept(CallVisitor &v) {
        v.visit__subgrid_print(this);
    }
};


class Call__subgrid_printSimp : public virtual GridLibCall {
  public:
    Call__subgrid_printSimp(SgFunctionCallExp *location)
        : GridLibCall(CALL__SUBGRID_PRINTSIMP, location)
    { }

    virtual void print(std::ostream &out) const {
    }

    virtual void printSimp(std::ostream &out) const {
        out << "<Call__subgrid_printSimp>";
    }

    virtual void accept(CallVisitor &v) {
        v.visit__subgrid_printSimp(this);
    }
};


class Call__subgridptrvec_print : public virtual GridLibCall {
  public:
    Call__subgridptrvec_print(SgFunctionCallExp *location)
        : GridLibCall(CALL__SUBGRIDPTRVEC_PRINT, location)
    { }

    virtual void print(std::ostream &out) const {
    }

    virtual void printSimp(std::ostream &out) const {
        out << "<Call__subgridptrvec_print>";
    }

    virtual void accept(CallVisitor &v) {
        v.visit__subgridptrvec_print(this);
    }
};


class Call__grid_print : public virtual GridLibCall {
  public:
    Call__grid_print(SgFunctionCallExp *location)
        : GridLibCall(CALL__GRID_PRINT, location)
    { }

    virtual void print(std::ostream &out) const {
    }

    virtual void printSimp(std::ostream &out) const {
        out << "<Call__grid_print>";
    }

    virtual void accept(CallVisitor &v) {
        v.visit__grid_print(this);
    }
};


class Call__grid_printSimp : public virtual GridLibCall {
  public:
    Call__grid_printSimp(SgFunctionCallExp *location)
        : GridLibCall(CALL__GRID_PRINTSIMP, location)
    { }

    virtual void print(std::ostream &out) const {
    }

    virtual void printSimp(std::ostream &out) const {
        out << "<Call__grid_printSimp>";
    }

    virtual void accept(CallVisitor &v) {
        v.visit__grid_printSimp(this);
    }
};


class Call__distribution_print : public virtual GridLibCall {
  public:
    Call__distribution_print(SgFunctionCallExp *location)
        : GridLibCall(CALL__DISTRIBUTION_PRINT, location)
    { }

    virtual void print(std::ostream &out) const {
    }

    virtual void printSimp(std::ostream &out) const {
        out << "<Call__distribution_print>";
    }

    virtual void accept(CallVisitor &v) {
        v.visit__distribution_print(this);
    }
};


class Call__distribution_printSimp : public virtual GridLibCall {
  public:
    Call__distribution_printSimp(SgFunctionCallExp *location)
        : GridLibCall(CALL__DISTRIBUTION_PRINTSIMP, location)
    { }

    virtual void print(std::ostream &out) const {
    }

    virtual void printSimp(std::ostream &out) const {
        out << "<Call__distribution_printSimp>";
    }

    virtual void accept(CallVisitor &v) {
        v.visit__distribution_printSimp(this);
    }
};


class Call__environment_input : public virtual GridLibCall {
  public:
    Call__environment_input(SgFunctionCallExp *location,
                            std::string inputFile)
        : GridLibCall(CALL__ENVIRONMENT_INPUT, location),
          mInputFile(inputFile)
    { }

    virtual void print(std::ostream &out) const {
    }

    virtual void printSimp(std::ostream &out) const {
        out << "<Call__environment_input>";
    }

    virtual void accept(CallVisitor &v) {
        v.visit__environment_input(this);
    }

    virtual std::string inputFile() {
        return mInputFile;
    }

  private:
    std::string mInputFile;
};


class Call__schedule_newFromFile : public virtual GridLibCall {
  public:
    Call__schedule_newFromFile(SgFunctionCallExp *location)
        : GridLibCall(CALL__SCHEDULE_NEWFROMFILE, location)
    { }

    virtual void print(std::ostream &out) const {
    }

    virtual void printSimp(std::ostream &out) const {
        out << "<Call__schedule_newFromFile>";
    }

    virtual void accept(CallVisitor &v) {
        v.visit__schedule_newFromFile(this);
    }
};


class Call__schedule_input : public virtual GridLibCall {
  public:
    Call__schedule_input(SgFunctionCallExp *location)
        : GridLibCall(CALL__SCHEDULE_INPUT, location)
    { }

    virtual void print(std::ostream &out) const {
    }

    virtual void printSimp(std::ostream &out) const {
        out << "<Call__schedule_input>";
    }

    virtual void accept(CallVisitor &v) {
        v.visit__schedule_input(this);
    }
};


class Call__schedule_print : public virtual GridLibCall {
  public:
    Call__schedule_print(SgFunctionCallExp *location)
        : GridLibCall(CALL__SCHEDULE_PRINT, location)
    { }

    virtual void print(std::ostream &out) const {
    }

    virtual void printSimp(std::ostream &out) const {
        out << "<Call__schedule_print>";
    }

    virtual void accept(CallVisitor &v) {
        v.visit__schedule_print(this);
    }
};


class Call__schedule_printSimp : public virtual GridLibCall {
  public:
    Call__schedule_printSimp(SgFunctionCallExp *location)
        : GridLibCall(CALL__SCHEDULE_PRINTSIMP, location)
    { }

    virtual void print(std::ostream &out) const {
    }

    virtual void printSimp(std::ostream &out) const {
        out << "<Call__schedule_printSimp>";
    }

    virtual void accept(CallVisitor &v) {
        v.visit__schedule_printSimp(this);
    }
};


class Call__data_new : public virtual GridLibCall {
  public:
    Call__data_new(SgFunctionCallExp *location, std::string ident,
        EnvRef grid, EnvRef sched, EnvRef dist)
        :
        GridLibCall(CALL__DATA_NEW, location),
        mIdent(ident), mGrid(grid), mSched(sched), mDist(dist)
    { }


    virtual void print(std::ostream &out) const {
    }

    virtual void printSimp(std::ostream &out) const {
        out << "<Call__data_new>";
    }

    virtual void accept(CallVisitor &v) {
        v.visit__data_new(this);
    }

    virtual std::string ident() { return mIdent; }
    virtual EnvRef grid() { return mGrid; }
    virtual EnvRef sched() { return mSched; }
    virtual EnvRef dist() { return mDist; }

  private:
    std::string mIdent;
    EnvRef mGrid;
    EnvRef mSched;
    EnvRef mDist;
};


class Call__data_newMetaDataCopied : public virtual GridLibCall {
  public:
    Call__data_newMetaDataCopied(SgFunctionCallExp *location)
        : GridLibCall(CALL__DATA_NEWMETADATACOPIED, location)
    { }

    virtual void print(std::ostream &out) const {
    }

    virtual void printSimp(std::ostream &out) const {
        out << "<Call__data_newMetaDataCopied>";
    }

    virtual void accept(CallVisitor &v) {
        v.visit__data_newMetaDataCopied(this);
    }
};


class Call__data_print : public virtual GridLibCall {
  public:
    Call__data_print(SgFunctionCallExp *location)
        : GridLibCall(CALL__DATA_PRINT, location)
    { }

    virtual void print(std::ostream &out) const {
    }

    virtual void printSimp(std::ostream &out) const {
        out << "<Call__data_print>";
    }

    virtual void accept(CallVisitor &v) {
        v.visit__data_print(this);
    }
};


class Call__data_printSimp : public virtual GridLibCall {
  public:
    Call__data_printSimp(SgFunctionCallExp *location)
        : GridLibCall(CALL__DATA_PRINTSIMP, location)
    { }

    virtual void print(std::ostream &out) const {
    }

    virtual void printSimp(std::ostream &out) const {
        out << "<Call__data_printSimp>";
    }

    virtual void accept(CallVisitor &v) {
        v.visit__data_printSimp(this);
    }
};


class Call__data_printForProc : public virtual GridLibCall {
  public:
    Call__data_printForProc(SgFunctionCallExp *location)
        : GridLibCall(CALL__DATA_PRINTFORPROC, location)
    { }

    virtual void print(std::ostream &out) const {
    }

    virtual void printSimp(std::ostream &out) const {
        out << "<Call__data_printForProc>";
    }

    virtual void accept(CallVisitor &v) {
        v.visit__data_printForProc(this);
    }
};


class Call__data_input : public virtual GridLibCall {
  public:
    Call__data_input(SgFunctionCallExp *location)
        : GridLibCall(CALL__DATA_INPUT, location)
    { }

    virtual void print(std::ostream &out) const {
    }

    virtual void printSimp(std::ostream &out) const {
        out << "<Call__data_input>";
    }

    virtual void accept(CallVisitor &v) {
        v.visit__data_input(this);
    }
};


class Call__data_forceUpdate : public virtual GridLibCall {
  public:
    Call__data_forceUpdate(SgFunctionCallExp *location)
        : GridLibCall(CALL__DATA_FORCEUPDATE, location)
    { }

    virtual void print(std::ostream &out) const {
    }

    virtual void printSimp(std::ostream &out) const {
        out << "<Call__data_forceUpdate>";
    }

    virtual void accept(CallVisitor &v) {
        v.visit__data_forceUpdate(this);
    }
};


class Call__data_apply : public virtual GridLibCall {
  public:
    Call__data_apply(SgFunctionCallExp *location,
                     EnvRef lhs,
                     EnvRef rhs,
                     SgFunctionRefExp *func)
        : GridLibCall(CALL__DATA_APPLY, location),
          mLHS(lhs),
          mRHS(rhs),
          mFunc(func)
    { }

    virtual void print(std::ostream &out) const {
    }

    virtual void printSimp(std::ostream &out) const {
        out << "<Call__data_apply>";
    }

    virtual void accept(CallVisitor &v) {
        v.visit__data_apply(this);
    }

    EnvRef lhs() { return mLHS; }
    EnvRef rhs() { return mRHS; }
    SgFunctionRefExp *func() { return mFunc; }
    SgNode *funcDef();

  private:
    EnvRef mLHS;
    EnvRef mRHS;
    SgFunctionRefExp *mFunc;
};


class Call__data_add : public virtual GridLibCall {
  public:
    Call__data_add(SgFunctionCallExp *location)
        : GridLibCall(CALL__DATA_ADD, location)
    { }

    virtual void print(std::ostream &out) const {
    }

    virtual void printSimp(std::ostream &out) const {
        out << "<Call__data_add>";
    }

    virtual void accept(CallVisitor &v) {
        v.visit__data_add(this);
    }
};


class Call__data_sub : public virtual GridLibCall {
  public:
    Call__data_sub(SgFunctionCallExp *location)
        : GridLibCall(CALL__DATA_SUB, location)
    { }

    virtual void print(std::ostream &out) const {
    }

    virtual void printSimp(std::ostream &out) const {
        out << "<Call__data_sub>";
    }

    virtual void accept(CallVisitor &v) {
        v.visit__data_sub(this);
    }
};


class Call__data_mult : public virtual GridLibCall {
  public:
    Call__data_mult(SgFunctionCallExp *location)
        : GridLibCall(CALL__DATA_MULT, location)
    { }

    virtual void print(std::ostream &out) const {
    }

    virtual void printSimp(std::ostream &out) const {
        out << "<Call__data_mult>";
    }

    virtual void accept(CallVisitor &v) {
        v.visit__data_mult(this);
    }
};


class Call__data_pow : public virtual GridLibCall {
  public:
    Call__data_pow(SgFunctionCallExp *location)
        : GridLibCall(CALL__DATA_POW, location)
    { }

    virtual void print(std::ostream &out) const {
    }

    virtual void printSimp(std::ostream &out) const {
        out << "<Call__data_pow>";
    }

    virtual void accept(CallVisitor &v) {
        v.visit__data_pow(this);
    }
};


class Call__data_sum : public virtual GridLibCall {
  public:
    Call__data_sum(SgFunctionCallExp *location)
        : GridLibCall(CALL__DATA_SUM, location)
    { }

    virtual void print(std::ostream &out) const {
    }

    virtual void printSimp(std::ostream &out) const {
        out << "<Call__data_sum>";
    }

    virtual void accept(CallVisitor &v) {
        v.visit__data_sum(this);
    }
};


/**
 * Returns an environment ID for an object referenced in an AST.  The
 * node might directly refer to the object or refer to it through a
 * call to environment_
 */
std::string getEnvironmentalIDFromSgNode(SgNode *node);

#endif
