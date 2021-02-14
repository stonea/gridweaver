/******************************************************************************
 * GRIDGEN: Grid Generating Compiler
 * By: Andy Stone (aistone@gmail.com)
 * (C) Copyright 2011 Colorado State University
 *****************************************************************************/
/** \addtogroup Utils
 *  @{
 */
#ifndef ERROR_HPP_
#define ERROR_HPP_

#include <string>

/**
 * Initialization function for the errors module.
 */
void initializeModule_error();

/**
 * Errorcodes are used to specify what type of error has occured.
 */
enum ERRCODE {
    ERR_NULL = 0,

    ERR_GENERIC__TODO,


    ERR_UTILS__UNABLE_TO_CREATE_DIR,

    ERR_ENV__DUPLICATE_ENTRY,

    ERR_SG__INVALID_SGID,

    ERR_GRID__UNKNOWN_SUBGRID,

    ERR_DIST__INVALID_SG,

    ERR_SCHEDULE__INVALID_RANK,

    ERR_VISPAGE__DUPLICATE_ENTRY,

    ERR_UNCLASSIFIED
};


/** This struct is for entries in the global table of error information */
struct Error {
    //Error(ERRCODE code, std::string sCode, std::string msg) :
    //    code(code), sCode(sCode), msg(msg) { }

    ERRCODE code;
    std::string sCode;
    std::string msg;
};

/** Tlobal table of error information */
static struct Error gErrors[] = {
    {ERR_GENERIC__TODO, "ERR_GENERIC__TODO", "A TODO item remains in the GridWeaver source code."},

    {ERR_UTILS__UNABLE_TO_CREATE_DIR, "ERR_UTILS__UNABLE_TO_CREATE_DIR", "Unable to create directory."},

    {ERR_ENV__DUPLICATE_ENTRY, "ERR_ENV__DUPLICATE_ENTRY", "Attempt to add object to environment with duplicated identifier"},

    {ERR_SG__INVALID_SGID, "ERR_SG__INVALID_SGID", "Invalid Subgrid ID"},

    {ERR_GRID__UNKNOWN_SUBGRID, "ERR_GRID__UNKNOWN_SUBGRID", "Unknown subgrid"},
    
    {ERR_DIST__INVALID_SG, "ERR_DIST__INVALID_SG", "Invalid Subgrid"},

    {ERR_SCHEDULE__INVALID_RANK, "ERR_SCHEDULE__INVALID_RANK", "Invalid MPI rank"},
    
    {ERR_VISPAGE__DUPLICATE_ENTRY, "ERR_VISPAGE__DUPLICATE_ENTRY", "Duplicate entry in visualization table."},

    // We have a terminator option so that when iterating through this array
    // we know when to stop.
    {ERR_NULL, "", ""}
};




/**
 * Print a back trace and the message for the specified error.
 */
void error(ERRCODE code);

/**
 * Print a back trace and the message for the specified error.
 * Also print an argument (which triggered the error).
 */
void error(ERRCODE code, std::string arg);

#endif
/** @}*/
