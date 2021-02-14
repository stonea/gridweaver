/******************************************************************************
 * GRIDGEN: Grid Generating Compiler
 * By: Andy Stone (aistone@gmail.com)
 * (C) Copyright 2011 Colorado State University
 *****************************************************************************/
/** \addtogroup Utils
 *  @{
 */

#ifndef UTILS_HPP
#define UTILS_HPP_

//#include <rose.h>
#include <string>
#include <ostream>
#include <sstream>
#include <set>
#include <map>
#include <math.h>
#include <iostream>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "iserializable.hpp"
#include "environment.hpp"
#include <mpi.h>

/**
 * Initialization function for the utils module.
 */
void initializeModule_utils();

// ===================
// - [MPI] -
// ===================
///@{
    /** Return the number of MPI ranks that are running. */
    int myRank();

    /** Return true if this process is the MPI master rank. */
    bool isMasterRank();

    /** Return the rank number for this process. */
    int numRanks();

    /** Return the rank of the MPI master rank. */
    int masterRank();
///@}

// ===================
// - [Debugging] -
// ===================
/** @name Debugging */
///@{
    /**
     * Debugging macro:
     *
     * This macro is taken the OpenAnalysis project's OA_DEBUG_CTRL_MACRO.
     *
     *
     * GW_DEBUG_CTRL(ModuleNameListDEBUG, DeBugDEBUG)
     *
     * The GridWeaver debug control macro retrieves a ':' colon-separated  list
     * from the environment variable GW_DEBUG and compares this list to the
     * list of ':' separate names passed to it in ModuleNameListDEBUG.  If it
     * finds an exact name match for any of the elements it sets the bool
     * variable true, otherwise it set it false.  This variable can then be
     * used to control printing of debug information.  
     *
     * To use, within the program code (must be executed), declare a bool
     * variable and call the macro:
     *
     *      bool debug;
     *      GW_DEBUG_CTRL("thismodname:groupmodname:all", debug);
     *
     * Where the text between the "" is one or more names, separated with ':' 
     * that will be compared to the environment variable returned with 
     * getenv("GW_DEBUG"). The parameter "debug" is the name of the bool
     * variable that will be set to true of false depending on if the
     * environment variable is matched. 
     *
     * The reason ModuleNameListDEBUG takes a list is so you can place 
     * a specific entry for this one module, and then add in zero or 
     * more group names, allowing you to turn on debugging for a group 
     * of modules by only setting a single environment variable
     *
     * When you want to output debugging information, use something like this 
     * within your code:
     * 
     * if(debug) {
     *   std::cout << "This is a debug message << std::endl ;
     * }
     *
     * To turn debugging on, under the bash shell use the command:
     *
     *  export GW_DEBUG=DEBUGFLAG1:DEBUGFLAG2:etc
     *
     * and then run your code.
     * 
     * To turn debugging off, issued the command:
     * 
     *  export GW_DEBUG=
    */
    #define GW_DEBUG_CTRL(ModuleNameListDEBUG, DeBugDEBUG)\
        {\
            DeBugDEBUG = false;\
            char *ept = getenv("GW_DEBUG");\
            if( ept != NULL ) {\
                while (*ept == ':') {++ept;}\
                char *ept1 = ept;\
                const char *mpt1;\
                const char *mpt;\
                mpt1 = ModuleNameListDEBUG;\
                while (*mpt1 == ':') {++mpt1;}\
                mpt = mpt1;\
                while(*mpt1 != '\0') {\
                    while(*ept != '\0'){\
                        if( *ept == *mpt ){\
                            *ept++;\
                            *mpt++;\
                            if ((*ept == '\0' || *ept == ':' ) && \
                              (*mpt == '\0' || *mpt == ':')) {\
                                DeBugDEBUG = true;\
                                break;\
                            }\
                        }\
                        else {\
                            mpt = mpt1;\
                            while ((*ept != ':') && (*ept != '\0')){++ept;}\
                            while (*ept == ':') {++ept;}\
                        }\
                    }\
                    if (DeBugDEBUG) {break;}\
                    while( (*mpt1 != ':') && (*mpt1 != '\0') ){++mpt1;}\
                    while (*mpt1 == ':') {++mpt1;}\
                    mpt = mpt1; ept = ept1;\
                }\
            }\
        }


    /**
     * If the variable debug is set to true print the message m to the console
     * with information about where it's being called.
     */
    #define DBG_MSG(M) \
        if(debug) { \
            cout << hiDbgFile(__FILE__) \
                 << hiDbgFile(":")      \
                 << hiDbgFile(str(__LINE__)) \
                 << "  "                \
                 << hiDbgMsg((M))       \
                 << endl;               \
        }

    /**
     * The following macros are for printing variables.
     */
    #define DBG_MSG_V(M, V1) \
        if(debug) { \
            cout << hiDbgFile(__FILE__) \
                 << hiDbgFile(":")      \
                 << hiDbgFile(str(__LINE__)) \
                 << "  "                \
                 << hiDbgMsg((M))       \
                 << "  "                \
                 << hiDbgFile((#V1))    \
                 << hiDbgFile(" = ")    \
                 << (V1)                \
                 << endl;               \
        }

    #define DBG_MSG_V2(M, V1, V2) \
        if(debug) { \
            cout << hiDbgFile(__FILE__) \
                 << hiDbgFile(":")      \
                 << hiDbgFile(str(__LINE__)) \
                 << "  "                \
                 << hiDbgMsg((M))       \
                 << "  "                \
                 << hiDbgFile((#V1))    \
                 << hiDbgFile(" = ")    \
                 << (V1)                \
                 << "  "                \
                 << hiDbgFile((#V1))    \
                 << hiDbgFile(" = ")    \
                 << V2                  \
                 << endl;               \
        }

    #define DBG_MSG_V3(M, V1, V2, V3)  \
        if(debug) { \
            cout << hiDbgFile(__FILE__) \
                 << hiDbgFile(":")      \
                 << hiDbgFile(str(__LINE__)) \
                 << "  "                \
                 << hiDbgMsg((M))       \
                 << "  "                \
                 << hiDbgFile((#V1))    \
                 << hiDbgFile(" = ")    \
                 << (V1)                \
                 << "  "                \
                 << hiDbgFile((#V2))    \
                 << hiDbgFile(" = ")    \
                 << V2                  \
                 << "  "                \
                 << hiDbgFile((#V3))    \
                 << hiDbgFile(" = ")    \
                 << V3                  \
                 << endl;               \
        }

    #define DBG_MSG_V4(M, V1, V2, V3, V4) \
        if(debug) { \
            cout << hiDbgFile(__FILE__) \
                 << hiDbgFile(":")      \
                 << hiDbgFile(str(__LINE__)) \
                 << "  "                \
                 << hiDbgMsg((M))       \
                 << "  "                \
                 << hiDbgFile((#V1))    \
                 << hiDbgFile(" = ")    \
                 << (V1)                \
                 << "  "                \
                 << hiDbgFile((#V2))    \
                 << hiDbgFile(" = ")    \
                 << (V2)                \
                 << "  "                \
                 << hiDbgFile((#V3))    \
                 << hiDbgFile(" = ")    \
                 << (V3)                \
                 << "  "                \
                 << hiDbgFile((#V4))    \
                 << hiDbgFile(" = ")    \
                 << (V4)                \
                 << endl;               \
        }

    #define DBG_MSG_V5(M, V1, V2, V3, V4, V5) \
        if(debug) { \
            cout << hiDbgFile(__FILE__) \
                 << hiDbgFile(":")      \
                 << hiDbgFile(str(__LINE__)) \
                 << "  "                \
                 << hiDbgMsg((M))       \
                 << "  "                \
                 << hiDbgFile((#V1))    \
                 << hiDbgFile(" = ")    \
                 << (V1)                \
                 << "  "                \
                 << hiDbgFile((#V2))    \
                 << hiDbgFile(" = ")    \
                 << (V2)                \
                 << "  "                \
                 << hiDbgFile((#V3))    \
                 << hiDbgFile(" = ")    \
                 << (V3)                \
                 << "  "                \
                 << hiDbgFile((#V4))    \
                 << hiDbgFile(" = ")    \
                 << (V4)                \
                 << "  "                \
                 << hiDbgFile((#V5))    \
                 << hiDbgFile(" = ")    \
                 << (V5)                \
                 << endl;               \
        }

///@}

// ===================
// - [Color codes] -
// ===================
/** @name Color codes */
///@{
    #define COLOR_DISABLE  "\e[0m"
    // regular colors
    #define COLOR_BLACK    "\e[0;30m"
    #define COLOR_RED      "\e[0;31m"
    #define COLOR_GREEN    "\e[0;32m"
    #define COLOR_YELLOW   "\e[0;33m"
    #define COLOR_BLUE     "\e[0;34m"
    #define COLOR_MAGENTA  "\e[0;35m"
    #define COLOR_CYAN     "\e[0;36m"
    #define COLOR_WHITE    "\e[0;37m"
    // emphasized (bolded) colors
    #define COLOR_EBLACK   "\e[1;30m"
    #define COLOR_ERED     "\e[1;31m"
    #define COLOR_EGREEN   "\e[1;32m"
    #define COLOR_EYELLOW  "\e[1;33m"
    #define COLOR_EBLUE    "\e[1;34m"
    #define COLOR_EMAGENTA "\e[1;35m"
    #define COLOR_ECYAN    "\e[1;36m"
    #define COLOR_EWHITE   "\e[1;37m"
    // underlined colors
    #define COLOR_UBLACK   "\e[4;30m"
    #define COLOR_URED     "\e[4;31m"
    #define COLOR_UGREEN   "\e[4;32m"
    #define COLOR_UYELLOW  "\e[4;33m"
    #define COLOR_UBLUE    "\e[4;34m"
    #define COLOR_UMAGENTA "\e[4;35m"
    #define COLOR_UCYAN    "\e[4;36m"
    #define COLOR_UWHITE   "\e[4;37m"
    // background colors
    #define COLOR_BBLACK   "\e[40m"
    #define COLOR_BRED     "\e[41m"
    #define COLOR_BGREEN   "\e[42m"
    #define COLOR_BYELLOW  "\e[43m"
    #define COLOR_BBLUE    "\e[44m"
    #define COLOR_BMAGENTA "\e[45m"
    #define COLOR_BCYAN    "\e[46m"
    #define COLOR_BWHITE   "\e[47m"
///@}

// =============================
// - [Simple math functions] -
// =============================
/** @name Simple math functions */
//@{
    /**
     * Round x down to the nearest multiple of m.
     */
    #define ROUND_DOWN_BLK(x, m) (floor(( (double) (x-1) / (m) )) ) * (m) + 1

    /**
     * Round x up to the nearest multiple of m.
     */
    #define ROUND_UP_BLK(x, m) (ceil(( (double) (x) / (m) )) ) * (m)

    /**
     * Return the smaller of x and y.
     */
    #define MIN(X,Y) ((X) < (Y) ? (X) : (Y))

    /**
     * Return the larger of x and y.
     */
    #define MAX(X,Y) ((X) > (Y) ? (X) : (Y))
///@}

// =============================
// - [File I/O functions] -
// =============================
/** @name File I/O functions */
//@{
    /** Create the specified directory if it does not already exist. */
    void createDirectory(const std::string &dirName);
///@}

// =============================
// - [SVG functions] -
// =============================
/** @name SVG functions */
///@{
    /**
     * Convert idx to a color.
     */
    std::string num2SVGcolor(int idx);
///@}

// =============================
// - [String functions] -
// =============================
/** @name String functions */
///@{
    /** Remove white space around a string */
    void trim(std::string& str);

    /** Convert an integer into a string */
    std::string str(int i);

    /** Print a SgNode and its children */
    //void printNodeAndChildren(std::ostream &os, SgNode *node);
///@}

// =============================
// - [Indentation functions] -
// =============================
/** @name Indentation functions */
///@{
    extern int gNestedObjects;
    extern int gIndentLevel;

    /**
     * Print indented space onto os.  This function is invoked by using
     * ostream's operator<<.  For example:  os << indt << "Hello";
     **/
    std::ostream &indt(std::ostream &os);

    /**
     * Add an additional layer of indentation. This function is invoked by
     * using ostream's operator<<.  For example: os << pushIndt;
     **/
    std::ostream &pushIndt(std::ostream &os);

    /**
     * Add an additional layer of indentation. This function is invoked by
     * using ostream's operator<<.  For example: os << popIndt;
     **/
    std::ostream &popIndt(std::ostream &os);
///@}

// ====================================
// - [Syntax Highlighting functions] -
// ====================================
/** @name Syntax Highlighting functions */
///@{
    /**
     * Turn syntax highlighting off.
     */
    void turnOffSyntaxHighlighting();

    /**
     * A manipulator for highlighting types when output to an ostream.
     */
    class hiType {
      public:
        hiType(std::string val) : val(val) {}
        std::string val;
    };
    std::ostream &operator<<(std::ostream &out, hiType s);

    /**
     * A manipulator for highlighting identifiers of environmental objects
     * when output to an ostream.
     */
    class hiIdent {
      public:
        hiIdent(std::string val) : val(val) {}
        std::string val;
    };
    std::ostream &operator<<(std::ostream &out, hiIdent s);

    /**
     * A manipulator for highlighting properties of environmental objects
     * when output to an ostream.
     */
    class hiProp {
      public:
        hiProp(std::string val) : val(val) {}
        std::string val;
    };
    std::ostream &operator<<(std::ostream &out, hiProp s);

    /**
     * A manipulator for highlighting values when output to an ostream.
     */
    class hiVal {
      public:
        hiVal(std::string val) : val(val) {}
        std::string val;
    };
    std::ostream &operator<<(std::ostream &out, hiVal s);

    /**
     * A manipulator for highlighting formatting characters (for example
     * brackets) when output to an ostream.
     */
    class hiFmt {
      public:
        hiFmt(std::string val) : val(val) {}
        std::string val;
    };
    std::ostream &operator<<(std::ostream &out, hiFmt s);

    /**
     * A manipulator for highlighting section headings when listing an object's
     * properties.
     */
    class hiHeading {
      public:
        hiHeading(std::string val) : val(val) {}
        std::string val;
    };
    std::ostream &operator<<(std::ostream &out, hiHeading s);

    /**
     * A manipulator for highlighting error messages.
     */
    class hiErr {
      public:
        hiErr(std::string val) : val(val) {}
        std::string val;
    };
    std::ostream &operator<<(std::ostream &out, hiErr s);

    /**
     * A manipulator for highlighting file information on debugging messages.
     */
    class hiDbgFile {
      public:
        hiDbgFile(std::string val) : val(val) {}
        std::string val;
    };
    std::ostream &operator<<(std::ostream &out, hiDbgFile s);

    /**
     * A manipulator for highlighting file information on debugging messages.
     */
    class hiDbgMsg {
      public:
        hiDbgMsg(std::string val) : val(val) {}
        std::string val;
    };
    std::ostream &operator<<(std::ostream &out, hiDbgMsg s);

///@}

// =============================
// - [Printing functions] -
// =============================
/** @name Printing functions */
///@{
    
    /**
     * Print "objType objName {" and push indentation.  This function is useful
     * for implementing print() methods of IPrintable objects.
     **/
    void printObj_start(std::ostream &out, const std::string &objType,
                        const std::string &objName);

    /**
     * Print "}" and push indentation.  This function is useful for
     * implementing print() methods of IPrintable objects.
     **/
    void printObj_end(std::ostream &out);

    /**
     * Print "property = value".  This function is useful for
     * implementing print() methods of IPrintable objects.
     **/
    void printObj_property(std::ostream &out, const std::string &property,
                           const std::string &value);
    void printObj_property(std::ostream &out, const std::string &property, int value);
    void printObj_property(std::ostream &out, const std::string &property, bool value);
    void printObj_property(std::ostream &out, const std::string &property,
                           const IPrintable &value);
    void printObj_property(std::ostream &out, const std::string &property,
                           const IPrintable *value);
    void printObj_property(std::ostream &out, const std::string &property);

    /**
     * Print "property = value" for the value on a specified MPI rank.
     */
    void printObj_propertyFromRank(std::ostream &out, int rank,
        const std::string &property, const std::string &value);
    void printObj_propertyFromRank(std::ostream &out, int rank,
        const std::string &property, int value);
    void printObj_propertyFromRank(std::ostream &out, int rank,
        const std::string &property, bool value);
    void printObj_propertyFromRank(std::ostream &out, int rank,
        const std::string &property, const IPrintable &value);
    void printObj_propertyFromRank(std::ostream &out, int rank,
        const std::string &property, const IPrintable *value);
    void printObj_propertyFromRank(std::ostream &out, int rank,
        const std::string &property);

    /**
     * Start a section with a heading when listing an object's properties.
     * (useful when objects have several properties).
     */
    void printObj_startSection(std::ostream &out, const std::string &heading);

    /**
     * End a section when listing an object's properties (see
     * printObj_startSection).
     */
    void printObj_endSection(std::ostream &out);

    /**
     * Print a collection of pointers to objects that all inheret from
     * IPrintable.
     **/
    template<typename IteratorT>
    void printPtrs(std::ostream &out, IteratorT b, IteratorT e)
    {
        int i = 0;
        out << "[";
        while(b != e) {
            if(i != 0) { out << ", "; }
            (*b)->printSimp(out);
            b++; i++;
        }
        out << "]";
    }

    /**
     * Print a collection of objects that inheret from IPrintable.
     **/
    template<typename IteratorT>
    void printObjs(std::ostream &out, IteratorT b, IteratorT e)
    {
        int i = 0;
        out << "[";
        while(b != e) {
            if(i != 0) { out << ", "; }
            (*b).printSimp(out);
            b++; i++;
        }
        out << "]";
    }

    /**
     * Print a collection of values that can be passed to an ostream via
     * the << operator.
     **/
    template <typename IteratorT>
    void printVals(std::ostream &out, IteratorT b, IteratorT e)
    {
        int i = 0;
        out << "[";
        while(b != e) {
            if(i != 0) { out << ", "; }
            out << *b;
            b++; i++;
        }
        out << "]";
    }
    
    /**
     * Print a collection of values that can be passed to an ostream via
     * the << operator from a specified rank.
     **/
    template <typename IteratorT>
    void printValsFromRank(
        std::ostream &out, int rank, IteratorT b, IteratorT e)
    {
        const int MSG_SIZE  = 0;
        const int MSG_VALUE = 1;
        const int MSG_NUM_MSGS = 2;
        MPI_Request req[MSG_NUM_MSGS];

        int size;
        char *cstr;

        // If the rank send the value
        if(myRank() == rank) {
            std::stringstream ss;
            printVals(ss, b, e);

            size = ss.str().length() + 1;
            cstr = new char[size];
            strcpy(cstr, ss.str().c_str());
            
            MPI_Isend(&size, 1, MPI_INT, masterRank(), MSG_SIZE,
                      MPI_COMM_WORLD, &req[MSG_SIZE]);
            MPI_Isend(cstr, size, MPI_CHAR, masterRank(), MSG_VALUE,
                      MPI_COMM_WORLD, &req[MSG_VALUE]);
        }

        // Have master receive and print the value
        if(isMasterRank()) {
            char *cstrRecv;
            int ret;
            
            MPI_Recv(&size, 1, MPI_INT, rank, MSG_SIZE,
                     MPI_COMM_WORLD, MPI_STATUS_IGNORE);
            cstrRecv = new char[size];
            ret = MPI_Recv(cstrRecv, size, MPI_CHAR, rank, MSG_VALUE,
                     MPI_COMM_WORLD, MPI_STATUS_IGNORE);
            
            out << cstrRecv;
            delete [] cstrRecv;
        }
        
        // Have rank wait for its communication to complete
        if(myRank() == rank) {
            MPI_Waitall(1, req, MPI_STATUSES_IGNORE);
            delete [] cstr;
        }
    }


    /**
     * Allow STL pairs to be printed to ostream objects.
     */
    template <typename T1, typename T2>
    std::ostream &operator<<(std::ostream &out, const std::pair<T1, T2> &val)
    {
        out << hiFmt("(")
            << val.first
            << hiFmt(": ")
            << val.second
            << hiFmt(")");
        return out;
    }

    /**
     * Allow STL sets to be printed to ostream objects.
     */
    template <typename T>
    void printSet(std::ostream &out, const std::set<T> &val)
    {
        out << hiFmt("{");
        for(typename std::set<T>::iterator i = val.begin();
            i != val.end(); i++)
        {
            if(i != val.begin()) { out << hiFmt(", "); }
            out << *i;
        }
        out << hiFmt("}");
    }
///@}


#endif
/** @}*/
