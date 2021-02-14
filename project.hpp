/******************************************************************************
 * GRIDGEN: Grid Generating Compiler
 * By: Andy Stone (aistone@gmail.com)
 * (C) Copyright 2011 Colorado State University
 *****************************************************************************/
#ifndef PROJECT_HPP_
#define PROJECT_HPP_

#include "gridLibCall.hpp"
#include "environment.hpp"
#include <string>
#include <vector>
#include <rose.h>

/** Stores location of a source file in a project. */
struct FileInfo {
    FileInfo(std::string path, std::string filename) :
        mPath(path), mFileName(filename) {}

    std::string  mPath;
    std::string  mFileName;


    // Return filename ROSE stores the post-processed version of the file in
    std::string postProcessedFileName() {
        std::string ret;

        // Add _postprocessed after filename (but before extension)
        int pos = mFileName.find_last_of('.');
        ret  = mFileName.substr(0,pos);
        ret += "_postprocessed";
        ret += mFileName.substr(pos);

        // Make sure fortran extension has lower-case f
        pos = ret.find_last_of(".F");
        if(pos != -1) {
            ret[pos] = 'f';
        }

        return ret;
    }
};

/**
 * Represents a project to compile with GridGen.
 **/
class Project {
  public:
    /** Construct a project given command line arguments */
    Project(int argc, char **argv, bool analyze = true);
   
    /**
     * Analyze the project (extract information about grids and function
     * call locations.
     **/
    void analyze();

    /** Generate code for project */
    void generate();

  private:
    /** Generate a comment above each gridgen call */
    //void generateComments(const CallsVectorT &calls);

    void replaceApply(const CallsVectorT &calls);
    void replaceApply(Call__data_apply *call);
    void startLoops(Call__data_apply *call, ostringstream &ss);
    void endLoops(Call__data_apply *call, ostringstream &ss);

    /** Functions to insert code into AST */
    void placeAboveCall(SgFunctionCallExp *node, const string &s);
    void replaceCall(SgFunctionCallExp *node, const string &s);
    
  // member vars
    SgProject *mProj;
    std::vector<FileInfo> mFiles;
};

#endif
