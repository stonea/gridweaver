/******************************************************************************
 * GRIDGEN: Grid Generating Compiler
 * By: Andy Stone (aistone@gmail.com)
 * (C) Copyright 2011 Colorado State University
 *****************************************************************************/
#include "project.hpp"

#include "analyses.hpp"
#include "utils.hpp"
#include <iostream>
#include <string>
using namespace std;

Project::Project(int argc, char **argv, bool analyze) {
    // Load project through ROSE frontend
    mProj = frontend(argc, argv);
    if(mProj == NULL) {
        cerr << "Rose unable to read project." << endl;
        exit(1);
    }
    generatePDF(*mProj);

    // Extract information about files in project and their paths
    typedef Rose_STL_Container<std::string> containerT;
    containerT files = mProj->getAbsolutePathFileNames();
    for(containerT::iterator i = files.begin();
        i != files.end(); i++)
    {
        int pos = i->find_last_of('/');
        string path = i->substr(0, pos);
        string file = i->substr(pos+1);
        mFiles.push_back(FileInfo(path, file));
    }

    // conduct an analysis of the code if specified
    if(analyze) { this->analyze(); }
}


void Project::analyze() {
    // Gather calls to library and construct environment
    CallsAnalysis callAnl;
    CallsVectorT calls = callAnl.analyze(mProj);
    gEnv.buildFromCalls(calls);

    gEnv.print(cout);

    //Instrument calls with comments
    replaceApply(calls);
}


   
void Project::generate() {
    // Use ROSE to output the resulting program
    mProj->unparse();

    // Move output files into their correct directories
    typedef vector<FileInfo>::iterator  iterT;
    for(iterT i = mFiles.begin(); i != mFiles.end(); i++) {
        // move rose_<filename>.<ext>
        string cmd = "mv rose_" + i->mFileName + " " + i->mPath;
        system(cmd.c_str());

        // move <filename>_postprocesed.<ext>
        cmd = "mv " + i->postProcessedFileName() + " " + i->mPath;
        system(cmd.c_str());
    }
}

void Project::replaceApply(const CallsVectorT &calls) {
    // Iterate through calls, find apply statements
    for(CallsConstIteratorT i = calls.begin(); i != calls.end(); i++) {
        if((*i)->operation() == CALL__DATA_APPLY) {
            replaceApply(dynamic_cast<Call__data_apply*>(*i));
        }
    }
}

void Project::replaceApply(Call__data_apply *call) {
    ostringstream ss;

    // Place new code in a string
    startLoops(call, ss);
    ss << indt
       << call->funcDef()->unparseToString();
    //printNodeAndChildren(cout, call->funcDef()->get_traversalSuccessorByIndex(0));
    endLoops(call, ss);

    //data_apply%vals(x, y, lbid) = func(neighs)
    
    // Remove old code
    replaceCall(call->location(), ss.str());
}

void Project::startLoops(Call__data_apply *call, ostringstream &ss) {
    ss << pushIndt;
    ss << indt << "do __gg_idx_lbid = lbound("
       << call->lhs().ident() << "\%vals,3), "
       << "ubound(" << call->lhs().ident() << "\%vals,3)" << pushIndt;
    ss << indt << "do __gg_idx_y = 1, "
       << call->lhs().ident() << "\%dist\%blockSize(2)" << pushIndt;
    ss << indt << "do __gg_idx_x = 1, "
       << call->lhs().ident() << "\%dist\%blockSize(1)" << pushIndt;
}

void Project::endLoops(Call__data_apply *call, ostringstream &ss) {
    ss << popIndt << indt << "end do";
    ss << popIndt << indt << "end do";
    ss << popIndt << indt << "end do";
    ss << popIndt << endl;
}


void Project::placeAboveCall(SgFunctionCallExp *node, const string &s) {
    SageInterface::addTextForUnparser(node, s, AstUnparseAttribute::e_after);
}

void Project::replaceCall(SgFunctionCallExp *node, const string &s) {
    SgNode *parent = node->get_parent();
    while(parent->variantT() !=  V_SgExprStatement) {
        parent = parent->get_parent();
    }
    SgExprStatement *parentExp = isSgExprStatement(parent);
    SgNullStatement *newStmt = new SgNullStatement(parent->get_file_info());

    // Insert new code
    SageInterface::addTextForUnparser(newStmt, s, AstUnparseAttribute::e_after);
       
    // Remove old code
    SageInterface::replaceStatement(parentExp, newStmt);
}

