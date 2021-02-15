/******************************************************************************
 * GRIDGEN: Grid Generating Compiler
 * By: Andy Stone (aistone@gmail.com)
 * (C) Copyright 2011 Colorado State University
 *****************************************************************************/

#include "codegen.hpp"

#include "utils.hpp"
#include <string>
using namespace std;


void buildHeader(ostringstream &ss, const string &dataName)
{
    ss << indt << "! <<<--- GRIDWEAVER REPLACED DATA APPLY HERE --->>>\n";
    ss << indt << "gw____blkW = distribution_width("
               << dataName << "\%dist)";
    ss << indt << "gw____blkH = distribution_height("
               << dataName << "\%dist)";

    // Loops
    ss << indt << "! Iterate over all points in all local blocks";
    ss << indt << "do gw____lbid=lbound(self%vals,3), ubound(self%vals,3)"
       << pushIndt;
    ss << indt << "do gw____blkJ=1,gw____blkH" << pushIndt;
    ss << indt << "do gw____blkI=1,gw____blkW" << pushIndt;
}

void buildFooter(ostringstream &ss)
{
    // end loops
    ss << popIndt << indt << "end do";
    ss << popIndt << indt << "end do";
    ss << popIndt << indt << "end do";

    // Footer
    ss << indt << "! <<<--- END OF GRIDWEAVER REPLACEMENT --->>>\n";
}

void extractBody(string &body, SgFunctionDefinition *func) {
    // Extract body of function
    ostringstream ssBody;
    SgStatementPtrList stmts = func->get_body()->get_statements();
    for(int i = 0; i < stmts.size(); i++) {
        SgNode *n = stmts[i];

        if(isSgVariableDeclaration(n)) { continue; }
        if(isSgInterfaceStatement(n)) { continue; }

        ssBody << endl << n->unparseToCompleteString();
    }
    body = ssBody.str();
}


void findAndReplaceInString(string &body, string strFind, string strReplace)
{
    while(body.find(strFind) != string::npos) {
        int idx = body.find(strFind);
        body.erase(idx, strFind.size());
        body.insert(idx, strReplace);
    }
}

//------------------------------------------------------------------


CodeGen::CodeGen(CallsAnalysis *analysis) :
    mpCalls(analysis)
{
}

void CodeGen::apply() {
    DataApplyGenerator gen(mpCalls);
    gen.accept(mpCalls->calls());
}


void DataApplyGenerator::visit__data_apply1(Call__data_apply1 *call) {
    // Get this statement's location in the AST 
    SgFunctionCallExp *location = call->location();
    SgExprStatement *parent = isSgExprStatement(location->get_parent());
    SgNullStatement *newStmt = new SgNullStatement(parent->get_file_info());

    // ***********************************************
    // ***** Generate text for replacement code: *****
    // ***********************************************
    ostringstream ss;
    buildHeader(ss, call->dataIn1());

    // ******************************************
    // ***** Generate stencil function code *****
    // ******************************************

    // Extract body of function
    string body;
    extractBody(body, call->stencilFuncDef());

    // Build strings that represents the data objects
    ostringstream ssDataOut, ssDataIn1;
    ssDataOut << call->dataOut() << "\%vals(gw____blkI, gw____blkJ, gw____lbid)";
    ssDataIn1 << call->dataIn1() << "\%vals";

    // Find and replace instances the return statement
    SgFunctionDeclaration *decl = call->stencilFuncDef()->get_declaration();
    string funcName = decl->get_name().getString();
    findAndReplaceInString(body, funcName, ssDataOut.str());

    // Find and replace instances to data accesses
    string dataIn1Name = (decl->get_args()[0])->get_qualified_name().getString();
    findAndReplaceInString(body, dataIn1Name, ssDataIn1.str());

    // Find and replace i index
    string iIdx = (decl->get_args()[1])->get_qualified_name().getString();
    findAndReplaceInString(body, iIdx, "gw___blkI");

    // Find and replace j index
    string jIdx = (decl->get_args()[2])->get_qualified_name().getString();
    findAndReplaceInString(body, jIdx, "gw___blkJ");

    // Insert body
    ss << body << endl;
    
    // insert footer
    buildFooter(ss);

    // *********************************************
    // ***** Insert new code; remove old code: *****
    // *********************************************
    SageInterface::addTextForUnparser(
        newStmt, ss.str(), AstUnparseAttribute::e_after);
    SageInterface::replaceStatement(parent, newStmt);
}


void DataApplyGenerator::visit__data_apply2(Call__data_apply2 *call) {
    // Get this statement's location in the AST 
    SgFunctionCallExp *location = call->location();
    SgExprStatement *parent = isSgExprStatement(location->get_parent());
    SgNullStatement *newStmt = new SgNullStatement(parent->get_file_info());

    // ***********************************************
    // ***** Generate text for replacement code: *****
    // ***********************************************
    ostringstream ss;
    buildHeader(ss, call->dataIn1());

    // ******************************************
    // ***** Generate stencil function code *****
    // ******************************************

    // Extract body of function
    string body;
    extractBody(body, call->stencilFuncDef());

    // Build strings that represents the data objects
    ostringstream ssDataOut, ssDataIn1, ssDataIn2;
    ssDataOut << call->dataOut() << "\%vals(gw____blkI, gw____blkJ, gw____lbid)";
    ssDataIn1 << call->dataIn1() << "\%vals";
    ssDataIn2 << call->dataIn2() << "\%vals";

    // Find and replace instances the return statement
    SgFunctionDeclaration *decl = call->stencilFuncDef()->get_declaration();
    string funcName = decl->get_name().getString();
    findAndReplaceInString(body, funcName, ssDataOut.str());

    // Find and replace instances to data accesses
    string dataIn1Name = (decl->get_args()[0])->get_qualified_name().getString();
    string dataIn2Name = (decl->get_args()[1])->get_qualified_name().getString();
    findAndReplaceInString(body, dataIn1Name, ssDataIn1.str());
    findAndReplaceInString(body, dataIn2Name, ssDataIn2.str());

    // Find and replace i index
    string iIdx = (decl->get_args()[1])->get_qualified_name().getString();
    findAndReplaceInString(body, iIdx, "gw___blkI");

    // Find and replace j index
    string jIdx = (decl->get_args()[2])->get_qualified_name().getString();
    findAndReplaceInString(body, jIdx, "gw___blkJ");

    // Insert body
    ss << body << endl;
    
    // insert footer
    buildFooter(ss);

    // *********************************************
    // ***** Insert new code; remove old code: *****
    // *********************************************
    SageInterface::addTextForUnparser(
        newStmt, ss.str(), AstUnparseAttribute::e_after);
    SageInterface::replaceStatement(parent, newStmt);
}


void DataApplyGenerator::visit__data_apply3(Call__data_apply3 *call) {
    // Get this statement's location in the AST 
    SgFunctionCallExp *location = call->location();
    SgExprStatement *parent = isSgExprStatement(location->get_parent());
    SgNullStatement *newStmt = new SgNullStatement(parent->get_file_info());

    // ***********************************************
    // ***** Generate text for replacement code: *****
    // ***********************************************
    ostringstream ss;
    buildHeader(ss, call->dataIn1());

    // ******************************************
    // ***** Generate stencil function code *****
    // ******************************************

    // Extract body of function
    string body;
    extractBody(body, call->stencilFuncDef());

    // Build strings that represents the data objects
    ostringstream ssDataOut, ssDataIn1, ssDataIn2, ssDataIn3;

    ssDataOut << call->dataOut() << "\%vals(gw____blkI, gw____blkJ, gw____lbid)";
    ssDataIn1 << call->dataIn1() << "\%vals";
    ssDataIn2 << call->dataIn2() << "\%vals";
    ssDataIn3 << call->dataIn3() << "\%vals";

    // Find and replace instances the return statement
    SgFunctionDeclaration *decl = call->stencilFuncDef()->get_declaration();
    string funcName = decl->get_name().getString();
    findAndReplaceInString(body, funcName, ssDataOut.str());

    // Find and replace instances to data accesses
    string dataIn1Name = (decl->get_args()[0])->get_qualified_name().getString();
    string dataIn2Name = (decl->get_args()[1])->get_qualified_name().getString();
    string dataIn3Name = (decl->get_args()[2])->get_qualified_name().getString();
    findAndReplaceInString(body, dataIn1Name, ssDataIn1.str());
    findAndReplaceInString(body, dataIn2Name, ssDataIn2.str());
    findAndReplaceInString(body, dataIn3Name, ssDataIn3.str());

    // Find and replace i index
    string iIdx = (decl->get_args()[1])->get_qualified_name().getString();
    findAndReplaceInString(body, iIdx, "gw___blkI");

    // Find and replace j index
    string jIdx = (decl->get_args()[2])->get_qualified_name().getString();
    findAndReplaceInString(body, jIdx, "gw___blkJ");

    // Insert body
    ss << body << endl;
    
    // insert footer
    buildFooter(ss);

    // *********************************************
    // ***** Insert new code; remove old code: *****
    // *********************************************
    SageInterface::addTextForUnparser(
        newStmt, ss.str(), AstUnparseAttribute::e_after);
    SageInterface::replaceStatement(parent, newStmt);
}


void DataApplyGenerator::visit__data_apply4(Call__data_apply4 *call) {
    // Get this statement's location in the AST 
    SgFunctionCallExp *location = call->location();
    SgExprStatement *parent = isSgExprStatement(location->get_parent());
    SgNullStatement *newStmt = new SgNullStatement(parent->get_file_info());

    // ***********************************************
    // ***** Generate text for replacement code: *****
    // ***********************************************
    ostringstream ss;
    buildHeader(ss, call->dataIn1());

    // ******************************************
    // ***** Generate stencil function code *****
    // ******************************************

    // Extract body of function
    string body;
    extractBody(body, call->stencilFuncDef());

    // Build strings that represents the data objects
    ostringstream ssDataOut, ssDataIn1, ssDataIn2, ssDataIn3, ssDataIn4;

    ssDataOut << call->dataOut() << "\%vals(gw____blkI, gw____blkJ, gw____lbid)";
    ssDataIn1 << call->dataIn1() << "\%vals";
    ssDataIn2 << call->dataIn2() << "\%vals";
    ssDataIn3 << call->dataIn3() << "\%vals";
    ssDataIn4 << call->dataIn4() << "\%vals";

    // Find and replace instances the return statement
    SgFunctionDeclaration *decl = call->stencilFuncDef()->get_declaration();
    string funcName = decl->get_name().getString();
    findAndReplaceInString(body, funcName, ssDataOut.str());

    // Find and replace instances to data accesses
    string dataIn1Name = (decl->get_args()[0])->get_qualified_name().getString();
    string dataIn2Name = (decl->get_args()[1])->get_qualified_name().getString();
    string dataIn3Name = (decl->get_args()[2])->get_qualified_name().getString();
    string dataIn4Name = (decl->get_args()[3])->get_qualified_name().getString();
    findAndReplaceInString(body, dataIn1Name, ssDataIn1.str());
    findAndReplaceInString(body, dataIn2Name, ssDataIn2.str());
    findAndReplaceInString(body, dataIn3Name, ssDataIn3.str());
    findAndReplaceInString(body, dataIn4Name, ssDataIn4.str());

    // Find and replace i index
    string iIdx = (decl->get_args()[1])->get_qualified_name().getString();
    findAndReplaceInString(body, iIdx, "gw___blkI");

    // Find and replace j index
    string jIdx = (decl->get_args()[2])->get_qualified_name().getString();
    findAndReplaceInString(body, jIdx, "gw___blkJ");

    // Insert body
    ss << body << endl;
    
    // insert footer
    buildFooter(ss);

    // *********************************************
    // ***** Insert new code; remove old code: *****
    // *********************************************
    SageInterface::addTextForUnparser(
        newStmt, ss.str(), AstUnparseAttribute::e_after);
    SageInterface::replaceStatement(parent, newStmt);
}


void DataApplyGenerator::visit__data_apply5(Call__data_apply5 *call) {
    // Get this statement's location in the AST 
    SgFunctionCallExp *location = call->location();
    SgExprStatement *parent = isSgExprStatement(location->get_parent());
    SgNullStatement *newStmt = new SgNullStatement(parent->get_file_info());

    // ***********************************************
    // ***** Generate text for replacement code: *****
    // ***********************************************
    ostringstream ss;
    buildHeader(ss, call->dataIn1());

    // ******************************************
    // ***** Generate stencil function code *****
    // ******************************************

    // Extract body of function
    string body;
    extractBody(body, call->stencilFuncDef());

    // Build strings that represents the data objects
    ostringstream ssDataOut, ssDataIn1, ssDataIn2, ssDataIn3, ssDataIn4,
        ssDataIn5;

    ssDataOut << call->dataOut() << "\%vals(gw____blkI, gw____blkJ, gw____lbid)";
    ssDataIn1 << call->dataIn1() << "\%vals";
    ssDataIn2 << call->dataIn2() << "\%vals";
    ssDataIn3 << call->dataIn3() << "\%vals";
    ssDataIn4 << call->dataIn4() << "\%vals";
    ssDataIn5 << call->dataIn5() << "\%vals";

    // Find and replace instances the return statement
    SgFunctionDeclaration *decl = call->stencilFuncDef()->get_declaration();
    string funcName = decl->get_name().getString();
    findAndReplaceInString(body, funcName, ssDataOut.str());

    // Find and replace instances to data accesses
    string dataIn1Name = (decl->get_args()[0])->get_qualified_name().getString();
    string dataIn2Name = (decl->get_args()[1])->get_qualified_name().getString();
    string dataIn3Name = (decl->get_args()[2])->get_qualified_name().getString();
    string dataIn4Name = (decl->get_args()[3])->get_qualified_name().getString();
    string dataIn5Name = (decl->get_args()[4])->get_qualified_name().getString();
    findAndReplaceInString(body, dataIn1Name, ssDataIn1.str());
    findAndReplaceInString(body, dataIn2Name, ssDataIn2.str());
    findAndReplaceInString(body, dataIn3Name, ssDataIn3.str());
    findAndReplaceInString(body, dataIn4Name, ssDataIn4.str());
    findAndReplaceInString(body, dataIn5Name, ssDataIn5.str());

    // Find and replace i index
    string iIdx = (decl->get_args()[1])->get_qualified_name().getString();
    findAndReplaceInString(body, iIdx, "gw___blkI");

    // Find and replace j index
    string jIdx = (decl->get_args()[2])->get_qualified_name().getString();
    findAndReplaceInString(body, jIdx, "gw___blkJ");

    // Insert body
    ss << body << endl;
    
    // insert footer
    buildFooter(ss);

    // *********************************************
    // ***** Insert new code; remove old code: *****
    // *********************************************
    SageInterface::addTextForUnparser(
        newStmt, ss.str(), AstUnparseAttribute::e_after);
    SageInterface::replaceStatement(parent, newStmt);
}


void DataApplyGenerator::visit__data_apply6(Call__data_apply6 *call) {
    // Get this statement's location in the AST 
    SgFunctionCallExp *location = call->location();
    SgExprStatement *parent = isSgExprStatement(location->get_parent());
    SgNullStatement *newStmt = new SgNullStatement(parent->get_file_info());

    // ***********************************************
    // ***** Generate text for replacement code: *****
    // ***********************************************
    ostringstream ss;
    buildHeader(ss, call->dataIn1());

    // ******************************************
    // ***** Generate stencil function code *****
    // ******************************************

    // Extract body of function
    string body;
    extractBody(body, call->stencilFuncDef());

    // Build strings that represents the data objects
    ostringstream ssDataOut, ssDataIn1, ssDataIn2, ssDataIn3, ssDataIn4,
        ssDataIn5, ssDataIn6;

    ssDataOut << call->dataOut() << "\%vals(gw____blkI, gw____blkJ, gw____lbid)";
    ssDataIn1 << call->dataIn1() << "\%vals";
    ssDataIn2 << call->dataIn2() << "\%vals";
    ssDataIn3 << call->dataIn3() << "\%vals";
    ssDataIn4 << call->dataIn4() << "\%vals";
    ssDataIn5 << call->dataIn5() << "\%vals";
    ssDataIn6 << call->dataIn6() << "\%vals";

    // Find and replace instances the return statement
    SgFunctionDeclaration *decl = call->stencilFuncDef()->get_declaration();
    string funcName = decl->get_name().getString();
    findAndReplaceInString(body, funcName, ssDataOut.str());

    // Find and replace instances to data accesses
    string dataIn1Name = (decl->get_args()[0])->get_qualified_name().getString();
    string dataIn2Name = (decl->get_args()[1])->get_qualified_name().getString();
    string dataIn3Name = (decl->get_args()[2])->get_qualified_name().getString();
    string dataIn4Name = (decl->get_args()[3])->get_qualified_name().getString();
    string dataIn5Name = (decl->get_args()[4])->get_qualified_name().getString();
    string dataIn6Name = (decl->get_args()[5])->get_qualified_name().getString();
    findAndReplaceInString(body, dataIn1Name, ssDataIn1.str());
    findAndReplaceInString(body, dataIn2Name, ssDataIn2.str());
    findAndReplaceInString(body, dataIn3Name, ssDataIn3.str());
    findAndReplaceInString(body, dataIn4Name, ssDataIn4.str());
    findAndReplaceInString(body, dataIn5Name, ssDataIn5.str());
    findAndReplaceInString(body, dataIn6Name, ssDataIn6.str());

    // Find and replace i index
    string iIdx = (decl->get_args()[1])->get_qualified_name().getString();
    findAndReplaceInString(body, iIdx, "gw___blkI");

    // Find and replace j index
    string jIdx = (decl->get_args()[2])->get_qualified_name().getString();
    findAndReplaceInString(body, jIdx, "gw___blkJ");

    // Insert body
    ss << body << endl;
    
    // insert footer
    buildFooter(ss);

    // *********************************************
    // ***** Insert new code; remove old code: *****
    // *********************************************
    SageInterface::addTextForUnparser(
        newStmt, ss.str(), AstUnparseAttribute::e_after);
    SageInterface::replaceStatement(parent, newStmt);
}


void DataApplyGenerator::visit__data_apply7(Call__data_apply7 *call) {
    // Get this statement's location in the AST 
    SgFunctionCallExp *location = call->location();
    SgExprStatement *parent = isSgExprStatement(location->get_parent());
    SgNullStatement *newStmt = new SgNullStatement(parent->get_file_info());

    // ***********************************************
    // ***** Generate text for replacement code: *****
    // ***********************************************
    ostringstream ss;
    buildHeader(ss, call->dataIn1());

    // ******************************************
    // ***** Generate stencil function code *****
    // ******************************************

    // Extract body of function
    string body;
    extractBody(body, call->stencilFuncDef());

    // Build strings that represents the data objects
    ostringstream ssDataOut, ssDataIn1, ssDataIn2, ssDataIn3, ssDataIn4,
        ssDataIn5, ssDataIn6, ssDataIn7;

    ssDataOut << call->dataOut() << "\%vals(gw____blkI, gw____blkJ, gw____lbid)";
    ssDataIn1 << call->dataIn1() << "\%vals";
    ssDataIn2 << call->dataIn2() << "\%vals";
    ssDataIn3 << call->dataIn3() << "\%vals";
    ssDataIn4 << call->dataIn4() << "\%vals";
    ssDataIn5 << call->dataIn5() << "\%vals";
    ssDataIn6 << call->dataIn6() << "\%vals";
    ssDataIn7 << call->dataIn7() << "\%vals";

    // Find and replace instances the return statement
    SgFunctionDeclaration *decl = call->stencilFuncDef()->get_declaration();
    string funcName = decl->get_name().getString();
    findAndReplaceInString(body, funcName, ssDataOut.str());

    // Find and replace instances to data accesses
    string dataIn1Name = (decl->get_args()[0])->get_qualified_name().getString();
    string dataIn2Name = (decl->get_args()[1])->get_qualified_name().getString();
    string dataIn3Name = (decl->get_args()[2])->get_qualified_name().getString();
    string dataIn4Name = (decl->get_args()[3])->get_qualified_name().getString();
    string dataIn5Name = (decl->get_args()[4])->get_qualified_name().getString();
    string dataIn6Name = (decl->get_args()[5])->get_qualified_name().getString();
    string dataIn7Name = (decl->get_args()[6])->get_qualified_name().getString();
    string dataIn8Name = (decl->get_args()[7])->get_qualified_name().getString();
    findAndReplaceInString(body, dataIn1Name, ssDataIn1.str());
    findAndReplaceInString(body, dataIn2Name, ssDataIn2.str());
    findAndReplaceInString(body, dataIn3Name, ssDataIn3.str());
    findAndReplaceInString(body, dataIn4Name, ssDataIn4.str());
    findAndReplaceInString(body, dataIn5Name, ssDataIn5.str());
    findAndReplaceInString(body, dataIn6Name, ssDataIn6.str());
    findAndReplaceInString(body, dataIn7Name, ssDataIn7.str());

    // Find and replace i index
    string iIdx = (decl->get_args()[1])->get_qualified_name().getString();
    findAndReplaceInString(body, iIdx, "gw___blkI");

    // Find and replace j index
    string jIdx = (decl->get_args()[2])->get_qualified_name().getString();
    findAndReplaceInString(body, jIdx, "gw___blkJ");

    // Insert body
    ss << body << endl;
    
    // insert footer
    buildFooter(ss);

    // *********************************************
    // ***** Insert new code; remove old code: *****
    // *********************************************
    SageInterface::addTextForUnparser(
        newStmt, ss.str(), AstUnparseAttribute::e_after);
    SageInterface::replaceStatement(parent, newStmt);
}


void DataApplyGenerator::visit__data_apply8(Call__data_apply8 *call) {
    // Get this statement's location in the AST 
    SgFunctionCallExp *location = call->location();
    SgExprStatement *parent = isSgExprStatement(location->get_parent());
    SgNullStatement *newStmt = new SgNullStatement(parent->get_file_info());

    // ***********************************************
    // ***** Generate text for replacement code: *****
    // ***********************************************
    ostringstream ss;
    buildHeader(ss, call->dataIn1());

    // ******************************************
    // ***** Generate stencil function code *****
    // ******************************************

    // Extract body of function
    string body;
    extractBody(body, call->stencilFuncDef());

    // Build strings that represents the data objects
    ostringstream ssDataOut, ssDataIn1, ssDataIn2, ssDataIn3, ssDataIn4,
        ssDataIn5, ssDataIn6, ssDataIn7, ssDataIn8;

    ssDataOut << call->dataOut() << "\%vals(gw____blkI, gw____blkJ, gw____lbid)";
    ssDataIn1 << call->dataIn1() << "\%vals";
    ssDataIn2 << call->dataIn2() << "\%vals";
    ssDataIn3 << call->dataIn3() << "\%vals";
    ssDataIn4 << call->dataIn4() << "\%vals";
    ssDataIn5 << call->dataIn5() << "\%vals";
    ssDataIn6 << call->dataIn6() << "\%vals";
    ssDataIn7 << call->dataIn7() << "\%vals";
    ssDataIn8 << call->dataIn8() << "\%vals";

    // Find and replace instances the return statement
    SgFunctionDeclaration *decl = call->stencilFuncDef()->get_declaration();
    string funcName = decl->get_name().getString();
    findAndReplaceInString(body, funcName, ssDataOut.str());

    // Find and replace instances to data accesses
    string dataIn1Name = (decl->get_args()[0])->get_qualified_name().getString();
    string dataIn2Name = (decl->get_args()[1])->get_qualified_name().getString();
    string dataIn3Name = (decl->get_args()[2])->get_qualified_name().getString();
    string dataIn4Name = (decl->get_args()[3])->get_qualified_name().getString();
    string dataIn5Name = (decl->get_args()[4])->get_qualified_name().getString();
    string dataIn6Name = (decl->get_args()[5])->get_qualified_name().getString();
    string dataIn7Name = (decl->get_args()[6])->get_qualified_name().getString();
    string dataIn8Name = (decl->get_args()[7])->get_qualified_name().getString();
    findAndReplaceInString(body, dataIn1Name, ssDataIn1.str());
    findAndReplaceInString(body, dataIn2Name, ssDataIn2.str());
    findAndReplaceInString(body, dataIn3Name, ssDataIn3.str());
    findAndReplaceInString(body, dataIn4Name, ssDataIn4.str());
    findAndReplaceInString(body, dataIn5Name, ssDataIn5.str());
    findAndReplaceInString(body, dataIn6Name, ssDataIn6.str());
    findAndReplaceInString(body, dataIn7Name, ssDataIn7.str());
    findAndReplaceInString(body, dataIn8Name, ssDataIn8.str());

    // Find and replace i index
    string iIdx = (decl->get_args()[1])->get_qualified_name().getString();
    findAndReplaceInString(body, iIdx, "gw___blkI");

    // Find and replace j index
    string jIdx = (decl->get_args()[2])->get_qualified_name().getString();
    findAndReplaceInString(body, jIdx, "gw___blkJ");

    // Insert body
    ss << body << endl;
    
    // insert footer
    buildFooter(ss);

    // *********************************************
    // ***** Insert new code; remove old code: *****
    // *********************************************
    SageInterface::addTextForUnparser(
        newStmt, ss.str(), AstUnparseAttribute::e_after);
    SageInterface::replaceStatement(parent, newStmt);
}


void DataApplyGenerator::visit__data_apply9(Call__data_apply9 *call) {
    // Get this statement's location in the AST 
    SgFunctionCallExp *location = call->location();
    SgExprStatement *parent = isSgExprStatement(location->get_parent());
    SgNullStatement *newStmt = new SgNullStatement(parent->get_file_info());

    // ***********************************************
    // ***** Generate text for replacement code: *****
    // ***********************************************
    ostringstream ss;
    buildHeader(ss, call->dataIn1());

    // ******************************************
    // ***** Generate stencil function code *****
    // ******************************************

    // Extract body of function
    string body;
    extractBody(body, call->stencilFuncDef());

    // Build strings that represents the data objects
    ostringstream ssDataOut, ssDataIn1, ssDataIn2, ssDataIn3, ssDataIn4,
        ssDataIn5, ssDataIn6, ssDataIn7, ssDataIn8, ssDataIn9;

    ssDataOut << call->dataOut() << "\%vals(gw____blkI, gw____blkJ, gw____lbid)";
    ssDataIn1 << call->dataIn1() << "\%vals";
    ssDataIn2 << call->dataIn2() << "\%vals";
    ssDataIn3 << call->dataIn3() << "\%vals";
    ssDataIn4 << call->dataIn4() << "\%vals";
    ssDataIn5 << call->dataIn5() << "\%vals";
    ssDataIn6 << call->dataIn6() << "\%vals";
    ssDataIn7 << call->dataIn7() << "\%vals";
    ssDataIn8 << call->dataIn8() << "\%vals";
    ssDataIn9 << call->dataIn9() << "\%vals";

    // Find and replace instances the return statement
    SgFunctionDeclaration *decl = call->stencilFuncDef()->get_declaration();
    string funcName = decl->get_name().getString();
    findAndReplaceInString(body, funcName, ssDataOut.str());

    // Find and replace instances to data accesses
    string dataIn1Name = (decl->get_args()[0])->get_qualified_name().getString();
    string dataIn2Name = (decl->get_args()[1])->get_qualified_name().getString();
    string dataIn3Name = (decl->get_args()[2])->get_qualified_name().getString();
    string dataIn4Name = (decl->get_args()[3])->get_qualified_name().getString();
    string dataIn5Name = (decl->get_args()[4])->get_qualified_name().getString();
    string dataIn6Name = (decl->get_args()[5])->get_qualified_name().getString();
    string dataIn7Name = (decl->get_args()[6])->get_qualified_name().getString();
    string dataIn8Name = (decl->get_args()[7])->get_qualified_name().getString();
    string dataIn9Name = (decl->get_args()[8])->get_qualified_name().getString();
    findAndReplaceInString(body, dataIn1Name, ssDataIn1.str());
    findAndReplaceInString(body, dataIn2Name, ssDataIn2.str());
    findAndReplaceInString(body, dataIn3Name, ssDataIn3.str());
    findAndReplaceInString(body, dataIn4Name, ssDataIn4.str());
    findAndReplaceInString(body, dataIn5Name, ssDataIn5.str());
    findAndReplaceInString(body, dataIn6Name, ssDataIn6.str());
    findAndReplaceInString(body, dataIn7Name, ssDataIn7.str());
    findAndReplaceInString(body, dataIn8Name, ssDataIn8.str());
    findAndReplaceInString(body, dataIn9Name, ssDataIn9.str());

    // Find and replace i index
    string iIdx = (decl->get_args()[1])->get_qualified_name().getString();
    findAndReplaceInString(body, iIdx, "gw___blkI");

    // Find and replace j index
    string jIdx = (decl->get_args()[2])->get_qualified_name().getString();
    findAndReplaceInString(body, jIdx, "gw___blkJ");

    // Insert body
    ss << body << endl;
    
    // insert footer
    buildFooter(ss);

    // *********************************************
    // ***** Insert new code; remove old code: *****
    // *********************************************
    SageInterface::addTextForUnparser(
        newStmt, ss.str(), AstUnparseAttribute::e_after);
    SageInterface::replaceStatement(parent, newStmt);
}


void DataApplyGenerator::visit__data_apply10(Call__data_apply10 *call) {
    // Get this statement's location in the AST 
    SgFunctionCallExp *location = call->location();
    SgExprStatement *parent = isSgExprStatement(location->get_parent());
    SgNullStatement *newStmt = new SgNullStatement(parent->get_file_info());

    // ***********************************************
    // ***** Generate text for replacement code: *****
    // ***********************************************
    ostringstream ss;
    buildHeader(ss, call->dataIn1());

    // ******************************************
    // ***** Generate stencil function code *****
    // ******************************************

    // Extract body of function
    string body;
    extractBody(body, call->stencilFuncDef());

    // Build strings that represents the data objects
    ostringstream ssDataOut, ssDataIn1, ssDataIn2, ssDataIn3, ssDataIn4,
        ssDataIn5, ssDataIn6, ssDataIn7, ssDataIn8, ssDataIn9, ssDataIn10;

    ssDataOut << call->dataOut() << "\%vals(gw____blkI, gw____blkJ, gw____lbid)";
    ssDataIn1 << call->dataIn1() << "\%vals";
    ssDataIn2 << call->dataIn2() << "\%vals";
    ssDataIn3 << call->dataIn3() << "\%vals";
    ssDataIn4 << call->dataIn4() << "\%vals";
    ssDataIn5 << call->dataIn5() << "\%vals";
    ssDataIn6 << call->dataIn6() << "\%vals";
    ssDataIn7 << call->dataIn7() << "\%vals";
    ssDataIn8 << call->dataIn8() << "\%vals";
    ssDataIn9 << call->dataIn9() << "\%vals";
    ssDataIn10 << call->dataIn10() << "\%vals";

    // Find and replace instances the return statement
    SgFunctionDeclaration *decl = call->stencilFuncDef()->get_declaration();
    string funcName = decl->get_name().getString();
    findAndReplaceInString(body, funcName, ssDataOut.str());

    // Find and replace instances to data accesses
    string dataIn1Name = (decl->get_args()[0])->get_qualified_name().getString();
    string dataIn2Name = (decl->get_args()[1])->get_qualified_name().getString();
    string dataIn3Name = (decl->get_args()[2])->get_qualified_name().getString();
    string dataIn4Name = (decl->get_args()[3])->get_qualified_name().getString();
    string dataIn5Name = (decl->get_args()[4])->get_qualified_name().getString();
    string dataIn6Name = (decl->get_args()[5])->get_qualified_name().getString();
    string dataIn7Name = (decl->get_args()[6])->get_qualified_name().getString();
    string dataIn8Name = (decl->get_args()[7])->get_qualified_name().getString();
    string dataIn9Name = (decl->get_args()[8])->get_qualified_name().getString();
    string dataIn10Name = (decl->get_args()[9])->get_qualified_name().getString();
    findAndReplaceInString(body, dataIn1Name, ssDataIn1.str());
    findAndReplaceInString(body, dataIn2Name, ssDataIn2.str());
    findAndReplaceInString(body, dataIn3Name, ssDataIn3.str());
    findAndReplaceInString(body, dataIn4Name, ssDataIn4.str());
    findAndReplaceInString(body, dataIn5Name, ssDataIn5.str());
    findAndReplaceInString(body, dataIn6Name, ssDataIn6.str());
    findAndReplaceInString(body, dataIn7Name, ssDataIn7.str());
    findAndReplaceInString(body, dataIn8Name, ssDataIn8.str());
    findAndReplaceInString(body, dataIn9Name, ssDataIn9.str());
    findAndReplaceInString(body, dataIn10Name, ssDataIn10.str());

    // Find and replace i index
    string iIdx = (decl->get_args()[1])->get_qualified_name().getString();
    findAndReplaceInString(body, iIdx, "gw___blkI");

    // Find and replace j index
    string jIdx = (decl->get_args()[2])->get_qualified_name().getString();
    findAndReplaceInString(body, jIdx, "gw___blkJ");

    // Insert body
    ss << body << endl;
    
    // insert footer
    buildFooter(ss);

    // *********************************************
    // ***** Insert new code; remove old code: *****
    // *********************************************
    SageInterface::addTextForUnparser(
        newStmt, ss.str(), AstUnparseAttribute::e_after);
    SageInterface::replaceStatement(parent, newStmt);
}

