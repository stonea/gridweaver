/******************************************************************************
 * GRIDGEN: Grid Generating Compiler
 * By: Andy Stone (aistone@gmail.com)
 * (C) Copyright 2011 Colorado State University
 *****************************************************************************/
#include "expressions.hpp"

#include <sstream>
using namespace std;

string Exp::toString() {
    ostringstream ss;

    switch(mCmd) {
        case CMD_IF:
            ss << "IF(" << mLeft->toString() << ")";
        break;

        case CMD_AND:
            ss << "AND(" << mLeft->toString() << ", " << mRight->toString()
               << ")";
        break;

        case CMD_EQ:
            ss << "EQ(" << mLeft->toString() << ", " << mRight->toString()
               << ")";
        break;

        case CMD_IDX:
            ss << "IDX(" << mLeft->mCmd << ")";
        break;

        case CMD_CONST:
            ss << "CONST(" << mLeft->mCmd << ")";
        break;

        case CMD_PARAM:
            ss << "PARAM(" << mLeft->mCmd << ")";
        break;

        case CMD_MINUS:
            ss << "MINUS(" << mLeft->toString() << ", " << mRight->toString()
               << ")";
        break;

        case CMD_PLUS:
            ss << "PLUS(" << mLeft->toString() << ", " << mRight->toString()
               << ")";
        break;
    }

    return ss.str();
}

