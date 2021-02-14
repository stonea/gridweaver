/******************************************************************************
 * GRIDGEN: Grid Generating Compiler
 * By: Andy Stone (aistone@gmail.com)
 * (C) Copyright 2011 Colorado State University
 *****************************************************************************/
#ifndef EXPRESSIONS_HPP_
#define EXPRESSIONS_HPP_

#include <stdio.h>
#include <iostream>
using namespace std;

enum Command {
    CMD_IF    = -1,
    CMD_AND   = -2,
    CMD_EQ    = -3,
    CMD_IDX   = -4,
    CMD_CONST = -5,
    CMD_PARAM = -6,
    CMD_MINUS = -7,
    CMD_PLUS  = -8,

    CMD_NUM_COMMANDS=8
};

class Exp {
  public:
    Exp() { mLeft = NULL; mRight = NULL; }
    int getCommand() { return mCmd; }
    Exp *getLeft() { return mLeft; }
    Exp *getRight() { return mRight; }
    void setCommand(int cmd) { mCmd = cmd; }
    void setLeft(Exp *left) { mLeft = left; }
    void setRight(Exp *right) { mRight = right; }
    string toString();

  private:
    int mCmd;
    Exp* mLeft;
    Exp* mRight;
};


#endif
