/******************************************************************************
 * GRIDGEN: Grid Generating Compiler
 * By: Andy Stone (aistone@gmail.com)
 * (C) Copyright 2011 Colorado State University
 *****************************************************************************/
#include "utils.hpp"

#include <algorithm>
#include <sstream>
#include <iostream>
#include <sys/stat.h>
#include <errno.h>
#include <unistd.h>
#include <mpi.h>
#include <string.h>
using namespace std;

int gNestedObjects = 0;
int gIndentLevel = 0;
bool gHighlighting = true;

void initializeModule_utils() {
    // If stdout is directing to a file turn off highlighting
    if(! isatty(fileno(stdout))) { gHighlighting = false; }
}

int myRank() {
    int r;
    MPI_Comm_rank(MPI_COMM_WORLD, &r);
    return r;
}

bool isMasterRank() {
    return(myRank() == masterRank());
}

int numRanks() {
    int n;
    MPI_Comm_size(MPI_COMM_WORLD, &n);
    return n;
}

int masterRank() {
    return 0;
}

void createDirectory(const std::string &dirName) {
    struct stat sb;

    // check if dirName already exists as a directory
    int statCode = stat(dirName.c_str(), &sb);

    if(statCode == -1 && errno == ENOENT) {
        if(mkdir(dirName.c_str(), 0755) == -1) {
            error(ERR_UTILS__UNABLE_TO_CREATE_DIR, dirName);
        }
    } else if(statCode == 0) {
        if(S_ISDIR(sb.st_mode)) return;
        error(ERR_UTILS__UNABLE_TO_CREATE_DIR, dirName);
    } else {
        error(ERR_UTILS__UNABLE_TO_CREATE_DIR, dirName);
    }
}

string num2SVGcolor(int idx) {
    switch(idx) {
        case 0: return "crimson";
        case 1: return "orange";
        case 2: return "yellow";
        case 3: return "lightgreen";
        case 4: return "cornflowerblue";
        case 5: return "violet";
        default: return "black";
    }
}

void trim(string& str) {
    str.erase(str.begin(), find_if(str.begin(), str.end(),
        not1(ptr_fun<int, int>(isspace))));
    str.erase(find_if(str.rbegin(), str.rend(),
        not1(ptr_fun<int, int>(isspace))).base(), str.end());
}

string str(int i) {
    stringstream ss;
    ss << i;
    return ss.str();
}

ostream &indt(ostream &os) {
    os << '\n';
    os.flush();
    for(int i = 0; i < gIndentLevel; i++) { os << "    "; }
    return os;
}

ostream &pushIndt(ostream &os) {
    gIndentLevel++;
    return os;
}

ostream &popIndt(ostream &os) {
    gIndentLevel--;
    return os;
}

void turnOffSyntaxHighlighting() {
    gHighlighting = false;
}

ostream &operator<<(ostream &out, hiType s) {
    if(gHighlighting) {
        out << COLOR_GREEN << s.val << COLOR_DISABLE;
    } else {
        out << s.val;
    }
    return out;
}

ostream &operator<<(ostream &out, hiIdent s) {
    if(gHighlighting) {
        out << COLOR_WHITE << s.val << COLOR_DISABLE;
    } else {
        out << s.val;
    }
    
    return out;
}

ostream &operator<<(ostream &out, hiProp s) {
    if(gHighlighting) {
        out << COLOR_CYAN << s.val << COLOR_DISABLE;
    } else {
        out << s.val;
    }

    return out;
}

ostream &operator<<(ostream &out, hiVal s) {
    if(gHighlighting) {
        out << COLOR_DISABLE << s.val << COLOR_DISABLE;
    } else {
        out << s.val;
    }

    return out;
}

ostream &operator<<(ostream &out, hiFmt s) {
    if(gHighlighting) {
        out << COLOR_DISABLE << s.val << COLOR_DISABLE;
    } else {
        out << s.val;
    }

    return out;
}

ostream &operator<<(ostream &out, hiHeading s) {
    if(gHighlighting) {
        out << COLOR_MAGENTA << s.val << COLOR_DISABLE;
    } else {
        out << s.val;
    }

    return out;
}

ostream &operator<<(ostream &out, hiErr s) {
    if(gHighlighting) {
        out << COLOR_MAGENTA << s.val << COLOR_DISABLE;
    } else {
        out << s.val;
    }

    return out;
}

ostream &operator<<(ostream &out, hiDbgFile s) {
    if(gHighlighting) {
        out << COLOR_EWHITE << s.val << COLOR_DISABLE;
    } else {
        out << s.val;
    }

    return out;
}

ostream &operator<<(ostream &out, hiDbgMsg s) {
    if(gHighlighting) {
        out << COLOR_DISABLE << s.val << COLOR_DISABLE;
    } else {
        out << s.val;
    }

    return out;
}


void printObj_start(ostream &out, const string &objType, const string &objName)
{
    out << indt << hiType(objType) << " "
                << hiIdent(objName)
                << hiFmt(" {")
                << pushIndt;
}

void printObj_end(ostream &out) {
    out << popIndt << indt << hiFmt("}") << endl;
}

void printObj_property(ostream &out, const string &property,
                       const string &value)
{
    out << indt << hiProp(property)
                << hiFmt(" = ")
                << hiVal(value);
}

void printObj_property(ostream &out, const string &property, int value) {
    out << indt << hiProp(property)
                << hiFmt(" = ")
                << hiVal(str(value));
}

void printObj_property(ostream &out, const string &property, bool value) {
    out << indt << hiProp(property)
                << hiFmt(" = ")
                << hiVal((value ? "true" : "false"));
}

void printObj_property(ostream &out, const string &property,
                       const IPrintable &value)
{
    ostringstream propVal;
    value.printSimp(propVal);

    out << indt << hiProp(property)
                << hiFmt(" = ")
                << hiVal(propVal.str());
}

void printObj_property(ostream &out, const string &property,
                       const IPrintable *value)
{
    ostringstream propVal;
    if(value != NULL) {
        value->printSimp(propVal);
    } else {
        propVal << "NULL";
    }

    out << indt << hiProp(property)
                << hiFmt(" = ")
                << hiVal(propVal.str());
}


void printObj_property(ostream &out, const string &property) {
    out << indt << hiProp(property)
                << hiFmt(" = ");
}

void printObj_propertyFromRank(std::ostream &out, int rank,
    const std::string &property, const std::string &value)
{
    const int MSG_SIZE  = 0;
    const int MSG_VALUE = 1;
    const int MSG_NUM_MSGS = 2;
    MPI_Request req[MSG_NUM_MSGS];

    int size;
    char *cstr;

    // If the rank send the value
    if(myRank() == rank) {
        size = value.length() + 1;
        cstr = new char[size];
        strcpy(cstr, value.c_str());
        
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
        
        printObj_property(out, property + "@" + str(rank), string(cstrRecv));
        delete [] cstrRecv;
    }
    
    // Have rank wait for its communication to complete
    if(myRank() == rank) {
        MPI_Waitall(1, req, MPI_STATUSES_IGNORE);
        delete [] cstr;
    }
}

void printObj_propertyFromRank(std::ostream &out, int rank,
    const std::string &property, int value)
{
    error(ERR_GENERIC__TODO);
}

void printObj_propertyFromRank(std::ostream &out, int rank,
    const std::string &property, bool value)
{
    error(ERR_GENERIC__TODO);
}

void printObj_propertyFromRank(std::ostream &out, int rank,
    const std::string &property, const IPrintable &value)
{
    error(ERR_GENERIC__TODO);
}

void printObj_propertyFromRank(std::ostream &out, int rank,
    const std::string &property, const IPrintable *value)
{
    error(ERR_GENERIC__TODO);
}

void printObj_propertyFromRank(std::ostream &out, int rank,
    const std::string &property)
{
    if(isMasterRank()) {
        printObj_property(out, property + "@" + str(rank));
    }
}

void printObj_startSection(std::ostream &out, const std::string &heading) {
    out << indt << hiHeading(heading) << pushIndt;
}

void printObj_endSection(std::ostream &out) {
    out << endl << popIndt;
}

