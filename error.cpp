#include "error.hpp"
#include <iostream>
#include <stdio.h>
#include <unistd.h>
#include <stdlib.h>
#include "utils.hpp"
using namespace std;

// Headers needed for stack dump:
#include <cxxabi.h>
#include <execinfo.h>
#include <execinfo.h>
#include <signal.h>
#include <stdio.h>
#include <stdio.h>
#include <stdlib.h>
#include <stdlib.h>
#include <string.h>
#include <ucontext.h>
#include <unistd.h>
#include <mpi.h>

void initializeModule_error() {

}

// Function from: <http://blog.aplikacja.info/2010/12/backtraces-for-c/>
/** Print a demangled stack backtrace of the caller function to FILE* out. */
static void print_stacktrace(const char* source, FILE *out = stderr, unsigned int max_frames = 63)
{
    char linkname[512]; /* /proc//exe */
    char buf[512];
    pid_t pid;
    int ret;

    /* Get our PID and build the name of the link in /proc */
    pid = getpid();
    snprintf(linkname, sizeof(linkname), "/proc/%i/exe", pid);

    /* Now read the symbolic link */
    ret = readlink(linkname, buf, 512);
    buf[ret] = 0;

    fprintf(out, "---------------------------------------------\n");
    fprintf(out, "stack trace (%s) for process %s (PID:%d):\n",
        source, buf, pid);

    // storage array for stack trace address data
    void* addrlist[max_frames+1];

    // retrieve current stack addresses
    int addrlen = backtrace(addrlist, sizeof(addrlist) / sizeof(void*));

    if (addrlen == 0) {
        fprintf(out, "  \n");
        return;
    }

    // resolve addresses into strings containing "filename(function+address)",
    // this array must be free()-ed
    char** symbollist = backtrace_symbols(addrlist, addrlen);

    // allocate string which will be filled with the demangled function name
    size_t funcnamesize = 256;
    char* funcname = (char*)malloc(funcnamesize);

    // iterate over the returned symbol lines. skip first two,
    // (addresses of this function and handler)
    for (int i = 2; i < addrlen; i++)
    {
    char *begin_name = 0, *begin_offset = 0, *end_offset = 0;

    // find parentheses and +address offset surrounding the mangled name:
    // ./module(function+0x15c) [0x8048a6d]
    for (char *p = symbollist[i]; *p; ++p)
    {
        if (*p == '(')
        begin_name = p;
        else if (*p == '+')
        begin_offset = p;
        else if (*p == ')' && begin_offset) {
        end_offset = p;
        break;
        }
    }

    if (begin_name && begin_offset && end_offset
        && begin_name < begin_offset)
    {
        *begin_name++ = '\0';
        *begin_offset++ = '\0';
        *end_offset = '\0';

        // mangled name is now in [begin_name, begin_offset) and caller
        // offset in [begin_offset, end_offset). now apply
        // __cxa_demangle():

        int status;
        char* ret = abi::__cxa_demangle(begin_name,
                        funcname, &funcnamesize, &status);
        if (status == 0) {
        funcname = ret; // use possibly realloc()-ed string
        fprintf(out, "  (PID:%d) %s : %s+%s\n",
            pid, symbollist[i], funcname, begin_offset);
        }
        else {
        // demangling failed. Output function name as a C function with
        // no arguments.
        fprintf(out, "  (PID:%d) %s : %s()+%s\n",
            pid, symbollist[i], begin_name, begin_offset);
        }
    }
    else
    {
        // couldn't parse the line? print the whole line.
        fprintf(out, "  (PID:%d) %s: ??\n", pid, symbollist[i]);
    }
    }

    free(funcname);
    free(symbollist);

    fprintf(out, "---------------------------------------------\n");
}

Error findError(ERRCODE code) {
    // iterate through the global table of error information until we find the one
    // with the proper code
    for(int i = 0; ; i++) {
        if(gErrors[i].code == ERR_NULL) break;
        if(gErrors[i].code == code) return gErrors[i];
    }

    Error e;
    e.code  = ERR_UNCLASSIFIED;
    e.sCode = "ERR_UNCLASSIFIED";
    e.msg   = "An unclassified error occured.";
    
    return e;
}

void error(ERRCODE code) {
    if(isMasterRank()) {
        Error err = findError(code);

        cerr << hiErr("Encountered error: ") << hiErr(err.sCode) << endl;
        cerr << "  " << hiErr(err.msg) << endl;
        print_stacktrace("gridweaver");
    
        MPI_Abort(MPI_COMM_WORLD, 1);
    }
    MPI_Barrier(MPI_COMM_WORLD);
}

void error(ERRCODE code, string arg) {
    if(isMasterRank()) {
        Error err = findError(code);

        cerr << hiErr("Encountered error: ") << hiErr(err.sCode) << endl;
        cerr << "  " << hiErr(err.msg) << endl;
        cerr << "  " << hiProp("Argument: ") << hiVal(arg) << endl;
        print_stacktrace("gridweaver");

        MPI_Abort(MPI_COMM_WORLD, 1);
    }
    MPI_Barrier(MPI_COMM_WORLD);
}
