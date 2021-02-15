/******************************************************************************
 * GRIDGEN: Grid Generating Compiler
 * By: Andy Stone (aistone@gmail.com)
 * (C) Copyright 2011 Colorado State University
 *****************************************************************************/
#include <iostream>
#include <string>
#include <rose.h>
#include <getopt.h>
#include "environment.hpp"
#include "error.hpp"
#include "utils.hpp"
#include "svgprinter.hpp"
#include "cellfieldpicture.hpp"
#include "vispage.hpp"
#include "binIO.hpp"
#include "runtimewrapper.hpp"
#include "analyses.hpp"
#include "codegen.hpp"
#include <mpi.h>
using namespace std;

#define VERSION_MAJOR 0
#define VERSION_MINOR 1


void runScratchCode();

/***
 * Debugging options:
 *
 *           all:   All modules
 *   environment:   Environment module
 *       wrapper:   Fortran to C++ wrapper
 */


// Command line options (see GNU getlongopt for more info).
static struct option options[] = {
    {              "version", no_argument,  0, 'v'},
    {                 "help", no_argument,  0, 'h'},
    
    // We have a terminator option so that when iterating through this array
    // we know when to stop.
    {(const char*)NULL, 0, 0, '\0'}
};

/**
 * Print usage information about gridweaver to the console.
 */
void usage() {
    cout << "gridweaver opts" << endl;
}

/**
 * Print the current version of GridWeaver to the console.  This message is
 * displayed when the -v command line flag is passed to gridweaver.
 */
void version() {
    cout << "GridWeaver semi-regular grid computation compiler: version "
         << VERSION_MAJOR << "." << VERSION_MINOR << endl;
}

/**
 * Print help message to the console.  This message is displayed when the -h
 * command line flag is passed to gridweaver.
 */
void help() {
    usage();

    cout << "Options:";
    for(int i = 0; ; i++) {
        if(options[i].name == (const char*)NULL) break;
        cout << " " << options[i].name;
    }
    cout << endl;
}



/**
 * Run any initialization code that exists for every module.
 */
void initializeModules() {
    initializeModule_error();
    initializeModule_iprintable();
    initializeModule_svgprinter();
    initializeModule_cellfieldpicture();
    initializeModule_vispage();
    initializeModule_binio();
    initializeModule_utils();
    initializeModule_grid();
    initializeModule_environment();
    initializeModule_distribution();
    initializeModule_schedule();
    initializeModule_runtimewrapper();
}

/**
 * Initialize MPI
 */
void initializeMPI(int *argc, char **argv[]) {
    int err;

    err = MPI_Init(argc, argv);
    if (err != MPI_SUCCESS) {
        printf("Error starting MPI program. Terminating.\n");
        MPI_Abort(MPI_COMM_WORLD, err);
    } 

    MPI_Barrier(MPI_COMM_WORLD);
}

/**
 * GridWeaver begins execution here.  Read in command line arguments and
 * execute the appropriate subprogram.
 */
int main(int argc, char *argv[]) {
    SgProject *proj = NULL;

    // Initialization
    initializeMPI(&argc, &argv);
    initializeModules();
    runScratchCode();
    
    // Only have the master process execute
    if(myRank() == 0) {
        // Read in and respond to command line arguments
        int optIdx = 0;
        char opt;
        while((opt = getopt_long(argc, argv, "vh12",
                                 options, &optIdx)) != -1)
        {
            switch(opt) {
                case 'v':
                    version();
                    break;
                
                case 'h':
                    help();
                    break;
            }
        }

        // Copy non argopt params into their own 
        cout << "Count is: " << argc << endl;
        for(int i = optind; i < argc; i++) {
            cout << argv[i] << endl;
        }

        // Analyze the program, extract calls
        proj = frontend(argc, argv);

        CallsAnalysis analysis;
        analysis.analyze(proj);
        //analysis.print(cout);
        Environment::print(cout);

        CodeGen gen(&analysis);
        gen.apply();
    }

    MPI_Barrier(MPI_COMM_WORLD);
    MPI_Finalize(); 

    if(proj != NULL) {
        proj->unparse();
    }
    return 1;
}


/**
 * This function is for temporary scratch code that will be executed at the
 * start.
 */
void runScratchCode() {
    /*int nProcs = 4;
    int n = 10;
    int blkW = 5; int blkH = 5;

    Neighbor *neigh_n  = Environment::newNeighbor("neigh_n");
              neigh_n->set(0, 1);
    Neighbor *neigh_ne = Environment::newNeighbor("neigh_ne");
              neigh_ne->set(1, 1);
    Neighbor *neigh_e  = Environment::newNeighbor("neigh_e");
              neigh_e->set(1, 0);
    Neighbor *neigh_se = Environment::newNeighbor("neigh_se");
              neigh_se->set(1, -1);
    Neighbor *neigh_s  = Environment::newNeighbor("neigh_s");
              neigh_s->set(0, -1);
    Neighbor *neigh_sw = Environment::newNeighbor("neigh_sw");
              neigh_sw->set(-1,-1);
    Neighbor *neigh_w  = Environment::newNeighbor("neigh_w");
              neigh_w->set(-1, 0);
    Neighbor *neigh_nw = Environment::newNeighbor("neigh_nw");
              neigh_nw->set(-1, 1);

    Subgrid *sgA = Environment::newSubgrid("sgA"); sgA->setExtents(n, n);

    Grid *g = Environment::newGrid("g");
    g->addSubgrid(sgA);
    
    Distribution *dist = Environment::newDistribution("dist");
    dist->applyFillBlock(g, 4, 2);
    //dist->visualize("dist");
    Schedule *sched = Environment::newSchedule("sched");
    sched->calculate(g, dist);

    Environment::print(cout);*/
}
