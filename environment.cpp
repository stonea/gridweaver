/******************************************************************************
 * GRIDGEN: Grid Generating Compiler
 * By: Andy Stone (aistone@gmail.com)
 * (C) Copyright 2011 Colorado State University
 *****************************************************************************/
#include "environment.hpp"
#include "utils.hpp"
#include "error.hpp"
#include "binIO.hpp"
#include <iostream>
#include <fstream>
using namespace std;

Environment::NeighborMap_t      Environment::mNeighbors;
Environment::SubgridMap_t       Environment::mSubgrids;
Environment::GridMap_t          Environment::mGrids;
Environment::DistributionMap_t  Environment::mDistributions;
Environment::ScheduleMap_t      Environment::mSchedules;

static bool debug = false;
void initializeModule_environment() {
    GW_DEBUG_CTRL("environment:all", debug);
}

void Environment::clear() {
    mNeighbors.clear();
    mSubgrids.clear();
    mGrids.clear();
    mDistributions.clear();
    mSchedules.clear();
}

Neighbor* Environment::newNeighbor(const string &id) {
    DBG_MSG_V("Create a new neighbor.", id);

    // Create the new neighbor
    Neighbor *neigh = new Neighbor(id);

    // Attempt to insert the new neighbor into the environment.  Error out
    // if the neighbor already exists.
    pair<NeighborMap_t::iterator, bool> ret;
    ret = mNeighbors.insert(make_pair(id, neigh));
    if(ret.second == false) {
        error(ERR_ENV__DUPLICATE_ENTRY, id);
    }
    return (*ret.first).second;
}

void Environment::newObj(const std::string &id, Neighbor **neigh) {
    *neigh = newNeighbor(id);
}

Subgrid* Environment::newSubgrid(const string &id) {
    DBG_MSG_V("Create a new subgrid.", id);

    // Create the new subgrid
    Subgrid *sg = new Subgrid(id);

    // Attempt to insert the new neighbor into the environment.  Error out
    // if the neighbor already exists.
    pair<SubgridMap_t::iterator, bool> ret;
    ret = mSubgrids.insert(make_pair(id, sg));
    if(ret.second == false) {
        error(ERR_ENV__DUPLICATE_ENTRY, id);
    }
    return (*ret.first).second;
}

void Environment::newObj(const std::string &id, Subgrid **sg) {
    *sg = newSubgrid(id);
}

Grid* Environment::newGrid(const string &id) {
    DBG_MSG_V("Create a new grid.", id);

    // Create the new grid
    Grid *g = new Grid(id);

    // Attempt to insert the new grid into the environment.  Error out
    // if the grid already exists.
    pair<GridMap_t::iterator, bool> ret;
    ret = mGrids.insert(make_pair(id, g));
    if(ret.second == false) {
        error(ERR_ENV__DUPLICATE_ENTRY, id);
    }
    return (*ret.first).second;
}

void Environment::newObj(const std::string &id, Grid **grid) {
    *grid = newGrid(id);
}

Distribution* Environment::newDistribution(const std::string &id) {
    DBG_MSG_V("Create a new distribution.", id);

    // Create the new distribution
    Distribution *dist = new Distribution(id);

    // Attempt to insert the new distribution into the environment.  Error out
    // if the distribution already exists.
    pair<DistributionMap_t::iterator, bool> ret;
    ret = mDistributions.insert(make_pair(id, dist));
    if(ret.second == false) {
        error(ERR_ENV__DUPLICATE_ENTRY, id);
    }
    return (*ret.first).second;
}

void Environment::newObj(const std::string &id, Distribution **dist) {
    *dist = newDistribution(id);
}

Schedule* Environment::newSchedule(const std::string &id) {
    DBG_MSG_V("Create a new schedule.", id);

    // Create the new schedule
    Schedule *sched = new Schedule(id);

    // Attempt to insert the new distribution into the environment.  Error out
    // if the distribution already exists.
    pair<ScheduleMap_t::iterator, bool> ret;
    ret = mSchedules.insert(make_pair(id, sched));
    if(ret.second == false) {
        error(ERR_ENV__DUPLICATE_ENTRY, id);
    }
    return (*ret.first).second;
}

void Environment::newObj(const std::string &id, Schedule **sched) {
    *sched = newSchedule(id);
}

Neighbor* Environment::getNeighbor(const string &id) {
    return (*mNeighbors.find(id)).second;
}

Subgrid* Environment::getSubgrid(const string &id) {
    return (*mSubgrids.find(id)).second;
}

Grid* Environment::getGrid(const string &id) {
    return (*mGrids.find(id)).second;
}

Distribution* Environment::getDistribution(const string &id) {
    return (*mDistributions.find(id)).second;
}

Schedule* Environment::getSchedule(const std::string &id) {
    return (*mSchedules.find(id)).second;
}

void Environment::print(ostream &out) {
    // have master rank print neighbors
    if(isMasterRank()) {
        if(mNeighbors.empty()) {
            out << indt << "Environment contains no neighbors.";
        }
        for(NeighborMap_t::const_iterator i = mNeighbors.begin();
            i != mNeighbors.end(); i++)
        {
            (i->second)->print(out);
        }
    }

    // have master rank print subgrids
    if(isMasterRank()) {
        if(mSubgrids.empty()) {
            out << indt << "Environment contains no subgrids.";
        }
        for(SubgridMap_t::const_iterator i = mSubgrids.begin();
            i != mSubgrids.end(); i++)
        {
            (i->second)->print(out);
        }
    }
    
    // have master rank print grids
    if(isMasterRank()) {
        if(mSubgrids.empty()) {
            out << indt << "Environment contains no grids.";
        }
        for(GridMap_t::const_iterator i = mGrids.begin();
            i != mGrids.end(); i++)
        {
            (i->second)->print(out);
        }
    }

    // have master rank print distributions
    if(isMasterRank()) {
        if(mDistributions.empty()) {
            out << indt << "Environment contains no distributions.";
        }
        for(DistributionMap_t::const_iterator i = mDistributions.begin();
            i != mDistributions.end(); i++)
        {
            (i->second)->print(out);
        }
    }
    
    // print schedules
    if(isMasterRank()) {
        if(mSchedules.empty()) {
            out << indt << "Environment contains no schedules.";
        }
    }
    for(ScheduleMap_t::const_iterator i = mSchedules.begin();
        i != mSchedules.end(); i++)
    {
        (i->second)->print(out);
    }
}

void Environment::printSimp(ostream &out) {
    out << "<Environment>";
}

void Environment::output(ostream &out) {
    BinIO::outEnvMap(out, mNeighbors);
    BinIO::outEnvMap(out, mSubgrids);
    BinIO::outEnvMap(out, mGrids);
    BinIO::outEnvMap(out, mDistributions);
    BinIO::outEnvMap(out, mSchedules);
}

void Environment::input(istream &in) {
    BinIO::inEnvMap(in, mNeighbors);
    BinIO::inEnvMap(in, mSubgrids);
    BinIO::inEnvMap(in, mGrids);
    BinIO::inEnvMap(in, mDistributions);
    BinIO::inEnvMap(in, mSchedules);
}

