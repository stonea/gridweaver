/******************************************************************************
 * GRIDGEN: Grid Generating Compiler
 * By: Andy Stone (aistone@gmail.com)
 * (C) Copyright 2011 Colorado State University
 *****************************************************************************/
/** \addtogroup Environmental
 *  @{
 */
#ifndef ENVIRONMENT_HPP_
#define ENVIRONMENT_HPP_

#include <map>

#include "grid.hpp"
#include "distribution.hpp"
#include "schedule.hpp"
#include "dataObject.hpp"

/**
 * Initialization function for the environment module.
 */
void initializeModule_environment();


/**
 * The GridWeaver programming model includes an environment of objects that are
 * used to specify a semi-regular grid computation.  These objects include:
 *
 *   - Neighbors
 *   - Subgrids
 *   - Grids
 *   - Distributions
 *   - Schedules
 *   - Data objects
 *
 * These objects are instantiated by and into the environment and can be
 * loaded and stored into state files.  All environmental objects are also
 * printable (that is a representation of them can printed to the console).
 * Some are also graphical (that is a representation of them can be printed
 * in SVG format).
 *
 * Neighbors, subgrids, grids, distributions, and data objects are globally
 * replicated across all MPI ranks.  Schedule objects are distributed across
 * ranks.
 */
class Environment {
  public:
    // =======================
    // - [Construction] -
    // =======================
    /** @name Construction */
    ///@{
        /**
         * Populate an environment with objects given a list of gridweaver
         * calls.
         */
        //void buildFromCalls(const CallsVectorT &calls);
    ///@}

    // =======================
    // - [Population] -
    // =======================
    /** @name Population */
    ///@{
        /**
         * Remove all entities from the environment.
         */
        static void clear();

        /**
         * Add a new neighbor into the environment with the specified
         * identifier.
         */
        static Neighbor* newNeighbor(const std::string &id);
        static void newObj(const std::string &id, Neighbor **neigh);

        /**
         * Add a new subgrid into the environment with the specified
         * identifier.
         */
        static Subgrid* newSubgrid(const std::string &id);
        static void newObj(const std::string &id, Subgrid **sg);

        /**
         * Add a new grid into the environment with the specified identifier.
         */
        static Grid* newGrid(const std::string &id);
        static void newObj(const std::string &id, Grid **grid);

        /**
         * Add a new distribution into the environment with the specified
         * identifier.
         */
        static Distribution* newDistribution(const std::string &id);
        static void newObj(const std::string &id, Distribution **dist);

        /**
         * Add a new schedule into the environment with the specified
         * identifier.
         */
        static Schedule* newSchedule(const std::string &id);
        static void newObj(const std::string &id, Schedule **sched);

        /**
         * Add a new data object into the environment with the specified
         * identifier.
         */
        static DataObject* newDataObject(const std::string &id);
        static void newObj(const std::string &id, DataObject **data);
    ///@}

    // =======================
    // - [Accessors] -
    // =======================
    /** @name Accessors */
    ///@{
        /**
         * Retrieve the neighbor with the specified identifier.  If the
         * specified neighbor does not exist throw an error.
         **/
        static Neighbor* getNeighbor(const std::string &id);

        /**
         * Retrieve the subgrid with the specified identifier.  If the
         * specified subgrid does not exist throw an error.
         **/
        static Subgrid* getSubgrid(const std::string &id);
    
        static std::map<std::string, Neighbor*>::const_iterator neighborsBegin();
        static std::map<std::string, Neighbor*>::const_iterator neighborsEnd();
        
        /**
         * Retrieve the grid with the specified identifier.  If the
         * specified grid does not exist throw an error.
         **/
        static Grid* getGrid(const std::string &id);
        
        /**
         * Retrieve the distribution with the specified identifier.  If the
         * specified distribution does not exist throw an error.
         **/
        static Distribution* getDistribution(const std::string &id);

        /**
         * Retrieve the schedule object with the specified identifier.  If the
         * specified distribution does not exist throw an error.
         **/
        static Schedule* getSchedule(const std::string &id);

        /**
         * Retrieve the data object with the specified identifier.  If the
         * specified distribution does not exist throw an error.
         **/
        DataObject* getDataObject(const std::string &id);
 
        /** Return the number of neighbors in the environment. */
        static int numNeighbors() { return mNeighbors.size(); }

        /** Return the number of subgrids in the environment. */
        static int numSubgrids() { return mSubgrids.size(); }

        /** Return the number of grids in the environment. */
        static int numGrids() { return mGrids.size(); }

        /** Return the number of distributions in the environment. */
        static int numDistributions() { return mDistributions.size(); }

        /** Return the number of schedules in the environment. */
        static int numSchedules() { return mSchedules.size(); }
    ///@}


    // =======================
    // - [Input/Output] -
    // =======================
    /** @name Input/Output */
    ///@{
        static void print(std::ostream &out);
        static void printSimp(std::ostream &out);
        static void output(std::ostream &out);
        static void input(std::istream &in);
    ///@}


    typedef std::map<std::string, Neighbor*>     NeighborMap_t;
    typedef std::map<std::string, Subgrid*>      SubgridMap_t;
    typedef std::map<std::string, Grid*>         GridMap_t;
    typedef std::map<std::string, Distribution*> DistributionMap_t;
    typedef std::map<std::string, Schedule*>     ScheduleMap_t;
    typedef std::map<std::string, DataObject*>   DataObjectMap_t;

  private:
    // Maps of identifiers to environment objects:
    static NeighborMap_t     mNeighbors;
    static SubgridMap_t      mSubgrids;
    static GridMap_t         mGrids;
    static DistributionMap_t mDistributions;
    static ScheduleMap_t     mSchedules;
    static DataObjectMap_t   mDataObjects;
};

#endif
/** @}*/
