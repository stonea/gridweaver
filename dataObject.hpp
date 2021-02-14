/******************************************************************************
 * GRIDGEN: Grid Generating Compiler
 * By: Andy Stone (aistone@gmail.com)
 * (C) Copyright 2011 Colorado State University
 *****************************************************************************/
#ifndef DATA_OBJECT_HPP__
#define DATA_OBJECT_HPP__

#include <string>
#include <vector>
#include "iprintable.hpp"
#include "iserializable.hpp"
#include "ienvironmental.hpp"
#include "schedule.hpp"
#include "grid.hpp"
#include "distribution.hpp"

using namespace std;

class DataObject : public virtual IPrintable,
                   public virtual IEnvironmental
{
  public:
    DataObject(string name)
        :
        mName(name),
        mGrid(NULL),
        mSchedule(NULL),
        mDistribution(NULL)
    {}

    DataObject(string name,
               Grid  *grid,
               Schedule  *schedule,
               Distribution  *distribution)
        :
        mName(name),
        mGrid(grid),
        mSchedule(schedule),
        mDistribution(distribution)
    {}
    
    void setGrid(Grid *grid) {
        mGrid = grid;
    }
    
    void setSchedule(Schedule *schedule) {
        mSchedule = schedule;
    }
    
    void setDistribution(Distribution *distribution) {
        mDistribution = distribution;
    }

    virtual void print(ostream &out) const;
    virtual void printSimp(ostream &out) const;

    virtual string getID() const { return mName; }

  private:
    string  mName;
    Grid  *mGrid;
    Schedule  *mSchedule;
    Distribution  *mDistribution;
};

#endif
