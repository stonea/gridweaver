/******************************************************************************
 * GRIDGEN: Grid Generating Compiler
 * By: Andy Stone (aistone@gmail.com)
 * (C) Copyright 2012 Colorado State University
 *****************************************************************************/
/** \addtogroup Environmental
 *  @{
 */
#ifndef IENVIRONMENTAL_HPP_
#define IENVIRONMENTAL_HPP_

#include <string>

class Environment;

/**
 * Environmental objects are objects that may be instantiated from an
 * environment and have an associated identifier.  Environmental objects
 * are responsible for containing their identifier.
 *
 * Classes of type IEnvironmental should friend Environment.
 */
class IEnvironmental {
  public:
    // =======================
    // - [Accessors] -
    // =======================
    /** @name Accessors */
    ///@{
        /** Return the identifier for this object. */
        virtual std::string getID() const = 0;
    ///@}
};

#endif
/** @}*/
