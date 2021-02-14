/******************************************************************************
 * GRIDGEN: Grid Generating Compiler
 * By: Andy Stone (aistone@gmail.com)
 * (C) Copyright 2011 Colorado State University
 *****************************************************************************/
/** \addtogroup IO
 *  @{
 */
#ifndef IPRINTABLE_HPP_
#define IPRINTABLE_HPP_

#include <ostream>

/**
 * Initialization function for the iprintable module.
 */
void initializeModule_iprintable();

/**
 * Printable objects are able to push human readable representations of
 * themselves onto a text stream such as the console.  There are two
 * representations of printable objects: (1) the detailed representation, and
 * the (2) simple representation.
 */
class IPrintable {
  public:
    // =======================
    // - [Input and Output] -
    // =======================
    /** @name Input and Output */
    ///@{
        /**
         * Output a detailed representation of this object onto a text stream.
         * The detailed representation should properly indent and highlight the
         * output using the indt and syntax manipulators in <utils.hpp>.
         **/
        virtual void print(std::ostream &out) const = 0;

        /**
         * Output a simplified representation of this object onto a text
         * stream.
         **/
        virtual void printSimp(std::ostream &out) const = 0;
    ///@}
};

/** Let IPrintable objects be passed to ostreams via <<. */
std::ostream& operator<<(std::ostream& os, const IPrintable& val);

/** Let pointers to IPrintable objects be passed to ostreams via <<. */
std::ostream& operator<<(std::ostream& os, const IPrintable *val);

#endif
/** @}*/
