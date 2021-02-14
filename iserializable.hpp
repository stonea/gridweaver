/******************************************************************************
 * GRIDGEN: Grid Generating Compiler
 * By: Andy Stone (aistone@gmail.com)
 * (C) Copyright 2011 Colorado State University
 *****************************************************************************/
/** \addtogroup IO
 *  @{
 */
#ifndef ISERIALIZABLE_HPP_
#define ISERIALIZABLE_HPP_

#include <ostream>

/**
 * Serializable objects are able to push or pull binary copies of themselves
 * onto or from streams.
 */
class ISerializable {
  public:
    // =======================
    // - [Input and Output] -
    // =======================
    /** @name Input and Output */
    ///@{

        /** Output a binary copy of this object onto a stream. */
        virtual void output(std::ostream &out) const = 0;

        /** Input a binary copy of the object from a stream. */
        virtual void input(std::istream &in) = 0;

    ///@}
};

#endif
/** @}*/
