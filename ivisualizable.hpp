/******************************************************************************
 * GRIDGEN: Grid Generating Compiler
 * By: Andy Stone (aistone@gmail.com)
 * (C) Copyright 2011 Colorado State University
 *****************************************************************************/
/** \addtogroup IO
 *  @{
 */
#ifndef IVISUALIZABLE_HPP_
#define IVISUALIZABLE_HPP_

#include <string>

/**
 * Visualizable objects are able to output a visual representation of themselves
 * into one or more SVG files that are presentable to the user via an HTML
 * page.
 */
class IVisualizable {
  public:
    // =======================
    // - [Input and Output] -
    // =======================
    /** @name Input and Output */
    ///@{
        /**
         * Output files that visualize this object under the specified directory.
         **/
        virtual void visualize(const std::string &outDir) const = 0;
    ///@}
};

#endif
/** @}*/
