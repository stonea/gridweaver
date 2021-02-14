/******************************************************************************
 * GRIDGEN: Grid Generating Compiler
 * By: Andy Stone (aistone@gmail.com)
 * (C) Copyright 2011 Colorado State University
 *****************************************************************************/
/** \addtogroup IO
 *  @{
 */

#ifndef VISPAGE_HPP_
#define VISPAGE_HPP_

#include <string>
#include <map>
#include "cellfieldpicture.hpp"

/**
 * Initialization function for the vispage module.
 */
void initializeModule_vispage();

/**
 * A visualization page contains a collection of visualizations that the
 * user can interact with.
 */
class VisPage {
  public:
    // =======================
    // - [Construction] -
    // =======================
    /** @name Construction */
    ///@{
        /** Add a visuzliation to the page with the given ID. */
        void add(const std::string &id, CellFieldPicture pic);
    ///@}

    // =======================
    // - [Input/Output] -
    // =======================
    /** @name Input/Output */
    ///@{
        /**
         * Output the visualization page and its visualizations into the
         * specified directory.
         */
        void save(const std::string &dirName);
    ///@}


    typedef std::map<std::string, CellFieldPicture> VisualizationMap_t;

  private:
    // =======================
    // - [Input/Output] -
    // =======================
        /**
         * Output the header for the HTML file that we will place the
         * visualizations in.
         */
        void outputHtmlHeader(std::ostream &out);

        /**
         * Output the footer for the HTML file that we will place the
         * visualizations in.
         */
        void outputHtmlFooter(std::ostream &out);

        /**
         * Embed the image to the visualization in the HTML file.
         */
        void embedVisualization(std::ostream &out, const std::string &filename,
                                const std::string &embedName);

    std::map<std::string, CellFieldPicture> mVisualizations;
};

#endif
/** @}*/
