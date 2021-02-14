/******************************************************************************
 * GRIDGEN: Grid Generating Compiler
 * By: Andy Stone (aistone@gmail.com)
 * (C) Copyright 2011 Colorado State University
 *****************************************************************************/
/** \addtogroup IO
 *  @{
 */
#ifndef CELLFIELDPICTURE_HPP_
#define CELLFIELDPICTURE_HPP_

#include <string>

/**
 * Initialization function for the cellfieldpicture module.
 */
void initializeModule_cellfieldpicture();

/**
 * @ingroup IO
 *
 * Outputs an array of circles (cells).  Each circle may filled with a
 * specified color and be labeled with text.  A box may optionally be placed
 * behind a region of cells.
 */
class CellFieldPicture {
  public:
    CellFieldPicture(int width, int height);

    // =======================
    // - [Input and Output] -
    // =======================
    /** @name Input and Output */
    ///@{
        void saveToFile(std::string filename);
    ///@}

    // =======================
    // - [Modifiers] -
    // =======================
    /** @name Modifiers */
    ///@{
        /** Set the label for a given cell. */
        void setLabel(int x, int y, std::string label);

        /** Set the stroke color for a given cell. */
        void setStroke(int x, int y, std::string stroke);

        /** Set the fill color for a given cell. */
        void setFill(int x, int y, std::string fill);

        /**
         * Set the coordinates of an optional box that is placed behind a
         * region of cells.
         */
        void setBox(int x1, int y1, int x2, int y2);
    ///@}

    // =======================
    // - [Accessors] -
    // =======================
    /** @name Accessors */
    ///@{
        std::string getLabel(int x, int y);
        std::string getStroke(int x, int y);
        std::string getFill(int x, int y);
    ///@}


  private:
    int mWidth; int mHeight;
    std::string **mLabel;
    std::string **mStroke;
    std::string **mFill;

    // A box may be placed behind some set of the cells
    int mBoxX1, mBoxY1, mBoxX2, mBoxY2;

    // The following constants control the spacing between cells:
    int mSpacing;
    int mRadius;

    static const int NO_BOX = -1;
};

#endif
/** @}*/
