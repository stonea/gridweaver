/******************************************************************************
 * GRIDGEN: Grid Generating Compiler
 * By: Andy Stone (aistone@gmail.com)
 * (C) Copyright 2011 Colorado State University
 *****************************************************************************/

#include "vispage.hpp"
#include "error.hpp"
#include "utils.hpp"
#include <iostream>
#include <fstream>
using namespace std;

void initializeModule_vispage() {
}

void VisPage::add(const string &id, CellFieldPicture pic) {
    pair<VisualizationMap_t::iterator, bool> ret;

    ret = mVisualizations.insert(make_pair(id, pic));
    if(ret.second == false) {
        error(ERR_VISPAGE__DUPLICATE_ENTRY, id);
    }
}


void VisPage::save(const string &dirName) {
    createDirectory(dirName);

    // Output each of the visualizations
    for(VisualizationMap_t::iterator i = mVisualizations.begin();
        i != mVisualizations.end(); i++)
    {
        (i->second).saveToFile(dirName + "/" + i->first + ".svg");
    }

    // Output the index.html file
    //----------------------------
        string filename = dirName + "/index.html";
        ofstream index(filename.c_str());
        outputHtmlHeader(index);

        // iterate through each of the visualizations and place it in the
        // HTML file
        for(VisualizationMap_t::iterator i = mVisualizations.begin();
            i != mVisualizations.end(); i++)
        {
            embedVisualization(index, i->first + ".svg", i->first);
        }

        outputHtmlFooter(index);
}

void VisPage::outputHtmlHeader(std::ostream &out) {
    out << indt << "<html>" << pushIndt;
    out << indt << "<head>" << pushIndt;
    out << indt << "<title>Visualization</title>";
    out << popIndt << indt << "</head>";
    out << indt << "<body>" << pushIndt;
}

void VisPage::outputHtmlFooter(std::ostream &out) {
    out << popIndt << indt << "</body>";
    out << popIndt << indt << "</html>";
}

void VisPage::embedVisualization(
    ostream &out, const string &filename, const string &embedName)
{
    // Output title for the embed
    out << indt << "<h1>" << embedName << ":</h1>";

    // Output the embed
    out << indt
        << "<embed id=\""
        << embedName
        << "\" src=\""
        << filename
        << "\" type=\"image/svg+xml\" width=\"100%\" height=\"100%\" />";
}

