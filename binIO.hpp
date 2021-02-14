/******************************************************************************
 * GRIDGEN: Grid Generating Compiler
 * By: Andy Stone (aistone@gmail.com)
 * (C) Copyright 2011 Colorado State University
 *****************************************************************************/
/** \addtogroup IO
 *  @{
 */
#ifndef BINIO_HPP_
#define BINIO_HPP_

#include "environment.hpp"
#include "iserializable.hpp"
#include <iostream>
#include <map>
#include <string>

/**
 * Initialization function for the binio module.
 */
void initializeModule_binio();

/**
 * @ingroup IO
 *
 * This class includes several methods for inputting and outputting primitive
 * and serializable objects from and to input and output streams.
 */
class BinIO {
  public:
    // =======================
    // - [Output primitives] -
    // =======================
    /** @name Output primitives */
    ///@{

        /** Output an integer onto the stream. */
        static void out(std::ostream &out, const int &val);

        /** Output a double onto the stream. */
        static void out(std::ostream &out, const double &val);

        /** Output a boolean onto the stream. */
        static void out(std::ostream &out, const bool &val);

        /** Output a string onto the stream. */
        static void out(std::ostream &out, const std::string &s);

        /** Output a serializable object onto the stream. */
        static void out(std::ostream &out, const ISerializable &obj);

        /** Output a pointer to an environmental object. */
        static void outIdent(std::ostream &out, const IEnvironmental &obj);

    ///@}

    // =======================
    // - [Output arrays] -
    // =======================
    /** @name Output arrays */
    ///@{

        /** Output an array of primitives or objects. */
        template <typename T>
        static void out(std::ostream &out, T* A, int size);

        /**
         * Output an array of entities with a specialized output function
         * per object.
         */
        template <typename T>
        static void out(std::ostream &out, T* A, int size,
                        void (*saveFunc)(std::ostream&, const T&));

    ///@}

    // ==============================
    // - [Output STL collections] -
    // ==============================
    /** @name Output STL collections */
    ///@{

        /** Output a collection of primitives or objects. */
        template <typename IteratorT>
        static void out(std::ostream &out, IteratorT b, IteratorT e);

        /**
         * Output a collection of entities with a specialized output function
         * per object.
         */
        template <typename IteratorT, typename T>
        static void out(std::ostream &out, IteratorT b, IteratorT e,
                        void (*saveFunc)(std::ostream&, const T&));

        /**
         * Output an STL vector.  When outputting a vector that itself contains
         * STL objects it is necessary to use this function in place
         * of the iterators based output function.
         */
        template <typename T>
        static void out(std::ostream &out, const std::vector<T> &v);

        /** Output a collection of pointers to environmental objects.  */
        template <typename IteratorT>
        static void outIdents(std::ostream &out, IteratorT b, IteratorT e);
        
        /**
         * Output a map of pointers of environmental objects to some other
         * entity.
         **/

        /** Output an environmental map.  */
        template<typename T>
        static void outEnvMap(std::ostream &out,
                              const std::map<std::string, T*> &map);

    ///@}
    
    // =======================
    // - [Input primitives] -
    // =======================
    /** @name Input primitives */
    ///@{

        /** Input an integer from the stream. */
        static void in(std::istream &in, int &val);

        /** Input a double onto the stream. */
        static void in(std::istream &in, double &val);

        /** Input a boolean onto the stream. */
        static void in(std::istream &in, bool &val);

        /** Input a string onto the stream. */
        static void in(std::istream &in, std::string &s);

        /** Input a serializable object onto the stream. */
        static void in(std::istream &in, ISerializable &obj);

        /**
         * Input a pointer to an indentifiable object.  The parameter getter
         * should be a pointer to a function that can get a pointer to the
         * object (for example: &Environment::getSubgrid).
         **/
        template <typename T>
        static void inIdent(std::istream &in, T *ptr,
            T (*getter)(const std::string&));
    ///@}

    // =======================
    // - [Input arrays] -
    // =======================
    /** @name Input arrays */
    ///@{

        /** Input an array of primitives or objects. */
        template <typename T>
        static void in(std::istream &in, T** A);

        /**
         * Input an array of entities with a specialized input function
         * per object.
         */
        template <typename T>
        static void in(std::istream &in, T** A,
                       void (*loadFunc)(std::istream&, T&));

        /**
         * Input an STL vector.  When inputting a vector that itself contains
         * STL objects it is necessary to use this function in place
         * of the iterators based input functions.
         */
        template <typename T>
        static void in(std::istream &in, std::vector<T> &v);

    ///@}

    // ==============================
    // - [Input STL collections] -
    // ==============================
    /** @name Input STL collections */
    ///@{

        /**
         * Input a collection of primitives or objects.  The parameter insert is
         * an inserter iterator into the collection (for example STL's
         * back_inserter).  The parameter blank is a blank instance of the type
         * of object to read in.
         **/
        template <typename InserterT, typename T>
        static void in(std::istream &in,
                       InserterT insert,
                       T blank);

        /**
         * Input a collection of entites with a specialized input function.
         */
        template <typename InserterT, typename T>
        static void in(std::istream &in, InserterT inserter,
                       void (*loadFunc)(std::istream&, T&),
                       T blank);
        
        /**
         * Input a collection of pointers to identifiable objects.  This
         * function is passed an inserter iterator (for example
         * STL's back_inserter) where the pointers will be placed.  This
         * function is also passed a getter function from the environment
         * (for example &Environment::getSubgrid).
         **/
        template <typename InserterT, typename T>
        static void inIdents(std::istream &in,
                             InserterT insert,
                             T (*getter)(const std::string&));
        
        /** Input an environmental map.  */
        template<typename T>
        static void inEnvMap(std::istream &in, std::map<std::string, T*> &map);

    ///@}
};

template <typename T>
void BinIO::out(std::ostream &out, T* A, int size) {
    // output size of collection
    BinIO::out(out, size);

    // output each entity in the array individually 
    for(int i = 0; i < size; i++) {
        BinIO::out(out, A[i]);
    }
}

template <typename T>
void BinIO::out(std::ostream &out, T* A, int size,
                void (*saveFunc)(std::ostream&, const T&))
{
    // output size of collection
    BinIO::out(out, size);

    // output each entity in the array individually using the save function
    for(int i = 0; i < size; i++) {
        saveFunc(out, A[i]);
    }
}

template <typename IteratorT>
void BinIO::out(std::ostream &out, IteratorT b, IteratorT e)
{
    // output size of collection
    int sz = distance(b, e);
    BinIO::out(out, sz);
    
    // output each entity in the collection individually 
    while(b != e) {
        BinIO::out(out, (*b));
        b++;
    }
}

template <typename IteratorT, typename T>
void BinIO::out(std::ostream &out, IteratorT b, IteratorT e,
                void (*saveFunc)(std::ostream&, const T&))
{
    // output size of collection
    int sz = distance(b, e);
    BinIO::out(out, sz);
    
    // output each entity in the collection individually using the
    // save function
    while(b != e) {
        saveFunc(out, *b);
        b++;
    }
}

template <typename T>
void BinIO::out(std::ostream &out, const std::vector<T> &v) {
    typename std::vector<T>::size_type sz;
    int isz;
    sz = v.size();
    isz = sz;
    BinIO::out(out, isz);

    for(typename std::vector<T>::const_iterator i = v.begin();
                                                i != v.end(); i++)
    {
        BinIO::out(out, *i);
    }
}

template <typename IteratorT>
void BinIO::outIdents(std::ostream &out, IteratorT b, IteratorT e)
{
    // output size of collection
    int sz = distance(b, e);
    BinIO::out(out, sz);
    
    // output each identifier in the collection individually 
    while(b != e) {
        BinIO::out(out, (*b)->getID());
        b++;
    }
}

template<typename T>
void BinIO::outEnvMap(std::ostream &out, const std::map<std::string, T*> &map) {
    // Output size of the map
    BinIO::out(out, (int)map.size());

    // Output each (key, value) pair in the map
    for(typename std::map<std::string, T*>::const_iterator i = map.begin();
        i != map.end(); i++)
    {
        BinIO::out(out, i->first);
        (i->second)->output(out);
    }
}

template <typename T>
void BinIO::inIdent(std::istream &in, T *ptr,
                    T (*getter)(const std::string&))
{
    std::string ident;
    
    BinIO::in(in, ident);
    *ptr = getter(ident);
}

template <typename T>
void BinIO::in(std::istream &in, T** A)
{
    // input size of array
    int sz;
    BinIO::in(in, sz);
    *A = new T [sz];
    
    // input each entity in the collection individually 
    for(int i = 0; i < sz; i++) {
        BinIO::in(in, (*A)[i]);
    }
}

template <typename T>
void BinIO::in(std::istream &in, T** A,
               void (*loadFunc)(std::istream&, T&))
{
    // input size of array
    int sz;
    BinIO::in(in, sz);
    *A = new T [sz];
    
    // input each entity in the collection individually using the load
    // function
    for(int i = 0; i < sz; i++) {
        loadFunc(in, (*A)[i]);
    }
}

template <typename T>
void BinIO::in(std::istream &in, std::vector<T> &v)
{
    int size;
    BinIO::in(in, size);
    v.resize(size);

    for(int i = 0; i < size; i++) {
        BinIO::in(in, v[i]);
    }
}

template <typename InserterT, typename T>
void BinIO::in(std::istream &in, InserterT insert, T blank) {
    // input size of collection
    int sz;
    BinIO::in(in, sz);
    
    // output each entity in the collection individually 
    for(int i = 0; i < sz; i++) {
        BinIO::in(in, blank);
        *insert = blank;
    }
}

template <typename InserterT, typename T>
void BinIO::in(std::istream &in,
               InserterT inserter,
               void (*loadFunc)(std::istream&, T&),
               T blank)
{
    // input size of collection
    int sz;
    BinIO::in(in, sz);
    
    // input each entity in the collection individually using loadFunc
    for(int i = 0; i < sz; i++) {
        T tmp;
        loadFunc(in, tmp);
        *inserter = tmp;
    }
   
}

template <typename InserterT, typename T>
void BinIO::inIdents(std::istream &in, InserterT insert,
                     T (*getter)(const std::string&))
{
    std::string ident;
    int size;
        
    BinIO::in(in, size);

    for(int i = 0; i < size; i++) {
        BinIO::in(in, ident);
        *insert = getter(ident);
    }
}


template<typename T>
void BinIO::inEnvMap(std::istream &in, std::map<std::string, T*> &map)
{
    int size;
    std::string ident;
    T *value;
    
    // Input size of map
    BinIO::in(in, size);

    // Read in each element of the map
    for(int i = 0; i < size; i++) {
        BinIO::in(in, ident);
        Environment::newObj(ident, &value);
        BinIO::in(in, *value);
    }
}

#endif
