/**
 * TODO: This module allocates for only 256 file-handles and will not reuse
 *       a handle even after a file is closed.
 */
#include "stdio.h"
#include "stdlib.h"

FILE* fptrs[256];   // Room for 256 open files
int nextFile = 0;   // Next file-id to use

extern "C" {
    void __cio_fopen(char *filename, int &unit);
    void __cio_fclose(int &unit);
    void __cio_readInt(int &unit, int &val);
    void __cio_readString(int &unit, char *val, int &read);
    void __cio_setColor(int &color);
}

void __cio_fopen(char *filename, int &unit) {
    FILE *f = fopen(filename, "r");
    fptrs[nextFile] = f;
    unit = nextFile++;
}

void __cio_fclose(int &unit) {
    fclose(fptrs[unit]);
}

void __cio_readInt(int &unit, int &val) {
    fread((char*)&val, sizeof(int), 1, fptrs[unit]);
}

void __cio_readString(int &unit, char *val, int &read) {
    char c;

    read = 0;
    while(true) {
        fread(val, 1, 1, fptrs[unit]);
        if(*val == '\0') {
            return;
        }
        val++; read++;
    }
}

void __cio_setColor(int &color) {
    switch(color) {
        case -3: printf("\e[40;97m");   break;
        case -2: printf("\e[1;30m");   break;
        case -1: printf("\e[4;30m");   break;
        case 0:  printf("\e[0m");   break;
        case 1:  printf("\e[36m");  break;
        case 2:  printf("\e[34m");  break;
        case 3:  printf("\e[35m");  break;
        case 4:  printf("\e[32m");  break;
        case 10: printf("\e[37m");  break;
    }
}
