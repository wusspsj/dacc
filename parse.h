#include<stdio.h>
#include<stdlib.h>
#include<string.h>
struct strlist{
	char *str;          //source string,before and after translation
	size_t len;          //len means str[len] is the start of the next str,len is the num of the characters in the str;
	int type;      //0:computation,1:data,2shape,3:shell,4:calc,5:dedup;"no 6:ref"use as free;7:io;10:arg;11:dataref;12:.type;13:.range;20:dedup:;
	struct strlist *next;// 1:data,2:shape,3:io->rw,4:shell,5:calc,6:free,7:pre,10:arg,11:dataref,12:.type,13:.range;14:atomic;15:critical-->only in ompcc;30:scluster,32:scluster end
};
struct atomlist{
    char *st;
    char *en;
    int type;// atomic:1; critical:2;
    struct atomlist *next;
};
struct dataref{
	char *pos;//position
	int tp;//0:compt or 1:ref
	struct dataref *next;
};

struct datalist{         //data informantion
	char *dname;
	char *type;
	char *def;
	int dim;
	char **range;
	int dc;           //0:define in shape,else integer: define in the %dth dac_data;//no longer support undeclared define in shape
	struct dataref *dref;
	struct dataref *refcur;
	struct datalist *next;
};
struct datarefl{
	char *pos;
	struct datalist *dp;
	struct datarefl *next;
};
struct irangelist{//indexed range list
	struct varlist *var;
	int range;    // the serial num of ranges,from 0
	struct irangelist *next;
};
struct indexlist{
	char *iname;
	int id;  //from 0 or 1?->0, id unified
	int iol; //io line bit mask, using bits to mark which io line the index var appeared
	struct irangelist *ir;//signed ranges of each data
	struct indexlist *next;
};

struct splist{
	char *spname;
	int id;  //from 0 or 1?->0,id unified
	int iol;
	//	struct spvarlist *spv;
	struct irangelist *ir;//signed ranges of each data
	struct splist *next;
};
struct rangei{//range-index list
	int id;   //indicates range id ?from 0 or 1??from 0
	struct indexlist *idl;
	struct splist *spl;
	struct rangei *next;
};

struct varlist{
	char *vname;
	int dim;
	int vid;//id from 0
	int io;//io property;1,in;2,out,init with 0
	int ioid;
	int sis;//single-index-sp,123,init with 0--->random;atomic 1:0001;index 2:0010;sp 4:0100;
	struct rangei *ri;//range-index list,record indexed ranges
//	int spid;//sp property id,from 0 or 1?,from 1,0 indicates none
	char *ddname;//dedup name
//	int ddc;//what is ddc?->dedup count, the dedup ker num returned from chkdedup ->no need in var,should be used in re
	struct varlist *next;
};
struct typelist{
	char *type;
	struct typelist *next;
};
struct kernellist{
	int count;             //count related to the kernel id
	char *sname;	       //related shell name
	struct typelist *t;
	struct kernellist *next;
};
struct arglist{
	char *nmortp;  //name or type,name when calling,type in calc definition
	struct arglist *next;
};
struct calclist{         //calc information
	char *cname;
	char *start;     //indicates the '{'
	size_t clen;     //size from '{' to '}'
	int datan;    //data num
	int argn;     //arg num
	struct arglist *args;
	struct datalist *vars;
	struct strlist *strl;
	struct kernellist *ker;
	struct calclist *next;
};
struct ioprop{
	char *ins;
	char *ine;
	char *outs;
	char *oute;
	int id;//from 0
	struct irangelist *vars;
	struct rangei *indexs;
	struct ioprop *next;
};
struct shelllist{
	char *shname;
	int inum;     //index num
	int spnum;
	int dnum;     //data num
	int ionum;
//	int atomen;
	struct varlist *vars;
	struct indexlist *index;
	struct splist *sp;
	struct ioprop *ioexp;
//	struct posprop *sps;   //special prop
	struct shelllist *next;
};

struct dd{              //dedup
	char *ddname;
	int ddc;
	int ddflag;
};
struct reflist{
	struct datalist *ref;
	int iod;
	int idx;  // init with 0;explicit atomic 1, or atomic-dup sign 2; --> index bit mask
	int mask;//bit mask--->consider size_t for these masks??
//	int tid; //What's the purpose of tid?
	struct dd *ddi;//dedup index
	struct reflist *next;
};
struct shapelist{
	struct datalist *data;
	char **range_s;
	char *type;
	int dim_s;
	struct shapelist *next;
};
