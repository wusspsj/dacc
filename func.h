int dacocl(char *,char *,char *);
int dacomp(char *,char *,char *);
//first pass
void signstr(char *cur,size_t len,int type);
void rmcomment(char *cur,size_t *slen);
int getstrtype(char *c,int pos);
int getdata(char *c,int d);
int getshape(char *c);
size_t checkmain(char *cur);
void signref(char *c,size_t len);
void signcalc(char *c, size_t slen);
void signshell(char *c, size_t slen);
//void signdedup(char *c, size_t slen);
int chkchar(char c);

//second pass omp
void dealcomptOMP(struct strlist *p);
int dealshapeOMP(struct shapelist *p);
struct datalist * dealdataOMP(struct datalist *p);
int dealrwOMP(struct strlist *p);
int dealfreeOMP(void);

//second pass ocl
void dealcomptOCL(struct strlist *p);
int dealshapeOCL(struct shapelist *p);
struct datalist * dealdataOCL(struct datalist *p);
int dealrwOCL(struct strlist *p);
int dealfreeOCL(void);
